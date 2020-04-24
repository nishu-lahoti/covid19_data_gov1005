library(tidyverse)
library(dplyr)
library(readr)
library(rvest)
library(janitor)
library(skimr)
library(sf)
library(maps)
library(tibble)
library(countrycode)
library(rworldmap)
library(gganimate)


##### Policy: Oxford + JHU Data #####

# Import and clean Oxford Covid-Policy-Tracker data. Both sets are downloaded
# here, but we will likely only need the first (the second is for our own
# reference, as it contains detailed explanatory notes).

oxford <- read.csv(url("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv")) %>% 
  mutate(new_date = as.Date(as.character(Date), format = "%Y%m%d"))

# oxford_detailed <- read.csv(url("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest_withnotes.csv"))

# Read in and clean global JHU CSSE data. Update the date format and create an
# increment column for the confirmed cases across the globe

# Confirmed

global_confirmed <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")) %>% 
  clean_names() %>% 
  pivot_longer(cols = -c(province_state, country_region, lat, long), names_to = "date", values_to = "confirmed") %>%
  select(country_region, date, confirmed)

global_confirmed <- global_confirmed %>% 
  mutate(sep_date = sub("x", "", date)) %>%
  mutate(new_date = as.Date(sep_date, format = "%m_%d_%y")) %>%
  mutate(helper = c(
    confirmed[1],
    confirmed[1:(nrow(global_confirmed) - 1)])
  ) %>%
  mutate(increment = confirmed - helper) %>%
  group_by(country_region)

# Deaths

global_deaths <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")) %>% 
  clean_names() %>%
  pivot_longer(cols = -c(province_state, country_region, lat, long), names_to = "date", values_to = "deaths") %>%
  select(country_region, date, deaths)

global_deaths <- global_deaths %>% 
  mutate(sep_date = sub("x", "", date)) %>%
  mutate(new_date = as.Date(sep_date, format = "%m_%d_%y")) %>%
  mutate(helper = c(
    deaths[1],
    deaths[1:(nrow(global_deaths) - 1)])
  ) %>%
  mutate(increment = deaths - helper) %>%
  group_by(country_region)

# Recovered

global_recovered <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")) %>% 
  clean_names() %>%
  pivot_longer(cols = -c(province_state, country_region, lat, long), names_to = "date", values_to = "recovered") %>%
  select(country_region, date, recovered)

global_recovered <- global_recovered %>% 
  mutate(sep_date = sub("x", "", date)) %>%
  mutate(new_date = as.Date(sep_date, format = "%m_%d_%y")) %>%
  mutate(helper = c(
    recovered[1],
    recovered[1:(nrow(global_recovered) - 1)])
  ) %>%
  mutate(increment = recovered - helper) %>%
  group_by(country_region)

# Join JHU data 

covidGlobal <- global_confirmed %>%
  inner_join(
    global_deaths, 
    by = c("country_region", "new_date"), 
    suffix = c("_confirmed", "_deaths")
  ) %>%
  inner_join(
    global_recovered, 
    by = c("country_region", "new_date"), 
    suffix = c("_confirmed", "_recovered")
  ) %>%
  select(
    country_region, 
    new_date, 
    confirmed, 
    increment_confirmed, 
    deaths, 
    increment_deaths, 
    recovered, 
    increment
  ) %>% 
  rename(
    Country = country_region
  )

# Use countrycode package to standardize all country names, for easy joining
# with Oxford data (which comes with CountryCode column)

covidGlobal <- covidGlobal %>% 
  mutate(CountryCode = countrycode(Country, origin = 'country.name', destination = 'iso3c')) %>% 
  filter(Country != "Diamond Princess", Country != "MS Zaandam")

# Diamond Princess, Kosovo, MS Zaandam

stringency <- oxford %>% 
  full_join(covidGlobal, by = c("CountryCode", "new_date")) %>% 
  filter(!is.na(confirmed)) %>% 
  select(
    Country, 
    CountryCode, 
    new_date, 
    S1_School.closing,
    S1_IsGeneral,
    S2_Workplace.closing,
    S2_IsGeneral,
    S3_Cancel.public.events,
    S3_IsGeneral,
    S4_Close.public.transport,
    S4_IsGeneral,
    S5_Public.information.campaigns,
    S5_IsGeneral,
    S6_Restrictions.on.internal.movement,
    S6_IsGeneral,
    S7_International.travel.controls,
    S8_Fiscal.measures,
    S9_Monetary.measures,
    S10_Emergency.investment.in.health.care,
    S11_Investment.in.Vaccines,
    S12_Testing.framework,
    S13_Contact.tracing,
    StringencyIndexForDisplay,
    confirmed,
    increment_confirmed,
    deaths,
    increment_deaths,
    recovered,
    increment
  )


regions <- read.csv(url("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv")) %>% 
  select(name, region, sub.region) %>% 
  rename(Country = name)

stringency_regions <- stringency %>% 
  full_join(regions, by = "Country")

population_data_18 <- read_csv("API_pop.csv", skip = 3) %>% 
  clean_names() %>% 
  select(country_code, x2018) %>% 
  rename(pop_2018 = x2018)

policy <- stringency_regions %>% 
  full_join(population_data_18, by = c("CountryCode" = "country_code")) %>% 
  mutate(confirmed_per_capita = confirmed / pop_2018,
         deaths_per_confirmed = deaths / confirmed,
         recovered_per_confirmed = recovered / confirmed)

policy_log <- policy %>% 
  mutate(log_confirmed = log10(confirmed), 
         log_deaths = log10(deaths),
         log_recovered = log10(recovered))


saveRDS(policy_log, file = "policy.RDS")
