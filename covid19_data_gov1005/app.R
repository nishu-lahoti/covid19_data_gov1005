library(shiny)
library(tidyverse)
library(tidytext)
library(shinythemes)
library(plotly)
library(rvest)
library(janitor)
library(countrycode)
library(gganimate)

# Potentially read in data directly from GitHub Repo. This will automatically
# update Shiny IF we run a CRONR job for the script that creates the RDS files.

# For spread:

covidGlobal <- readRDS("../covid19_data_gov1005/covidGlobal.RDS")
covidUS <- readRDS("../covid19_data_gov1005/covidUS.RDS")
worldometer_data <- readRDS("../covid19_data_gov1005/worldometer.RDS")
tests_per_state <- readRDS("../covid19_data_gov1005/tests_per_state.RDS")

# For policy:
policy <- readRDS("../covid19_data_gov1005/policy.RDS")

# For economics:
stock_data <- readRDS("../covid19_data_gov1005/stock_data.RDS")
gdp_percapita_cases <- readRDS("../covid19_data_gov1005/gdp_per_capita.RDS")


######## Stock Data Start Here #########

# Function to take stock indices from yahoo and scrape data every time its run
# (updated daily)

# stock <- function(url) {
#   stock_source <- paste0(url)
#   stock_html <- read_html(stock_source)
#   stock_data <- stock_html %>% 
#     html_nodes("table")
#   stock_data <- stock_data[[1]] %>% 
#     html_table
#   stock_data <- stock_data %>% 
#     clean_names() %>% 
#     select(date, close)
# }
# 
# # Korea
# 
# kospi <- stock("https://finance.yahoo.com/quote/%5EKS11/history?p=%5EKS11") %>% 
#   rename(KOSPI = close)
# kospi$date <- as.Date(kospi$date, format = "%B %d,%Y") 
# 
# # USA
# 
# nasdaq <- stock("https://finance.yahoo.com/quote/%5EIXIC/history?p=%5EIXIC") %>% 
#   rename(NASDAQ = close)
# nasdaq$date <- as.Date(nasdaq$date, format = "%B %d,%Y") 
# 
# # World
# 
# msci <- stock("https://finance.yahoo.com/quote/MSCI/history?p=MSCI") %>% 
#   rename(MSCI = close)
# msci$date <- as.Date(msci$date, format = "%B %d,%Y") 
# 
# # China
# 
# sse_china <- stock("https://finance.yahoo.com/quote/000001.SS/history?p=000001.SS") %>% 
#   rename(SSE_China = close)
# sse_china$date <- as.Date(sse_china$date, format = "%B %d,%Y") 
# 
# # Europe as a whole
# 
# dax <- stock("https://finance.yahoo.com/quote/%5EGDAXI/history?p=%5EGDAXI") %>% 
#   rename(DAX = close)
# dax$date <- as.Date(dax$date, format = "%B %d,%Y") 
# 
# # Italy
# 
# ftse_italy <- stock("https://finance.yahoo.com/quote/%5EFTSE%3FP%3DFTSE/history/") %>% 
#   rename(FTSE_Italy = close)
# ftse_italy$date <- as.Date(ftse_italy$date, format = "%B %d,%Y") 
# 
# # Spain
# 
# ibex_spain <- stock("https://finance.yahoo.com/quote/%5EIBEX/history?p=%5EIBEX") %>% 
#   rename(IBEX_Spain = close)
# ibex_spain$date <- as.Date(ibex_spain$date, format = "%B %d,%Y") 
# 
# # Willing to add more countries here. Perhaps France/Germany? Iran? Singapore? 
# 
# stock_data <- kospi %>% 
#   left_join(nasdaq, by = "date", na.rm = TRUE) %>% 
#   left_join(msci, by = "date", na.rm = TRUE) %>% 
#   left_join(sse_china, by = "date", na.rm = TRUE) %>% 
#   left_join(dax, by = "date", na.rm = TRUE) %>% 
#   left_join(ftse_italy, by = "date", na.rm = TRUE) %>% 
#   left_join(ibex_spain, by = "date", na.rm = TRUE) 
# stock_data$KOSPI <- gsub(',', '', stock_data$KOSPI) %>% as.numeric(stock_data$KOSPI)
# stock_data$NASDAQ <- gsub(',', '', stock_data$NASDAQ) %>% as.numeric(stock_data$NASDAQ)
# stock_data$MSCI <- gsub(',', '', stock_data$MSCI) %>% as.numeric(stock_data$MSCI)
# stock_data$SSE_China <- gsub(',', '', stock_data$SSE_China) %>% as.numeric(stock_data$SSE_China)
# stock_data$DAX <- gsub(',', '', stock_data$DAX) %>% as.numeric(stock_data$DAX)
# stock_data$FTSE_Italy <- gsub(',', '', stock_data$FTSE_Italy) %>% as.numeric(stock_data$FTSE_Italy)
# stock_data$IBEX_Spain <- gsub(',', '', stock_data$IBEX_Spain) %>% as.numeric(stock_data$IBEX_Spain)
# 
# covid_global_stock <- covidGlobal %>% 
#   full_join(stock_data, by = c("new_date" = "date")) %>% 
#   select(Country, new_date, confirmed, deaths, recovered, KOSPI, NASDAQ, MSCI, SSE_China, DAX, FTSE_Italy, IBEX_Spain)
# 




########## Shiny App Starts Here ##########

# Define UI
ui <- navbarPage("The COVID-19 Data Project",
                 theme = shinytheme("flatly"),
                 
                 tabPanel("About",
                          column(8,
                                 p("Over the past months, the spread of COVID-19 has upended the world, 
                           with almost 3 million confirmed cases and 200,000 deaths reported to date. 
                           Cities, states, and entire nations have shut down, as travel-restrictions 
                           and stay-at-home orders have come into effect. In the U.S. alone, more than
                           26 million citizens have filed for unemployment, with jobless claim filings
                           growing at a historically unprecedented pace. As politicians, researchers, 
                           lawyers, and other experts scramble for answers, uncertainty about when the
                           world can reopen and begin on a road-to-recovery has only continued to grow."),
                                 br(),
                                 p("The purpose of this project is to analyze the spread and impact of the 
                           coronavirus pandemic and to understand the efficacy of various policies 
                           enacted around the globe in mitigating the crisis. We seek to shed light 
                           on many different aspects of the pandemic using data visualization and 
                           statistical analysis, as well as provide commentary on the data we have. 
                           This site is organized into three parts:"),
                                 br(),
                                 h4(em("Spread:")),
                                 p("COVID-19 has spread more rapidly than most politicians and people, in general, expected.
                                 In some countries, the total number of confirmed cases and deaths exponentially increased
                                 over the early months of 2020. In other countries, the total number of confirmed cases is uncertain
                                 due to a lack of testing. In “Spread”, we explore how COVID-19 spread in countries across the world and
                                 the correlation with each country's ability to test."),
                                 br(),
                                 h4(em("Policy:")),
                                 p("Governments around the world have implemented a variety of measures to respond to 
                            the threat and spread of COVID-19, ranging from mild to severe. Such measures include schools and workplaces closing, 
                            international travel controls, and emergency investment in healthcare and vaccines. 
                            In “Policy”, we explore the stringency of common policy responses governments have taken 
                            and compare these responses across countries and regions."),
                                 br(),
                                 h4(em("Economic Impact:")))
                 ),
                 
                 
                 # Add top two visualizations.
                 
                 
                 tabPanel("Spread",
                          h1("Visualizing the rate of spread across countries"),
                          p("The rapidity with which COVID-19 spread within countries surprised political leaders
                            and society, as a whole. In the early months of 2020, most global citizens believed
                            the Coronavirus to be a problem distant to their daily life, sequestered to a manufacturing
                            city in the heart of China. As the virus began spreading outside of Wuhan, society-at-large continued
                            to believe it could be contained and would not impact daily life. The aim of the visualizations below
                            are to dispel that belief, showing just how quickly Coronavirus has spread in countries across the world,
                            it's devastation, and the eventual pathway to recovery."),
                          # sidebarLayout(
                            # sidebarPanel(
                              h3("Select a country to see how the arc of COVID-19 cases and deaths"),
                              selectInput("country_region", NULL,
                                          choices = c("Afghanistan",				
                                                      "Angola",			
                                                      "Albania",
                                                      "Algeria",	
                                                      "Andorra",				
                                                      "Argentina",		
                                                      "Australia",		
                                                      "Austria",				
                                                      "Azerbaijan",			
                                                      "Burundi",
                                                      "Belgium",				
                                                      "Burkina Faso",
                                                      "Burma",		
                                                      "Bangladesh",			
                                                      "Bulgaria",			
                                                      "Bahrain",				
                                                      "Bosnia and Herzegovina",				
                                                      "Belize",			
                                                      "Bolivia",				
                                                      "Brazil",				
                                                      "Barbados",
                                                      "Brunei",			
                                                      "Botswana",				
                                                      "Canada",			
                                                      "Chile",				
                                                      "China",				
                                                      "Cameroon",				
                                                      "Congo (Kinshasa)",				
                                                      "Colombia",				
                                                      "Costa Rica",
                                                      "Cuba",				
                                                      "Cyprus",				
                                                      "Czechia",				
                                                      "Germany",				
                                                      "Djibouti",				
                                                      "Dominica",				
                                                      "Denmark",				
                                                      "Dominican Republic",				
                                                      "Ecuador",
                                                      "Egypt",				
                                                      "Spain",				
                                                      "Estonia",				
                                                      "Ethiopia",				
                                                      "Finland",				
                                                      "France",				
                                                      "Gabon",				
                                                      "Ghana",				
                                                      "Gambia",
                                                      "Greece",				
                                                      "Guatemala",				
                                                      "Guyana",				
                                                      "Honduras",				
                                                      "Croatia",				
                                                      "Hungary",				
                                                      "Indonesia",				
                                                      "India",				
                                                      "Ireland",				
                                                      "Iran",
                                                      "Iraq",				
                                                      "Iceland",			
                                                      "Israel",				
                                                      "Italy",				
                                                      "Jamaica",				
                                                      "Jordan",				
                                                      "Japan",				
                                                      "Kazakhstan",				
                                                      "Kenya",				
                                                      "Kyrgyzstan",
                                                      "Korea, South",				
                                                      "Kuwait",				
                                                      "Laos",				
                                                      "Lebanon",				
                                                      "Libya",				
                                                      "Sri Lanka",				
                                                      "Luxembourg",			
                                                      "Morocco",				
                                                      "Moldova",				
                                                      "Madagascar",
                                                      "Mexico",				
                                                      "Mali",				
                                                      "Mongolia",				
                                                      "Mozambique",			
                                                      "Mauritania",			
                                                      "Mauritius",				
                                                      "Malawi",				
                                                      "Malaysia",				
                                                      "Namibia",
                                                      "Niger",				
                                                      "Nigeria",				
                                                      "Nicaragua",				
                                                      "Netherlands",				
                                                      "Norway",				
                                                      "New Zealand",				
                                                      "Oman",				
                                                      "Pakistan",			
                                                      "Panama",			
                                                      "Peru",
                                                      "Philippines",				
                                                      "Papua New Guinea",			
                                                      "Poland",				
                                                      "Portugal",				
                                                      "Paraguay",				
                                                      "West Bank and Gaza",			
                                                      "Qatar",				
                                                      "Romania",				
                                                      "Russia",				
                                                      "Rwanda",
                                                      "Saudi Arabia",			
                                                      "Sudan",				
                                                      "Singapore",				
                                                      "Sierra Leone",				
                                                      "El Salvador",				
                                                      "San Marino",				
                                                      "Serbia",				
                                                      "South Sudan",				
                                                      "Slovakia",				
                                                      "Slovenia",
                                                      "Sweden",
                                                      "Switzerland",	
                                                      "Eswatini",				
                                                      "Seychelles",				
                                                      "Syria",				
                                                      "Chad",				
                                                      "Thailand",				
                                                      "Trinidad and Tobago",				
                                                      "Tunisia",				
                                                      "Turkey",				
                                                      "Tanzania",				
                                                      "Uganda",				
                                                      "Ukraine",	
                                                      "United Kingdom",
                                                      "United Arab Emirates",
                                                      "Uruguay",			
                                                      "US",			
                                                      "Uzbekistan",				
                                                      "Venezuela",				
                                                      "Vietnam",				
                                                      "South Africa",				
                                                      "Zambia",				
                                                      "Zimbabwe",
                                                      "Taiwan*"	)),
                            textOutput("selected_var"),
                            plotOutput("covidSpread"),
                            plotOutput("covidDeaths"),
                          br(),
                          p("We aimed to understand if there was bias in the reporting of new COVID-19 cases.
                            Specifically, how did a country's capacity to test influence it's total case reporting?
                            We suspected that countries with a greater ability to test would report a greater number of cases,
                            and vice versa."),
                          plotOutput("covidHighTests"),
                          plotOutput("covidMTests"),
                          plotOutput("covidLogTests"),
                          plotOutput("covidLogMTests")),
                 
                 # Add narrative about policy. One - three paragraphs.
                 # Add top two visualizations.
                 
                 tabPanel("Policy",
                          h1("How Countries Have Responded with Policy"),
                          p("More information on Policy..."),
                          sidebarLayout(
                            sidebarPanel(
                              helpText("Look at country-specific policy"),
                              selectInput("indexInput", "Select a policy response",
                                          choices = c("School closing", "Workplace closing", "Cancel public events", 
                                                      "Public transport closings", "Public info campaigns", 
                                                      "Restrictions on internal movement", "International travel controls", 
                                                      # "Fiscal measures", "Monetary measures", "Emergency investment in healthcare",
                                                      # "Investment in vaccines", 
                                                      "Testing policy", "Contact tracing")),
                              selectInput("countryInput", "Select a country",
                                          choices = c("Afghanistan",				
                                                      "Angola",			
                                                      "Albania",
                                                      "Algeria",	
                                                      "Andorra",				
                                                      "Argentina",		
                                                      "Australia",		
                                                      "Austria",				
                                                      "Azerbaijan",			
                                                      "Burundi",
                                                      "Belgium",				
                                                      "Burkina Faso",
                                                      "Burma",		
                                                      "Bangladesh",			
                                                      "Bulgaria",			
                                                      "Bahrain",				
                                                      "Bosnia and Herzegovina",				
                                                      "Belize",			
                                                      "Bolivia",				
                                                      "Brazil",				
                                                      "Barbados",
                                                      "Brunei",			
                                                      "Botswana",				
                                                      "Canada",			
                                                      "Chile",				
                                                      "China",				
                                                      "Cameroon",				
                                                      "Congo (Kinshasa)",				
                                                      "Colombia",				
                                                      "Costa Rica",
                                                      "Cuba",				
                                                      "Cyprus",				
                                                      "Czechia",				
                                                      "Germany",				
                                                      "Djibouti",				
                                                      "Dominica",				
                                                      "Denmark",				
                                                      "Dominican Republic",				
                                                      "Ecuador",
                                                      "Egypt",				
                                                      "Spain",				
                                                      "Estonia",				
                                                      "Ethiopia",				
                                                      "Finland",				
                                                      "France",				
                                                      "Gabon",				
                                                      "Ghana",				
                                                      "Gambia",
                                                      "Greece",				
                                                      "Guatemala",				
                                                      "Guyana",				
                                                      "Honduras",				
                                                      "Croatia",				
                                                      "Hungary",				
                                                      "Indonesia",				
                                                      "India",				
                                                      "Ireland",				
                                                      "Iran",
                                                      "Iraq",				
                                                      "Iceland",			
                                                      "Israel",				
                                                      "Italy",				
                                                      "Jamaica",				
                                                      "Jordan",				
                                                      "Japan",				
                                                      "Kazakhstan",				
                                                      "Kenya",				
                                                      "Kyrgyzstan",
                                                      "Korea, South",				
                                                      "Kuwait",				
                                                      "Laos",				
                                                      "Lebanon",				
                                                      "Libya",				
                                                      "Sri Lanka",				
                                                      "Luxembourg",			
                                                      "Morocco",				
                                                      "Moldova",				
                                                      "Madagascar",
                                                      "Mexico",				
                                                      "Mali",				
                                                      "Mongolia",				
                                                      "Mozambique",			
                                                      "Mauritania",			
                                                      "Mauritius",				
                                                      "Malawi",				
                                                      "Malaysia",				
                                                      "Namibia",
                                                      "Niger",				
                                                      "Nigeria",				
                                                      "Nicaragua",				
                                                      "Netherlands",				
                                                      "Norway",				
                                                      "New Zealand",				
                                                      "Oman",				
                                                      "Pakistan",			
                                                      "Panama",			
                                                      "Peru",
                                                      "Philippines",				
                                                      "Papua New Guinea",			
                                                      "Poland",				
                                                      "Portugal",				
                                                      "Paraguay",				
                                                      "West Bank and Gaza",			
                                                      "Qatar",				
                                                      "Romania",				
                                                      "Russia",				
                                                      "Rwanda",
                                                      "Saudi Arabia",			
                                                      "Sudan",				
                                                      "Singapore",				
                                                      "Sierra Leone",				
                                                      "El Salvador",				
                                                      "San Marino",				
                                                      "Serbia",				
                                                      "South Sudan",				
                                                      "Slovakia",				
                                                      "Slovenia",
                                                      "Sweden",
                                                      "Switzerland",	
                                                      "Eswatini",				
                                                      "Seychelles",				
                                                      "Syria",				
                                                      "Chad",				
                                                      "Thailand",				
                                                      "Trinidad and Tobago",				
                                                      "Tunisia",				
                                                      "Turkey",				
                                                      "Tanzania",				
                                                      "Uganda",				
                                                      "Ukraine",	
                                                      "United Kingdom",
                                                      "United Arab Emirates",
                                                      "Uruguay",			
                                                      "US",			
                                                      "Uzbekistan",				
                                                      "Venezuela",				
                                                      "Vietnam",				
                                                      "South Africa",				
                                                      "Zambia",				
                                                      "Zimbabwe",
                                                      "Taiwan*"	))
                            ),
                            mainPanel(plotOutput("countryPolicy"))),
                          
                          sidebarLayout(
                            sidebarPanel(
                              helpText("Compare global policy"),
                              sliderInput("dateInput",
                                          "Select a date:",
                                          min = as.Date("2020-01-22","%Y-%m-%d"),
                                          max = Sys.Date(),
                                          value = as.Date("2016-01-22"),
                                          timeFormat = "%Y-%m-%d"),
                              radioButtons("caseInput", "Choose a case type",
                                           choices = c("Confirmed", "Deaths", "Recovered"),
                                           selected = "Confirmed")
                              
                            ),
                            mainPanel(plotOutput("globalPolicy")))),
                 
                 
                 # Add narrative about policy. One - three paragraphs.
                 
                 tabPanel("Economic Impact",
                          h1("Economic Impact of COVID-19"),
                          p("More information on Economics..."),
                          sidebarLayout(
                            sidebarPanel(
                              helpText("Look at COVID-19's impact on stock prices for select countries"),
                              selectInput("countryInput", "Select a Country: ",
                                          choices = c("China", 
                                                      "Germany", 
                                                      "Italy", 
                                                      "South Korea", 
                                                      "Spain", 
                                                      "United States")),
                              radioButtons("caseInput", "Choose a case type",
                                           choices = c("Confirmed", "Deaths", "Recovered"),
                                           selected = "Confirmed")
                            ),
                            mainPanel(plotOutput("stock_impact"))),
                          
                          sidebarLayout(
                            sidebarPanel(
                              helpText("Compare COVID-19's impact among countries of different GDP levels"),
                              radioButtons("caseInput", "Choose a case type",
                                           choices = c("Confirmed", "Deaths", "Recovered"),
                                           selected = "Confirmed")
                              
                            ),
                            mainPanel(plotOutput("gdp_cases")))),
                 
                 tabPanel("Team",
                          column(6,
                                 h1("The Team"),
                                 h4("Rebecca Xi"),
                                 p("is a sophomore at Harvard concentrating 
                                   in Applied Math & Economics with a secondary in Government. 
                                   Her github can be found", a(href =  "https://github.com/beccaxi",
                                                               "here.")),
                                 h4("Katelyn Li"),
                                 p("is a junior at Harvard concentrating 
                                   in Neuroscience with a secondary in Government. 
                                   Her github can be found", a(href =  "https://github.com/katelynxli",
                                                               "here.")),
                                 h4("Nishu Lahoti"),
                                 p("is a Masters student between Harvard's
                                   Graduate School of Design and Engineering. His github
                                   can be found", a(href = "https://github.com/nishu-lahoti", "here.")),
                                
                                h4("Jun-Yong Kim"),
                                p("is a first-year at Harvard prospectively 
                                   concentrating in Statistics and Sociology. His github can
                                   be found", a(href = "https://github.com/juniyong247", "here."))),
                          column(6,
                                 h1("Data Sources"),
                                 h4(a(href = "https://github.com/CSSEGISandData/COVID-19",
                                      "2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE")),
                                 p("This is the data repository for the 2019 Novel Coronavirus Visual Dashboard operated
                                 by the Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)."),
                                 h4(a(href = "https://github.com/nytimes/covid-19-data",
                                      "NYTimes Coronavirus (Covid-19) Data in the United States")),
                                 p("The New York Times is releasing a series of data files with cumulative counts of 
                                   coronavirus cases in the United States, at the state and county level, over time."),
                                 h4(a(href = "https://www.worldometers.info/coronavirus/",
                                      "Worldometer COVID-19 Data")),
                                 p("Worldometer manually analyzes, validates, and aggregates data from thousands of sources 
                                 in real time and provides global COVID-19 live statistics for a wide audience of caring people around the world."),
                                 h4(a(href = "https://finance.yahoo.com",
                                      "Yahoo Finance Daily Stock Indices Database")),
                                 p("Yahoo Finance tracks the daily prices of different stock indices from all around the world."), 
                                 h4(a(href = "https://github.com/OxCGRT/covid-policy-tracker",
                                      "Oxford Covid-19 Government Response Tracker")),
                                 p("The OxCGRT collects systematic information on which governments have taken which measures, and when, 
                                 scoring the stringency of such measures and aggregating these scores into a common Stringency Index."),
                                 h4(a(href = "https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv",
                                      "ISO-3166 Country and Dependent Territories Lists with UN Regional Codes")),
                                 p("These lists are the result of merging data from two sources, the Wikipedia ISO 3166-1 article for alpha and numeric
                                   country codes, and the UN Statistics site for countries' regional, and sub-regional codes.")
                          )
                 )
)


# Define server logic
server <- function(input, output) {
  
  # Spread
  
  output$selected_var <- renderText({
    paste(input$country_region)
  })
  
  output$worldometer_log <- renderPlot({
    
    worldometer_log <- worldometer_data %>%
      mutate(log_cases = log(total_cases),
             log_deaths = log(total_deaths),
             log_recovered = log(total_recovered),
             log_tests = log(total_tests),
             log_tests_1m = log(tests_1m_pop))
    
    ggplot(worldometer_log, aes(log_cases, log_tests_1m, color = country_other)) +
      geom_point() +
      theme(legend.position = "none") +
      labs(
        title = "Logarithmic comparison of cases to tests",
        x = "Cases \n(x10,000)",
        y = "Tests per 1M \n(x10,000)"
      )
    
  })
  
  output$covidSpread <- renderPlot({
    
    covidGlobal %>%
      filter(country_region == input$country_region,
             increment_confirmed >= 0) %>%
      ggplot(aes(x = new_date, y = increment_confirmed)) + 
      geom_col() +
      labs(
        title = "Incremental Cases of COVID-19\nmeasured over time",
        x = "Date",
        y = "New Daily Cases"
      )
    
  })
  
  output$covidDeaths <- renderPlot({
    
    covidGlobal %>%
      filter(country_region == input$country_region,
             increment_confirmed >= 0) %>%
      ggplot(aes(x = new_date, y = increment_deaths)) + 
      geom_col() +
      labs(
        title = "Incremental Deaths of COVID-19\nmeasured over time",
        x = "Date",
        y = "New Daily Deaths"
      )
    
  })
  
options(scipen = 999)
  
# Normal
worldometer_tests <- worldometer %>%
  filter(total_cases >= 15000, 
          !is.na(total_tests))
  
# Logarithmic
  
worldometer_log <- worldometer %>%
  mutate(log_cases = log(total_cases),
          log_deaths = log(total_deaths),
          log_recovered = log(total_recovered),
          log_tests = log(total_tests),
          log_tests_1m = log(tests_1m_pop))
  
  output$covidHighTests <- renderPlot({
  # Visualizing total cases and total deaths against total tests. A good next step may be to filter by countries of interest and to get a good enough
  # sample of countries that have tested. Qualify a country based on total number of cases (>1000). Maybe there is a weak positive correlation.
  
  
  ggplot(worldometer_tests, aes(total_cases, total_tests, color = country_other)) + 
    geom_point() +
    geom_jitter() +
    theme_classic() +
    theme(legend.position = "top") +
    labs(
      title = "Comparing COVID-19 Cases versus Total Tests",
      subtitle = "Comparing total conducted tests \nfor countries with over 15,000 reported cases.",
      x = "Total Cases",
      y = "Tests per 1M",
      color = "Country"
    )
  
  })

output$covidMTests <- renderPlot({

  ggplot(worldometer_tests, aes(total_cases, tests_1m_pop, color = country_other)) + 
    geom_point() +
    geom_jitter() +
    theme_classic() +
    theme(legend.position = "top") +
    labs(
      title = "COVID-19 Country Testing Capacity",
      subtitle = "Visualizing a country's case rate against testing rate\nfor countries with over 15,000 reported cases.",
      x = "Total Cases",
      y = "Tests per 1M",
      color = "Country"
    )
  
})
  

output$covidLogTests <- renderPlot({
  # Logarithmic plot of total tests
  
  ggplot(worldometer_log, aes(log_cases, log_tests, color = country_other)) +
    geom_point() +
    theme(legend.position = "none") +
    labs(
      title = "Logarithmic comparison of cases to tests",
      x = "Cases \n(x10,000)",
      y = "Tests \n(x10,000)"
    )
})
  
output$covidLogMTests <- renderPlot({
  # Logarithmic plot of tests per 1m
  
  ggplot(worldometer_log, aes(log_cases, log_tests_1m, color = country_other)) +
    geom_point() +
    theme(legend.position = "none") +
    labs(
      title = "Logarithmic comparison of cases to tests",
      x = "Cases \n(x10,000)",
      y = "Tests per 1M \n(x10,000)"
    )
  })
  
  # Policy
  
  output$countryPolicy <- renderPlot({
    
    if(input$indexInput == "School closing") {
      color <- policy %>% 
        filter(CountryCode == "USA") %>% 
        filter(new_date >= "2020-02-01") %>% 
        pull(S1_School.closing)
      subtitle <- "Policy response: Closings of schools and universities"
      breaks <- c("0", "1", "2")
      labels <- c("No measures", "Recommend closing", "Require closing")
      values <- c("green", "yellow", "red")
    } 
    else if(input$indexInput == "Workplace closing") {
      color <- policy %>% 
        filter(CountryCode == "USA") %>% 
        filter(new_date >= "2020-02-01") %>% 
        pull(S2_Workplace.closing)
      subtitle <- "Policy response: Closings of workplaces"
      breaks <- c("0", "1", "2")
      labels <- c("No measures", "Recommend closing", "Require closing")
      values <- c("green", "yellow", "red")
    } 
    else if(input$indexInput == "Cancel public events") {
      color <- policy %>% 
        filter(CountryCode == "USA") %>% 
        filter(new_date >= "2020-02-01") %>% 
        pull(S3_Cancel.public.events)
      subtitle <- "Policy response: Cancelling public events"
      breaks <- c("0", "1", "2")
      labels <- c("No measures", "Recommend cancelling", "Require cancelling")
      values <- c("green", "yellow", "red")
    }
    else if(input$indexInput == "Public transport closings") {
      color <- policy %>% 
        filter(CountryCode == "USA") %>% 
        filter(new_date >= "2020-02-01") %>% 
        pull(S4_Close.public.transport)
      subtitle <- "Policy response: Closing of public transport"
      breaks <- c("0", "1", "2")
      labels <- c("No measures", "Recommend closing", "Require closing")
      values <- c("green", "yellow", "red")
    }
    else if(input$indexInput == "Public info campaigns") {
      color <- policy %>% 
        filter(CountryCode == "USA") %>% 
        filter(new_date >= "2020-02-01") %>% 
        pull(S5_Public.information.campaigns)
      subtitle <- "Policy response: Presence of public info campaigns"
      breaks <- c("0", "1")
      labels <- c("No COVID-19 public information campaign", "COVID-19 public information campaign")
      values <- c("red", "green")
    }
    else if(input$indexInput == "Restrictions on internal movement") {
      color <- policy %>% 
        filter(CountryCode == "USA") %>% 
        filter(new_date >= "2020-02-01") %>% 
        pull(S6_Restrictions.on.internal.movement)
      subtitle <- "Policy response: Restrictions on internal movement"
      breaks <- c("0", "1", "2")
      labels <- c("No measures", "Recommend movement restriction", "Restrict movement")
      values <- c("green", "yellow", "red")
    }
    else if(input$indexInput == "International travel controls") {
      color <- policy %>% 
        filter(CountryCode == "USA") %>% 
        filter(new_date >= "2020-02-01") %>% 
        pull(S7_International.travel.controls)
      subtitle <- "Policy response: Restrictions on international travel"
      breaks <- c("0", "1", "2", "3")
      labels <- c("No measures", "Screening", "Quarantine on high-risk regions", "Ban on high-risk regions")
      values <- c("green", "yellow", "orange", "red")
    }
    else if(input$indexInput == "Fiscal measures") {
      color <- policy %>% 
        filter(CountryCode == "USA") %>% 
        filter(new_date >= "2020-02-01") %>% 
        pull(S8_Fiscal.measures)
      subtitle <- "Policy response: Value of fiscal stimuli (in USD)"
    }
    else if(input$indexInput == "Monetary measures") {
      color <- policy %>% 
        filter(CountryCode == "USA") %>% 
        filter(new_date >= "2020-02-01") %>% 
        pull(S9_Monetary.measures)
      subtitle <- "Policy response: Monetary measures (Value of interest rate, in %)"
    }
    else if(input$indexInput == "Emergency investment in healthcare") {
      color <- policy %>% 
        filter(CountryCode == "USA") %>% 
        filter(new_date >= "2020-02-01") %>% 
        pull(S10_Emergency.investment.in.health.care)
      subtitle <- "Policy response: Emergency investment in healthcare (in USD)"
    }
    else if(input$indexInput == "Investment in vaccines") {
      color <- policy %>% 
        filter(CountryCode == "USA") %>% 
        filter(new_date >= "2020-02-01") %>% 
        pull(S11_Investment.in.Vaccines)
      subtitle <- "Policy response: Investment in vaccines (in USD)"
    }
    else if(input$indexInput == "Testing policy") {
      color <- policy %>% 
        filter(CountryCode == "USA") %>% 
        filter(new_date >= "2020-02-01") %>% 
        pull(S12_Testing.framework)
      subtitle <- "Policy response: Testing policy"
      breaks <- c("0", "1", "2", "3")
      labels <- c("No testing policy", 
                  "only testing those who have symptoms and meet specific criteria", 
                  "testing of anyone showing COVID-19 symptoms",
                  "open public testing")
      values <- c("red", "orange", "yellow", "green")
    }
    else if(input$indexInput == "Contact tracing") {
      color <- policy %>% 
        filter(CountryCode == "USA") %>% 
        filter(new_date >= "2020-02-01") %>% 
        pull(S13_Contact.tracing)
      subtitle <- "Policy response: Contact tracing"
      breaks <- c("0", "1")
      labels <- c("No contact tracing", 
                  "Limited contact tracing")
      values <- c("red", "green")
    }
    else {
      color <- policy %>% 
        filter(CountryCode == "USA") %>% 
        filter(new_date >= "2020-02-01") %>% 
        pull(StringencyIndexForDisplay)
      subtitle <- "Stringency Index"
    } 
    
    
    # Create plot for one country's response
    
    
    policy %>%  
      filter(CountryCode == "input$countryInput") %>% 
      filter(new_date >= "2020-02-01") %>%
      ggplot(aes(x = new_date, color = as.factor(color))) +
      geom_line(aes(y = log_confirmed), linetype = "solid" ) +
      geom_line(aes(y = log_deaths), linetype = "dashed") +
      geom_line(aes(y = log_recovered), linetype = "dotted") +
      scale_color_manual(
        breaks = breaks,
        labels = labels,
        values = values
      ) +
      labs(
        title = "Spread and Stringency Response in USA, February Onward",
        subtitle = subtitle,
        x = "Time",
        y = "Count (log transformed)",
        color = "Stringency"
      ) +
      scale_x_date(
        date_breaks = "1 week", 
        date_labels = "%b %d"
      ) +
      theme_classic() 
    
  })
  
  # Create scatterplot for global response
  
  output$globalPolicy <- renderPlot({
    
    if(input$caseInput == "Confirmed") {
      x_value <- policy %>% 
        filter(new_date == input$dateInput) %>% 
        pull(log_confirmed)
      x_axis <- "Confirmed cases (log transformed)"
    }
    else if(input$caseInput == "Recovered") {
      x_value <- policy %>% 
        filter(new_date == input$dateInput) %>% 
        pull(log_recovered)
      x_axis <- "Recoveries (log transformed)"
    }
    else{
      x_value <- policy %>% 
        filter(new_date == input$dateInput) %>% 
        pull(log_deaths)
      x_axis <- "Deaths (log transformed)"
    }
    
    policy %>% 
      filter(new_date == input$dateInput) %>% 
      ggplot(aes(x = x_value, y = StringencyIndexForDisplay, label = CountryCode, color = sub.region)) +
      geom_point() +
      geom_text() +
      labs(
        title = "Relationship between Number of Cases and Government Response",
        subtitle = "Overall Stringency Index",
        x = x_axis,
        y = "Stringency Index",
        color = "Region"
      ) +
      theme_classic()
    
  })
  
}
  
  # Economic Impact
  
#   output$stock_impact <- renderPlot({
#     
#     if(input$countryInput == "China") {
#       y_value <- covid_global_stock %>% 
#         filter(Country == "CHN") %>% 
#         pull(SSE_China)
#       y_axis <- "Index: SSE"
#       subtitle <- "In China"
#     }
#    else if(input$countryInput == "Germany") {
#       y_value <- covid_global_stock %>% 
#         filter(Country == "GER") %>% 
#         pull(DAX)
#       y_axis <- "Index: DAX"
#       subtitle <- "In Germany"
#     }
#     else if(input$countryInput == "Italy") {
#       y_value <- covid_global_stock %>% 
#         filter(Country == "ITA") %>% 
#         pull(FTSE_Italy)
#       y_axis <- "Index: FTSE"
#       subtitle <- "In Italy"
#     }
#     else if(input$countryInput == "South Korea") {
#       y_value <- covid_global_stock %>% 
#         filter(Country == "KOR") %>% 
#         pull(KOSPI)
#       y_axis <- "Index: KOSPI"
#       subtitle <- "In South Korea"
#     }
#     else if(input$countryInput == "Spain") {
#       y_value <- covid_global_stock %>% 
#         filter(Country == "SPA") %>% 
#         pull(IBEX_Spain)
#       y_axis <- "Index: IBEX"
#       subtitle <- "In Spain"
#     }
#     else {
#       y_value <- covid_global_stock %>% 
#         filter(Country == "USA") %>% 
#         pull(NASDAQ)
#       y_axis <- "Index: NASDAQ"
#       subtitle <- "In the United States"
#     } 
#     
#     
#     if(input$caseInput == "Confirmed") {
#       x_value <- covid_global_stock %>% 
#         pull(confirmed)
#       x_axis <- "Number of Confirmed Cases (log transformed)"
#       title <- "Impact of Confirmed Cases on Stock Indices"
#     }
#     else if(input$caseInput == "Recovered") {
#       x_value <- covid_global_stock %>% 
#         pull(recovered)
#       x_axis <- "Number of Recovered Cases (log transformed)"
#       title <- "Impact of Recovered Cases on Stock Indices"
#     }
#     else{
#       x_value <- covid_global_stock %>% 
#         pull(deaths)
#       x_axis <- "Number of Deaths (log transformed)"
#       title <- "Impact of Number of Deaths on Stock Indices"
#     }
#     # Create plot for one country's response
#     
#     stock_data %>%  
#       filter(y_value != "NA", x_value != "0") %>%
#       ggplot(aes(x = log(x_value))) +
#       geom_line(aes(y = y_value), linetype = "solid") +
#       labs(
#         title = title,
#         subtitle = subtitle,
#         x = x_axis,
#         y = y_axis
#       ) + 
#       transition_reveal(new_date) + 
#       theme_classic() 
#     
#   })
#   
#   output$gdp_cases <- renderPlot({
#     
#     worldometer_data$country_other <- countrycode(worldometer_data$country_other, origin = "country.name", destination = "iso3c", warn = FALSE)
#     tidy_gdp_pop <- gdp_percapita_cases %>% 
#       left_join(worldometer_data, by = c("country_code" = "country_other")) %>% 
#       select(country_code, pop_2018, gdp_2018, gdp_per_capita, total_cases, total_deaths, total_recovered) %>% 
#       na.omit() 
#              
#     if(input$caseInput == "Confirmed") {
#       y_value <- tidy_gdp_pop$log_cases
#       y_axis <- "Total Confirmed Cases (log transformed)"
#     }
#     else if(input$caseInput == "Recovered") {
#       y_value <- tidy_gdp_pop$log_deaths
#       y_axis <- "Total Recovered Cases (log transformed)"
#     }
#     else{
#       y_value <- tidy_gdp_pop$log_recovered
#       y_axis <- "Total Deaths (log transformed)"
#     }
#     
#     
#     tidy_gdp_pop %>% 
#       ggplot(aes(x = log(gdp_per_capita), y = y_value, label = country_code)) +
#       geom_point() +
#       geom_text() +
#       labs(
#         title = "Relationship between GDP Per Capita and the Number of Cases",
#         subtitle = "By Type of Case",
#         x = "GDP Per Capita (log transformed)",
#         y = y_axis
#       ) +
#       theme_classic()
#   
# })
#   
#   }

# Run the application 
shinyApp(ui = ui, server = server)

