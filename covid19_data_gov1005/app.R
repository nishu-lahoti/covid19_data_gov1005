library(shiny)
library(tidyverse)
library(tidytext)
library(shinythemes)
library(plotly)

# Potentially read in data directly from GitHub Repo. This will automatically update Shiny IF we run a CRONR job for the script
# that creates the RDS files.

covidGlobal <- readRDS("../covid19_data_gov1005/covidGlobal.RDS")
covidUS <- readRDS("../covid19_data_gov1005/covidUS.RDS")
worldometer_data <- readRDS("../covid19_data_gov1005/worldometer.RDS")
tests_per_state <- readRDS("../covid19_data_gov1005/tests_per_state.RDS")
gdp_percapita_cases <- readRDS("../covid19_data_gov1005/gdp_per_capita.RDS")


########### Stock Data Starts Here ##########

# Function to take stock indices from yahoo and scrape data every time its run (updated daily)

stock <- function(url) {
  stock_source <- paste0(url)
  stock_html <- read_html(stock_source)
  stock_data <- stock_html %>% 
    html_nodes("table")
  stock_data <- stock_data[[1]] %>% 
    html_table
  stock_data <- stock_data %>% 
    clean_names() %>% 
    select(date, close)
}


#korea

kospi <- stock("https://finance.yahoo.com/quote/%5EKS11/history?p=%5EKS11") %>% 
  rename(KOSPI = close)
kospi$date <- as.Date(kospi$date, format = "%B %d,%Y") 

#usa

nasdaq <- stock("https://finance.yahoo.com/quote/%5EIXIC/history?p=%5EIXIC") %>% 
  rename(NASDAQ = close)
nasdaq$date <- as.Date(nasdaq$date, format = "%B %d,%Y") 

#world

msci <- stock("https://finance.yahoo.com/quote/MSCI/history?p=MSCI") %>% 
  rename(MSCI = close)
msci$date <- as.Date(msci$date, format = "%B %d,%Y") 

#china

sse_china <- stock("https://finance.yahoo.com/quote/000001.SS/history?p=000001.SS") %>% 
  rename(SSE_China = close)
sse_china$date <- as.Date(sse_china$date, format = "%B %d,%Y") 

#europe as a whole 

stxe600_europe <- stock("https://finance.yahoo.com/quote/%5ESTOXX/history?p=%5ESTOXX") %>% 
  rename(STXE600_Europe = close)
stxe600_europe$date <- as.Date(stxe600_europe$date, format = "%B %d,%Y") 

#italy

ftse_italy <- stock("https://finance.yahoo.com/quote/%5EFTSE%3FP%3DFTSE/history/") %>% 
  rename(FTSE_Italy = close)
ftse_italy$date <- as.Date(ftse_italy$date, format = "%B %d,%Y") 

#spain

ibex_spain <- stock("https://finance.yahoo.com/quote/%5EIBEX/history?p=%5EIBEX") %>% 
  rename(IBEX_Spain = close)
ibex_spain$date <- as.Date(ibex_spain$date, format = "%B %d,%Y") 

#willing to add more countries here. Perhaps France / Germany ? Iran? Singapore? 

stock_data <- kospi %>% 
  left_join(nasdaq, by = "date", na.rm = TRUE) %>% 
  left_join(msci, by = "date", na.rm = TRUE) %>% 
  left_join(sse_china, by = "date", na.rm = TRUE) %>% 
  left_join(stxe600_europe, by = "date", na.rm = TRUE) %>% 
  left_join(ftse_italy, by = "date", na.rm = TRUE) %>% 
  left_join(ibex_spain, by = "date", na.rm = TRUE) 
stock_data$KOSPI <- gsub(',', '', stock_data$KOSPI) %>% as.numeric(stock_data$KOSPI)
stock_data$NASDAQ <- gsub(',', '', stock_data$NASDAQ) %>% as.numeric(stock_data$NASDAQ)
stock_data$MSCI <- gsub(',', '', stock_data$MSCI) %>% as.numeric(stock_data$MSCI)
stock_data$SSE_China <- gsub(',', '', stock_data$SSE_China) %>% as.numeric(stock_data$SSE_China)
stock_data$STXE600_Europe <- gsub(',', '', stock_data$STXE600_Europe) %>% as.numeric(stock_data$STXE600_Europe)
stock_data$FTSE_Italy <- gsub(',', '', stock_data$FTSE_Italy) %>% as.numeric(stock_data$FTSE_Italy)
stock_data$IBEX_Spain <- gsub(',', '', stock_data$IBEX_Spain) %>% as.numeric(stock_data$IBEX_Spain)

covid_global_stock <- covidGlobal %>% 
  left_join(stock_data, by = c("new_date" = "date")) %>% 
  rename(country = "country_region")


########## Shiny App Starts Here ##########

# Define UI for application that draws a histogram
ui <- navbarPage("COVID-19",
                 theme = shinytheme("flatly"),
                 
                 # Add narrative about overall project. One - three paragraphs
                 # Add top two visualizations.
                 
                 tabPanel("About"),
                 
                 # Add narrative about spread data processing. One - three paragraphs.
                 # Add top two visualizations.
                 
                 tabPanel("Spread",
                          h1("Background"),
                          p("The goal of this project is..."),
                          sidebarLayout(
                            sidebarPanel(
                              helpText("Do something interesting with spread data!"),
                              h3("Search something"),
                              selectInput("country_region", NULL,
                                          choices = list("US" = "US",
                                                         "Spain" = "Spain",
                                                         "Italy" = "Italy"))),
                            plotOutput("covidSpread"))),
                          
                # Add narrative about policy. One - three paragraphs.
                # Add top two visualizations.
                
                 tabPanel("Policy",
                          column(6,
                                 h1("How Countries Have Responded with Policy"),
                                 p("More information on Policy..."))),
                
                # Add narrative about economics. One - three paragraphs.
                # Add top two visualizations.
                
                 tabPanel("Economics",
                                 h1("Economic Impact of COVID-19"),
                                 p("More information on Economics..."), 
                                 sidebarLayout(
                                   sidebarPanel(
                                     helpText("Do something interesting with economic data!"),
                                     h3("Search something"),
                                     selectInput("country", NULL,
                                                 choices = list("US" = "US",
                                                                "Spain" = "Spain",
                                                                "Italy" = "Italy"))),
                                   plotOutput("gdpPerCap"))),
                 tabPanel("Team",
                          column(6,
                                 h1("The Team"),
                                 p("Team Backgrounds...")),
                          column(6,
                                 h1("Sources"),
                                 p("Information on Sources")))
)
  
# Define server logic required to draw a histogram
server <- function(input, output) {

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
        ggplot(aes(x = new_date, y = increment_confirmed)) + geom_col()
      
    })
    
    output$gdpPerCap <- renderPlot({
      
      covid_global_stock %>%
        filter(NASDAQ != "NA", country == "USA") %>%
        ggplot(aes(x = log(confirmed))) +
        transition_reveal(new_date) + 
        geom_line(aes(y = NASDAQ), na.rm = TRUE) 
      
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
