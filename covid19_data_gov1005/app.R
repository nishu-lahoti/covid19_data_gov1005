library(shiny)
library(tidyverse)
library(tidytext)
library(shinythemes)
library(plotly)

# Potentially read in data directly from GitHub Repo. This will automatically update Shiny IF we run a CRONR job for the script
# that creates the RDS files.

covidGlobal <- readRDS("../team_data/covidGlobal.RDS")
covidUS <- readRDS("../team_data/covidUS.RDS")
worldometer_data <- readRDS("../team_data/worldometer.RDS")
tests_per_state <- readRDS("../team_data/tests_per_state.RDS")


# Define UI for application that draws a histogram
ui <- navbarPage("COVID-19",
                 theme = shinytheme("flatly"),
                 
                 # Add narrative about overall project. One - three paragraphs
                 # Add top two visualizations.
                 
                 tabPanel("About",
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
                 
                 # Add narrative about spread data processing. One - three paragraphs.
                 # Add top two visualizations.
                 
                 tabPanel("Spread",
                          column(6,
                                 h1("Information on COVID-19 Spread"),
                                 p("More information on Spread...")),
                          column(6,
                                 plotOutput("worldometer_log")),
                          
                # Add narrative about policy. One - three paragraphs.
                # Add top two visualizations.
                
                 tabPanel("Policy",
                          column(6,
                                 h1("How Countries Have Responded with Policy"),
                                 p("More information on Policy..."))),
                
                # Add narrative about economics. One - three paragraphs.
                # Add top two visualizations.
                
                 tabPanel("Economics",
                          column(6,
                                 h1("Economic Impact of COVID-19"),
                                 p("More information on Economics..."))),
                 tabPanel("Team",
                          column(6,
                                 h1("The Team"),
                                 p("Team Backgrounds...")),
                          column(6,
                                 h1("Sources"),
                                 p("Information on Sources")))
                 ))
  
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
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
