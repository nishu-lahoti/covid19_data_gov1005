#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# I am still learning Shiny, so this was tough for me. I started by following the code in the 
# online document recommended in the assignment, but quickly entered a study hall to get assistance.
# Alongside Connor Riordan and Tahmid Ahmid, I used fluidPage in the UI to do a plotOutput.

ui <- fluidPage(
    headerPanel("Covid-19 Cases by State"),
    mainPanel(
        # Use imageOutput to place the image on the page
        plotOutput("distPlot")
    )
)

# In the server command, we used the renderImage functionality to pull the .png file we created
# and to augment it to a height and width that fit within the frame.

server <- function(input, output) {
    
    output$distPlot <- renderImage({"spread/ms_6/us-covid19.png"
        list(src = "us-covid19.png",
             contentType = 'image/png',
             width = 800,
             height = 800,
             alt = "This is alternate text")}, 
        deleteFile = FALSE)
}

shinyApp(ui, server)

# Run the application 
shinyApp(ui = ui, server = server)

