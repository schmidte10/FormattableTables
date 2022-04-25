#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "GBR Temperature Logger"),
    dashboardSidebar(),
    dashboardBody( 
        fluidRow( 
             box(
                title = "options", 
                sliderInput("lat",
                            "Latitude:",
                            min = -25,
                            max = -8,
                            step = 0.5,
                            value = c(-25,-8), pre = "\u00B0"), 
                sliderInput("lon", 
                            "Longitude:",
                            min = 140,
                            max = 155,
                            value = c(142,150),
                            step = 0.5,
                            pre = "\u00B0"), 
                sliderInput("depth", 
                            "depth", 
                            min = 0, 
                            max = 25, 
                            value = c(0,25), post = "m"), 
                sliderInput("YEAR", 
                            "YEAR", 
                            min = 1991, 
                            max = 2022, 
                            value = c(1991, 2022), sep=""), 
                selectInput("heatwave_yr", "Heatwave data", 
                            choices = c("All","Heatwave years", "Ambient years", "Heatwave vs. Ambient")), 
                height = 550, width = 2), 
        box(plotOutput("plot"),height = 550))

           
            ))


server <- function(input, output) { }



# Run the application 
shinyApp(ui = ui, server = server)
