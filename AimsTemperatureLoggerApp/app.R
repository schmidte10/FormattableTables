#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    navbarPage("GBR Temperature Data", theme = shinytheme("lumen"),
               tabPanel("Regional data", fluid = TRUE, icon = icon("globe-americas"),
                        tags$style(button_color_css),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Latitude:",
                        min = -45,
                        max = 0,
                        value = c(-45,0), pre = "\u00B0"), 
            sliderInput("bins", 
                        "Longitude:",
                        min = 140,
                        max = 155,
                        value = c(142,150), pre = "\u00B0"), 
            sliderInput("bins", 
                        "Depth", 
                        min = 0, 
                        max = 25, 
                        value = c(0,25), post = "m"), 
            sliderInput("bins", 
                        "YEAR", 
                        min = 1991, 
                        max = 2022, 
                        value = c(1991, 2022), sep=""), 
            selectInput("heatwave_yr", "Heatwave data", 
                        choices = c("All","Heatwave years", "Ambient years", "Heatwave vs. Ambient"))
        ),

        # Show a plot of the generated distribution
        mainPanel()
        )
    )
               )
    )




# Define server logic required to draw a histogram
server <- function(input, output) {
  }

# Run the application 
shinyApp(ui = ui, server = server)
