#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2) 
library(sf) 
library(dataaimsr)
library(rnaturalearth)
library(rnaturalearthdata)
# get data 
my_api_key <- Sys.getenv("AIMS_DATAPLATFORM_API_KEY") 
summary_series_data <- aims_data("temp_loggers", api_key = my_api_key,
                                 summary = "summary-by-series")

gbrdata <- as.data.frame(summary_series_data) %>% 
  drop_na(lat) %>% 
  drop_na(lon) %>% 
  separate(time_coverage_start, 
           sep="-", 
           remove=FALSE, 
           into = c("START_YEAR", "START_MONTH", "START_DAY"))%>%  
  mutate(YEAR = as.numeric(START_YEAR), 
         MONTH = as.numeric(START_MONTH), 
         DAY = as.numeric(START_DAY)) %>% 
  separate(time_coverage_end, 
           sep="-", 
           remove=FALSE, 
           into = c("END_YEAR", "END_MONTH", "END_DAY"))%>%  
  mutate(YEAR = as.numeric(END_YEAR), 
         MONTH = as.numeric(END_MONTH), 
         DAY = as.numeric(END_DAY)) 
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    navbarPage("GBR Temperature Data", theme = shinytheme("yeti"),
               tabPanel("Regional data", fluid = TRUE, icon = icon("globe-americas"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("lat",
                        "Latitude:",
                        min = -45,
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
                        choices = c("All","Heatwave years", "Ambient years", "Heatwave vs. Ambient"))
        ),

        # Show a plot of the generated distribution
        mainPanel( 
          plotOutput("plot"))
          ))))




# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  gbrdata_slider_Finder <- reactive({ 
        gbrdata %>% 
      filter(depth >= input$depth[1],depth <= input$depth[2], 
             lat >= input$lat[1], lat <= input$lat[2], 
             lon >= input$lon[1], lon <= input$lon[2], 
             YEAR >= input$YEAR[1], YEAR <= input$YEAR[2])
    
  }) 
  
  
  output$plot <-renderPlot({ 
    world <- ne_countries(scale = "medium", returnclass = "sf")
    ggplot(data = world) +
      geom_sf() +
      geom_point(data = gbrdata_slider_Finder(), aes(x = lon, y = lat, colour = depth), size = 3)+
      scale_color_gradient(low = "steelblue", high = "yellow")+
      coord_sf(xlim = c((min(gbrdata_slider_Finder()$lon)-1), 
                         (max(gbrdata_slider_Finder()$lon)+1)),
                        ylim = c((min(gbrdata_slider_Finder()$lat)-1), 
                                 (max(gbrdata_slider_Finder()$lat)+1)), 
                        expand = FALSE) +
      theme(panel.background = element_rect(fill = "aliceblue")) + 
      xlab("Longitude") + 
      ylab("Latitude")
      #geom_rect(data = gbrdata_slider_Finder(), aes(xmin=min(input$lon), 
                                                   # xmax=max(input$lon), 
                                                   # ymin=min(input$lat), 
                                                   # ymax=max(input$lat)), 
               # fill=NA, colour = "black")
    })
}
  

# Run the application 
shinyApp(ui = ui, server = server)
