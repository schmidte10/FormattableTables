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
library(ozmaps)
library(cowplot)
library(shinydashboard)
library(ggh4x)
library(DT)
library(purrr)
library(lubridate)
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
                            value = c(-15,-20), pre = "\u00B0"), 
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
                            value = c(7,15), post = "m"), 
                dateRangeInput("DATERANGE", 
                            "DATERANGE", 
                            start = "2015-01-01", 
                            end = "2020-12-31",
                            min = "1991-11-20", 
                            max = "2021-02-03", 
                            format = "yyyy-mm-dd", 
                            startview = "year"),
                numericInput("TDP", 
                             "TDP", 
                             value = 100, 
                             min = 0, 
                             max = max(summary_series_data$cal_obs)),
                #textInput("REEF", 
                        #label = "Reef search",
                        #value = "Type reef name here"),
                #selectInput("heatwave_yr", "Heatwave data", 
                            #choices = c("All","Heatwave years", "Ambient years", "Heatwave vs. Ambient")), 
                height = 650, width = 3), 
        box(plotOutput("plot"), 
            height = 650, width = 3), 
        box(plotOutput("oz_plot"), 
            height = 320, width = 4),
        fluidRow(box(plotOutput("temp_plot"), 
                                height= 300, width = 4)), 
            
        box(DT::dataTableOutput('table'), 
            width = 12)
            
        )
        )
    )


server <- function(input, output, session) { 
    
    gbrdata_slider_Finder <- reactive({ 
        gbrdata %>% 
            filter(depth >= input$depth[1],depth <= input$depth[2], 
                   lat >= input$lat[1], lat <= input$lat[2], 
                   lon >= input$lon[1], lon <= input$lon[2], 
                   time_coverage_start >= paste(as.character(input$DATERANGE[1])), 
                   time_coverage_end <= paste(as.character(input$DATERANGE[2]))) 
    })
      
   
    gbrdata_plot_filters <- reactive({ 
      geo_reefs <-  aims_data("temp_loggers", 
                                 api_key = my_api_key,
                                 summary = "summary-by-series", 
                              filters = list(max_lat = input$lat[2], 
                                             min_lat = input$lat[1],
                                             max_lon = input$lon[2], 
                                             min_lon = input$lon[1], 
                                             from_date = paste(as.character(input$DATERANGE[1])), 
                                             thru_date = paste(as.character(input$DATERANGE[2]))
                                 ))  %>% 
        filter(depth >= input$depth[1],depth <= input$depth[2]) 
      
      geo_reefs_list <- geo_reefs$series_id 
      my_data <- function(series_number, my_api_key, ...) {
        aims_data("temp_loggers", api_key = my_api_key,  summary = "daily",
                  filters = list(series_id = series_number,...))
      } 
      
      purrr::map_df(geo_reefs_list, my_data,
                    my_api_key = my_api_key, 
                    size = input$TDP) %>% 
        mutate(time = ymd(time)) %>% #reformat date column as a 'date' variable 
        separate(time, 
                 sep="-", 
                 remove=FALSE, 
                 into = c("YEAR", "MONTH", "DAY"))%>%  
        mutate(YEAR = as.numeric(YEAR), 
               MONTH = as.numeric(MONTH), 
               DAY = as.numeric(DAY)) %>% 
        mutate(HEATWAVE= case_when(YEAR == c("1998") ~ "heatwave_yr", 
                                   YEAR == c("2002") ~ "heatwave_yr", 
                                   YEAR == c("2016") ~ "heatwave_yr", 
                                   YEAR == c("2017") ~ "heatwave_yr", 
                                   YEAR == c("2020") ~ "heatwave_yr", 
                                   TRUE ~ "ambient_year")) 
      
    })
    
    
    #--- mapping plot---#
    output$plot <-renderPlot({ 
        world <- ne_countries(scale = "medium", returnclass = "sf")
        ggplot(data = world) +
            geom_sf() +
            geom_point(data = gbrdata_slider_Finder(), aes(x = lon, y = lat, colour = depth), size = 3)+
            scale_color_gradient(low = "cyan1", high = "black", limits = c(0,25))+
            coord_sf(xlim = c((min(gbrdata_slider_Finder()$lon)-2), 
                              (max(gbrdata_slider_Finder()$lon)+2)),
                     ylim = c((min(gbrdata_slider_Finder()$lat)-2), 
                              (max(gbrdata_slider_Finder()$lat)+2)), 
                     expand = FALSE) +
            theme(panel.background = element_rect(fill = "aliceblue"), 
                  legend.position = c(0.8,0.95), 
                  legend.direction = "horizontal", 
                  plot.margin = unit(c(0, 0, 0, 0), "pt")) + 
            xlab("Longitude") + 
            ylab("Latitude")  +
            force_panelsizes(rows = unit(7, "in"),
                             cols = unit(5, "in"))
    }, height = 600, width = 400) 
    
    #--- Australia plot ---#
    output$oz_plot <- renderPlot({ 
        oz_states <- ozmaps::ozmap_states
        ggplot(oz_states) + 
            geom_sf() + 
            coord_sf() +
            theme(panel.background = element_rect(fill = "aliceblue", color = "black"), 
            axis.text = element_blank(), 
            axis.ticks=element_blank())+  
            geom_rect(data = gbrdata_slider_Finder(), aes(xmin=min(input$lon),  
                                                          xmax=max(input$lon), 
                                                          ymin=min(input$lat), 
                                                          ymax=max(input$lat)), 
            fill=NA, colour = "red") 
            
        
        }, height = 300, width = 430)
    
    #--- Data table ---#
    output$table <- renderDT(gbrdata_slider_Finder(),
                             options =list(scrollX = TRUE))
   
    #--- data plot ---#
    output$temp_plot <- renderPlot({ 
      
      gbrdata_plot_filters() %>% 
        na.omit() %>% 
        ggplot(aes(cal_val))+
        geom_density(aes(fill = HEATWAVE), 
                     size = 1, 
                     position = "identity", 
                     alpha = 0.8, 
                     adjust = 1.5) + 
        scale_fill_manual(labels = c("Ambient", "Heatwave"), 
                          values = c("#56B4E9", "#D55E00")) + 
        scale_x_continuous(breaks = seq(21,34,2), 
                           limits = c(21,32))+
        theme_light()+ 
        xlab("Temperature (\u00B0C)") + 
        ylab("Density")+ 
        labs(fill = "Temperature years") + 
        theme(legend.direction = "horizontal",
              legend.position = c(.25,.90)) + 
        guides(shape = guide_legend(override.aes = list(size = 0.2)))
      
      
      }, height = 275, width = 600)  
    }



# Run the application 
shinyApp(ui = ui, server = server)
