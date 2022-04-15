#--- import needed packages ---#
library(usethis) 
library(dataaimsr) 
library(purrr)
library(tidyverse)
library(lubridate)
library(reshape2)
library(formattable)
library(DT)
library(ggplot2) 
#--- API Key should be stored, BUT NOT pushed to GitHUB ---#
#usethis::edit_r_environ()
my_api_key <- Sys.getenv("AIMS_DATAPLATFORM_API_KEY") 

#--- Importing data from AIMS temperature loggers - single site ---#
dataaimsr::aims_expose_attributes("temp_loggers")

#summary for data
summary_series_data <- aims_data("temp_loggers", api_key = my_api_key,
                                 summary = "summary-by-series")
head(summary_series_data) 

gbrdata <- as.data.frame(summary_series_data) %>% 
  drop_na(lat) %>% 
  drop_na(lon) %>% 
  filter(lat > -25,
         lat < -10,
         lon > 141) %>% 
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
  

max(na.omit(gbrdata$depth))
min(na.omit(gbrdata$START_YEAR)) 
max(na.omit(gbrdata$END_YEAR))




#--- mapping ---# 
gbrdata %>% mapview(xcol= "lon", ycol = "lat", crs = 4269, grid = FALSE)


# or 

reefs <- st_as_sf(gbrdata, coords = c("lon", "lat"),  crs = 4326)
reef_map_dk <- mapview(reefs, map.types = "Stamen.Toner")
