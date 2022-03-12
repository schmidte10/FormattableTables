library(usethis) 
library(dataaimsr) 
library(tidyverse)
usethis::edit_r_environ()
my_api_key <- Sys.getenv("AIMS_DATAPLATFORM_API_KEY") 

data_frame <- aims_data("temp_loggers", api_key = my_api_key, filters = list(
  "series" = "STCRISPFL1",
  "from_date" = "2014-03-16T00:00:00",
  "thru_date" = "2015-01-07T00:00:00"
))

dataframe

dataaimsr::aims_expose_attributes("temp_loggers") 
head(aims_data("temp_loggers", api_key = my_api_key, 
          summary = "summary-by-series"))

CairnsRegion <- aims_data("temp_loggers", 
                       api_key = my_api_key,
                       summary = "summary-by-series", 
                       filters = list(max_lat = -15.000, 
                                      min_lat = - 19.000,
                                      max_lon = 147.500, 
                                      min_lon = 145.500, 
                                      from_date = "2015-01-01", 
                                      thru_date = "2020-12-31"
                                      )) 
unique(agincourt$site)
plot(agincourt, ptype = "map") 

CairnsRegion <- aims_data("temp_loggers", 
                          api_key = my_api_key,
                          summary = "summary-by-series", 
                          filters = list(max_lat = -15.000, 
                                         min_lat = - 19.000,
                                         max_lon = 147.500, 
                                         min_lon = 145.500, 
                                         from_date = "2015-01-01", 
                                         thru_date = "2020-12-31"
                          ))  %>% 
  filter(between (depth, 7,15)) 

CairnsRegion %>% summarise( 
  number_of_reefs = length(site), 
  min_depth = min(depth), 
  max_depth = max(depth), 
  mean_depth = mean(depth)
  )


