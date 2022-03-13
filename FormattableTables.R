#--- import needed packages ---#
library(usethis) 
library(dataaimsr) 
library(purrr)
library(tidyverse)

#--- API Key should be stored, BUT NOT pushed to GitHUB ---#
usethis::edit_r_environ()
my_api_key <- Sys.getenv("AIMS_DATAPLATFORM_API_KEY") 

#--- Importing data from AIMS temperature loggers - single site ---#
dataaimsr::aims_expose_attributes("temp_loggers")

stcrispin <- aims_data("temp_loggers", 
                       api_key = my_api_key, 
                       summary = "daily",
                       filters = list(
                         "series" = "STCRISPFL1",
                         "from_date" = "2014-03-16T00:00:00",
                         "thru_date" = "2015-01-07T00:00:00"
))
head(stcrispin)

#--- selecting a region ---#
CairnsRegion <- aims_data("temp_loggers", 
                       api_key = my_api_key,
                       summary = "summary-by-series", 
                       filters = list(max_lat = -15.000, 
                                      min_lat = - 19.000,
                                      max_lon = 150.500, 
                                      min_lon = 143.500, 
                                      from_date = "2015-01-01", 
                                      thru_date = "2020-12-31"
                                      )) 
unique(CairnsRegion$site); plot(CairnsRegion, ptype = "map") 


CairnsRegion2 <- aims_data("temp_loggers", 
                          api_key = my_api_key,
                          summary = "summary-by-series", 
                          filters = list(max_lat = -15.000, 
                                         min_lat = - 19.000,
                                         max_lon = 147.500, 
                                         min_lon = 145.500, 
                                         from_date = "2015-01-01", 
                                         thru_date = "2020-12-31"
                          ))  ; plot(CairnsRegion2, ptype = "map") 

#--- filtering data ---#
CairnsRegion3 <- CairnsRegion2%>% 
  filter(between (depth, 7,15)) 

#--- summarising data ---#
CairnsRegion3 %>% summarise( 
  number_of_reefs = length(site), 
  min_depth = min(depth), 
  max_depth = max(depth), 
  mean_depth = mean(depth)
  )

#--- Importing data from multiple sites ---#
reef_list <- CairnsRegion3$series_id
aims_data_per_series <- function(series_number, my_api_key, ...) {
  aims_data("temp_loggers", api_key = my_api_key,  summary = "daily",
            filters = list(series_id = series_number, ...))
}
CairnsTemp <- purrr::map_df(reef_list, aims_data_per_series,
                         my_api_key = my_api_key,
                         from_date = "2015-01-01",
                         thru_date = "2020-12-31")

CairnsTemp_summary <- CairnsTemp %>% 
  group_by(site) %>% 
  summarise( 
    earliest_date = min(time), 
    latest_date = max(time), 
    days_obsrvd = length(time)); 
print(as_tibble(CairnsTemp_summary), n=length(CairnsTemp_summary$site)) 

#--- Importing data from multiple sites (for loop) ---#
from_date_list = c("2015-01-01","2016-01-01","2017-01-01","2018-01-01","2019-01-01","2020-01-01") 
thru_date_list = c("2015-12-31","2016-12-31","2017-12-31","2018-12-31","2019-12-31","2020-12-31")

CairnsTemp_list <- list()
for (i in 1:6) { 
 CairnsTemp_list[[i]] <-  purrr::map_df(reef_list, aims_data_per_series,
                my_api_key = my_api_key,
                from_date = from_date_list[[i]],
                thru_date = thru_date_list[[i]])
  
}
head(CairnsTemp_list[[1]])
CairnsTemp2 <- bind_rows(CairnsTemp_list)
CairnsTemp_summary2 <- CairnsTemp2 %>% 
  group_by(site) %>% 
  summarise( 
    earliest_date = min(time), 
    latest_date = max(time), 
    days_obsrvd = length(time)) 

print(as_tibble(CairnsTemp_summary2), n=length(CairnsTemp_summary$site)) 
head(CairnsTemp_summary); head(CairnsTemp_summary2)
# Now all data is included and we can get on too making tables

#--- formatting data for table ---# 
print(as_tibble(CairnsTemp_summary2), n=length(CairnsTemp_summary$site)) 
head(CairnsTemp_summary); head(CairnsTemp_summary2) 

flsdljfdhskjfhsdkj
