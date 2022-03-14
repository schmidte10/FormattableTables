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

CairnsTemp2 <- CairnsTemp2 %>% 
  mutate(time = as_date(time)) %>% #reformat date column as a 'date' variable 
  separate(time, 
           sep="-", 
           remove=FALSE, 
           into = c("YEAR", "MONTH", "DAY")) 
#save dataframe
save(CairnsTemp2, file="CairnsTemp2.Rda")
print(as_tibble(CairnsTemp_summary2), n=length(CairnsTemp_summary$site)) 
head(CairnsTemp_summary); head(CairnsTemp_summary2)
# Now all data is included and we can get on too making tables

#--- formatting data for table ---# 
#load dataframe
load("CairnsTemp2.Rda")  
#extracting summer months
summer <- CairnsTemp2 %>%  
  mutate(YEAR = as.numeric(YEAR), 
         MONTH = as.numeric(MONTH), 
         DAY = as.numeric(DAY)) %>%
  filter(MONTH == c(1,2,3,12)); unique(summer$MONTH) 

#--- gather yearly means ---#
mst <- summer %>% 
  group_by(site, YEAR)%>% 
  na.omit() %>%
  summarise(temp_mean = mean(cal_val), 
             max_mean = mean(cal_max), 
             depth_mean = mean(depth), 
             range_mean = mean(cal_max - cal_min)) %>% 
  ungroup()

#--- getting data into table format in R ---# 
mst_temp <- mst %>% 
  dcast(site~YEAR, value.var = "temp_mean") 
colnames(mst_temp)[2:7] = paste0('Mean_',colnames(mst_temp)[2:7])
mst_max <- mst %>% 
  dcast(site~YEAR, value.var = "max_mean")
colnames(mst_max)[2:7] = paste0('Max_',colnames(mst_max)[2:7])
mst_depth <- mst %>% 
  dcast(site~YEAR, value.var = "depth_mean")
colnames(mst_depth)[2:7] = paste0('Depth_',colnames(mst_depth)[2:7])
mst_range <- mst %>% 
  dcast(site~YEAR, value.var = "range_mean") 
colnames(mst_range)[2:7] = paste0('Range_',colnames(mst_range)[2:7])

# join all the data together
mst_all <- full_join(mst_temp,mst_max, by="site") %>% 
  full_join(mst_range, by ="site") %>% 
  full_join(mst_depth[c(1,7)], by = "site") %>% 
  mutate_if(is.numeric, round, digits = 2); names(mst_all) 
save(mst_all, file="mst_all.Rda")

#--- put table into formattable ---# 
# load data
load("mst_all.RDA")
mst_all2 <- mst_all
mst_all2[is.na(mst_all2)] = '' 

CairnsRegion_Table <- formattable(mst_all2, 
            align = c("l","r","r","r","r","r","r","r","r","r","r","r","r"
                      ,"r","r","r","r","r","r","r","r","r","r","r","r","r"),
            list(site = formatter( 
              "span",
              style = ~style(color = "grey", font.weigh = "bold")), 
              area(col = `Mean_2015`:`Mean_2020`) ~ color_tile("white","red"), 
              area(col = `Max_2015`:`Max_2020`) ~ color_tile("yellow","red"), 
              area(col = `Range_2015`:`Range_2020`) ~ color_tile("palegreen","green3"), 
              Depth_2020 = color_bar("pink", 'proportion', 0.2))) 

CairnsRegion_Table = as.datatable(formattable(CairnsRegion_Table)) %>% 
  formatStyle(colnames(mst_all2), `text-align` = 'right')

#--- splitting data ---# 
CairnsTemp4 <- CairnsTemp2 %>% 
  mutate(heatwave_years = case_when( 
    YEAR == "2016" ~ "heatwave_yr", 
    YEAR == "2017" ~ "heatwave_yr", 
    YEAR == "2020" ~ "heatwave_yr",
    TRUE ~ "ambient_yr"))

agg_data <- CairnsTemp4 %>% 
  group_by(heatwave_years)%>% 
  na.omit() %>%
  summarise(temp_mean = mean(cal_val), 
            max_mean = mean(cal_max), 
            range_mean = mean(cal_max - cal_min)) %>% 
  ungroup() 

agg_data[nrow(agg_data)+1,] <- list("Difference",
                                    agg_data$temp_mean[2]-agg_data$temp_mean[1], 
                                 agg_data$max_mean[2]- agg_data$max_mean[1], 
                                 agg_data$range_mean[2] - agg_data$range_mean[1])
agg_data[3,]
  
  
  
