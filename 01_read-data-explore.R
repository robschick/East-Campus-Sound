library(tidyverse)
library(sp)
library(leaflet)
library(fields)
library(lubridate)

mycols <- readr::cols(
  observer = col_character(),
  recLevel = col_double(),
  date = col_date(format = "%m/%d/%Y"),
  time = col_time(format = "%H:%M:%S")
)

ec_dat <- read_csv(file = here::here('east-campus-sound_data.csv'), col_types = mycols) 

ec_xy <- read_csv(file = here::here('east-campus-lat-long.csv')) 


# Loudest 3 locations
ec_dat %>% 
  group_by(observer) %>% 
  summarize(count = n(), 
            median = median(recLevel)) %>% 
  top_n(3) 

# Quietest 3 locations
ec_dat %>% 
  group_by(observer) %>% 
  summarize(count = n(), 
            median = median(recLevel)) %>% 
  top_n(-3)

# Class Average received Level
classAvg <- ec_dat %>% 
  summarise(median = median(recLevel)) %>% 
  as.vector()
