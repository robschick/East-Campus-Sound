library(tidyverse)

mycols <- readr::cols(
  observer = col_character(),
  recLevel = col_double(),
  date = col_date(format = "%m/%d/%Y"),
  time = col_time(format = "%H:%M:%S")
)

ec_dat <- read_csv(file = here::here('east-campus-sound_data.csv'), col_types = mycols) 

ec_xy <- read_csv(file = here::here('east-campus-lat-long.csv')) 
ec_xy[ec_xy$observer == "Harriet Caplin", 'long'] <- -78.915
ec_xy[ec_xy$observer == "Harriet Caplin", 'lat'] <- 36.0049


ec_dat %>% 
  group_by(observer) %>% 
  summarize(count = n(), 
            median = median(recLevel), 
            min = min(recLevel),
            max = max(recLevel))

# Plotting
ggplot(ec_dat, aes(x = date, y = recLevel)) +
  geom_point()+
  geom_smooth()
