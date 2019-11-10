library(googlesheets)
library(tidyverse)
(my_sheets <- gs_ls())
my_sheets %>% glimpse()
ec <- gs_title("East Campus Recordings")

mycols <- readr::cols(
  observer = col_character(),
  recLevel = col_double(),
  date = col_date(format = "%m/%d/%Y"),
  time = col_time(format = "%H:%M:%S")
)

ec_dat <- ec %>%
  gs_read(ws = "Sheet1", col_types = mycols)

ec_xy <- ec %>%
  gs_read(ws = "lat/lon_coords")

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
