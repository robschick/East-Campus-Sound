ec_dat <- ec_dat %>% 
  mutate(date_time = lubridate::ymd_hms(paste(ec_dat$date, ec_dat$time)))


# Does time of day matter?
ec_dat <- ec_dat %>% 
  mutate(is_pm = lubridate::pm(time)) %>% 
  mutate(month = lubridate::month(date, label = TRUE))

# What does fivenum return?
ec_dat %>% 
  group_by(observer, is_pm) %>% 
  summarise(n = n(),
            median = fivenum(recLevel)[3]) %>% 
  arrange(observer)
# ok, but it may be easier to view as a plot

# What about Weekday?
ec_dat <- ec_dat %>% 
  mutate(day_of_week = lubridate::wday(date, label = TRUE))

# Logical to see if they are above/below the class average
ec_dat <- ec_dat %>% 
  mutate(is_below = if_else(recLevel < as.numeric(classAvg), TRUE, FALSE))
