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

# Class Average
ec_dat %>% 
  summarise(median = median(recLevel))

# Class Summary - sorted by average
ec_dat %>% 
  group_by(observer) %>% 
  summarise(n = n(),
            min = fivenum(recLevel)[1],
            Q1 = fivenum(recLevel)[2],
            median = fivenum(recLevel)[3],
            Q3 = fivenum(recLevel)[4],
            max = fivenum(recLevel)[5]) %>% 
  arrange(median)

# Does time of day matter?
ec_dat <- ec_dat %>% 
  mutate(is_pm = lubridate::pm(time))

ec_dat %>% 
  group_by(observer, is_pm) %>% 
  summarise(n = n(),
            median = fivenum(recLevel)[3]) %>% 
  arrange(observer)
# ok, but it may be easier to view as a plot

# What about Weekday?
ec_dat <- ec_dat %>% 
  mutate(day_of_week = lubridate::wday(date, label = TRUE))

# Plotting
# boxplot 
ggplot(ec_dat) + 
  geom_boxplot(aes(y = recLevel))

# not that helpful - right? How about by observer?
ec_dat %>% 
  arrange(median)

ggplot(ec_dat, aes(x = fct_reorder(observer, recLevel), y = recLevel)) + 
  geom_boxplot()+
  labs(x = 'Observer', y = 'Received Level (dB)')

# but we can't see the labels
ggplot(ec_dat, aes(x = fct_reorder(observer, recLevel), y = recLevel)) + 
  geom_boxplot()+
  labs(x = 'Observer', y = 'Received Level (dB)') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

# AM/PM Plotting
ggplot(ec_dat, aes(x = fct_reorder(observer, recLevel), y = recLevel, fill = is_pm)) + 
  geom_boxplot()+
  labs(x = 'Observer', y = 'Received Level (dB)') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

# but that's kind of messy - let's facet it
ggplot(ec_dat, aes(x = fct_reorder(observer, recLevel), y = recLevel, fill = is_pm)) + 
  geom_boxplot()+
  labs(x = 'Observer', y = 'Received Level (dB)') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  facet_grid(~is_pm)

# Boxplot by day
ggplot(ec_dat, aes(x = day_of_week, y = recLevel)) + 
  geom_boxplot()+
  labs(x = 'Day of Week', y = 'Received Level (dB)')

# Boxplot by day = AM/PM
ggplot(ec_dat, aes(x = day_of_week, y = recLevel, fill = is_pm)) + 
  geom_boxplot()+
  labs(x = 'Day of Week', y = 'Received Level (dB)')

# but how many points do we have eachday?
ec_dat %>% 
  group_by(day_of_week) %>% 
  count()

ec_dat %>% 
  group_by(day_of_week, is_pm) %>% 
  count()


# points over time
ggplot(ec_dat, aes(x = date, y = recLevel)) +
  geom_point()+
  geom_smooth()


ggplot(ec_dat, aes(x = date, y = recLevel)) +
  geom_point()+
  geom_smooth()+
  facet_grid(~ is_pm)
