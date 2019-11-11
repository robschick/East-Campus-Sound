library(tidyverse)
library(sp)
library(leaflet)

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
classAvg <- ec_dat %>% 
  summarise(median = median(recLevel)) %>% 
  as.vector()

ec_dat <- ec_dat %>% 
  mutate(is_below = if_else(recLevel < as.numeric(classAvg), TRUE, FALSE))


# Class Summary - sorted by average
ec_dat_sum <- ec_dat %>% 
  group_by(observer) %>% 
  summarise(n = n(),
            min = fivenum(recLevel)[1],
            Q1 = fivenum(recLevel)[2],
            median = fivenum(recLevel)[3],
            Q3 = fivenum(recLevel)[4],
            max = fivenum(recLevel)[5]) %>% 
  arrange(median)

# join the x,y data
ec_dat_sum_xy <- left_join(ec_dat_sum, ec_xy, by = 'observer')


# Does time of day matter?
ec_dat <- ec_dat %>% 
  mutate(is_pm = lubridate::pm(time)) %>% 
  mutate(month = lubridate::month(date, label = TRUE))

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
ggplot(ec_dat, aes(x = fct_reorder(observer, recLevel), y = recLevel)) + 
  geom_boxplot()+
  labs(x = 'Observer', y = 'Received Level (dB)')

# but we can't see the labels
ggplot(ec_dat, aes(x = fct_reorder(observer, recLevel), y = recLevel)) + 
  geom_boxplot()+
  labs(x = 'Observer', y = 'Received Level (dB)') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 

# Which axis orientation do you like better?
ggplot(ec_dat, aes(x = fct_reorder(observer, recLevel), y = recLevel)) + 
  geom_boxplot()+
  labs(x = 'Observer', y = 'Received Level (dB)') +
  coord_flip()

# # Add average bar and color
# ggplot(ec_dat, aes(x = fct_reorder(observer, recLevel), y = recLevel, fill = is_below)) + 
#   geom_bar()+
#   labs(x = 'Observer', y = 'Received Level (dB)') +
#   coord_flip()


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

# explore by day
ggplot(ec_dat, aes(x = date, color = day_of_week, y = recLevel)) + 
  geom_point()+
  geom_smooth()+
  labs(x = 'Day of Week', y = 'Received Level (dB)')


# let's facet that
ggplot(ec_dat, aes(x = date, y = recLevel)) + 
  geom_point()+
  geom_smooth()+
  geom_hline(yintercept = as.numeric(classAvg)) +
  facet_wrap(~day_of_week, nrow = 2)+
  labs(x = 'Day of Week', y = 'Received Level (dB)')

# explore by time of day
ggplot(ec_dat, aes(x = date, color = is_pm, y = recLevel)) + 
  geom_point()+
  geom_smooth()+
  facet_wrap(~is_pm)+
  labs(x = 'Day of Week', y = 'Received Level (dB)')

# change palette
ggplot(ec_dat, aes(x = date, color = is_pm, y = recLevel)) + 
  geom_point()+
  geom_smooth()+
  facet_wrap(~is_pm)+
  scale_color_brewer(palette = 'Set1')+
  labs(x = 'Day of Week', y = 'Received Level (dB)')

# Plot a histogram
ggplot(ec_dat, aes(x = recLevel)) + 
  geom_histogram()+
  facet_grid(is_pm ~ .)+
  labs(x = 'Received Level (dB)')

# Plot a histogram - diff colors + density
ggplot(ec_dat, aes(x = recLevel, y = ..density..)) + 
  geom_histogram(fill = 'cornsilk', colour = 'grey60')+
  geom_density()+
  facet_grid(is_pm ~ .)+
  labs(x = 'Received Level (dB)')

# Plot a histogram - diff colors + density + average
# makde the data
vline.dat <- ec_dat %>% 
  group_by(is_pm) %>% 
  summarize(avg = median(recLevel))

ggplot(ec_dat, aes(x = recLevel, y = ..density..)) + 
  geom_histogram(fill = 'cornsilk', colour = 'grey60')+
  geom_density()+
  geom_vline(aes(xintercept = avg), data = vline.dat)+
  facet_grid(is_pm ~ .)+
  labs(x = 'Received Level (dB)')

# Boxplot by day = AM/PM
ggplot(ec_dat, aes(x = day_of_week, y = recLevel, fill = is_pm)) + 
  geom_boxplot()+
  labs(x = 'Day of Week', y = 'Received Level (dB)')

ggplot(ec_dat, aes(x = day_of_week, y = recLevel)) + 
  geom_boxplot()+
  labs(x = 'Day of Week', y = 'Received Level (dB)')+
  facet_wrap(~month)

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


# Make a map
mybins <- seq(min(ec_dat_sum_xy$min), max(ec_dat_sum_xy$max), by=5)
mypalette <- colorBin(palette="Blues", domain=ec_dat_sum_xy$median, na.color="transparent", bins=mybins)

mytext <- paste(
  "Observer: ", ec_dat_sum_xy$observer, "<br/>", 
  "SPL (dB): ", ec_dat_sum_xy$median, sep="") %>%
  lapply(htmltools::HTML)


m <- leaflet(ec_dat_sum_xy) %>% 
  addTiles()  %>% 
  addCircleMarkers(~long, ~lat,
                   fillColor = ~mypalette(median), fillOpacity = 0.9, radius=10, stroke=FALSE,
                   label = mytext,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"),
                                                textsize = "13px", direction = "auto")) %>%
  addLegend(pal=mypalette, values=~median, opacity=0.9, title = "Received Level (dB)", position = "bottomright" )
m  
