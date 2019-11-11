# points over time

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

ggplot(ec_dat, aes(x = date_time, y = recLevel)) +
  geom_point()+
  geom_smooth()


ggplot(ec_dat, aes(x = date, y = recLevel)) +
  geom_point()+
  geom_smooth()+
  facet_grid(~ is_pm)