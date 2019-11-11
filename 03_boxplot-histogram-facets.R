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


