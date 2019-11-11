# Get x,y data joined up
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
