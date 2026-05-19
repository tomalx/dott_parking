library(sf)
library(leaflet)
library(tidyverse)


parking_dott <- st_read(choose.files())
parking_pmt <- st_read(choose.files())

parking_dott <- parking_dott %>% #st_transform(4326) %>% 
  mutate(park_id = name, map = "dott") %>%
  select(park_id, map)
parking_pmt <- parking_pmt %>% #st_transform(4326) %>% 
  mutate(map = "pmt") %>% 
  select(park_id)


parking_merge <- bind_rows(parking_dott, parking_pmt) #%>% 
  # group_by(park_id) %>% 
  # summarise(n = n()) %>% 
  # filter(n == 2) %>% 
  # select(park_id)



parking_merge <- parking_merge %>% 
  arrange(park_id) %>% 
  group_by(park_id) %>% 
  summarize() %>% 
  st_cast("LINESTRING")# %>% 
  # mutate( distance = st_length(.)) %>% 
  # filter(as.numeric(distance) < 2)
  
parking_merge <- parking_merge %>% mutate(distance = 5)

 # st_cast("LINESTRING")
  
#library(khroma)

bright_fun <- khroma::color("sunset")
palette <- bright_fun(3)




leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(data = parking_pmt %>% st_transform(4326), 
                   color = palette[1], 
                   opacity = 1,
                   #weight = 0.2,
                   fillColor = palette[1], 
                   radius = 1,
                   popup = paste0("<b>pmt</b> <br/>", parking_pmt$park_id),
                   group = "pmt"
                   ) %>%
  addCircleMarkers(data = parking_dott %>% st_transform(4326), 
                   color = palette[3], 
                   opacity = 1,
                   #weight = 0.2,
                   fillColor = palette[3], 
                   radius = 1,
                   popup = paste0("<b>dott</b> <br/>", parking_dott$park_id),
                   group = "pmt"
                   ) %>%
  addPolylines(data = parking_merge %>% st_transform(4326), 
               weight = 2,
               opacity = 0.6,
               color = palette[2], 
               popup = paste0("<b>park_id: </b> ", parking_dott$park_id,"<br/> distance: ",round(parking_merge$distance,0),"m"), 
               group = "difference") %>% 
  addLayersControl(overlayGroups = c("pmt", "dott", "difference"))
