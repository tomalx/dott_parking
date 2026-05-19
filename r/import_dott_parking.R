library(sf)
library(leaflet)
library(tidyverse)


parking_dott <- st_read(choose.files())
parking_pmt <- st_read(choose.files())



parking_dott <- parking_dott %>% #st_transform(4326) %>% 
  mutate(park_id = name) %>%
  select(park_id)
parking_pmt <- parking_pmt %>% #st_transform(4326) %>% 
  #mutate(map = "pmt") %>% 
  select(park_id)


parking_merge <- inner_join(parking_dott %>% st_drop_geometry(), parking_pmt %>% st_drop_geometry(), by = "park_id") #%>% 
parking_dott_merge <- parking_dott %>% filter(park_id %in% parking_merge$park_id) %>% arrange(park_id)
parking_pmt_merge <- parking_pmt %>% filter(park_id %in% parking_merge$park_id) %>% arrange(park_id)
#parking_merge <- bind_cols(parking_dott_merge, parking_pmt_merge)
parking_merge

#parking_merge <- parking_merge %>% 
lines <- st_union(parking_dott_merge$geometry, parking_pmt_merge$geometry, by_feature = TRUE) %>% st_cast("LINESTRING")
parking_merge <- bind_cols(parking_merge, lines) %>% st_as_sf() %>% 
  mutate( distance = st_length(.) %>% as.numeric() %>% format(scientific = FALSE)) %>% 
  mutate( distance = round(as.numeric(distance), 2))


#warning if any park_ids don't match

  # group_by(park_id) %>% 
  # summarise(n = n()) %>% 
  # filter(n == 2) %>% 
  # select(park_id)


### method 2 ###
parking_merge <- bind_rows(parking_dott, parking_pmt)
parking_merge <- parking_merge %>%
  arrange(park_id) %>%
  group_by(park_id) %>%
  summarize(do_union = FALSE) %>%
  st_cast("LINESTRING") %>%
  mutate( distance = st_length(.) %>% as.numeric() %>% format(scientific = FALSE)) %>% 
  mutate( distance = round(as.numeric(distance), 2))
#   

parking_merge <- parking_merge %>% filter(distance > 5)

 # st_cast("LINESTRING")
  
#library(khroma)

bright_fun <- khroma::color("bright")
palette <- bright_fun(6)




leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(data = parking_pmt_merge %>% st_transform(4326), 
                   color = palette[1], 
                   opacity = 0.5,
                   weight = 2,
                   fillColor = palette[1],
                   fillOpacity = 0.4,
                   radius = 2.5,
                   popup = paste0("<b>pmt</b> <br/>", parking_pmt_merge$park_id),
                  # label = ~park_id,
                 #  labelOptions = labelOptions(noHide = TRUE, direction = "bottom", textOnly = TRUE, textsize = 7),
                   group = "pmt"
                   ) %>%
  addCircleMarkers(data = parking_dott_merge %>% st_transform(4326), 
                   color = palette[2], 
                   opacity = 0.5,
                   weight = 2,
                   fillColor = palette[2],
                   fillOpacity = 0,
                   radius = 4,
                   popup = paste0("<b>dott</b> <br/>", parking_dott_merge$park_id),
                   group = "dott"
                   ) %>%
  addPolylines(data = parking_merge %>% st_transform(4326), 
               weight = 3,
               opacity = 0.4,
               color = palette[4], 
               popup = paste0("<b>park_id: </b> ", parking_merge$park_id,"<br/> distance: ",round(parking_merge$distance,0),"m"), 
               group = "difference") %>% 
  addLayersControl(overlayGroups = c("pmt", "dott", "difference")) %>% 
  addLegend(position = "bottomright", colors = palette[c(1,2,4)], labels = c("pmt", "dott", "difference"))




####



map <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(data = parking_pmt_merge %>% st_transform(4326), 
                   color = palette[1], 
                   opacity = 0.5,
                   weight = 2,
                   fillColor = palette[1],
                   fillOpacity = 0.4,
                   radius = 2.5,
                   popup = paste0("<b>pmt</b> <br/>", parking_pmt_merge$park_id),
                   # label = ~park_id,
                   #  labelOptions = labelOptions(noHide = TRUE, direction = "bottom", textOnly = TRUE, textsize = 7),
                   group = "pmt"
  ) %>%
  addCircleMarkers(data = parking_dott_merge %>% st_transform(4326), 
                   color = palette[2], 
                   opacity = 0.5,
                   weight = 2,
                   fillColor = palette[2],
                   fillOpacity = 0,
                   radius = 4,
                   popup = paste0("<b>dott</b> <br/>", parking_dott_merge$park_id),
                   group = "dott"
  ) 

# loop to add marking merge polylines

for(i in 1:length(parking_merge$park_id)){
  
  map <- map %>% addPolylines(data = parking_merge[i,] %>% st_transform(4326), 
               weight = 3,
               opacity = 0.4,
               color = palette[4], 
               popup = paste0("<b>park_id: </b> ", parking_merge$park_id[i],"<br/> distance: ",round(parking_merge$distance[i],0),"m"), 
               group = parking_merge$park_id[i])
}

map %>%
  addLayersControl(overlayGroups = parking_merge$park_id, options = layersControlOptions(collapsed = FALSE))
