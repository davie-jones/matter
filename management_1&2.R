install.packages("sf")

library(tidyverse)
library(sf)
library(htmlTable)
library(leaflet)
library(mapview)

masterdat <- 'Management_stats.xlsx'

poly <- 'fields.shp'

pointblue.palette <-
  c('#4495d1',
    '#74b743',
    '#f7941d',
    '#005baa',
    '#bfd730',
    '#a7a9ac',
    '#666666')

#get full list of fields from shapefile

fields <- st_read(poly, quiet = TRUE) %>%
      filter(field_name %in% list) %>% 
      st_set_geometry(NULL) 

#only include these fields from granular data

list <- c("Yards, Working Paddock and Race", "Parsons spin off", "Yet to be named 1", "Yet to be named 2", "Non - Crop", "Crop", "Collins 1", "Collins 2", "Collins 3", "Collins 4", "Collins Triangle", "Parsons", "Pond Paddock", "Top of the gully", "The Pines", "The Bog", "River Gully Paddock", "Pump Gully", "Railway 1", "Railway 2", "O'Leary 1", "Driveway")

d <- read_xlsx(masterdat)

dat <- read_xlsx(masterdat) %>%
  select(field, season, ndays, ADH) %>%
  mutate(season = factor(season, levels = c('growing', 'dormant', 'total')),
         ndays_round = txtRound(ndays, digits = 1),
         ADH_round = txtRound(ADH, digits = 1)) %>%
  complete(field, season, fill = list(ndays = 0, 
                                      ADH = 0, 
                                      ndays_round = '0.0', 
                                      ADH_round = '0.0')) %>%
  arrange(field, season)

#htmlm pop up 

dat_lab <- dat %>%
  select(-ndays_round, -ADH_round) %>%
  gather(ndays:ADH, key = 'var', value = 'value') %>%
  unite('var', var, season) %>%
  spread(key = 'var', value = 'value') %>%
  mutate(label_adh = map(dat %>% filter(season == 'total') %>% pull(field),
                         ~ dat %>% filter(field == .x) %>% select(ADH_round) %>%
                           htmlTable(header = 'ADH',
                                     align = 'r',
                                     rnames = c('Growing (Sep 2018 - Jul 2019)',
                                                'Dormant (Jun 2018 - Aug 2018)',
                                                'Total (Jun 2018 - July 2019)'),
                                     total = T,
                                     caption = paste0('<b>Pasture ', .x, '</b>'))),
         label_days = map(dat %>% filter(season == 'total') %>% pull(field),
                          ~ dat %>% filter(field == .x) %>% select(ndays_round) %>%
                            htmlTable(header = 'Days',
                                      align = 'r',
                                      rnames = c('Growing (Sep 2018 - May 2019)',
                                                 'Dormant (Jun 2018 - Aug 2018)',
                                                 'Total (Jun 2018 - May 2019)'),
                                      total = T,
                                      caption = paste0('<b>Pasture ', .x, '</b>'))))


#shapefile set up

shp_poly <- st_read(poly, quiet = TRUE) %>%
  filter(field_name %in% list) %>% 
  st_transform('+proj=longlat +datum=WGS84') %>%
  full_join(dat_lab, by = c('field_name' = 'field'))

## color palette for adh days range
pal1 <- colorBin(palette = colorRamp(colors = c('#ffffff', pointblue.palette[4])),
                 domain = c(0, max(dat$ADH)),
                 bins = c(0, 650, 700, 750, 775, 800),
                 na.color = pointblue.palette[6])

## color palette for ndays range
pal2 <- colorBin(palette = colorRamp(colors = c('#ffffff', pointblue.palette[2])),
                 domain = c(0, max(dat$ADH)),
                 bins = c(0, 0.01, 25, 50, 100, 150),
                 na.color = pointblue.palette[6])

#animal days per hectare

map1 <- leaflet(shp_poly, height = 500) %>% 
  setView(lng = 175.2047031, lat = -39.9744283, zoom = 14) %>%
  
  ## background terrain
  addProviderTiles("Esri.WorldImagery") %>%
  
  ## growing
  addPolygons(fillColor = ~ pal1(as.numeric(ADH_growing)),
              group = 'Growing (Sep 2018 - May 2019)',
              popup = ~ label_adh,
              color = 'black', fillOpacity = 1, weight = 1.5) %>%
  
  ## dormant
  addPolygons(fillColor = ~ pal1(as.numeric(ADH_dormant)),
              group = 'Dormant (Jun 2018 - Aug 2018)',
              popup = ~ label_adh,
              color = 'black', fillOpacity = 1, weight = 1.5) %>%
  
  ## total
  addPolygons(fillColor = ~ pal1(as.numeric(ADH_total)),
              group = 'Total (Jun 2018 - May 2019)',
              popup = ~ label_adh,
              color = 'black', fillOpacity = 1, weight = 1.5) %>%
  
  ## legend
  addLegend(position = 'topright',
            colors = pal1(c(0, 15, 30, 75, 125)),
            labels = c('0', '< 25', '25 - 50', '50 - 100', '> 100'),
            na.label = 'No data',
            opacity = 1, 
            title = 'Animal days<br>per hectare') %>%
  
  ## toggles
  addLayersControl(baseGroups = c('Growing (Sep 2018 - May 2019)',
                                  'Dormant (Jun 2018 - Aug 2018)',
                                  'Total (Jun 2018 - May 2019)'),
                   options = layersControlOptions(collapsed = F),
                   position = 'bottomleft'
  )

### grazing days 

map2 <- leaflet(shp_poly, height = 500) %>% 
  setView(lng = 175.2047031, lat = -39.9744283, zoom = 14) %>%
  
  ## background terrain
  addProviderTiles("Esri.WorldImagery") %>%
                 
  
  ## growing
  addPolygons(fillColor = ~ pal2(as.numeric(ndays_growing)),
              group = 'Growing (Sep 2018 - May 2019)',
              popup = ~ label_days,
              color = 'black', fillOpacity = 1, weight = 1.5) %>%
  
  ## dormant
  addPolygons(fillColor = ~ pal2(as.numeric(ndays_dormant)),
              group = 'Dormant (Jun 2018 - Aug 2018)',
              popup = ~ label_days,
              color = 'black', fillOpacity = 1, weight = 1.5) %>%
  
  
  ## total
  addPolygons(fillColor = ~ pal2(as.numeric(ndays_total)),
              group = 'Total (Jun 2018 - May 2019',
              popup = ~ label_days,
              color = 'black', fillOpacity = 1, weight = 1.5) %>%
  
  ## legend
  addLegend(position = 'topright',
            colors = pal2(c(0, 15, 30, 75, 125)),
            labels = c('0', '< 25', '25 - 50', '50 - 100', '> 100'),
            na.label = 'No data',
            opacity = 1, 
            title = 'Total<br>grazing days') %>%
  
  ## toggles
  addLayersControl(baseGroups = c('Growing (Sep 2018 - May 2019)',
                                  'Dormant (Jun 2018 - Aug 2018)',
                                  'Total (Jun 2018 - May 2019)'),
                   options = layersControlOptions(collapsed = F),
                   position = 'bottomleft'
  )


