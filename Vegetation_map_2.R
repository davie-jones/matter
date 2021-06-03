install.packages("sf")

library(tidyverse)
library(sf)
library(htmlTable)
library(leaflet)
library(mapview)

masterdat_veg <- 'Vegetation.xlsx'

list <- c("Yards, Working Paddock and Race", "Parsons spin off", "Yet to be named 1", "Yet to be named 2", "Non - Crop", "Crop", "Collins 1", "Collins 2", "Collins 3", "Collins 4", "Collins Triangle", "Parsons", "Pond Paddock", "Top of the gully", "The Pines", "The Bog", "River Gully Paddock", "Pump Gully", "Railway 1", "Railway 2", "O'Leary 1", "Driveway")

poly_veg <- 'fields.shp'

pointblue.palette <-
  c('#4495d1',
    '#74b743',
    '#f7941d',
    '#005baa',
    '#bfd730',
    '#a7a9ac',
    '#666666')

# DATA SET UP-------------
dat <- readxl::read_xlsx(masterdat_veg)  %>%
  mutate(cover = round(cover, digits = 1),
         vegtype = as.factor(vegtype),
         Pasture = as.factor(Pasture)) %>% 
  filter(Year == max(Year)) %>%
  spread(key = vegtype, value = cover)


maxyear = readxl::read_xlsx(masterdat_veg)  %>%
  pull(Year) %>% max()

minyear = maxyear - 3

dat <- readxl::read_xlsx(masterdat_veg)  %>%
  mutate(vegtype = as.factor(vegtype),
         Pasture = as.factor(Pasture),
         group = case_when(Year<2018 ~ 'baseline',
                           Year>minyear ~ 'recent')) %>%
  filter(!is.na(group)) %>% 
  group_by(Pasture, group, vegtype) %>%
  summarize(cover = round(mean(cover, na.rm = T), digits = 0)) %>%
  ungroup()

# calculate net change between recent and baseline years
net_change <- dat %>%
  spread(key = group, value = cover) %>%
  mutate(net = recent - baseline,
         prop = case_when(abs(net) >= 10 ~ (net/baseline) * 100,
                          abs(net) >= 5 & vegtype %in% c('PereGr', 'NativeGr', 'Trees') ~ (net/baseline) * 100,
                          TRUE ~ 0),
         prop = case_when(is.infinite(prop) ~ 1000,
                          TRUE ~ prop),
         text = case_when(net > 0 ~ paste0('+', round(net, digits = 0), '%'),
                          net < 0 ~ paste0(round(net, digits = 0), '%'),
                          net == 0 ~ '0%'))
# ,
#          text2 = case_when(prop < -90 ~ paste0('>90% decline'),
#                           prop < -10 ~ paste0('>10% decline'),
#                           prop == 0 ~ paste0('little change'),
#                           prop >= 90 ~ paste0('>90% increase'),
#                           prop > 10 ~ paste0('>10% increase')))

# format to include in pop-up tables
net_change_long <- net_change %>%
  select(-net, -prop) %>%
  gather(baseline:text, key = group, value = cover) %>%
  arrange(Pasture, vegtype, group)

#Popup HTML Tables 

dat_lab <- net_change %>%
  select(-baseline, -recent, -net, -text) %>%
  spread(key = vegtype, value = prop) %>%
  mutate(
    label_PereGr = map(unique(net_change$Pasture),
                       ~ net_change_long %>% 
                         filter(Pasture == .x & vegtype == 'PereGr') %>%
                         select(cover) %>%
                         htmlTable(header = c('Average<br>% Cover'), 
                                   rnames = c('2015-2017', paste0(minyear, '-', maxyear), 'Difference'),
                                   align = 'r', total = T,
                                   caption = paste0('<b>Pasture ', .x, 
                                                    '</b>: Perennial Grasses'))),
    label_AnnualGr = map(unique(net_change$Pasture),
                         ~ net_change_long %>% 
                           filter(Pasture == .x & vegtype == 'AnnualGr') %>%
                           select(cover) %>%
                           htmlTable(header = c('Average<br>% Cover'), 
                                     rnames = c('2015-2017', paste0(minyear, '-', maxyear), 'Difference'),
                                     align = 'r', total = T,
                                     caption = paste0('<b>Pasture ', .x, 
                                                      '</b>: Annual Grasses'))),
    label_NativeGr = map(unique(net_change$Pasture),
                         ~ net_change_long %>% 
                           filter(Pasture == .x & vegtype == 'NativeGr') %>%
                           select(cover) %>%
                           htmlTable(header = c('Average<br>% Cover'), 
                                     rnames = c('2015-2017', paste0(minyear, '-', maxyear), 'Difference'),
                                     align = 'r', total = T,
                                     caption = paste0('<b>Pasture ', .x, 
                                                      '</b>: Native Grasses'))),
    label_Grass = map(unique(net_change$Pasture),
                      ~ net_change_long %>% 
                        filter(Pasture == .x & vegtype == 'Grass') %>%
                        select(cover) %>%
                        htmlTable(header = c('Average<br>% Cover'), 
                                  rnames = c('2015-2017', paste0(minyear, '-', maxyear), 'Difference'),
                                  align = 'r', total = T,
                                  caption = paste0('<b>Pasture ', .x, 
                                                   '</b>: All Grasses'))),
    label_Trees = map(unique(net_change$Pasture),
                       ~ net_change_long %>% 
                         filter(Pasture == .x & vegtype == 'Trees') %>%
                         select(cover) %>%
                         htmlTable(header = c('Average<br>% Cover'), 
                                   rnames = c('2015-2017', paste0(minyear, '-', maxyear), 'Difference'),
                                   align = 'r', total = T,
                                   caption = paste0('<b>Pasture ', .x, 
                                                    '</b>: Trees'))),
    label_Crops = map(unique(net_change$Pasture),
                      ~ net_change_long %>% 
                        filter(Pasture == .x & vegtype == 'AnnualCrop') %>%
                        select(cover) %>%
                        htmlTable(header = c('Average<br>% Cover'), 
                                  rnames = c('2015-2017', paste0(minyear, '-', maxyear), 'Difference'),
                                  align = 'r', total = T,
                                  caption = paste0('<b>Pasture ', .x, 
                                                   '</b>: Crops'))),
    label_Weeds = map(unique(net_change$Pasture),
                      ~ net_change_long %>% 
                        filter(Pasture == .x & vegtype == 'Weeds') %>%
                        select(cover) %>%
                        htmlTable(header = c('Average<br>% Cover'), 
                                  rnames = c('2015-2017', paste0(minyear, '-', maxyear), 'Difference'),
                                  align = 'r', total = T,
                                  caption = paste0('<b>Pasture ', .x, 
                                                   '</b>: Weeds'))),
    label_BareGround = map(unique(net_change$Pasture),
                           ~ net_change_long %>% 
                             filter(Pasture == .x & vegtype == 'BareGround') %>%
                             select(cover) %>%
                             htmlTable(header = c('Average<br>% Cover'), 
                                       rnames = c('2015-2017', paste0(minyear, '-', maxyear), 'Difference'),
                                       align = 'r', total = T,
                                       caption = paste0('<b>Pasture ', .x, 
                                                        '</b>: Bare Ground'))))


#Shapefiles set up 

shp_poly <- st_read(poly_veg, quiet = TRUE) %>%
  filter(field_name %in% list) %>% 
  st_transform('+proj=longlat +datum=WGS84') %>%
  full_join(dat_lab, by = c('field_name' = 'Pasture'))

#Color set up

pal <- colorBin(palette = colorRamp(colors = c(pointblue.palette[3], 
                                               '#ffffff', 
                                               pointblue.palette[4])),
                domain = c(-100, 100),
                bins = c(-100, -90, -10, 10, 90, max(net_change$prop)),
                na.color = pointblue.palette[7])

#Map

map2 <- leaflet(shp_poly, height = 500) %>% 
  setView(lng = 175.2047031, lat = -39.9744283, zoom = 14) %>%
  
  ## background terrain
  addProviderTiles("Esri.WorldStreetMap",
                   options = providerTileOptions(minzoom = 14, maxzoom = 15)) %>%

  
  ## perennial grasses
  addPolygons(fillColor = ~ pal(PereGr),
              group = 'Perennial Grasses',
              popup = ~ label_PereGr,
              color = 'black', fillOpacity = 0.9, weight = 1.5) %>%
  
  ## native grasses
  addPolygons(fillColor = ~ pal(NativeGr),
              group = 'Native Grasses',
              popup = ~ label_NativeGr,
              color = 'black', fillOpacity = 0.9, weight = 1.5) %>%
  
  ## annual grasses
  addPolygons(fillColor = ~ pal(AnnualGr),
              group = 'Annual Grasses',
              popup = ~ label_AnnualGr,
              color = 'black', fillOpacity = 0.9, weight = 1.5) %>%
  
  ## all grasses
  addPolygons(fillColor = ~ pal(Grass),
              group = 'All Grasses',
              popup = ~ label_Grass,
              color = 'black', fillOpacity = 0.9, weight = 1.5) %>%
  
  ## trees
  addPolygons(fillColor = ~ pal(Trees),
              group = 'Trees',
              popup = ~ label_Trees,
              color = 'black', fillOpacity = 0.9, weight = 1.5) %>%
  
  ## crops
  addPolygons(fillColor = ~ pal(AnnualCrop),
              group = 'Crops',
              popup = ~ label_Crops,
              color = 'black', fillOpacity = 0.9, weight = 1.5) %>%
  
  ## weeds
  addPolygons(fillColor = ~ pal(Weeds),
              group = 'Invasive Weeds',
              popup = ~ label_Weeds,
              color = 'black', fillOpacity = 0.9, weight = 1.5) %>%
  
  ## bare ground
  addPolygons(fillColor = ~ pal(BareGround),
              group = 'Bare Ground',
              popup = ~ label_BareGround,
              color = 'black', fillOpacity = 0.9, weight = 1.5) %>%
  
  ## legend
  addLegend(position = 'topright',
            title = '% Change relative<br>to 2015-17',
            colors = pal(c(-95, -25, 0, 25, 95)),
            values = net_change %>% pull(prop),
            labels = c('strong decline', 'decline', 
                       'little change', 'increase', 'strong increase'),
            labFormat = labelFormat(suffix = '%'),
            na.label = 'No data',
            opacity = 1) %>%
  
  ## toggles
  addLayersControl(baseGroups = c('Perennial Grasses', 'Native Grasses',
                                  'Annual Grasses', 'All Grasses', 'Trees',
                                  'Crops', 'Invasive Weeds', 'Bare Ground'),
                   options = layersControlOptions(collapsed = F),
                   position = 'bottomleft')

map2
