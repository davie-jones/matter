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

#HMTL set up

dat_lab <- dat %>%
  mutate(
    label_PereGr = map(
      dat$Pasture,
      ~ dat %>% filter(Pasture == .x) %>%
        select(PereGr) %>%
        htmlTable(
          header = '% Cover',
          rnames = 'Perennial Grasses',
          caption = paste0('<b>Pasture ', .x, '</b>')
        )
    ),
    label_AnnualGr = map(
      dat$Pasture,
      ~ dat %>% filter(Pasture == .x) %>%
        select(AnnualGr) %>%
        htmlTable(
          header = '% Cover',
          rnames = 'Annual Grasses',
          caption = paste0('<b>Pasture ', .x, '</b>')
        )
    ),
    label_NativeGr = map(
      dat$Pasture,
      ~ dat %>% filter(Pasture == .x) %>%
        select(NativeGr) %>%
        htmlTable(
          header = '% Cover',
          rnames = 'Native Grasses',
          caption = paste0('<b>Pasture ', .x, '</b>')
        )
    ),
    label_Grass = map(
      dat$Pasture,
      ~ dat %>% filter(Pasture == .x) %>%
        select(Grass) %>%
        htmlTable(
          header = '% Cover',
          rnames = 'All Grasses',
          caption = paste0('<b>Pasture ', .x, '</b>')
        )
    ),
    label_Trees = map(
      dat$Pasture,
      ~ dat %>% filter(Pasture == .x) %>%
        select(Trees) %>%
        htmlTable(
          header = '% Cover',
          rnames = 'Trees',
          caption = paste0('<b>Pasture ', .x, '</b>')
        )
    ),
    label_Forbs = map(
      dat$Pasture,
      ~ dat %>% filter(Pasture == .x) %>%
        select(Forbs) %>%
        htmlTable(
          header = '% Cover',
          rnames = 'Forbs',
          caption = paste0('<b>Pasture ', .x, '</b>')
        )
    ),
    label_Weeds = map(
      dat$Pasture,
      ~ dat %>% filter(Pasture == .x) %>%
        select(Weeds) %>%
        htmlTable(
          header = '% Cover',
          rnames = 'Invasive Weeds',
          caption = paste0('<b>Pasture ', .x, '</b>')
        )
    ),
    label_BareGround = map(
      dat$Pasture,
      ~ dat %>% filter(Pasture == .x) %>%
        select(BareGround) %>%
        htmlTable(
          header = '% Cover',
          rnames = 'Bare Ground',
          caption = paste0('<b>Pasture ', .x, '</b>')
        )
    )
  )

shp_poly <- st_read(poly, quiet = TRUE) %>%
  filter(field_name %in% list) %>% 
  st_transform('+proj=longlat +datum=WGS84') %>%
  full_join(dat_lab, by = c('field_name' = 'Pasture'))

pal <-
  colorBin(
    palette = colorRamp(colors = c('#ffffff', pointblue.palette[4])),
    domain = c(0, 100),
    bins = c(0, 1, 5, 10, 20, 50, 100),
    na.color = pointblue.palette[6]
  )

map1 <- leaflet(shp_poly, height = 500) %>% 
  setView(lng = 175.2047031, lat = -39.9744283, zoom = 14) %>%
  
  ## background terrain
  addProviderTiles("Esri.WorldStreetMap",
                   options = providerTileOptions(minzoom = 14, maxzoom = 15)) %>%
  
  
  ## perennial grasses
  addPolygons(fillColor = ~ pal(as.numeric(PereGr)),
              group = 'Perennial Grasses',
              popup = ~ label_PereGr,
              color = 'black', fillOpacity = 0.9, weight = 1.5) %>%
  
  ## native grasses
  addPolygons(fillColor = ~ pal(as.numeric(NativeGr)),
              group = 'Native Grasses',
              popup = ~ label_NativeGr,
              color = 'black', fillOpacity = 0.9, weight = 1.5) %>%
  
  
  ## annual grasses
  addPolygons(fillColor = ~ pal(as.numeric(AnnualGr)),
              group = 'Annual Grasses',
              popup = ~ label_AnnualGr,
              color = 'black', fillOpacity = 0.9, weight = 1.5) %>%
  
  ## all grasses
  addPolygons(fillColor = ~ pal(as.numeric(Grass)),
              group = 'All Grasses',
              popup = ~ label_Grass,
              color = 'black', fillOpacity = 0.9, weight = 1.5) %>%
  
  ## shrubs
  addPolygons(fillColor = ~ pal(as.numeric(Trees)),
              group = 'Trees',
              popup = ~ label_Trees,
              color = 'black', fillOpacity = 0.9, weight = 1.5) %>%
  
  ## forbs
  addPolygons(fillColor = ~ pal(as.numeric(Forbs)),
              group = 'Forbs',
              popup = ~ label_Forbs,
              color = 'black', fillOpacity = 0.9, weight = 1.5) %>%
  
  ## weeds
  addPolygons(fillColor = ~ pal(as.numeric(Weeds)),
              group = 'Invasive Weeds',
              popup = ~ label_Weeds,
              color = 'black', fillOpacity = 0.9, weight = 1.5) %>%
  
  ## bare ground
  addPolygons(fillColor = ~ pal(as.numeric(BareGround)),
              group = 'Bare Ground',
              popup = ~ label_BareGround,
              color = 'black', fillOpacity = 0.9, weight = 1.5) %>%
  
  ## legend
  addLegend(position = 'topright', 
            pal = pal,
            values = dat %>% gather(AnnualGr:Weeds, key = 'key', value = 'value') %>% pull(value),
            labFormat = labelFormat(suffix = '%'),
            na.label = 'No data',
            opacity = 1, 
            title = '% Cover') %>%
  
  ## toggles
  addLayersControl(baseGroups = c('Perennial Grasses', 'Native Grasses', 
                                  'Annual Grasses', 'All Grasses', 'Trees',
                                  'Forbs', 'Invasive Weeds', 'Bare Ground'),
                   options = layersControlOptions(collapsed = F),
                   position = 'bottomleft'
  ) 

map1
