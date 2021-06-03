#Soil data

install.packages("leaflet")
install.packages("readr")
install.packages("ggmap")
install.packages("readxl")
install.packages("ggplot2")

library(leaflet)
library(sp)
library(ggplot2)
library(ggmap)
library(readr)
library(readxl)

data <- read_excel("FarmData.xlsx")

data$long <- as.numeric(data$long)
data$lat <- as.numeric(data$lat)
data$Ph <- as.character(data$Ph)
data$`Olsen P` <- as.character(data$`Olsen P`)
data$Ca <- as.character(data$Ca)
data$Na <- as.character(data$Na)
data$Sulphate <- as.character(data$Sulphate)
data$K <- as.character(data$K)
data$Mg <- as.character(data$Mg)
data$Carbon <- as.character(data$Carbon)
data$CEC <- as.character(data$CEC)

data.SP <- SpatialPointsDataFrame(data[,c(1,2)], data[,-c(1,2)])

providers$Esri.DeLorme

m = leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addCircleMarkers(data = data, lng= ~Long, lat= ~Lat, popup= ~paste(Location,"<br>",
                                                               "<b>Ph Level:</b>", Ph, sep=" ","<br>", 
                                                               "<b>Olsen P:</b>", `Olsen P`, "<br>",
                                                               "<b>Calcium:</b>", Ca, "<br>",
                                                               "<b>Magnesium:</b>", Mg, "<br>",
                                                               "<b>Potassium:</b>", K, "<br>",
                                                               "<b>Sodium:</b>", Na, "<br>",
                                                               "<b>Sulphate:</b>", Sulphate, "<br>",
                                                               "<b>CEC:</b>", CEC, "<br>",
                                                               "<b>Carbon:</b>", Carbon, "<br>"
                                                               ))

m

