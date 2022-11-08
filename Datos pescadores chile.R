### DATOS PESCADORES

library(tidyverse)
library(sf)
library(mapview)
library(rvest)
library(httr)
library(terra)
library(readxl)

##### Desembarque pesca de sernapesca.cl para introduccion
Desembarque_pesca_Chile_2002_2017 <- read_excel("Desembarque_pesca_Chile 2002_2017.xls") %>% dplyr::select(-ESPECIE)
Desembarque <- pivot_longer(Desembarque_pesca_Chile_2002_2017, cols = 1:16, names_to = "Years", values_to = "value")
ggplot(Desembarque, aes(x=Years, y=value))+ geom_point()+geom_smooth(method = lm, formula = y ~ x)+
  theme_bw()+ ylab("Total annual fish landings")

##### revision KML de caletas chile
# Read the KML file as a Spatial object

unzip("/home/giorgia/Desktop/CALETAS 240 TOTAL (POSICIONES).kmz")
caletas <-sf::st_read("doc.kml")
Layers <- st_layers("doc.kml")

Coords2 <- read_rds("Poligonos_Ciudades.rds")

caletas <- list()

for(i in 2:length(Layers$name)){
  caletas[[i]] <- st_read("doc.kml", layer = Layers$name[i]) %>% 
    mutate(Region = Layers$name[i])
}

caletas <- caletas %>% 
  purrr::reduce(bind_rows)

Geometries <- sf::st_geometry_type(caletas)

caletas <- caletas[Geometries == "POINT",]

caletas <- caletas %>% 
  dplyr::select(Name, Region) %>% 
  terra::vect()

terra::writeVector(caletas, "Caletas.shp")

caletas <- read_sf("Caletas.shp")

Distance <- st_distance(caletas, Coords2)

Mins <- apply(Distance, MARGIN = 1, min)

caletas$Distancia_Min <- NA
caletas$Ciudad_Cercana <- NA

for(i in 1:length(Mins)){
  caletas$Distancia_Min[i] <- Mins[i]
  caletas$Ciudad_Cercana[i] <-Coords2$Ciudad[as.numeric(Distance[i,]) == as.numeric(Mins[i])]
}



Valor_caletas <- caletas %>% 
  mutate(Dist_Km = Distancia_Min/1000) %>% 
  mutate(Indice = 1/(Distancia_Min + 1), Indice_Km =  1/(Dist_Km + 1)) %>% 
  group_by(Ciudad_Cercana) %>% 
  summarise(Indice = sum(Indice), Indice_Km = sum(Indice_Km))


Valor_caletas <-Valor_caletas %>% rename(Ciudad=Ciudad_Cercana) %>% 
  dplyr::select(Ciudad, Indice_Km)

saveRDS(Valor_caletas, "Valor_caletas.rds")



