# PROPORCION CON DISTANCIA A LA COSTA de cada poligono de las ciudades trabajadas
# considerando distintas distancias a la costa

library(sf)
library(tidyverse)
library(terra)
library(geodata)


####
#Poligonos de las ciudades que poseen un indice marino
Coords2 <- read_rds("Poligonos_Ciudades.rds")


#sacamos Puente alto por interferencia
#Geometry type: GEOMETRYCOLLECTION
Coords2 <- dplyr::filter(Coords2, Ciudad != "puente alto")
#lo puedo sumar al final con proporcion 0

####
# Loop para sacar info de todos los poligonos de las ciudades
Coords2$Proporcion_Costera_1800 <- NA
Coords2$Proporcion_Costera_1000 <- NA
Coords2$Proporcion_Costera_600 <- NA


for(i in 1:nrow(Coords2)){
  message(paste("Starting with", Coords2[i,]$Ciudad))
  Temp <- terra::vect(sf::st_make_valid(Coords2[i,]))
  Coords  <- centroids(Temp) %>% st_as_sf() %>% st_coordinates()
  Alt <- geodata::elevation_3s(lon = Coords[1], lat = Coords[2], path = getwd()) %>% crop(Temp) %>% terra::distance()
  Alt[Alt == 0] <- NA
  
  if(sum(is.na(values(Alt))) == ncell(Alt)){
    Coords2$Proporcion_Costera_1800[i] <- 0
    Coords2$Proporcion_Costera_1000[i] <- 0
    Coords2$Proporcion_Costera_600[i] <- 0
  }
  else if(sum(is.na(values(Alt))) != ncell(Alt)){
    Alt <- terra::distance(Alt)
    Alt <- Alt %>% terra::mask(Temp)
    
    m <- c(0, 1800, 1,
           1800, Inf, 0)
    
    rclmat <- matrix(m, ncol=3, byrow=TRUE)
    rc1 <- classify(Alt, rclmat, include.lowest=TRUE)
    
    Prop <- terra::freq(rc1) %>% as.data.frame()
    Coords2$Proporcion_Costera_1800[i] <- (dplyr::filter(Prop, value == 1) %>% pull(count))/sum(Prop$count)
    
    m <- c(0, 1000, 1,
           1000, Inf, 0)
    
    rclmat <- matrix(m, ncol=3, byrow=TRUE)
    rc1 <- classify(Alt, rclmat, include.lowest=TRUE)
    
    Prop <- terra::freq(rc1) %>% as.data.frame()
    Coords2$Proporcion_Costera_1000[i] <- (dplyr::filter(Prop, value == 1) %>% pull(count))/sum(Prop$count)
    
    m <- c(0, 600, 1,
           600, Inf, 0)
    
    rclmat <- matrix(m, ncol=3, byrow=TRUE)
    rc1 <- classify(Alt, rclmat, include.lowest=TRUE)
    
    Prop <- terra::freq(rc1) %>% as.data.frame()
    Coords2$Proporcion_Costera_600[i] <- (dplyr::filter(Prop, value == 1) %>% pull(count))/sum(Prop$count)
  }
  
  print(paste("La proporcion costera de", Coords2[i,]$Ciudad, "es:", Coords2$Proporcion_Costera_1000[i], ",", i, "de", nrow(Coords2)))
}

#para mirar los datos
#SoloCosta <- Coords2 %>% dplyr::filter(Proporcion_Costera_1800 > 0)

Prop_Polig <- Coords2 %>%
  as.data.frame() %>% 
  dplyr::select(Ciudad, Proporcion_Costera_1800, Proporcion_Costera_1000, Proporcion_Costera_600)

#agrego puente alto
library(readxl)
puente_alto <- read_excel("puente_alto.xls")
  
Prop_Polig <- Prop_Polig %>% bind_rows(puente_alto)

saveRDS(Prop_Polig, "Prop_Polig.rds")
