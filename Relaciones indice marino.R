# Relaciones del indice marino- conexion con el mar

Indice <- readRDS("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/ModelacionAsentamientosGit/Indice.rds") %>% rename(Ciudad= City)


###DISTANCIA COSTA CIUDADES CHILE##################################################################

Valores_DistCosta <- readRDS("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/ModelacionAsentamientosGit/DistCosta_Chile.rds")
Validacion_Indice <- left_join(Indice, Valores_DistCosta)

Validacion_Indice <- Validacion_Indice %>% dplyr::filter(!is.na(Dist_Costa)) %>% rename(City=Ciudad)

#Validacion_IndiceSur <- Validacion_Indice %>% dplyr::filter((Lat  <= -40 ))
#Validacion_IndiceNorte <- Validacion_Indice %>% dplyr::filter((Lat  >= -40 ))
#Validacion_IndiceValpo <- Validacion_Indice %>% dplyr::filter(Ciudad %in% c("Valparaíso", "Viña del Mar", "Concón", "Gran Valparaíso"))



ggplot(Validacion_Indice) + geom_point(aes(x=Dist_Costa, y=Index)) +  scale_colour_continuous()+
  ylab("Indice conexión marina")+ xlab("Distancia a la costa (metros)")+ 
  scale_x_continuous(label = scales::comma)+ theme_classic()



#logaritmico considerando latitud de las ciudades
ggplot(Validacion_Indice) + geom_point(aes(x=Dist_Costa, y=Index, color = Lat)) + 
  scale_y_log10() + scale_x_log10(label = scales::comma) + scale_colour_continuous()+ 
  ylab("Indice conexión marina")+ xlab("Distancia a la costa (metros)")+ theme_classic()



library(minpack.lm)
FitNLS <- nlsLM(Index ~ a*Dist_Costa^b, data=Validacion_Indice, start = list(a=0.3,b=-2.32999e-05 ))
Resid <- broom::augment(FitNLS) %>% arrange(Dist_Costa)

ggplot(Resid) + geom_point(aes(x=Dist_Costa, y=Index)) + geom_path(aes(x=Dist_Costa, y = .fitted)) + 
  scale_y_log10() + scale_x_log10(label = scales::comma) + theme_classic()+
  ylab("Indice conexión marina")+ xlab("Distancia a la costa (metros)")


###RELACION CON PRIMERO POBLADORES######################################################################


Prob_punto <- read_rds("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/ModelacionAsentamientosGit/Prob_puntoCiudades_grupos.rds") %>% 
  rename(City =Ciudad)

Relacion <- left_join(Validacion_Indice, Prob_punto)




###PROPORCION CON DISTANCIA A LA COSTA##################################################################### 

# Poligonos ciudades

library(sf)
library(tidyverse)
library(terra)
library(geodata)


Coords <- read_rds("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/ModelacionAsentamientosGit/CoordsCiudades.rds") %>% 
  dplyr::filter(!is.na(Lon)) %>% 
  st_as_sf(coords = c("Lon", "Lat"), crs ='+proj=longlat +datum=WGS84') %>% 
  dplyr::mutate(Ciudad = stringr::str_to_lower(Ciudad)) %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)


Urban <- read_sf("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/ModelacionAsentamientosGit/Areas_Pobladas.shp")%>% 
  st_make_valid() %>% 
  st_transform('+proj=longlat +datum=WGS84') %>% 
  rename(Ciudad = Localidad) %>% 
  dplyr::mutate(Ciudad = stringr::str_to_lower(Ciudad))


Coords2 <- Urban %>% 
  merge(Coords)

rm(Urban)
rm(Coords)
gc()

# Loop para sacar info de todos los poligonos de las ciudades
Coords2$Proporcion_Costera <- NA

Coords2 <- dplyr::filter(Coords2, Ciudad != "puente alto")

for(i in 1:nrow(Coords2)){
  message(paste("Starting with", Coords2[i,]$Ciudad))
  Temp <- terra::vect(sf::st_make_valid(Coords2[i,]))
  Coords  <- centroids(Temp) %>% st_as_sf() %>% st_coordinates()
  Alt <- geodata::elevation_3s(lon = Coords[1], lat = Coords[2], path = getwd()) %>% crop(Temp) %>% terra::distance()
  Alt[Alt == 0] <- NA
  
  if(sum(is.na(values(Alt))) == ncell(Alt)){
    Coords2$Proporcion_Costera[i] <- 0
  }
  else if(sum(is.na(values(Alt))) != ncell(Alt)){
    Alt <- terra::distance(Alt)
    Alt <- Alt %>% terra::mask(Temp)
    
    m <- c(0, 1800, 1,
           1800, Inf, 0)
    
    rclmat <- matrix(m, ncol=3, byrow=TRUE)
    rc1 <- classify(Alt, rclmat, include.lowest=TRUE)
    
    Prop <- terra::freq(rc1) %>% as.data.frame()
    Coords2$Proporcion_Costera[i] <- (dplyr::filter(Prop, value == 1) %>% pull(count))/sum(Prop$count)
  }
  
  print(paste("La proporcion costera de", Coords2[i,]$Ciudad, "es:", Coords2$Proporcion_Costera[i], ",", i, "de", nrow(Coords2)))
}

#para mirar los datos
#SoloCosta <- Coords2 %>% dplyr::filter(Proporcion_Costera > 0)

Prop_Polig <- Coords2 %>% dplyr::select(Ciudad, geometry, Proporcion_Costera)
saveRDS(Prop_Polig, "Prop_Polig.rds")


###POBLACION URBANA POR CIUDAD ##################################################################

Pop <- read.csv("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/ModelacionAsentamientosGit/Pobl_comuna.csv") %>% 
        dplyr::select(Comuna, X2017_urbano) %>% rename(City=Comuna) %>% rename(Urban_pop20217= X2017_urbano)


###RELACION CON AVES MARINAS ####################################################################





###RELACION CON COSTANER CAMINABLE ##############################################################

#Walk <- read.csv()










