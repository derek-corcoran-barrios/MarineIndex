## Este script se realizó para generar polígonos de las ciudades ya seleccionadas 
## que poseen un valor de Indice marino

#revision de poligonos de ciudades (hecho el 11 dic 2022)
#shiny::runGitHub("derek-corcoran-barrios/ExploradorComunas")

#poligonos que no vale la pena arreglar por el tamaño
#LEBU (2 poligonos en "Urban")
#MACHALI (solo 1 poligono en "Urban)


library(sf)
library(tidyverse)
library(terra)
library(geodata)
library(leaflet)


#Usando coords a partir de las ciudades del indice marino

Coords <-  readRDS("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/ModelacionAsentamientosGit/Indice.rds") %>% 
  rename(Ciudad= City) %>% 
  dplyr::mutate(Ciudad = stringr::str_to_lower(Ciudad)) %>% 
  as.data.frame() %>% dplyr::select(Ciudad)

Coords$Ciudad[82] <- "gran santiago"
Coords$Ciudad <- janitor::make_clean_names(Coords$Ciudad) %>% str_replace_all("_", " ")

Urban <- read_sf("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/ModelacionAsentamientosGit/Areas_Pobladas.shp") %>% 
  st_make_valid() %>% 
  st_transform('+proj=longlat +datum=WGS84') %>% 
  rename(Ciudad = Localidad) %>% 
  dplyr::mutate(Ciudad = stringr::str_to_lower(Ciudad)) 

Urban$Ciudad <- janitor::make_clean_names(Urban$Ciudad) %>% str_replace_all("_", " ")

#Arreglo de nombres en Urban para que quede igual a los nombres de las ciudades del indice y el resto de las variables:
Urban$Ciudad[620] <- "coyhaique"
Urban$Ciudad[116] <- "padre las casas"
Urban$Ciudad[456] <- "santo domingo"

###### Correccion de polígonos

######## sumando dos localidades pequeñas de otro DF (http://datos.cedeus.cl/)
#solo poseen los puntos y no son poligonos

#towns <- read_sf("towns.shp") %>% dplyr::select(name, geometry) %>% filter(name %in% c("Tongoy", "HualpÃ©n")) %>% rename(Ciudad=name)
#towns$Ciudad[1] <-"tongoy"
#towns$Ciudad[2] <-"hualpen"

#Urban <- Urban %>% bind_rows(towns)
#rm(towns)


#para detectar ciudades de ambos DF y si es que coninciden
#Coords$Ciudad[!(Coords$Ciudad %in% Urban$Ciudad)] %>% sort



#### Para areas metropolitanas se trabajo con los poligonos de "area urbana consolidada" (http://datos.cedeus.cl/)

Conurbaciones <- read_sf("area_urbana_consolidada_2017Polygon.shp") %>%
  janitor::clean_names() %>% 
  st_transform(crs="+proj=longlat +datum=WGS84") 


#GRAN SANTIAGO
#Primero saco los otros poligonos de santiago de urban q corresponde a la comuna
Urban <- Urban %>% dplyr::filter(Ciudad!= "santiago", Ciudad!= "santiago 2")

#Identifico el poligono del gran santiago y luego filtro de DF
#leaflet() %>% addTiles() %>% addPolygons(data = gran_santiago, popup = ~NOMBRE, label =~NOMBRE)

gran_santiago <- Conurbaciones %>% dplyr::filter(nombre== "GRAN SANTIAGO") %>% dplyr::select(nombre, geometry) %>% 
  rename(Ciudad=nombre) 
gran_santiago$st_area_sh <- sf::st_area(gran_santiago) %>% as.numeric()
gran_santiago[1]<- "gran santiago"

Urban <- Urban %>% bind_rows(gran_santiago)
rm(gran_santiago)

#CONURBACION CHILLAN
conurbacion_chillan <- Conurbaciones %>% dplyr::filter(nombre== "CHILLÁN-CHILLÁN VIEJO") %>% dplyr::select(nombre, geometry) %>% 
  rename(Ciudad=nombre) 
conurbacion_chillan$st_area_sh <- sf::st_area(conurbacion_chillan) %>% as.numeric()
conurbacion_chillan[1]<- "conurbacion chillan"

Urban <- Urban %>% bind_rows(conurbacion_chillan)
rm(conurbacion_chillan)

#CONURBACION COQUIMBO-LA SERENA
conurbacion_coquimbo_laserena <- Conurbaciones %>% dplyr::filter(nombre== "COQUIMBO-LA SERENA") %>% dplyr::select(nombre, geometry) %>% 
  rename(Ciudad=nombre) 
conurbacion_coquimbo_laserena$st_area_sh <- sf::st_area(conurbacion_coquimbo_laserena) %>% as.numeric()
conurbacion_coquimbo_laserena [1]<- "conurbacion la serena coquimbo"

Urban <- Urban %>% bind_rows(conurbacion_coquimbo_laserena)
rm(conurbacion_coquimbo_laserena)

#GRAN CONCEPCIÓN
Gran_Concepción <- Conurbaciones %>% dplyr::filter(nombre== "GRAN CONCEPCIÓN") %>% dplyr::select(nombre, geometry) %>% 
  rename(Ciudad=nombre)
Gran_Concepción$st_area_sh <- sf::st_area(Gran_Concepción) %>% as.numeric()
Gran_Concepción[1]<- "gran concepcion"

Urban <- Urban %>% bind_rows(Gran_Concepción)
rm(Gran_Concepción)


#TEMUCO-PADRE LAS CASAS
Gran_Temuco <- Conurbaciones %>% dplyr::filter(nombre== "TEMUCO-PADRE LAS CASAS") %>% dplyr::select(nombre, geometry) %>% 
  rename(Ciudad=nombre) 
Gran_Temuco$st_area_sh <- sf::st_area(Gran_Temuco) %>% as.numeric()
Gran_Temuco[1]<- "gran temuco"

Urban <- Urban %>% bind_rows(Gran_Temuco)
rm(Gran_Temuco)


#GRAN VALPARAÍSO
Gran_Valparaíso <- Conurbaciones %>% dplyr::filter(nombre== "GRAN VALPARAÍSO") %>% dplyr::select(nombre, geometry) %>% 
  rename(Ciudad=nombre) 
Gran_Valparaíso$st_area_sh <- sf::st_area(Gran_Valparaíso) %>% as.numeric()
Gran_Valparaíso[1]<- "gran valparaiso"

Urban <- Urban %>% bind_rows(Gran_Valparaíso)
rm(Gran_Valparaíso)

rm(Conurbaciones)

### Urban posee ciudades con multipoligonos, en donde el primer poligono tiene el nombre de la ciudad y luego se van enumerando
# seleccion de poligonos, visualizando con Leaflet

#para visualizar cada grupo de poligonos y selecciono el correcto
Iquique <- Urban %>% dplyr::filter(str_detect(Ciudad, "iquique"))

#leaflet() %>% addTiles() %>% addPolygons(data = as_Spatial(Iquique), popup = ~objectid, label =~objectid)
#Identifico a Iquique 15 como el necesario

#SACO IQUIQUE 15 A PARTE Y LUEGO SOLO SUMO IQUIQUE 15 A URBAN... LAS OTRAS CIUDADES QUEDAN IGUAL YA Q SON LAS Q NO TIENEN NUMERO
Iquique <- Iquique %>% dplyr::filter(Ciudad=="iquique 15")

Urban <- Urban %>% dplyr::filter(Ciudad != "iquique"   & Ciudad != "iquique 2" & Ciudad != "iquique 3" & Ciudad != "iquique 4" &
                                 Ciudad != "iquique 5" & Ciudad != "iquique 6" & Ciudad != "iquique 7" & Ciudad != "iquique 8" &
                                 Ciudad != "iquique 9" & Ciudad != "iquique 10" & Ciudad != "iquique 11" & Ciudad != "iquique 12" &
                                   Ciudad != "iquique 13" & Ciudad != "iquique 14")

Urban <- Urban %>% bind_rows(Iquique)
Urban$Ciudad[643]<-"iquique"

rm(Iquique)
gc()

#Para los demas poligonos no es necesario modificar ya que al unirse las ciudades se une con el primer poligono q son los correctos 
# Calama <- Urban %>% dplyr::filter(str_detect(Ciudad, "calama"))
# leaflet() %>% addTiles() %>% addPolygons(data = as_Spatial(Calama), popup = ~objectid, label =~objectid)
# "calama"
# 
# Lota <- Urban %>% dplyr::filter(str_detect(Ciudad, "lota"))
# leaflet() %>% addTiles() %>% addPolygons(data = as_Spatial(Lota), popup = ~objectid, label =~objectid)
# "lota"
# 
# RioNegro <- Urban %>% dplyr::filter(str_detect(Ciudad, "rio negro"))
# leaflet() %>% addTiles() %>% addPolygons(data = as_Spatial(RioNegro), popup = ~objectid, label =~objectid)
# "rio negro"



##########

#completados los arreglos en Urban (poligonos) y Coords (nombres ciudades) los junto:

Coords2 <- Urban %>%   merge(Coords)

rm(Urban)
rm(Coords)
gc()

Coords2 %>% saveRDS("Poligonos_Ciudades.rds")

#Area <- Coords2 %>% as.data.frame() %>% dplyr::select(-geometry)





#######################################
