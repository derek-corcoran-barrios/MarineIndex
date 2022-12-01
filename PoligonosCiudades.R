## Este script se realizó para generar polígonos de las ciudades ya seleccionadas 
## que poseen un valor de Indice marino

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
##
Conurbación_Chillán <- c("chillán", "chillán viejo") %>% janitor::make_clean_names()%>% str_replace_all("_", " ")
Con_Chillan <- Urban %>%  dplyr::filter(Ciudad %in% Conurbación_Chillán) %>% 
  sf::st_union()  %>% st_as_sf() %>% mutate(Ciudad = "conurbacion chillan")

Con_Chillan <- st_sf(geometry=Con_Chillan) %>% rename(geometry=x)
Urban <- Urban %>% bind_rows(Con_Chillan)

##
Conurbación_La_Serena_Coquimbo <- c("la serena", "coquimbo")%>% janitor::make_clean_names()%>% str_replace_all("_", " ")
Con_sercoq <- Urban %>%  dplyr::filter(Ciudad %in% Conurbación_La_Serena_Coquimbo) %>% 
  sf::st_union() %>% st_as_sf() %>% mutate(Ciudad = "conurbacion la serena coquimbo")
Con_sercoq <- st_sf(geometry=Con_sercoq) %>% rename(geometry=x)

Urban <- Urban %>% bind_rows(Con_sercoq)

##
Gran_Concepción <- c("concepcion", "coronel", "chiguayante", "hualpen", "hualqui", "lota", "penco", "san pedro de la paz", "talcahuano", "tome") %>% 
  janitor::make_clean_names()%>% str_replace_all("_", " ")
gran_conce <- Urban %>%  dplyr::filter(Ciudad %in% Gran_Concepción) %>% 
  sf::st_union() %>% st_as_sf() %>% mutate(Ciudad = "gran concepcion")
gran_conce <- st_sf(geometry=gran_conce) %>% rename(geometry=x)

Urban <- Urban %>% bind_rows(gran_conce)

##
Gran_Temuco <- c("temuco", "padre las casas") %>% 
  janitor::make_clean_names()%>% str_replace_all("_", " ")
gran_temu <- Urban %>%  dplyr::filter(Ciudad %in% Gran_Temuco) %>% 
  sf::st_union() %>% st_as_sf() %>% mutate(Ciudad = "gran temuco")
gran_temu <- st_sf(geometry=gran_temu) %>% rename(geometry=x)

Urban <- Urban %>% bind_rows(gran_temu)

##
Gran_Valparaíso<- c( "valparaiso", "vina del mar", "quilpue", "villa alemana", "concon")%>% 
  janitor::make_clean_names()%>% str_replace_all("_", " ")
gran_valpo <- Urban %>%  dplyr::filter(Ciudad %in% Gran_Valparaíso) %>% 
  sf::st_union() %>% st_as_sf() %>% mutate(Ciudad = "gran valparaiso")
gran_valpo <- st_sf(geometry=gran_valpo) %>% rename(geometry=x)

Urban <- Urban %>% bind_rows(gran_valpo)


rm(Con_Chillan)
rm(Con_sercoq)
rm(gran_conce)
rm(gran_temu)
rm(gran_valpo)



######## sumando dos localidades pequeñas de otro DF (http://datos.cedeus.cl/)
#solo poseen los puntos y no son poligonos

#towns <- read_sf("towns.shp") %>% dplyr::select(name, geometry) %>% filter(name %in% c("Tongoy", "HualpÃ©n")) %>% rename(Ciudad=name)
#towns$Ciudad[1] <-"tongoy"
#towns$Ciudad[2] <-"hualpen"

#Urban <- Urban %>% bind_rows(towns)
#rm(towns)


#para detectar ciudades de ambos DF y si es que coninciden
#Coords$Ciudad[!(Coords$Ciudad %in% Urban$Ciudad)] %>% sort



#### Solo falta el gran santiago, por lo que se descarga como una capa aparte q incluye toda la ciudad (http://datos.cedeus.cl/)

#Primero saco los otros poligonos de santiago de urban q corresponde a la comuna
Urban <- Urban %>% dplyr::filter(Ciudad!= "santiago", Ciudad!= "santiago 2")

gran_santiago <- read_sf("area_urbana_consolidada_2017Polygon.shp") %>% 
  st_transform(crs="+proj=longlat +datum=WGS84")

#Identifico el poligono del gran santiago y luego filtro de DF
#leaflet() %>% addTiles() %>% addPolygons(data = gran_santiago, popup = ~NOMBRE, label =~NOMBRE)

gran_santiago <- gran_santiago %>% dplyr::filter(NOMBRE== "GRAN SANTIAGO") %>% dplyr::select(NOMBRE, geometry) %>% 
  rename(Ciudad=NOMBRE) 
gran_santiago[1]<- "gran santiago"

Urban <- Urban %>% bind_rows(gran_santiago)
rm(gran_santiago)




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

Area <- Coords2 %>% as.data.frame() %>% dplyr::select(-geometry)





#######################################
