###Poblacion en ciudades de Chile

#Obtenido del ultimo censo de chile año 2017, archivo de poblacion por comuna se seleccionó la poblacion urbana
#se generó la información de las areas metropolitanas
#se sumó el area de pueblos pequeños de otro archivo 


#library(data.table)
library(tidyverse)
library(readxl)
library(janitor)

Pop_comuna2017 <- read_excel("Pop_comuna2017.xls", sheet = "Comuna") %>% clean_names() %>% 
  dplyr::filter(grupos_de_edad== "Total Comuna") %>% 
  dplyr::select(nombre_region,nombre_comuna, total_area_urbana) %>% 
  rename(total=total_area_urbana) %>%  rename(ciudad=nombre_comuna) %>% 
  rename(region=nombre_region) %>% mutate_if(is.character, str_to_lower) 


##Edicion nombres de ciudades
#Arreglo de nombres en base de datos con este simbolo "(*)" (region del ñuble anteriormente region biobio) 

Pop_comuna2017$ciudad[231] <- "chillán"
Pop_comuna2017$ciudad[236] <- "chillán viejo"

#Cambio de nombre comunas por real nombre de ciudades

# Editar Calera por La Calera, Natales por Puerto Natales, Puerto Williams por Cabo de Hornos


##
GranSantiago <- Pop_comuna2017 %>% dplyr::filter(region =="metropolitana de santiago" ) %>% 
  dplyr::filter(ciudad %in% c("cerrillos", "la reina", "pudahuel", "cerro navia", "las condes", "quilicura",
                            "conchalí", "lo barnechea","quinta normal", "el bosque", "lo espejo",
                            "recoleta",  "estación central", "lo prado", "renca","huechuraba", "macul",
                            "san miguel", "independencia", "maipú", "San joaquín", "la cisterna",
                            "ñuñoa",  "san ramón", "la florida", "pedro aguirre cerda",
                            "santiago", "la pintana", "peñalolén", "vitacura", "la granja",
                            "providencia", "puente alto", "san bernardo")) %>% group_by(region) %>% 
  summarize(n = sum(total)) %>% 
  dplyr::rename(ciudad = region) %>% dplyr::rename(total = n)

GranSantiago$ciudad[1] <- "gran santiago"

##


Conurbación_Chillán <- Pop_comuna2017 %>%
  dplyr::filter(ciudad %in% c("chillán", "chillán viejo")) %>% group_by(region) %>% summarize(n = sum(total)) %>% 
  dplyr::rename(ciudad = region) %>% dplyr::rename(total = n)

Conurbación_Chillán$ciudad[1] <- "conurbación chillán"



##
Conurbación_La_Serena_Coquimbo <- Pop_comuna2017 %>%
  dplyr::filter(ciudad %in% c("la serena", "coquimbo")) %>% group_by(region) %>% summarize(n = sum(total)) %>% 
  dplyr::rename(ciudad = region) %>% dplyr::rename(total = n)

Conurbación_La_Serena_Coquimbo$ciudad[1] <- "conurbación la serena-coquimbo"

##
Gran_Concepción <- Pop_comuna2017 %>%
  dplyr::filter(ciudad %in% c("concepción", "coronel", "chiguayante", "hualpén", "hualqui", "lota", "penco", "san pedro de la paz", "talcahuano", "tomé")) %>% group_by(region) %>% summarize(n = sum(total)) %>% 
  dplyr::rename(ciudad = region) %>% dplyr::rename(total = n)

Gran_Concepción$ciudad[1] <- "gran concepción"

##
Gran_Temuco <- Pop_comuna2017 %>%
  dplyr::filter(ciudad %in% c("temuco", "padre las casas")) %>% group_by(region) %>% summarize(n = sum(total)) %>% 
  dplyr::rename(ciudad = region) %>% dplyr::rename(total = n)

Gran_Temuco$ciudad[1] <- "gran temuco"
##

Gran_Valparaíso <- Pop_comuna2017 %>%
  dplyr::filter(ciudad %in% c( "valparaíso", "viña del mar", "quilpué", "villa alemana", "concón")) %>% group_by(region) %>% summarize(n = sum(total)) %>% 
  dplyr::rename(ciudad = region) %>% dplyr::rename(total = n)

Gran_Valparaíso$ciudad[1] <- "gran valparaíso"
###

Pop_urbes2017 <- Pop_comuna2017 %>% dplyr::select(ciudad, total)

Pop_urbes2017 <-rbind(Pop_urbes2017, GranSantiago, Conurbación_Chillán, Conurbación_La_Serena_Coquimbo, Gran_Concepción,Gran_Temuco, Gran_Valparaíso)
Pop_urbes2017 <- Pop_urbes2017%>% rename(poblacion=total)

#########
#Buscando los poblados q poseen indice marino
#tabla copiada en excel. la informacion fue extraída desde "INE 2019. CIUDADES, PUEBLOS,ALDEAS Y CASERÍOS 2019 "

Pop_pueblos <- read.csv("Pop_pueblos.csv") %>% rename(ciudad=pueblo) 


#arreglo de ultimos detalles
Pop_chile2017 <- rbind(Pop_urbes2017, Pop_pueblos)
Pop_chile2017$ciudad %>% str_remove_all("\\(\\*\\)")

Pop_chile2017$ciudad[345] <- "puerto natales"
Pop_chile2017$ciudad[340] <- "puerto williams"

saveRDS(Pop_chile2017, "Population2017_forIndex.rds")




