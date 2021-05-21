#Idice con clasificacion_Fran

library(tidyverse)
library(ggrepel)
library(rvest)
library(tm)
library(tidytext)
library(raster)

Unicas_Fran <-readRDS("createIndex/Unicas_Fran20Mayo.rds")
Text <- readRDS("createIndex/Texts.rds") %>% dplyr::filter(Total >=500)

Texts <- full_join(Text, Unicas_Fran) %>% 
  dplyr::filter(!is.na(Conexion_Marina)) %>% 
  mutate(Index = Conexion_Marina*Freq) %>% 
  group_by(City) %>% 
  summarise(Index = sum(Index)) %>% 
  mutate(Index = Index/max(Index)) %>% 
  arrange(Index)

saveRDS(Texts, "Indice.rds") 



Valores_DistCosta <- readRDS("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/ModelacionAsentamientosGit/DistCosta_Chile.rds")
Indice <- readRDS("Indice.rds") %>% rename(Ciudad= City)
Validacion_Indice <- left_join(Indice, Valores_DistCosta)

Validacion_Indice <- Validacion_Indice %>% dplyr::filter(!is.na(Dist_Costa))
Validacion_IndiceSur <- Validacion_Indice %>% dplyr::filter((Lat  <= -40 ))
Validacion_IndiceNorte <- Validacion_Indice %>% dplyr::filter((Lat  >= -40 ))

Validacion_IndiceValpo <- Validacion_Indice %>% dplyr::filter(Ciudad %in% c("Valparaíso", "Viña del Mar", "Concón", "Gran Valparaíso"))



ggplot(Validacion_Indice) + geom_point(aes(x=Dist_Costa, y=Index)) +  scale_colour_continuous()+
  ylab("Indice conexión marina")+ xlab("Distancia a la costa (metros)")+ 
  scale_x_continuous(label = scales::comma)+ theme_classic()

ggplot(Validacion_IndiceValpo) + geom_point(aes(x=Dist_Costa, y=Index))+ 
  geom_text_repel(aes(x=Dist_Costa, y=Index, label=Ciudad)) +
  ylab("Indice conexión marina")+ xlab("Distancia a la costa (metros)")+ 
  scale_x_continuous(label = scales::comma)+ theme_classic()




#logaritmico considerando latitud de las ciudades
ggplot(Validacion_Indice) + geom_point(aes(x=Dist_Costa, y=Index, color = Lat)) + 
  scale_y_log10() + scale_x_log10(label = scales::comma) + scale_colour_continuous()+ 
  ylab("Indice conexión marina")+ xlab("Distancia a la costa (metros)")+ theme_classic()


FitNLS <- nls(Index ~ a*Dist_Costa^b, data=Validacion_Indice, start = list(a=0.3,b=-2.32999e-05 ))
Resid <- broom::augment(FitNLS) %>% arrange(Dist_Costa)

ggplot(Resid) + geom_point(aes(x=Dist_Costa, y=Index)) + geom_path(aes(x=Dist_Costa, y = .fitted)) + scale_y_log10() + scale_x_log10(label = scales::comma)
