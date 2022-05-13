  #Indice con clasificacion_ realizado por Fran

library(tidyverse)
library(ggrepel)
library(rvest)
library(tm)
library(tidytext)
library(raster)
library(patchwork)

#Marcado de palabras del wiki: indice marino
#Unicas_Fran <-readRDS("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/MarineIndex/createIndex/Unicas_Fran20Mayo.rds")
#Unicas_Fran <-readRDS("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/MarineIndex/createIndex/Unicas_Fran15junio.rds")
Unicas_Fran <-readRDS("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/MarineIndex/createIndex/Unicas_completo.rds")


Text <- readRDS("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/MarineIndex/createIndex/Texts.rds") %>% dplyr::filter(Total >=500)

Texts <- full_join(Text, Unicas_Fran) %>% 
  dplyr::filter(!is.na(Conexion_Marina)) %>% 
  mutate(Index = Conexion_Marina*Freq) %>% 
  group_by(City) %>% 
  summarise(Index = sum(Index)) %>% 
  mutate(Index = Index/max(Index)) %>% 
  arrange(Index)

saveRDS(Texts, "/home/giorgia/Documents/Doctorado tesis/Mod.distrib/MarineIndex/createIndex/Indice.rds") 


Valores_DistCosta <- readRDS("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/ModelacionAsentamientosGit/DistCosta_Chile.rds")
Indice <- readRDS("Indice.rds") %>% rename(Ciudad= City)
Validacion_Indice <- left_join(Indice, Valores_DistCosta)
Validacion_Indice <- Validacion_Indice %>% dplyr::filter(!is.na(Dist_Costa))
saveRDS(Validacion_Indice, "/home/giorgia/Documents/Doctorado tesis/Mod.distrib/MarineIndex/createIndex/Validacion_Indice.rds")



Datos_costeros <- Validacion_Indice %>% dplyr::filter(Dist_Costa <40000)###############
saveRDS(Datos_costeros, "/home/giorgia/Documents/Doctorado tesis/Mod.distrib/MarineIndex/createIndex/Indice_DatosCosta.rds")
##########################################



###############################´
###############################
#INICIO ACA

Validacion_Indice <- readRDS("createIndex/Indice_DatosCosta.rds")



Validacion_IndiceNorteG <- Validacion_Indice %>% dplyr::filter((Lat  <= -17 & Lat > -27))
Validacion_IndiceNorteC <- Validacion_Indice %>% dplyr::filter((Lat  <= -27 & Lat > -32 ))
Validacion_IndiceCentro <- Validacion_Indice %>% dplyr::filter((Lat  <= -32 & Lat > -38))
Validacion_IndiceSur <- Validacion_Indice %>% dplyr::filter((Lat <= -38 & Lat > -41.5))
Validacion_IndiceAustral <- Validacion_Indice %>% dplyr::filter((Lat <= -41.5 ))


Validacion_IndiceValpo <- Validacion_Indice %>% dplyr::filter(Ciudad %in% c("Valparaíso", "Viña del Mar", "Concón", "Gran Valparaíso"))
Validacion_IndiceEjemplo <- Validacion_Indice %>% dplyr::filter(Ciudad %in% c("Santo Domingo","Coquimbo", "Gran Valparaíso", "Puerto Montt", "Iquique", "Valdivia", "Punta Arenas",
                                                                              "Santiago", "Putre", "Calama", "Curicó", "Osorno", "Chillán"))




ggplot(Validacion_Indice) + geom_point(aes(x=Dist_Costa, y=Index)) +  scale_colour_continuous()+
  ylab("Indice conexión marina")+ xlab("Distancia a la costa (metros)")+ 
  scale_x_continuous(label = scales::comma)+ theme_classic()

ggplot(Datos_costeros) + geom_point(aes(x=Dist_Costa, y=Index)) +  scale_colour_continuous()+
  ylab("Indice conexión marina")+ xlab("Distancia a la costa (metros)")+ 
  scale_x_continuous(label = scales::comma)+ theme_classic()


ggplot(Validacion_IndiceValpo) + geom_point(aes(x=Dist_Costa, y=Index))+ 
  geom_text_repel(aes(x=Dist_Costa, y=Index, label=Ciudad)) +
  ylab("Indice conexión marina")+ xlab("Distancia a la costa (metros)")+ 
  scale_x_continuous(label = scales::comma)+ theme_classic()  

ggplot(Validacion_IndiceEjemplo) + geom_point(aes(x=Dist_Costa, y=Index))+ 
  geom_text_repel(aes(x=Dist_Costa, y=Index, label=Ciudad))+ geom_hline(yintercept = 0, linetype="dashed") +
  ylab("Indice conexión marina")+ xlab("Distancia a la costa (metros)")+ 
  scale_x_continuous(label = scales::comma)+ theme_classic()


NG<- ggplot(Validacion_IndiceNorteG) + geom_point(aes(x=Dist_Costa, y=Index)) +  scale_colour_continuous()+
  ylab("Indice conexión marina")+ xlab("Distancia a la costa (metros)")+ 
  scale_x_continuous(label = scales::comma)+ theme_classic()+ ggtitle("Norte Grande")

NC<- ggplot(Validacion_IndiceNorteC) + geom_point(aes(x=Dist_Costa, y=Index)) +  scale_colour_continuous()+
  ylab("Indice conexión marina")+ xlab("Distancia a la costa (metros)")+ 
  scale_x_continuous(label = scales::comma)+ theme_classic()+ ggtitle("Norte Chico")

C<- ggplot(Validacion_IndiceCentro) + geom_point(aes(x=Dist_Costa, y=Index)) +  scale_colour_continuous()+
  ylab("Indice conexión marina")+ xlab("Distancia a la costa (metros)")+ 
  scale_x_continuous(label = scales::comma)+ theme_classic()+ ggtitle("Centro")

S<-  ggplot(Validacion_IndiceSur) + geom_point(aes(x=Dist_Costa, y=Index)) +  scale_colour_continuous()+
  ylab("Indice conexión marina")+ xlab("Distancia a la costa (metros)")+ 
  scale_x_continuous(label = scales::comma)+ theme_classic()+ ggtitle("Sur")

A<-  ggplot(Validacion_IndiceAustral) + geom_point(aes(x=Dist_Costa, y=Index)) +  scale_colour_continuous()+
  ylab("Indice conexión marina")+ xlab("Distancia a la costa (metros)")+ 
  scale_x_continuous(label = scales::comma)+ theme_classic()+ ggtitle("Austral")

NG+NC+C+S+A

ggplot(Validacion_Indice) + geom_point(aes(x=Dist_Costa, y=Index, color = Lat)) +  scale_colour_continuous()+
  ylab("Indice conexión marina")+ xlab("Distancia a la costa (metros)")+ 
  scale_x_continuous(label = scales::comma)+ theme_classic()

MinIndice <- abs(min(Validacion_Indice$Index)) + (abs(min(Validacion_Indice$Index))*0.01)

Validacion_Indice <- Validacion_Indice %>% 
  mutate(IndicePositivo = Index + MinIndice)

#logaritmico considerando latitud de las ciudades
ggplot(Validacion_Indice, aes(x=Dist_Costa, y=IndicePositivo, color = Lat)) + geom_point() + 
  scale_y_log10() + scale_x_log10(label = scales::comma) + scale_colour_continuous()+ 
  geom_text(aes(label = Ciudad)) + 
  ylab("Indice conexión marina")+ xlab("Distancia a la costa (metros)")+ theme_classic()


#library(minpack.lm)
#FitNLS <- nlsLM(Index ~ a*Dist_Costa^b, data=Validacion_Indice, start = list(a=0.3,b=-2.32999e-05 ))
#FitNLS2 <- nlsLM(Index ~ a*Dist_Costa^b + a*Lat^c , data=Validacion_Indice, start = list(a=0.3,b=-2.32999e-05, c = 2))
FitGLM <-glm(IndicePositivo ~ I(log(Dist_Costa)), data=Validacion_Indice, family = quasipoisson)
model_null <-glm(IndicePositivo ~ 1, data=Validacion_Indice, family = quasipoisson)
FitGLM2 <-glm(IndicePositivo ~ I(log(Dist_Costa)) + Lat, data=Validacion_Indice, family = quasipoisson)
library(MuMIn)


options(na.action = "na.fail")


dd <- model.sel(FitGLM2, FitGLM, model_null,
          rank = "QAIC",
          rank.args = list(chat = deviance(FitGLM2) / df.residual(FitGLM2)))

library(MASS)
FitGLM_NB <- glm.nb(IndicePositivo ~ I(log(Dist_Costa)), data=Validacion_Indice)
FitGLM_NB2 <- glm.nb(IndicePositivo ~ I(log(Dist_Costa)) + Lat , data=Validacion_Indice)

dd <- dredge(FitGLM_NB2)


  Validacion_Indice$Pred <- predict(FitGLM, type = "response", newdata = Validacion_Indice)

Validacion_Indice <- Validacion_Indice %>% mutate(Pred = (Pred - MinIndice), Resid = Index - Pred) %>% 
  arrange(Dist_Costa)

ggplot(Validacion_Indice, aes(x = Dist_Costa, y = Pred)) + geom_path() + geom_point(aes(y =Index), color = "red")

ggplot(Validacion_Indice, aes(x = Dist_Costa, y = Pred + MinIndice)) + geom_path() + geom_point(aes(y =IndicePositivo), color = "red") + scale_y_log10() + scale_x_log10()

###########
ggplot(Validacion_Indice) + geom_point(aes(x=Dist_Costa, y=IndicePositivo)) +  scale_colour_continuous()+
  ylab("Indice conexión marina")+ xlab("Distancia a la costa (metros)")+ 
  scale_x_continuous(label = scales::comma)+ theme_classic()
