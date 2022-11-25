# Relaciones del indice marino- conexion con el mar

library(tidyverse)

Indice <- readRDS("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/ModelacionAsentamientosGit/Indice.rds") %>% 
  rename(Ciudad= City)

Indice$Ciudad[82] <- "Gran Santiago"


###DISTANCIA COSTA CIUDADES CHILE##################################################################

Valores_DistCosta <- readRDS("DistCosta_Chile.rds")



#agregar conurbaciones

Validacion_Indice <- left_join(Indice, Valores_DistCosta)

Validacion_Indice <- Validacion_Indice %>% dplyr::filter(!is.na(Dist_Costa)) %>% rename(City=Ciudad) %>% distinct() %>% 
  dplyr::select(-Lon,-Lat)
Validacion_Indice$City <- janitor::make_clean_names(Validacion_Indice$City) %>% str_replace_all("_", " ")

rm(Indice)
rm(Valores_DistCosta)
gc()


###RELACION CON PRIMERO POBLADORES######################################################################

Prob_punto <- read_rds("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/ModelacionAsentamientosGit/Prob_puntoCiudades_grupos.rds") %>% 
  rename(City =Ciudad) %>% 
  mutate(P_PescadorArcaico = ifelse(is.na(P_PescadorArcaico), 0, P_PescadorArcaico),
         P_RecolectorMaritimo = ifelse(is.na(P_RecolectorMaritimo), 0, P_RecolectorMaritimo))

Relacion <- left_join(Validacion_Indice, Prob_punto) %>% distinct()
Relacion <- Relacion %>% dplyr::select(-P_PrimerosHabitantes)

rm(Prob_punto, Validacion_Indice)
gc()

##filtro hualpen y tongoy porque no encontré los poligonos de estos poblados
Relacion <- Relacion %>% dplyr::filter(City != "tongoy" & City != "hualpen")


###PROPORCION CON DISTANCIA A LA COSTA##################################################################### 

Prop_DistCosta <- read_rds("Prop_Polig.rds")

Relacion$City <- janitor::make_clean_names(Relacion$City) %>% str_replace_all("_", " ")

Prop_DistCosta$Ciudad <- janitor::make_clean_names(Prop_DistCosta$Ciudad) %>% str_replace_all("_", " ")

Prop_DistCosta <- Prop_DistCosta %>% rename(City = Ciudad)


Relacion <- left_join(Relacion, Prop_DistCosta)

rm(Prop_DistCosta)
gc()

###POBLACION URBANA POR CIUDAD ##################################################################

Pop <- readRDS("Population2017_forIndex.rds") %>% rename(City=ciudad)

Pop$City <- janitor::make_clean_names(Pop$City) %>% str_replace_all("_", " ")

Relacion <- left_join(Relacion, Pop)

rm(Pop)
gc()

###RELACION CON DIVERSIDAD DE AVES (MARINAS) ####################################################################


Birds <- readRDS("Bird_type_prop.rds")
Birds$Ciudad <- janitor::make_clean_names(Birds$Ciudad) %>% str_replace_all("_", " ")

Birds <- Birds %>% rename(City = Ciudad)

Relacion <- left_join(Relacion, Birds) 


Relacion <- Relacion %>%
   mutate_at(c("Riqueza_Total", "Riqueza_Aquatic", "Proportion_Aquatic", "shallow_water_bird_richness", 
                "seabird_richness", "shorebird_richness", "shallow_water_bird_prop", 
                "seabird_prop", "shorebird_prop"), ~case_when(!is.na(.) & !is.na(Proporcion_Costera_1800)~ .,
                                                              is.na(.) & !is.na(Proporcion_Costera_1800)~ 0
                ))

rm(Birds)
gc()


#### RELACOIN CON DIVERSIDAD DE PLANTAS ##########################

Plantas <- readRDS("RiquezaPlantas_Ciudad.rds") %>% rename(Riq_Plantas= Riqueza_Total)

Relacion <- left_join(Relacion, Plantas)
rm(Plantas)

gc()

###RELACION CON COSTANERA CAMINABLE ##############################################################

Walk <- read.csv("variables ciudades a mano.csv")

Walk$City <- janitor::make_clean_names(Walk$City) %>% str_replace_all("_", " ")

#Walk$KmWalk_WB <- readr::parse_number(Walk$KmWalk_WB)
Walk$KmWalk_WB <- ifelse(is.na(Walk$KmWalk_WB), 0 , Walk$KmWalk_WB)

Walk$KmWalk_Total<- Walk$KmWalk_sea + Walk$KmWalk_WB

Relacion <- left_join(Relacion, Walk) 

rm(Walk)
gc()

### blue visibility index

Viewshed <- readRDS("viewshed_altitid.rds")
Viewshed$Ciudad <- janitor::make_clean_names(Viewshed$Ciudad) %>% str_replace_all("_", " ")
Viewshed <- Viewshed %>% dplyr::rename(City = Ciudad)

Relacion <- Relacion %>% left_join(Viewshed)
rm(Viewshed)

### Area por ciudad

Area <- readRDS("Poligonos_Ciudades.rds") %>% as.data.frame() %>% dplyr::select(Ciudad, st_area_sh) %>% 
  rename(City = Ciudad) 

Relacion <- Relacion %>% left_join(Area)%>% rename(Area = st_area_sh)
rm(Area)

### número de pescadores por ciudad

pescadores <- readRDS("Pescadores_ciudad_ENE.rds") %>% ungroup() %>% dplyr::select(Ciudad,n) %>% rename(City=Ciudad) %>% 
  rename(pescadores=n) %>% replace_na()
pescadores$City <- janitor::make_clean_names(pescadores$City) %>% str_replace_all("_", " ") 

Relacion <- Relacion %>% left_join(pescadores) %>% 
  mutate(pescadores = ifelse(is.na(pescadores), 0, pescadores))

rm(pescadores)

###### número de caletas por ciudad

Caletas <- readRDS("Valor_caletas.rds") %>% rename(City=Ciudad) %>% replace_na()%>% dplyr::select(-geometry) %>%  as.data.frame()
Caletas$City <- janitor::make_clean_names(Caletas$City) %>% str_replace_all("_", " ") 

Relacion <- Relacion %>% left_join(Caletas) %>% mutate(Indice_Km = ifelse(is.na(Indice_Km), 0, Indice_Km))

rm(Caletas)
gc()

################## 
## Test con GBM
Variables <- Relacion %>% dplyr::select(-Lon, -Lat)
Variables <- Variables[complete.cases(Variables),]    ################# ????
ForModel <- Variables %>% dplyr::select(-City)

library(caret)
library(gbm)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = c(0.1,0.01),
                        n.minobsinnode = 20)


set.seed(825)


gbmFit2 <- train(Index ~ ., data = ForModel, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = TRUE, 
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)
summary(gbmFit2)

plot(gbmFit2)

saveRDS(gbmFit2, "ResultadoGBM2.rds")
saveRDS(Variables, "Variables.rds")

par(mfrow=c(2,2))


###Resultados modelo y graficos

gbmFit2 <-readRDS("ResultadoGBM2.rds")
variables <- readRDS("Variables.rds")

library(tidyverse)
library(caret)
library(gbm)
library(patchwork)

### Relaciones entre Indice y Distancia a la costa

Var1 <- plot(gbmFit2$finalModel, i.var = 1, return.grid = T)
p1 <-ggplot(Var1, aes(x = Dist_Costa, y = y)) + geom_path() +xlab("Distance to the coast (m)") + ylab("Marine Index")+
  theme_bw() +ggtitle('A')

Var2 <- plot(gbmFit2$finalModel, i.var = 19, return.grid = T)
p2 <-ggplot(Var2, aes(x = KmWalk_Total, y = y)) + geom_path()+xlab("Total walk-able water body coastline") + ylab("Marine Index")+
  theme_bw()+ggtitle('B')

Var3 <- plot(gbmFit2$finalModel, i.var = 17, return.grid = T)
p3 <-ggplot(Var3, aes(x = KmWalk_sea, y = y)) + geom_path() +xlab("Walk-able marine coastline") + ylab("Marine Index")+
  theme_bw()+ggtitle('C')

Var4 <- plot(gbmFit2$finalModel, i.var = 2, return.grid = T)
p4 <-ggplot(Var4, aes(x = P_PescadorArcaico, y = y)) + geom_path() +xlab("Archaic fisherman communities' probability") + ylab("Marine Index")+
  theme_bw()+ggtitle('D')

Var5 <- plot(gbmFit2$finalModel, i.var = 7, return.grid = T)
p5 <-ggplot(Var5, aes(x = poblacion, y = y)) + geom_path() +xlab("Urban population in 2017") + ylab("Marine Index")+
  theme_bw() +ggtitle('E')

Var6 <- plot(gbmFit2$finalModel, i.var = 21, return.grid = T)
p6<-ggplot(Var6, aes(x = Mean, y = y)) + geom_path() +xlab("Mean of blueness visibility index") + ylab("Marine Index")+
  theme_bw()+ggtitle('F')

Var7 <- plot(gbmFit2$finalModel, i.var = 24, return.grid = T)
p7<-ggplot(Var7, aes(x = n_viewshed, y = y)) + geom_path() +xlab("Proxi of city's area") + ylab("Marine Index")+
  theme_bw() +ggtitle('G')#+annotate("text", x=0.75, y=0.01, label="Variable importance= 2.74416536")

Var8 <- plot(gbmFit2$finalModel, i.var = 4, return.grid = T)
p8<-ggplot(Var8, aes(x = Proporcion_Costera_1800, y = y)) + geom_path() +xlab("Ocean proportion 1800 m from the coast") + ylab("Marine Index")+
  theme_bw()+ggtitle('H')

Var9 <- plot(gbmFit2$finalModel, i.var = 3, return.grid = T)
p9<-ggplot(Var9, aes(x = P_RecolectorMaritimo, y = y)) + geom_path() +xlab("Marine gatherers' probability") + ylab("Marine Index")+
  theme_bw()+ggtitle('I')

Var10 <- plot(gbmFit2$finalModel, i.var = 8, return.grid = T)
p10<-ggplot(Var10, aes(x = Riqueza_Total, y = y)) + geom_path() +xlab("Total richness of birds") + ylab("Marine Index")+
  theme_bw()+ggtitle('J')

Var11 <- plot(gbmFit2$finalModel, i.var = 10, return.grid = T)
p11<-ggplot(Var11, aes(x = Proportion_Aquatic, y = y)) + geom_path() +xlab("Aquatic bird proportion") + ylab("Marine Index")+
  theme_bw()+ggtitle('K')

Var12 <- plot(gbmFit2$finalModel, i.var = 26, return.grid = T)
p12<-ggplot(Var12, aes(x = Indice_Km, y = y)) + geom_path() +xlab("Artisanal fishing cove value") + ylab("Marine Index")+
  theme_bw()+ggtitle('L')

Var13 <- plot(gbmFit2$finalModel, i.var = 25, return.grid = T)
p13 <-ggplot(Var13, aes(x = pescadores, y = y)) + geom_path() +xlab("Fishing and aquaculture activities") + ylab("Marine Index")+
  theme_bw()+ggtitle('M')

Var14 <- plot(gbmFit2$finalModel, i.var = 14, return.grid = T)
p14<-ggplot(Var14, aes(x = seabird_prop, y = y)) + geom_path() +xlab("Seabird proportion") + ylab("Marine Index")+
  theme_bw()+ggtitle('N')

Var15 <- plot(gbmFit2$finalModel, i.var = 15, return.grid = T)
p15<-ggplot(Var15, aes(x = shallow_water_bird_prop, y = y)) + geom_path() +xlab("Shallow water bird proportion") + ylab("Marine Index")+
  theme_bw()+ggtitle('O')

Var16 <- plot(gbmFit2$finalModel, i.var = 12, return.grid = T)
p16<-ggplot(Var16, aes(x = shallow_water_bird_richness, y = y)) + geom_path() +xlab("Shallow water bird richness") + ylab("Marine Index")+
  theme_bw()+ggtitle('P')

Var17 <- plot(gbmFit2$finalModel, i.var = 9, return.grid = T)
p17<-ggplot(Var17, aes(x = Riqueza_Aquatic, y = y)) + geom_path() +xlab("Aquatic bird richness") + ylab("Marine Index")+
  theme_bw()+ggtitle('Q')

Var18 <- plot(gbmFit2$finalModel, i.var = 16, return.grid = T)
p18<-ggplot(Var18, aes(x = shorebird_prop, y = y)) + geom_path() +xlab("Shorebird proportion") + ylab("Marine Index")+
  theme_bw()+ggtitle('R')

Var19 <- plot(gbmFit2$finalModel, i.var = 22, return.grid = T)
p19<-ggplot(Var19, aes(x = Quant_90, y = y)) + geom_path() +xlab("Quantile 90 of blueness visibility index") + ylab("Marine Index")+
  theme_bw()+ggtitle('S')

Var20 <- plot(gbmFit2$finalModel, i.var = 11, return.grid = T)
p20<-ggplot(Var20, aes(x = seabird_richness, y = y)) + geom_path() +xlab("Seabird richness") + ylab("Marine Index")+
  theme_bw()+ggtitle('T')

Var21 <- plot(gbmFit2$finalModel, i.var = 13, return.grid = T)
p21<-ggplot(Var21, aes(x = shorebird_richness, y = y)) + geom_path() +xlab("Shorebird richness") + ylab("Marine Index")+
  theme_bw()+ggtitle('U')

Var22 <- plot(gbmFit2$finalModel, i.var = 6, return.grid = T)
p22<-ggplot(Var22, aes(x = Proporcion_Costera_600, y = y)) + geom_path() +xlab("Ocean proportion 600 m from the coast") + ylab("Marine Index")+
  theme_bw()+ggtitle('V')

p1/(p2+p3)
(p4+p5)/(p6+p7)/(p8+p9+p10)
(p11+p12+p13)/(p14+p15+p16)/(p17+p18+p19)/(p20+p21+p22)
                    
                     
#########################################################
#Caracteristicas variables

library(tidyverse)

 View(summary(variables))
 
 
 
 
 Pop <- read.csv("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/ModelacionAsentamientosGit/Pobl_comuna.csv") %>% 
   dplyr::select(Region,Comuna, X2017_urbano) %>% rename(City=Comuna) %>% rename(Urban_pop20217= X2017_urbano)
 
  Pop$City <- janitor::make_clean_names(Pop$City) %>% str_replace_all("_", " ")
 
 
 Variables <- left_join(Variables, Pop)
 

 
 
 
 
 