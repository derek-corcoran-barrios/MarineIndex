# Relaciones del indice marino- conexion con el mar

library(tidyverse)

Indice <- readRDS("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/ModelacionAsentamientosGit/Indice.rds") %>% rename(Ciudad= City)


###DISTANCIA COSTA CIUDADES CHILE##################################################################

Valores_DistCosta <- readRDS("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/ModelacionAsentamientosGit/DistCosta_Chile.rds")
Validacion_Indice <- left_join(Indice, Valores_DistCosta)

Validacion_Indice <- Validacion_Indice %>% dplyr::filter(!is.na(Dist_Costa)) %>% rename(City=Ciudad)

rm(Indice)
rm(Valores_DistCosta)

#Relacion[Relacion$City =="santiago 2",]$Urban_pop20217 <- XXX

gc()


###RELACION CON PRIMERO POBLADORES######################################################################


Prob_punto <- read_rds("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/ModelacionAsentamientosGit/Prob_puntoCiudades_grupos.rds") %>% 
  rename(City =Ciudad)

Relacion <- left_join(Validacion_Indice, Prob_punto)
Relacion <- Relacion %>% dplyr::select(-P_PrimerosHabitantes)



###PROPORCION CON DISTANCIA A LA COSTA##################################################################### 

Prop_DistCosta <- read_rds("Prop_Polig.rds")

Relacion$City <- janitor::make_clean_names(Relacion$City) %>% str_replace_all("_", " ")

Prop_DistCosta$Ciudad <- janitor::make_clean_names(Prop_DistCosta$Ciudad) %>% str_replace_all("_", " ")

Prop_DistCosta <- Prop_DistCosta %>% rename(City = Ciudad)


Relacion <- left_join(Relacion, Prop_DistCosta)

rm(Prob_punto, Prop_DistCosta, Validacion_Indice)

gc()

###POBLACION URBANA POR CIUDAD ##################################################################

Pop <- read.csv("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/ModelacionAsentamientosGit/Pobl_comuna.csv") %>% 
        dplyr::select(Comuna, X2017_urbano) %>% rename(City=Comuna) %>% rename(Urban_pop20217= X2017_urbano)


Pop$City <- janitor::make_clean_names(Pop$City) %>% str_replace_all("_", " ")


Relacion <- left_join(Relacion, Pop)

rm(Pop)

gc()
###RELACION CON AVES MARINAS ####################################################################


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


###RELACION CON COSTANERA CAMINABLE ##############################################################

Walk <- read.csv("variables ciudades a mano.csv")

Walk$City <- janitor::make_clean_names(Walk$City) %>% str_replace_all("_", " ")

Walk$KmWalk_WB <- readr::parse_number(Walk$KmWalk_WB)
Walk$KmWalk_WB <- ifelse(is.na(Walk$KmWalk_WB), 0 , Walk$KmWalk_WB)

Walk$KmWalk_Total<- Walk$KmWalk_sea + Walk$KmWalk_WB



Relacion <- left_join(Relacion, Walk) 

rm(Walk)

gc()

### blue visib index


Viewshed <- readRDS("viewshed_altitid.rds")

Viewshed$Ciudad <- janitor::make_clean_names(Viewshed$Ciudad) %>% str_replace_all("_", " ")

Viewshed <- Viewshed %>% dplyr::rename(City = Ciudad)

Relacion <- Relacion %>% left_join(Viewshed)

### Test con GBM
Variables <- Relacion %>% dplyr::select(-Lon, -Lat)
Variables <- Variables[complete.cases(Variables),]
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

gbmFit2 <-read_rds("ResultadoGBM2.rds")

library(tidyverse)
library(caret)
library(gbm)
library(patchwork)

### Relaciones entre Indice y Distancia a la costa

Var1 <- plot(gbmFit2$finalModel, i.var = 1, return.grid = T)
p1 <-ggplot(Var1, aes(x = Dist_Costa, y = y)) + geom_path() +xlab("Distance to the coast (m)") + ylab("Marine Index")+
  theme_bw() +ggtitle('A')

Var2 <- plot(gbmFit2$finalModel, i.var = 19, return.grid = T)
p2 <-ggplot(Var2, aes(x = KmWalk_Total, y = y)) + geom_path()+xlab("Total walk-able water body coastline 
 (m)") + ylab("Marine Index")+
  theme_bw()+ggtitle('B')

Var3 <- plot(gbmFit2$finalModel, i.var = 15, return.grid = T)
p3 <-ggplot(Var3, aes(x = seabird_prop, y = y)) + geom_path()+xlab("Seabird proportion") + ylab("Marine Index")+
  theme_bw()+ggtitle('C')

Var4 <- plot(gbmFit2$finalModel, i.var = 24, return.grid = T)
p4 <-ggplot(Var4, aes(x = n_viewshed, y = y)) + geom_path() +xlab("Area of the city") + ylab("Marine Index")+
  theme_bw()+ggtitle('D')

Var5 <- plot(gbmFit2$finalModel, i.var = 7, return.grid = T)
p5 <-ggplot(Var5, aes(x = Urban_pop20217, y = y)) + geom_path() +xlab("Urban population in 2017") + ylab("Marine Index")+
  theme_bw() +ggtitle('E')

Var6 <- plot(gbmFit2$finalModel, i.var = 2, return.grid = T)
p6<-ggplot(Var6, aes(x = P_PescadorArcaico, y = y)) + geom_path() +xlab("Archaic fisherman communities probability") + ylab("Marine Index")+
  theme_bw()+ggtitle('F')

Var7 <- plot(gbmFit2$finalModel, i.var = 3, return.grid = T)
p7<-ggplot(Var7, aes(x = P_RecolectorMaritimo, y = y)) + geom_path() +xlab("Maritime gatherers probability") + ylab("Marine Index")+
  theme_bw() +ggtitle('G')#+annotate("text", x=0.75, y=0.01, label="Variable importance= 2.74416536")

Var8 <- plot(gbmFit2$finalModel, i.var = 8, return.grid = T)
p8<-ggplot(Var8, aes(x = Riqueza_Total, y = y)) + geom_path() +xlab("Total richness of birds") + ylab("Marine Index")+
  theme_bw()+ggtitle('H')

Var9 <- plot(gbmFit2$finalModel, i.var = 21, return.grid = T)
p9<-ggplot(Var9, aes(x = Mean, y = y)) + geom_path() +xlab("Mean of blueness visibility index") + ylab("Marine Index")+
  theme_bw()+ggtitle('I')

Var10 <- plot(gbmFit2$finalModel, i.var = 12, return.grid = T)
p10<-ggplot(Var10, aes(x = seabird_richness, y = y)) + geom_path() +xlab("Seabird richness") + ylab("Marine Index")+
  theme_bw()+ggtitle('J')

Var11 <- plot(gbmFit2$finalModel, i.var = 14, return.grid = T)
p11<-ggplot(Var11, aes(x = shallow_water_bird_prop, y = y)) + geom_path() +xlab("Shallow water bird proportion") + ylab("Marine Index")+
  theme_bw()+ggtitle('K')

Var12 <- plot(gbmFit2$finalModel, i.var = 13, return.grid = T)
p12<-ggplot(Var12, aes(x = shorebird_richness, y = y)) + geom_path() +xlab("Shorebird richness") + ylab("Marine Index")+
  theme_bw()+ggtitle('L')

Var13 <- plot(gbmFit2$finalModel, i.var = 10, return.grid = T)
p13 <-ggplot(Var13, aes(x = Proportion_Aquatic, y = y)) + geom_path() +xlab("Aquatic bird proportion") + ylab("Marine Index")+
  theme_bw()+ggtitle('M')

Var14 <- plot(gbmFit2$finalModel, i.var = 16, return.grid = T)
p14<-ggplot(Var14, aes(x = shorebird_prop, y = y)) + geom_path() +xlab("Shorebird proportion") + ylab("Marine Index")+
  theme_bw()+ggtitle('N')

Var15 <- plot(gbmFit2$finalModel, i.var = 9, return.grid = T)
p15<-ggplot(Var15, aes(x = Riqueza_Aquatic, y = y)) + geom_path() +xlab("Aquatic birds richness") + ylab("Marine Index")+
  theme_bw()+ggtitle('O')

Var16 <- plot(gbmFit2$finalModel, i.var = 11, return.grid = T)
p16<-ggplot(Var16, aes(x = shallow_water_bird_richness, y = y)) + geom_path() +xlab("Shallow water bird richness") + ylab("Marine Index")+
  theme_bw()+ggtitle('P')

p1/(p2+p3)
(p4+p5)/(p6+p7)/(p8+p9+p10)/(p11+p12+p13)/(p14+p15+p16)
                    
                     
#########################################################
#Caracteristicas variables

library(tidyverse)

Variables <- read_rds("Variables.rds")
 View(summary(Variables))
 
 
 
 
 Pop <- read.csv("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/ModelacionAsentamientosGit/Pobl_comuna.csv") %>% 
   dplyr::select(Region,Comuna, X2017_urbano) %>% rename(City=Comuna) %>% rename(Urban_pop20217= X2017_urbano)
 
  Pop$City <- janitor::make_clean_names(Pop$City) %>% str_replace_all("_", " ")
 
 
 Variables <- left_join(Variables, Pop)
 

 
 
 
 
 