#ENCUESTA NACIONAL DE EMPLEO: base datos empleo pescadores

# Se seleccionan 4 bases de datos que corresponde a trimestres del año
# Desde enero a dic 2016, ya que hasta ese año se consideró a la pesca como una actividad por separado

library(tidyverse)
library(readr)

EFM <- read.csv("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/ModelacionAsentamientosGit/INE/Encuesta_empleo 2020/ene-2016-02.csv")
AMJ <- read_csv("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/ModelacionAsentamientosGit/INE/Encuesta_empleo 2020/ene-2016-05.csv")
JAS <- read.csv("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/ModelacionAsentamientosGit/INE/Encuesta_empleo 2020/ene-2016-08.csv")
OND <- read.csv("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/ModelacionAsentamientosGit/INE/Encuesta_empleo 2020/ene-2016-11.csv")

#Pescadores:B13- rama de la actividad económica de la empresa que le paga al ocupado
EFM_b13 <- EFM %>% dplyr::filter(b13 ==2)
AMJ_b13 <- AMJ %>% dplyr::filter(b13 ==2)
JAS_b13 <- JAS %>% dplyr::filter(b13 ==2)
OND_b13 <- OND %>% dplyr::filter(b13 ==2)

#Pescadores:B14- rama de la actividad económica de la empresa donde trabaja el ocupado
EFM_b14 <- EFM %>% dplyr::filter(b14 ==2)
AMJ_b14 <- AMJ %>% dplyr::filter(b14 ==2)
JAS_b14 <- JAS %>% dplyr::filter(b14 ==2)
OND_b14 <- OND %>% dplyr::filter(b14 ==2)

#Pescadores:E18- rama o actividad económica ... otro (no se encontró definicion para este item)
EFM_e18 <- EFM %>% dplyr::filter(e18 ==2)
AMJ_e18 <- AMJ %>% dplyr::filter(e18 ==2)
JAS_e18 <- JAS %>% dplyr::filter(e18 ==2)
OND_e18 <- OND %>% dplyr::filter(e18 ==2)

#juntando la info pescadores
ENE_pescadores <- bind_rows(EFM_b13, AMJ_b13, JAS_b13, OND_b13, 
                            EFM_b14, AMJ_b14, JAS_b14, OND_b14,
                            EFM_e18, AMJ_e18, JAS_e18, OND_e18)


saveRDS(ENE_pescadores, "ENE_pescadores.rds")
#################################################

ENE_pescadores <- read_rds("ENE_pescadores.rds") %>% dplyr::select(region, r_p_c)

#r_p_c corresponde al codigo unico territorial

r_c_p <- read.csv("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/ModelacionAsentamientosGit/INE/Encuesta_empleo 2020/Codigos r_p_c.csv") %>% 
  tidyr::unite("Ciudad", Nombre:X, na.rm = T, sep = " ") %>% rename(r_p_c = Código)

ENE <-left_join(ENE_pescadores, r_c_p)

ENE <- ENE %>% group_by(region, Ciudad) %>% summarise(n = n())

saveRDS(ENE, "Pescadores_ciudad_ENE.rds")




