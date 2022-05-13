#Belleza escenica, analisis Viewshed

#remotes::install_git("https://github.com/STBrinkmann/GVI")

library(tidyverse)
library(raster)



#Capa landcover para Valparaíso

LandCover <- readRDS("~/Documents/Doctorado tesis/Monitoreo aves/Capas respaldo/LandCoverVRegion.rds")
#+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs
Altura <- read_rds("~/Documents/Doctorado tesis/Monitoreo aves/Capas respaldo/Alt.rds")
#+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0
Borde_costa_masc <- read_rds("~/Documents/Doctorado tesis/Monitoreo aves/Capas respaldo/Borde_costa_masc.rds")
#+proj=longlat +datum=WGS84 +no_defs 

#Data

#For the first two examples we will use a Digital Elevation Model (DEM), a binary Greenspace Mask 
#based on a land cover classification and a Digital Surface Model (DSM). The DSM is generated from 
#LiDAR data collected in 2013. A detailed explanation on how the DSM has been generated using R is 
#provided in this tutorial. To reduce the size of the R package, the sample data has been uploaded 
#to a separate GitHub repository and needs to be downloaded first.

#Load DSM, DEM and Greenspace Mask, and generate the observer location as a sf POINT feature.

# Load libraries. if one is not installed, use the "install.packages()" function
library(terra)
library(sf)
library(sfheaders)
library(raster)

# Load raster objects
DEM <- rast(Altura) %>% terra::project("+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs")
DSM <- rast(Altura) %>% terra::project("+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs")
GreenSpace <- rast(LandCover) %>% terra::project("+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs", method = "near")



################################################

# Generate single observer point
observer <- st_sf(sf_point(c(-33.047, -71.612)), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
  st_transform(crs = "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs")


#1. Single Point

#Calculate the viewshed for a 200 meters radius around the observers position at 1.7 meters height (eye level).

library(GVI)
vs <- viewshed(observer = observer, dsm_rast = DSM, dtm_rast = DEM, max_distance = 200, observer_height = 1.7, plot = TRUE)



