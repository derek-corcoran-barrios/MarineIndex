## Este script se realizó para generar polígonos de las ciudades ya seleccionadas que poseen un valor de 
## indice marino

##


library(sf)
library(tidyverse)
library(terra)
library(geodata)
library(rGEDI)


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

## Corregir Iquique

Iquique <- dplyr::filter(Coords2, Ciudad == "iquique") %>% 
  tibble::rowid_to_column() %>% 
  dplyr::filter(rowid == 12) %>% 
  dplyr::select(-rowid)

Coords2 <- dplyr::filter(Coords2, Ciudad != "iquique") %>% 
  dplyr::bind_rows(Iquique)

rm(Urban)
rm(Coords)
gc()
#######################################
# Ahora para cada ciudad obtendremos la altura


dir.create("Alturas_Ciudades")
dir.create("Alturas_Ciudades/DEM")

for(i in 1:nrow(Coords2)){
  Poligono <- Coords2[i,] %>% st_buffer(dist = 1000) %>% terra::vect()
  Centroid <- Coords2[i,] %>% st_centroid() %>% st_coordinates() %>% as.data.frame()
  DEM <- geodata::elevation_3s(lon = Centroid$X, lat = Centroid$Y, path = getwd())
  DEM <- DEM %>% terra::crop(Poligono) %>% terra::mask(Poligono)
  background <- terra::rasterize(Poligono, DEM, field = 0)
  DEM[is.na(DEM)] <- 0
  DEM <- (DEM + background)
  plot(DEM, colNA = "black", main = Coords2$Ciudad[i])
  terra::writeRaster(DEM, paste0("Alturas_Ciudades/DEM/", Coords2$Ciudad[i], ".tif"), overwrite = T)
  message(paste("Ciudad", i, "=", Coords2$Ciudad[i], "de", nrow(Coords2), "lista!"))
}

### Corregir santiago

Coords2 <- Coords2[-c(218,219),]


dir.create("Alturas_Ciudades/DSM")


for(i in 1:nrow(Coords2)){
  Poligono <- Coords2[i,] %>% st_buffer(dist = 1000)
  BBOX <- Poligono %>% st_bbox() %>% as.numeric()
  ul_lat<- BBOX[4] 
  lr_lat<- BBOX[2]
  lr_lon<- BBOX[3] 
  ul_lon<- BBOX[1]
  daterange=c("2015-01-01","2022-04-01")
  gLevel2A <-gedifinder(product="GEDI02_A",ul_lat, ul_lon, lr_lat, lr_lon,version="002",daterange=daterange)
  outdir= "/home/giorgia/Documents/Doctorado tesis/Mod.distrib/MarineIndex/gedilayers"
  gediDownload(filepath=gLevel2A,outdir=outdir, timeout = 50)
  
  Elevs <- list()
  
  for(j in 1:length(gLevel2A)){
    Temp <- gLevel2A[j] %>% str_split(pattern = "/", simplify = T) %>% as.vector()
    Temp <- Temp[length(Temp)]
    Temp <- paste0("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/MarineIndex/gedilayers/", Temp)
    gedilevel2a<-readLevel2A(level2Apath = Temp)
    level2AM<-getLevel2AM(gedilevel2a)
    level2AM <- level2AM %>% 
      dplyr::select("beam", "degrade_flag", "quality_flag", "solar_elevation", "lat_lowestmode", "lon_lowestmode", 
                    "elev_highestreturn", "elev_lowestmode") %>% 
      dplyr::filter(lat_lowestmode >= lr_lat, lat_lowestmode <= ul_lat) %>% 
      dplyr::filter(lon_lowestmode >= ul_lon, lon_lowestmode <= lr_lon) %>% 
      dplyr::filter(quality_flag == 1)
    Elevs[[j]] <- level2AM %>% as.data.frame()
    gc()
    message(paste("layer", j, "of", length(gLevel2A), "Ready!"))
    gc()
  }
  
  Elevs <- Elevs %>% 
    purrr::reduce(bind_rows)
  Elevs <- Elevs %>% 
    dplyr::select("lat_lowestmode", "lon_lowestmode", "elev_highestreturn", "elev_lowestmode") %>% 
    rowwise() %>% 
    mutate(Elevation = median(c(elev_highestreturn, elev_lowestmode))) %>% 
    dplyr::select("lat_lowestmode", "lon_lowestmode", "Elevation")
  
  library(sf)
  
  Elevs <- Elevs %>% 
    st_as_sf(coords= c("lon_lowestmode", "lat_lowestmode"), crs ='+proj=longlat +datum=WGS84' ) 
  write_sf(Elevs, paste0("Alturas_Ciudades/DSM/", Coords2$Ciudad[i],".shp"))
  message(paste("Ciudad", i, "=", Coords2$Ciudad[i], "de", nrow(Coords2), "lista!", Sys.time()))
}
