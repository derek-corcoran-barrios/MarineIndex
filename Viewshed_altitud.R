### Viewshed para ciudades de Chile

# Uso de capa de altura de ciudades (DEM) y limite costero 

library(sf)
library(terra)
library(sfheaders)
library(GVI)

## Hacemos lista de ciudades

Ciudades <- list.files("Alturas_Ciudades/DEM/", full.names = T)
Names <- list.files("Alturas_Ciudades/DEM/", full.names = F) %>% str_remove_all(".tif")
### Para leer alturas DEM

CRS <- "PROJCRS[\"WGS 84 / UTM zone 19S\",\n    BASEGEOGCRS[\"WGS 84\",\n        ENSEMBLE[\"World Geodetic System 1984 ensemble\",\n            MEMBER[\"World Geodetic System 1984 (Transit)\"],\n            MEMBER[\"World Geodetic System 1984 (G730)\"],\n            MEMBER[\"World Geodetic System 1984 (G873)\"],\n            MEMBER[\"World Geodetic System 1984 (G1150)\"],\n            MEMBER[\"World Geodetic System 1984 (G1674)\"],\n            MEMBER[\"World Geodetic System 1984 (G1762)\"],\n            MEMBER[\"World Geodetic System 1984 (G2139)\"],\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]],\n            ENSEMBLEACCURACY[2.0]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4326]],\n    CONVERSION[\"UTM zone 19S\",\n        METHOD[\"Transverse Mercator\",\n            ID[\"EPSG\",9807]],\n        PARAMETER[\"Latitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8801]],\n        PARAMETER[\"Longitude of natural origin\",-69,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"Scale factor at natural origin\",0.9996,\n            SCALEUNIT[\"unity\",1],\n            ID[\"EPSG\",8805]],\n        PARAMETER[\"False easting\",500000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",10000000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Engineering survey, topographic mapping.\"],\n        AREA[\"Between 72°W and 66°W, southern hemisphere between 80°S and equator, onshore and offshore. Argentina. Bolivia. Brazil. Chile. Colombia. Peru.\"],\n        BBOX[-80,-72,0,-66]],\n    ID[\"EPSG\",32719]]"

VBVI <- list()

for(x in 1:length(Ciudades)){
  try({
    CiudadsRast <- rast(Ciudades[x]) %>% project(CRS, method = "bilinear")
    
    RES1 <- res(CiudadsRast)[1]
    RES2 <- round(res(CiudadsRast)[1], -1)
    
    e <- round(ext(CiudadsRast))
    CiudadsRast2 <- terra::rast(crs = crs(CiudadsRast),
                                xmin =  e[1],
                                xmax = e[2],
                                ymin = e[3],
                                ymax = e[4],
                                resolution = RES1)
    CiudadsRast2[] <- terra::values(CiudadsRast, mat = FALSE)
    
    CiudadsRast3 <- terra::rast(crs = crs(CiudadsRast),
                                xmin =  e[1],
                                xmax = e[2],
                                ymin = e[3],
                                ymax = e[4],
                                resolution = RES2)
    
    CiudadsRast2<- resample(CiudadsRast2, CiudadsRast3, method = "bilinear")
    
    
    NoSea <- CiudadsRast2
    NoSea[NoSea <= 0] <- NA
    
    m <- c(-Inf, 0, 1,
           0, Inf, 0)
    rclmat <- matrix(m, ncol=3, byrow=TRUE)
    Sea <- classify(CiudadsRast2, rclmat, include.lowest=FALSE)
    
    set.seed(2022)
    
    Ncell <- NoSea %>% values() %>% as.vector() %>% na.omit() %>% length() 
    
    Observer <- spatSample(NoSea, size = round(Ncell*0.2), method= "regular", na.rm=T, as.df=FALSE, as.points=TRUE, values=TRUE) %>% 
      st_as_sf() %>% 
      st_transform(crs = 32719)
    
    VBVI[[x]] <- vgvi_from_sf(observer = Observer,
                              dsm_rast = CiudadsRast2, dtm_rast = CiudadsRast2, greenspace_rast = Sea,
                              max_distance = 1000, observer_height = 1.7,
                              m = 0.5, b = 8, mode = "logit") %>% mutate(Ciudad = Names[x]) %>% 
      as.data.frame() %>% 
      group_by(Ciudad) %>% 
      summarise(Median = median(VGVI, na.rm = T), Mean = mean(VGVI, na.rm = T), Quant_90 = quantile(VGVI, probs = c(0.9), na.rm = T), Quant_80 = quantile(VGVI, probs = c(0.8), na.rm = T), n = n())
    
  })
  message(paste(x, "of", length(Names)))
}


VBVI <- purrr::reduce(VBVI, bind_rows)

VBVI <- VBVI %>%  rename(n_viewshed = n)

saveRDS(VBVI, "viewshed_altitid.rds")
