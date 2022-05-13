#Rgedi

#####################
## Installation

#The CRAN version:
#install.packages("rGEDI")

#The development version:
#library(devtools)
#devtools::install_git("https://github.com/carlos-alberto-silva/rGEDI", dependencies = TRUE)

#dependencies packages: terra,  lidR (depende de terra tb)

# loading rGEDI package
library(rGEDI)

# otros paquetes
library(dplyr)
library(stringr)


##################### Find GEDI data within study area (GEDI finder tool)

# Study area boundary box coordinates: Chile Area
# ul_lat<- -17.30 
# lr_lat<- -60.00
# ul_lon<- -80.00
# lr_lon<- -65.00 

#Area de estudio Valparaiso

ul_lat<- -32.902345
lr_lat<- -33.102865
lr_lon<- -71.474957
ul_lon<- -71.727533


# Specifying the date range
daterange=c("2015-01-01","2022-04-01")

# Get path to GEDI data
#gLevel1B<-gedifinder(product="GEDI01_B",ul_lat, ul_lon, lr_lat, lr_lon,version="002",daterange=daterange)
gLevel2A <-gedifinder(product="GEDI02_A",ul_lat, ul_lon, lr_lat, lr_lon,version="002",daterange=daterange)
#gLevel2B<-gedifinder(product="GEDI02_B",ul_lat, ul_lon, lr_lat, lr_lon,version="002",daterange=daterange)


##### Test para una ventana mas chica de tiempo

#daterangeTest=c("2019-07-01","2019-08-22")
#gLevel2ATest <-gedifinder(product="GEDI02_A",ul_lat, ul_lon, lr_lat, lr_lon,version="002",daterange=daterangeTest)


#################### Downloading GEDI data


# Set output dir for downloading the files
dir.create("gedilayers")
outdir= paste0(getwd(),"/gedilayers")

Downloaded <- list.files(pattern = ".h5")[str_detect(list.files(pattern = ".h5"), "curltmp", negate = T)]


# Downloading GEDI data

#gediDownload(filepath=gLevel1B,outdir=outdir)
#gediDownload(filepath=gLevel2A,outdir=outdir)
#gediDownload(filepath=gLevel2B,outdir=outdir)
gediDownload(filepath=gLevel2A,outdir=outdir, timeout = 30)


################### Reading GEDI data

# Reading GEDI data

gedilevel2a<-readLevel2A(level2Apath = "gedilayers/GEDI02_A_2020144084705_O08185_04_T02302_02_003_01_V002.h5")


############### Get GEDI Elevation and Height Metrics (GEDI Level2A)

# Get GEDI Elevation and Height Metrics
level2AM<-getLevel2AM(gedilevel2a)
#head(level2AM[,c("beam","shot_number","elev_highestreturn","elev_lowestmode","rh100")])



level2AM <- level2AM %>% 
  dplyr::select("beam", "degrade_flag", "quality_flag", "solar_elevation", "lat_lowestmode", "lon_lowestmode", 
                "elev_highestreturn", "elev_lowestmode")

library(sf)

level2AM <- level2AM %>% 
  st_as_sf(coords= c("lon_lowestmode", "lat_lowestmode"), crs ='+proj=longlat +datum=WGS84' ) %>% 
  st_crop(xmin = -75.65, xmax = -66.95, ymin = -57, ymax = -17.5)

dir.create("GeneratedRasters")

library(stars)
level2AMRaster <- stars::st_rasterize(level2AM)

#ul_lat<- -17.58 
#lr_lat<- -20.00
#ul_lon<- -66.95
#lr_lon<- -75.65 
  

##          beam       shot_number elev_highestreturn elev_lowestmode rh100
##  1: BEAM0000 19640002800109382           740.7499        736.3301  4.41
##  2: BEAM0000 19640003000109383           756.0878        746.7614  9.32
##  3: BEAM0000 19640003200109384           770.3423        763.1509  7.19
##  4: BEAM0000 19640003400109385           775.9838        770.6652  5.31
##  5: BEAM0000 19640003600109386           777.8409        773.0841  4.75
##  6: BEAM0000 19640003800109387           778.7181        773.6990  5.01

# Converting shot_number as "integer64" to "character"
#level2AM$shot_number<-paste0(level2AM$shot_number)

#library(sp)
# Converting Elevation and Height Metrics as data.table to SpatialPointsDataFrame
#level2AM_spdf<-SpatialPointsDataFrame(cbind(level2AM$lon_lowestmode,level2AM$lat_lowestmode),
#                                      data=level2AM)

# Exporting Elevation and Height Metrics as ESRI Shapefile
#raster::shapefile(level2AM_spdf,paste0(outdir,"\\GEDI02_A_2019108080338_O01964_T05337_02_001_01_sub"))

#level2AM_spdf %>% sf::st_as_sf() %>% sf::write_sf()


library(leaflet)

leaflet() %>%
  addCircleMarkers(data = level2AM,
                   radius = 1,
                   opacity = 1,
                   color = "red")  %>%
  addScaleBar(options = list(imperial = FALSE)) %>%
  addProviderTiles(providers$Esri.WorldImagery)  %>%
  addLegend(colors = c("red","green"), labels= c("All samples","Clip bbox"),title ="GEDI Level2A") 


