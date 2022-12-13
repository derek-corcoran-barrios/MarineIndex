# Diversidad de plantas en Chile
# Descarga de iNaturalist

library(tidyverse)
library(rworldxtra)
library(sf)
library(rinat)
library(terra)

#Poligonos de las ciudades que poseen un indice marino
Coords2 <- readRDS("Poligonos_Ciudades.rds")


#### Descarga de especies a partir de poligonos

Temp <- list()

for(i in 1:nrow(Coords2)){
  Sys.sleep(15)
  BOX <- st_bbox(Coords2[i,]) %>% as.vector()
  BOX <- BOX[c(2,1,4,3)]
  try({
    Test <- rinat::get_inat_obs(geo = T, 
                                #year = 2020, 
                                bounds = BOX, 
                                maxresults = 10000)
    Test <- Test %>% 
      dplyr::filter(str_detect(scientific_name, " "))
    Temp[[i]] <- Test %>% st_as_sf(coords = c("longitude","latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
      st_intersection(Coords2[i,])
    #Areas[[i]]$Richness[i] <- Temp[[i]]$scientific_name %>% unique() %>% length()
    #Areas$n[i] <- nrow(Temp[[i]])
    #saveRDS(Areas, "Areas.rds")
    saveRDS(Temp, "Sample.rds")
  })
  
  message(paste(i, "of", nrow(Coords2), "ready", Sys.time()))
  if((i %% 10) == 0){
    beepr::beep(2)
    #print(paste("Number of species =", Areas$Richness[i]))
    gc()
  }
}  

cond <- vector()

for(i in 1:length(Temp)){
  cond[i] <- "sf" %in% class(Temp[[i]])
}

Temp <- Temp[cond]

Especies <- Temp %>% 
  purrr::map(~dplyr::mutate(.x, 
                            description = as.character(description),
                            tag_list = as.character(tag_list),
                            species_guess = as.character(species_guess),
                            geoprivacy = as.character(geoprivacy),
                            taxon_geoprivacy = as.character(taxon_geoprivacy),
                            sound_url = as.character(sound_url),
                            positioning_method = as.character(positioning_method),
                            positioning_device = as.character(positioning_device),
                            license = as.character(license),
                            scientific_name = as.character(scientific_name),
                            common_name = as.character(common_name),
                            user_name=as.character(user_name),
                            image_url = as.character(image_url),
                            iconic_taxon_name = as.character(iconic_taxon_name),
                            time_observed_at = as.character(time_observed_at))) %>% 
  purrr::reduce(bind_rows)


Classes  <- Temp %>% purrr::map(class)

Logic <- vector()

for(i in 1:length(Classes)){
  Logic[i] <- "sf" %in% Classes[[i]]
}

Especies <- Temp[Logic]

NROWS <- Classes  <- Temp %>% purrr::map(nrow) %>% purrr::reduce(c)

NROWS <- NROWS > 0


Especies <- Especies[NROWS]
Especies <- Especies %>% purrr::reduce(bind_rows)


saveRDS(Especies, "Diversidad_total.rds")




####################################################################
### Ahora arreglo de plantas por grupos

Especies <- readRDS("Diversidad_total.rds")
Plantas <- Especies %>% dplyr::filter(iconic_taxon_name %in% c("Plantae"))

library(taxize)


## Especies unicas
Especies <- unique(Plantas$scientific_name) %>% sort()

## Resolver taxonomia usando el backbone the GBIF con taxize

Fixed <- taxize::gnr_resolve(sci = Especies, data_source_ids = 11, canonical = T, best_match_only = T, fields = "all")

## Solo retenemos especies de plantas y con un score mayor a 0.9
## arreglos de taxonomia desde gbif
Fixed_Corrected <- Fixed %>% dplyr::filter(str_detect(classification_path, "Plantae"), score >= 0.9) %>% 
  dplyr::select(user_supplied_name, matched_name2) %>% 
  dplyr::rename(scientific_name = user_supplied_name)

library(rgbif)

RGBIF_CORRECTED <- rgbif::name_backbone_checklist(Fixed_Corrected$matched_name2) %>% 
  dplyr::select(verbatim_name, family, species) %>% 
  rename(matched_name2 = verbatim_name)


Plantas_Corregida <- Plantas %>% 
  right_join(Fixed_Corrected) %>% 
  right_join(RGBIF_CORRECTED)

### HASTA ACA OJO que SCIENTIFIC NAME PASA A ESPECIE ABAJO

# Abundancia de plantas por ciudad
Plantas_Abundancia <- Plantas_Corregida %>%
  as.data.frame() %>% 
  group_by(species, Ciudad, comuna, family) %>% 
  dplyr::summarise(n = n())


#todas las familias presentes en la base de datos
Familias <- Plantas_Abundancia %>% 
  ungroup() %>% 
  dplyr::select(family) %>% 
  distinct() %>% 
  arrange(family)

#write_csv(Familias, "Familias_Para_Clasificar.csv") FAMILIAS POR AMBIENTE Y HABITAT
#Familias_ <- readr::read_csv("Familias_Para_Clasificar.csv") %>% 
#  rename(family = Family)

#Aves_Abundancia <- Aves_Abundancia %>% left_join(Familias_)

#Riqueza_Por_Environment <- Aves_Abundancia %>% 
#  group_by(Ciudad, Environment) %>% 
#  dplyr::summarise(Riqueza_Aquatic = n()) %>% 
#  ungroup() %>% 
#  dplyr::filter(Environment == "Aquatic") %>% 
#  dplyr::select(-Environment)

Riqueza_Por_Ciudad <- Plantas_Abundancia %>% 
  group_by(Ciudad) %>% 
  dplyr::summarise(Riqueza_Total = n()) %>% 
  ungroup()

saveRDS(Riqueza_Por_Ciudad, "RiquezaPlantas_Ciudad.rds")


#Riqueza_Proporcion <- full_join(Riqueza_Por_Ciudad, Riqueza_Por_Environment) %>% 
#  mutate(Riqueza_Aquatic = ifelse(is.na(Riqueza_Aquatic), 0, Riqueza_Aquatic), Proportion_Aquatic = Riqueza_Aquatic/Riqueza_Total)



######### 
