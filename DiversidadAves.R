# Diversidad de aves en Chile
# Descarga de iNaturalist

library(tidyverse)
library(rworldxtra)
library(sf)
library(rinat)
library(terra)


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



Coords2 <- Coords2[-c(218,219),]



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

Aves <- Temp %>% 
  purrr::map(~dplyr::mutate(.x, description = as.character(description),
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
                            image_url = as.character(image_url),
                            iconic_taxon_name = as.character(iconic_taxon_name),
                            time_observed_at = as.character(time_observed_at))) %>% 
  purrr::reduce(bind_rows)


Classes  <- Temp %>% purrr::map(class)

Logic <- vector()

for(i in 1:length(Classes)){
  Logic[i] <- "sf" %in% Classes[[i]]
}

Aves <- Temp[Logic]

NROWS <- Classes  <- Temp %>% purrr::map(nrow) %>% purrr::reduce(c)

NROWS <- NROWS > 0


Aves <- Aves[NROWS]
Aves <- Aves %>% purrr::reduce(bind_rows)
Aves <- Aves %>% dplyr::filter(iconic_taxon_name %in% c("Aves","Animalia"))

saveRDS(Aves, "Diversidad_aves.rds")

Aves <- readRDS("Diversidad_aves.rds")

library(taxize)


## Especies unicas
Especies <- unique(Aves$scientific_name) %>% sort()

## Resolver taxonomia usando el backbone the GBIF con tazize

Fixed <- taxize::gnr_resolve(sci = Especies, data_source_ids = 11, canonical = T, best_match_only = T, fields = "all")

## Solo retenemos especies de aves y con un score mayor a 0.9
Fixed_Corrected <- Fixed %>% dplyr::filter(str_detect(classification_path, "Aves"), score >= 0.9) %>% 
  dplyr::select(user_supplied_name, matched_name2, classification_path, classification_path_ranks) %>% 
  tidyr::separate(classification_path, into = c("kingdom","phylum","class","order","family","genus","species")) %>% 
  dplyr::select(user_supplied_name, matched_name2, family) %>% 
  dplyr::rename(scientific_name = user_supplied_name)


Aves_Corregida <- Aves %>% 
  right_join(Fixed_Corrected)


Aves_Abundancia <- Aves_Corregida %>%
  as.data.frame() %>% 
  group_by(scientific_name, Ciudad, comuna, family) %>% 
  dplyr::summarise(n = n())

Familias <- Aves_Abundancia %>% 
  ungroup() %>% 
  dplyr::select(family) %>% 
  distinct() %>% 
  arrange(family)

#write_csv(Familias, "Familias_Para_Clasificar.csv")
Familias <- readr::read_csv("Familias_Para_Clasificar.csv") %>% 
  rename(family = Family)

Aves_Abundancia <- Aves_Abundancia %>% left_join(Familias)

Riqueza_Por_Environment <- Aves_Abundancia %>% 
  group_by(Ciudad, Environment) %>% 
  dplyr::summarise(Riqueza_Aquatic = n()) %>% 
  ungroup() %>% 
  dplyr::filter(Environment == "Aquatic") %>% 
  dplyr::select(-Environment)

Riqueza_Por_Ciudad <- Aves_Abundancia %>% 
  group_by(Ciudad) %>% 
  dplyr::summarise(Riqueza_Total = n()) %>% 
  ungroup()

Riqueza_Proporcion <- full_join(Riqueza_Por_Ciudad, Riqueza_Por_Environment) %>% 
  mutate(Riqueza_Aquatic = ifelse(is.na(Riqueza_Aquatic), 0, Riqueza_Aquatic), Proportion_Aquatic = Riqueza_Aquatic/Riqueza_Total)


Aves_Habitat <- Aves_Abundancia %>% 
  group_by(Ciudad, Habitat) %>% 
  dplyr::summarise(Riqueza_Habitat = n()) %>% 
  ungroup() %>% 
  dplyr::filter(Habitat != "Terrestrial") %>% 
  full_join(Riqueza_Por_Ciudad) %>% 
  dplyr::mutate(Porportion_Habitat = Riqueza_Habitat/Riqueza_Total)

Aves_Habitat_Riqueza <- Aves_Habitat %>% 
  dplyr::select("Ciudad", "Habitat", "Riqueza_Habitat") %>% 
  dplyr::filter(!is.na(Habitat)) %>% 
  dplyr::mutate(Habitat = str_replace_all(Habitat, " ", "_"), Habitat = paste0(Habitat, "_richness")) %>% 
  pivot_wider(names_from = Habitat, values_from = Riqueza_Habitat, values_fill = 0)

Aves_Habitat_Prop <- Aves_Habitat %>% 
  dplyr::select("Ciudad", "Habitat", "Porportion_Habitat") %>% 
  dplyr::filter(!is.na(Habitat)) %>% 
  dplyr::mutate(Habitat = str_replace_all(Habitat, " ", "_"), Habitat = paste0(Habitat, "_prop")) %>% 
  pivot_wider(names_from = Habitat, values_from = Porportion_Habitat, values_fill = 0)

Aves_Habitat_Total <- full_join(Aves_Habitat_Riqueza, Aves_Habitat_Prop)

Riqueza_Proporcion_Final <- full_join(Riqueza_Proporcion, Aves_Habitat_Total) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

saveRDS(Riqueza_Proporcion_Final, "Bird_type_prop.rds")

######### 
