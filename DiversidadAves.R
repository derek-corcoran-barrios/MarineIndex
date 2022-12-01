# Diversidad de aves en Chile
# Descarga de iNaturalist

library(tidyverse)
library(rworldxtra)
library(sf)
library(rinat)
library(terra)

####################################################################
### Arreglo de aves por grupos

library(taxize)
Aves <- readRDS("Diversidad_total.rds")%>% dplyr::filter(iconic_taxon_name %in% c("Aves","Animalia"))

## Especies unicas
Especies <- unique(Aves$scientific_name) %>% sort()

## Resolver taxonomia usando el backbone the GBIF con taxize

Fixed <- taxize::gnr_resolve(sci = Especies, data_source_ids = 11, canonical = T, best_match_only = T, fields = "all")

## Solo retenemos especies de aves y con un score mayor a 0.9
## arreglos de taxonomia desde gbif
Fixed_Corrected <- Fixed %>% dplyr::filter(str_detect(classification_path, "Aves"), score >= 0.9) %>% 
  dplyr::select(user_supplied_name, matched_name2) %>% 
  dplyr::rename(scientific_name = user_supplied_name)

library(rgbif)

RGBIF_CORRECTED <- rgbif::name_backbone_checklist(Fixed_Corrected$matched_name2) %>% 
  dplyr::select(verbatim_name, family, species) %>% 
  rename(matched_name2 = verbatim_name)


Aves_Corregida <- Aves %>% 
  right_join(Fixed_Corrected) %>% 
  right_join(RGBIF_CORRECTED)


# Abundancia de aves por ciudad
Aves_Abundancia <- Aves_Corregida %>%
  as.data.frame() %>% 
  group_by(scientific_name, Ciudad, comuna, family) %>% 
  dplyr::summarise(n = n())


#todas las familias presentes en la base de datos
Familias <- Aves_Abundancia %>% 
  ungroup() %>% 
  dplyr::select(family) %>% 
  distinct() %>% 
  arrange(family)

#write_csv(Familias, "Familias_Para_Clasificar.csv") FAMILIAS POR AMBIENTE Y HABITAT
Familias_ <- readr::read_csv("Familias_Para_Clasificar.csv") %>% 
  rename(family = Family)

Aves_Abundancia <- Aves_Abundancia %>% left_join(Familias_)

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
