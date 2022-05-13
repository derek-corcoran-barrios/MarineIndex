#Idea: Indice marino

## Cargo los paquetes necesarios
#.longitude, .latitude
  library(tidytext)
  library(tidyverse)
  library(rvest)
  library(tm)
  
  # Obtengo los nombres de las ciudades desde wikipedia
  
  Chile2 <- read_html("https://es.wikipedia.org/wiki/Anexo:Ciudades_de_Chile") %>% 
    rvest::html_nodes("td:nth-child(2) a") %>% 
    html_attr('title')
  

  # Obtengo los links de esas ciudades desde wikipedia
  
  Chileref2 <- read_html("https://es.wikipedia.org/wiki/Anexo:Ciudades_de_Chile") %>% 
    rvest::html_nodes("td:nth-child(2) a") %>% 
    html_attr('href')
  
  # Genero el dataframe con los links
  Chile_DF <- tibble(Ciudad = Chile2, link = paste0("https://es.wikipedia.org", Chileref2)) %>% 
    distinct() %>% 
    mutate(Ciudad = str_remove_all(Ciudad," de Chile"), Ciudad = str_remove_all(Ciudad," \\(Chile\\)"), Ciudad = str_remove_all(Ciudad," \\(Temuco\\)"), Ciudad = str_remove_all(Ciudad," \\(ciudad\\)"), Ciudad = str_remove_all(Ciudad," \\(aún no redactado\\)"), Ciudad = str_remove_all(Ciudad," \\(localidad\\)"), Ciudad = str_remove_all(Ciudad," \\(comuna\\)"), Ciudad = str_remove_all(Ciudad," \\(Los Andes\\)"), Ciudad = str_remove_all(Ciudad," \\(Lampa\\)"))
  
  # Empiezo un objeto text para llenarlo con el contenido de wikipedia
  
  Texts <- list()
  
  ## Uso las stopwords de tm en español para luego quitarlas del corpus
  
  stopwords <-tibble(word = tm::stopwords("spanish"),
                     lexicon = "custom")
  
  Nuestras_Stop <- tibble(word = c("habitantes", "población", "ciudad", "comuna", "localidad", "atacama", "censo", 
                                   "chile", 
                                   "paipote", "pueblo", "región", "según", "área", 
                                   "perteneciente","huatulame", "parte", 
                                   "capital", "tres", "cuenta",  
                                   "til", "san", "cuales", 
                                   "hombres", "kilómetros", "mujeres", "poblado", 
                                   "sitio", "total", "ubicada", "avenida", "comunas", "metropolitana", 
                                   "zona","españoles", "gran", 
                                   "biobío", "transporte", "francisco", 
                                   "junto", "mostazal", "central", "lugar", 
                                   "chiloé", "provincia", "ñuble", "regional", "título", "aysén", 
                                   "placilla", "calero", "división","flexquoteflexdisplayflexflexdirectionrowmwparseroutput	", 
                                  "nombre", "ˈkɑːkrən" , "flexquotedisplayflexflexdirectioncolumnbackgroundcolorfffborderleftpx",
                                   "calleuque","don", "palena", "colchagua", "ª", 
                                   "alcalde", "araucanía", "cantidad", "cautín", "circunscripción", 
                                   "complejidad"), lexicon = "Nuestro")
  
  stopwords <- bind_rows(stopwords, Nuestras_Stop)
  
  # Para mas detalles en textmining ver el siguiente link
  ##https://www.tidytextmining.com/##
  
  Para_Sacar <- str_c(Chile_DF$Ciudad, collapse = "|")
  Lat <- list()
  Lon <- list()
  
  Coords <- list()
  
  for(i in 1:nrow(Chile_DF)){
    # para cada fila (Ciudad)
    ## Leo el html
    Temp <- read_html(Chile_DF$link[i]) %>%
      # Extraigo los parrafos (Sin titulos)
      html_nodes("p") %>% 
      # lo tranformo en texto
      html_text() %>% 
      str_remove_all(Para_Sacar)
    
    Lat[[i]] <- read_html(Chile_DF$link[i]) %>%
      # Extraigo los parrafos (Sin titulos)
      html_nodes(".latitude") %>% 
      # lo tranformo en texto
      html_text() 
    
    Lat[[i]] <- Lat[[i]] %>% str_remove_all(",") %>% as.numeric()
    Lat[[i]] <- Lat[[i]][!is.na(Lat[[i]])]
    Lat[[i]] <- Lat[[i]][1]
    
    Lon[[i]] <- read_html(Chile_DF$link[i]) %>%
      # Extraigo los parrafos (Sin titulos)
      html_nodes(".longitude") %>% 
      # lo tranformo en texto
      html_text() 
    
    Lon[[i]] <- Lon[[i]] %>% str_remove_all(",") %>% as.numeric()
    Lon[[i]] <- Lon[[i]][!is.na(Lon[[i]])]
    Lon[[i]] <- Lon[[i]][1]
    
    Coords[[i]] <- tibble(Ciudad = Chile_DF$Ciudad[i],
                          Lon = Lon[[i]],
                          Lat = Lat[[i]])
    
    
    
    ## Para esa ciudad genero un dataframe conlos parrafos y el nombre de ciudad
    Texts[[i]] <- tibble(Paragraph = Temp, City = Chile_DF$Ciudad[i]) %>%
      # Saco numeros y simbolos
      mutate(Paragraph = gsub(x = Paragraph, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = "")) %>% 
      ## Hago una fila por palabra
      unnest_tokens(word, Paragraph) %>% 
      ## QUito los stopwords
      anti_join(stopwords)  %>%
      ## Agrupo por palabra y ciudad
      group_by(word, City) %>% 
      ## Obtengo el numero de veces que aparece cada palabra y lo ordeno
      summarise(n = n()) %>% 
      # Lo pongo en orden descendiente
      arrange(desc(n)) %>% 
      # Desagrupo
      ungroup %>%
      ## Calculo la frecuencia de cada palabra
      mutate(Total = sum(n), Freq = n/Total)
     message(paste("Ciudad", Chile_DF$Ciudad[i], "lista,", i, "de", nrow(Chile_DF)))
  }  
  
  ## Uno todas las ciudades
  Texts <- Texts %>% reduce(bind_rows)
  
  Coords <- Coords %>% reduce(bind_rows)
  
  saveRDS(Texts, "Texts.rds")
  saveRDS(Coords, "CoordsCiudades.rds")

  ################################################################################ Indice marino  
  ## Filtramos localidades que tienen al menos 500 palabras
  
  Texts <- read_rds("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/MarineIndex/Texts.rds") %>% 
    dplyr::filter(Total >= 500)
  
  Palabras_Unicas <- tibble(word = unique(Texts$word), Conexion_Marina = NA) %>% 
    mutate(Conexion_Marina = case_when(str_detect(word,"pesc") ~ 1,
                                       str_detect(word, "^marin") ~ 1,
                                       str_detect(word, "surf") ~ 1,
                                       word == "^marinada" ~ 0,
                                       word == "marin"~ 0,
                                       word == "oceanográfico" ~ 1,
                                       word == "mar" ~ 1,
                                       str_detect(word, "^play") ~ 1,
                                       str_detect(word, "^puerto") ~ 1,
                                       word == "ciudadpuerto" ~ 1,
                                       word == "bordemar" ~ 1,
                                       str_detect(word, "^isla") ~ 1,
                                       str_detect(word,"naveg") ~ 1,
                                       word == "nadar" ~ 1,
                                       str_detect(word, "^naval") ~ 1,
                                       str_detect(word,"aeronaval") ~ 1,
                                       str_detect(word,"mareal") ~ 1,
                                       str_detect(word,"marisc") ~ 1,
                                       str_detect(word,"embarca") ~ 1,
                                       str_detect(word, "^barca") ~ 1,
                                       str_detect(word,"barco") ~ 1,
                                       str_detect(word,"caleta") ~ 1,
                                       str_detect(word,"balneario") ~ 1,
                                       str_detect(word,"roquerío") ~ 1,
                                       str_detect(word,"marejada") ~ 1,
                                       str_detect(word,"tsunami") ~ 1,
                                       word == "maremoto" ~ 1,
                                       word == "pez" ~ 1,
                                       word == "peces" ~ 1,
                                       str_detect(word,"ballen") ~ 1,
                                       str_detect(word,"ballenar") ~ 0,
                                       str_detect(word,"portua") ~ 1,
                                       str_detect(word,"oleaje") ~ 1,
                                       word == "faro" ~ 1,
                                       word == "faros" ~ 1,
                                       str_detect(word, "^duna") ~ 1,
                                       word == "dunalastair" ~ 0,
                                       word == "farellón" ~ 1,
                                       str_detect(word, "^balneario") ~ 1,
                                       str_detect(word, "^bahía") ~ 1,
                                       word == "golfo" ~ 1,
                                       str_detect(word, "^litoral") ~ 1,
                                       str_detect(word,"coster") ~ 1,
                                       word == "andinocostero	" ~ 0,
                                       word == "ola	" ~ 1,
                                       word == "olas	" ~ 1,
                                       str_detect(word, "^marítim") ~ 1
                                       
                                       #TRUE ~ 0
                                       ))
  
  saveRDS(Palabras_Unicas, "Unicas.rds")
  
    Texts <- full_join(Texts, Palabras_Unicas) %>% 
      dplyr::filter(!is.na(Conexion_Marina)) %>% 
      mutate(Index = Conexion_Marina*Freq) %>% 
      group_by(City) %>% 
      summarise(Index = sum(Index)) %>% 
      mutate(Index = Index/max(Index)) %>% 
      arrange(Index)

  saveRDS(Texts, "Indice.rds")
  
###################

## Ejemplo idea grafico de suma de frecuencias de palabras que empiezan con Pesc

Texts <- readRDS("Texts.rds")
    
Pesc <- Texts %>% 
  dplyr::filter(str_detect(string = word, pattern = "pesc")) %>% 
  group_by(City) %>% 
  summarise(Index = sum(Freq)) %>% 
  ungroup() %>% 
  mutate(Index = Index/max(Index)) %>% 
  slice_max(order_by = Index, n = 15) %>% 
  mutate(City = fct_reorder(City, Index))


ggplot(Pesc, aes(x = City, y = Index)) + geom_col() + coord_flip()+
  xlab("Ciudad") + ylab("Indice de conexión con el mar") +theme_classic()

## Ejemplo idea grafico de suma de frecuencias de palabras que empiezan con Marin

Marin <- Texts %>% 
  dplyr::filter(str_detect(string = word, pattern = "marin")) %>% 
  group_by(City) %>% 
  summarise(Index = sum(Freq)) %>% 
  ungroup() %>% 
  mutate(Index = Index/max(Index)) %>% 
  slice_max(order_by = Index, n = 15) %>% 
  mutate(City = fct_reorder(City, Index))


ggplot(Marin, aes(x = City, y = Index)) + geom_col() + coord_flip() +
  xlab("Ciudad") + ylab("Indice de conexión con el mar") +theme_classic()

## Generacion capa de distancia a la costa

library(raster)
library(tidyverse)

SA <- getData(name = "worldclim", var = "tmin", res = 2.5)
SA <- SA[[1]]

e <- new("Extent", xmin = -87.4234279284911, xmax = -36.7920990123765, 
         ymin = -61.1848485817944, ymax = 9.4573077351334)

SA <- crop(SA, e)

values(SA) <- ifelse(is.na(values(SA)), 1, NA)

SA <- distance(SA)

saveRDS(SA, "DistCosta_SA.rds")

##########
# Extraccion de valores para las ciudades de chile

Coords <- read_rds("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/ModelacionAsentamientosGit/CoordsCiudades.rds") %>% dplyr::filter(!is.na(Lon))
DistCosta <- readRDS("/home/giorgia/Documents/Doctorado tesis/Mod.distrib/ModelacionAsentamientosGit/DistCosta_SA.rds")
Puntos <-SpatialPoints(coords = Coords[c("Lon", "Lat")], proj4string=CRS(as.character("+proj=longlat +datum=WGS84 +no_defs")))

Valores_DistCosta <- data.frame(Dist_Costa = raster::extract(DistCosta, Puntos, method = "bilinear"))
Valores_DistCosta <- bind_cols(Coords, Valores_DistCosta)

saveRDS(Valores_DistCosta, "DistCosta_Chile.rds")



