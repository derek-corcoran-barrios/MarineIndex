library(tidytext)
library(tidyverse)
library(rvest)

Chile2 <- read_html("https://es.wikipedia.org/wiki/Anexo:Ciudades_de_Chile") %>% 
  rvest::html_nodes("td:nth-child(2) a") %>% 
  html_attr('title')

Chileref2 <- read_html("https://es.wikipedia.org/wiki/Anexo:Ciudades_de_Chile") %>% 
  rvest::html_nodes("td:nth-child(2) a") %>% 
  html_attr('href')


Chile_DF <- tibble(Ciudad = Chile2, link = paste0("https://es.wikipedia.org", Chileref2)) %>% distinct()

Texts <- list()

stopwords <-tibble(word = tm::stopwords("spanish"),
                   lexicon = "custom")

for(i in 1:nrow(Chile_DF)){
  Temp <- read_html(Chile_DF$link[i]) %>% 
    html_nodes("p") %>% 
    html_text()
  Texts[[i]] <- tibble(Paragraph = Temp, City = Chile_DF$Ciudad[i]) %>%
    mutate(Paragraph = gsub(x = Paragraph, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = "")) %>% 
    unnest_tokens(word, Paragraph) %>% 
    anti_join(stopwords)  %>% 
    group_by(word, City) %>% 
    summarise(n = n()) %>% 
    arrange(desc(n)) %>% 
    ungroup %>% 
    mutate(Total = sum(n), Freq = n/Total)
   message(paste("Ciudad", Chile_DF$Ciudad[i], "lista,", i, "de", nrow(Chile_DF)))
}  

Texts <- Texts %>% reduce(bind_rows)
  
saveRDS(Texts, "Texts.rds")


