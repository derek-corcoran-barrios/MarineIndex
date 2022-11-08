  #
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Clasificación de palabras para Indice Marino"),

    # Sidebar with a slider input for number of bins 
            textOutput("Word"),
    
            radioButtons("Tipo", "Tipo de palabra", choices = c("Marino" = 1, "Neutro" = 0, "Terrestre" = -1)),
    
        actionButton("ToDo","Asignar"),
           textOutput("Per"),
           dataTableOutput("Table")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    id <- character(0)
    n <- 0
    
    observeEvent(input$ToDo, {
        id <- showNotification(paste("Message",reactiveA(), "=",input$Tipo, n), duration = NULL)
        n <<- n + 1
        DF <- data()
        DF[DF$word == reactiveA(),2] <- as.numeric(input$Tipo)
        saveRDS(DF, "Unicas.rds")
    })
    
    data <- reactiveFileReader(1000,
                               session,
                                filePath = "Unicas.rds",
                            readFunc =  readRDS)
    
    
    output$Per <-renderText({
        DF <- data()
        Per <- round((nrow(dplyr::filter(DF, !is.na(Conexion_Marina)))/nrow(DF))*100,2)
        paste0(Per, "% listo, ", nrow(dplyr::filter(DF, !is.na(Conexion_Marina))), " de ", nrow(DF))
    })
    
    
    reactiveA <- reactive({
        DF <- data() %>% dplyr::filter(is.na(Conexion_Marina))
        index <- sample(x = 1:nrow(DF), 1)
        DF <- DF[index,] %>% pull(word)
    })
    
    output$Word <- renderText({
        as.character(reactiveA())
    })
    
    output$Table <- renderDataTable({
        DF <- data()
        Texts <- readRDS("Texts.rds")
        full_join(Texts, DF) %>% 
            dplyr::filter(!is.na(Conexion_Marina)) %>% 
            mutate(Index = Conexion_Marina*Freq) %>% 
            group_by(City) %>% 
            summarise(Index = sum(Index)) %>% 
            mutate(Index = Index/max(Index)) %>% 
            arrange(desc(Index))
    })
    
    
    
    

}

# Run the application 
shinyApp(ui = ui, server = server)
