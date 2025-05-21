library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(plotly)

qualite <- read.table("qualite-de-lair-dans-le-reseau-de-transport-francilien (1).csv", header = TRUE, sep = ";", quote = "\"")

qualite_clean <- qualite %>%
  filter(rowSums(is.na(.)) != ncol(.)) %>%
  select(where(~ !all(is.na(.)))) %>%
  distinct()


qualite_metro <- qualite_clean %>%
  filter(grepl("Métro", Nom.de.la.ligne))

degres_pollution <- c("pollution faible" = 1, "pollution moyenne" = 2, "pollution élevée" = 3, "station aérienne" = NA)
qualite_metro$niveau_pollution <- degres_pollution[qualite_metro$niveau_pollution]

qualite_metro$Niveau_Label <- factor(qualite_metro$niveau_pollution,
                                     levels = c(1, 2, 3),
                                     labels = c("Faible", "Moyenne", "Élevée"))

ui <- fluidPage(
  titlePanel("Dashboard qualité de l'air - Métro parisien"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("ligne_filter", "Filtrer par ligne :", choices = c("Toutes", unique(qualite_metro$Nom.de.la.ligne)), selected = "Toutes"),
      hr(),
      h4("Résumé :"),
      verbatimTextOutput("resume_text")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Carte pollution", leafletOutput("map", height = 400)),
        tabPanel("Répartition pollution", plotlyOutput("barplot")),
        tabPanel("Top stations polluées", tableOutput("top_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    if (input$ligne_filter == "Toutes") {
      qualite_metro
    } else {
      filter(qualite_metro, Nom.de.la.ligne == input$ligne_filter)
    }
  })
  
  output$map <- renderLeaflet({
    leaflet(filtered_data()) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~stop_lon,
        lat = ~stop_lat,
        color = ~case_when(
          niveau_pollution == 1 ~ "green",
          niveau_pollution == 2 ~ "orange",
          niveau_pollution == 3 ~ "red"
        ),
        radius = 10,
        label = ~paste0(Nom.de.la.Station, " - ", Niveau_Label)
      )
  })
  
  output$barplot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Niveau_Label, fill = Niveau_Label)) +
      geom_bar() +
      labs(title = "Répartition des niveaux de pollution", x = "Niveau de pollution", y = "Nombre de stations") +
      scale_fill_manual(values = c("green", "orange", "red")) +
      theme_minimal()
    ggplotly(p)
  })
  
  output$top_table <- renderTable({
    filtered_data() %>%
      filter(!grepl("pas de données", Nom.de.la.Station)) %>%
      arrange(desc(niveau_pollution)) %>%
      select(Nom.de.la.Station, Nom.de.la.ligne, Niveau_Label)
  })
  
  output$resume_text <- renderText({
    paste("Stations affichées :", nrow(filtered_data()), "\n",
          "Stations avec pollution élevée :", sum(filtered_data()$niveau_pollution == 3))
  })
}

shinyApp(ui, server)
