library(shiny)
library(leaflet)
library(sf)
library(rnaturalearth)
library(dbscan)
library(cluster)
library(ggplot2)
# Chargement des données géographiques de l'Afrique de l'Ouest
west_africa <- subset(ne_countries(scale = "medium", continent = "Africa"), subregion == "Western Africa")

# Chargement des données depuis le fichier CSV
data <- read.csv("ACLED-Western_Africa.csv")

# Clusterisation des données
clusters <- dbscan(data[, c("latitude", "longitude")], eps = 0.1, minPts = 5)
data$cluster <- as.factor(clusters$cluster)

# Vérification de l'existence de la variable 'annee'
if (!"annee" %in% colnames(data)) {
  stop("La variable 'annee' n'est pas présente dans les données.")
}
# UI
ui <- fluidPage(
  titlePanel("Projet Mouhamadou Moustapha WADE"),
  
  navbarPage(
    "Events by country West Africa",
    
    # onglet - Filtrage des événements
    tabPanel(
      "Filtrage des événements",
      
      sidebarLayout(
        sidebarPanel(
          selectInput("country_filter", "Choisir un pays :", choices = unique(data$pays)),
          br(),
          selectInput("event_type_filter", "Choisir un type d'événement :", choices = unique(data$type)),
          br(),
          sliderInput("year_filter", "Choisir une année :", min = min(data$annee), max = max(data$annee), value = c(min(data$annee), max(data$annee)))
        ),
        mainPanel(
          fluidRow(
            column(
              width = 2
            ),
            column(
              width = 10,
              h3("Carte filtrée"),
              leafletOutput("filtered_map")
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Filtrage des événements
  filteredData <- reactive({
    subset(data, pays == input$country_filter & type == input$event_type_filter & annee >= input$year_filter[1] & annee <= input$year_filter[2])
  })
  
  # Carte filtrée
  output$filtered_map <- renderLeaflet({
    filtered <- filteredData()
    
    leaflet(filtered) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        color = ~cluster,
        opacity = 0.8,
        fillOpacity = 0.8,
        label = ~paste("Pays :", pays, "<br>Type :", type, "<br>Année :", annee),
        clusterOptions = markerClusterOptions()
      )
  })
}

#Lancement de l'application Shiny
shinyApp(ui = ui, server = server)
