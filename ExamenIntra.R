library(shiny)
library(rvest)
library(dplyr)
library(plotly)
library(ggplot2)

# Fonction pour extraire les données de la page web de la BRH
extract_data <- function(url) {
  tryCatch({
    page <- read_html(url)
    data <- page %>% html_nodes("table") %>% html_table(fill = TRUE)
    return(data)
  }, error = function(e) {
    print("Erreur lors de l'extraction des données.")
    return(NULL)
  })
}

# Fonction pour prétraiter les données
preprocess_data <- function(data) {
  if (is.null(data) || length(data) == 0 || all(sapply(data, is.null))) {
    return(NULL)
  }
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  # Renommer les colonnes
  colnames(data) <- c("Banque", "Achat", "Vente", "Spread")
  # Convertir les valeurs en numérique
  data$Achat <- as.numeric(data$Achat)
  data$Vente <- as.numeric(data$Vente)
  data$Spread <- as.numeric(data$Spread)
  
  # Calculer les colonnes Spread et Vente en raison de la confusion des données
  data$Spread <- data$Spread - data$Vente
  data$Vente <- data$Vente + data$Spread
  
  return(data)
}

# UI du tableau de bord
ui <- fluidPage(
  titlePanel("Taux de change HTG/USD"),
  sidebarLayout(
    sidebarPanel(
      selectInput("graph_select", "Sélectionner un graphique", 
                  choices = c("Spread des taux de change pour chaque banque", 
                              "Comparaison des taux de Vente entre les banques", 
                              "Distribution des taux de change par banque"), 
                  selected = "Spread des taux de change pour chaque banque"),
      # Remplacer "Sélectionner une période" par "Date" et utiliser un seul calendrier
      dateInput("date_range", "Date", value = NULL)
      # Vous pouvez ajouter d'autres éléments interactifs ici si nécessaire
    ),
    mainPanel(
      plotlyOutput("plot_output")
    )
  )
)

# Serveur du tableau de bord
server <- function(input, output, session) {
  auto_refresh <- reactiveTimer(60000)  # Actualisation toutes les minutes
  
  # Extraction des données du lien
  data <- reactive({
    auto_refresh()
    extract_data("https://www.brh.ht/taux-affiches-par-les-banques-et-les-agents-de-change-2/")
  })
  
  # Prétraitement des données
  preprocessed_data <- reactive({
    preprocess_data(data())
  })
  
  # Filtrage des données en fonction de la sélection de la banque et de la période
  filtered_data <- reactive({
    req(preprocessed_data())
    data <- preprocessed_data()
    return(data)
  })
  
  output$plot_output <- renderPlotly({
    switch(input$graph_select,
           "Spread des taux de change pour chaque banque" = {
             p <- ggplot(data = filtered_data(), aes(x = Banque, y = Spread, fill = Banque)) +
               geom_bar(stat = "identity") +
               labs(x = "Banque", y = "Spread", title = "Spread des taux de change pour chaque banque") +
               theme_minimal() +
               theme(axis.text.x = element_text(angle = 45, hjust = 1))
             ggplotly(p)
           },
           "Comparaison des taux de Vente entre les banques" = {
             p <- ggplot(data = filtered_data(), aes(x = Banque, y = Vente, fill = Banque)) +
               geom_boxplot() +
               labs(x = "Banque", y = "Vente", title = "Comparaison des taux de Vente entre les banques") +
               scale_fill_manual(values = c("#FF5733", "#FFBD33", "#FFD133", "#FFF733", "#D1FF33", "#33FF45", "#33FFC6", "#33D9FF", "#336CFF", "#8333FF")) + #Pour les couleurs j'aurais pu écrire leurs noms au lieu des codes 
               theme_minimal() +
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + # Rotation du texte sur l'axe des abscisses
               guides(fill = "none") # Supprimer la légende
             ggplotly(p)
           },
           "Distribution des taux de change par banque" = {
             p <- ggplot(data = filtered_data(), aes(x = Vente)) +
               geom_density(alpha = 0.5, fill = "blue") +
               labs(x = "Vente", y = "Densité", title = "Distribution des taux de change par banque") +
               theme_minimal() +
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # Rotation du texte sur l'axe des abscisses
             ggplotly(p)
           }
    )
  })
}

# Exécution de l'application Shiny
shinyApp(ui = ui, server = server)
