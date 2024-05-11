# Charger les bibliothèques nécessaires pour l'application Shiny
library(stats)
library(shiny)          # Pour construire des applications web interactives avec R
library(shinydashboard) # Pour créer des tableaux de bord
library(DT)             # Pour afficher des tableaux de données interactifs
library(dplyr)          # Pour manipuler des données
library(purrr)          # Pour programmer fonctionnellement avec des listes et des vecteurs
library(corrplot)       # Pour visualiser des matrices de corrélation
library(ggplot2)        # Pour la création de graphiques
library(htmltools)      # Pour manipuler du HTML
library(readxl)         # Pour lire des fichiers Excel
library(broom)          # Pour convertir des modèles statistiques en données ordonnées
library(plotly)         # Pour créer des graphiques interactifs
library(scales)         # Pour formater et mapper des données numériques dans des graphiques
library(forecast)            # Pour effectuer des prévisions et des analyses de séries chronologiques
library(shinydashboardPlus)  # Pour créer des tableaux de bord Shiny plus avancés
library(car)                 # Pour effectuer des analyses de régression et d'autres analyses statistiques
library(shinyjs)             # Pour ajouter des fonctionnalités JavaScript à vos applications Shiny
library(FactoMineR)          # Pour effectuer des analyses factorielles des données (ACP, ACM, etc.)
library(factoextra)          # Pour la visualisation des résultats d'analyse factorielle
library(dbscan)              # Pour effectuer l'algorithme de clustering DBSCAN
library(cluster)             # Pour effectuer des analyses de clustering et de regroupement
library(flexdashboard)
library(Factoshiny)
library(tseries)
library(dendextend)  # for hang.dendrogram
library(ggdendro)



# Définir la fonction de préparation des données pour le clustering
prepareDataForClustering <- function(df) {
  # Supprimer les colonnes non numériques
  df <- df[, sapply(df, is.numeric)]
  # Remplacer les NA par la médiane des colonnes
  df[] <- lapply(df, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))
  return(df)
}







ui <- dashboardPage(
  dashboardHeader(
    title = tags$span(tags$img(src = "OIG2.png", height = "50px"), "AppDASH")
  ),
  dashboardSidebar(
    fileInput('file1', 'Choisir un fichier',
              accept = c(
                'text/csv',
                'text/comma-separated-values,text/plain',
                '.csv',
                '.txt',
                '.xlsx',  # Accepter les fichiers Excel
                ".xls"
              )),
    tags$hr(),
    checkboxInput('header', 'Entête', TRUE),
    actionButton("build_ui", "EXÉCUTER", class = "btn-custom action-button"),
    actionButton("showHelp", "Afficher l'Aide", class = "btn-custom action-button"),
    tags$div(class = "sidebar-footer", 
             style = "position: absolute; bottom: 0; width: 100%; padding: 10px;",
             useShinyjs(),  # Initialize shinyjs
             actionButton("logout", "Réinitialiser l'application")
    )
  ),
  
  dashboardBody(
    tags$style(HTML("
:root {
  --primary-color: #007bff;
  --primary-hover-color: #0056b3;
  --secondary-color: #888888;
  --background-color: #F0F0F0;
  --text-color: #333;
  --border-color: #AAAAAA;
  --input-background: #EEE;
  --input-border: #AAA;
  --box-shadow-color: #BDC3C7;
  --box-border-primary: #3498DB;
  --box-border-warning: #F1C40F;
}

body {
  font-family: 'Your Font Name', sans-serif;
  background-color: var(--background-color);
  color: var(--text-color);
}
/* Button styling */
.btn {
  background-color: var(--primary-color);
  color: var(--text-color);
  border: none;
  border-radius: 20px;
  padding: 10px 24px;
  font-size: 10px;
  text-transform: uppercase;
  font-weight: bold;
  transition: background-color 0.3s ease;
}

.btn:hover {
  background-color: var(--primary-hover-color);
  cursor: pointer;
}


  ")),
      
    
     
    tabsetPanel(id = "main_tabs",
      # Onglet pour afficher les données
      tabPanel(span(icon("database",style = "color:#337ab7;"), "Données"),
               DTOutput("table"),  # Afficher le tableau de données
               actionButton("addRow", "Ajouter une ligne"), # Bouton pour ajouter une nouvelle ligne
      ),
      # Onglet pour afficher le résumé des données
      tabPanel(span(icon("bar-chart",style = "color:#337ab7;"), "Résumé"), 
               fluidRow(
                 # Première rangée avec des informations sur les lignes et les colonnes
                 h4(strong("Dimention")),
                 column(2, img(src = "loii.png", height = "100px")), # Image illustrative
                 column(3, h4("LIGNES"), textOutput("rowCount")),  # Affichage du nombre de lignes
                 column(2, img(src = "his.png", height = "100px")),  # Image illustrative
                 column(1, h4("COLONNES"), textOutput("colCount")),  # Affichage du nombre de colonnes
               ),
               hr(),  # Ligne de séparation
               h4(strong("vous pouvez changer les classes des variables"), 
                  style = "color: white; background-color: blue; padding: 10px;"),
              
               DTOutput("summaryTable"),  # Affichage du tableau récapitulatif
               uiOutput("varTypeSelectors"),  # Affichage des sélecteurs de type de variables
      ),
      # Onglet pour l'imputation des données manquantes
      tabPanel(span(icon("filter", style = "color: #337ab7;"), "Imputation"),
               uiOutput("missingValuesUI"),  # Interface pour l'imputation des données manquantes
               textOutput("missingValuesInfo"),  # Affichage des informations sur les valeurs manquantes
               actionButton("validateImputation", "Valider les Données", class = "action-button"),  # Bouton pour valider l'imputation
      ),
      # Onglet pour afficher les statistiques des données
      tabPanel(span(icon("pie-chart", style = "color: #337ab7;"), "Statistiques"),
               fluidRow(   
                 selectInput("dist_column", "Sélectionner la colonne pour la visualisation", choices = NULL),  # Sélection de la colonne pour la visualisation
                 radioButtons("dist_type", "Type de visualisation",
                              choices = list("Densité" = "density", "Q-Q Plot" = "qqplot")),  # Type de visualisation
                 plotlyOutput("distPlot")  # Affichage du graphique
               ),
               fluidRow(
                 # Deuxième rangée avec les graphiques d'histogramme et de boîte
                 column(6, 
                        selectInput("hist_column", "Sélectionner la colonne pour l'histogramme", choices = NULL),  # Sélection de la colonne pour l'histogramme
                        sliderInput("hist_bins", "Nombre de barres :", min = 1, max = 50, value = 30),  # Nombre de barres pour l'histogramme
                        plotlyOutput("histogramPlot")  # Affichage de l'histogramme interactif
                 ),
                 column(6, 
                        selectInput("box_column", "Sélectionner la colonne pour le diagramme en boîte", choices = NULL),  # Sélection de la colonne pour le diagramme en boîte
                        sliderInput("box_width", "Largeur du boîte :", min = 0.1, max = 1, value = 0.5, step = 0.1),  # Largeur de la boîte
                        plotlyOutput("boxplotPlot"))  # Affichage du diagramme en boîte
               ),
               fluidRow(
                 # Troisième rangée avec les graphiques à secteurs et en barres
                 column(6, 
                        selectInput("pie_column", "Sélectionner la colonne pour le diagramme à secteurs", choices = NULL),  # Sélection de la colonne pour le diagramme à secteurs
                        plotlyOutput("pieChartPlot")  # Affichage du diagramme à secteurs
                 ),
                 column(6, 
                        selectInput("crossbar_x", "Sélectionner la colonne pour l'axe des X", choices = NULL),  # Sélection de la colonne pour l'axe X
                        selectInput("crossbar_y", "Sélectionner la colonne pour l'axe des Y", choices = NULL),  # Sélection de la colonne pour l'axe Y
                        selectInput("crossbar_fill", "Sélectionner la colonne pour la couleur", choices = NULL),  # Sélection de la colonne pour la couleur
                        plotlyOutput("crossBarPlot"))  # Affichage du diagramme en barres
               ),
               fluidRow(
                 # Quatrième rangée avec les graphiques d'histogramme et de boîte groupés
                 column(6, 
                        selectInput("hist_group_column", "Sélectionner la colonne pour grouper l'histogramme", choices = NULL),  # Sélection de la colonne pour grouper l'histogramme
                        selectInput("hist_value_column", "Sélectionner la colonne pour les valeurs de l'histogramme", choices = NULL),  # Sélection de la colonne pour les valeurs de l'histogramme
                        plotlyOutput("groupedHistogramPlot")  # Affichage de l'histogramme groupé
                 ),
                 column(6, 
                        selectInput("box_group_column", "Sélectionner la colonne pour grouper le boxplot", choices = NULL),  # Sélection de la colonne pour grouper le boxplot
                        selectInput("box_value_column", "Sélectionner la colonne pour les valeurs du boxplot", choices = NULL),  # Sélection de la colonne pour les valeurs du boxplot
                        plotlyOutput("groupedBoxplotPlot"))  # Affichage du boxplot groupé
               ),
      ),
      # Onglet pour afficher les corrélations entre les variables
      tabPanel(span(icon("circle", style = "color:#337ab7;"), "Correlations"),
               uiOutput("correlationMethod"),  # Interface pour choisir la méthode de calcul des corrélations
               plotOutput("correlationPlot"),  # Affichage du graphique des corrélations
      ),
      # Onglet pour effectuer une régression linéaire
      tabPanel(span(icon("line-chart", style = "color: #337ab7;"), "Régression Linéaire"),
               selectInput("var_explicative", "Variable Explicative (X)", choices = NULL),  # Sélection de la variable explicative
               selectInput("var_a_expliquer", "Variable à Expliquer (Y)", choices = NULL),  # Sélection de la variable à expliquer
               actionButton("run_regression", "Exécuter la Régression Linéaire"),  # Bouton pour exécuter la régression linéaire
               verbatimTextOutput("regression_results"),  # Affichage des résultats textuels de la régression
               plotOutput("regression_plot")  # Affichage du graphique de la régression
      ),
      tabPanel("Tests", icon = icon("flask", style = "color: #337ab7;"),
               navlistPanel(
                 "Tests de Conformité",
                 tabPanel("Test de Kolmogorov-Smirnov",
                          h3("Test de Kolmogorov-Smirnov"),
                          selectInput("columnKS", "Sélectionner la colonne pour le test", choices = NULL),
                          selectInput("ksLaw", "Choisir la loi pour l'hypothèse H0:", choices = c("Exponentielle", "Uniforme", "Normale")),
                          plotlyOutput("ksPlot"),
                          verbatimTextOutput("ksResult")
                 ),
                 tabPanel("Test de D'adéquation de khi-deux",
                          h3("Test de D'adéquation de khi-deux"),
                          selectInput("columnChi", "Sélectionner la colonne pour le test", choices = NULL),
                          selectInput("chiLaw", "Choisir la loi pour l'hypothèse H0:", choices = c("Exponentielle", "Uniforme", "Normale")),
                          plotlyOutput("khiDeuxPlot"),
                          verbatimTextOutput("khiDeuxResult")
                 )
               ),
               navlistPanel(
                 "Tests de Normalité",
                 tabPanel("Droite de Henry",
                          h3("Test de la Droite de Henry"),
                          selectInput("columnHenry", "Sélectionner la colonne pour le test", choices = NULL),
                          p("Détails sur le test de la Droite de Henry..."),
                          plotlyOutput("henryPlot"),
                          verbatimTextOutput("henryResult")
                 ),
                 tabPanel("Test de Jarque-Bera",
                          h3("Test de Jarque-Bera"),
                          selectInput("columnJarque", "Sélectionner la colonne pour le test", choices = NULL),
                          p("Détails sur le test de Jarque-Bera..."),
                          plotlyOutput("jarquePlot"),
                          verbatimTextOutput("jarqueResult")
                 )
               )
      ), 
      
      # Onglet pour effectuer une analyse factorielle

      tabPanel(span(icon("sitemap", style = "color: #337ab7;"), "Analyse factorielle"),
               
               radioButtons("analysis_type", "Select Analysis Type:", choices = c("PCA", "MCA")),
               radioButtons("output_type", "Choose Output Type:",
                            choices = c("Summary of Results" = "summary",
                                        "Eigenvalues" = "eigen",
                                        "Results on Variables" = "var",
                                     "Results on Individuals" = "ind")),
               fluidRow(
               column(4,
               actionButton("run_analysis", "Run Analysis"),
               ),
               column(4,
               downloadButton("downloadPlot", "Download Plot"),
                 )
               ),
      
      mainPanel(
        conditionalPanel(
          condition = "input.output_type == 'summary'",
          h3("Summary of Results"),
          verbatimTextOutput("summaryOutput")
        ),
        conditionalPanel(
          condition = "input.output_type == 'eigen'",
          h3("Eigenvalues"),
          plotOutput("eigenPlot"),
          DTOutput("eigenTable")
        ),
        conditionalPanel(
          condition = "input.output_type == 'var'",
          h3("Variable Results"),
          plotOutput("varPlot"),
          DTOutput("varTable")
          
        ),
        conditionalPanel(
          condition = "input.output_type == 'ind'",
          h3("Individual Results"),
          plotOutput("indPlot"),
          DTOutput("indTable")
          
        )
      )
    ),
  
    tabPanel(span(icon("object-group", style = "color: #337ab7;"),  "Classification"),
             fluidRow(
               column(12, plotOutput("dendroPlot", height = "500px"))  # Adjust height as needed
             ),
               
              downloadButton("downloadDendro", "Télécharger Dendrogramme")
               
               
             
    ) ,
     
    )
    
    
    
  )
)


server <- function(input, output, session) {
  
  # Function to show the welcome message modal
  showWelcomeMessage <- function() {
    showModal(modalDialog(
      title = "Bienvenue dans mon application!",
      HTML("Merci d'utiliser mon application !😊"),
      footer = NULL
    ))
    # Close the modal after 2 seconds
    Sys.sleep(1)
    removeModal()  # Close the modal
  }
  
  # Call the function to show the welcome message when the app is loaded
  shinyjs::runjs(showWelcomeMessage())
  


  
  
  data <- reactiveVal()  # Réactive pour stocker les données
  
  # Réactive pour indiquer si les données ont été validées ou non
  dataValidated <- reactiveVal(FALSE)
  
  # Réactive pour indiquer si les données sont affichées ou non
  dataDisplayed <- reactiveVal(FALSE)
  
  # Réactive pour stocker les nouvelles données
  new_data <- reactiveVal()
  
  # Réactive pour stocker les données imputées
  imputedData <- reactiveVal()
  
  # Réactive pour stocker les résultats des tests
  # Réactive pour stocker les résultats des tests
  test_result <- reactiveVal()
  
  observe({
    file <- input$file1
    if (is.null(file)) return()
    # Vérifier le type de fichier et lire en conséquence
    if (grepl("\\.csv$", file$name)) {
      data(read.csv(file$datapath))
    } else if (grepl("\\.(xlsx|xls)$", file$name)) {
      data(readxl::read_excel(file$datapath))
    } else if (grepl("\\.txt$", file$name)) {
      # Spécifier le séparateur
      data(read.delim(file$datapath, sep = ","))  # Par exemple, pour un séparateur de tabulation
      # data(read.delim(file$datapath, sep = ","))  # Si le séparateur est une virgule (par défaut)
    } else {
      # Type de fichier non pris en charge
      return()
    }
  })
  
  
  
  dataDisplayed <- reactiveVal(FALSE)
  
  # Observer pour afficher les données lorsque le bouton "EXÉCUTER" est cliqué
  observeEvent(input$build_ui, {
    dataDisplayed(TRUE)  # Afficher les données lorsqu'EXÉCUTER est cliqué
    shinyjs::enable("main_tabs")
    shinyjs::enable("main_tabs-2") 
    shinyjs::enable("main_tabs-3")
    shinyjs::enable("main_tabs-4")
    shinyjs::enable("main_tabs-5")
    shinyjs::enable("main_tabs-6")
  })
  
  # Affichage des données dans un tableau interactif
  output$table <- DT::renderDT({
    if (dataDisplayed()) {
      DT::datatable(data(), editable = TRUE, options = list(pageLength = 5))
    }
  }, server = TRUE)
  
  # Observer pour mettre à jour les données lors de l'édition d'une cellule dans le tableau
  observeEvent(input$table_cell_edit, {
    info <- input$table_cell_edit
    current_data <- data()
    current_data[info$row, info$col] <- info$value
    data(current_data)
  })
  
  proxy <- dataTableProxy('table')
  
  # Observer pour ajouter une nouvelle ligne au tableau
  observeEvent(input$addRow, {
    current_data <- data()
    empty_row <- if (nrow(current_data) > 0) {
      sapply(current_data, function(column) {
        if (is.numeric(column)) {
          0  
        } else if (is.integer(column)) {
          0L  
        } else if (is.factor(column)) {
          factor("")  
        } else {
          ""  
        }
      }, simplify = FALSE) 
    } else {
      as.list(setNames(rep("", ncol(current_data)), names(current_data)))  
    }
    
    new_data <- rbind(empty_row, current_data)
    data(new_data)  
    replaceData(proxy, data(), resetPaging = FALSE, rownames = NULL)
  })
  
  currentData <- reactive({
    if (is.null(new_data())) {
      data()
    } else {
      new_data()
    }
  })
  
  # Les sorties réactives peuvent maintenant utiliser currentData() comme source de données
  
  # Affichage du nombre de lignes
  output$rowCount <- renderText({
    if (dataDisplayed()) {
      nrow(currentData())
    }
  })
  
  # Affichage du nombre de colonnes
  output$colCount <- renderText({
    if (dataDisplayed()) {
      ncol(currentData())
    }
  })
  
  # Fonction pour compter les valeurs manquantes
  count_missing <- function(temp) {
    missing_count <- sum(is.na(temp), na.rm = TRUE) + sum(temp == "", na.rm = TRUE) + sum(temp == "NULL", na.rm = TRUE)
    total_count <- length(temp)
    missing_percentage <- (missing_count / total_count) * 100
    return(list(missing_count = missing_count, missing_percentage = missing_percentage))
  }
  
  # Affichage des sélecteurs de type de variables
  output$varTypeSelectors <- renderUI({
    df <- currentData()
    req(df)
    
    num_cols <- max(2, floor(12 / min(6, length(names(df)))))
    
    fields <- lapply(names(df), function(col) {
      column(
        num_cols,
        selectInput(
          inputId = paste0("type_", col),
          label = paste("Type de", col),
          choices = c("Numérique", "Catégorique"),
          selected = if (is.numeric(df[[col]])) "Numérique" else "Catégorique"
        )
      )
    })
    
    do.call(fluidRow, fields)
  })


output$summaryTable <- renderDT({
  # Vérifiez si les données sont affichées
  if (dataDisplayed()) {
    req(currentData())  # Assurez-vous que les données actuelles sont disponibles
    df <- currentData()
    
    # Modifier les types de données selon le choix de l'utilisateur
    for (col in names(df)) {
      type_selected <- input[[paste0("type_", col)]]
      if (!is.null(type_selected) && type_selected == "Numérique" && !is.numeric(df[[col]])) {
        df[[col]] <- as.numeric(df[[col]])  # Convertir en numérique si sélectionné et non déjà numérique
      } else if (!is.null(type_selected) && type_selected == "Catégorique" && !is.factor(df[[col]])) {
        df[[col]] <- as.factor(df[[col]])  # Convertir en facteur si sélectionné et non déjà facteur
      }
    }
    
    # Calculer les statistiques descriptives
    stats_list <- purrr::map(df, function(x) {
      missing_info <- count_missing(x)
      if(is.numeric(x)) {
        c(
          "Classe" = "Numérique",
          "Nombre des Valeurs manquantes" = missing_info$missing_count,
          "Pourcentage manquant" = sprintf("%.2f%%", missing_info$missing_percentage),
          "Max" = max(x, na.rm = TRUE),
          "Min" = min(x, na.rm = TRUE),
          "Moyenne" = mean(x, na.rm = TRUE),
          "Médiane" = median(x, na.rm = TRUE),
          "Écart-type" = sd(x, na.rm = TRUE),
          "Variance" = var(x, na.rm = TRUE),
          "Éléments uniques" = length(unique(x))
        )
      } else {
        c(
          "Classe" = class(x)[1],
          "Nombre des Valeurs manquantes" = missing_info$missing_count,
          "Pourcentage manquant" = sprintf("%.2f%%", missing_info$missing_percentage),
          "Max" = NA,
          "Min" = NA,
          "Moyenne" = NA,
          "Médiane" = NA,
          "Écart-type" = NA,
          "Variance" = NA,
          "Éléments uniques" = length(unique(na.omit(x)))
        )
      }
    })
    stats_df <- dplyr::bind_rows(stats_list, .id = "Colonne")
    datatable(stats_df, options = list(pageLength = 5, autoWidth = TRUE))
  }
})

transformedData <- reactiveVal()  # Variable réactive pour stocker les données transformées

observe({
  req(dataDisplayed())  # Assurez-vous que le fichier est chargé
  df <- read.csv(input$file1$datapath)  # Ou votre fonction de chargement de données
  
  # Appliquer les changements de type
  for (col in names(df)) {
    type_selected <- input[[paste0("type_", col)]]
    if (!is.null(type_selected) && type_selected == "Numérique" && !is.numeric(df[[col]])) {
      df[[col]] <- as.numeric(df[[col]])
    } else if (!is.null(type_selected) && type_selected == "Catégorique" && !is.factor(df[[col]])) {
      df[[col]] <- as.factor(df[[col]])
    }
  }
  
  # Mettre à jour la variable réactive avec les données transformées
  transformedData(df)
})

# Observer pour afficher la boîte de dialogue d'aide
observeEvent(input$showHelp, {
  showModal(modalDialog(
    title = "Comment utiliser cette application",
    HTML("
        <h4>Introduction</h4>
        <p>Cette application vous permet de manipuler et analyser des données à travers diverses fonctionnalités intégrées. Voici comment vous pouvez utiliser chaque partie :</p>
        <ul>
          <li><b>Données :</b> Charger et visualiser les données à partir de fichiers CSV ou Excel.</li>
          <li><b>Résumé :</b> Affiche un résumé statistique des données chargées, incluant max, min, moyenne, etc.</li>
          <li><b>Imputation :</b> Permet d'imputer les valeurs manquantes dans vos données.</li>
          <li><b>Distributions :</b> Visualiser les distributions des variables à l'aide de graphiques comme les histogrammes ou les Q-Q plots.</li>
          <li><b>Statistiques :</b> Examine les statistiques descriptives et permet des comparaisons à travers des graphiques de boîte ou des histogrammes groupés.</li>
          <li><b>Corrélations :</b> Affiche les corrélations entre les variables numériques sous forme de graphique.</li>
          <li><b>Régression Linéaire :</b> Effectue et visualise une régression linéaire entre deux variables sélectionnées.</li>
          <li><b>Tests :</b> Exécute différents tests statistiques tels que le test de jarque bera pour la normalité ou droite de henry.</li>
         <li><b>Analyse Factorielle :</b> Réalise une analyse des composantes principales (ACP) pour étudier les relations entre plusieurs variables quantitatives.</li>
         <li><b>Classification :</b> Applique des méthodes de classification comme le clustering pour regrouper les données en catégories basées sur des caractéristiques similaires.</li>
     </ul>
        <p>Pour des instructions détaillées, veuillez consulter le manuel d'utilisateur ou regarder les tutoriels vidéo.</p>
      "),
    tags$a(href = "vedio.mp4", target = "_blank", "Regarder le Tutoriel", class = "btn btn-primary"),
    footer = modalButton("Fermer")
  ))
})

# Observer pour mettre à jour le menu déroulant de sélection de colonne
observe({
  df <- transformedData()
  updateSelectInput(session, "selectedColumn", choices = names(df))
})

# Observer pour mettre à jour les menus déroulants en fonction des données transformées ou imputées
observe({
  req(transformedData())  # S'assurer que les données actuelles sont disponibles
  df <- if (is.null(imputedData())) transformedData() else imputedData()
  numericCols <- names(df)[sapply(df, is.numeric)]
  categoricalCols <- names(df)[sapply(df, Negate(is.numeric))]
  
  # Mettre à jour les menus déroulants avec les colonnes numériques et catégoriques
  updateSelectInput(session, "hist_column", choices = numericCols)
  updateSelectInput(session, "pie_column", choices = names(df))
  updateSelectInput(session, "box_column", choices = numericCols)
  updateSelectInput(session, "variable_x_correlation", choices = numericCols)
  updateSelectInput(session, "variable_y_correlation", choices = numericCols)
  updateSelectInput(session, "var_explicative", choices = numericCols)
  updateSelectInput(session, "var_a_expliquer", choices = numericCols)
  updateSelectInput(session, "crossbar_x", choices = names(df))
  updateSelectInput(session, "crossbar_y", choices = names(df))
  updateSelectInput(session, "crossbar_fill", choices = names(df))
  updateSelectInput(session, "hist_group_column", choices = categoricalCols)
  updateSelectInput(session, "hist_value_column", choices = numericCols)
  updateSelectInput(session, "box_group_column", choices = categoricalCols)
  updateSelectInput(session, "box_value_column", choices = numericCols)
  updateSelectInput(session, "dist_column", choices = numericCols)
  
})

imputedData <- reactiveVal(NULL)

# Ajustement de la fonction pour détecter les colonnes avec des valeurs manquantes
missingValuescol <- function(df) {
  cols_with_na <- sapply(df, function(x) any(is.na(x) | x == "" | x == "NULL"))
  names(df)[cols_with_na]
}

# Mise à jour de renderUI
output$missingValuesUI <- renderUI({
  req(currentData())
  data <- currentData()  # Assurez-vous que ceci obtient vos données actuelles
  if (is.null(data) && dataDisplayed()) {
    return(h4("Aucune donnée n'a été chargée"))
  }
  
  cols_with_missing <- missingValuescol(currentData())
  
  if (length(cols_with_missing) > 0) {
    tagList(
      h4(strong("Des valeurs manquantes ont été trouvées"), 
         style = "color: white; background-color: blue; padding: 10px;"), 
      selectInput("selectedColumn", "Select a column to impute", choices = cols_with_missing),
      selectInput("imputeFunction", "Select function to impute", choices = c("Mode", "Mean", "Median", "Min", "Max")),
      actionButton("imputeData", "Impute"),
      DTOutput("imputedTable"),
      tags$footer(
        tags$div(
          downloadButton("downloadData", "Télécharger les Données", icon = icon("download" ,style = "color: #337ab7;")),
          style = "position: fixed; bottom: 10px; left: 50%; transform: translateX(-50%);"
        )
      )
    )
  } else {
    h4(strong("Les données n'ont pas de valeurs manquantes"), 
       style = "color: white; background-color: blue; padding: 10px;")  
  }
})


observeEvent(input$selectedColumn, {
  data <- currentData()
  selected_col <- input$selectedColumn
  if (is.numeric(data[[selected_col]]) && dataDisplayed()) {
    updateSelectInput(session, "imputeFunction", choices = c( "Moyenne", "Mediane", "Min", "Max"))
  } else {
    updateSelectInput(session, "imputeFunction", choices = c( "Mode"))
  }
})
observeEvent(input$imputeData, {
  print("Début de l'imputation")
  
  # Use the imputed data if available; otherwise, fall back to the original data
  df <- if(is.null(imputedData())) transformedData() else currentData()
  
  req(df, input$selectedColumn, input$imputeFunction)  # Ensure necessary inputs are present
  print("Données chargées")
  
  col_to_impute <- input$selectedColumn
  method <- input$imputeFunction
  print(paste("Imputation pour", col_to_impute, "avec méthode", method))
  # Normaliser les valeurs manquantes, les chaînes vides et "NULL" en NA avant l'imputation
  df[[col_to_impute]][df[[col_to_impute]] == ""] <- NA
  df[[col_to_impute]][df[[col_to_impute]] == "NULL"] <- NA
  # Convertir en NA les éléments qui sont littéralement 'NA' comme chaîne de caractères
  df[[col_to_impute]][df[[col_to_impute]] == "NA"] <- NA
  
  tryCatch({
    if(is.numeric(df[[col_to_impute]])) {
      # Numeric imputation based on the selected method
      imputed_values <- switch(method,
                               "Moyenne" = mean(df[[col_to_impute]], na.rm = TRUE),
                               "Mediane" = median(df[[col_to_impute]], na.rm = TRUE),
                               "Min" = min(df[[col_to_impute]], na.rm = TRUE),
                               "Max" = max(df[[col_to_impute]], na.rm = TRUE))
      df[[col_to_impute]][is.na(df[[col_to_impute]])] <- imputed_values
    } else if(method == "Mode" && !is.numeric(df[[col_to_impute]])) {
      modeValue <- names(which.max(table(df[[col_to_impute]], useNA = "no")))
      df[[col_to_impute]][is.na(df[[col_to_impute]])] <- modeValue
      print(paste("La valeur du mode pour la colonne", col_to_impute, "est", modeValue))
    }
    ####
    for (col in names(df)) {
      type_selected <- input[[paste0("type_", col)]]
      if (!is.null(type_selected) && type_selected == "Numérique" && !is.numeric(df[[col]])) {
        df[[col]] <- as.numeric(df[[col]])  # Convertir en numérique si sélectionné et non déjà numérique
      } else if (!is.null(type_selected) && type_selected == "Catégorique" && !is.factor(df[[col]])) {
        df[[col]] <- as.factor(df[[col]])  # Convertir en facteur si sélectionné et non déjà facteur
      }
    }
    # Update the imputed data with the latest imputation
    imputedData(df)
    print("Imputation terminée avec succès.")
    
  }, error = function(e) {
    print(paste("Error in imputation:", e$message))
  })
  
  # Optionally, refresh a specific output to show imputation has occurred
  output$imputedTable <- renderDT({
    req(imputedData())
    datatable(isolate(imputedData()), options = list(pageLength = 5))
  })
  
})

# Gestionnaire de téléchargement pour télécharger les données imputées
output$downloadData <- downloadHandler(
  filename = function() {
    paste("donnees-imputees-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    df <- imputedData()  # Récupérer les données imputées (ou les données originales)
    write.csv(df, file, row.names = FALSE)  # Écrire les données dans un fichier CSV
  }
)

# Variable réactive pour indiquer si les données ont été validées après l'imputation
dataValidated <- reactiveVal(FALSE)

# Observer pour valider les données après l'imputation
observeEvent(input$validateImputation, {
  # Mettre à jour la valeur réactive après l'imputation
  dataValidated(TRUE)
})

# Rendu du graphique de corrélation
output$correlationPlot <- renderPlot({
  df <- req(transformedData(),dataValidated())  # Utiliser les données transformées
  num_df <- df %>% select_if(is.numeric)  # Sélectionner uniquement les données numériques
  
  # Calculer la matrice de corrélation
  correlation_matrix <- cor(num_df, use = "pairwise.complete.obs")
  
  # Choisir la méthode de visualisation en fonction de l'input de l'utilisateur
  method <- switch(input$correlation_method,
                   "number" = "number",
                   "circle" = "circle",
                   "circle")  # Méthode par défaut
  
  # Afficher la matrice de corrélation
  corrplot(correlation_matrix, method = method)
})

# Mise à jour des options de la méthode de visualisation de corrélation
observe({
  output$correlationMethod <- renderUI({
    req(transformedData(),dataValidated())
    df <- if (is.null(imputedData())) transformedData() else imputedData()
    if (ncol(df) > 0) {
      radioButtons("correlation_method", "Méthode de visualisation de la corrélation",
                   choices = c("Nombre" = "number", "Cercle" = "circle"),
                   selected = "circle")
    }
  })
})

# Rendu de l'histogramme
output$histogramPlot <- renderPlotly({
  if (dataDisplayed()) {
    if (dataValidated()) {  # Vérifier si les données ont été validées
      req(transformedData())  # Assurer que les données ont été traitées
      df <- if (is.null(imputedData())) transformedData() else imputedData()
      selected_col <- input$hist_column  # Colonne sélectionnée par l'utilisateur pour l'histogramme
      num_bins <- input$hist_bins  # Nombre de barres spécifié par l'utilisateur
      
      # S'assurer que la colonne sélectionnée existe dans le dataframe
      req(selected_col %in% names(df))
      
      # Créer l'histogramme avec ggplot
      p <- ggplot(df, aes_string(x = selected_col)) +
        geom_histogram(bins = num_bins, fill = 'blue', color = 'black') +
        labs(x = selected_col, y = "Nombre") +
        theme_minimal()
      
      # Convertir l'objet ggplot en un objet plotly pour l'interactivité
      ggplotly(p, tooltip = c("x", "y"))
    }
  }
})

# Rendu du diagramme en boîte à moustaches
output$boxplotPlot <- renderPlotly({
  if (dataDisplayed()) {
    if (dataValidated()) {  
      req(transformedData())  # S'assurer qu'il y a des données disponibles
      df <- if (is.null(imputedData())) transformedData() else imputedData()  # Utiliser les données réactives
      selected_col <- input$box_column  # Colonne sélectionnée par l'utilisateur pour le graphique
      box_width <- input$box_width  # Largeur de la boîte spécifiée par l'utilisateur
      
      # S'assurer que la colonne sélectionnée est numérique
      req(is.numeric(df[[selected_col]]))
      
      # Créer le diagramme en boîte avec ggplot2
      p <- ggplot(df, aes_string(y = selected_col)) +
        geom_boxplot(width = box_width) +
        labs(y = selected_col, x = "") +
        theme_minimal()
      
      # Ajouter des annotations de texte pour les statistiques sommaires
      summary_df <- df %>%
        summarise(min = min(!!sym(selected_col), na.rm = TRUE),
                  max = max(!!sym(selected_col), na.rm = TRUE),
                  median = median(!!sym(selected_col), na.rm = TRUE),
                  Q1 = quantile(!!sym(selected_col), 0.25, na.rm = TRUE),
                  Q3 = quantile(!!sym(selected_col), 0.75, na.rm = TRUE))
      
      p <- p + 
        geom_text(data = summary_df, aes(x = 1, y = min, label = paste("Min :", min)),
                  hjust = -0.3, vjust = 0) +
        geom_text(data = summary_df, aes(x = 1, y = max, label = paste("Max :", max)),
                  hjust = -0.3, vjust = 1) +
        geom_text(data = summary_df, aes(x = 1, y = median, label = paste("Médiane :", median)),
                  hjust = -0.3, vjust = 1) +
        geom_text(data = summary_df, aes(x = 1, y = Q1, label = paste("Q1 :", Q1)),
                  hjust = -0.3, vjust = 1) +
        geom_text(data = summary_df, aes(x = 1, y = Q3, label = paste("Q3 :", Q3)),
                  hjust = -0.3, vjust = 1)
      
      ggplotly(p, tooltip = c("y"))  # Convertir l'objet ggplot en un objet plotly
    }
  }
})

# Rendu UI pour sélectionner la colonne
output$selectColumn <- renderUI({
  # Création d'un menu déroulant pour les noms de colonnes de 'data'
  selectInput("selectedColumn", "Sélectionnez la colonne", choices = names(data))
})

# Rendu du pie chart avec Plotly
output$pieChartPlot <- renderPlotly({
  if (dataDisplayed()) {
    if (dataValidated()) {  
      # S'assurer que la colonne a été sélectionnée et que les données sont disponibles
      req(input$pie_column, transformedData())
      
      # Préparer les données pour le pie chart
      pie_data <- transformedData() %>%
        count(!!sym(input$pie_column)) %>%
        rename(labels = !!sym(input$pie_column), values = n)
      
      # Créer le pie chart avec Plotly
      plot_ly(pie_data, labels = ~labels, values = ~values, type = 'pie', hole = 0.4) %>%
        layout(title = paste('Répartition de', input$pie_column), 
               showlegend = TRUE,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
  }
})

# Observer pour générer le graphique croisé
observe({
  if (dataValidated() && !is.null(input$crossbar_x) && !is.null(input$crossbar_y) && !is.null(input$crossbar_fill)) {
    df <- if (is.null(imputedData())) transformedData() else imputedData()
    p <- ggplot(df, aes_string(x = input$crossbar_x, y = input$crossbar_y, fill = input$crossbar_fill)) + 
      geom_bar(stat = "identity", position = position_dodge()) +
      theme_minimal() +
      labs(x = input$crossbar_x, y = input$crossbar_y) +
      ggtitle("Graphique Croisé")
    
    output$crossBarPlot <- renderPlotly({
      ggplotly(p) # Convertir ggplot en graphique plotly interactif
    })
  }
})

# Observer pour exécuter la régression linéaire
observeEvent(input$run_regression, {
  # Assurez-vous que des variables ont été sélectionnées et que les données sont validées
  req(input$var_explicative,input$var_a_expliquer, dataValidated())
  
  # Accéder aux données imputées ou aux données originales si aucune imputation n'a été réalisée
  df <- if(is.null(imputedData())) transformedData() else imputedData()
  
  # Ajuster le modèle de régression linéaire
  fit <- lm(reformulate(input$var_explicative,input$var_a_expliquer), data = df)
  
  # Créer le graphique de régression
  output$regression_plot <- renderPlot({
    if (dataDisplayed()) {
      p <- ggplot(df, aes_string(x = input$var_explicative, y = input$var_a_expliquer)) +
        geom_point() +  # Points de données
        geom_smooth(method = "lm", col = "red") +  # Ligne de régression
        labs(x = input$var_explicative, y = input$var_a_expliquer) +
        theme_minimal()
      
      print(p)  # Afficher le graphique
    }
  })
  
  # Stocker les résultats de la régression pour un affichage ultérieur
  output$regression_results <- renderPrint({
    summary(fit)  # Affiche le résumé du modèle de régression
  })
})

# Observer pour générer l'histogramme groupé
output$groupedHistogramPlot <- renderPlotly({
  if (dataDisplayed()) {
    req(input$hist_group_column, input$hist_value_column)  # S'assurer que les entrées sont remplies
    data <- transformedData()  # Obtenir vos données
    
    p <- ggplot(data, aes_string(x = input$hist_value_column, fill = input$hist_group_column)) +
      geom_histogram(position = "dodge", binwidth = 1) +  # La largeur du bin doit être ajustée au besoin
      theme_minimal() +
      labs(fill = "Group", x = input$hist_value_column, y = "Count") +
      scale_fill_brewer(palette = "Set1")  # Palette de couleurs pour les barres
    
    ggplotly(p)  # Convertir l'objet ggplot en un objet plotly
  }
})

# Observer pour générer le boxplot groupé
output$groupedBoxplotPlot <- renderPlotly({
  if (dataDisplayed()) {
    req(input$box_group_column, input$box_value_column)
    data <- transformedData()  # Obtenir vos données
    
    p <- ggplot(data, aes_string(x = input$box_group_column, y = input$box_value_column, fill = input$box_group_column)) +
      geom_boxplot() +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1") +
      labs(x = "Group", y = "Value", fill = "Category")
    
    ggplotly(p)  # Convertir l'objet ggplot en un objet plotly
  }
})

# Observer pour générer le graphique de distribution
output$distPlot <- renderPlotly({
  if (dataDisplayed()) {
    if(dataValidated()) {  
      req(input$dist_column, transformedData())
      df <- transformedData()
      selected_col <- input$dist_column
      plot_type <- input$dist_type
      
      if(plot_type == "density") {
        p <- ggplot(df, aes_string(x = selected_col)) +
          geom_density(fill = 'steelblue', alpha = 0.5) +
          labs(x = selected_col, y = "Densité") +
          theme_minimal()
        ggplotly(p)
      } else if(plot_type == "qqplot") {
        qq_data <- qqnorm(df[[selected_col]], plot.it = FALSE)
        p <- ggplot() +
          geom_point(aes(x = qq_data$x, y = qq_data$y)) +
          geom_line(aes(x = qq_data$x, y = qq_data$x), color = "red") +
          labs(x = "Théorique", y = "Échantillon") +
          ggtitle("Q-Q Plot") +
          theme_minimal()
        ggplotly(p)
      }
    }
  }
})


 
  
  
   
  
  
results <- eventReactive(input$run_analysis, {
  req(transformedData())
  if (input$analysis_type == "PCA") {
    df <- na.omit(transformedData()[, sapply(transformedData(), is.numeric)])
    if(ncol(df) > 1) {
      res.pca <- PCA(df, graph = FALSE)
      print(summary(res.pca))  # Debugging output
      return(res.pca)
    } else {
      return(NULL)
    }
  } else {
    df <- na.omit(transformedData()[, sapply(transformedData(), is.factor)])
    if(ncol(df) > 1) {
      res.mca <- MCA(df, graph = FALSE)
      print(summary(res.mca))  # Debugging output
      return(res.mca)
    } else {
      return(NULL)
    }
  }
})


output$analysis_ui <- renderUI({
  if (input$analysis_type == "PCA") {
    checkboxGroupInput("vars_for_pca", "Select Variables for PCA", choices = names(transformedData()[, sapply(transformedData(), is.numeric)]))
  } else {
    checkboxGroupInput("vars_for_mca", "Select Variables for MCA", choices = names(transformedData()[, sapply(transformedData(), is.factor)]))
  }
})
  output$summaryOutput <- renderPrint({
    req(results())
    
  })
  
  output$eigenPlot <- renderPlot({
    req(results())
    if (input$analysis_type == "PCA") {
      # Plotting the eigenvalues or variance for PCA
      fviz_screeplot(results(), choice = "eigenvalue")
    } else {
      # Assuming you also want eigenvalues for MCA, although MCA uses different metrics
      fviz_screeplot(results(), choice = "eigenvalue", type = "mca")
    }
  })
  output$eigenTable <- renderDT({
    req(results())
    results()$eig
  })
  
  output$varTable <- renderDT({
    req(results())
    results()$var$coord
  })
  
  output$varPlot <- renderPlot({
    req(results())
    fviz_pca_var(results(), col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
  })
  
  output$indTable <- renderDT({
    req(results())
    results()$ind$coord
  })
  
  output$indPlot <- renderPlot({
    req(results())
    fviz_pca_ind(results(), col.ind = "cos2", gradient.cols = c("#999999", "#FF0000", "#0000FF"))
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(input$analysis_type, "Results-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      g <- if (input$analysis_type == "PCA") {
        fviz_pca_biplot(results())
      } else {
        fviz_mca_biplot(results())
      }
      ggsave(file, plot = g, width = 12, height = 8)
    }
  )
  # Réactive pour stocker les données préparées pour le clustering
 
  observeEvent(input$logout, {
    shinyjs::redirect(url = "http://127.0.0.1:7567", session = session)
  })
  # Charger les données et mettre à jour les choix de colonne
  observeEvent(input$loadData, {
    req(transformedData())
    data <- if (is.null(imputedData())) transformedData() else imputedData()
    # Filtrer pour garder seulement les colonnes numériques
    numericCol <- names(data)[sapply(data, is.numeric)]
    
    # Mettre à jour les choix de colonne pour chaque test avec seulement des colonnes numériques
    updateSelectInput(session, "columnKS", choices = numericCol)
    updateSelectInput(session, "columnChi", choices = numericCol)
    updateSelectInput(session, "columnHenry", choices = numericCol)
    updateSelectInput(session, "columnJarque", choices = numericCol)
  })
  
  # Réagir aux choix de la loi pour le test de Kolmogorov-Smirnov
  observe({
    input$ksLaw  # Pour s'assurer que la réactivité est basée sur ce choix
    req(transformedData(),dataValidated())
    data <- transformedData()
    updateSelectInput(session, "columnKS", choices = names(data)[sapply(data, is.numeric)])
  })
  
  # Réagir aux choix de la loi pour le test de khi-deux
  observe({
    input$chiLaw  # Pour s'assurer que la réactivité est basée sur ce choix
    req(transformedData(),dataValidated())
    data <- if (is.null(imputedData())) transformedData() else imputedData()
    updateSelectInput(session, "columnChi", choices = names(data)[sapply(data, is.numeric)])
  })
  
  output$ksResult <- renderText({
    req(input$columnKS, input$ksLaw)
    data <- if (is.null(imputedData())) transformedData() else imputedData()
    column_data <- data[[input$columnKS]]
    ks_test <- ks_test(column_data, input$ksLaw)  # Appeler une fonction hypothétique pour le test
    paste("Statistique de test:", ks_test$statistic, "\nP-value:", ks_test$p.value)
  })
  
  output$ksPlot <- renderPlotly({
    req(input$columnKS, input$ksLaw)  # Utilisation de transformedData()
    data <-if (is.null(imputedData())) transformedData() else imputedData()
    column_data <- as.numeric(data[[input$columnKS]])
    
   
    
    
    dens <- density(column_data, na.rm = TRUE)  # Calculer la densité des données
    
    #Créer le graphique de base avec les données empiriques
    p <- plot_ly(x = dens$x, y = dens$y, type = 'scatter', mode = 'lines',fill = 'tozeroy',
                 name = 'Density Empirical', line = list(color = 'blue')) %>%
      layout(title = paste("Density with Theoretical Distribution (", input$ksLaw, ")"),
             xaxis = list(title = input$columnKS), yaxis = list(title = "Density"))
    
    # Choix de la loi pour afficher la distribution théorique correspondante
    switch(input$ksLaw,
           "Normale" = {
             mu <- mean(column_data, na.rm = TRUE)
             sigma <- sd(column_data, na.rm = TRUE)
             theoretical_y <- dnorm(dens$x, mean = mu, sd = sigma)
           },
           "Uniforme" = {
             theoretical_y <- dunif(dens$x, min = min(column_data), max = max(column_data))
           },
           "Exponentielle" = {
             rate <- 1 / mean(column_data, na.rm = TRUE)
             theoretical_y <- dexp(dens$x, rate = rate)
           }
    )
    
    # Ajouter la ligne théorique au graphique
    p %>% add_lines(x = dens$x, y = theoretical_y,
                    name = paste('Theoretical', input$ksLaw),
                    line = list(color = 'red'))
  })
  
  
  
  # Fonction hypothétique pour exécuter un test de Kolmogorov-Smirnov
  ks_test <- function(data, law) {
    if (law == "Normale") {
      fit <- MASS::fitdistr(data, "normal")
      test <- stats::ks.test(data, "pnorm", mean = fit$estimate["mean"], sd = fit$estimate["sd"])
    } else if (law == "Uniforme") {
      test <- stats::ks.test(data, "punif", min = min(data), max = max(data))
    } else {  # Exponentielle
      fit <- MASS::fitdistr(data, "exponential")
      test <- stats::ks.test(data, "pexp", rate = 1 / fit$estimate["rate"])
    }
    test
  }
  # Fonction hypothétique pour exécuter un test de khi-deux
  # Fonction hypothétique pour calculer les fréquences attendues selon différentes lois
  calculate_expected_frequencies <- function(data, column, law) {
    n <- length(data[[column]])
    categories <- sort(unique(data[[column]]))
    if (law == "Uniforme") {
      expected <- rep(n / length(categories), length(categories))
    } else if (law == "Exponentielle") {
      # Une simplification; dans la pratique, ajustez selon les besoins
      rates <- exp(-as.numeric(categories)) # Exponentielle décrémente avec la catégorie
      expected <- n * rates / sum(rates)
    } else {  # Normale ou autre
      # Supposer une distribution de probabilité normale centrée sur la moyenne des catégories
      mean_val <- mean(as.numeric(categories))
      sd_val <- sd(as.numeric(categories))
      probabilities <- dnorm(categories, mean = mean_val, sd = sd_val)
      expected <- n * probabilities / sum(probabilities)
    }
    return(expected)
  }
  
  # Fonction hypothétique pour calculer les fréquences attendues selon différentes lois
  calculate_expected_frequencies <- function(data, column, law) {
    n <- length(data[[column]])
    categories <- sort(unique(data[[column]]))
    if (law == "Uniforme") {
      expected <- rep(n / length(categories), length(categories))
    } else if (law == "Exponentielle") {
      # Une simplification; dans la pratique, ajustez selon les besoins
      rates <- exp(-as.numeric(categories)) # Exponentielle décrémente avec la catégorie
      expected <- n * rates / sum(rates)
    } else {  # Normale ou autre
      # Supposer une distribution de probabilité normale centrée sur la moyenne des catégories
      mean_val <- mean(as.numeric(categories))
      sd_val <- sd(as.numeric(categories))
      probabilities <- dnorm(categories, mean = mean_val, sd = sd_val)
      expected <- n * probabilities / sum(probabilities)
    }
    return(expected)
  }
  
  # Fonction pour exécuter le test de khi-deux
  chi_square_test <- function(data, column, law) {
    observed <- table(data[[column]])
    expected <- calculate_expected_frequencies(data, column, law)
    test <- chisq.test(observed, p = expected, rescale.p = TRUE)
    test
  }
  
  # Utilisation dans Shiny
  output$khiDeuxResult <- renderText({
    req(input$columnChi, input$chiLaw, transformedData())  # Utilisation de transformedData()
    data <- if (is.null(imputedData())) transformedData() else imputedData()
    chi_test <- chi_square_test(data, input$columnChi, input$chiLaw)
    paste("Statistique de test:", chi_test$statistic, "\nP-value:", chi_test$p.value)
  })
  
  output$khiDeuxPlot <- renderPlotly({
    req(input$columnChi, input$chiLaw)
    data <- transformedData()
    table_data <- table(data[[input$columnChi]])
    
    # Calculate density
    density_data <- density(rep(as.numeric(names(table_data)), table_data))
    
    # Create the plot with Plotly
    p <- plot_ly(x = density_data$x, y = density_data$y, type = 'scatter', mode = 'lines', 
                 fill = 'tozeroy', 
                 line = list(color = 'steelblue')) %>%
      layout(title = "Density Plot",
             xaxis = list(title = input$columnChi),
             yaxis = list(title = "Density"))
    
    p
  })
  

  # Réagir aux choix de colonne pour le test de Jarque-Bera
  observe({
    input$jbLaw  # Pour s'assurer que la réactivité est basée sur ce choix
    req(transformedData(),dataValidated())
    data <- if (is.null(imputedData())) transformedData() else imputedData()
    updateSelectInput(session, "columnJarque", choices = names(data)[sapply(data, is.numeric)])
  })
  
  # Réagir aux choix de colonne pour le droite de Henry (Q-Q plot)
  observe({
    input$henryLaw  # Pour s'assurer que la réactivité est basée sur ce choix
    req(transformedData(),dataValidated())
    data <- if (is.null(imputedData())) transformedData() else imputedData()
    updateSelectInput(session, "columnHenry", choices = names(data)[sapply(data, is.numeric)])
  })
  
  
  # Fonction pour exécuter le test de Jarque-Bera
  jarque_bera_test <- function(data, column) {
    if (column %in% names(data)) {
      test_result <- jarque.bera.test(data[[column]])
      return(test_result)
    } else {
      return(NULL)
    }
  }
  
  output$jarqueResult <- renderText({
    req(input$columnJarque, transformedData())  # Utilisation de transformedData()
    data <-if (is.null(imputedData())) transformedData() else imputedData()
    test_result <- jarque_bera_test(data(), input$columnJarque)  # Utilisation de data() pour obtenir les données réactives
    paste("JB Statistic:", test_result$statistic, "\nP-value:", test_result$p.value)
  })
  
  output$jarquePlot <- renderPlotly({
    req(input$columnJarque, transformedData())  # Utilisation de transformedData()
    data <- if (is.null(imputedData())) data() else imputedData()
    # Create density plot with Plotly
    p <- plot_ly(x = data()[[input$columnJarque]], type = "histogram", histnorm = "probability density",
                 marker = list(color = "steelblue"), opacity = 0.7) %>%
      layout(title = "Histogram for Jarque-Bera Test",
             xaxis = list(title = input$columnJarque),
             yaxis = list(title = "Density"))
    
    p
  })
  
  output$henryPlot <- renderPlotly({
    req(input$columnHenry, transformedData())  # Utilisation de transformedData()
    data <- if (is.null(imputedData())) transformedData() else imputedData()
    
    # Créer les quantiles normaux et les données de quantiles
    qq <- qqnorm(data()[[input$columnHenry]], plot.it = FALSE)
    df <- data.frame(Theoretical = qq$x, Sample = qq$y)
    
    p <- plot_ly(data = df, x = ~Theoretical, y = ~Sample, type = 'scatter', mode = 'markers') %>%
      add_lines(x = ~Theoretical, y = ~Theoretical, line = list(color = 'red')) %>%
      layout(title = "Graphique Henry (Q-Q Plot)",
             xaxis = list(title = "Quantiles théoriques"),
             yaxis = list(title = "Quantiles de l'échantillon"))
    
    p
  })
  
  output$dendroPlot <- renderPlot({
    req(results())
    if(!is.null(results())) {
      dist_mat <- dist(results()$ind$coord)  # Use PCA/MCA individual coordinates
      hc <- hclust(dist_mat)
      dend <- as.dendrogram(hc)
      dend <- color_branches(dend, k = input$numClusters)
      plot(dend)  # Utilisez plot standard pour les dendrogrammes
    }
  })
  
  
  output$downloadDendro <- downloadHandler(
    filename = function() { "dendrogram.png" },
    content = function(file) {
      png(file)
      dist_mat <- dist(results()$ind$coord)
      hc <- hclust(dist_mat)
      dend <- as.dendrogram(hc)
      dend <- color_branches(dend, k = input$numClusters)
      plot(dend)
      dev.off()
    }
  )
  
  observeEvent(input$main_tabs, {
    # Check if user tries to access certain tabs without data validation
    if (input$main_tabs %in% c( "Tests","Statistiques", "Correlations", "Analyse factorielle") && !dataValidated()) {
      # Show modal dialog
      showModal(modalDialog(
        title = "Validation Requise",
        "Vous devez valider les données dans le tab Imputation avant d'accéder à cette fonctionnalité.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      # Optionally force the user back to a safe tab
      updateTabsetPanel(session, "main_tabs", selected = "Imputation")
    }
  }, ignoreInit = TRUE)
  observeEvent(input$validateImputation, {
    dataValidated(TRUE)
    print("Data has been validated!")  # Debugging to confirm it's triggered
  })
  
  
}

shinyApp(ui, server) 

