# app_v4.R — Interface complète de l'application THERMIC (version modifiée)
# Modifications: note sur le choix d'emplacement d'enregistrement + formulation export
# + Mise à jour "Model Philosophy Overview" (pas de "single global slope")
# + Rappel FR sous les filtres
# + Encadré d'info dans l’onglet Mixed Model

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(openxlsx)
library(lme4)
library(lmerTest)
library(DT)

# Interface utilisateur / User Interface
ui <- fluidPage(
  # 1) CSS léger pour centrer le titre (et optionnellement ajuster l'espacement)
  tags$head(
    tags$style(HTML("
      #app-title { display:block; text-align:center; margin-bottom: 8px; }
      /* Optionnel : contenir la largeur globale si tu veux */
      /* .container-fluid { max-width: 1300px; } */
    "))
  ),
  
  # 2) Titre centré (windowTitle conservé si tu en as besoin)
  titlePanel(
    div(id = "app-title",
        "THERMIC - Thermal Resistance Modeling and Inactivation Calculator"),
    windowTitle = "THERMIC"
  ),
  
  # Ligne 1 : logos THERMIC, IAD, INRAE, UBE
  tags$div(
    style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
    tags$img(src = "THERMIC_logo.png", height = "300px"),
    tags$img(src = "IAD_logo.png", height = "100px"),
    tags$img(src = "INRAE_logo.png", height = "60px"),
    tags$img(src = "UBE_logo.png", height = "90px")
  ),
  
  # Ligne 2 : logo PAM centré et aligné
  tags$div(
    style = "display: flex; justify-content: center; align-items: center; gap: 30px; margin-bottom: 20px;",
    tags$img(src = "PAM_logo.png", height = "95px")
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("Data Loading"),
      fileInput("file", "Upload your data file (.tsv or .txt format)", 
                accept = c(".txt", ".tsv")),
      actionButton("load_sample", "Use Built-in Dataset", class = "btn-info"),
      
      hr(),
      h4("Prediction Parameters"),
      sliderInput("temp_step", "Temperature step for predictions (°C)", 
                  min = 1, max = 10, value = 5, step = 1),
      sliderInput("temp_range", "General prediction temperature range (°C)", 
                  min = 40, max = 100, value = c(50, 85), step = 5),
      
      hr(),
      h4("Filters"),
      selectInput("filter_type", "Filter by TypeMicroorganism", choices = NULL, multiple = TRUE),
      selectInput("filter_micro", "Filter by Microorganism", choices = NULL, multiple = TRUE),
      selectInput("filter_matrix", "Filter by Matrix_Type", choices = NULL, multiple = TRUE),
      helpText("Important: Filter by either 'Microorganism' OR 'TypeMicroorganism' (not both), and by 'Matrix_Type' to focus your analysis."),
      helpText("Important : filtrez soit par 'Microorganism' soit par 'TypeMicroorganism' (pas les deux), et utilisez 'Matrix_Type' pour cibler l'analyse."),
      
      hr(),
      h4("Uncertainty Parameters"),
      checkboxInput("use_uncertainty", "Include uncertainty analysis (if available)", TRUE),
      conditionalPanel(
        condition = "input.use_uncertainty == true",
        radioButtons("confidence_level", "Preferred confidence level:", 
                     choices = list("95%" = 95, "99%" = 99), selected = 95),
        checkboxInput("weighted_analysis", "Use weighted analysis based on uncertainty", TRUE),
        helpText("When enabled, predictions are weighted by their inverse variance.")
      ),
      
      hr(),
      h4("Heuristic Model Parameters"),
      numericInput("heuristic_shift", "Heuristic correction factor (log10 units)", 
                   value = 0.2, min = -2, max = 2, step = 0.1),
      checkboxInput("heuristic_use_range", "Apply heuristic correction only within specific range", FALSE),
      conditionalPanel(
        condition = "input.heuristic_use_range == true",
        sliderInput("heuristic_range", "Heuristic correction range (°C)", 
                    min = 40, max = 100, value = c(50, 75), step = 5)
      ),
      helpText("Positive values increase D-values, negative values decrease them."),
      
      h4("Truncated Model Parameters"),
      sliderInput("trunc_range", "Correction range for truncated model (°C)", 
                  min = 40, max = 100, value = c(50, 75), step = 5),
      numericInput("truncated_shift", "Truncated correction factor (log10 units)", 
                   value = 0.1, min = -2, max = 2, step = 0.1),
      helpText("Correction is applied only within the specified temperature range."),
      
      h4("Mixed Effects Model Parameters"),
      checkboxInput("mixed_use_range", "Apply mixed model correction only within specific range", FALSE),
      conditionalPanel(
        condition = "input.mixed_use_range == true",
        sliderInput("mixed_range", "Mixed model correction range (°C)", 
                    min = 40, max = 100, value = c(45, 85), step = 5)
      ),
      
      hr(),
      actionButton("start_analysis", "Start Analysis", class = "btn-primary btn-lg"),
      
      br(), br(),
      conditionalPanel(
        condition = "output.validation_messages != ''",
        div(style = "background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 10px; border-radius: 5px;",
            h5("⚠️ Validation Warnings:"),
            textOutput("validation_messages")
        )
      ),
      
      br(),
      p("Note: Analysis may take a few seconds to complete.", 
        style = "color: gray; font-size: 0.9em;")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("📋 Introduction & Guide", 
                 div(style = "padding: 20px;",
                     h2("THERMIC - User Guide"),
                     
                     h3("🆕 New Features in Version 3 / Nouvelles fonctionnalités de la version 3"),
                     div(style = "background-color: #e8f5e8; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                         tags$ul(
                           tags$li(tags$strong("Enhanced microorganism support / Support étendu des microorganismes:"), " Now includes Yeast_ascospores, Moulds, and Mould_ascospores / Inclut maintenant Yeast_ascospores, Moulds, et Mould_ascospores"),
                           tags$li(tags$strong("Confidence interval analysis / Analyse des intervalles de confiance:"), " Supports 95% and 99% CI for D and Z values / Support des IC à 95% et 99% pour les valeurs D et Z"),
                           tags$li(tags$strong("Weighted predictions / Prédictions pondérées:"), " Uses inverse variance weighting when uncertainty data is available / Utilise la pondération par variance inverse quand les données d'incertitude sont disponibles"),
                           tags$li(tags$strong("Flexible temperature ranges / Plages de température flexibles:"), " Model-specific temperature ranges for targeted corrections / Plages de température spécifiques aux modèles pour des corrections ciblées"),
                           tags$li(tags$strong("Improved uncertainty visualization / Visualisation améliorée de l'incertitude:"), " Confidence bands on all plots / Bandes de confiance sur tous les graphiques")
                         )
                     ),
                     
                     h3("📝 Application Overview / Aperçu de l'application"),
                     p("This application performs meta-analysis of D-values (decimal reduction time) for thermal inactivation of microorganisms. D-values represent the time required to reduce a microbial population by one log unit (90% reduction) at a given temperature."),
                     p("Cette application effectue une méta-analyse des valeurs D (temps de réduction décimale) pour l'inactivation thermique des microorganismes. Les valeurs D représentent le temps nécessaire pour réduire une population microbienne d'une unité logarithmique (réduction de 90%) à une température donnée."),
                     
                     h3("🎯 Objectives / Objectifs"),
                     div(style = "display: flex; gap: 20px;",
                         div(style = "flex: 1;",
                             h4("English:"),
                             tags$ul(
                               tags$li("Predict D-values across different temperatures using existing experimental data"),
                               tags$li("Apply various correction models to improve prediction accuracy"),
                               tags$li("Account for variability between microorganisms and publications"),
                               tags$li("Incorporate experimental uncertainties through confidence intervals"),
                               tags$li("Provide robust statistical modeling with weighted analysis")
                             )
                         ),
                         div(style = "flex: 1;",
                             h4("Français:"),
                             tags$ul(
                               tags$li("Prédire les valeurs D à différentes températures en utilisant des données expérimentales existantes"),
                               tags$li("Appliquer divers modèles de correction pour améliorer la précision des prédictions"),
                               tags$li("Tenir compte de la variabilité entre microorganismes et publications"),
                               tags$li("Incorporer les incertitudes expérimentales par des intervalles de confiance"),
                               tags$li("Fournir une modélisation statistique robuste avec analyse pondérée")
                             )
                         )
                     ),
                     
                     h3("📊 Data Structure Requirements / Exigences de structure des données"),
                     div(style = "display: flex; gap: 20px;",
                         div(style = "flex: 1;",
                             h4("English:"),
                             p("Your TSV file must contain the following columns:"),
                             h5("Required Columns:"),
                             tags$ul(
                               tags$li(tags$strong("Microorganism:"), " Name of the microorganism"),
                               tags$li(tags$strong("TypeMicroorganism:"), " Category: 'Bacteria', 'Bacterial spores', 'Enveloped virus', 'non-enveloped virus', 'Yeast', 'Yeast_ascospores', 'Moulds', 'Mould_ascospores'"),
                               tags$li(tags$strong("Temperature:"), " Temperature in °C (numeric)"),
                               tags$li(tags$strong("Dvalue:"), " D-value in minutes (numeric)"),
                               tags$li(tags$strong("Z_publication:"), " Z-value from publication (numeric, can be empty)"),
                               tags$li(tags$strong("Publication:"), " Reference publication"),
                               tags$li(tags$strong("Category:"), " Additional categorization"),
                               tags$li(tags$strong("Matrix_Type:"), " Type of matrix/medium")
                             )
                         ),
                         div(style = "flex: 1;",
                             h4("Français:"),
                             p("Votre fichier TSV doit contenir les colonnes suivantes:"),
                             h5("Colonnes requises:"),
                             tags$ul(
                               tags$li(tags$strong("Microorganism:"), " Nom du microorganisme"),
                               tags$li(tags$strong("TypeMicroorganism:"), " Catégorie: 'Bacteria', 'Bacterial spores', 'Enveloped virus', 'non-enveloped virus', 'Yeast', 'Yeast_ascospores', 'Moulds', 'Mould_ascospores'"),
                               tags$li(tags$strong("Temperature:"), " Température en °C (numérique)"),
                               tags$li(tags$strong("Dvalue:"), " Valeur D en minutes (numérique)"),
                               tags$li(tags$strong("Z_publication:"), " Valeur Z de la publication (numérique, peut être vide)"),
                               tags$li(tags$strong("Publication:"), " Publication de référence"),
                               tags$li(tags$strong("Category:"), " Catégorisation supplémentaire"),
                               tags$li(tags$strong("Matrix_Type:"), " Type de matrice/milieu")
                             )
                         )
                     ),
                     
                     h3("🧮 Model Philosophy Overview / Aperçu de la philosophie des modèles"),
                     div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 25px;",
                         h4("Two Fundamentally Different Approaches / Deux approches fondamentalement différentes"),
                         
                         # INDIVIDUAL-BASED
                         div(style = "margin-bottom: 20px;",
                             h5("📊 Individual-Based Models (Base, Heuristic, Truncated) / Modèles basés sur l'individu (Base, Heuristique, Tronqué)"),
                             div(style = "background-color: #e3f2fd; padding: 15px; border-radius: 5px; margin: 10px 0;",
                                 div(style = "display: flex; gap: 20px;",
                                     div(style = "flex: 1;",
                                         h6("English"),
                                         p(tags$strong("Core principle:"), " each microorganism keeps its own thermal sensitivity (microbe-specific slope 1/Z)."),
                                         tags$ul(
                                           tags$li("Relationship: ", tags$code("log10(D) = log10(D_ref) - (T - T_ref)/Z")),
                                           tags$li("Heuristic and truncated options do not change the slope: they adjust the intercept (heuristic) or restrict the valid range (truncated).")
                                         )
                                     ),
                                     div(style = "flex: 1;",
                                         h6("Français"),
                                         p(tags$strong("Principe:"), " chaque microorganisme conserve sa propre sensibilité thermique (pente 1/Z spécifique)."),
                                         tags$ul(
                                           tags$li("Relation : ", tags$code("log10(D) = log10(D_ref) - (T - T_ref)/Z")),
                                           tags$li("Les variantes heuristique et tronquée ne modifient pas la pente : elles ajustent l’ordonnée (heuristique) ou limitent la plage valide (tronquée).")
                                         )
                                     )
                                 )
                             )
                         ),
                         
                         # MIXED-EFFECTS
                         div(style = "margin-bottom: 20px;",
                             h5("🔬 Mixed-Effects Model / Modèle à effets mixtes"),
                             div(style = "background-color: #fff3e0; padding: 15px; border-radius: 5px; margin: 10px 0;",
                                 div(style = "display: flex; gap: 20px;",
                                     div(style = "flex: 1;",
                                         h6("English"),
                                         tags$ul(
                                           tags$li(tags$strong("Population-level temperature effect (fixed effect):"), " estimated from all data."),
                                           tags$li(tags$strong("Random effects:"), " ", tags$code("(1|Microorganism)"), " (vertical shift), ", tags$code("(Temperature|Publication)"), " (slope/intercept deviations)."),
                                           tags$li(tags$strong("Model selection by AIC:"), " compares ", tags$code("(1|Publication)"), ", ", tags$code("(Temperature|Publication)"), ", and ", tags$code("(1|Microorganism)+(Temperature|Publication)"), "; fallback is random-intercept."),
                                           tags$li(tags$strong("Inverse-variance weighting"), " when D-value SEs are available.")
                                         )
                                     ),
                                     div(style = "flex: 1;",
                                         h6("Français"),
                                         tags$ul(
                                           tags$li(tags$strong("Effet de température au niveau population (effet fixe):"), " estimé sur l’ensemble des données."),
                                           tags$li(tags$strong("Effets aléatoires :"), " ", tags$code("(1|Microorganism)"), " (décalage vertical), ", tags$code("(Temperature|Publication)"), " (déviations de pente/intercept)."),
                                           tags$li(tags$strong("Sélection par AIC :"), " compare ", tags$code("(1|Publication)"), ", ", tags$code("(Temperature|Publication)"), " et ", tags$code("(1|Microorganism)+(Temperature|Publication)"), " ; repli : intercept aléatoire."),
                                           tags$li(tags$strong("Pondération par variance inverse"), " si des SE sur D sont disponibles.")
                                         )
                                     )
                                 )
                             )
                         ),
                         
                         # WHEN TO USE
                         div(style = "background-color: #fff8e1; padding: 15px; border-radius: 5px; border-left: 4px solid #ffc107;",
                             h5("🎯 When to Use Each Approach / Quand utiliser chaque approche"),
                             div(style = "display: flex; gap: 20px;",
                                 div(style = "flex: 1;",
                                     h6("English"),
                                     tags$ul(
                                       tags$li(tags$strong("Individual-based:"), " when specific Z-values are trusted and microbe-level variability should be preserved."),
                                       tags$li(tags$strong("Mixed-effects:"), " when a robust population trend is desired across heterogeneous studies; partial pooling adds stability.")
                                     )
                                 ),
                                 div(style = "flex: 1;",
                                     h6("Français"),
                                     tags$ul(
                                       tags$li(tags$strong("Modèles individuels :"), " quand des valeurs Z spécifiques sont fiables et que l’on veut préserver la variabilité au niveau microbe."),
                                       tags$li(tags$strong("Modèle à effets mixtes :"), " quand on vise une tendance de population robuste sur des études hétérogènes ; la mise en commun partielle apporte de la stabilité.")
                                     )
                                 )
                             )
                         ),
                         
                         # WHY DIFFERENT
                         div(style = "background-color: #f0f8ff; padding: 15px; border-radius: 5px; margin-top: 15px;",
                             h5("🔍 Why results can differ? / Pourquoi des résultats différents ?"),
                             div(style = "display: flex; gap: 20px;",
                                 div(style = "flex: 1;",
                                     h6("English"),
                                     tags$ol(
                                       tags$li(tags$strong("Slopes & partial pooling:"), " mixed-effects estimates a population-level effect; if random slopes are selected, publication-specific slopes deviate around it; otherwise a common slope is used."),
                                       tags$li(tags$strong("Smoothing:"), " partial pooling pulls predictions toward the population trend, especially for sparse/noisy data."),
                                       tags$li(tags$strong("Variability management:"), " individual models preserve slope differences; mixed-effects explains variability via random intercepts and (optionally) random slopes.")
                                     )
                                 ),
                                 div(style = "flex: 1;",
                                     h6("Français"),
                                     tags$ol(
                                       tags$li(tags$strong("Pentes & mise en commun partielle :"), " l’effet de population est estimé ; si des pentes aléatoires sont retenues, les pentes varient par publication ; sinon, une pente commune est utilisée."),
                                       tags$li(tags$strong("Lissage :"), " la mise en commun partielle rapproche les prédictions de la tendance de population, surtout quand les données sont rares/bruitées."),
                                       tags$li(tags$strong("Gestion de la variabilité :"), " les modèles individuels préservent les différences de pente ; les effets mixtes expliquent la variabilité via des intercepts (et éventuellement) des pentes aléatoires.")
                                     )
                                 )
                             )
                         )
                     )
                 )
        ),
        
        tabPanel("📊 Data Preview", 
                 h3("Data Overview"),
                 DTOutput("table_preview"),
                 br(),
                 div(id = "data_summary", 
                     h4("Data Summary"),
                     verbatimTextOutput("data_summary_text")
                 )
        ),
        
        tabPanel("📈 Base Model", 
                 fluidRow(
                   column(8, 
                          h3("Base Model - Predicted D-values"),
                          plotOutput("plot_base", height = "400px"),
                          br(),
                          h4("Upper and Lower Limits"),
                          plotOutput("limits_base", height = "400px")
                   ),
                   column(4,
                          h3("Prediction Table"),
                          DTOutput("table_base"),
                          br(),
                          downloadButton("download_base", "Download Base Model", class = "btn-info")
                   )
                 )
        ),
        
        tabPanel("🔧 Heuristic Model",
                 fluidRow(
                   column(8,
                          h3("Heuristic Model - Corrected D-values"),
                          plotOutput("plot_heuristic", height = "400px"),
                          br(),
                          h4("Upper and Lower Limits"),
                          plotOutput("limits_heuristic", height = "400px")
                   ),
                   column(4,
                          h3("Prediction Table"),
                          DTOutput("table_heuristic"),
                          br(),
                          downloadButton("download_heuristic", "Download Heuristic Model", class = "btn-info")
                   )
                 )
        ),
        
        tabPanel("✂️ Truncated Model", 
                 fluidRow(
                   column(8,
                          h3("Truncated Model - Range-Specific Correction"),
                          plotOutput("plot_truncated", height = "400px"),
                          br(),
                          h4("Upper and Lower Limits"),
                          plotOutput("limits_truncated", height = "400px")
                   ),
                   column(4,
                          h3("Prediction Table"),
                          DTOutput("table_truncated"),
                          br(),
                          downloadButton("download_truncated", "Download Truncated Model", class = "btn-info")
                   )
                 )
        ),
        
        tabPanel("🔬 Mixed Model", 
                 fluidRow(
                   column(8,
                          h3("Mixed Effects Model - Advanced Correction"),
                          plotOutput("plot_mixed", height = "400px"),
                          br(),
                          h4("Upper and Lower Limits"),
                          plotOutput("limits_mixed", height = "400px"),
                          br(),
                          div(style = "background:#fff3e0; padding:12px; border-left:4px solid #fb8c00; border-radius:6px;",
                              h5("Modeling Notes / Notes de modélisation"),
                              tags$ul(
                                tags$li("AIC-driven selection among random-effects structures: ", 
                                        tags$code("(1|Publication)"), ", ",
                                        tags$code("(Temperature|Publication)"), ", ",
                                        tags$code("(1|Microorganism) + (Temperature|Publication)"), "."),
                                tags$li("Fallback to random-intercept when comparison fails."),
                                tags$li("Inverse-variance weighting when D-value SEs are available.")
                              )
                          )
                   ),
                   column(4,
                          h3("Prediction Table"),
                          DTOutput("table_mixed"),
                          br(),
                          downloadButton("download_mixed", "Download Mixed Model", class = "btn-info"),
                          br(), br(),
                          div(id = "mixed_model_info",
                              h5("Model Information:"),
                              verbatimTextOutput("mixed_model_summary")
                          )
                   )
                 )
        ),
        
        tabPanel("💾 Download Results", 
                 h3("Export Results"),
                 br(),
                 downloadButton("download_excel", "Download All Predictions (Excel)", 
                                class = "btn-success btn-lg"),
                 br(), br(),
                 p("Download an Excel workbook containing predictions from all models, confidence intervals, and fit diagnostics."),
                 p("You can choose the destination folder and filename on your computer via the standard download dialog. / Vous pourrez choisir le dossier et le nom de fichier via la boîte de dialogue de téléchargement standard.")
        ),
        
        tabPanel("📝 Log Messages", 
                 h3("Analysis Log"),
                 verbatimTextOutput("log_messages"),
                 br(),
                 p("This log shows the progress and any issues during analysis.")
        )
      )
    )
  )
)

# Charger le serveur / Load the server
source("server_logic_v4.R", local = TRUE)

# Lancer l'application / Launch the application
shinyApp(ui = ui, server = server)
