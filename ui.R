library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = tags$span("Data Explorer Pro", style = "font-weight: 600;")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Chargement de données", tabName = "data_upload", icon = icon("database")),
      menuItem("Analyses", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("Modélisation", tabName = "modelling", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("\n      body { font-family: 'Segoe UI', Arial, sans-serif; }\n      .content-wrapper { background: linear-gradient(135deg, #f5f7fb 0%, #eef4ff 100%); }\n      .main-header .logo { font-weight: 700; letter-spacing: 0.2px; }\n      .box { border-radius: 14px; border: 1px solid #dfe7f2; box-shadow: 0 8px 20px rgba(15, 23, 42, 0.06); }\n      .small-box { border-radius: 14px; box-shadow: 0 8px 20px rgba(15, 23, 42, 0.06); }\n      .alert-info { background: linear-gradient(90deg, #eef6ff 0%, #f8fbff 100%); border-color: #cfe2ff; color: #0d3b66; }\n      .btn { border-radius: 8px; }\n      .form-group { margin-bottom: 10px; }\n    "))),
    tabItems(
      tabItem(
        tabName = "data_upload",
        fluidRow(
          valueBoxOutput("rows_value", width = 3),
          valueBoxOutput("columns_value", width = 3),
          valueBoxOutput("missing_value_count", width = 3),
          valueBoxOutput("target_value_box", width = 3)
        ),
        fluidRow(
          box(
            title = "Dataset status",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            uiOutput("dataset_status")
          )
        ),
        fluidRow(
          box(
            title = "Upload",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            fileInput("file", "Choose a CSV file:"),
            checkboxInput("header", "Does the file have a header row?", TRUE),
            actionButton("loadData", "Load Data")
          ),
          box(
            title = "Preprocessing",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            selectInput("categorical_variables", "Select Categorical and Nominal Variables:", multiple = TRUE, choices = NULL),
            selectInput("column_to_convert", "Select Column to Convert", choices = NULL),
            actionButton("convert_to_numeric", "Convert to Numeric"),
            br(), br(),
            selectInput("training_variable", "Variable for Training:", choices = ""),
            selectInput("missing_columns", "Select Variables with Missing Values:", multiple = TRUE, choices = NULL),
            actionButton("removeMissingCols", "Remove Selected Columns"),
            actionButton("removeMissingRows", "Remove Rows with NA"),
            actionButton("remove_duplicates", "Remove Duplicates"),
            br(), br(),
            numericInput("rows_to_delete", "Number of Rows to Delete", value = 0, min = 0),
            numericInput("target_value", "Target Value to Delete", value = 4),
            actionButton("delete_rows", "Delete Rows"),
            br(), br(),
            actionButton("normalize", "Normalize Data"),
            actionButton("zscore_normalize", "Normalize Data (Z-score)"),
            br(), br(),
            actionButton("refreshData", "Refresh Data"),
            actionButton("resetData", "Reset dataset", icon = icon("rotate-left"))
          )
        )
      ),
      tabItem(
        tabName = "analysis",
        fluidRow(
          box(
            title = "Aperçu des données",
            status = "primary",
            solidHeader = TRUE,
            style = "overflow-x: auto;",
            DTOutput("table"),
            width = 12
          ),
          box(
            title = "Variable Selection",
            status = "primary",
            solidHeader = TRUE,
            width = 2,
            selectInput("x_variable", "X Variable:", choices = ""),
            selectInput("y_variable", "Y Variable:", choices = "")
          ),
          box(
            title = "Statistiques descriptives unidimensionnelle",
            status = "primary",
            solidHeader = TRUE,
            tabsetPanel(
              tabPanel("Histogramme", numericInput("binwidth_input", "Binwidth:", value = 0.2, min = 0.01, step = 0.01), plotlyOutput("histogram")),
              tabPanel("Box Plot", plotlyOutput("boxplot")),
              tabPanel("Density Plot", plotlyOutput("densityplot")),
              tabPanel("Extra", verbatimTextOutput("univariate_analysis")),
              tabPanel("Résumé", verbatimTextOutput("summary"))
            ),
            width = 5
          ),
          box(
            title = "Analyse bidimensionnelle",
            status = "primary",
            solidHeader = TRUE,
            tabsetPanel(
              tabPanel("Correlation plot", plotlyOutput("bivariate_analysis")),
              tabPanel("Correlation Matrix", plotOutput("correlation_matrix_plot"))
            ),
            width = 5
          ),
          box(
            title = "Label Analysis",
            status = "primary",
            solidHeader = TRUE,
            tabsetPanel(
              tabPanel("Bar Plot", plotOutput(outputId = "label_analysis", height = 500, width = 600)),
              tabPanel("Pie Chart", plotOutput("pie_chart", height = 500, width = 600))
            ),
            width = 6
          )
        )
      ),
      tabItem(
        tabName = "modelling",
        fluidRow(
          box(
            title = "SVM",
            status = "primary",
            solidHeader = TRUE,
            tabsetPanel(
              tabPanel("Résumé", verbatimTextOutput("model_results")),
              tabPanel("Evaluation", plotOutput("confusion_matrix_plot"), plotOutput("roc_auc_curve_plot")),
              tabPanel("PDPs", uiOutput("pdp_output")),
              conditionalPanel(condition = "input.train_model > 0", downloadButton("downloadSVM", "Download SVM Model"))
            ),
            width = 6
          ),
          box(
            title = "Random Forest",
            status = "primary",
            solidHeader = TRUE,
            tabsetPanel(
              tabPanel("Résumé", verbatimTextOutput("model_results_RF")),
              tabPanel("Evaluation", plotOutput("confusion_matrix_plot_RF"), plotOutput("roc_auc_curve_plot_RF")),
              tabPanel("Feature Importances", plotOutput("feature_importance_plot")),
              conditionalPanel(condition = "input.train_model > 0", downloadButton("downloadRF", "Download Random Forest Model"))
            ),
            width = 6
          ),
          box(
            title = "Logistic Regression",
            status = "primary",
            solidHeader = TRUE,
            tabsetPanel(
              tabPanel("Résumé", verbatimTextOutput("model_results_LR")),
              tabPanel("Evaluation", plotOutput("confusion_matrix_plot_LR"), plotOutput("roc_auc_curve_plot_LR")),
              tabPanel("Feature Importances", plotOutput("feature_importance_plot_LR")),
              conditionalPanel(condition = "input.train_model > 0", downloadButton("downloadLR", "Download Logistic Regression Model"))
            ),
            width = 6
          ),
          box(
            title = "Splitting Data",
            status = "primary",
            solidHeader = TRUE,
            column(
              6,
              sliderInput("training_percentage", "Training Set Percentage:", value = 70, min = 1, max = 99, step = 1, ticks = FALSE, width = "100%"),
              sliderInput("test_percentage", "Test Set Percentage:", value = 30, min = 1, max = 99, step = 1, ticks = FALSE, width = "100%")
            ),
            column(6, actionButton("train_model", "Entraîner le modèle"))
          )
        )
      )
    )
  )
)
