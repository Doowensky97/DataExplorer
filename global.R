required_packages <- c(
  "shiny",
  "DT",
  "dplyr",
  "ggplot2",
  "plotly",
  "e1071",
  "rpart",
  "psych",
  "corrplot",
  "caret",
  "pROC",
  "ROCR",
  "pdp",
  "randomForest",
  "shinyjs",
  "shinyWidgets",
  "shinydashboard"
)

user_lib <- Sys.getenv("R_LIBS_USER")
if (!nzchar(user_lib)) {
  user_lib <- file.path(Sys.getenv("HOME"), "R", "win-library", paste0(R.version$major, ".", R.version$minor))
}
if (!dir.exists(user_lib)) {
  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
}
.libPaths(c(user_lib, .libPaths()))

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org", lib = user_lib)
  }
  library(pkg, character.only = TRUE)
}

options(shiny.maxRequestSize = 30 * 1024^2)

source("model_utils.R")

update_dataset_inputs <- function(session, data_df, selected_target = NULL) {
  if (is.null(data_df) || ncol(data_df) == 0) {
    return()
  }

  choices <- names(data_df)
  effective_target <- if (!is.null(selected_target) && nzchar(selected_target)) {
    selected_target
  } else {
    tail(choices, 1)
  }

  updateSelectInput(session, "x_variable", choices = choices)
  updateSelectInput(session, "y_variable", choices = choices)
  updateSelectInput(session, "training_variable", choices = choices, selected = effective_target)
  updateSelectInput(session, "categorical_variables", choices = choices)
  updateSelectInput(session, "missing_columns", choices = names(data_df)[colSums(is.na(data_df)) > 0])
  updateSelectInput(session, "column_to_convert", choices = choices)
}
