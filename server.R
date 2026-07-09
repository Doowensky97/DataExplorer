library(shiny)

server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  training_set <- reactiveVal(NULL)
  test_set <- reactiveVal(NULL)
  trained_model <- reactiveVal(NULL)
  trained_model_RF <- reactiveVal(NULL)
  logistic_model <- reactiveVal(NULL)

  refresh_dataset_summary <- function(data_input) {
    if (is.null(data_input)) {
      output$rows_value <- renderValueBox(valueBox("0", "Rows", icon = icon("table"), color = "blue"))
      output$columns_value <- renderValueBox(valueBox("0", "Columns", icon = icon("columns"), color = "teal"))
      output$missing_value_count <- renderValueBox(valueBox("0", "Missing values", icon = icon("exclamation-triangle"), color = "yellow"))
      output$target_value_box <- renderValueBox(valueBox("—", "Target", icon = icon("bullseye"), color = "purple"))
      output$dataset_status <- renderUI({
        div(class = "alert alert-info", style = "padding: 10px; border-radius: 8px;", "No dataset loaded yet. Upload a CSV file to start exploring.")
      })
      return()
    }

    target_name <- if (!is.null(input$training_variable) && nzchar(input$training_variable)) input$training_variable else names(data_input)[ncol(data_input)]
    output$rows_value <- renderValueBox(valueBox(nrow(data_input), "Rows", icon = icon("table"), color = "blue"))
    output$columns_value <- renderValueBox(valueBox(ncol(data_input), "Columns", icon = icon("columns"), color = "teal"))
    output$missing_value_count <- renderValueBox(valueBox(sum(is.na(data_input)), "Missing values", icon = icon("exclamation-triangle"), color = "yellow"))
    output$target_value_box <- renderValueBox(valueBox(target_name, "Target", icon = icon("bullseye"), color = "purple"))
    output$dataset_status <- renderUI({
      div(
        class = "alert alert-info",
        style = "padding: 10px; border-radius: 8px;",
        strong("Current dataset: "), paste(nrow(data_input), "rows and", ncol(data_input), "columns"),
        br(),
        strong("Target variable: "), target_name
      )
    })
  }

  reset_dataset_state <- function() {
    data(NULL)
    training_set(NULL)
    test_set(NULL)
    trained_model(NULL)
    trained_model_RF(NULL)
    logistic_model(NULL)
    update_dataset_inputs(session, NULL)
    refresh_dataset_summary(NULL)
    output$table <- renderDT({ datatable(data.frame(), options = list(scrollX = TRUE)) })
    output$summary <- renderPrint({ cat("No dataset loaded yet.\n") })
  }

  observeEvent(input$file, {
    req(input$file)
    file_ext <- tools::file_ext(input$file$name)
    if (tolower(file_ext) != "csv") {
      showModal(modalDialog("Veuillez charger un fichier CSV valide.", title = "Erreur de chargement", easyClose = TRUE))
      return()
    }

    data_input <- read.csv(input$file$datapath, header = input$header, stringsAsFactors = FALSE)
    if (ncol(data_input) < 2) {
      showModal(modalDialog("Le fichier doit contenir au moins deux colonnes.", title = "Erreur de chargement", easyClose = TRUE))
      return()
    }

    data_input <- data_input[, -1, drop = FALSE]
    last_column <- names(data_input)[ncol(data_input)]
    data_input[[last_column]] <- as.factor(data_input[[last_column]])
    data(data_input)
    update_dataset_inputs(session, data_input, last_column)
    refresh_dataset_summary(data_input)
    showModal(modalDialog("Le fichier a été chargé avec succès.", easyClose = TRUE))
  })

  observeEvent(input$loadData, {
    req(data())
    output$table <- renderDT({ datatable(data(), options = list(scrollX = TRUE)) })
    output$summary <- renderPrint({ summary(data()) })
    refresh_dataset_summary(data())
  })

  observeEvent(input$normalize, {
    req(data())
    data(as.data.frame(scale(data())))
    refresh_dataset_summary(data())
  })

  observeEvent(input$zscore_normalize, {
    req(data())
    data(as.data.frame(scale(data())))
    refresh_dataset_summary(data())
    showNotification("Les données ont été normalisées avec Z-score.", type = "message")
  })

  observeEvent(input$categorical_variables, {
    req(data())
    data_input <- data()
    for (var in input$categorical_variables) {
      if (var %in% colnames(data_input)) {
        data_input[[var]] <- as.factor(data_input[[var]])
      }
    }
    data(data_input)
  })

  observeEvent(input$convert_to_numeric, {
    req(data(), input$column_to_convert)
    data_input <- data()
    if (input$column_to_convert %in% colnames(data_input)) {
      data_input[[input$column_to_convert]] <- as.integer(data_input[[input$column_to_convert]])
      data(data_input)
      update_dataset_inputs(session, data_input, input$training_variable)
      showModal(modalDialog("La colonne a été convertie en numérique avec succès.", easyClose = TRUE))
    } else {
      showModal(modalDialog("Veuillez sélectionner une colonne valide.", title = "Erreur de conversion", easyClose = TRUE))
    }
  })

  observeEvent(input$removeMissingCols, {
    req(data())
    if (is.null(input$missing_columns) || length(input$missing_columns) == 0) {
      showModal(modalDialog("Veuillez sélectionner des variables à supprimer.", title = "Aucune sélection", easyClose = TRUE))
      return()
    }

    data_input <- data()[, !names(data()) %in% input$missing_columns, drop = FALSE]
    data(data_input)
    update_dataset_inputs(session, data_input, input$training_variable)
    refresh_dataset_summary(data_input)
    output$table <- renderDT({ datatable(data_input, options = list(scrollX = TRUE)) })
    showModal(modalDialog("Les variables sélectionnées ont été supprimées.", easyClose = TRUE))
  })

  observeEvent(input$removeMissingRows, {
    req(data())
    data_input <- na.omit(data())
    data(data_input)
    update_dataset_inputs(session, data_input, input$training_variable)
    refresh_dataset_summary(data_input)
    showModal(modalDialog("Les lignes avec des valeurs manquantes ont été supprimées.", easyClose = TRUE))
  })

  observeEvent(input$refreshData, {
    req(data())
    update_dataset_inputs(session, data(), input$training_variable)
    refresh_dataset_summary(data())
    showModal(modalDialog("Les données ont été actualisées avec succès.", easyClose = TRUE))
  })

  observeEvent(input$resetData, {
    reset_dataset_state()
    showNotification("Le dataset a été réinitialisé.", type = "message")
  })

  observeEvent(input$delete_rows, {
    req(data())
    data_input <- data()
    rows_to_delete <- which(data_input[[input$training_variable]] == input$target_value)
    if (length(rows_to_delete) < input$rows_to_delete) {
      showModal(modalDialog("Le nombre de lignes demandées est supérieur au nombre disponible. Les lignes disponibles ont été supprimées.", easyClose = TRUE))
      data_input <- data_input[-rows_to_delete, ]
    } else {
      data_input <- data_input[-rows_to_delete[1:input$rows_to_delete], ]
    }
    data(data_input)
    update_dataset_inputs(session, data_input, input$training_variable)
    refresh_dataset_summary(data_input)
    showModal(modalDialog("Les lignes ont été supprimées avec succès.", easyClose = TRUE))
  })

  observeEvent(input$remove_duplicates, {
    req(data())
    n_before <- nrow(data())
    data_unique <- data() %>% distinct(.keep_all = TRUE)
    n_after <- nrow(data_unique)
    n_duplicates <- n_before - n_after
    showNotification(paste("Le nombre de doublons supprimés est de :", n_duplicates), type = "message")
    data(data_unique)
    refresh_dataset_summary(data_unique)
  })

  output$histogram <- renderPlotly({
    req(data(), input$x_variable)
    ggplotly(
      ggplot(data(), aes(x = .data[[input$x_variable]])) +
        geom_histogram(binwidth = input$binwidth_input, fill = "blue", color = "black", alpha = 0.7) +
        labs(title = "Histogram", x = input$x_variable, y = "Frequency")
    )
  })

  output$boxplot <- renderPlotly({
    req(data(), input$x_variable)
    ggplotly(
      ggplot(data(), aes(y = .data[[input$x_variable]])) +
        geom_boxplot(fill = "green", color = "black", alpha = 0.7) +
        labs(title = "Box Plot", y = input$x_variable)
    )
  })

  output$densityplot <- renderPlotly({
    req(data(), input$x_variable)
    ggplotly(
      ggplot(data(), aes(x = .data[[input$x_variable]])) +
        geom_density(fill = "purple", color = "black", alpha = 0.7) +
        labs(title = "Density Plot", x = input$x_variable, y = "Density")
    )
  })

  output$univariate_analysis <- renderPrint({
    req(data())
    var <- input$training_variable
    if (is.null(var) || var == "") {
      return("Veuillez sélectionner une variable pour l'analyse unidimensionnelle.")
    }
    if (!var %in% colnames(data())) {
      return("La variable sélectionnée n'existe pas dans les données.")
    }

    cat("Analyse unidimensionnelle pour la variable :", var, "\n")
    cat("Nombre de valeurs manquantes :", sum(is.na(data()[[var]])), "\n")
    if (is.numeric(data()[[var]])) {
      cat("Moyenne :", mean(data()[[var]], na.rm = TRUE), "\n")
      cat("Écart-type :", sd(data()[[var]], na.rm = TRUE), "\n")
      cat("Valeurs uniques :", length(unique(na.omit(data()[[var]]))), "\n")
    } else if (is.factor(data()[[var]])) {
      cat("Nombre de catégories :", length(levels(data()[[var]])), "\n")
      cat("Fréquence des catégories :\n")
      print(table(data()[[var]]))
    }
  })

  output$bivariate_analysis <- renderPlotly({
    req(data(), input$x_variable, input$y_variable)
    plot_ly(data(), x = ~.data[[input$x_variable]], y = ~.data[[input$y_variable]], type = "scatter", mode = "markers") %>%
      layout(xaxis = list(title = input$x_variable), yaxis = list(title = input$y_variable))
  })

  output$correlation_matrix_plot <- renderPlot({
    req(data())
    numeric_data <- data()[sapply(data(), is.numeric)]
    if (ncol(numeric_data) < 2) {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Pas assez de variables numériques pour calculer une matrice de corrélation.")
      return()
    }
    corr_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
    corrplot(corr_matrix, method = "color", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
  })

  output$label_analysis <- renderPlot({
    req(data())
    last_column <- names(data())[ncol(data())]
    ggplot(data(), aes(x = .data[[last_column]], fill = .data[[last_column]])) +
      geom_bar() +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
      theme(legend.position = "none") +
      labs(title = "Frequency of each category", x = "Categorical variable", y = "Frequency")
  })

  output$pie_chart <- renderPlot({
    req(data())
    last_column <- names(data())[ncol(data())]
    ggplot(data(), aes(x = factor(1), fill = .data[[last_column]])) +
      geom_bar(width = 1) +
      coord_polar(theta = "y") +
      labs(title = "Pie Chart for the label", fill = last_column)
  })

  observeEvent(input$train_model, {
    req(data())
    if (!input$training_variable %in% colnames(data())) {
      showModal(modalDialog("La variable cible sélectionnée n'est pas valide.", title = "Erreur", easyClose = TRUE))
      return()
    }

    training_percentage <- input$training_percentage
    test_percentage <- input$test_percentage
    if (is.null(training_percentage) || is.null(test_percentage) || training_percentage + test_percentage != 100) {
      showModal(modalDialog("Veuillez spécifier des pourcentages valides pour l'ensemble d'entraînement et l'ensemble de test.", title = "Erreur", easyClose = TRUE))
      return()
    }

    showNotification("Entraînement des modèles en cours…", type = "message", duration = 5)

    training_rows <- round(training_percentage * nrow(data()) / 100)
    training_set(data()[1:training_rows, , drop = FALSE])
    test_set(data()[(training_rows + 1):nrow(data()), , drop = FALSE])

    fitted_models <- train_models(training_set(), test_set(), input$training_variable)

    trained_model(list(
      model = fitted_models$svm$model,
      metrics = fitted_models$svm$summary$metrics,
      precision = fitted_models$svm$summary$precision,
      recall = fitted_models$svm$summary$recall,
      f1_score = fitted_models$svm$summary$f1_score
    ))

    trained_model_RF(list(
      model = fitted_models$rf$model,
      metrics_RF = fitted_models$rf$summary$metrics,
      accuracy_RF = fitted_models$rf$summary$accuracy,
      precision_RF = fitted_models$rf$summary$precision,
      recall_RF = fitted_models$rf$summary$recall,
      F1_RF = fitted_models$rf$summary$f1_score
    ))

    logistic_model(list(
      model = fitted_models$logistic$model,
      metrics = fitted_models$logistic$summary$metrics,
      accuracy = fitted_models$logistic$summary$accuracy,
      precision = fitted_models$logistic$summary$precision,
      recall = fitted_models$logistic$summary$recall,
      F1 = fitted_models$logistic$summary$f1_score
    ))

    output$downloadSVM <- downloadHandler(filename = function() "svm_model.rds", content = function(file) saveRDS(fitted_models$svm$model, file))
    output$downloadRF <- downloadHandler(filename = function() "rf_model.rds", content = function(file) saveRDS(fitted_models$rf$model, file))
    output$downloadLR <- downloadHandler(filename = function() "LR_model.rds", content = function(file) saveRDS(fitted_models$logistic$model, file))
    showNotification("Entraînement terminé avec succès.", type = "message")
  })

  output$model_results <- renderPrint({
    req(trained_model())
    metrics <- trained_model()$metrics
    cat("Confusion Matrix:\n")
    print(metrics$table)
    cat("\nAccuracy:", metrics$overall['Accuracy'], "\n")
    cat("Precision:", trained_model()$precision, "\n")
    cat("Recall:", trained_model()$recall, "\n")
    cat("F1 Score:", trained_model()$f1_score, "\n")
  })

  output$model_results_RF <- renderPrint({
    req(trained_model_RF())
    metrics_RF <- trained_model_RF()$metrics_RF
    cat("Confusion Matrix:\n")
    print(metrics_RF$table)
    cat("\nAccuracy:", trained_model_RF()$accuracy_RF, "\n")
    cat("Precision:", trained_model_RF()$precision_RF, "\n")
    cat("Recall:", trained_model_RF()$recall_RF, "\n")
    cat("F1 Score:", trained_model_RF()$F1_RF, "\n")
  })

  output$model_results_LR <- renderPrint({
    req(logistic_model())
    metrics_logistic <- logistic_model()$metrics
    cat("Confusion Matrix:\n")
    print(metrics_logistic$table)
    cat("\nAccuracy:", logistic_model()$accuracy, "\n")
    cat("Precision:", logistic_model()$precision, "\n")
    cat("Recall:", logistic_model()$recall, "\n")
    cat("F1 Score:", logistic_model()$F1, "\n")
  })

  output$pdp_output <- renderUI({
    req(trained_model(), training_set())
    feature_names <- setdiff(names(training_set()), input$training_variable)
    plot_output_list <- lapply(seq_along(feature_names), function(i) {
      plotlyOutput(outputId = paste0("pdp_plot_", i))
    })
    do.call(tagList, plot_output_list)
  })

  observe({
    req(trained_model(), training_set())
    feature_names <- setdiff(names(training_set()), input$training_variable)
    lapply(seq_along(feature_names), function(i) {
      local({
        feature_name <- feature_names[i]
        output[[paste0("pdp_plot_", i)]] <- renderPlotly({
          req(trained_model(), training_set())
          pdp_feature <- partial(trained_model()$model, pred.var = feature_name, train = training_set())
          plotly::ggplotly(ggplot2::autoplot(pdp_feature))
        })
      })
    })
  })

  output$confusion_matrix_plot <- renderPlot({
    req(trained_model())
    plot_confusion_matrix(trained_model()$metrics$table, "Confusion Matrix")
  })

  output$confusion_matrix_plot_RF <- renderPlot({
    req(trained_model_RF())
    plot_confusion_matrix(trained_model_RF()$metrics_RF$table, "Confusion Matrix for Random Forest Model")
  })

  output$roc_auc_curve_plot_RF <- renderPlot({
    req(trained_model_RF(), test_set())
    test_probabilities_RF <- predict(trained_model_RF()$model, newdata = test_set(), type = "prob")[, 2]
    true_outcomes_RF <- as.numeric(test_set()[[input$training_variable]]) - 1
    roc_obj_RF <- roc(response = true_outcomes_RF, predictor = test_probabilities_RF)
    roc_plot_RF <- ggroc(roc_obj_RF, colour = "steelblue", size = 2) +
      geom_ribbon(aes(x = 1 - specificity, ymin = 0, ymax = sensitivity), fill = "steelblue", alpha = 0.2) +
      annotate("text", x = 0.6, y = 0.2, label = paste0("AUC = ", round(auc(roc_obj_RF), 2)), color = "red", size = 5) +
      ggtitle(paste0("ROC Curve for Random Forest Model (AUC = ", round(auc(roc_obj_RF), 2), ")")) +
      theme_minimal()
    print(roc_plot_RF)
  })

  output$roc_auc_curve_plot <- renderPlot({
    req(trained_model(), test_set())
    test_probabilities <- attr(predict(trained_model()$model, newdata = test_set(), probability = TRUE), "probabilities")[, 2]
    true_outcomes <- as.numeric(test_set()[[input$training_variable]]) - 1
    roc_obj <- roc(true_outcomes, test_probabilities)
    roc_df <- data.frame(sensitivity = roc_obj$sensitivities, specificity = roc_obj$specificities)
    roc_plot <- ggroc(roc_obj, colour = "steelblue", size = 2) +
      geom_ribbon(data = roc_df, aes(x = 1 - specificity, ymin = 0, ymax = sensitivity), fill = "steelblue", alpha = 0.2) +
      annotate("text", x = 0.6, y = 0.2, label = paste0("AUC = ", round(auc(roc_obj), 2)), color = "red", size = 5) +
      ggtitle(paste0("ROC Curve (AUC = ", round(auc(roc_obj), 2), ")")) +
      theme_minimal()
    print(roc_plot)
  })

  output$roc_auc_curve_plot_LR <- renderPlot({
    req(logistic_model(), test_set())
    test_probabilities_logistic <- predict(logistic_model()$model, newdata = test_set(), type = "response")
    true_outcomes_logistic <- as.numeric(test_set()[[input$training_variable]]) - 1
    roc_obj_logistic <- roc(response = true_outcomes_logistic, predictor = test_probabilities_logistic)
    roc_plot_logistic <- ggroc(roc_obj_logistic, colour = "steelblue", size = 2) +
      geom_ribbon(aes(x = 1 - specificity, ymin = 0, ymax = sensitivity), fill = "steelblue", alpha = 0.2) +
      annotate("text", x = 0.6, y = 0.2, label = paste0("AUC = ", round(auc(roc_obj_logistic), 2)), color = "red", size = 5) +
      ggtitle(paste0("ROC Curve for Logistic Regression Model (AUC = ", round(auc(roc_obj_logistic), 2), ")")) +
      theme_minimal()
    print(roc_plot_logistic)
  })

  output$confusion_matrix_plot_LR <- renderPlot({
    req(logistic_model())
    plot_confusion_matrix(logistic_model()$metrics$table, "Confusion Matrix for Logistic Regression Model")
  })

  output$feature_importance_plot <- renderPlot({
    req(trained_model_RF())
    rf_model <- trained_model_RF()$model
    importances <- importance(rf_model)
    importance_df <- data.frame(feature = rownames(importances), importance = importances[, 1], stringsAsFactors = FALSE)
    importance_df <- importance_df[order(importance_df$importance, decreasing = TRUE), ]
    ggplot(importance_df, aes(x = reorder(feature, importance), y = importance)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = "Feature Importances", x = "Features", y = "Importance") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$feature_importance_plot_LR <- renderPlot({
    req(logistic_model())
    coefficients <- exp(coef(logistic_model()$model))
    coef_df <- data.frame(Feature = names(coefficients), OddsRatio = coefficients)
    ggplot(coef_df, aes(x = reorder(Feature, OddsRatio), y = OddsRatio)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = "Feature Odds Ratios", x = "Features", y = "Odds Ratio") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
}
