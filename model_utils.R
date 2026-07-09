build_confusion_summary <- function(model, truth) {
  cm <- confusionMatrix(model, truth)
  list(
    metrics = cm,
    accuracy = cm$overall["Accuracy"],
    precision = cm$byClass["Precision"],
    recall = cm$byClass["Recall"],
    f1_score = cm$byClass["F1"]
  )
}

plot_confusion_matrix <- function(confusion_matrix, title) {
  confusion_df <- as.data.frame(confusion_matrix)
  colnames(confusion_df) <- c("Prediction", "Reference", "Freq")
  ggplot(confusion_df, aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%d\n(%.1f%%)", Freq, Freq / sum(Freq) * 100)), vjust = 1) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(title = title, x = "Actual", y = "Predicted") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

train_models <- function(train_data, test_data, target_variable) {
  formula_str <- as.formula(paste(target_variable, "~ ."))

  svm_model <- svm(formula_str, data = train_data, probability = TRUE)
  svm_pred <- predict(svm_model, newdata = test_data, probability = TRUE)
  svm_summary <- build_confusion_summary(svm_pred, test_data[[target_variable]])

  rf_model <- randomForest(formula_str, data = train_data, ntree = 500)
  rf_pred <- predict(rf_model, newdata = test_data)
  rf_summary <- build_confusion_summary(rf_pred, test_data[[target_variable]])

  logistic_model_fit <- glm(formula_str, data = train_data, family = binomial())
  logistic_probabilities <- predict(logistic_model_fit, newdata = test_data, type = "response")
  logistic_predicted <- ifelse(logistic_probabilities > 0.5, 4, 2)
  logistic_summary <- build_confusion_summary(factor(logistic_predicted, levels = c(2, 4)), test_data[[target_variable]])

  list(
    svm = list(model = svm_model, summary = svm_summary),
    rf = list(model = rf_model, summary = rf_summary),
    logistic = list(model = logistic_model_fit, summary = logistic_summary)
  )
}
