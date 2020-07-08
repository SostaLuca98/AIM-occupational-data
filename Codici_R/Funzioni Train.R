# Title     : Funzioni per Train and Test
# Objective : Sperare che sta diavoleria funzioni
# Created by: david
# Created on: 15/06/2020

# id: model id
# object: regsubsets object
# data: data used to fit regsubsets
# outcome: outcome variable
get_model_formula <- function(id, object, outcome){
  # get models data
  models <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  for (i in 1:length(predictors)) {
    if (predictors[i]=='Erasmus1') {predictors[i]='Erasmus'}
    if (predictors[i]=='Stage1') {predictors[i]='Stage'}
}
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}


get_cv_error <- function(model.formula, data, numfold){
  set.seed(1)
  train.control <- trainControl(method = "cv", number = numfold)
  cv <- train(model.formula, data = data, method = "lm",
              trControl = train.control)
  cv$results$RMSE
}
