#' Calculate the confusion matrix and required metrics.
#' @description {This function calculates the confusion matrix by comparing the predicted values with the true values and calculates the following six metrics: 1.Prevalence, 2.Accuracy, 3.Sensitivity, 4.Specificity, 5.False.Discovery.Rate, 6.Diagnostic.Odds.Ratio. The size of the cut-off value may affect the output results and the performance of the model. Therefore, the cut-off value of this function is user-definable, and its default value is 0.5. }
#' @param pred.value  The predicted value of fitted logistic model
#' @param actual.value  The actual value of the data set
#' @param cutoff The cut-off values for measuring metrics.
#' @return The confusion matrix and six metrics
#' @author Leshan Zhao
#' @export
#'

confusion.matrix <- function(pred.value, actual.value, cutoff=0.5){
  Confusion <- data.frame(pred=pred.value, actual=actual.value)
  Confusion[Confusion >= cutoff]=1
  Confusion[Confusion < cutoff]=0
  Confusion[,c(1,2)]=lapply(Confusion[,c(1,2)], as.factor)

  N = nrow(Confusion)
  TP = nrow(Confusion[Confusion$pred=="1" & Confusion$actual=="1", ])
  TN = nrow(Confusion[Confusion$pred=="0" & Confusion$actual=="0", ])
  FP = nrow(Confusion[Confusion$pred=="1" & Confusion$actual=="0", ])
  FN = nrow(Confusion[Confusion$pred=="0" & Confusion$actual=="1", ])

  Accuracy <- (TP+TN)/N
  Prevalence <- (TP+FN)/N
  Sensitivity <- TP/(TP+FN)
  Specificity <- TN/(TN+FP)
  False.Discovery.Rate <- FP/(TP+FP)
  Diagnostic.Odds.Ratio <- (TP*TN)/(FN*FP)

  metrics <- c(Prevalence,Accuracy,Sensitivity,Specificity,False.Discovery.Rate,Diagnostic.Odds.Ratio)
  names(metrics) <-c("Prevalence",
                     "Accuracy",
                     "Sensitivity",
                     "Specificity",
                     "False.Discovery.Rate",
                     "Diagnostic.Odds.Ratio")

  matrix <- c(TP,FP,FN,TN)
  names(matrix) <- c("TP","FP","FN","TN")

  output.list <- list("metrics" = metrics,
                      "matrix" = matrix)
  return(output.list)
}

#' Calculate the predicted value.
#' @description {Input the parameters of the logistic regression model and predictor variables to obtain the predicted values.}
#' @param model The parameters of the logistic regression model.
#' @param X  X An \eqn{n \times p} \code{double} value of the matrix containing the values of the predictors.
#' @return The confusion matrix and six metrics
#' @author Fangjian Yang
#' @export
#'
logistic_pred <- function(model,X){
  Xi=X.format(X,intercept = T)
  predic <- 1/(1+exp(-Xi%*%model))
  return(predic)
}