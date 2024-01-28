#' Calculate the six metrics of the model with different cut-off values
#' @description {This function allows the user to plot the evaluation of the six metrics on a grid of predicted cutoff values from 0.1 to 0.9, in steps of 0.1.}
#' @param X  X An \eqn{n \times p} \code{double} value of the matrix containing the values of the predictors.
#' @param Y  A factor vector indicating the category of response
#' @param interval The range of cut-off value (Default value is from 0.1 to 0.9).
#' @param step  Step length of the interval of cut-off values(Default value is 0.1).
#' @return The dataframe of six metrics with different cutoff value
#' @author Leshan Zhao
#' @export
#'

metrics.table <- function(X, Y, interval=c(0.1,0.9), step=0.1){
  library(ggplot2)
  model = Beta.hat(X, Y)
  predict <- logistic_pred(model, X)
  actual.value <- as.numeric(Y)-1
  cutoff.list <- seq(interval[1],interval[2],step)
  n = length(cutoff.list)
  data <- matrix(NA,nrow=n,ncol=6)
  for(i in 1:n){
    data[i, ] <- confusion.matrix(predict,
                                  actual.value,
                                  cutoff=cutoff.list[i])$metrics
  }

  data <- cbind(cutoff.list,data)
  colnames(data) <-c("Cut-off value",
                     "Prevalence",
                     "Accuracy",
                     "Sensitivity",
                     "Specificity",
                     "False.Discovery.Rate",
                     "Diagnostic.Odds.Ratio")
  return(data)
}



