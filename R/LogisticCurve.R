#' Plot of the fitted logistic curve to the actual values
#' @description {This function plots the input X and Y through the ggplot function for logistic regression curves, and if the input dataset contains multiple predictors, the logistic curve is plotted separately for each predictor and all images are combined into a list data.}
#' @param X  X An \eqn{n \times p} \code{double} value of the matrix containing the values of the predictors.
#' @param Y  A factor vector indicating the category of response
#' @return The list of all logistic curve of predictor
#' @author Fangjian Yang
#' @export
#'

logistic_plot <- function(X,Y){
  library(ggplot2)
  n=dim(data.frame(X))[2]
  if(n==1)
  {
    plot=data.frame(X=X,response=as.factor(Y))
  }else{
    plot=data.frame(X,response=as.factor(Y))
  }
  plot.list <- list()
  var.list <- colnames(plot)
  for(i in 1:n)
  {
    temp.data <- data.frame(x=plot[,i],y=as.numeric(plot$response)-1)
    gp <- ggplot(temp.data, aes(x=x, y=y)) +
      geom_point(alpha=.5) +
      labs(x=var.list[i],y = "Response")+
      stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial),col="red", lty=2)
    plot.list[i] <- list(gp)
  }
  return(plot.list)
}
