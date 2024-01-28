#' Uniform data format for predictor variables
#' @description This function converts the input predictor variables into matrix format and adds intercepts variable accord by user
#' @param X An \eqn{n \times p} \code{double} value of the matrix containing the values of the predictors.
#' @param intercept A logical variable indicating whether to add an intercept to the predictor variables
#' @return A matrix containing the predictor variables
#' @author Fangjian Yang
#' @export
#'
X.format <- function(X,intercept=F)
{
  X=as.matrix(X)
  if(dim(X)[2]==1){colnames(X) <- "Predictor"}
  if(intercept){
    intercept <- rep(1,nrow(X))
    X <- as.matrix(cbind(intercept,X))
  }
  return(X)
}

#' Calculate the initial beta value
#' @description {Initial values for optimization obtained from the least-squares formula \deqn{(X^{T}X)^{−1}X^{T}Y}\\}
#' @param X An \eqn{n \times p} \code{double} value of the matrix containing the values of the predictors(Not including intercept).
#' @param Y A factor vector indicating the category of response
#' @return The initial beta vector for loss function
#' @author Fangjian Yang
#' @export
#'
Beta.init <- function(X,Y){
  Xi=X.format(X,intercept = T)
  Y=as.numeric(Y)-1
  Beta <- solve(t(Xi)%*%Xi)%*%t(Xi)%*%Y
  return(Beta)
}


#' Calculate the Pi vector for loss function
#' @description {the Pi vector for loss function from the formula \deqn{P_{i}=\frac{1}{1+e^{-x^{T}_{i}\beta}}}\\}
#' @param beta  The current beta vector(Including intercept).
#' @param Xi An \eqn{n \times p} \code{double} value of the matrix containing the values of the predictors(Including intercept).
#' @return The Pi value for loss function
#' @author Fangjian Yang
#' @export
#'
P.i <- function(beta,Xi){
  X=X.format(Xi,intercept = F)
  output <- rep(NA,nrow(X))
  for(i in 1:nrow(X)){
    output[i] <- 1/(1+exp(-t(X[i,])%*%beta))
  }
  return(output)
}

#' The numerical optimization loss function
#' @description {The estimator to be computed using numerical optimization is the following: \deqn{\hat\beta=\underset{\beta}{argmin}\sum_{i=1}^{n}（-y_{i}\cdot ln(p_{i})-(1-y_{i} \cdot ln(1-p_{i}))）}\\}
#' @param beta  The current beta vector(Including intercept).
#' @param y  A training factor vector indicating the category of response
#' @param xi An \eqn{n \times p} \code{double} value of the matrix containing the values of the predictors(Including intercept).
#' @return The estimator of loss function
#' @author Fangjian Yang
#' @export
#'
loss_func <- function(beta,y,xi){
  p <- P.i(beta,xi)
  temp <- -y*log(p)-(1-y)*log(1-p)
  return(sum(temp))
}

#' Summary of the output of the logistic regression function
#' @description {Inputting the data set into this function can fit a logistic regression, and can get the following output: 1. estimated logistic regression model; 2. confidence intervals for the parameters in the model; 3. logistic regression curves for each predictor; 4. confusion matrix; 5. output for the six metrics; and 6. metrics with different cutoff values.}
#' @param X  X An \eqn{n \times p} \code{double} value of the matrix containing the values of the predictors.
#' @param Y  A factor vector indicating the category of response
#' @param method The method to be used by optim() function include "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent". The default value is "BFGS"
#' @param cutoff The cut-off value for measuring metrics.
#' @param alpha A variable that indicating the significance level
#' @param B A variable indicating the number of bootstraps(Default 20)
#' @return The list of all output
#' @author Fangjian Yang
#' @export
#'


logistic.regression <- function(X, Y, method="BFGS", cutoff=0.5, alpha=0.1,B=20){
  beta.initial <- Beta.init(X, Y)
  model <- Beta.hat(X, Y,method)
  CI <- boot.confi(X, Y, alpha,B=20)
  predict <- logistic_pred(model, X)
  actual.value <- as.numeric(Y)-1
  Analysis <- confusion.matrix(predict, actual.value, cutoff=cutoff)
  p <- logistic_plot(X,Y)
  Yi <- as.numeric(Y)-1
  level <- as.character(unique(Y))
  names(level) <- unique(Yi)
  matrix <- matrix(Analysis$matrix,nrow=2,ncol=2)
  rownames(matrix) <- c(paste("Actual.", level["1"],sep=""),
                        paste("Actual.", level["0"],sep=""))
  colnames(matrix) <- c(paste("Predicted.", level["1"],sep=""),
                        paste("Predicted.", level["0"],sep=""))

  beta.info <- data.frame(model,beta.initial, t(CI))
  colnames(beta.info) <- c("Beta.hat","Beta.initial",paste("CI:",alpha/2,"%",sep=""),paste("CI:",1-alpha/2,"%",sep=""))
  table <- metrics.table(X,Y)
  table.2 <- data.frame("cutoff"=NA,"value"=NA)[-1,]
  for(i in 2:7){
    temp <- table[,c(1,i)]
    colnames(temp) <- c("cutoff","value")
    table.2=rbind(table.2,temp)
  }
  table.2=cbind(table.2,data.frame(group=rep(colnames(table)[-1],each=9)))
  Cutoff.image <- ggplot(table.2,aes(x=cutoff,y=value,col=group))+geom_line(size=1)


  result.list <- list("Level"=level,"Beta"=beta.info,"Confusion.Matrix"=matrix,"Metrics"=Analysis$metrics,"Metrics Table"=table,"Cut-off image"=Cutoff.image,"Logistic Curve"=p)
  result.list
  return(result.list)
}

