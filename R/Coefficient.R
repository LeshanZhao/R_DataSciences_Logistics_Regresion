#' Estimate the coefficient vector β
#' @description {Estimate the coefficient vector β which includes the independent variables/predictors plus the intercept}
#' @param X An \eqn{n \times p} \code{double} value of the matrix containing the values of the predictors(Not including intercept).
#' @param Y A factor vector indicating the category of response
#' @param method The method to be used by optim() function include "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent". The default value is "BFGS"
#' @return The optimal coefficient vector β
#' @author Fangjian Yang
#' @export
#'

Beta.hat <-  function(X,Y,method="BFGS"){
  Beta <- Beta.init(X,Y)
  Xi=X.format(X,intercept = T)
  Y=as.numeric(Y)-1
  Beta.hat <- optim(Beta,loss_func,y=Y,x=Xi,method=method)$par
  return(Beta.hat)
}
