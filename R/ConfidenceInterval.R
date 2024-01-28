#' Calculate confidence intervals by bootstrap
#' @description {Bootstrap Confidence intervals: the user must be able to choose (i) the significance level α  to obtain for the 1−α  confidence intervals for β , and (ii) the number of bootstraps which by default will be 20.}
#' @param X An \eqn{n \times p} \code{double} value of the matrix containing the values of the predictors(Not including intercept).
#' @param Y A factor vector indicating the category of response
#' @param alpha A variable that indicating the significance level
#' @param B A variable vector indicating the number of bootstraps(Default 20)
#' @return Bootstrap Confidence intervals
#' @author Fangjian Yang
#' @export
#'
#'
boot.confi <- function(X,Y,alpha,B=20){
  X=X.format(X,intercept = F)
  n=nrow(X)
  boot_mat <- matrix(data = NA,nrow = B,ncol = ncol(X)+1)
  colnames(boot_mat) <- c("intercept",colnames(X))
  for(i in 1:B){
    resample <- sample(1:n, replace = TRUE)
    boot_mat[i,]  <- Beta.hat(X[resample,],Y[resample])
  }
  confi.interval <- apply(boot_mat,2,quantile,probs=c(alpha/2,1-alpha/2))
  return(confi.interval)
}
