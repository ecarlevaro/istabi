#' Comp Tests
#'
#'  \description{This function computes the test statistics}
#'
#'  \usage{function(theta0, VffLags=NULL) is used to get the test statistics}
#' @param theta0, the parameters of interest
#' @param VffLags, amount of lags
#'
#' @returns the parameter test statistics
#' @export
#'
#' @examples
mom_IV  <- function(theta0, z) {
  Yb = Y %*% b
  
  uHat <- (Mx %*% Yb) %>%
    as.vector()
  # return fT
  cbind(Z * uHat[1:T])
  
}
  