#' Generation of Global Objects
#'
#'  \description{
#' This function builds the global objects required to run 'Stage2_rover_Perserverence()'}
#'
#' @param PARAMS_CFG, a tibble with information about each parameter space
#' @param Z, the set of instrument variables
#' @param X, the set endogenous variables
#' @param SPVALUE, the critical value of the test statistics
#'
#' @returns global objects
#' @export
#'
#' @examples
generate_global<- function(PARAMS_CFG,  Z, X, SPVALUE){

  KZ <<- NCOL(Z)
  I_T <<- diag(NROW(X))
  # X may not exist
  if (exists('X', mode='numeric')) {
    Xtrans <<- t(X)
    Mx<<- I_T - X %*% solve(t(X) %*% X) %*% t(X)
    KX <<- NCOL(X)
    KZMKX <<- KZ - KX
    SdF <<- (NCOL(Z)-NCOL(X))
  }else {
    Mx <<- I_T
    KX <<- 0
    KZMKX <<- KZ - KX
    Sdf <<- NCOL(Z)
  }
  Ztrans <<- t(Z)

  # For Qll computation
  ones_T <<- rep(1, times=Times)
  ones_Ttrans <<- t(ones_T)
  M_ones_T <<-diag(Times)-ones_T%*%solve((ones_Ttrans %*% ones_T)) %*% ones_Ttrans

  r <<- 1-(10/Times)


  SCRITICAL <<- qchisq(SPVALUE, df=SdF)



}
