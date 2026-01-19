#' Generation of Global Objects
#'
#'  \description{
#' This function builds the global objects required to run 'Stage2_rover_Perserverence()'}
#'
#' @param PARAMS_CFG, a tibble with information about each parameter space
#' @param Z, the set of instrument variables (optional, only required for the IV estimator)
#' @param X, the set of exogenous variables (optional)
#' @param SPVALUE, the critical value of the test statistics
#'
#' @returns global objects
#' @export
#'
#' @examples
generate_global<- function(PARAMS_CFG, SPVALUE, ...){

  NPARAMS <<- NROW(filter(PARAMS_CFG, N_TICKS>0))
  NSIPARAMS <<- 0 # will be updated if X exists
  NFIXPARAMS <<- NROW(filter(PARAMS_CFG, N_TICKS==0)) # fix parameters
  KZ <<- NEQS
  if (exists('Z', mode='numeric')) {
    KZ <<- NCOL(Z)
    Ztrans <<- t(Z)
  }
  
  # X may not exist
  if (exists('X', mode='numeric')) {
    I_T <<- diag(NROW(X))
    Xtrans <<- t(X)
    Mx<<- I_T - X %*% solve(t(X) %*% X) %*% t(X)
    KX <<- NCOL(X)
    KZMKX <<- KZ - KX
    SdF <<- (NCOL(Z)-NCOL(X))
    NSIPARAMS <- NCOL(X) # constants
  } else {
    Mx <<- Times
    KX <<- 0
    KZMKX <<- KZ - KX
    SdF <<- NEQS*KZ
  }
  

  # For Qll computation
  ones_T <<- rep(1, times=Times)
  ones_Ttrans <<- t(ones_T)
  M_ones_T <<-diag(Times)-ones_T%*%solve((ones_Ttrans %*% ones_T)) %*% ones_Ttrans

  r <<- 1-(10/Times)


  SCRITICAL <<- qchisq(SPVALUE, df=SdF)
  
  if (exists('NCORES', mode='numeric')) {
    message(paste0("Will use ", NCORES, " cores for parallel processing"))
  } else {
    warning("No NCORES varialbe found. Using single core for processing. Did you forget to define NCORES?")
    NCORES = 1
  }

}
