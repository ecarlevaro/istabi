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
generate_global<- function(SPVALUE, ...){
  
  PARAMS_CFG <<- dplyr::mutate(PARAMS_CFG, N_TICKS = ((maxVal-minVal)/stepSize)+1)
  NPARAMS <<- NROW(dplyr::filter(PARAMS_CFG, N_TICKS > 0))
  BIGT <<- NROW(Y)
  
  verify_global(PARAMS_CFG, Y)
  
  NSIPARAMS <<- 0 # will be updated if X exists
  NFIXPARAMS <<- NROW(dplyr::filter(PARAMS_CFG, N_TICKS==0)) # fix parameters
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
    Mx <<- BIGT
    KX <<- 0
    KZMKX <<- KZ - KX
    SdF <<- NEQS*KZ
  }
  

  # For Qll computation
  ones_T <<- rep(1, times=BIGT)
  ones_Ttrans <<- t(ones_T)
  M_ones_T <<-diag(BIGT)-ones_T%*%solve((ones_Ttrans %*% ones_T)) %*% ones_Ttrans

  r <<- 1-(10/BIGT)


  SCRITICAL <<- qchisq(SPVALUE, df=SdF)
  
  if (exists('NCORES', mode='numeric')) {
    message(paste0("Will use ", NCORES, " cores for parallel processing"))
  } else {
    warning("No NCORES varialbe found. Using single core for processing. Did you forget to define NCORES?")
    NCORES = 1
  }
  
  if (exists('STEPSPERCORE', mode='numeric')) {
    message(paste0("Will use ", STEPSPERCORE, " steps per core in the local search algorithm."))
  } else {
    STEPSPERCORE = 200 
  }
  
  gridLines <<- build_lines(PARAMS_CFG, 'iniVal')
  print(gridLines)
  Grid <<- build_grid(PARAMS_CFG, gridLines)

}
