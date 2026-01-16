#' Verify Global Objects
#'  \description{
#' This function verifies the global objects produced by 'generate_global()'}
#'
#' @param PARAMS_CFG, parameters
#' @param Y, the set of outcomes variables
#' @param Z, the set of instruments
#'
#' @returns a verification all objects are valid
#' @export
#'
#' @examples
verify_global <- function(PARAMS_CFG, Y, ...) 
{
  T <- NROW(Y)
  NPARAMS = NROW(filter(PARAMS_CFG, N_TICKS > 0))
  if (!exists("NEQS", mode = "numeric")) {
    stop("How many equations?")
  }
  if (!exists("Y", mode = "numeric")) {
    stop("Objects Y and X should exist before calling efisegri_ND_fcns.R")
  }
  if (exists("Z", mode = "numeric")) {
    if (!(NROW(Z) == NROW(Y))) {
      stop("The number of rows of X, Y and Z must coincide")
    }
    if (!(NROW(Z) == NEQS * T)) {
      stop("The number of rows of Z does not coincide with the number of moment equations and observations.")
    }
    KZ = NCOL(Z)
    if (KZ < NPARAMS) {
      stop("THere are less instruments than parameters. Order condition not satisfied")
    }
  }
  if (exists("X", mode = "numeric")) {
    if (!(NROW(X) == NROW(Y) & (NROW(Z) == NROW(X)))) {
      stop("Nomber of rows of X must coincide with Z and Y")
    }
  }
  theta0 = PARAMS_CFG$iniVal
  names(theta0) = PARAMS_CFG$name
  if (anyNA(GET_MOM_VALUES(theta0))) {
    stop(paste0("get_b(THETA0) returns NAs in comp_S().  Make sure your GET_B() fcn does what it meant. THETA0 is:", 
                PARAMS_CFG$iniVal))
  }
  if (!is.matrix(GET_MOM_VALUES(theta0))) {
    stop("GET_MOM_VALUES() function should return a matrix. Maybe add as.matrix() to the output?")
  }
  walk(MODELEQS, function(thisEq) {
    if (!exists("Y", mode = "numeric", where = thisEq)) {
      stop("Objects Y should exist before calling efisegri_ND_fcns.R")
    }
    if (exists("Z", mode = "numeric", where = thisEq)) {
      if (!(NROW(thisEq$Z) == NROW(thisEq$Y))) {
        stop("The number of rows of X, Y and Z must coincide")
      }
      if (!(NROW(thisEq$Z) == T)) {
        stop("The number of rows of Z does not coincide with the number of moment equations and observations.")
      }
    }
    if (exists("X", mode = "numeric", where = thisEq)) {
      if (!(NROW(thisEq$X) == NROW(thisEq$Y))) {
        stop("Nomber of rows of X must coincide with Z and Y")
      }
      if (exists("Z", mode = "numeric", where = thisEq)) { 
        if (!(NROW(thisEq$Z) ==  NROW(thisEq$X))) {
          stop("Nomber of rows of X must coincide with Z and Y")
        }
      }
    }
  })
}
