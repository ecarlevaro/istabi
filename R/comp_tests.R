#' Comp Tests
#'
#'  \description{This function computes the test statistics}
#'
#'  \usage{function(theta0, VffLags=NULL) is used to get the test statistics}
#' @param theta0, the parameters of interest
#' @param z, a vector of size T with the values of the instruments (Z) for each observation. It is used to compute the moment equations.
#' @param VffLags, amount of lags
#'
#' @returns the parameter test statistics
#' @export
#'
#' @examples
comp_tests <- function(theta0, typeVarF='const', VffLags=NULL, HACprewhite=FALSE, zVffLags=NULL) { tryCatch({
  
  c2_hat= NA
  
  if (KX>0) {
    # IV estimator - IV moments
    b <- GET_B(theta0)
    Yb = Y %*% b
    
    uHat <- (Mx %*% Yb) %>%
      as.vector()
    fT = cbind(Z * uHat[1:T])
  } else {
    # Generic moments
    # THETA0 <- Grid[1,]
    fT <- GET_MOM_VALUES(theta0, z) # a T x G (=NEQS) matrix
  }
  
  T = NROW(fT)
  FT = apply(X=as.matrix(fT), MARGIN=2, FUN=sum)

  #	Variance of f_t
  if (typeVarF == 'HAC')
  {
    Vff = lm(fT ~ 1) %>%
      sandwich::vcovHAC(., sandwich=FALSE, adjust=TRUE)
  } else if (typeVarF == 'HC3' || typeVarF=='const')
  {
    Vff <- lm(fT ~ 1) %>% 
      sandwich::vcovHC(., type=typeVarF, sandwich=FALSE)
  } else if (typeVarF == 'NeweyWest')
  {
    Vff <- lm(fT ~ 1) %>%
      # Matlab code uses lag=3. If you use lag=NULL it is automatically selected. Alternatively use kernHAC().
      sandwich::NeweyWest(., lag=VffLags, prewhite=HACprewhite, sandwich = FALSE)
  } else {
    stop('typeVarF not recognized')
  }
  
  if (KX >0){
    # Two-stage estimator
    # only necessary if X is not-null (i.e, if there is a nuisance parameter c)
    H = Z %*% solve(Vff) %*% Ztrans
    c2_hat = solve( Xtrans %*%H%*% X) %*% Xtrans %*%H%*% Yb
    uHat = (Yb - X%*%c2_hat) %>% as.vector()
    
    fT = cbind(Z1 * uHat[1:T], Z2*uHat[(T+1):(2*T)]) # T x K
    FT = apply(X=fT, MARGIN=2, FUN=sum) # K x 1
    Vff <- lm(fT ~ 1) %>%
      sandwich::NeweyWest(., lag=VffLags, prewhite=HACprewhite, sandwich = FALSE)
    # Impose the restrictions (if any)
    Vff <- RS * Vff  # elemet-by-element multiplication
    
    VffInvSqrt = mtx_inv_sqrt(Vff)
    A = VffInvSqrt %*% ((1/T)*(Ztrans %*% X))
    Atrans = (1/T)* (Xtrans %*% Z %*% t(VffInvSqrt))
    
    ### S-stat with a nuisance parameter (X neq empty) ----
    M_VGamma = diag(KZ) - A %*% solve(Atrans%*%A) %*% Atrans # symmetric matrix
    # Compact SVD: L are the left-hand vectors associated with non-null singular values. These vectors span the matrix M_VGamma
    L = svd(M_VGamma)$u[ , (1:KZMKX)]
    
    theS = (1/T) * ((t(Yb)%*% (Z%*%VffInvSqrt %*% L)) %*% (t(L) %*% (VffInvSqrt%*%Ztrans %*% Yb)))
    
  } else {
  
    # The 2-step estimator is not needed since theta0 is fixed under the null, not nuisance parameter here)
    #VffInvSqrt = mtx_inv_sqrt(Vff)
    theS = (1/T) * t(FT) %*% solve(Vff) %*% FT
    
    if (is_scalar_vector(Vff)) {
      VffInvSqrt = (1/sqrt(Vff))
    } else {
      VffInvSqrt = mtx_inv_sqrt(Vff)
    }
  }

  ### qLL-Stab (qllStilde) ----
  # Magnusson Mavroeidis 2014, Appendix p17
  # Standarside moments (F hat in the Appendix)
  Fstd_hat = fT %*% VffInvSqrt
  DeltafT  <- rbind(Fstd_hat[1, ],
                    base::diff(Fstd_hat, differences=1))
  r_T = cumprod(r * rep(1, times=T)) # (T x kz)
  RDel <- c(1, r_T[1:(T-1)]) %>%
    toeplitz(.) %>%
    {. * (lower.tri(., diag = TRUE))}
  H_hat = RDel %*% DeltafT # T x kz
  r_Ttrans = t(r_T)
  M_rT <- diag(T) - r_T %*% solve(r_Ttrans %*% r_T ) %*% r_Ttrans
  G_hat = M_rT %*% H_hat

  TSSR_Ghat = G_hat^2 %>%
    apply( ., MARGIN=2, FUN=sum) %>%
    sum(.)

  N_hat = M_ones_T %*% Fstd_hat
  TSSR_Nhat = N_hat^2 %>%
    apply( ., MARGIN=2, FUN=sum) %>%
    sum(.)

  return(c('S' = theS,
    'qLLStab' = TSSR_Nhat - r * TSSR_Ghat,
    'c2_hat' = c2_hat))},
  error = function(msg){ return(c('S' = NA,
                                  'qLLStab' = NA,
                                  'c2_hat' = NA))})
}

