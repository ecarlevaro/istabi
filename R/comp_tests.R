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
comp_tests <- function(theta0, VffLags=NULL) { tryCatch({
  c2_hat= NA
  T<-NROW(Y)
  b <- GET_B(theta0)
  Yb = Y %*% b

  uHat <- (Mx %*% Yb) %>%
    as.vector()
  fT = cbind(Z * uHat[1:T])
  FT = apply(X=fT, MARGIN=2, FUN=sum)

  Vff <- lm(fT ~ 1) %>%
    sandwich::NeweyWest(., lag=VffLags, prewhite=FALSE, sandwich = FALSE)

  Vff <- R * Vff

  if (KX >0){

    H = Z %*% solve(Vff) %*% Ztrans
    c2_hat = solve( Xtrans %*%H%*% X) %*% Xtrans %*%H%*% Yb
    uHat = (Yb - X%*%c2_hat) %>% as.vector()

    fT = cbind(Z * uHat[1:T]) # T x K
    FT = apply(X=fT, MARGIN=2, FUN=sum) # K x 1
    Vff <- lm(fT ~ 1) %>%
      sandwich::NeweyWest(., lag=VffLags, prewhite=FALSE, sandwich = FALSE)

    Vff <- R * Vff

    VffInvSqrt = mtx_inv_sqrt(Vff)
    A = VffInvSqrt %*% ((1/T)*(Ztrans %*% X))
    Atrans = (1/T)* (Xtrans %*% Z %*% t(VffInvSqrt))
  } else {

    VffInvSqrt = mtx_inv_sqrt(Vff)
    A = VffInvSqrt
    Atrans = t(A)
  }

  M_VGamma = diag(KZ) - A %*% solve(Atrans%*%A) %*% Atrans # symmetric matrix

  L = svd(M_VGamma)$u[ , (1:KZMKX)]

  theS = (1/T) * ((t(Yb)%*% (Z%*%VffInvSqrt %*% L)) %*% (t(L) %*% (VffInvSqrt%*%Ztrans %*% Yb)))


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

 # c( tryCatch({ 'S' = theS }, error = function(msg){ return(NA)}),
 # tryCatch({'qLLStab' = TSSR_Nhat - r * TSSR_Ghat }, error = function(msg){ return(NA)}),
 # tryCatch({'c2_hat' = c2_hat }, error = function(msg){ return(NA)}))
}

