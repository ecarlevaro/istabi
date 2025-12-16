test_that("Build lines works", {
  DATA <-readRDS("C:/Users/User/Box/iStabi/Examples/example_NKPC/sim_MM2014/sim_NKPC_sams_2K.rds")[[1]]%>%
    rename('y_t' = 'y', 'x_t' = 'x') %>%
    mutate('t' = 1:NROW(.), .before=y_t)


  ZLAGS = 1

  sam <- map_dfc(1:ZLAGS, function(lagOrder) {
    DATA %>%
      mutate(across(y_t:x_t, .fns=dplyr::lag, .names="{.col}M{lagOrder}"))}) %>%
    mutate('y_tP1' = dplyr::lead(y_t),
           'I_x_tM1' = if_else( t<(NROW(DATA)/2), 0, x_tM1)) %>%
    .[-c(1, NROW(.)), ]

  #build objects
  NEQS = 1
  # Yb
  # Get the vector b
  get_b <- function(theta0) {
    c(1,
      -theta0['beta'],
      -theta0['gamma'])
  }

  Y <- cbind( 'y_t' = sam$y_t, 'y_tP1' = sam$y_tP1, 'x_t' = sam$x_t)

  X <- rep(1, times=NROW(Y))
  NEQS = 1

  #instruments
  Z <- select(sam,'x_tM1'= x_tM1, 'I_x_tM1' = I_x_tM1) %>%
    as.matrix(.)
  Ztrans <- t(Z)

  ### Restrictions on Vff
  R <-    rbind(
    cbind(matrix(1, nrow=ncol(Z), ncol=ncol(Z)))
  )

  NPARAMS<-2
  T= 1998
  GET_B = get_b


  MODELEQS <- list(list('Y' = Y,  'get_b'=NULL, 'X'=X, 'Z'=Z))

  PARAMS_CFG <- tribble(~name, ~iniVal, ~stepSize, ~minVal, ~maxVal, ~decimalPlaces,
                        'beta',  NA, 0.025, 0, 1, 1,
                        'gamma', NA, 0.025, 0, 1, 1)  %>%
    mutate('N_TICKS' = ((maxVal-minVal)/stepSize)+1)
  # True values are beta = 0.8
  # gamma = 0.2
  # see file:///C:/Users/a1766132/Box/iStabi/Examples/example_NKPC/sim_MM2014/sim_NKPC.html
  # replace NaN in N_TICKS by 0
  PARAMS_CFG$N_TICKS <- ifelse(is.na(PARAMS_CFG$N_TICKS), 0, PARAMS_CFG$N_TICKS)
  PARAMS_CFG$N_TICKS[PARAMS_CFG$N_TICKS == Inf] <- 0

  # Confidence level for the S test. It guides the search. For robustness to local minimum, you want a high value here  (a high S critical value since it determines the cutoff in tne numerical search)
  SPVALUE = .95
  # Size of adjacent cells to explores is NCORES * STEPSPERCORE
  names(PARAMS_CFG$iniVal) <- PARAMS_CFG$name # these names are used by GET_B()
  names(PARAMS_CFG$minVal) <- PARAMS_CFG$name # these names are used by GET_B()
  names(PARAMS_CFG$maxVal) <- PARAMS_CFG$name # these names are used by GET_B()


  # Initial value
  PARAMS_CFG$iniVal <- as.data.frame(Y) %>%
    lm(y_t ~ 1 + y_tP1 + x_t, data=.) %>%
    .$coefficients %>%
    {c('beta' = .[['y_tP1']],
       'gamma' = .[['x_t']])}

  PARAMS_CFG[(PARAMS_CFG$name == 'beta'), 'iniVal'] <- 0.6
  PARAMS_CFG[(PARAMS_CFG$name == 'gamma'), 'iniVal'] <- 0.6

  PARAMS_CFG


  # Build the lines

  thisGridLines <- build_lines(PARAMS_CFG, originsColName='iniVal')
  psUnderNull <-1
  Grid <- build_grid(PARAMS_CFG, thisGridLines, NEQS, psUnderNull)
  ## Desct stats from the MATRIX
  descStats <- as_tibble(cbind(Y, Z))%>%
    map_dfr(., function(thisVar) {
      c('mean'=mean(thisVar), 'sd'=sd(thisVar), 'min'=min(thisVar), 'max'=max(thisVar), 'N'=length(thisVar)/NEQS) %>%
        round(., digits=4)
    }) %>%
    mutate(., 'Variable' = c(colnames(Y), colnames(Z)),
           .before=mean)

  verify_global(PARAMS_CFG, Y, Z, Times=T, NEQS)

  generate_global(PARAMS_CFG,  Z, X, SPVALUE, Times=T)





  comp_tests(THETA0, VffLags=NULL)
})
