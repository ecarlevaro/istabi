test_that("Rover works", {
  ##DATA <-readRDS("C:/Users/User/Box/iStabi/Examples/example_NKPC/sim_MM2014/sim_NKPC_sams_2K.rds")[[1]]%>%
# rename('y_t' = 'y', 'x_t' = 'x') %>%
 # mutate('t' = 1:NROW(.), .before=y_t)


  DATA <-readRDS("C:/Users/a1766132/Box/iStabi/Examples/example_NKPC/sim_MM2014/sim_NKPC_sams_2K.rds")[[1]]%>%
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

  verify_global(PARAMS_CFG, Y, Z)

  generate_global(PARAMS_CFG,  Z, X, SPVALUE)





  #DT::datatable(descStats,
  #              options=list('scrollX'=TRUE))
 # fileName = here('2_0_objs_input_2param2k_(1).rds')
 # if (file.exists(fileName)) {
 #   stop(paste("The file ", fileName, "already exists! I don't overwrite it"))
 # } else {
  #  saveRDS(list('Y'=Y, 'Z'=Z, 'X'=X, 'Y'=Y, 'X'=X, 'Z'=Z, 'Z'=Z,
   #              'Xtrans'=Xtrans, 'Mx'=Mx, 'Ztrans'=Ztrans, 'R'=R,
  #               'T'=T, 'KX'=KX, 'KZ'=KZ, 'KZMKX'=KZMKX, 'ones_T'=ones_T, 'ones_Ttrans'=ones_Ttrans, 'M_ones_T'=M_ones_T, 'I_T'=I_T, 'r'=r,
   #              'PARAMS_CFG'=PARAMS_CFG, 'MODELEQS'=MODELEQS,
   #              'GET_B'=GET_B,
   #              'SCRITICAL'=SCRITICAL, 'SdF'=SdF, 'SPVALUE'=SPVALUE, 'NPARAMS'=NPARAMS, 'NEQS'=NEQS),
          #  fileName)
#
 # }

  shell.exec(here())

  SCRITICAL <- qchisq(SPVALUE, df=SdF)

  log_print(c('SCRITICAL: ', SCRITICAL))
  log_print(c('# of instruments', NCOL(Z)))
  log_print(c('# of identified params', NCOL(X)))
  log_print(c('S dg. of freedom: ', SdF))


  R <-    rbind(
    cbind(matrix(1, nrow=ncol(Z), ncol=ncol(Z)))
  )
  print(PARAMS_CFG)
  print(R)

  Stage1IniVals <- expand.grid('beta' = PARAMS_CFG$iniVal['beta'],
                               'gamma' = PARAMS_CFG$iniVal['gamma'], use.names=FALSE)

  DT::datatable(Stage1IniVals,
                filter='top', options=list('scrollX'=TRUE)) %>%
    formatRound(1:(NPARAMS), 3)

  # for a global search you use the initial values that were there
  fstRes <- tibble('method' = 'OLS-Stage0',
                   'beta' = PARAMS_CFG$iniVal[['beta']],
                   'gamma' = PARAMS_CFG$iniVal[['gamma']],
                   'value' = 1,
                   'S critical' = SCRITICAL,
                   'convergence' = TRUE,
                   'tSeconds' = -1,
                   'counts.function' = -1,
                   'counts.gradient' = -1,
                   'message' = 'Initial value from the Stage0 by OLS')
  NPARAMS = 2
  minsS <- NA

  # verify values in the parameter space
  inParamSpace <- map2_dfc(select(fstRes, 2:(NPARAMS+1)), colnames(fstRes)[2:(NPARAMS+1)], function(aParam, paramName) {
    #paramName = 'phiPi'
    # aParam = fstRes$phiPi
    out <- as.logical((PARAMS_CFG$minVal[paramName] <= aParam)*(aParam <= PARAMS_CFG$maxVal[paramName]))
    names(out) = paste0(paramName, '_IN')

    out
  })
  colnames(inParamSpace) <- paste(colnames(fstRes)[2:(NPARAMS+1)], '_IN', sep='')
  fstRes <- bind_cols(fstRes, inParamSpace)

  fstRes %>%
    DT::datatable(filter='top',
                  options=list('scrollX'=TRUE)) %>%
    formatRound(2:(NPARAMS+2), 3)

  IniVals <- fstRes
  IniVals[, 2:(NPARAMS+1)] <- map2_dfr(fstRes[, 2:(NPARAMS+1)], PARAMS_CFG$decimalPlaces, ~round(.x, digits=.y))

  IniVals <- IniVals %>%
    # filter all rows in fstRes that
    filter(., value <= SCRITICAL) %>%
    # in the parameter space
    filter(., across(ends_with('_IN'), ~.x == TRUE))
  #select rows that are unique
  IniVals <- IniVals[!duplicated(IniVals[, 2:(NPARAMS+1)]), ]

  DT::datatable(IniVals,
                filter='top', options=list('scrollX'=TRUE)) %>%
    formatRound(2:(NPARAMS+2), 3)
  if(NROW(IniVals) == 0) { stop('No initial values found')}

  # theIniVal is used to build the Grid: the Grid is centred at theIniVal
  theIniVal <- IniVals %>%
    filter(., value == min(IniVals$value))
  print(theIniVal)


  PARAMS_CFG[, 'INIVAL_STAGE2'] <- NULL
  INIVAL_STAGE2_column <-  map2_dbl(as.vector(theIniVal[, 2:(NPARAMS+1)], mode='numeric'),
                                    PARAMS_CFG$decimalPlaces,
                                    ~round(.x, digits=.y)) %>%
    list('name' = PARAMS_CFG$name,
         'INIVAL_STAGE2' = .) %>%
    as_tibble()
  PARAMS_CFG <- left_join(PARAMS_CFG, INIVAL_STAGE2_column, by='name')


  # Fill missing values for fixed parameters
  PARAMS_CFG[which(is.na(PARAMS_CFG$INIVAL_STAGE2)), 'INIVAL_STAGE2'] <- PARAMS_CFG[which(is.na(PARAMS_CFG$INIVAL_STAGE2)), 'iniVal']

  minsS <- NULL
  fstRes <- NULL
  IniVals <- PARAMS_CFG$iniVal
  reqObjs <- c('sam',
               'Y','Z', 'X',
               'Y', 'X',  'Z', 'R',
               'Xtrans', 'Mx', 'Ztrans',
               'T', 'KX', 'KZ', 'KZMKX', 'ones_T', 'ones_Ttrans', 'M_ones_T', 'I_T', 'r',
               'PARAMS_CFG', 'MODELEQS',
               'GET_B',
               'SCRITICAL', 'SdF', 'SPVALUE', 'NPARAMS', 'NEQS',
               'IniVals', 'theIniVal')

  map_lgl(reqObjs, ~exists(.x), envir=environment()) %>%
    {if (prod(.) == FALSE) {
      log_print(reqObjs[ as.logical(TRUE- (.))])
      stop('Some required objects missings')}}

  NCORES = 6
  #

  if (LOCALSEARCH == 'TRUE') {
    LOCALSEARCH = TRUE
  } else {
    LOCALSEARCH = FALSE
  }
  UPDATE = TRUE # TRUE if you want to updated the Grid from the 'Explored_recovery' files in case of failur?
  STEPSPERCORE = 100

  NPARAMS = 2 # keep this as the number of columns in the Grid even if some parameters are fixed

  slurm_ntasks <- as.numeric(Sys.getenv("SLURM_NTASKS")) # Obtain environment variable SLURM_NTASKS
  print(paste0("Slurm_ntasks is: ", slurm_ntasks))
  if (is.numeric(slurm_ntasks)) {
    log_print("Assigning ncores from slurm_ntasks")
    NCORES = slurm_ntasks # if slurm_ntasks is numerical, then assign it to cores
  } else {
    log_print("Assigning ncores using detectCores()")
    NCORES = detectCores() # Figure out how many cores there are
  }
  # NCORES = 1

  reqObjs <- c('sam',
               'Y','Z', 'X',
               'Y', 'X',  'Z', 'R',
               'Xtrans', 'Mx', 'Ztrans',
               'T', 'KX', 'KZ', 'KZMKX', 'ones_T', 'ones_Ttrans', 'M_ones_T', 'I_T', 'r',
               'PARAMS_CFG', 'MODELEQS',
               'GET_B',
               'SCRITICAL', 'SdF', 'SPVALUE', 'NPARAMS', 'NEQS',
               'IniVals', 'theIniVal')
  map_lgl(reqObjs, ~exists(.x), envir=environment()) %>%
    {if (prod(.) == FALSE) {
      log_print(reqObjs[ as.logical(TRUE- (.))])
      stop('Some required objects missings')}}


  nGrid <- NROW(Grid)
  log_print(paste0("Grid size in memory: ", lobstr::obj_size(Grid), " bytes"))


  setNumericRounding(2) # get rid of funny problems when updating test values in the Grid

  PARAMS_CFG$GRID_MIN <- Grid[, .(minbeta=min(beta), mingamma=min(gamma))] %>%
    unlist
  PARAMS_CFG$GRID_MAX <- Grid[, .(maxbeta=max(beta), maxgamma=max(gamma))] %>%
    unlist
  print(select(PARAMS_CFG, name, GRID_MIN, GRID_MAX))
  print(paste0('Grid has, ', nGrid, ' rows.'))

  tibble('KZ'=KZ,
         'KZMKX'=KZMKX,
         'SPVALUE' = SPVALUE,
         'S_DEGREES_FREEDOM' = SdF,
         'SCRITICAL' = SCRITICAL) %>% print()

  tictoc::tic()

  NCORES<-6



  Explored <- Grid[!is.na(S)]
  iniVal <- PARAMS_CFG$INIVAL_STAGE2
  IniRows <-iniVal
  fileName = paste0('2_2_search_stage2_grid_2k.rds')

  #theta0 = PARAMS_CFG$iniVal

  expResult <- Stage2_rover_Perseverance(iniVal=PARAMS_CFG$iniVal, WTLags = 1, localSearch = FALSE)
  Explored <- Grid
  log_print(paste0("Explored has", NROW(Explored)))

})
