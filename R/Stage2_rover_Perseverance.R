#' Stage2_rover_Perseverance
#'
#'  \description{This function fills in the test statistics in the grid produced by 'build_grid().
#'* All these variables should be defined beforehand
#'* Y, Z, Ztrans: matrices with data
#'* T: integer, the number of observations
#'* get_b: a function which accepts the vector theta0 and outputs the value of the vector b(theta0) such that Y%\*%b is the moment equation.
#'* Requires access to global objects: STCPARAMNAMES, ESTVALUESNAMES, SCRITICAL, NPARAMS, GET_B, nextThetas}
#'
#'  \usage{Stage2_rover_Perseverance(iniVal, WTLags, localSearch = FALSE  )}
#'
#' @param iniVal, an initial value to start grid search over
#' @param WTLags, the amount of lags
#'@param localSearch, TRUE or FALSE depending if you want a global or local search respectively
#'
#'@returns the parameter value not rejected by null over the created grid
#' @export
#'
#' @examples


Stage2_rover_Perseverance <- function(iniVal, WTLags, localSearch = FALSE ) {
  # DEBUG
  # iniVal <- PARAMS_CFG$INIVAL_STAGE2
  # iniVal <- IniRows
  # WTLags = -1
  # localSearch = FALSE
  #browser()

  if (WTLags == -1) {
    thisVffLags <- NULL
  } else {
    thisVffLags <- WTLags
  }

  # Character vectors to select columns in the stupid Grid (which is a data.table)
  STCPARAMNAMES <- PARAMS_CFG$name
  # Column names with tests and nuisance parameters
  ESTVALUESNAMES <- c("S", "qLLStab",  "c2_hat")
  sS <- PARAMS_CFG$stepSize
  names(sS) = PARAMS_CFG$name

  #---------------------------------------------------------
  # COMPUTE nextRows, A VECTOR OF ROW NUMBERS TO EXPLORE IN THE GRID
  if (localSearch)
  {
    if (NROW(iniVal) == 1) {
      log_print(paste0(substr(Sys.time(), start=12, stop=13), ":",
                       substr(Sys.time(), start=15, stop=16),
                       ". Ini value is a 1-dim vector. Compute the space of vector local to this."))

      # Compute values in the neighbourhood of iniVal
      Nadj = NCORES*STEPSPERCORE
      StepsM = rbind( (1:Nadj) %x% diag(PARAMS_CFG$stepSize),
                      -(1:Nadj) %x% diag(PARAMS_CFG$stepSize))
      mNrows = NPARAMS*Nadj*2 # Size fo the m matrix below
      wantPrecisionMax = max(PARAMS_CFG$decimalPlaces)

      # Round iniVal to desired precision
      iniVal <- map2_dbl(iniVal, PARAMS_CFG$decimalPlaces, function(i, w) {
        round(i, digits=w)    })
      nextRows <- matrix(iniVal, nrow=mNrows, ncol=NPARAMS, byrow=TRUE,
                         dimnames=list(c(),
                                       PARAMS_CFG$name)) %>%
        {(.) + StepsM} %>%
        # Best way to convert a matrix to a list
        as_tibble() %>%
        # DT requires a list
        {Grid[as.list(.), , nomatch=NULL, which=TRUE]} %>%
        unique(.)

      log_print(paste0(substr(Sys.time(), start=12, stop=13), ":",
                       substr(Sys.time(), start=15, stop=16),
                       ". nextRows contains", NROW(nextRows), " rows"))


    }  else if (is.data.table(iniVal)) {

      log_print(paste0(substr(Sys.time(), start=12, stop=13), ":",
                       substr(Sys.time(), start=15, stop=16),
                       "iniVal is a data table with ", NROW(iniVal), " rows."))

      nextRows <- Grid[iniVal, nomatch=NULL, which=TRUE] %>%
        unique(.)
      rm(iniVal)

    } else if ( is.integer(iniVal) & NROW(iniVal)>1 ) {
      log_print(paste0(substr(Sys.time(), start=12, stop=13), ":",
                       substr(Sys.time(), start=15, stop=16),
                       " iniVal is a vector of row positions with ", NROW(iniVal), " rows."))
      nextRows <- IniRows

    } else {

      log_print(paste0(substr(Sys.time(), start=12, stop=13), ":",
                       substr(Sys.time(), start=15, stop=16),
                       " ERROR. iniVal has to be double or a data table"))

      stop("iniVal has to be double (a point) or a tibble with candidate points.")
    }
    if (!is.data.table(Grid)) { stop('Object Grid does not exist or it is not a data.table')}

  }   else
  {
    # A global search
    log_print(paste0(substr(Sys.time(), start=12, stop=13), ":",
                     substr(Sys.time(), start=15, stop=16),
                     ". Do a GLOBAL search. nextRows contains all row numbers in the Grid."))

    nextRows <- 1:NROW(Grid)

  }

  #---------------------------------------------------------
  # Start explorer
  log_print("Calling garbage collector before repeat to start exploration")
  gc() %>% log_print()

  repeat {
    log_print(paste0(substr(Sys.time(), start=12, stop=13), ":",
                     substr(Sys.time(), start=15, stop=16),
                     "(top of repeat loop). Starting an exploration"))
    # Store data on perfomance
    PerfExp <- tibble('time' = Sys.time(),
                      'length_minutes' = NA,
                      'npoints' = NA,
                      'points_per_minute' = NA)

    # Compute test values for each row in nextRows. For debugging purposes you DON'T want "future_"
    nRows = NROW(nextRows)
    # 6e5
    if (nRows > 6e5) {

      log_print(paste0(substr(Sys.time(), start=12, stop=13), ":",
                       substr(Sys.time(), start=15, stop=16),
                       "hs. There are ", (nRows) , " rows to explore. I split it in baches of 200,000 rows."))

      #width = 2e5
      width = 2e5
      nIter = floor((nRows) / width)
      pos <- cbind('start' =  (0:(nIter-1))*width + (0:(nIter-1) +1),
                   'end' = (0:(nIter-1) +1)*width + (0:(nIter-1) +1))
      if (pos[nIter, 'end'] < nRows) {
        pos <- rbind(pos,
                     c('start'= pos[nIter, 'end'] + 1, 'end'=  nRows))
      } else if(pos[nIter, 'end'] > nRows) {
        pos[nIter, 'end'] = nRows
      }
    } else {
      pos <- cbind('start' =  1,
                   'end' = nRows-1)
    }

    # ITERATION # : START : END
    # 1 : 1 : 3800967 - DONE
    # 2 : 3800968 : 2*3800967
    ###################################################################
    ############################# FOR LOOP ##########################
    for (i in 1:NROW(pos)) {
      # i = 1
      sptS = pos[i, 'start']
      sptE = pos[i, 'end']

      log_print(paste0(substr(Sys.time(), start=12, stop=13), ":",
                       substr(Sys.time(), start=15, stop=16),
                       "hs. (inside for loop). Exploring ", (sptE-sptS), " points. The first point is [",
                       paste(Grid[nextRows[sptS], 1:NPARAMS], collapse=', '),
                       "]. The last point is [",
                       paste(Grid[nextRows[sptE], 1:NPARAMS], collapse=', '),
                       "]."))


      testValues <- as.data.frame(t(future_apply(Grid[nextRows[sptS:sptE], 1:NPARAMS], 1,
                                                 comp_tests,
                                                 VffLags = thisVffLags,
                                                 future.seed = 7561234)))


     # testValues <- as.data.frame(t(future_apply(Grid[nextRows[sptS:sptE], 1:NPARAMS], 1,
      #                                           tryCatch(
        #                                           { comp_tests
       #                                            }, error = function(msg){
            #                                         return(NA)}),
             #                                   VffLags = thisVffLags,
              #                                   future.seed = 7561234)))



      #as.data.frame(t(future_apply(Grid[nextRows[sptS:sptE], 1:NPARAMS], 1, comp_tests, VffLags = NULL,
      #                             future.globals = c() )))
      #future({comp_tests(as.vector(Grid[1, 1:NPARAMS]), VffLags=NULL)})
      #testValues <- future_pmap_dfr(Grid[nextRows[sptS:sptE], 1:NPARAMS], function(...) {
      # comp_tests(c(...), VffLags=thisVffLags)
      #})

      log_print(paste0(substr(Sys.time(), start=12, stop=13), ":",
                       substr(Sys.time(), start=15, stop=16),
                       "hs. (inside for loop). Finished computing test values for this batch. Now about to update the Grid"))

      # Modification by reference (make sure setNumericRounding(2))
      Grid[nextRows[sptS:sptE],  (ESTVALUESNAMES) := testValues]

      #Grid[nextRows[sptS:sptE], (ESTVALUESNAMES) := testValues]
      if(.Last.updated != NROW(testValues)) {
        stop(message='Possible loss of information. Not all values computed in testValues are in the Grid.')
      }
#need package for datetime_Stamp
      log_print(paste0(substr(Sys.time(), start=12, stop=13), ":",
                       substr(Sys.time(), start=15, stop=16),
                       "hs. (inside for loop). Saving explored rows to file."))

      saveRDS(Grid[nextRows[sptS:sptE]],
              file=here(str_c('Explored_recovery_', '.rds')))

      rm(testValues)
      log_print("Calling garbage collector after finishing this batch in the
                for loop ")
      gc() %>% log_print()

      # Update perfomance data and print it to log
      PerfExp <- add_row(PerfExp,
                         tibble('time' = Sys.time(),
                                'length_minutes' = as.double(difftime(Sys.time(),PerfExp[[nrow(PerfExp), 'time']], units='mins')),
                                'npoints' = sptE-sptS,
                                'points_per_minute' = npoints / length_minutes))
      log_print(PerfExp)

    } # close for loop
    ############################# </FOR LOOP> ##########################
    ###################################################################

    if (localSearch) {
      # COMPUTE FURTHER VALUES TO EXPLORE
      log_print(paste0(substr(Sys.time(), start=12, stop=13), ":",
                       substr(Sys.time(), start=15, stop=16),
                       "hs. (inside repeat loop). Computing further values to explore."))
      # TODO: this search step requires the names of the parameters.
      # TODO: When data.table v is available you can improve the beginnig of this query by only requestin the columns you need (STCPARAMNAMES and S):
      # Grid[nextRows, j, env=list(j = as.list(c(STCPARAMNAMES, 'S'))), verbose=TRUE]

      nextRows <- Grid[nextRows, ][S<SCRITICAL, ][,
                                                  list('beta' = c('beta'+sS['beta'], 'beta', 'beta'-sS['beta'], 'beta' ),
                                                       'gamma' = c('gamma','gamma'+sS['gamma'], 'gamma', 'gamma'-sS['gamma']))] %>%
        unique(.) %>%
        {Grid[., list(S, ROW_NUMBER), on=c('beta', 'gamma'), nomatch=NULL][
          is.na(S), list(ROW_NUMBER)]$ROW_NUMBER}

      log_print(str_c("nextRows has ", NROW(nextRows), " after unique()"))
      # nextRows can be empty after the second set of filters
      if (NROW(nextRows) == 0) {

        log_print(paste0(substr(Sys.time(), start=12, stop=13), ":",
                         substr(Sys.time(), start=15, stop=16),
                         "hs. No more rows to explore!"))
        break()
      }

    } else {

      break() # for a global search we're finished now

    }

    log_print("Calling garbage collector after computing nextRows")
    gc() %>% log_print()


  }   # close repeat

  log_print('Perseverance has landed.')

  c('Perseverance has landed.')

}
