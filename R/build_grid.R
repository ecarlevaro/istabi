#'Build Grid
#'
#'\description{This function builds a grid with columns that contain:
#'* the values of each parameter that are desired to be explored
#'* An empty value for each of the test statistics that this function looks at
#' S and QLLstab }
#'
#'\usage{build_grid(paramsCfg, thisGridLines, neqs, psUnderNull) }
#'
#'@param paramsCfg, tibble with information about each parameter space.
#'@param thisGridLines, a list of the values for each parameter that will be
#'  explored to compute test statistic in 'Stage2_rover_Perseverance()'
#'@param neqs, number of equations in the model (equal to the global constant
#'  NEQS)
#'@param psUnderNull, number of parameters estimated under the null that want to
#'  be stored in the Grid.
#'
#'@returns a numeric grid
#'@export
#'
#' @examples #Example 1
#' PARAMS_CFG <- tribble(
#'~name,    ~iniVal, ~stepSize, ~minVal, ~maxVal, ~decimalPlaces,
#''alpha',   0.50,       0.10,       0.15,    0.98,      2)  %>%
#'  dplyr::mutate('N_TICKS' = ((maxVal-minVal)/stepSize)+1)
#'
#'   originsColName='iniVal'
#'
#' gridLines <- build_lines(PARAMS_CFG, 'iniVal')
#'
#' Grid <- build_grid(PARAMS_CFG, gridLines)
#'
#' view(Grid)
build_grid <- function(paramsCfg, thisGridLines, neqs, psUnderNull) {

  dimLines <- map_int(thisGridLines, NROW)
  log_print(paste0("The grid should have: ", prod(dimLines), " rows"))

  GridParams  = expand.grid(thisGridLines)  %>%
    as.data.table() %>%
    setkeyv(., names(thisGridLines))
  gc()
  nNuisParams = 0
  colNames <-  c('S', 'qLLStab')
  if( exists('X', mode='numeric')) {
    nNuisParams = NCOL(X)
    colNames <-  c('S', 'qLLStab', str_c(rep("c2_hat", times=nNuisParams), 1:nNuisParams))
  }
  GridEstValues = matrix(as.double(NA),
                         ncol = (2+nNuisParams), nrow=1,
                         dimnames=list(NULL, colNames)) %>%
    as_tibble()

  GridDT <- cbind(GridParams, GridEstValues, 'ROW_NUMBER'=NA) %>%
    as.data.table()

  rm(GridParams, GridEstValues)
  gc()

  setkeyv(GridDT, paramsCfg$name)
  GridDT[, ROW_NUMBER := 1:NROW(GridDT)]

  GridDT
}
