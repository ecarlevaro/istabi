#'Build Lines
#'
#'  \description{
#' This function lines of the 'build_grid()' function.
#'* For each parameter 'build_lines()' generates a range of values from the minimum value
#' to the maximum value by the step size specified in PARAMS_CFG}
#'
#'
#'\usage{build_lines(PARAMS_CFG, originsColName='iniVal')}
#'
#'@param PARAMS_CFG, tibble with information about each parameter space
#'@param originsColName, The column name in 'PARAMS_CFG' that contains the parameter initial values
#'
#'@returns the parameter value for each line in the grid
#'@export
#'
#' @examples #Example 1
#' PARAMS_CFG <- tribble(~name, ~iniVal, ~stepSize, ~minVal, ~maxVal, ~decimalPlaces,
#''beta',  0.6, 0.025, 0, 1, 1,
#''gamma', 0.6, 0.025, 0, 1, 1)  %>%
#'  dplyr::mutate('N_TICKS' = ((maxVal-minVal)/stepSize)+1)
#'originsColName='iniVal'
#'thisGridLines <- build_lines(PARAMS_CFG, originsColName='iniVal')
#'view(thisGridLines)
build_lines <- function(PARAMS_CFG, originsColName) {
  # originsColName = 'iniVal'
  ## TODO: generalise grid construction for N parameters using the do.call() fcn
  # Build sequence of parameter values  in each dimension.
  # The parameter with longest step is the last in the matrix, that's why we NEED TO arrange by N_TICKS
  Ticks <- PARAMS_CFG %>%
    arrange(., N_TICKS)
  lines <- pmap(Ticks, function(...) {
    #thisParam <- Ticks[1,]
    thisParam <- tibble(...)
    origin <- round(thisParam[, originsColName, drop = TRUE], digits = 4)

    if (thisParam$N_TICKS == 0) {
      startValue = origin
      endValue = origin
    } else {
      # The start and end values are the closest value to the minVal that is a multiple of stepSize such that a integer number of jumps will reach origin
      startValue = origin - floor((origin - thisParam$minVal) / thisParam$stepSize) *
        thisParam$stepSize
      endValue = origin + floor((thisParam$maxVal - origin) / thisParam$stepSize) *
        thisParam$stepSize
    }

    {
      seq(from = startValue,
          to = endValue,
          by = thisParam$stepSize)
    } %>%
      round(., digits = 4)

  })
  names(lines) = Ticks$name
  # Reorder as in paramCfg
  linesOrdered <- lines[match(PARAMS_CFG$name, names(lines))]

  # Build the grid by combining above sequences (lines)

  linesOrdered

}
