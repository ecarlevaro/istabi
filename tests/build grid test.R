test_that("Build grid works", {
  PARAMS_CFG <- tribble(~name, ~iniVal, ~stepSize, ~minVal, ~maxVal, ~decimalPlaces,
                        'beta',  0.6, 0.025, 0, 1, 1,
                        'gamma', 0.6, 0.025, 0, 1, 1) %>%
    mutate('N_TICKS' = ((maxVal-minVal)/stepSize)+1)

  originsColName='iniVal'

  thisGridLines <- build_lines(PARAMS_CFG, originsColName='iniVal')
  NEQS = 1
  Grid <- build_grid(PARAMS_CFG, thisGridLines, NEQS, psUnderNull)


})
