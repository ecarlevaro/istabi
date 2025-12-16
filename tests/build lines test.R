test_that("Build lines works", {
  PARAMS_CFG <- tibble::tribble(~name, ~iniVal, ~stepSize, ~minVal, ~maxVal, ~decimalPlaces,
                        'beta',  0.6, 0.025, 0, 1, 1,
                        'gamma', 0.6, 0.025, 0, 1, 1)  %>%
    dplyr::mutate('N_TICKS' = ((maxVal-minVal)/stepSize)+1)

  originsColName='iniVal'

  thisGridLines <- iStabi::build_lines(PARAMS_CFG, originsColName='iniVal')

  View(thisGridLines)
})
