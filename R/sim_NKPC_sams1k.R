#' Inference on hybrid NKPC curve as in Mavroeidis (2005)
#'
#'  There are 2,000 simulated models for each case each
#'  @format ## `sim_NKPC_sams1k`
#'  a data frame with 4 variables and 300 observations. :
#' \describe{
#'   \item{s}{ }
#'   \item{pi}{inflation}
#'   \item{v}{}
#'   \item{eps}{}
#'   ...
#' }
#' @source <https://www.who.int/teams/global-tuberculosis-programme/data>
#'sim_NKPC_sams1K <- readRDS("C:/Users/User/Box/iStabi/Examples/example_NKPC/sim_NKPC_sams1K.rds")
#'usethis::use_data(sim_NKPC_sams1k)
"sim_NKPC_sams1K"
