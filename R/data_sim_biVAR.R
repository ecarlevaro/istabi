#' Bivariate VAR with structural shocks and GARCH volatility
#'
#' A simulated bivariate structural VAR dataset generated from
#' \deqn{B_{0} y_{t} = B_{1} y_{t-1} + w_{t}}{B0 y_t = B1 y_{t-1} + w_t}
#' where \eqn{w_t} is a \eqn{(K \times 1)} vector of structural shocks and
#' \eqn{B_0} is a \eqn{(K \times K)} matrix capturing contemporaneous responses
#' across variables.
#' Each structural shock component \eqn{w_{i,t}} follows a different GARCH(1,1)
#' volatility process.
#' True parameters are
#' \deqn{
#' \left[
#' \begin{array}{cc}
#' 1 & -0.15 \\
#' 0.75 & 1
#' \end{array}
#' \right] y_t
#' =
#' \left[
#' \begin{array}{cc}
#' 0.5 & 0.2 \\
#' 0.1 & 0.4
#' \end{array}
#' \right] y_{t-1}
#' + w_t
#' }{[1  -0.15; 0.75  1] y_t = [0.5  0.2; 0.1  0.4] y_{t-1} + w_t}
#' with the element (1,2) in \eqn{B_0} is  \eqn{\beta = -0.15} and the element (2,1) in \eqn{B_0} is \eqn{\alpha = 0.75}.
#'
#' @format A tibble with 1,016 rows and 8 variables:
#' \describe{
#'   \item{i}{num. First variable}
#'   \item{s}{num. Second variable}
#'   \item{vare}{num. First variable structural shock}
#'   \item{eta}{num. Second variable structural shock}
#'   \item{sigmaSqVare}{num. First variable structural shock variance} 
#'   \item{sigmaSqEta}{num. Second variable structural shock variance}
#' }
#' @source Created for examples (see file CalCrisis-HARRVBig-NoZ-1016-Tres.R in STC or external documentation for this package).
"data_sim_biVAR"
