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
#' @format A tibble with 2,000 rows and 5 variables:
#' \describe{
#'   \item{time}{Integer. Time period}
#'   \item{y1}{num. First variable}
#'   \item{y2}{num. Second variable}
#'   \item{w1}{num. First structural shock}
#'   \item{w2}{num. Second structural shock} 
#' }
#' @source Created for examples.
"data_sim_biVAR"
