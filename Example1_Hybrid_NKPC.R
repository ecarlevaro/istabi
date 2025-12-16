#required packages to make a package
pacman::p_load(devtools, roxygen2, testthat)
#dependecies
#pacman::p_load(tidyverse, data.table, DT, plotly, gridExtra, latex2exp, here, logr, future.apply, furrr)
#pacman::p_load(dplyr,parallel,sandwich,here,future,logr,profvis,lobstr,parallel)
#https://ourcodingclub.github.io/tutorials/writing-r-package/
#create_package
#https://combine-australia.github.io/r-pkg-dev/functions.html
#load_all()
#run these two lines and then library(iStabi) in normal rstudio to install package
# load_all loads a package. It roughly simulates what happens when a package is installed and loaded with library().
devtools::load_all()

devtools::install()
library(tidyverse)
# True values
thetaStar <- c('gammaF' = 0.591,
               'lambda' = 0.015,
               'gammaB' = 0.378)

### LOAD DATA ----
DATA <- readRDS('sim_NKPC_sams1K.rds')[[1]] %>%
  rename('s_t' = 's', 'pi_t' = 'pi') %>%
  mutate('t' = 1:NROW(.), .before=s_t)

# Build sample
ZLAGS = 2

breakDate = NROW(DATA)/2

Dlags <- tibble('s_tM1' = lag(DATA$s_t),
                'pi_tM1' = lag(DATA$pi_t),
                'pi_tP1' = lead(DATA$pi_t))
Dlags[, 'I_s_tM1'] <- c(rep(0, breakDate), Dlags$s_tM1[(breakDate+1):NROW(DATA)])

sel = c((ZLAGS+1):(NROW(DATA)-1))
sam <- cbind(DATA[sel, c('t', 's_t', 'pi_t')], Dlags[sel, ])

Z <- sam[, c('pi_tM1', 's_tM1', 'I_s_tM1')] %>%
  #cbind('Constant'=1, .) %>%
  as.matrix(.)

NEQUATIONS = 1
# Yb
# Get the vector b
GET_B <- function(theta0) {
  c(1,
    -theta0['lambda'],
    -theta0['gammaF'])
}
# Y = [y_t s_t y_t+1]
Y <- cbind( 'pi_t' = sam$pi_t, 's_t' = sam$s_t, 'pi_tP1' = sam$pi_tP1 )

X <- sam['pi_tM1'] %>% as.matrix(.)


get_deriv_B <- function(theta0) {

  rbind(c(0, 0),
        c(-1, 0),
        c(0, -1))

}

# Data for closed-form solution in the Standard IV objects
# The nuisance variable is the LAST one.
stdIVobjs <- list('y' = {sam['pi_t'] %>% as.matrix()},
                  'X' = { sam[c('s_t' , 'pi_tP1', 'pi_tM1')] %>% as.matrix()},
                  'Z' = { sam[c('s_tM1', 'I_s_tM1', 'pi_tM1')] %>% as.matrix()})

descStats <- as_tibble(cbind(Y, Z))%>%
  summarise(across(.fns = list('mean'=mean, 'sd'=sd, 'min'=min, 'max'=max))) %>%
  round(., digits=3) %>%
  mutate(., 'N' = NROW(Y))

#gridExtra::tableGrob(t(descStats), rows=colnames(descStats), cols = NULL) %>%
#  ggsave(plot=., 'descStats.pdf',
#         width=18, height=18, units='cm')
DT::datatable(descStats,
              options=list('scrollX'=TRUE))

#### Search settings ----
NCORES = 4
STEPSPERCORE = 200
# Size of adjacent cells to explores is NCORES * STEPSPERCORE

# PARAMS_CFG a tibble where each row defines properties of the parameters in the name column
# stepSize: size of the step for each parameter. You can have different sizes (and precision) for each paramter.
# minVal and maxVal: Limits of the parameter space for each parameters. A list where each element defines the minimum and maximum for each parameter
# ORDER: the order of the parameter has to match the order in theta (as used by the get_b() fcn)
PARAMS_CFG <- tribble(~name, ~iniVal, ~stepSize, ~minVal, ~maxVal,
                      'lambda', NA, 0.003, 0, 1,
                      'gammaF',  NA, 0.02, 0, 1) %>%
  mutate('N_TICKS' = round(((maxVal-minVal)/stepSize)+1, 0))

# Initial value
PARAMS_CFG$iniVal <- sam %>%
  lm(pi_t ~ 1 + s_t  + pi_tP1  + pi_tM1, data=.) %>%
  .$coefficients %>%
  {c('lambda' = .[['s_t']],
     'gammaF' = .[['pi_tP1']])}

gridLines <- build_lines(PARAMS_CFG, 'iniVal')

# paramsCfg <- PARAMS_CFG
# thisGridLines <- thisGridLines
# NEQS = 1
# psUnderNull = 1
Grid <- build_grid(PARAMS_CFG, gridLines)

theta0 = c('lambda' = 0.015, 'gammaF' = 0.591)
GET_B(theta0)
# TODO: test that the moment function works
# TODO: object Mx not found. Generate all objects

NEQS = 1
T = NROW(sam)
MODELEQS <- list(list('Y' = Y,  'get_b'=NULL, 'X'=X, 'Z'=Z))
# Level of condfidence of the S set. The closer to 1, the more robust the search is to local minima but it takes longer.
SPVALUE = 0.95
# R is NEQS*ncol(Z) where ncol(Z) is the number of instruments
# By default this matrix has to be automatically built depending on the dimensions of MODELEQS.
# The user can alternatively provide this matrix
R <-    rbind(
  cbind(matrix(1, nrow=ncol(Z), ncol=ncol(Z)))
)

print(R)

theta0 = c('lambda' = 0.015, 'gammaF' = 0.591) # the true theta with the estimated (nuisance) parameter 0.378
comp_tests(theta0, VffLags = NULL)
print(SCRITICAL)

theta0 = c('lambda' = 0.015, 'gammaF' = 0.291)
comp_tests(theta0, VffLags = NULL)
theta0 = c('lambda' = 0.015, 'gammaF' = 0.491)
comp_tests(theta0, VffLags = NULL)

# For 3 moment conditions the critical value at 95% confidence for the qllStab is 19.95 and 18.14 at 90% confidence
Explored <- ROVER(searchType = ‘global’)

