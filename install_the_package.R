# If you load the Grid Package RStudio project
# and then run these lines you will install the package
# under the Windows user profile page
# Users\<user name>\AppData\Local|R\win-library\4.4\iStabi
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
