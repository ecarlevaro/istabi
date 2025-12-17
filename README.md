# iStabi - Identification by stability restrictions
Estimation of time-series models using stability restrictions. Estimation proceeds in a generalised method of moments framework using an inferential procedure that searches for candidate values of the parameters of interest.

The user provides the moment(s) of interest and properties of the parameter space. This package carries out a grid search over the parameter space and returns a set of admissible values for the parameters.

It is an R implementation of methods described in [Magnusson, L. M., \& Mavroeidis, S. (2014). Identification using stability restrictions. Econometrica, 82(5), 1799-1851](http://dx.doi.org/10.3982/ECTA9612).

Current implementation relies on a 2-step GMM instrumental variable estimator.

With [Caleb Hudson](https://github.com/CalebZHudson).

## INSTALLATION

You first need to clone ('download') the repo to a local folder. Then open R (or RStudio) and move to that folder. Once inside the folder run the script `
The key two lines are

```{}
devtools::load_all()

devtools::install()
```

which will install the library in your users' folder.

Then you can use it as any library with

```{}
library(iStabi)
```

## USAGE

See the vignette file under `vignettes/2_exploration_hybridNKPC_M2005.qmd` which will replicate some of the work related to the forward-looking Phillips curve as in Mavroeidis, S. (2005). “Identification Issues in Forward-Looking Models Estimated by
GMM, with an Application to the Phillips Curve”. In: Journal of Money, Credit and
Banking 37.3, pp. 421–448. doi: 10.1353/mcb.2005.0031.