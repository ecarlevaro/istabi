# iStabi - Identification by stability restrictions
Estimation of time-series models using stability restrictions. Estimation proceeds in a generalised method of moments framework using an inferential procedure that searches for candidate values of the parameters of interest.

The user provides the moment(s) of interest and properties of the parameter space. This package carries out a grid search over the parameter space and returns a set of admissible values for the parameters.

It is an R implementation of methods described in Magnusson, L. M., \& Mavroeidis, S. (2014). Identification using stability restrictions. Econometrica, 82(5), 1799-1851.

Current implementation relies on a 2-step GMM instrumental variable estimator.
