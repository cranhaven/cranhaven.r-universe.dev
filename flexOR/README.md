# flexOR
Flexible Estimation of Odds Ratio Curves: Introducing the flexOR Package

## Description
Explore the relationship between continuous predictors and binary outcomes with flexOR, an R package designed for robust nonparametric estimation of odds ratio curves. 
Overcome limitations of traditional regression methods by leveraging smoothing techniques, particularly spline-based methods, providing adaptability to complex datasets. 
The package includes options for automatic selection of degrees of freedom in multivariable models, enhancing adaptability to diverse datasets and intuitive visualization functions facilitate the interpretation and presentation of estimated odds ratio curves.

## Installation
If you want to use the release version of the **flexOR** package, you can install the package from CRAN as follows:
```r
install.packages(pkgs="flexOR");
```
If you want to use the development version of the **flexOR** package, you can install the package from GitHub via the [**remotes**](https://remotes.r-lib.org) package:
```r
remotes::install_github(
  repo="martaaaa/flexOR",
  build=TRUE,
  build_manual=TRUE
);
```

## Authors
Marta Azevedo, Luís Meira-Machado <lmachado@math.uminho.pt> \
and Artur Araujo <artur.stat@gmail.com> \
Maintainer: Marta Azevedo <marta.vasconcelos4@gmail.com>

## Funding
This research was financed by **FCT** -- *Fundação para a Ciência e a Tecnologia*, under Projects UIDB/00013/2020, UIDP/00013/2020, and EXPL/MAT-STA/0956/2021.

## References
Hosmer, D. W. and Lemeshow, S. and Sturdivant, R. X. (2013). *Applied Logistic Regression: Third Edition*, John Wiley and Sons Inc., New York, NY.

Royston, P. and Altman, D. G. and Sauerbrei, W. (2006). Dichotomizing continuous predictors in multiple regression: A bad idea. *Statistics in Medicine*, **25**(1), 127–141. [doi:10.1002/sim.2331](https://doi.org/10.1002/sim.2331)

Hastie, T. J. and Tibshirani, R. J. (1990). *Generalized Additive Models*, Chapman & Hall/CRC, New York, NY.

Wood, S. N. (2017). *Generalized Additive Models: An Introduction with R: Second Edition*, Chapman & Hall/CRC, London, UK.

Akaike, H. (1974). A new look at the statistical model identification. *IEEE Transactions on Automatic Control*, **19**(6), 716–723. [doi:10.1109/TAC.1974.1100705](https://doi.org/10.1109/TAC.1974.1100705)

Hurvich, C. M. and Simonoff, J. S. and Tsai, C. (1998). Smoothing parameter selection in nonparametric regression using an improved akaike information criterion. *Journal of the Royal Statistical Society Series B: Statistical Methodology*, **60**(2), 271–293. [doi:10.1111/1467-9868.00125](https://doi.org/10.1111/1467-9868.00125)

Schwarz, G. (1978). Estimating the dimension of a model. *Annals of Statistics*, **6**(2), 461–464. [doi:10.1214/aos/1176344136](https://doi.org/10.1214/aos/1176344136)

Cadarso-Suárez, C. and Meira-Machado, L. and Kneib, T. and Gude, F. (2010). Flexible hazard ratio curves for continuous predictors in multi-state models: an application to breast cancer data. *Statistical Modelling*, **10**(3), 291–314. [doi:10.1177/1471082X0801000303](https://doi.org/10.1177/1471082X0801000303)

Meira-Machado, L. and Cadarso-Suárez, C. and Gude, F. and Araújo, A. (2013). smoothHR: An R Package for Pointwise Nonparametric Estimation of Hazard Ratio Curves of Continuous Predictors. *Computational and Mathematical Methods in Medicine*, **2013**, 11 pages. [doi:10.1155/2013/745742](https://doi.org/10.1155/2013/745742)

de Boor, C. (2001). *A Practical Guide to Splines: Revised Edition*, Springer, New York, NY.

Wood, S. N. and Pya, N. and Safken, B. (2016). Smoothing Parameter and Model Selection for General Smooth Models. *Journal of the American Statistical Association*, **111**(516), 1548-1563. [doi:10.1080/01621459.2016.1180986](https://doi.org/10.1080/01621459.2016.1180986)
