
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OPSR <img src="man/figures/logo.svg" align="right" height="139" alt="" />

<!-- badges: start -->

<!-- badges: end -->

Estimates ordered probit switching regression (OPSR) models - a Heckman
type selection model with an ordinal selection and continuous outcomes.
Different model specifications are allowed for each treatment/regime.

## Installation

Install from CRAN:

``` r
install.packages("OPSR")
```

You can install the development version of `OPSR` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dheimgartner/OPSR")
```

## Example

`OPSR` can be used whenever the ordinal treatment is not assigned
exogenously but self-selected and one is interested in the treatment
effect on a continuous outcome. The motivating example is telework
frequency (conceptually, the treatment) and vehicle miles driven (the
outcome of interest). We assume that two distinct processes lead people
to choose a certain telework frequency and how mobile they are. Further
and most importantly, the possibility of selection on unobservables
exists. I.e., unobserved factors (as part of the errors of the two
processes) might influence both the ordinal and continuous outcome. This
leads to error correlation which needs to be accounted for in the
modeling effort in spirit of Heckman.

``` r
library(OPSR)
#> To cite package 'OPSR' in publications use:
#> 
#>   Heimgartner D, Wang X (2025). "OPSR: A package for estimating ordered
#>   probit switching regression models in R." Arbeitsbericht Verkehrs-
#>   und Raumplanung 1907, IVT, ETH Zurich. doi:10.3929/ethz-b-000729641
#>   <https://doi.org/10.3929/ethz-b-000729641>.
#> 
#>   Wang X, Mokhtarian PL (2024). "Examining the treatment effect of
#>   teleworking on vehicle-miles driven: Applying an ordered probit
#>   selection model and incorporating the role of travel stress."
#>   _Transportation Research Part A_, *186*, 104072.
#>   doi:10.1016/j.tra.2024.104072
#>   <https://doi.org/10.1016/j.tra.2024.104072>.
#> 
#> To see these entries in BibTeX format, use 'print(<citation>,
#> bibtex=TRUE)', 'toBibtex(.)', or set
#> 'options(citation.bibtex.max=999)'.

f <-
  ## ordinal and continuous outcome
  twing_status | vmd_ln ~
  ## selection model
  edu_2 + edu_3 + hhincome_2 + hhincome_3 +
  flex_work + work_fulltime + twing_feasibility +
  att_proactivemode + att_procarowning +
  att_wif + att_proteamwork +
  att_tw_effective_teamwork + att_tw_enthusiasm + att_tw_location_flex |
  ## outcome model NTW
  female + age_mean + age_mean_sq +
  race_black + race_other +
  vehicle + suburban + smalltown + rural +
  work_fulltime +
  att_prolargehouse + att_procarowning +
  region_waa |
  ## outcome model NUTW
  edu_2 + edu_3 + suburban + smalltown + rural +
  work_fulltime +
  att_prolargehouse + att_proactivemode + att_procarowning |
  ## outcome model UTW
  female + hhincome_2 + hhincome_3 +
  child + suburban + smalltown + rural +
  att_procarowning +
  region_waa

fit <- opsr(f, telework_data, printLevel = 0)
summary(fit)
#> 
#> Call:
#> opsr(formula = f, data = telework_data, printLevel = 0)
#> 
#> BFGS maximization, 182 iterations
#> Return code 0: successful convergence 
#> Runtime: 5.28 secs
#> Number of regimes: 3 
#> Number of observations: 1584 (535, 322, 727)
#> Estimated parameters: 56 
#> 
#> Log-Likelihood: -3538.757 
#> AIC: 7189.513 
#> BIC: 7490.105 
#> Pseudo R-squared (EL): 0.4868 
#> Pseudo R-squared (MS): 0.462 
#> Multiple R-squared: 0.2367 (0.1787, 0.1806, 0.1244)
#> 
#> Estimates:
#>                               Estimate Std. error t value  Pr(> t)    
#> kappa1                       1.1466204  0.1696082   6.760 1.38e-11 ***
#> kappa2                       2.3801501  0.1771205  13.438  < 2e-16 ***
#> s_edu_2                      0.2443606  0.1376536   1.775 0.075867 .  
#> s_edu_3                      0.4023184  0.1308658   3.074 0.002110 ** 
#> s_hhincome_2                 0.0895885  0.1131361   0.792 0.428440    
#> s_hhincome_3                 0.2807199  0.1110217   2.529 0.011455 *  
#> s_flex_work                  0.2828764  0.0975378   2.900 0.003730 ** 
#> s_work_fulltime              0.2536533  0.0999697   2.537 0.011171 *  
#> s_twing_feasibility          0.1326350  0.0057808  22.944  < 2e-16 ***
#> s_att_proactivemode          0.0815302  0.0398707   2.045 0.040868 *  
#> s_att_procarowning          -0.0779199  0.0404062  -1.928 0.053803 .  
#> s_att_wif                    0.1176906  0.0384118   3.064 0.002185 ** 
#> s_att_proteamwork            0.0855596  0.0370847   2.307 0.021047 *  
#> s_att_tw_effective_teamwork  0.3151287  0.0435362   7.238 4.54e-13 ***
#> s_att_tw_enthusiasm          0.0852045  0.0379058   2.248 0.024589 *  
#> s_att_tw_location_flex       0.0816184  0.0402637   2.027 0.042653 *  
#> o1_(Intercept)               3.7426043  0.2775669  13.484  < 2e-16 ***
#> o1_female                   -0.2084407  0.1063050  -1.961 0.049905 *  
#> o1_age_mean                  0.0097620  0.0037000   2.638 0.008331 ** 
#> o1_age_mean_sq              -0.0004327  0.0002485  -1.741 0.081616 .  
#> o1_race_black               -0.3918706  0.2375626  -1.650 0.099036 .  
#> o1_race_other               -0.0192830  0.1762266  -0.109 0.912868    
#> o1_vehicle                   0.1314998  0.0487828   2.696 0.007026 ** 
#> o1_suburban                  0.0094855  0.1592882   0.060 0.952514    
#> o1_smalltown                 0.4147294  0.1828185   2.269 0.023297 *  
#> o1_rural                     0.4945182  0.2327146   2.125 0.033587 *  
#> o1_work_fulltime             0.4367238  0.1316097   3.318 0.000906 ***
#> o1_att_prolargehouse         0.1864675  0.0523970   3.559 0.000373 ***
#> o1_att_procarowning          0.1224735  0.0664025   1.844 0.065123 .  
#> o1_region_waa               -0.2397950  0.1093528  -2.193 0.028318 *  
#> o2_(Intercept)               2.4193729  0.3875823   6.242 4.31e-10 ***
#> o2_edu_2                     0.2221850  0.3404089   0.653 0.513949    
#> o2_edu_3                     0.6693748  0.3253659   2.057 0.039658 *  
#> o2_suburban                  0.4451185  0.1734935   2.566 0.010299 *  
#> o2_smalltown                 0.2200755  0.2926442   0.752 0.452037    
#> o2_rural                     0.8233954  0.3244967   2.537 0.011166 *  
#> o2_work_fulltime             0.7052761  0.1688305   4.177 2.95e-05 ***
#> o2_att_prolargehouse         0.1648046  0.0789276   2.088 0.036794 *  
#> o2_att_proactivemode        -0.1758775  0.0762701  -2.306 0.021112 *  
#> o2_att_procarowning          0.1697231  0.0869310   1.952 0.050892 .  
#> o3_(Intercept)               2.3798672  0.2883497   8.253  < 2e-16 ***
#> o3_female                   -0.3709856  0.1105030  -3.357 0.000787 ***
#> o3_hhincome_2                0.4654192  0.2557430   1.820 0.068779 .  
#> o3_hhincome_3                0.3013654  0.2470044   1.220 0.222434    
#> o3_child                     0.1892227  0.0601994   3.143 0.001671 ** 
#> o3_suburban                  0.2852249  0.1358278   2.100 0.035738 *  
#> o3_smalltown                 0.3087142  0.2846410   1.085 0.278110    
#> o3_rural                     0.8498792  0.3280278   2.591 0.009573 ** 
#> o3_att_procarowning          0.2499500  0.0583057   4.287 1.81e-05 ***
#> o3_region_waa               -0.2709285  0.1087091  -2.492 0.012694 *  
#> sigma1                       1.1772629  0.0485867  24.230  < 2e-16 ***
#> sigma2                       1.2358839  0.0675020  18.309  < 2e-16 ***
#> sigma3                       1.4267208  0.0427374  33.383  < 2e-16 ***
#> rho1                         0.0669559  0.0960455   0.697 0.485724    
#> rho2                         0.1326106  0.0680882   1.948 0.051459 .  
#> rho3                         0.3010864  0.0703368   4.281 1.86e-05 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Wald chi2 (null): 1261.888 on 45 DF, p-value: < 0
#> Wald chi2 (rho): 21.9907 on 3 DF, p-value: < 1e-04
# texreg::screenreg(fit, beside = TRUE, include.pseudoR2 = TRUE, include.R2 = TRUE)
```
