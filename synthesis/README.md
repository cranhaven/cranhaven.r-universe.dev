# synthesis: Synthetic data generator

Generate synthetic time series from commonly used statistical models, including linear, nonlinear and chaotic systems. Applications to testing methods can be found in Jiang, Z., Sharma, A., & Johnson, F. (2019) \doi{10.1016/j.advwatres.2019.103430} and Jiang, Z., Sharma, A., & Johnson, F. (2020) \doi{10.1029/2019WR026962} associated with an open-source tool by Jiang, Z., Rashid, M. M., Johnson, F., & Sharma, A. (2020) \doi{10.1016/j.envsoft.2020.104907}.

It can be used for variable selection, prediction, and classification and clustering problem generation. 

## Requirements
<pre>
Dependencies:
  stats, MASS

Suggest:
  testthat, devtools
</pre>

## Installation

You can install the package via devtools from [GitHub](https://github.com/) with:

``` r
devtools::install_github("zejiang-unsw/synthesis")
```

or via CRAN with: 

``` r
install.packages("synthesis")
```

## Report issues

https://github.com/zejiang-unsw/synthesis/issues

## Update history
### synthesis 1.2.2
* Included the affine error model.
* Included the Brownian motion models.
* Included the build-up and wash-off water quality model.
* Updated the vignette.

### synthesis 1.2.1
* Added a vignette to the package.
* Included the state-space model.

### synthesis 1.2.0
* Added a `NEWS.md` file to track changes to the package.
* Initial version submitted to CRAN. 


## Citation

Jiang, Z., Rashid, M. M., Johnson, F., & Sharma, A. (2020). A wavelet-based tool to modulate variance in predictors: An application to predicting drought anomalies. Environmental modelling & software, 135, 104907.

Jiang, Z., Sharma, A., & Johnson, F. (2020). Refining Predictor Spectral Representation Using Wavelet Theory for Improved Natural System Modeling. Water Resources Research, 56(3), e2019WR026962.

Jiang, Z., Sharma, A., & Johnson, F. (2019). Assessing the sensitivity of hydro-climatological change detection methods to model uncertainty and bias. Advances in Water Resources, 134, 103430.

Galelli, S., Humphrey, G. B., Maier, H. R., Castelletti, A., Dandy, G. C., & Gibbs, M. S. (2014). An evaluation framework for input variable selection algorithms for environmental data-driven models. Environmental modelling & software, 62, 33-51.

Sharma, A. (2000). Seasonal to interannual rainfall probabilistic forecasts for improved water supply management: Part 1 - A strategy for system predictor identification. Journal of Hydrology, 239(1), 232-239. 

