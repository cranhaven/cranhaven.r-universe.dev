## Bayesian Transmission Model
[![ForeSITE Group](https://github.com/EpiForeSITE/software/raw/e82ed88f75e0fe5c0a1a3b38c2b94509f122019c/docs/assets/foresite-software-badge.svg)](https://github.com/EpiForeSITE)

Provides estimates for critical epidemiological parameters that characterize the spread of bacterial pathogens in healthcare settings. Parameter estimated: Transmission rate (frequency-dependent or density-dependent mass action), importation probability, clearance rate (loss of colonization per colonized person per unit time), surveillance test sensitivity, surveillance test specificity, effect of covariate on transmission (multiplier in relation to overall transmission rate).

## Installation

You can install the stable version of `bayestransmission` from CRAN with:

``` r
install.packages("bayestransmission")
```

To get a bug fix or to use a feature from the development version, you
can install the development version of `bayestransmission` from
[GitHub](https://github.com/EpiForeSITE/bayestransmission) with:

You can install the development version of bayestransmission from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("EpiForeSITE/bayestransmission")
```

### System Requirements

This package requires a C++ compiler and the following system dependencies:

- R (>= 3.5.0)
- Rcpp (>= 1.0.0)
- RcppArmadillo

## Quick Start

```r
library(bayestransmission)

# Load example data
data(simulated.data)

# Set up model parameters
params <- LinearAbxModel(
  nstates = 2,
  SurveillanceTest = SurveillanceTestParams(
    colonized = Param(init = 0.8, weight = 1),
    uncolonized = Param(init = 1e-10, weight = 0)
  )
  # ... additional parameters
)

# Run MCMC
results <- runMCMC(
  data = simulated.data,
  modelParameters = params,
  nsims = 1000,
  nburn = 100,
  outputparam = TRUE,
  outputfinal = FALSE,
  verbose = TRUE
)
```

For more detailed examples, see the package vignettes:

```r
browseVignettes("bayestransmission")
```

## References

- Khader K, Thomas A, Stevens V, Visnovsky L, Nevers M, Toth D, Keegan LT, Jones M, Rubin M, Samore MH (2021). [Association Between Contact Precautions And Transmission of Methicillin-Resistant Staphylococcus Aureus in Veterans Affairs Hospitals](https://pubmed.ncbi.nlm.nih.gov/33720369/) [DOI](https://doi.org/10.1001/jamanetworkopen.2021.0971). JAMA Netw Open.

- Khader K, Munoz-Price LS, Hanson R, Stevens V, Keegan LT, Thomas A, Pezzin LE, Nattinger A, Singh S, Samore MH (2021). [Transmission Dynamics of Clostridioides difficile in 2 High-Acuity Hospital Units](https://doi.org/10.1093/cid/ciaa1580). Clin Infect Dis.

- Khader K, Thomas A, Huskins WC, Stevens V, Keegan LT, Visnovsky L, Samore MH (2021). [Effectiveness of Contact Precautions to Prevent Transmission of Methicillin-Resistant Staphylococcus aureus and Vancomycin-Resistant Enterococci in Intensive Care Units](https://doi.org/10.1093/cid/ciaa1603). Clin Infect Dis.

- Khader K, Thomas A, Jones M, Toth D, Stevens V, Samore MH (2019). [Variation and trends in transmission dynamics of Methicillin-resistant Staphylococcus aureus in veterans affairs hospitals and nursing homes](https://doi.org/10.1016/j.epidem.2019.100347). Epidemics.

- Thomas A, Khader K, Redd A, Leecaster M, Zhang Y, Jones M, Greene T, Samore M (2018). [Extended models for nosocomial infection: parameter estimation and model selection](https://doi.org/10.1093/imammb/dqx010). Math Med Biol, 35(suppl_1), 29-49.

- Khader K, Thomas A, Huskins WC, Leecaster M, Zhang Y, Greene T, Redd A, Samore MH (2017). [A dynamic transmission model to evaluate the effectiveness of infection control strategies](https://doi.org/10.1093/ofid/ofw247). Open Forum Infect Dis.

- Thomas A, Redd A, Khader K, Leecaster M, Greene T, Samore M (2015). [Efficient parameter estimation for models of healthcare-associated pathogen transmission in discrete and continuous time](https://doi.org/10.1093/imammb/dqt021). Math Med Biol, 32(1), 79-98."

This work was supported by the Centers for Disease Control and Prevention, Modeling Infectious Diseases in Healthcare Network award U01CK000585.
