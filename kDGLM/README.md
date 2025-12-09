# kDGLM: An R package for Bayesian analysis of Generalized Dynamic Linear Models

Welcome to the `kDGLM` GitHub repository! `kDGLM` is an innovative R package tailored for Bayesian analysis of Generalized Dynamic Linear Models (GDLM), catering to both uni- and multivariate exponential families. This package is your go-to tool for sequential inference on time series data, offering a broad spectrum of functionalities including fitting, smoothing, monitoring, and feed-forward interventions.

Developed based on the methodology proposed in Alves et al.([2024](https://doi.org/10.48550/arXiv.2201.05387)), `kDGLM` integrates seamlessly with established techniques in the literature, particularly those applied in Gaussian Dynamic Models. It supports various features such as discount strategies, autoregressive components, transfer functions, and more, leveraging the Kalman filter and smoothing properties for exceptional computational efficiency. With kDGLM, you can expect almost instantaneous fitting times that scale linearly with your time series length, making it an invaluable tool for analyzing extended time series datasets.

Currently, `kDGLM`  supports a variety of distributions, including:

- Univariate Normal (with unknown mean and observational variance);
- Bivariate Normal (with unknown means, observational variances, and correlation);
- Poisson;
- Gamma (with known shape and unknown mean);
- Multinomial (with known number of trials and unknown event probabilities).
  
Furthermore, `kDGLM`  allows for the joint modeling of multiple time series, provided each series adheres to one of the supported distributions. Our ongoing development efforts are focused on continuously expanding the range of supported distributions to enhance the package's versatility.

Whether you are analyzing time series data for research, business intelligence, or any other purpose, `kDGLM`  offers a powerful, efficient, and comprehensive solution for your needs. Explore our documentation to get started and join the community of users benefiting from the advanced capabilities of `kDGLM`!

# Installation
## Dependencies
Before installing kDGLM, ensure you have the following dependencies installed in your R environment:

- `extraDistr` >= 1.9.1
- `cubature` >= 2.1.0
- `Rfast` >= 2.0.8
- `generics` >= 0.1.3

Additionally, for an enhanced plotting experience, we recommend installing the `ggplot2` and `plotly` packages, which augment the built-in plot methods provided by `kDGLM`.

## Installing kDGLM
As of now, `kDGLM` is available directly from its GitHub repository. While we are in the process of submitting `kDGLM` to CRAN, you can install the latest version of the package by executing the following command in your R console:

```r
remotes::install_github('silvaneojunior/kDGLM')
```

This command uses the remotes package to install kDGLM. If you do not have remotes installed, you can install it first by running install.packages("remotes").

# Getting Started
After installation, load `kDGLM` into your R session:

```r
library(kDGLM)
```

To test if the package is working properly, execute the following code exemple:

```r
 # Poisson case
 data <- c(AirPassengers)

 level <- polynomial_block(rate = 1, order = 2, D = 0.95)
 season <- harmonic_block(rate = 1, order = 2, period = 12, D = 0.975)

 outcome <- Poisson(lambda = "rate", data = data)

 fitted.data <- fit(level, season,
   AirPassengers = outcome
 )
 summary(fitted.data)

plot(fitted.data, plot.pkg = "base")
```

Refer to the [vignettes](https://silvaneojunior.github.io/kDGLM/) for detailed usage instructions. Also see the documentation of the `fit` method for basic examples for each type of outcome the `kDGLM` package offers.

# Contributing

We welcome contributions! If you're interested in improving kDGLM, please consider submitting bug reports, feature requests, or pull requests.

# Acknowledgements

- Raíra Marotta, who gently provided the base code for the initial versions.

# Contact
For support or to provide feedback, please contact [Silvaneo dos Santos Jr.](mailto:silvaneo@dme.ufrj.br) or open an issue on the GitHub repository.
