<!-- badges: start -->
[![R-CMD-check](https://github.com/icpm-kth/uqsa/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/icpm-kth/uqsa/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# Uncertainty Quantification (UQ) and Sensitivity Analysis (SA)

This is an R package that performs *parameter estimation*,
*uncertainty quantification*, and *global sensitivity analysis* using
Bayesian (UQ) and variance decomposition methods (GSA). It is primarily designed for **biochemical reaction networks** describing intracellular cellular pathways and similar. All information about the model as well as the experimental data used for parameter estimation and uncertainty quantification is stored in the [SBtab](https://github.com/tlubitz/SBtab) table format for Systems Biology projects.

* **Source code:** https://github.com/icpm-kth/uqsa/
* **Documentation** https://icpm-kth.github.io/uqsa/
* **Examples:** https://icpm-kth.github.io/uqsa/articles/examples_overview.html
* **Cite:** https://icpm-kth.github.io/uqsa/articles/cite.html


## Acknowledgements

This open source software code was developed with support from the Swedish e-Science Research Centre (SeRC) as well as within the Human Brain Project, funded from the European Union’s Horizon 2020 Framework Programme for Research and Innovation (945539) (Human Brain Project SGA1, SGA2 and SGA3).

## Technical summary of the UQSA tool-set

- An easy-to-use, human- and machine-readable format for reaction-based
  models and calibration data (as tables/data-frames).
- Converting biological models into mathematical models, and code:
  + **Deterministic models** ODE solvers from the GSL odeiv2 module _written in C_
  + **Stochastic models** built in Gillespie algorithm implementation _written in C_
- We also create R-files for the optional use of ODE solvers from the [deSolve](https://cran.r-project.org/package=deSolve)
  package, by writing a custom likelihood function
- It is possible to use the [GillespieSSA2](https://cran.r-project.org/package=GillespieSSA2)
  package by writing a custom distance function.
- MCMC algorithms for sampling from the posterior distribution
  + **Deterministic models** Random Walk Metropolis and SMMALA, both
  available in a parallel tempering setting.
  + **Stochastic models** ABC-SMC or ABC-MCMC algorithm.
- By default we use an ABC distance function in the form of weighted
  Euclidean distance, with weights given by (the reciprocals of) the
  experimental measurement errors (when available). The ABC distance
  function can be defined by the user.
- A vine-copula-based approach for sampling from a non-trivial,
  non-orthogonal prior.
- Functionality for global sensitivity analysis on orthogonal and
  non-orthogonal input factors.
- Model simulations are specified as lists (a convenient abstraction)
- An event system for scheduled events within an experiment to model
  sudden activation (and similar interventions) in an experimental
  protocol.
- Complex experimental (transient) input data, such as neuroscience
  spike-trains, can be easily fitted to corresponding model functions.
- Both the <span class="smallcaps">uq</span> and <span
  class="smallcaps">sa</span> algorithms can be run on compute
  clusters across several nodes using
  [pbdMPI](https://cran.r-project.org/package=pbdMPI).
