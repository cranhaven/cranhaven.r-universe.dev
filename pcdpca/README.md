# pcdpca

Implementation of "Dynamic principal components of periodically correlated functional time series".

Two examples in `demo` directory:

  - pm10 data from Graz (comparizon with DFPCA paper)
  - simplation with parametrized periodicity

## Installation

    library("devtools")
    install_github("kidzik/pcdpca")

## Running a demo

    library("pcdpca")
    demo("simulation")
    demo("pcdpca.pm10")

## Usage

Let `X` be a multivariate time series, a matrix with `n` observations and `d` covariates, periodic with `period = 2`. Then

    FF = pcdpca(X, period=2)  # finds the optimal filter
    Yhat = pcdpca.scores(X, FF)  # applies the filter
    Yhat[,-1] = 0 # forces the use of only one component
    Xhat = pcdpca.inverse(Yhat, FF)  # deconvolution
    cat(sum((X-Xhat)^2) / sum(X^2)) # variance explained
