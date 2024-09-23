\name{a) bioPN package}
\alias{bioPN-package}
\docType{package}
\title{Simulation of deterministic and stochastic biochemical reaction networks using Petri Nets}
\description{
  bioPN is a package of C functions that can be used to simulate
  time-dependent evolution of biochemical reaction networks. The model
  is defined as a place/transition Petri Net, which is close to how
  biochemical reactions are defined. The model can be either
  deterministically solved using an explicit Runge Kutta Dormand Prince
  45 method, simulated using four highly optimized variants of the
  stochastic simulation algorithm, or as a deterministic/stochastic
  hybrid, according to the Haseltine and Rawlings' algorithm, or using
  the Partitioned Leaping Algorithm. The library has been optimized for
  speed and flexibility.

  bioPN has been tested only on 64 bits machines, relying on integers of
  64 bits. The behavior on 32 bits architectures is untested and not supported.
}
\details{
\tabular{ll}{
Package: \tab bioPN\cr
Type: \tab Package\cr
Version: \tab 1.2.0\cr
Date: \tab 2014-03-04\cr
License: \tab GPL (>=2)\cr
}
}
\author{
Roberto Bertolusso and Marek Kimmel

Maintainer: Roberto Bertolusso <rbertolusso@rice.edu>
}
\references{
The biological example presented in the functions is extracted from:
Paszek, P. (2007) Modeling stochasticity in gene regulation:
characterization in the terms of the underlying distribution function,
Bull Math Biol, 69, 1567-1601.
}
