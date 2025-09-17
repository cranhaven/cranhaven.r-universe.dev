\name{plot.netcmc}
\alias{plot.netcmc}

\title{
  A function that plots visual MCMC diagnostics of the fitted model.
}

\description{
  This function takes a netcmc object of samples from the posterior 
distribution of a parameter(s) and returns a visual convergence diaagnostics in 
the form of a density plot, trace plot, and ACF plot.
}

\usage{
  \method{plot}{netcmc}(x, \dots)
}

\arguments{

  \item{x}{
    A netcmc object of samples from the posterior 
    distribution of a parameter(s).
  }
  
  \item{\dots}{
    Ignored.s
  }
  
}

\value{Returns a trace plot, density plot and ACF plot for the posterior 
    distribution of a parameter(s) in a netcmc object.}

\author{
George Gerogiannis
}
