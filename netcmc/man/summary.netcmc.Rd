\name{summary.netcmc}
\alias{summary.netcmc}

\title{
  A function that gets a summary of the fitted model.
}

\description{
  This function takes a netcmc object and returns a summary of the fitted model.
The summary includes, for selected parameters, posterior medians and 95 percent 
credible intervals, the effective number of independent samples and the Geweke 
convergence diagnostic in the form of a Z-score.
}

\usage{
  \method{summary}{netcmc}(object, \dots)
}

\arguments{

  \item{object}{
    A netcmc fitted model object.
  }
  
  \item{\dots}{
    Ignored.s
  }
  
}

\value{Returns a model summary for a netcmc object.}

\author{
George Gerogiannis
}
