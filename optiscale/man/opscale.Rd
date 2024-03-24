\name{opscale}
\alias{opscale}

\title{
   Optimal scaling of a data vector
}

\description{
   This function produces an object of class "opscale", containing a 
   vector that is a least-squares approximation
   to a vector of quantitative values, subject to measurement constraints
   based upon a vector of qualitative data values.
}
\usage{
opscale(x.qual, x.quant = seq(1:length(x.qual)), level = 1, 
   process = 1, na.impute = FALSE, 
   na.assign = TRUE, rescale = TRUE)
}

\arguments{
  \item{x.qual}{
   A vector of data values, assumed to be qualitative.
}
  \item{x.quant}{
   A vector of quantitative values.
}
  \item{level}{
   Measurement level of \code{x.qual}. 1=nominal, 2=ordinal.
}
  \item{process}{
   Measurement process of \code{x.qual}. 1=discrete, 2=continuous.
}
  \item{na.impute}{
   If \code{TRUE}, then assign \code{x.quant} values to optimally 
   scaled vector for missing entries in \code{x.qual}.
   of \code{FALSE} then assign \code{NA} to entries in optimally scaled
   vector corresponding to missing entries in \code{x.qual}. 
}
  \item{na.assign}{
   If \code{TRUE}, then if \code{x.quant} is missing, assign
   mean of optimally scaled values for the corresponding
   \code{x.qual} value to the optimally scaled vector.
   If \code{FALSE}, then leave optimally
   scaled value missing, even if \code{x.qual} value is present.
}
  \item{rescale}{
   If \code{TRUE} then rescale optimally scaled vector 
   to same mean and standard deviation as \code{x.qual}.
}
}
\details{
   The \code{opscale()} function operationalizes a measurement theory
   proposed by Young (1981) in order to facilitate an analysis strategy
   called \dQuote{Alternating Least Squares, Optimal Scaling}.
   The optimal scaling transformation produces a vector (say, \code{OS})
   that is a least-squares approximation to \code{x.quant}, subject to
   measurement constraints based upon \code{x.qual}. 
   If \code{x.qual} is nominal level, then the values in \code{OS} are
   the conditional means of \code{x.quant}, within distinct categories
   of \code{x.qual}. If \code{x.qual} is ordinal level, then the values in 
   \code{OS} are the conditional means of \code{x.quant}, adjusted to be 
   weakly monotonic to the values in \code{x.qual}, using Kruskals (1964b)
   monotonic transformation.
   If \code{x.qual} is discrete, then all data objects sharing a common value
   in \code{x.qual} must be assigned the same value in \code{OS}.
   If \code{x.qual} is continuous, then data objects sharing a common value
   in \code{x.qual} can fall within a closed interval of values in \code{OS}.
   The continuous-discrete measurement process distinction corresponds to 
   Kruskals (1964a) primary and secondary approaches to ties.
}
\value{
   The \code{opscale()} function returns an object of class "opscale"
   containing a list with the following components:
      \item{qual}{The qualitative data vector, \code{x.qual}}
      \item{quant }{The vector of quantitative values, \code{x.quant}}
      \item{os}{The vector of optimally scaled values}
      \item{varname}{The name of the qualitative data vector, \code{x.qual}}
      \item{measlev}{The measurement level of the qualitative data vector specified
         in the \code{level} argument to \code{opscale}} 
      \item{measproc}{The measurement process of the qualitative data vector specified
         in the \code{process} argument to \code{opscale}} 
      \item{rescale}{Value of the \code{rescale} argument to \code{opscale}}
      \item{os.raw.mean}{Mean of optimally scaled values before rescaling}
      \item{os.raw.sd}{Standard deviation of optimally scaled values before rescaling}

}

\references{
   Kruskal, Joseph B. (1964a) \dQuote{Multidimensional Scaling by Optimizing
      Goodness of Fit to a Nonmetric Hypothesis.} \emph{Psychometrika} 29: 1-27.
      
   Kruskal, Joseph B. (1964b) \dQuote{Nonmetric Multidimensional Scaling:
      A Numerical Method.} \emph{Psychometrika} 29: 115-129.
      
   Young, Forrest W. (1981) \dQuote{Quantitative Analysis of
      Qualitative Data.} \emph{Psychometrika} 46: 357-388.
}

\seealso{
   \code{\link{plot.opscale}, \link{print.opscale},
   \link{summary.opscale}}
}

\examples{
  ###   x1 is vector of qualitative data
  ###   x2 is vector of quantitative values
            x1 <- c(1,1,1,1,2,2,2,3,3,3,3,3,3)     
            x2 <- c(3,2,2,2,1,2,3,4,5,2,6,6,4)     
  ###   Optimal scaling, specifying that x1
  ###   is ordinal-discrete
     op.scaled <- opscale(x.qual=x1, x.quant=x2,   
                  level=2, process=1)              
     print(op.scaled)
     summary(op.scaled)
}
