\name{4.3.time.handlers}
\alias{4.3.time.handlers}
\alias{time.handlers}
\alias{regularize}
\alias{repairLocalTime}

\title{
  pems Data Time Handlers
}

\description{
  Time handlers are subset of \code{pems.utils} functions that 
  work on or with time records (\code{time.stamp} and 
  \code{local.time}).  
}

\usage{

regularize(data, Hz=1, method=1, ...)

repairLocalTime(data, local.time, ref, ..., reset.count = TRUE,
      fun.name = "repairLocalTime")

}


\arguments{

  \item{data}{
   (Required, typically \code{pems}) The dataset to be worked with. 
   For \code{regularize}, the dataset to regularize (see below). 
   }

  \item{Hz}{
   (For \code{regularize}) (Required numeric) The time resolution to 
   regularize the data to in Hertz. So, the default, \code{Hz=1} is 
   one measurement (or row of data) per second.  
   }
   
  \item{method}{
   (For \code{regularize}) (Required numeric) The regularization method 
   to apply. The default, \code{method=1} uses \code{approx} to linearly 
   interpolate regular time-series for all supplied time-series. The 
   alternative \code{method=2} uses a bin-and-average strategy. 
   }

  \item{\dots}{
   (Optional) Other arguments, typically passed on. 
   }
   
   \item{local.time}{
   (For \code{repairLocalTime}) (Required pems.element) The 
   \code{local.time} \code{pems.element} to work been repaired. 
   }
   
   \item{ref, reset.count}{
   (For \code{repairLocalTime}) (Other arguments) \code{ref} is 
   a second source that local.time can be inferred from in cases 
   where \code{local.time} records are missing. If \code{TRUE}, 
   \code{reset.count} resets \code{local.time} so it starts at 0. 
   }
   
   \item{fun.name}{
   (\code{character}) (\code{pems} handler) argument used by 
   \code{pems.utils}. These can typically be ignored. 
   }

}

\details{
  \code{regularize} attempts to extrapolate a regular series, generated at the 
  time resolution requested, from the supplied \code{data}. Both methods can 
  be used for the regularization of irregularly time-logged data, but differ 
  in their data handling. Method \code{1} estimates measurements at regular 
  intervals by linearly interpolating between the last valid point and the next 
  valid point in supplied time-series. It therefore hole-fills gaps in 
  time-series and is perhaps best suited for use with sparser data-sets. It 
  can also be used to interolate time-series to higher time-resolutions, but 
  should not be used aggressively, e.g. to convert 1Hz data data to 10Hz. By 
  contrast, method \code{2} uses data binning to aggregate all supplied 
  measurements (e.g. all measurements between -0.5 and +0.5 seconds of requested 
  times when returning 1 Hz data) and (mean) average these. It is better suited 
  for use with higher resolution time-series (e.g. gong from about 10Hz to 1Hz) 
  and does not hole-fill empty time intervals. If you want mean binning and 
  hole-filling, apply method 2 then method 1, e.g.: 
  
  \code{new.data <- regularize(regularize(my.data, method=2), method=1)}
  
  \code{repairLocalTime} attempts to repair an incomplete \code{local.time} 
  record. For example, if you merge two datasets with overlapping but 
  different time ranges, one may not track the time range of the other and 
  this can generate incomplete time records. This function attempts to 
  hole-fill such cases.

}


\value{
   \code{regularize} returns the supplied dataset (\code{data}) with time-series 
   (\code{time.stamp} and \code{local.time}) are regularized at the 
   requestion time resolution, based on \code{Hz} value. It uses 
   \code{approx} or data binning to estimate associated changes for other 
   data-series.
   
   \code{repairLocalTime} returns a repaired \code{local.time} 
   \code{pem.element}, typically the supplied \code{local.time} with 
   any holes (\code{NAs}) it can fill filled. 

}

\references{

  \code{regularize(..., method=1)} uses \code{approx}:

  Base R Stats package function based on Becker, R. A., Chambers, 
  J. M. and Wilks, A. R. (1988) The New S Language. Wadsworth & 
  Brooks/Cole.  

  A lot of leg-work testing \code{regularize} was done by then Leeds 
  MSc Student Adrian Felipe Ortega Calle.  

}

\author{
  Karl Ropkins
}

\note{
  All time handlers should be used with care.  
}

%\section{Warning }{
%  No warnings
%}

\seealso{
  \code{\link{approx}} regarding data regularization methods.
}

%\examples{
%
%###########
%##example  
%###########
%
%}

\keyword{ methods }
