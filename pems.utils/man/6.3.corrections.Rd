\name{6.3.corrections}
\alias{6.3.corrections}
\alias{corrections}
\alias{calcPack2}
\alias{correctInput}
\alias{zeroNegatives}
\alias{correctBaseline}


\title{
  Corrections
}

\description{
  Corrections are a special subset of functions which by default 
  write over the elements that they recalculate.
}

\usage{

correctInput(input = NULL, ..., data = NULL,
         correction = NULL)

zeroNegatives(input = NULL, ..., data = NULL, 
         screen = FALSE)

correctBaseline(x, ..., data = NULL, output = "ans")

#associated

calcPack2(input, ..., settings = NULL, data = NULL)

}


\arguments{

  \item{input}{
   (Required data series typically vectors) The input to use when makin a correction. This 
   is typically a vector or element in either a \code{data.frame} or \code{pems} object.
   }

  \item{x}{
   (For \code{correctBaseline}) (Required data series typically vectors) The input to use when makin a correction. This 
   is typically a vector or element in either a \code{data.frame} or \code{pems} object.
  }

  \item{\dots}{
   (Optional) Other arguments, typically passed on. 
   }

  \item{data}{
   (Optional \code{data.frame} or \code{pems} object) The data source if either a \code{data.frame} 
   or \code{pems} object is being used.
   } 

  \item{correction}{
   (For \code{correctInput}, required function) The correction operation to apply to 
   \code{input}. This is typically a function or function name (as character string). 
   }

  \item{screen}{
   (For \code{zeroNegatives}, logical) If the user intends screening the correction before applying 
   it, this should be set to \code{TRUE}.  
   }

  \item{output}{
  (character) Where options exists for the function output, the required output. For 
  \code{correctBaseline}, current options: \code{'ans'} (the default) and 
  \code{'diagnostic'}.
  }

  \item{settings}{
   (For \code{calcPack2}, list) Any arguments to be used as settings when handling 
   \code{pems.elements}. Unless developing functions, this can typically be ignored. 
   }
    
}


\details{

  \code{correctInput} is a general correction handlers. It accepts an 
  \code{input} and a function, \code{correction}, which it applies to 
  \code{input}. 

  \code{zeroNegatives} resets any negative values in an \code{input} to 
  zero.

   \code{correctBaseline} attempts to correct the baseline of a supplied data 
   (\code{pems.element} \code{vector}) time-series. Baseline corrections are 
   carried out using methods from the \code{\link[baseline]{baseline}} package. See Below.

  \code{calcPack2} is an alternative version of \code{\link{calcPack}}. See 
  associated help for details. 
 
}


\value{
  With the exception of \code{calcPack2}, all the above functions generate 
  input corrections. 

   \code{correctBaseline} returns the supplied data time-series (\code{x}) 
   after applying the requested baseline correction (see below).

}

\references{

  baseline:

  Kristian Hovde Liland and Bjorn-Helge Mevik (2015). baseline:
  Baseline Correction of Spectra. R package version 1.2-1.
  https://CRAN.R-project.org/package=baseline

}

\author{
  Karl Ropkins
}

\note{

  By default, corrections return results in the format of the \code{input}. So: If 
  inputs are supplied as vectors, the answer is returned as a vector; If inputs are 
  supplied in a \code{pems} object, that \code{pems} object is returned with the answer 
  added in. This behaviour is enabled by the default \code{output = "special"}. Output 
  type can be forced by declaring \code{ouput} in the function call. Options 
  \code{"input"}, \code{"data.frame"} and \code{"pems"} return vectors, data.frames and 
  pems objects, respectively.

  Unlike other calculations, corrections automatically replace the associated 
  \code{input}, unless prevented (using \code{overwrite = FALSE}).

  This function management is handled by \code{\link{calcChecks}} and 
  \code{\link{calcPack}}. These are front and back end \code{calc...} 
  function add-ins that anyone can use as a 'minimal code' means of 
  integrating third-party and \code{pems.utils} functions.

  See \code{\link{calcChecks}} documentation for details. 

  \code{correctBaseline} is a recent transfer from \code{sleeper.service}. 
  It uses \code{\link[baseline]{baseline}} functions to provide 'best guess' baseline 
  corrections. 

  By default, it applies: 

  \code{baseline(..., method="rollingBall", wm=50, ws=50)} 

  Please Note the 'best guess': As baseline corrections are based statistical 
  estimates of likely baselines rather than actual measures of drift these should 
  be treated as estimates.

  Unit management is by \code{\link{convertUnits}}. See associated help 
  documentation for details.         
 
}

%\section{Warning }{
%  No warnings
%}

\seealso{

  \code{\link[baseline]{baseline}} regarding baseline corrections.

  \code{\link{common.calculations}}, \code{\link{calcVSP}} or \code{\link{calcEm}} for calculation functions.
 
  \code{\link{getElement}} (\code{\link{checkUnits}, etc} 
  and \code{\link{convertUnits}} for data management.

}
\examples{

###########
##example 1 
###########

#basic usage

zeroNegatives(-10:10) #etc



}

\keyword{ methods }
