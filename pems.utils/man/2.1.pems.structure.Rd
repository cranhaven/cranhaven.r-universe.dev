\name{2.1.pems.structure}
\alias{2.1.pems.structure}
\alias{pems.structure}
\alias{getPEMSElement}
\alias{getPEMSData}
\alias{getPEMSConstants}
\alias{pemsData}
\alias{pemsConstants}
\alias{pemsHistory}
\alias{pemsin}
\alias{pemsin2}
\alias{cpe}


\title{
  'pems' object structure
}

\description{
  This pages provides a brief outview description of 
  the 'pems' object structure. It also lists some 
  associated functions
}

\usage{

getPEMSElement(x, pems = NULL, units = NULL, ..., 
            fun.name="getPEMSElement", 
            if.missing = "stop", if.null = if.missing, 
            track.name = TRUE, .x = enquo(x))

getPEMSData(pems=NULL, ..., fun.name = "getPEMSData", 
            if.missing = "stop", .pems = enquo(pems))

getPEMSConstants(pems=NULL, ..., 
            fun.name = "getPEMSConstants", 
            if.missing = "stop", .pems = enquo(pems))

pemsData(pems=NULL, ..., 
          fun.name = "pemsData", if.missing = "stop",
          pems.name = deparse(substitute(pems)))

pemsConstants(pems=NULL, ..., 
          fun.name = "pemsConstants", if.missing = "stop",
          pems.name = deparse(substitute(pems)))

pemsHistory(pems=NULL, ..., 
         fun.name = "pemsHistory", if.missing = "stop",
         pems.name = deparse(substitute(pems)))
         
cpe(...)

}



\arguments{

  \item{x}{
  (Required vector, typically pems.element) For 
  \code{getPEMSElement}, the required data element.
}

  \item{pems}{
  (pems object) If supplied, the \code{pems} object 
  to search for \code{x} before checking the parent 
  environments and R workspace.
}

  \item{units}{
   (Optional) The units that \code{x} should be supplied 
   in (handled by \code{\link{convertUnits}}). 
}
  \item{\dots}{
  (Optional) Other Arguments.
}

  \item{fun.name, if.missing, if.null, track.name, 
  pems.name, .x, .pems}{
  (Various) Other options using for \code{pems.utils} 
  house-keeping. See \code{\link{check...}} for 
  definitions, although generally these can be ignored
  by users. See Note below.  
} 
    
}


\details{

  The \code{pems} object is a managed \code{data.frame}. 
  It has five main components: \code{data}, 
  \code{units}, \code{constants}, \code{history} and 
  \code{tags}. \code{data} is the main 
  \code{data.frame}. Each element (named 
  \code{data.frame} column) is a data-series of the 
  original PEMS data. \code{units} are the associated 
  unit definitions. \code{constants} is a list of 
  associated constants that are to be used with the 
  \code{pems} object. (The preference order is arguments 
  given in a call then \code{constants} declared in the 
  \code{pems} object then \code{constant} defaults held 
  by the \code{pems.utils} package.) \code{history} is a 
  log of \code{pems} object modifications. \code{tags} 
  are any other components that the user wishes to add 
  to a \code{pems} object as identifiers.

  \code{getPEMSElement} gets a requested 
  \code{pem.element} from \code{pems} if supplied or 
  from the local workspace. 

  \code{pemsData} and \code{getPEMSData} get the 
  data component of a supplied \code{pems} object.

  \code{pemsConstants} and \code{getPEMSConstants} get 
  all constants locally defined for the supplied 
  \code{pems} object. 

  \code{pemsHistory} gets the history of supplied 
  \code{pems} object.
  
  \code{cpe} combines \code{pems.elements}. It is 
  intended as an alternative to 
  \code{c(pems.element, ...)} while 
  that generic is in-development. 
}


\value{

  \code{getPEMSElement} returns the requested element 
  of a supplied \code{pems} object as a managed vector or 
  \code{pems.element}, if available. If missing, error 
  handling is by \code{checkIfMissing}. See 
  \code{\link{check...}} for more details.) 

  \code{pemsData} returns the data component of a 
  supplied \code{pems} object as a \code{data.frame}. 

  \code{getPEMSData} returns the data component of 
  a supplied \code{pems} object as a \code{data.frame}. 

  \code{pemsConstants} returns the constants component 
  of a supplied \code{pems} object as a \code{list}. 

  \code{getPEMSConstants} returns the constants 
  component of a supplied \code{pems} object as a 
  \code{list}. 

  \code{pemsHistory} returns the history component of 
  a supplied \code{pems} object as a \code{list}.
  
  \code{cpe} turns the concatenated form of supplied 
  input. 

}

\references{
  \code{rlang} and \code{dplyr} package functions now 
  do the heavy lifting for \code{getPEMSElement}.

  Lionel Henry and Hadley Wickham (2018). rlang: 
  Functions for Base Types and Core R and 
  'Tidyverse' Features. R package version 0.2.0.
  https://CRAN.R-project.org/package=rlang

  Hadley Wickham, Romain Francois, Lionel Henry and 
  Kirill Muller (2017). dplyr: A Grammar of Data 
  Manipulation. R package version
  0.7.4. https://CRAN.R-project.org/package=dplyr

}
\author{
  Karl Ropkins
}

\note{

  \code{pems...} functions are in development \code{pems} 
  object handlers. They are intended for 
  convenient 'front of house' use. As part of this role, 
  their structure will evolve over time, 
  so arguments and operations may change based on user 
  feedback. Those wishing to develop 
  future-proof third party functions should also 
  consider \code{check...} functions when developing 
  their code. See \code{\link{common.calculations}} 
  for some Examples.      
 
  \code{getPEMS...} functions are a revision of earlier 
  \code{pems...} \code{pems} object handlers. 
  They are intended to replace \code{pems...} code in 
  future package versions.       

  \code{rlang} and \code{dplyr} functions now do 
  the heavy lifting for \code{getPEMSElement}.

}

\seealso{
  See Also: \code{\link{check...}} for 
  \code{check...} function equivalents; 
  \code{\link{pems.generics}} for \code{pems} object 
  generic functions.
}

\examples{

###########
##example 1 
###########

#basic usage

#using example data pems.1
#(supplied as part of pems.utils package)

#pems structure
pems.1

# extracting the pems.1 element velocity
getPEMSElement(velocity, pems.1)

\dontrun{
#generic (SE) equivalents
pems.1$velocity
pems.1["velocity"] 
}

}
\keyword{ methods }
