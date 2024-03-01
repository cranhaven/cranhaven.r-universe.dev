\name{simulatesccsdata}
\alias{simulatesccsdata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulation of SCCS data
 %%  ~~function to do ... ~~
}
\description{
This function creates a simulated SCCS data set with given design parameters, and can be used to generate cases with observation and risk periods of different durations, multiple risk periods, repeated exposures, and washout periods. 

}
\usage{
simulatesccsdata(nindivs, astart, aend, adrug, aedrug, expogrp=c(0), eexpo,
                washout=NULL, ewashout=NULL, agegrp=NULL, eage=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{nindivs}{
a positive integer: number of cases to be generated (1 event per case).
}

  \item{astart}{
age at start of an observation period. It is a single number if the same start of observation for all cases is required or a vector of length equal to \code{nindivs} to allow different starts of observation periods for different cases. 
}
  \item{aend}{
 age at end of the observation period. A single number for the same end of observation periods for all cases or a vector to allow for different end of observation periods. 
}

   \item{adrug}{
a vector (of length \code{nindivs}) of  ages at which exposure starts or a matrix if there are multiple exposures. %%     ~~Describe \code{adrug} here~~
}
  \item{aedrug}{
a vector of ages at which exposure-related risk ends or a matrix if there are multiple exposures. The number of columns of \code{aedrug} is equal to the number of columns of \code{adrug}, that is \code{aedrug} should be given for each column in \code{adrug}. %%     ~~Describe \code{aedrug} here~~
}
  \item{expogrp}{
a vectors of days to the start of exposure-related risk, counted from \code{adrug}. E.g if the risk period is [\code{adrug}+c,\code{aedrug}], use expogrp = c. 
}

  \item{eexpo}{
a vector of exposure-related relative incidences.
}
 
% \item{sameexpopar}{
%logical, sets factor levels when multiple exposures are present. If TRUE (the default) no dose effect is assumed, the same parameters %for the exposure-related relative incidence are used after each exposure. If FALSE dose effects are assumed, different parameters for %the exposure-related relative incidence are used after each exposure.\code{eexpo} values should be given accordingly.
%}
 \item{washout}{
a vector of days to start of washout periods counted from \code{aedrug}. The default is  NULL, no washout periods.
}
 
 \item{ewashout}{
a vector of true relative incidence values associated with washout periods; it defaults to NULL when washout=NULL. 
}

  \item{agegrp}{
cut points of age groups, defaults to NULL (i.e no age effect included). These are given as the day of an age category starts, the first age category starts at the minimum of \code{astart}.
}
  \item{eage}{
a vector of age-related relative incidences. The default is NULL where there is no age effect i.e agegrp = NULL. If age-specific relative incidences are from a continuous function \code{eage} is a vector of relative incidences at each age and \code{agegrp}=NULL.
}

}
\details{The true relative incidences related to age and exposure could be generated from discrete or continuous distributions.} 

\value{
A data frame with columns "indiv" = individual identifier, "astart" = age on the day observation period starts, "adrug" = age on  the day exposure starts, "aedrug" = age at the end of exposure related risk period, "aend" = age at the end of observation period, and "aevent" = age on the day of outcome event.
}
\references{Farrington, P., Whitaker, H., and Ghebremichael-Weldeselassie, Y. (2018). Self-controlled Case Series Studies: A modelling Guide with R. Boca Raton: Chapman & Hall/CRC Press.}
\author{
Yonas Ghebremichael-Weldeselassie, Heather Whitaker, Paddy Farrington. %%  ~~who you are~~
}
%\note{
%%  ~~further notes~~}

%% ~Make other sections like Warning with \section{Warning }{....} ~


%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~}
\examples{

# Simulate data where all the cases have same start and end of 
# observation periods and no age effect 

set.seed(4321)

arisk <- round(runif(110,366,730)) # ages at start of exposure

simdata <- simulatesccsdata(nindivs=110, astart=366, aend=730,
                            adrug=arisk, aedrug=arisk+20, eexpo=2.5)
                            

simdata
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line