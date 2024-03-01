\name{semisccs}
\alias{semisccs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Semiparametric self-controlled case series method
%%  ~~function to do ... ~~
}
\description{
The function fits the semiparametric self-controlled case series method where the age effect is left unspecified, as published in Farrington and Whitaker (2006). %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
semisccs(formula, indiv, astart, aend, aevent, adrug, aedrug, expogrp = list(), 
            washout = list(), sameexpopar = list(), dataformat="stack", data)
} 
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{formula}{
model formula. The dependent variable should always be "event" e.g. event ~ itp. There is no need to specify age in the model formula.   %%     ~~Describe \code{formula} here~~
}
 
\item{indiv}{
a vector of individual identifiers of cases. %%     ~~Describe \code{adrug} here~~
}

\item{astart}{
a vector of ages at which the observation periods start.
}


\item{aend}{
a vector of ages at end of observation periods. 
}


\item{aevent}{
a vector of ages at event, an individual can experience multiple events. 
}

\item{adrug}{
a list of vectors of ages at start of exposures or a list of matrices if the exposures have multiple episodes (\code{dataformat} multi). Multiple exposures of the same type can be recorded as multiple rows (\code{dataformat} stack). One list item per exposure type.
}
  \item{aedrug}{
a list of vectors of ages at which exposure-related risk ends or a list of matrices if there are multiple episodes (repeat exposures in different columns) of the same exposure type. The dimension of each item of \code{aedrug} has to be equal to that of \code{adrug}, that is \code{aedrug} should be given for each exposure in \code{adrug}. %%     ~~Describe \code{aedrug} here~~
}

  \item{expogrp}{
list of vectors of days to the start of exposure-related risk, counted from \code{adrug}. E.g if the risk period is [\code{adrug}+c,\code{aedrug}], use expogrp = c. For multiple exposure types \code{expogrp} is a list of length as list \code{adrug}. The DEFAULT is a list of zeros where the exposure-related risk periods are [\code{adrug}, \code{aedrug}].
}
  \item{washout}{
list of vectors with days to start of washout periods counted from \code{aedrug}, the number of vectors in the list is equal to the number of exposure types or the length of \code{adrug}. The default is  NULL, no washout periods. The order of the list items corresponds to the order of exposures in \code{adrug}. %%     ~~Describe \code{washout} here~~
}
  \item{sameexpopar}{
a vector of logical values. If TRUE (the default) no dose effect is assumed: the same exposure parameters are used for multiple doses of the same exposure type, presented in \code{dataformat} 'multi'. If FALSE different relative incidences are estimated for different doses of the same exposure type. The length of the vector is equal to the length list \code{adrug}.
}
 
\item{dataformat}{
the way the input data are assembled. It accepts "multi" or "stack" (the default), where "multi" refers to a data assembled with one row representing one event and "stack" refers to a data frame where repeated exposures of the same type are stack in one column. In the "multi" dataformat different episodes of the same type are recorded as separate columns in the dataframe. 
}

\item{data}{
a data frame containing the input data. The data should be in 'stack' or 'multi' (see \code{dataformat}).
}

}
\details{
In the standard SCCS method both age and exposure effects are modelled using step functions. However, mis-specification of age groups in the standard SCCS may lead to bias in the exposure related relative incidence estimates. In the semiparametric SCCS no age groups are pre-specified. A parameter for each day an event occurred is fitted, which means that this method is only suitable for small to medium sized data sets. An alternative for large data sets is provided by \code{\link{smoothagesccs}}.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
The function returns age and exposure related relative incidence estimates along with 95\% confidence limits.
}
\references{
%% ~put references to the literature/web site here ~
Farrington, C. P., Whitaker, H. J. (2006). Semiparametric analysis of case series data. Applied Statistics, 55(5): 553--594.


Farrington, P., Whitaker, H., and Ghebremichael-Weldeselassie, Y. (2018). Self-controlled Case Series Studies: A modelling Guide with R. Boca Raton: Chapman & Hall/CRC Press.
}
\author{
Yonas Ghebremichael-Weldeselassie, Heather Whitaker, Paddy Farrington.
}
%\note{
%%  ~~further notes~~}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{standardsccs}}, \code{\link{smoothagesccs}}, \code{\link{smoothexposccs}}
}
\examples{

# Example 1
# Semiparametric model for the ITP and MMR vaccine data

itp.mod1 <- semisccs(event~mmr, indiv=case, astart=sta,
                      aend=end, aevent=itp, adrug=mmr, aedrug=mmr+42,
                      expogrp=c(0,15,29), data=itpdat)

itp.mod1


# Example 2
# Data on itp and mmr vaccine
# Sex and mmr interaction included 

itp.mod2 <- semisccs(event~factor(sex)*mmr, indiv=case,
                      astart=sta, aend=end, aevent=itp, adrug=mmr,
                      aedrug=mmr+42, expogrp=c(0,15,29), data=itpdat)


itp.mod2

}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Semi-parametric }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
