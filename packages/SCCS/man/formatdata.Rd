\name{formatdata}
\alias{formatdata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Formatting data
%%  ~~function to do ... ~~
}
\description{
Reformats the data based on age and/or season and exposure groups prior to fitting SCCS model.
}
\usage{
formatdata(indiv, astart, aend, aevent, adrug, aedrug, expogrp = list(),
           washout = list(), sameexpopar = list(), agegrp = NULL, 
           seasongrp=NULL, dob=NULL, cov = cbind(), dataformat="stack", data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{indiv}{
a vector of individual identifiers of cases %%     ~~Describe \code{adrug} here~~
}

\item{astart}{
a vector of ages at which the observation periods start
}

\item{aend}{
a vector of ages at end of observation periods 
}


\item{aevent}{
a vector of ages at event, an individual can experience multiple events
}


\item{adrug}{
a list of vectors of ages at start of exposures or a list of matrices if the exposures have multiple episodes (\code{dataformat} multi). Multiple exposures of the same type can be recorded as multiple rows (\code{dataformat} stack). One list item per exposure type.
}
  \item{aedrug}{
a list of vectors of ages at which exposure-related risk ends or a list of matrices if there are multiple episodes (repeat exposures in different columns) of the same exposure type. The dimension of each item of \code{aedrug} has to be equal to that of \code{adrug}, that is \code{aedrug} should be given for each exposure in \code{adrug}. %%     ~~Describe \code{aedrug} here~~
}

\item{expogrp}{
list of vectors of days to the start of exposure-related risk, counted from \code{adrug}. E.g if the risk period is [\code{adrug}+c,\code{aedrug}], use expogrp = list(c) or expogrp = c. For multiple exposure types \code{expogrp} is a list of vectors equal to the length of the list of \code{adrug}. The DEFAULT is a list of zeros where the exposure-related risk periods are [\code{adrug}, \code{aedrug}].
}

  \item{washout}{
list of vectors with days on start of washout periods counted from \code{aedrug}, the number of vectors in the list is equal to the number of exposures or the length of list of \code{adrug}. The default is  NULL, no washout periods. The order of the list corresponds to the order of exposures in \code{adrug}. %%     ~~Describe \code{washout} here~~
}


  \item{sameexpopar}{
a vector of logical values. If TRUE (the default) no dose effect is assumed, the same  exposure parameters are used for multiple doses/episodes of the same exposure type presented in \code{dataformat} 'multi'. If FALSE different relative incidences are estimated for different doses/episodes of the same exposure. The length of the vector is equal to the length of \code{adrug}. The order in which the elements of the vector are put corrosponds to the order of exposures \code{adrug}.
}
   \item{agegrp}{
a vector of cut points of the age groups where each value represents the start of an age catagory. The first element in the vector is the start of the second age group. The first age group starts at \code{astart}, the start of observation period. The defaults is NULL (i.e no age effects included). %%     ~~Describe \code{agegrp} here~~
}

\item{seasongrp}{
a vector of cut points for seasonal effects. The values should be given in ddmm format, representing the first days of each season group. The seasonal effect is a factor, the reference level being the time interval starting at the earliest date in \code{seasongrp}. The default is NULL where no seasonal effects are included in the model. 
}

\item{dob}{
a vector of birth dates of the cases, in ddmmyyyy format. They are used if seasonal effects are included in the model. The default \code{dob} is NULL but is required if \code{seasongrp} is not NULL.   
}


\item{cov}{
a vector (or a matrix if there are multiple) of fixed covariates. The default is NULL where no covariates are included.
}

\item{dataformat}{
the way the input data are assembled. It accepts "multi" or "stack" (the default), where "multi" refers to a data assembled with one row representing one event and "stack" refers to a data frame where repeated exposures of the same type are stack in one column. In the "multi" dataformat different episodes of the same exposure type are recorded as separate columns in the dataframe. 
}

\item{data}{
a data frame containing the input data. The data should be in 'stack' or 'multi' (see \code{dataformat}).
}

}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}*
\value{
a data frame containing the following columns: 

\item{indivL}{an identfier for each individual event.}
\item{event}{indicator for presence of an event within an interval. "1" where an event occured, "0" otherwise.}
\item{age}{factor for age groups.}
\item{Season}{a factor for season if \code{seasongrp} is specified.}
\item{exposures}{factors for exposure status of each exposure type. "0" for baseline/control periods, "1" for the first risk period. "1" for subsequent exposure risk periods if sameexpopar=TRUE, or increasing factor levels for each subsequent exposure if sameexpopar = FALSE. Indicators for washout periods (if there are any) are also included here. The column names of these factors are the same as the column names of the exposures in \code{adrug}.}
\item{interval}{length of interval. Needed for offsets within the model.}

There are also columns for eventday (day of adverse event), lower (day a period starts), upper (day a period ends), indiv (original individual indentifier), aevent, astart, aend and any covariates included in \code{cov}. 
}
\references{
Whitaker, H. J., Farrington, C. P., Spiessens, B., and Musonda, P. (2006). Tutorial in biostatistics: The self-controlled case series method. Statistics in Medicine 25, 1768--1797.


Farrington P., Whitaker H., and Ghebremichael-Weldeselassie Y. (2018). Self-controlled Case Series Studies: A modelling Guide with R. Boca Raton: Chapman & Hall/CRC Press.

}
\author{
Yonas Ghebremichael-Weldeselassie, Heather Whitaker, Paddy Farrington. 
}
%\note{
%%  ~~further notes~~}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~}
\examples{

# MMR vaccine and ITP data

# A single exposure with three risk periods and no age groups included

 itp.dat1 <- formatdata(indiv=case, astart=sta, aend=end,
                      aevent=itp, adrug=mmr, aedrug=mmr+42,
                      expogrp=c(0,15,29), 
                      data=itpdat)

 itp.dat1

# A single exposure with three risk periods and six age groups

 itp.dat2 <- formatdata(indiv=case, astart=sta, aend=end,
                      aevent=itp, adrug=mmr, aedrug=mmr+42,
                      expogrp=c(0,15,29), agegrp=c(427,488,549,610,671),
                      data=itpdat)
 itp.dat2

}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line


