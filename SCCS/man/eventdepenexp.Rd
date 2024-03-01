\name{eventdepenexp}
\alias{eventdepenexp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{SCCS with event-dependent exposure
 %%  ~~function to do ... ~~
}
\description{
One of the assumptions of the self-controlled case series model is that occurence of an event does not affect subsequent exposure. This function fits the modified SCCS model when the assumption is not satisfied, see Farrington et al (2009). This modified method assumes that no exposure is possible following a unique event. It requires that exposure is of a fixed finite length and that the end of observation that would have applied in the absence of the event is known.
}
\usage{
eventdepenexp(indiv, astart, aend, aevent, adrug, aedrug, expogrp=0,
             sameexpopar=T,agegrp=NULL, dataformat="stack", verbose=F,
             tolerance=1e-8,itermax=100, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 % \item{formula}{
%Model formula. The dependent variale should always be "event" e.g. event ~ itp + age, where itp is the exposure and age represent age effect. The age effect effects should always be put as age.   %%     ~~Describe \code{formula} here~~
%}
 
\item{indiv}{
a vector of individual identifiers of cases %%     ~~Describe \code{adrug} here~~
}

\item{astart}{
a vector of ages at which the observation periods start 
}


\item{aend}{
a vector of ages at end of observation periods, that would have applied in the absence of the event 
}


\item{aevent}{
a vector of ages at event (one event per case)
}

\item{adrug}{
a vector of ages at which exposure starts or a matrix if there are multiple episodes of the same exposure type (\code{dataformat} multi). Multiple exposures of the same exposure type can be recorded as multiple rows (\code{dataformat} stack). In this method only one exposure type can be included unlike the \code{standardsccs} where multiple exposure types can be analayzed in the same model %%     ~~Describe \code{adrug} here~~
}
  \item{aedrug}{
a vector of ages at which exposure-related risk ends or a matrix if there are multiple episodes of the same exposure type. The dimension of \code{aedrug} should be equal to the dimension of \code{adrug}, that is \code{aedrug} should be given for each column in \code{adrug} %%     ~~Describe \code{aedrug} here~~
}
  \item{expogrp}{
a vector of days to the start of exposure-related risk, counted from \code{adrug}. E.g if the risk period is [\code{adrug}+c,\code{aedrug}], use expogrp = list(c) or expogrp = c. The DEFAULT is a \code{expogrp}= 0 where the exposure-related risk period is [\code{adrug}, \code{aedrug}].
}

%To define multiple risk windows, \code{expogrp} is a vector of days on start of risk periods counted from \code{adrug}.

%  \item{washout}{
%list of vectors with days on start of washout periods counted from \code{aedrug}, the number of vectors in the list is equal to the number of exposures or the length of list of \code{adrug}. The default is  NULL, no washout periods. %%     ~~Describe \code{washout} here~~
%}
  \item{sameexpopar}{
a logical value. If TRUE (the default) no dose effect is assumed: the same exposure parameters are used for multiple doses/episodes of the same exposure type presented in \code{dataformat} 'multi'. If FALSE different relative incidences are estimated for different doses/episodes of the same exposure.
}
  \item{agegrp}{
a vector of cut points for the age groups where each value represents the start of an age catagory. The first element in the vector is the start of the second age group. The first age group starts at the minimum of \code{astart}, the start of observation period. The default is NULL (i.e no age effects included). %%     ~~Describe \code{agegrp} here~~
}

%\item{seasongrp}{
%cut points for seasonal effects. the values should be given in numbers as ddmm format, representing the first days of each season group. The seasonal effect is a factor, the reference level being the time interval starting at the earliest date in \code{seasongrp}.The default is NULL where no seasonal effects are considered in the model. 
%}

%\item{dob}{
%a vector of dates of births of the cases. \code{dob} have to be given as numbers in ddmmyyyy format. They are used if seasonal effects are included in the model. The default is NULL but has to be given if \code{seasongrp} is not NULL.   
%}

\item{dataformat}{
the way the input data are assembled. It accepts "multi" or "stack" (the default), where "multi" refers to a data assembled with one row representing one event and "stack" refers to a data frame where repeated exposures of the same type are stacked in one column. In the "multi" \code{dataformat} different episodes of the same exposure type are recorded as separate columns in the dataframe. 
}


\item{verbose}{
a logical value indicating whether information about the iterations should be printed. Default is FALSE

}

\item{tolerance}{
the convergence tolerance when estimating the parameters. Defaults to 1e-8.

}

\item{itermax}{
maximum number of iterations. 100 is the default.
}


\item{data}{
a data frame containing the input data. The data should be in 'stack' or 'multi' (see \code{dataformat}).
}
}
\details{
This model fits a SCCS model with event-dependent exposures. % more details will be added.

}

\value{
Relative incidence estimates along with their 95\% confidence intervals. 
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{

Farrington, C. P., Whitaker H.J., and Hocine M.N. (2009). Case series analysis for censored, perturbed or curtailed post-event exposures. Biotatistics, 10(1), 3-16.

Farrington P., Whitaker H., and Ghebremichael-Weldeselassie Y. (2018). Self-controlled Case Series Studies: A modelling Guide with R. Boca Raton: Chapman & Hall/CRC Press. 

%% ~put references to the literature/web site here ~
}
\author{
Yonas Ghebremichael-Weldeselassie, Heather Whitaker, Paddy Farrington. %%  ~~who you are~~
}

%\note{
%%  ~~further notes~~}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{semisccs}}, \code{\link{standardsccs}}
}
\examples{
library(SCCS)

# Analysis of rotavirus vaccination and intussusception data
# Model 1: Three doses of the same vaccine exposure OPV (OPV, OPV2 and opv3), 
# only one risk period [adrug, aedrug]


   rot.mod1 <- eventdepenexp(indiv=case, astart=sta, aend=end,
                         aevent=intus, adrug=cbind(rv,rvd2), 
                         expogrp=1,aedrug=cbind(rv+21,rvd2+21), 
                         agegrp=seq(56,168,14), dataformat="multi", data=rotdat)

   rot.mod1

# Model 2: Two doses with two riks periods, 1-7 and 8-21 


   rot.mod2 <- eventdepenexp(indiv=case, astart=sta, aend=end,
                            aevent=intus, adrug=cbind(rv,rvd2),
                            aedrug=cbind(rv+21,rvd2+21), expogrp=c(1,8),
                            agegrp=seq(56,168,14), dataformat="multi", 
                            data=rotdat)   

   rot.mod2

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Event dependent exposures}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
