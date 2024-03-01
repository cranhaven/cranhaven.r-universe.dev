\name{eventdepenobs}
\alias{eventdepenobs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{SCCS with event-dependent observation periods
 %%  ~~function to do ... ~~
}
\description{
One of the assumptions of the self-controlled case series models is that the observation period for each individual is independent of event times. If an event increases the risk of death, such as myocardial infraction or stroke, this assumption is violated. This function fits the modified SCCS model when the assumption is not satisfied i.e ages at end of observation periods might depend on age at event as outlined in Farrington et al (2011). %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eventdepenobs(formula, indiv, astart, aend, aevent, adrug, aedrug, censor,
              expogrp = list(), washout = list(), sameexpopar = list(), 
              agegrp = NULL, dataformat="stack", covariates=NULL, 
              regress=F, initval=rep(0.1, 7), data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
a model formula. The dependent variable should always be "event" e.g. event ~ itp. If age effects are included in the model, the word 'age' must be used in the formula, e.g event ~ itp + age. 
}

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
a vector of ages at event (one event per case)
}

\item{adrug}{
a list of vectors of ages at start of exposures or a list of matrices if the exposures have multiple episodes (\code{dataformat} multi). Multiple exposures of the same type can be recorded as multiple rows (\code{dataformat} stack). One list item per exposure type.
}
  \item{aedrug}{
a list of vectors of ages at which exposure-related risk ends or a list of matrices if there are multiple episodes (repeat exposures in different columns) of the same exposure type. The dimension of each item of \code{aedrug} has to be equal to that of \code{adrug}, that is \code{aedrug} should be given for each exposure in \code{adrug}. %%     ~~Describe \code{aedrug} here~~
}

\item{censor}{
a vector of indicators for whether an observation periods were censored (1 = observation period ended early, 0 = fully observed). 
}

  \item{expogrp}{
list of vectors of days to the start of exposure-related risk, counted from \code{adrug}. E.g if the risk period is [\code{adrug}+c,\code{aedrug}], use expogrp = list(c) or expogrp = c. For multiple exposure types \code{expogrp} is a list of vectors having the same length as list \code{adrug}. The DEFAULT is a list of zeros where the exposure-related risk periods are [\code{adrug}, \code{aedrug}].
}
  \item{washout}{
list of vectors with days on start of washout periods counted from \code{aedrug}, the number of vectors in the list is equal to the number of exposures or the length of list of \code{adrug}. The default is  NULL, no washout periods. The order of the list corresponds to the order of exposures in \code{adrug}. %%     ~~Describe \code{washout} here~~
}

  \item{sameexpopar}{
a vector of logical values. If TRUE (the default) no dose effect is assumed, the same exposure parameters are used for multiple doses/episodes of the same exposure type presented in \code{dataformat} 'multi'. If FALSE different relative incidences are estimated for different doses/episodes of the same exposure type. The length of the vector is equal to the length of the list \code{adrug}.
}
  \item{agegrp}{
a vector of cut points for the age groups where each value represents the start of an age catagory. The first element in the vector is the start of the second age group. The first age group starts at the minimum of \code{astart}, the start of observation period. The defaults is NULL (i.e no age effects included).
}

\item{dataformat}{
the way the input data are assembled. It accepts "multi" or "stack" (the default), where "multi" refers to a data assembled with one row representing one event and "stack" refers to a data frame where repeated exposures of the same exposure type are stacked in one column. In the "multi" dataformat different episodes of the same exposure type are recorded as separate columns in the dataframe. 
}


%  \item{weights}{
%user defined weights to be used as offsets in fitting the SCCS method using clogit, the default is weights=NULL where the weights %are chosen from one of the 4 models (described in Farrington et al (2011)) using AIC.
%  }

  \item{covariates}{
list of covariates believed to affect the age at censoring (age at end of observation period) (e.g. covariates = gender).
}

  \item{regress}{
logical, \code{regress}=T indicates that the parameters of the weight functions are regressed againt age at event or age at start of observation. The default is \code{regress}=F
}

  \item{initval}{
a vector of intial values used in fitting the weight functions. These are given in the order of: 1. Log mean of the exponential component 2. Intercept of the EG/EW log mean function  3. Intercept of the EG/EW log shape function 4. Intercept of the logit mixing probability function 5. Regression parameter of the G/W log mean functions, if regress = T 6. Regression parameter of the G/W log shape function, if regress=T 7. Regression parameter of the G/W logit mixing probability function, if regress=T. When \code{regress}=F only the first 4 are used. The default \code{initval} values are 0.1.

}

  \item{data}{
a data frame containing the input data. The data should be in 'stack' or 'multi' (see \code{dataformat}).
}

}
\details{
This model is suitable when the event increases the risk of death, such as myocardial infraction (MI) or stroke. It is not suitable when the event itself is death. Four models are fitted to the interval between the age at end of observation and the event date, these are detailed in section 5.4 of Farrington et al (2011). The model with the lowest AIC is selected, and used to estimate weights that replace interval lengths in the model formula. This modification allows unbiased estimates of the exposure effect to be estimated, while age effects take on a different interpretation as they include the thinning effect of censoring. 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{summary}{exposure related relative incidence estimates along with their 95\% confidence intervals, age related relative incidence estimates and estimates of interactions with covariates if there are any.}
  \item{modelfit}{model fit of the 4 different weight functions and their AIC values.}
%% ...
}
\references{
Farrington, C. P., Anaya-Izquierdo, A., Whitaker, H. J., Hocine, M.N., Douglas, I., and Smeeth, L. (2011). Self-Controlled case series analysis With event-Dependent observation periods. Journal of the American Statistical Association 106 (494), 417--426.

Farrington P., Whitaker H., and Ghebremichael-Weldeselassie Y. (2018). Self-controlled Case Series Studies: A modelling Guide with R. Boca Raton: Chapman & Hall/CRC Press. 

%% ~put references to the literature/web site here ~
}
\author{
Yonas Ghebremichael-Weldeselassie, Heather Whitaker, Paddy Farrington. %%  ~~who you are~~
}

%\note{
%%  ~~further notes~~}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~}
\examples{
 library(SCCS)
 
 # Nicotine replacement therapy and myocardial infarction (MI)
 # With no age effect included
 

 \donttest{nrt.mod <- eventdepenobs(event~nrt, indiv=case, astart=nrt,
             aend=act, aevent=mi, adrug=nrt, aedrug=nrt+28,
             censor=cen, expogrp=c(0,8,15,22), agegrp=NULL,
             data=nrtdat)}
 
 
 # Respiratory tract infections and MI
 # Age effect included
 # intial values provided and there are two risk periods
 
 \donttest{uni <- (1-duplicated(midat$case))}
 \donttest{ageq <- floor(quantile(midat$mi[uni==1], seq(0.1,0.9,0.1), names=FALSE))}
                   # age groups
 
 \donttest{mi.mod <- eventdepenobs(event~rti+age, indiv=case, astart=sta,
                         aend=end, aevent=mi, adrug=rti, aedrug=rti+14,
                         expogrp=c(0,8), agegrp=ageq, censor=cen, data=midat,
                         initval=rep(1.1,4))}
                         
 \donttest{mi.mod}
 
                         
 }


\keyword{Parametric}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
