\name{standardsccs}
\alias{standardsccs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The standard SCCS method
 %%  ~~function to do ... ~~
}
\description{
Fits the standard SCCS model where age and exposure effects are represented by piecewise constant functions. %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
standardsccs(formula, indiv, astart, aend, aevent, adrug, aedrug,
            expogrp = list(), washout = list(), sameexpopar = list(),
            agegrp = NULL, seasongrp=NULL, dob=NULL, dataformat="stack",
            data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
model formula. The dependent variable should always be "event" e.g. event ~ itp. If age and/or season effects are included, they should always be included as 'age' and 'season', e.g. event ~ itp + age.   %%     ~~Describe \code{formula} here~~
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
a vector of ages at event; an individual can experience multiple events
}

\item{adrug}{
a list of vectors of ages at start of exposures or a list of matrices if the exposures have multiple episodes (\code{dataformat} multi). Multiple exposures of the same type can be recorded as multiple rows (\code{dataformat} stack). One list item per exposure type.
}

  \item{aedrug}{
a list of vectors of ages at which exposure-related risk ends or a list of matrices if there are multiple episodes (repeat exposures in different columns) of the same exposure type. The dimension of each item of \code{aedrug} has to be equal to that of \code{adrug}, that is \code{aedrug} should be given for each exposure in \code{adrug}. %%     ~~Describe \code{aedrug} here~~
}

  \item{expogrp}{
list of vectors of days to the start of exposure-related risk, counted from \code{adrug}. E.g if the risk period is [\code{adrug}+c,\code{aedrug}], use expogrp =  c or expogrp = list(c). For multiple exposure types \code{expogrp} is a list of the same length as list \code{adrug}. The DEFAULT is a list of zeros where the exposure-related risk periods are [\code{adrug}, \code{aedrug}].
}
  \item{washout}{
list of vectors with days to start of washout periods counted from \code{aedrug}, the number of vectors in the list is equal to the number of exposures or the length of list \code{adrug}. The default is  NULL, no washout periods. The order of the list corresponds to the order of exposures in \code{adrug}. %%     ~~Describe \code{washout} here~~
}
    \item{sameexpopar}{
a vector of logical values. If TRUE (the default) no dose effect is assumed: the same exposure parameters are used for multiple doses/episodes of the same exposure type, presented in \code{dataformat} 'multi'. If FALSE different relative incidences are estimated for different doses of the same exposure. The length of the vector is equal to the length of list \code{adrug}. The order of the elements of the vector corrosponds to the order of exposures in list \code{adrug}.
}
  \item{agegrp}{
a vector of cut points of the age groups where each value represents the start of an age category. The first element in the vector is the start of the second age group. The first age group starts at the minimum of \code{astart}, the start of the observation period. The default is NULL (i.e no age effects included). %%     ~~Describe \code{agegrp} here~~
}

\item{seasongrp}{
a vector of cut points for seasonal effects. The values should be given in ddmm format, representing the first days of each season group. The seasonal effect is a factor, the reference level being the time interval starting at the earliest date in \code{seasongrp}. The default is NULL where no seasonal effects are included.
}

\item{dob}{
a vector of birth dates of the cases, in ddmmyyyy format. They are used if seasonal effects are included in the model. The default \code{dob} is NULL. Required if \code{seasongrp} is not NULL.   
}


\item{dataformat}{
the way the input data are assembled. It accepts "multi" or "stack" (the default), where "multi" refers to a data assembled with one row representing one event and "stack" refers to a data frame where repeated exposures of the same type are stack in one column. In the "multi" dataformat different episodes of the same exposure type are recorded as separate columns in the dataframe. 
}

\item{data}{
a data frame containing the input data. The data should be in 'stack' or 'multi' (see \code{dataformat}).
}
}
\details{
% If estimates of interactions of fixed covariates with the exposure effect and/or age effect are required they can be included by modifying the default formula. In the default formula the terms exposure, strata(indivL) and offset(log(interval)) are included, and are essential. Age groups, if cut points are given, are stored in age. Fixed covariates included in data retain their original variable names.

In the standard SCCS model, originally described in Farrington (1995), age and exposure effects are represented by step functions. Suppose that individual \eqn{i} has \eqn{n_i} events, \eqn{n_{ijk}} occurring in age group \eqn{j} and exposure group \eqn{k} and that the \eqn{s^{{th}}} event falls within age group \eqn{j_s} and exposure group \eqn{k_s}. The SCCS likelihood contribution for individual \eqn{i} is

\deqn{l_i = \frac{\prod_{s=1}^{n_i} \exp(\alpha_{j_s} + \beta_{k_s}) }{\Big(\sum_{j=1}^J\sum_{k=1}^K\exp(\alpha_j + \beta_k) e_{ijk}\Big)^{n_i}}.}


This is a multinomial likelihood with index \eqn{n_i}, responses \eqn{n_{ijk}} and probabilities

\deqn{p_{iuv} = \frac{\exp(\alpha_{u} + \beta_{v})e_{iuv} }{\sum_{j=1}^J \sum_{k=1}^K\exp(\alpha_j + \beta_k) e_{ijk}}.}

The standard SCCS likelihood is equivalent to a product multinomial likelihood.

The SCCS likelihood is equivalent to that of the conditional logistic model for \eqn{1:M} matched case-control studies: see Breslow and Day (1980), Chapter 7. This means that SCCS models can be fit using conditional logistic regression, with a factor for each individual event. 

The SCCS R package maximises the likelihood using clogit. Each event is assigned an identifier (indivL), and direct estimation is avoided using the option strata(indivL).

}

\value{
Relative incidence estimates along with their 95\% confidence limits. 
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Farrington, C.P. (1995). Relative incidence estimation from case series for vaccine safety evaluation. Biometrics 51, 228--235.


Breslow, N. E. and  Day, N. E. (1980). Statistical Methods in Cancer Research, volume I: The analysis of case-control studies. IARC Publications No.32.


Farrington, P., Whitaker, H., and Ghebremichael-Weldeselassie, Y. (2018). Self-controlled Case Series studies: A modelling Guide with R. Boca Raton: Chapman & Hall/CRC Press.

}
\author{
Yonas Ghebremichael-Weldeselassie, Heather Whitaker, Paddy Farrington.
}

%\note{
%%  ~~further notes~~}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{semisccs}}
}
\examples{

# Single exposure-related risk period with no age effect

  itp.mod1 <- standardsccs(event~mmr, indiv=case, astart=sta, aend=end,
                        aevent=itp, adrug=mmr, aedrug=mmr+42, data=itpdat)

  itp.mod1

# Single exposure-related risk period and age effect included

  itp.mod2 <- standardsccs(event~mmr+age, indiv=case, astart=sta, aend=end,
                        aevent=itp, adrug=mmr, aedrug=mmr+42,
                        agegrp=c(427,488,549,610,671), data=itpdat)

  itp.mod2

# Multiple risk periods and age effect included  
  
  itp.mod3 <- standardsccs(event~mmr+age, indiv=case, astart=sta, aend=end,
                        aevent=itp, adrug=mmr, aedrug=mmr+42, expogrp=c(0,15,29),
                        agegrp=c(427,488,549,610,671), data=itpdat)

  itp.mod3

# Multiple risk periods, washout periods and age effects

   \donttest{ageq <- floor(quantile(hipdat$frac, seq(0.05,0.95,0.05), 
                         names=FALSE))} # Age group 
                                      # cut points
   
   \donttest{hip.mod1 <- standardsccs(event~ad+age, indiv=case, astart=sta, aend=end,
                           aevent=frac, adrug=ad, aedrug=endad, expogrp=c(0,15,43),
                           washout=c(1,92,182), agegrp=ageq, data=hipdat)}


# Multiple/repeat exposures of the same exposure type, dataformat="stack" 

   \donttest{ageq <- floor(quantile(gidat$bleed[duplicated(gidat$case)==0], 
                seq(0.025,0.975,0.025), names=FALSE))}

   \donttest{gi.mod1 <- standardsccs(event~ns+relevel(age,ref=21), indiv=case, astart=sta,
                         aend=end, aevent=bleed, adrug=ns, aedrug=endns,
                         agegrp=ageq, dataformat="stack", data=gidat)}

   \donttest{gi.mod1}
   
#  Multiple doses of a vaccine each with different parameter estimates (sameexpopar=F)
  
   
   \donttest{ageg <- c(57,85,113,141,169,197,225,253,281,309,337)} # age group cut points
   
   \donttest{dtp.mod2 <- standardsccs(event~dtp+age, indiv=case, astart=sta, aend=end,
                           aevent=conv, adrug=cbind(dtp,dtpd2, dtpd3), 
                           aedrug=cbind(dtp+14,dtpd2+14,dtpd3+14), 
                           expogrp=c(0,4,8),agegrp=ageg, dataformat="multi", 
                           sameexpopar=FALSE, data=dtpdat)}

   \donttest{dtp.mod2}
   
# Multiple exposure types 

  \donttest{ageg <- seq(387,707,20) # Age group cut points}
 \donttest{con.mod <- standardsccs(event~hib+mmr+age, indiv=case, astart=sta, aend=end,
                          aevent=conv, adrug=cbind(hib,mmr), aedrug=cbind(hib+14,mmr+14), 
                          expogrp=list(c(0,8), c(0,8)), agegrp=ageg, data=condat)}

  \donttest{con.mod}

   
# Multiple doses/episodes of several exposure types, the doses of each exposure type
# have same paramter


   ageg <- c(57,85,113,141,169,197,225,253,281,309,337)   # age group cut points 

   hib.mod1 <- standardsccs(event~dtp+hib+age, indiv=case, astart=sta, 
                            aend=end, aevent=conv, 
                           adrug=list(cbind(dtp, dtpd2,dtpd3),
                                       cbind(hib,hibd2,hibd3)),
                            aedrug=list(cbind(dtp+14,dtpd2+14,dtpd3+14),
                                        cbind(hib+14,hibd2+14,hibd3+14)),
                            expogrp=list(c(0,4,8),c(0,8)),agegrp=ageg, 
                            dataformat="multi", data=hibdat)
                            
   hib.mod1                   


# Multiple doses/episodes of several exposure types, the doses of "dtp" 
# different parameters and the doses of the second exposure hib have
# same paramters


   \donttest{ageg <- c(57,85,113,141,169,197,225,253,281,309,337)}
                    # age group cut points 

   \donttest{hib.mod2 <- standardsccs(event~dtp+hib+age, 
                                   indiv=case, astart=sta, aend=end,
                        aevent=conv, adrug=list(cbind(dtp,dtpd2,dtpd3), 
                                      cbind(hib,hibd2,hibd3)), 
                        aedrug=list(cbind(dtp+3,dtpd2+3,dtpd3+3),
                                      cbind(hib+7,hibd2+7,hibd3+7)),
                        sameexpopar=c(FALSE,TRUE), agegrp=ageg, 
                        dataformat="multi", data=hibdat)}
   \donttest{hib.mod2}

   
# Season included in a model 

   month <- c(0101,0102,0103,0104,0105,0106,0107,0108,0109,0110,0111,0112) 
   # season cutpoints 
   
   int.mod <- standardsccs(event~opv+age+season, indiv=case, astart=sta,
                          aend=end, aevent=intus, adrug=cbind(opv,opvd2),
                          aedrug=cbind(opv+42,opvd2+42), expogrp=c(0,15,29),
                          agegrp=seq(30,330,30), seasongrp=month,dob=dob, 
                          dataformat="multi", data=intdat)
   int.mod
}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Parametric}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
