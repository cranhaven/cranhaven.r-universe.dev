\name{samplesize}
\alias{samplesize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Sample size calculation in SCCS
 %%  ~~function to do ... ~~
}
\description{
The function calculates the sample size required for an SCCS analysis.
}
\usage{
samplesize(eexpo, risk, astart, aend, p, alpha=0.05, power=0.8, eage=NULL,
          agegrp=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eexpo}{
the design value of the exposure related relative incidence.
}
 
\item{risk}{
a positive number showing the duration the risk period. It should not be greater than the duration of the shortest age group, if age groups are specified. 
}

\item{astart}{
age at start of the observation period. It is an integer greater or equal to 0, that is the same start of observation for all cases.
}

\item{aend}{
age at end of an observation period. It is an integer greater than 0, that is the same end of observation for all cases. 
}


\item{p}{
a vector/scalar of proportions of exposed in each age group, the default is p=1 where there are no age effects and all cases are exposed. 
}


\item{alpha}{
level of significance e.g 0.05.
}

\item{power}{
the power required to detect the relative incidence specified in \code{eexpo}, e.g 0.8.
}
  \item{eage}{
age related relative incidence parameters, the reference group is the first one.  
}
 
  \item{agegrp}{
a vector of cut points of the age groups where each value represents the start of an age category. The first element in the vector is the start of the second age group. The first age group starts at the minimum of \code{astart}, the start of observation period. The default is NULL (i.e no age effects included). %%     ~~Describe \code{agegrp} here~~
}

}

%\details{

%put more details here ........

%}

\value{
a sample size is produced.
}
\references{

Musonda, P., Farrington, C. P., and Whitaker, H. (2006). Sample sizes for self-controlled case series studies. Statistics in Medicine 25, 2618--2631. 

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
%\code{\link{semisccs}}\code{\link{standardsccs}}
%}
\examples{

# Sample size for exposure RI = 2.5 with 21 days risk period, 
# all cases exposed. The level of significance is 0.05 
# with 80\% power. The sample size of when p=1 is: 

   ss1 <- samplesize(eexpo=2.5, risk=21, astart=366, aend=730,
                 p=1, alpha=0.05, power=0.8)

   ss1


# the sample size of events from cases exposed or not when 75\% of the 
# population are exposed

   ss2 <- samplesize(eexpo=2.5, risk=21, astart=366, aend=730,
                 p=0.75, alpha=0.05, power=0.8)


   ss2

# Sample size when age effect is included and the proportions of the  
# target exposed population which are exposed in each age group
# are p=c(0.50,0.35,0.1,0.05): 

   ss3 <- samplesize(eexpo=2.5, risk=21, astart=366, aend=730,
                 p=c(0.50,0.35,0.1,0.05), alpha=0.05, power=0.8,
                 eage=c(1.2,1.6,2.0), agegrp=c(457,548,639))

   ss3
 
# Suppose that the sample is from the entire population with 75\% exposed,
# then p=0.75*c(0.50,0.35,0.1,0.05)  

  ss4 <- samplesize(eexpo=2.5, risk=21, astart=366, aend=730,
                 p=0.75*c(0.50,0.35,0.1,0.05), alpha=0.05, power=0.8,
                 eage=c(1.2,1.6,2.0), agegrp=c(457,548,639)) 
  ss4 
 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Sample size}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
