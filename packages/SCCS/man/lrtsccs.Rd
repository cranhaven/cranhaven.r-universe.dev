\name{lrtsccs}
\alias{lrtsccs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Likelihood ratio test for SCCS models
%%  ~~function to do ... ~~
}
\description{
The function performs the likelihood ratio test for SCCS models that are nested (up to combining of multinomial categories).
}
\usage{
lrtsccs(model1, model2)
} 
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{model1}{
an object fitted by the SCCS method e.g \code{\link{standardsccs}} to be compared with \code{model2}.  %%     ~~Describe \code{adrug} here~~
}
  \item{model2}{
an object fitted by the SCCS method e.g \code{\link{standardsccs}} to be compared with \code{model1}.
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
likelihood ratio test statistic, degrees of freedom and p-value.
}

\references{Farrington P., Whitaker H., and Ghebremichael-Weldeselassie Y. (2018). Self-controlled Case Series Studies: A modelling Guide with R. Boca Raton: Chapman & Hall/CRC Press.}

\author{
Yonas Ghebremichael-Weldeselassie, Heather Whitaker, Paddy Farrington.
}
%\note{
%%  ~~further notes~~}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{

 itp.mod1 <- standardsccs(event~mmr+age, indiv=case, astart=sta,aend=end,
                          aevent=itp, adrug=mmr, aedrug=mmr+42, expogrp=c(0,15,29),
                          agegrp=c(427,488,549,610,671), data=itpdat)
                          
 
 itp.mod2 <- standardsccs(event~age, indiv=case, astart=sta,
                         aend=end, aevent=itp, adrug=mmr, aedrug=mmr+42,
                         expogrp=c(0,15,29), agegrp=c(427,488,549,610,671),
                         data=itpdat)
                         
 itp.mod3 <- standardsccs(event~mmr + age, indiv=case, astart=sta,
                          aend=end, aevent=itp, adrug=mmr, aedrug=mmr+42,
                          agegrp=c(427,488,549,610,671), data=itpdat)
                        

# Compare itp.mod1 a model with both age and exposure (mmr) and itpmod2 a model
# with only age effect 

   lrtsccs(itp.mod1,itp.mod2)
   
# Compare itp.mod1 a model with both age and 3 exposure categories and itpmod3 
# a model with age and only one exposure category  

   lrtsccs(itp.mod3,itp.mod1)  # order of the objects doesn't matter
   
   }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{likelihodd ratio test}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
