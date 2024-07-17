\name{daIns}
\alias{daIns}
\docType{data}
\title{Liability Insurance Coverage for Hunters and Anglers in Mississippi}
\description{This data set contains a survey result about liability insurance purchase decision by hunters and anglers in Mississippi. There are 1653 observations for 14 variables.
\tabular{ll}{
\code{Y       } \tab Binary dependent variable = 1 if had liability insurance; 0 otherwise \cr
\code{Injury  } \tab Times of bodily injuries or property damages in the past three years \cr
\code{HuntYrs } \tab Years of hunting \cr
\code{Nonres  } \tab Dummy = 1 if nonresidents; 0 if Mississippi residents \cr
\code{Lspman  } \tab Dummy = 1 if purchased the license of resident sportsman; 0 otherwise \cr
\code{Lnong   } \tab Dummy = 1 if purchased the license of nonresident all game; 0 otherwise \cr
\code{Gender  } \tab Dummy = 1 if male; 0 otherwise \cr
\code{Age     } \tab Age of the hunter or angler \cr
\code{Race    } \tab Dummy = 1 if Caucasian; 0 otherwise \cr
\code{Marital } \tab Dummy = 1 if married; 0 otherwise \cr
\code{Edu     } \tab Years of education \cr
\code{Inc     } \tab Household income in 2004 (1,000 dollars) \cr
\code{TownPop } \tab Population size of the residence town (1,000) \cr
\code{FishYrs } \tab Years of fishing \cr
}
}
\usage{data(daIns)}
\format{A cross sectional data with 1653 observations and 14 variables.}
\details{The data set is from a telephone survey conducted in 2005 in Mississippi.}
\source{
Sun, C., S. Pokharel, W.D. Jones, S.C. Grado, and D.L. Grebner. 2007. Extent of recreational incidents and determinants of liability insurance coverage for hunters and anglers in Mississippi. Southern Journal of Applied Forestry 31(3):151-158.
}

\seealso{\code{\link{daInsNam}}}

\examples{
data(daIns)
class(daIns); dim(daIns)
head(daIns); tail(daIns)

ra <- glm(formula = Y ~ Injury + HuntYrs + Nonres +
                    Lspman + Lnong + Gender + Age +
                    Race + Marital + Edu + Inc + TownPop,
          family = binomial(link="logit"),
          data = daIns, x = TRUE, y= TRUE)
names(ra); summary(ra)

(ins.me <- maBina(w = ra))
(ins.mt <- maTrend(q=ins.me, nam.c="Age", nam.d="Nonres"))
plot(ins.mt)
}
\keyword{datasets}