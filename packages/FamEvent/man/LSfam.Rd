\name{LSfam}
\alias{LSfam}
\docType{data}
\title{ Ontario Lynch Syndrom families
%%   ~~ data name/kind ... ~~
}
\description{
Data from 32 Lynch Syndrome families segregating mismatch repair mutations selected from the Ontario Familial Colorectal Cancer Registry that includes 765 individuals, both probands and relatives. The families were ascertained throughout affected and mutation carrier probands.

}
\usage{data("LSfam")}
\format{
  A data frame with 765 observations on the following 11 variables.
  \describe{
    \item{\code{famID}}{Family identification (ID) numbers.}
    \item{\code{indID}}{Individuals ID numbers.}
    \item{\code{fatherID}}{Father ID numbers.}
    \item{\code{motherID}}{Mother ID numbers.}
    \item{\code{gender}}{Gender indicators: 1 for male, 0 for female.}
    \item{\code{status}}{Disease statuses: 1 for affected, 0 for unaffected.}
    \item{\code{time}}{Ages at diagnosis of colorectal cancer for the affected or ages of last follow-up for the unaffected.}
    \item{\code{currentage}}{Current ages in years.}
    \item{\code{mgene}}{MLH1 or MSH2 mutation indicators: 1 for mutated gene carriers, 0 for mutated gene noncarriers, or \code{NA} if missing.}
    \item{\code{proband}}{Proband indicators: 1 for proband, 0 for non-proband.}
    \item{\code{relation}}{Family members' relationship with the proband.
  \tabular{ll}{
    1  \tab Proband (self) \cr
    2  \tab Brother or sister \cr
    3  \tab Son or daughter \cr
    4  \tab Parent \cr
    5  \tab Nephew or niece \cr
    6  \tab Spouse \cr
    7  \tab Brother or sister in law \cr
    8  \tab Paternal grandparent \cr
    9  \tab Paternal uncle or aunt \cr
    10  \tab Paternal cousin \cr
    11  \tab Maternal grandparent \cr
    12  \tab Maternal uncle or aunt \cr
    13  \tab Maternal cousin  \cr
    14  \tab Son or daughter in law \cr
    15  \tab Grandchild \cr
    16  \tab Uncle's or aunt's spouse. \cr
  }

    
    }
  }
}

\references{
%%  ~~ possibly secondary sources and usages ~~
Choi, Y.-H., Cotterchio, M., McKeown-Eyssen, G., Neerav, M., Bapat, B., Boyd, K., Gallinger, S.,  McLaughlin, J., Aronson, M., and Briollais, L. (2009). Penetrance of Colorectal Cancer among MLH1/ MSH2 Carriers Participating in the Colorectal Cancer Familial Registry in Ontario, Hereditary Cancer in Clinical Practice, 7:14.
}
\examples{

data(LSfam)

# Summary of LSfam
summary.simfam(LSfam)

# Pedigree plot for the first family
plot.simfam(LSfam)

# Assign minimum age for fitting penmodel
attr(LSfam, "agemin") <- 18

fit <- penmodelEM(Surv(time, status) ~ gender + mgene, cluster = "famID", 
       parms = c(0.05, 2, 1, 3), data = LSfam[!is.na(LSfam$time) & LSfam$time > 18, ], 
       method = "mendelian", base.dist = "Weibull", design = "pop+", robust = TRUE)

summary(fit)

penetrance(fit, fixed = c(1, 1), age = c(50, 60, 70), CI = TRUE, MC = 100)

}
\keyword{datasets}
