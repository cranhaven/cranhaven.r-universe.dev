\name{OSTE}
\alias{OSTE}
\title{Optimal Survival Tree Ensemble
}
\description{
Optimal survival trees ensemble is the main function of \code{OSTE} package that grows a sufficiently large number, \code{t.initial}, of  survival trees and selects optimal survival trees from the total trees grown by random survival forest. Number of survival trees in the initial set, \code{t.initial}, is chosen by the user. If not chosen, then the default \code{t.initial = 500} is used. Based on empirical investigation, \code{t.initial =1000} is recommended.
}
\usage{
OSTE(formula = NULL, data, t.initial = NULL, v.size = NULL, mtry = NULL, M = NULL,
minimum.node.size = NULL, always.split.features = NULL, replace = TRUE,
splitting.rule = NULL, info = TRUE)
}
\arguments{
  \item{formula}{ Object of class formula describing the required model to be fitted. Interaction terms are not supported in the current version.
}
  \item{data}{A  \code{ nxd } matrix or data frame of \code{n} observations on \code{d} features along with response variables that are described by the \code{formula}.
}
  \item{t.initial}{ Number of survival trees to be grown initially. If equal to \code{NULL} then the defalut of \code{t.initial = 500} is taken. A recommended value is \code{t.initial = 1000}.
}
  \item{v.size}{Portion of data used for validation in the second phase i.e. for assessing survival trees performance in the ensemble. If equal to \code{NULL} then the defalut \code{v.size=0.1}
}
  \item{mtry}{Number of features selected at random at each node of the survival trees for splitting. If equal to \code{NULL} then the default \code{sqrt(d)} is taken.
}
  \item{M}{Percent of the best \code{t.initial} survival trees to be selected on the basis of their performance on out-of-bag observations. For selecting 20\% of trees, take \code{M=0.2}.
}
  \item{minimum.node.size}{Minimal node size. If equal to \code{NULL} then the default \code{minimum.node.size = 3} is executed.
}
  \item{always.split.features}{Vector of variable names if desired to be always selected in addition to the mtry variables tried for splitting.
}
  \item{replace}{Whether sampling should be done with or without replacement.
}
  \item{splitting.rule}{Splitting rule."\code{logrank}", "\code{C}" or "\code{maxstat}" are suported with default "\code{logrank}".
}
  \item{info}{If \code{TRUE}, displays process status .
}
}
\details{Large values are recommended for \code{t.initial} for better performance as possible under the available computational resources. The log-rank test statistic is used as defalut,
A C-index based splitting rule (Schmid et al. 2015) and maximally selected rank statistics  (Wright et al. 2016) are available. The C-index shows better predictive performance in case of high censoring rate, where logrank is best for situations where the data are noisy (Schmid et al. 2015).
}
\value{
\item{unique.death.times}{ Unique death times.}
\item{CHF}{ Estimated cumulative hazard function for each observation.}
\item{Survival_Prob}{ Estimated survival probability for each observation.}
\item{trees_selected}{ Number of trees selected.}
\item{mtry}{ Value of mtry used.}
\item{forest}{ Saved forest for prediction purposes.}
}
\references{ Marvin N. Wright, Andreas Ziegler (2017). ranger: A Fast Implementation of Random
  Forests for High Dimensional Data in C++ and R. Journal of Statistical Software,
  77(1), 1-17. doi:10.18637/jss.v077.i01


 Terry Therneau, Beth Atkinson and Brian Ripley (2015) rpart: Recursive Partitioning
  and Regression Trees. R package version 4.1-10.
  https://CRAN.R-project.org/package=rpart

Ulla B. Mogensen, Hemant Ishwaran, Thomas A. Gerds (2012). Evaluating Random Forests
for Survival Analysis Using Prediction Error Curves. Journal of Statistical Software,
50(11), 1-23. URL http://www.jstatsoft.org/v50/i11/.

 Schmid, M., Wright, M. N. & Ziegler, A. (2016). On the use of Harrell's C for clinical risk
prediction via random survival forests. Expert Syst Appl 63:450-459.
http://dx.doi.org/10.1016/j.eswa.2016.07.018.


 Wright,  M. N.,  Dankowski,  T. & Ziegler,  A. (2017). Unbiased split variable selection for random survival forests using maximally selected rank statistics. Stat Med.
 http://dx.doi.org/10.1002/sim.7212.

Zardad Khan, Asma Gul, Aris Perperoglou, Osama Mahmoud, Werner Adler, Miftahuddin and
  Berthold Lausen (2015). OTE: Optimal Trees Ensembles for Regression, Classification
  and Class Membership Probability Estimation. R package version 1.0.
  https://CRAN.R-project.org/package=OTE

  Gul, N., Faiz, N., Brawn, D., Kulakowski, R., Khan, Z., & Lausen, B. (2020). Optimal survival trees ensemble. arXiv preprint arXiv:2005.09043.
}
\author{
Naz Gul, Nosheen Faiz, Zardad Khan and Berthold Lausen.
}
\note{ In the case of missing values in any dataset prior action needs to be taken as the fuction can not handle them at the current version. Moreover, the status/delta variable in the data must be code as 0, 1.
}
\seealso{
\code{\link{VETERAN}}
}
\examples{

#Load the data
data(VETERAN)
library(survival)
library(prodlim)
library(ranger)
library(pec)
#Divide the data into training and test parts



 predictSurvProb.ranger <- function (object, newdata, times, ...) {

    ptemp <- ranger:::predict.ranger(object, data = newdata, importance = "none")$survival
    pos <- sindex(jump.times = object$unique.death.times,
                           eval.times = times)
    p <- cbind(1, ptemp)[, pos + 1, drop = FALSE]
    if (NROW(p) != NROW(newdata) || NCOL(p) != length(times))
      stop(paste("\nPrediction matrix has wrong dimensions:\nRequested newdata x times: ",
                 NROW(dts[trainind,]), " x ", length(1), "\nProvided prediction matrix: ",
                 NROW(p), " x ", NCOL(p), "\n\n", sep = ""))
    p
  }

n <- nrow(VETERAN)
trainind <- sample(1:n,n*0.7)
testind <- (1:n)[-trainind]

# Grow OSTE on the training data

OSTE.fit <- OSTE(Surv(time,status)~.,data=VETERAN[trainind,],t.initial=100)

# Predict on the test data

pred <- ranger:::predict.ranger(OSTE.fit$forest,data=VETERAN[testind,])

# Index various values

pred$survival
pred$survival

#etc.

# To calculate IBS
# Create formula
frm <- as.formula(Surv(time, status) ~ trt + celltype + karno + diagtime + age + prior)

PredError <- pec(object=OSTE.fit$forest, exact==TRUE,
                   formula = frm, cens.model="marginal",
                   data=VETERAN[testind,], verbose=F)
IBS <- crps(object = PredError, times =100, start = PredError$start)[2,1]
IBS
}
\keyword{ OSTE }
\keyword{ Optimal }
\keyword{ Survival }
\keyword{ Trees }
\keyword{ Ensemble }
