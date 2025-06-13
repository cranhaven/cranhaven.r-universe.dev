\name{hmd.DA.mx}
\alias{hmd.DA.mx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Model Life Table Discriminant Analysis
}
\description{
Training and testing steps to perform a discriminant analysis for a partial or complete human mortality rate schedule with the Human Mortality Database as the training set. 
}
\usage{
hmd.DA.mx(data, age, sex="female")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Partial or complete human mortality rate schedule(s) to be classified. The set of age-specific mortality rates to be classified must match up with one of the age groups from the HMD data (standard 5-year age intervals 0, 1-4, 5-9, 10-14...110+) and be in the original scale (non-logged), i.e. one could classify mortality rates for ages 0, 1-5, 10-14, 20-24, 80-84, but cannot classify mortality rates for ages 0-5, 20-29, 30-39, etc. This argument can be either a single schedule or multiple schedules, but either way \code{data} must be a matrix with ages in columns.  
}
  \item{age}{The ages of the mortality rates to be classified. This argument tells the function which mortality rates from the HMD should be used for the training models. \code{age} is a vector of length equal to the number of columns in \code{data} with the starting ages of the intervals to be classified. (e.g. if the user wants to classify mortality rates from ages 20-24, 25-29, 30-34, 35-39, 50-54, enter \code{age=c(seq(20,35,5),50)}).}
  \item{sex}{
"male" or "female". The sex for the schedule to be classified.
}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{train}{The output from the function \code{\link{MclustDA}}, the parameters and other summary information for the model best fitting each class according to BIC.}
  \item{out.dens}{The output from the function \code{\link{predict}(MclustDA model name)}, A matrix in which the [i,j]th entry is the density for observation i in the model for class j.}
  \item{classification}{The classification for the test data}
}
\references{
S. Clark and D. Sharrow (2011) "Contemporary Model Life Tables for Developed Countries: An Application of Model-Based Clustering." Paper presented at the annual meeting of the Population Association of America. \url{http://paa2011.princeton.edu/abstracts/111808}

C. Fraley and A. E. Raftery (2002) "Model-based clustering, discriminant analysis, and density estimation." \emph{Journal of the American Statistical Association} \bold{97}:611-631.

C. Fraley, A. E. Raftery, T. B. Murphy, L. Scrucca (2012) "mclust Version 4 for R: Normal Mixture Modeling for Model-Based Clustering, Classification, and Density Estimation", Technical Report no. 597, Department of Statistics, University of Washington

\emph{Human Mortality Database} University of California, Berkeley (USA), and Max Planck Institute for Demographic Research (Germany). Available at www.mortality.org or www.humanmortality.de (data downloaded February  2015).
}

\author{
David J. Sharrow
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{MclustDA}}, \code{\link{Mclust}}, \code{\link{MLTobs}}
}
\examples{
# some test data
data(MLTobs)

##48 Belgium 1860-64 (known class = 1)
##180 England 1925-29 (known class = 2)
##207 Estonia 2005-09 (known class = 7)
##266 France 1960-64 (known class = 3)
##410 Japan 2000-04 (known class = 5)
##607 Russia 1980-84 (known class = 6)
##798 USA 2000-04 (known class = 4)

country.nums <- c(48,180,207,266,410,607,798)
test.mx <- t(flt.mx[3:10,country.nums]) # mortality rates for ages 5-40
test.age <- seq(5,40,5)

# classify the test data matrix
examp.out <- hmd.DA.mx(data=test.mx, age=test.age, sex="female")
examp.out$classification

# classify the test data single schedule as matrix
examp.out2 <- hmd.DA.mx(data=t(as.matrix(test.mx[4,])), age=test.age, sex="female")
examp.out2$classification


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ cluster }
\keyword{ misc }% __ONLY ONE__ keyword per line
