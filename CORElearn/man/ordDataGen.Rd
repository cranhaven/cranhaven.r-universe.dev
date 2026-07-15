\name{ordDataGen}
\alias{ordDataGen}
\title{Artificial data for testing ordEval algorithms}
\description{
  The generator produces ordinal data simulating different profiles of attributes:
  basic, performance, excitement and irrelevant.
  }
\usage{
  ordDataGen(noInst, classNoise=0)
}
\arguments{
  \item{noInst}{Number of instances to generate.}
  \item{classNoise}{Proportion of randomly determined values in the class variable.}   
}
\details{
Problem is
described by six important and two
irrelevant features. The important features correspond to different
feature types from the marketing theory:  two basic features
(\eqn{B_{weak}} and \eqn{B_{strong}}), two performance features (\eqn{P_{weak}}
and \eqn{P_{strong}}), two excitement features (\eqn{E_{weak}} and
\eqn{E_{strong}}), and two irrelevant features (\eqn{I_{uniform}} and
\eqn{I_{normal}}). The values of all features are randomly generated
integer values from 1 to 5, indicating for example score assigned to
each of the features by the survey's respondent. The dependent
variable for each instance (class) is the sum of its features'
effects, which we scale to the uniform distribution of integers 1-5,
indicating, for example, an overall score assigned by the respondent.
\deqn{%
 C=b_w(B_{weak})+b_s(B_{strong})+p_w(P_{weak})+p_s(P_{strong})+e_w(E_{weak})+e_s(E_{strong})%
 }
}
\value{
The method returns a \code{\link{data.frame}} with \code{noInst} rows and 9 columns. 
Range of values of the attributes and class are integers in [1,5]
}
\author{ Marko Robnik-Sikonja }

\seealso{
\code{\link{classDataGen}},
\code{\link{regDataGen}},
\code{\link{ordEval}},
}

\examples{
#prepare a data set
dat <- ordDataGen(200)

# evaluate ordered features with ordEval
est <- ordEval(class ~ ., dat, ordEvalNoRandomNormalizers=100)
# print(est)  
plot(est)
}

\keyword{ data }
\keyword{datagen}
