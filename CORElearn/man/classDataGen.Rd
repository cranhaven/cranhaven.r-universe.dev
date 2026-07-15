\name{classDataGen}
\alias{classDataGen}
\title{Artificial data for testing classification algorithms}
\description{
  The generator produces classification data with 2 classes, 7 discrete and 3 numeric attributes.
}
\usage{
  classDataGen(noInst, t1=0.7, t2=0.9, t3=0.34, t4=0.32, 
               p1=0.5, classNoise=0)
}
\arguments{
  \item{noInst}{Number of instances to generate.}
  \item{t1, t2, t3}{ Parameters, which control the hardness of the discrete attributes.}
  \item{t4}{ Parameter, which controls the hardness of the numeric attributes..}
  \item{p1}{ Probability of class 1.}
  \item{classNoise}{Proportion of noise in the class variable for classification or virtual class variable for regression.}   
}
\value{
The method \code{classDataGen} returns a \code{\link{data.frame}} with \code{noInst} rows and 11 columns. 
Range of values of the attributes and class are
  \item{a1}{ 0,1}
  \item{a2}{ 0,1}
  \item{a3}{ a,b,c,d}
  \item{a4}{ 0,1}
  \item{a5}{ 0,1}
  \item{a6}{ a,b,c,d}
  \item{a7}{ a,b,c,d}
  \item{x1}{ numeric}
  \item{x2}{ numeric}
  \item{x3}{ numeric}
  \item{class}{ 1,2 }
For detailed specification of attributes (columns) see details section below.
}
\details{
Class probabilities are \code{p1} and \code{1 - p1}, respectively. The conditional distribution of attributes
under each of the classes depends on parameters \code{t1, t2, t3, t4} from [0,1].
Attributes a7 and x3 are irrelevant for all values of parameters.

Examples of extreme settings of the parameters.
\itemize{
  \item Setting satisfying t1*t2 = t3 implies no difference between the distributions
        of individual discrete attributes among the two classes. However, if t1 < 1, then
        the joint distribution of them is different for the two classes.
  \item Setting t1 = 1 and t2 = t3 implies no difference between the joint distribution
        of the discrete attributes among the two classes.
  \item Setting t1 = 1, t2 = 1, t3 = 0 implies disjoint supports of the distributions
       of a1, a2, a4, a5, so this allows exact classification.
  \item Setting t4 = 1 implies no difference between the distribution of x1, x2 between
       the classes. Setting t4 = 0 allows correct classification with probability one
       only using x1 and x2.
}
For class 1 the attributes have distributions
\tabular{ll}{
(a1, a2, a3) \tab  \eqn{D_1(t1, t2)} \cr
a4, a5, a6   \tab  \eqn{D_2(t3)} \cr
a7           \tab  irrelevant attribute, probabilities of \{a,b,c,d\} are (1/2, 1/6, 1/6, 1/6) \cr
x1, x2, x3   \tab  independent normal variables with mean 0 and standard  deviation 1, t4, 1 \cr
x4, x5      \tab independent uniformly distributed variables on [0,1] 
}
For class 2 the attributes have distributions
\tabular{ll}{
a1, a2, a3   \tab \eqn{D_2(t3)} \cr
(a4, a5, a6) \tab  \eqn{D_1(t1, t2)} \cr
a7           \tab irrelevant attribute, probabilities of \{a,b,c,d\} are (1/2, 1/6, 1/6, 1/6) \cr
x1, x2, x3   \tab independent normal variables with mean 0 and st. dev. t4, 1, 1 \cr
x4, x5       \tab independent uniformly distributed variables on [0,1] 
}
x3 is irrelevant for classification, since it has the same distribution under both classes.

Attributes in a bracket are mutually dependent. Otherwise, the attributes
are conditionally independent for each of the two classes. This means
that if we consider groups of the attributes such that the attributes in each
of the two brackets form a group and each of the remaining attributes forms a
group with one element, then for each class, we have 7 groups, which are
conditionally independent for the given class. Note that the splitting into
groups differs for class 1 and 2.

Distribution \eqn{D_1(t1,t2)} consists of three dependent attributes. The
distribution of individual attributes depends only on t1*t2. For a given t1*t2,
the level of dependence decreases with t1 and increases with t2. There are
two extreme settings:
Setting t1 = 1, t2 = t1*t2 has the largest t1 and the smallest t2 and all three
  attributes are independent.
Setting t1 = t1*t2, t2 = 1 has the smallest t1 and the largest t2 and also the
  largest dependence between attributes.

Distribution \eqn{D_2(t3)} is equal to \eqn{D_1(1, t3)}, so it contains three independent
attributes, whose distributions are the same as in \eqn{D_1(t1,t2)} for every
setting satifying t1*t2 = t3.

In other words, if t3 = t1*t2, then the distributions \eqn{D_1(t1, t2)} and \eqn{D_2(t3)}
have the same distributions of individual attributes and may differ only
in the dependences. There are no in \eqn{D_2(t3)} and there are some in \eqn{D_1(t1, t2)}
if t1 < 1.

\emph{Hardness of the discrete part}

Setting t1 = 1 and t2 = t3 implies no difference between the discrete
attributes among the two classes.

Setting satisfying t1*t2 = t3 implies no difference between the distributions
of individual discrete attributes among the two classes. However, there may
be a difference in dependences.

Setting t1 = 1, t2 = 1, t3 = 0 implies disjoint supports of the distributions
of a1, a2, a4, a5, so this allows exact classification.

\emph{Hardness of the continuous part}

Depends monotonically on t4. Setting t4 = 1 implies no difference between the
classes. Setting t4 = 0 allows correct classification with probability one.

}
\author{ Petr Savicky }

\seealso{
\code{\link{regDataGen}}, \code{\link{ordDataGen}},\code{\link{CoreModel}}.
}

\examples{
#prepare a classification data set
classData <-classDataGen(noInst=200)

# build random forests model with certain parameters
modelRF <- CoreModel(class~., classData, model="rf",
              selectionEstimator="MDL", minNodeWeightRF=5,
              rfNoTrees=100, maxThreads=1)
print(modelRF)
destroyModels(modelRF) # clean up
}


\keyword{ data }
\keyword{datagen}
