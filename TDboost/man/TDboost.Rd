\name{TDboost}
\alias{TDboost}
\alias{TDboost.more}
\alias{TDboost.fit}
\title{TDboost Tweedie Regression Modeling}
\description{Fits TDboost Tweedie Regression models.}
\usage{
TDboost(formula = formula(data),
    distribution = list(name="EDM",alpha=1.5),
    data = list(),
    weights,
    var.monotone = NULL,
    n.trees = 100,
    interaction.depth = 1,
    n.minobsinnode = 10,
    shrinkage = 0.001,
    bag.fraction = 0.5,
    train.fraction = 1.0,
    cv.folds=0,
    keep.data = TRUE,
    verbose = TRUE)

TDboost.fit(x,y,
        offset = NULL,
        misc = NULL,
        distribution = list(name="EDM",alpha=1.5),
        w = NULL,
        var.monotone = NULL,
        n.trees = 100,
        interaction.depth = 1,
        n.minobsinnode = 10,
        shrinkage = 0.001,
        bag.fraction = 0.5,
        train.fraction = 1.0,
        keep.data = TRUE,
        verbose = TRUE,
        var.names = NULL,
        response.name = NULL)

TDboost.more(object,
         n.new.trees = 100,
         data = NULL,
         weights = NULL,
         offset = NULL,
         verbose = NULL)
}
\arguments{
\item{formula}{a symbolic description of the model to be fit. The formula may 
   include an offset term (e.g. y~offset(n)+x). If \code{keep.data=FALSE} in 
   the initial call to \code{TDboost} then it is the user's responsibility to 
   resupply the offset to \code{\link{TDboost.more}}.}
\item{distribution}{a list with a component \code{name} specifying the distribution 
   and any additional parameters needed. Tweedie regression is available and \code{distribution} must a list of the form 
   \code{list(name="EDM",alpha=1.5)} where \code{alpha} is the index parameter that must be in (1,2]. 
When \code{alpha=2}, the distribution reduces to gamma.
  The current version's Tweedie regression methods do 
   not handle non-constant weights and will stop.}
\item{data}{an optional data frame containing the variables in the model. By
   default the variables are taken from \code{environment(formula)}, typically 
   the environment from which \code{TDboost} is called. If \code{keep.data=TRUE} in 
   the initial call to \code{TDboost} then \code{TDboost} stores a copy with the 
   object. If \code{keep.data=FALSE} then subsequent calls to 
   \code{\link{TDboost.more}} must resupply the same dataset. It becomes the user's 
   responsibility to resupply the same data at this point.}
\item{weights}{an optional vector of weights to be used in the fitting process. 
   Must be positive but do not need to be normalized. If \code{keep.data=FALSE} 
   in the initial call to \code{TDboost} then it is the user's responsibility to 
   resupply the weights to \code{\link{TDboost.more}}.}
\item{var.monotone}{an optional vector, the same length as the number of
predictors, indicating which variables have a monotone increasing (+1),
decreasing (-1), or arbitrary (0) relationship with the outcome.}
\item{n.trees}{the total number of trees to fit. This is equivalent to the
number of iterations and the number of basis functions in the additive
expansion.}
\item{cv.folds}{Number of cross-validation folds to perform. If \code{cv.folds}>1 then
\code{TDboost}, in addition to the usual fit, will perform a cross-validation, calculate
an estimate of generalization error returned in \code{cv.error}.}
\item{interaction.depth}{The maximum depth of variable interactions. 1 implies
an additive model, 2 implies a model with up to 2-way interactions, etc.}
\item{n.minobsinnode}{minimum number of observations in the trees terminal
nodes. Note that this is the actual number of observations not the total
weight.}
\item{shrinkage}{a shrinkage parameter applied to each tree in the expansion.
Also known as the learning rate or step-size reduction.}
\item{bag.fraction}{the fraction of the training set observations randomly
selected to propose the next tree in the expansion. This introduces randomnesses
into the model fit. If \code{bag.fraction}<1 then running the same model twice
will result in similar but different fits. \code{TDboost} uses the R random number
generator so \code{set.seed} can ensure that the model can be
reconstructed. Preferably, the user can save the returned
\code{\link{TDboost.object}} using \code{\link{save}}.}
\item{train.fraction}{The first \code{train.fraction * nrows(data)}
observations are used to fit the \code{TDboost} and the remainder are used for
computing out-of-sample estimates of the loss function.}
\item{keep.data}{a logical variable indicating whether to keep the data and
an index of the data stored with the object. Keeping the data and index makes
subsequent calls to \code{\link{TDboost.more}} faster at the cost of storing an
extra copy of the dataset.}
\item{object}{a \code{TDboost} object created from an initial call to
\code{\link{TDboost}}.}
\item{n.new.trees}{the number of additional trees to add to \code{object}.}
\item{verbose}{If TRUE, TDboost will print out progress and performance indicators.
If this option is left unspecified for TDboost.more then it uses \code{verbose} from
\code{object}.}

\item{x, y}{For \code{TDboost.fit}: \code{x} is a data frame or data matrix containing the
predictor variables and \code{y} is the vector of outcomes. The number of rows
in \code{x} must be the same as the length of \code{y}.}
\item{offset}{a vector of values for the offset}
\item{misc}{For \code{TDboost.fit}: \code{misc} is an R object that is simply passed on to
the TDboost engine.}
\item{w}{For \code{TDboost.fit}: \code{w} is a vector of weights of the same
length as the \code{y}.}
\item{var.names}{For \code{TDboost.fit}: A vector of strings of length equal to the
number of columns of \code{x} containing the names of the predictor variables.}
\item{response.name}{For \code{TDboost.fit}: A character string label for the response
variable.}

}
\details{
This package implements a regression tree based gradient boosting estimator for nonparametric multiple Tweedie regression. The code is a modified version of \code{gbm} library originally written by Greg Ridgeway.

Boosting is the process of iteratively adding basis functions in a greedy
fashion so that each additional basis function further reduces the selected
loss function. This implementation closely follows Friedman's Gradient
Boosting Machine (Friedman, 2001).

In addition to many of the features documented in the Gradient Boosting Machine,
\code{TDboost} offers additional features including the out-of-bag estimator for
the optimal number of iterations, the ability to store and manipulate the
resulting \code{TDboost} object.

\code{TDboost.fit} provides the link between R and the C++ TDboost engine. \code{TDboost}
is a front-end to \code{TDboost.fit} that uses the familiar R modeling formulas.
However, \code{\link[stats]{model.frame}} is very slow if there are many
predictor variables. For power-users with many variables use \code{TDboost.fit}.
For general practice \code{TDboost} is preferable.

}
\value{
\code{TDboost}, \code{TDboost.fit}, and \code{TDboost.more} return a
\code{\link{TDboost.object}}.
}
\references{
Yang, Y., Qian, W. and Zou, H. (2013), \dQuote{A Boosted Tweedie Compound Poisson Model for Insurance Premium} Preprint.


G. Ridgeway (1999). \dQuote{The state of boosting,} \emph{Computing Science and
Statistics} 31:172-181.

J.H. Friedman (2001). \dQuote{Greedy Function Approximation: A Gradient Boosting
Machine,} \emph{Annals of Statistics} 29(5):1189-1232.

J.H. Friedman (2002). \dQuote{Stochastic Gradient Boosting,} \emph{Computational Statistics
and Data Analysis} 38(4):367-378.

}

\author{Yi Yang \email{yi.yang6@mcgill.ca}, Wei Qian \email{wxqsma@rit.edu} and Hui Zou \email{hzou@stat.umn.edu}}


\seealso{
\code{\link{TDboost.object}},
\code{\link{TDboost.perf}},
\code{\link{plot.TDboost}},
\code{\link{predict.TDboost}},
\code{\link{summary.TDboost}},
}
\examples{
data(FHT)
# training on data1
TDboost1 <- TDboost(Y~X1+X2+X3+X4+X5+X6,         # formula
    data=data1,                   # dataset
    var.monotone=c(0,0,0,0,0,0), # -1: monotone decrease,
                                 # +1: monotone increase,
                                 #  0: no monotone restrictions
    distribution=list(name="EDM",alpha=1.5),
                                 # specify Tweedie index parameter
    n.trees=3000,                # number of trees
    shrinkage=0.005,             # shrinkage or learning rate,
                                 # 0.001 to 0.1 usually work
    interaction.depth=3,         # 1: additive model, 2: two-way interactions, etc.
    bag.fraction = 0.5,          # subsampling fraction, 0.5 is probably best
    train.fraction = 0.5,        # fraction of data for training,
                                 # first train.fraction*N used for training
    n.minobsinnode = 10,         # minimum total weight needed in each node
    cv.folds = 5,                # do 5-fold cross-validation
    keep.data=TRUE,              # keep a copy of the dataset with the object
    verbose=TRUE)                # print out progress

# print out the optimal iteration number M
best.iter <- TDboost.perf(TDboost1,method="test")
print(best.iter)

# check performance using 5-fold cross-validation
best.iter <- TDboost.perf(TDboost1,method="cv")
print(best.iter)

# plot the performance
# plot variable influence
summary(TDboost1,n.trees=1)         # based on the first tree
summary(TDboost1,n.trees=best.iter) # based on the estimated best number of trees

# making prediction on data2
f.predict <- predict.TDboost(TDboost1,data2,best.iter)

# least squares error
print(sum((data2$Y-f.predict)^2))

# create marginal plots
# plot variable X1 after "best" iterations
plot.TDboost(TDboost1,1,best.iter)
# contour plot of variables 1 and 3 after "best" iterations
plot.TDboost(TDboost1,c(1,3),best.iter)

# do another 20 iterations
TDboost2 <- TDboost.more(TDboost1,20,
                 verbose=FALSE) # stop printing detailed progress

# fit a gamma model (when alpha = 2.0)
data2 <- data1[data1$Y!=0,]
TDboost3 <- TDboost(Y~X1+X2+X3+X4+X5+X6,         # formula
    data=data2,                   # dataset
    distribution=list(name="EDM",alpha=2.0),
    n.trees=3000, 				 # number of trees
    train.fraction = 0.5,        # fraction of data for training,
    verbose=TRUE)                # print out progress
best.iter2 <- TDboost.perf(TDboost3,method="test")

}
\keyword{models}
\keyword{nonlinear}
\keyword{survival}
\keyword{nonparametric}
\keyword{tree}
