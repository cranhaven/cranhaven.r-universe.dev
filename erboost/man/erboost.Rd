\name{erboost}
\alias{erboost}
\alias{erboost.more}
\alias{erboost.fit}
\title{ER-Boost Expectile Regression Modeling}
\description{Fits ER-Boost Expectile Regression models.}
\usage{
erboost(formula = formula(data),
    distribution = list(name="expectile",alpha=0.5),
    data = list(),
    weights,
    var.monotone = NULL,
    n.trees = 3000,
    interaction.depth = 3,
    n.minobsinnode = 10,
    shrinkage = 0.001,
    bag.fraction = 0.5,
    train.fraction = 1.0,
    cv.folds=0,
    keep.data = TRUE,
    verbose = TRUE)

erboost.fit(x,y,
        offset = NULL,
        misc = NULL,
        distribution = list(name="expectile",alpha=0.5),
        w = NULL,
        var.monotone = NULL,
        n.trees = 3000,
        interaction.depth = 3,
        n.minobsinnode = 10,
        shrinkage = 0.001,
        bag.fraction = 0.5,
        train.fraction = 1.0,
        keep.data = TRUE,
        verbose = TRUE,
        var.names = NULL,
        response.name = NULL)

erboost.more(object,
         n.new.trees = 3000,
         data = NULL,
         weights = NULL,
         offset = NULL,
         verbose = NULL)
}
\arguments{
\item{formula}{a symbolic description of the model to be fit. The formula may 
   include an offset term (e.g. y~offset(n)+x). If \code{keep.data=FALSE} in 
   the initial call to \code{erboost} then it is the user's responsibility to 
   resupply the offset to \code{\link{erboost.more}}.}
\item{distribution}{a list with a component \code{name} specifying the distribution 
   and any additional parameters needed. Expectile regression is available and \code{distribution} must a list of the form 
   \code{list(name="expectile",alpha=0.25)} where \code{alpha} is the expectile 
   to estimate. The current version's expectile regression methods do 
   not handle non-constant weights and will stop.}
\item{data}{an optional data frame containing the variables in the model. By
   default the variables are taken from \code{environment(formula)}, typically 
   the environment from which \code{erboost} is called. If \code{keep.data=TRUE} in 
   the initial call to \code{erboost} then \code{erboost} stores a copy with the 
   object. If \code{keep.data=FALSE} then subsequent calls to 
   \code{\link{erboost.more}} must resupply the same dataset. It becomes the user's 
   responsibility to resupply the same data at this point.}
\item{weights}{an optional vector of weights to be used in the fitting process. 
   Must be positive but do not need to be normalized. If \code{keep.data=FALSE} 
   in the initial call to \code{erboost} then it is the user's responsibility to 
   resupply the weights to \code{\link{erboost.more}}.}
\item{var.monotone}{an optional vector, the same length as the number of
predictors, indicating which variables have a monotone increasing (+1),
decreasing (-1), or arbitrary (0) relationship with the outcome.}
\item{n.trees}{the total number of trees to fit. This is equivalent to the
number of iterations and the number of basis functions in the additive
expansion. The default number is 3000. \bold{Users should not always use the default value, but choose 
the appropriate value of \code{n.trees} based on their data.} Please see "details" section below.}
\item{cv.folds}{Number of cross-validation folds to perform. If \code{cv.folds}>1 then
\code{erboost}, in addition to the usual fit, will perform a cross-validation, calculate
an estimate of generalization error returned in \code{cv.error}.}
\item{interaction.depth}{The maximum depth of variable interactions. 1 implies
an additive model, 2 implies a model with up to 2-way interactions, etc. 
The default value is 3. \bold{Users should not always use the default value, but choose 
the appropriate value of \code{interaction.depth} based on their data.}
 Please see "details" section below.}
\item{n.minobsinnode}{minimum number of observations in the trees terminal
nodes. Note that this is the actual number of observations not the total
weight.}
\item{shrinkage}{a shrinkage parameter applied to each tree in the expansion.
Also known as the learning rate or step-size reduction.}
\item{bag.fraction}{the fraction of the training set observations randomly
selected to propose the next tree in the expansion. This introduces randomnesses
into the model fit. If \code{bag.fraction}<1 then running the same model twice
will result in similar but different fits. \code{erboost} uses the R random number
generator so \code{set.seed} can ensure that the model can be
reconstructed. Preferably, the user can save the returned
\code{\link{erboost.object}} using \code{\link{save}}.}
\item{train.fraction}{The first \code{train.fraction * nrows(data)}
observations are used to fit the \code{erboost} and the remainder are used for
computing out-of-sample estimates of the loss function.}
\item{keep.data}{a logical variable indicating whether to keep the data and
an index of the data stored with the object. Keeping the data and index makes
subsequent calls to \code{\link{erboost.more}} faster at the cost of storing an
extra copy of the dataset.}
\item{object}{a \code{erboost} object created from an initial call to
\code{\link{erboost}}.}
\item{n.new.trees}{the number of additional trees to add to \code{object}. The default number is 3000.}
\item{verbose}{If TRUE, erboost will print out progress and performance indicators.
If this option is left unspecified for erboost.more then it uses \code{verbose} from
\code{object}.}

\item{x, y}{For \code{erboost.fit}: \code{x} is a data frame or data matrix containing the
predictor variables and \code{y} is the vector of outcomes. The number of rows
in \code{x} must be the same as the length of \code{y}.}
\item{offset}{a vector of values for the offset}
\item{misc}{For \code{erboost.fit}: \code{misc} is an R object that is simply passed on to
the erboost engine.}
\item{w}{For \code{erboost.fit}: \code{w} is a vector of weights of the same
length as the \code{y}.}
\item{var.names}{For \code{erboost.fit}: A vector of strings of length equal to the
number of columns of \code{x} containing the names of the predictor variables.}
\item{response.name}{For \code{erboost.fit}: A character string label for the response
variable.}

}
\details{
Expectile regression (Newey & Powell 1987) is a nice tool for estimating the conditional expectiles of a response variable given a set of covariates. This package implements a regression tree based gradient boosting estimator for nonparametric multiple expectile regression. The code is a modified version of \code{gbm} library (\url{https://cran.r-project.org/package=gbm}) originally written by Greg Ridgeway.

Boosting is the process of iteratively adding basis functions in a greedy
fashion so that each additional basis function further reduces the selected
loss function. This implementation closely follows Friedman's Gradient
Boosting Machine (Friedman, 2001).

In addition to many of the features documented in the Gradient Boosting Machine,
\code{erboost} offers additional features including the out-of-bag estimator for
the optimal number of iterations, the ability to store and manipulate the
resulting \code{erboost} object.

Concerning tuning parameters, \bold{\code{interaction.depth}} and \bold{\code{n.trees}} are two of the most important tuning parameters in \pkg{erboost}. \bold{Users should not always use the default values of those two parameters, instead they should choose the appropriate values of \code{interaction.depth} and \code{n.trees} according to their data.} For example, if \code{n.trees}, which is the maximal number of trees to fit, is set to be too small, then it is possible that the actual optimal number of trees (which is \code{best.iter} selected by the function \code{erboost.perf} in "example" section) for a particular data  exceeds this number, resulting a sub-optimal model. \bold{Therefore, users should always fit the model with a large enough \code{n.trees} such that \code{n.trees} is greater than the potential optimal number of trees. The same principle also applies on \code{interaction.depth}}.

\code{erboost.fit} provides the link between R and the C++ erboost engine. \code{erboost}
is a front-end to \code{erboost.fit} that uses the familiar R modeling formulas.
However, \code{\link[stats]{model.frame}} is very slow if there are many
predictor variables. For power-users with many variables use \code{erboost.fit}.
For general practice \code{erboost} is preferable.

}
\value{
\code{erboost}, \code{erboost.fit}, and \code{erboost.more} return a
\code{\link{erboost.object}}.
}
\references{
Yang, Y. and Zou, H. (2015), \dQuote{Nonparametric Multiple Expectile Regression via ER-Boost,} \emph{Journal of Statistical Computation and Simulation}, 84(1), 84-95.



G. Ridgeway (1999). \dQuote{The state of boosting,} \emph{Computing Science and
Statistics} 31:172-181.

\url{https://cran.r-project.org/package=gbm}\cr

J.H. Friedman (2001). \dQuote{Greedy Function Approximation: A Gradient Boosting
Machine,} \emph{Annals of Statistics} 29(5):1189-1232.

J.H. Friedman (2002). \dQuote{Stochastic Gradient Boosting,} \emph{Computational Statistics
and Data Analysis} 38(4):367-378.

}

\author{Yi Yang \email{yiyang@umn.edu} and Hui Zou \email{hzou@stat.umn.edu}
}


\seealso{
\code{\link{erboost.object}},
\code{\link{erboost.perf}},
\code{\link{plot.erboost}},
\code{\link{predict.erboost}},
\code{\link{summary.erboost}},
}
\examples{

N <- 200
X1 <- runif(N)
X2 <- 2*runif(N)
X3 <- ordered(sample(letters[1:4],N,replace=TRUE),levels=letters[4:1])
X4 <- factor(sample(letters[1:6],N,replace=TRUE))
X5 <- factor(sample(letters[1:3],N,replace=TRUE))
X6 <- 3*runif(N)
mu <- c(-1,0,1,2)[as.numeric(X3)]

SNR <- 10 # signal-to-noise ratio
Y <- X1**1.5 + 2 * (X2**.5) + mu
sigma <- sqrt(var(Y)/SNR)
Y <- Y + rnorm(N,0,sigma)

# introduce some missing values
X1[sample(1:N,size=50)] <- NA
X4[sample(1:N,size=30)] <- NA

data <- data.frame(Y=Y,X1=X1,X2=X2,X3=X3,X4=X4,X5=X5,X6=X6)

# fit initial model
erboost1 <- erboost(Y~X1+X2+X3+X4+X5+X6,         # formula
    data=data,                   # dataset
    var.monotone=c(0,0,0,0,0,0), # -1: monotone decrease,
                                 # +1: monotone increase,
                                 #  0: no monotone restrictions
    distribution=list(name="expectile",alpha=0.5),
                                 # expectile
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


# check performance using a 50\% heldout test set
best.iter <- erboost.perf(erboost1,method="test")
print(best.iter)

# check performance using 5-fold cross-validation
best.iter <- erboost.perf(erboost1,method="cv")
print(best.iter)

# plot the performance
# plot variable influence
summary(erboost1,n.trees=1)         # based on the first tree
summary(erboost1,n.trees=best.iter) # based on the estimated best number of trees

# make some new data
N <- 20
X1 <- runif(N)
X2 <- 2*runif(N)
X3 <- ordered(sample(letters[1:4],N,replace=TRUE))
X4 <- factor(sample(letters[1:6],N,replace=TRUE))
X5 <- factor(sample(letters[1:3],N,replace=TRUE))
X6 <- 3*runif(N)
mu <- c(-1,0,1,2)[as.numeric(X3)]

Y <- X1**1.5 + 2 * (X2**.5) + mu + rnorm(N,0,sigma)

data2 <- data.frame(Y=Y,X1=X1,X2=X2,X3=X3,X4=X4,X5=X5,X6=X6)

# predict on the new data using "best" number of trees
# f.predict generally will be on the canonical scale
f.predict <- predict.erboost(erboost1,data2,best.iter)

# least squares error
print(sum((data2$Y-f.predict)^2))

# create marginal plots
# plot variable X1 after "best" iterations
plot.erboost(erboost1,1,best.iter)
# contour plot of variables 1 and 3 after "best" iterations
plot.erboost(erboost1,c(1,3),best.iter)

# do another 20 iterations
erboost2 <- erboost.more(erboost1,20,
                 verbose=FALSE) # stop printing detailed progress
}
\keyword{models}
\keyword{nonlinear}
\keyword{survival}
\keyword{nonparametric}
\keyword{tree}
