\name{get.biom}
\alias{get.biom}
\alias{print.BMark}
\alias{summary.BMark}
\alias{coef.BMark}
\title{Get biomarkers discriminating between two classes}
\description{Biomarkers can be identified in several ways: the classical way
  is to look at those variables with large model coefficients or large t
  statistics. One other is based on the higher criticism approach (HC),
  and the third possibility assesses the stability of these coefficients
  under subsampling of the data set.
}
\usage{
get.biom(X, Y, fmethod = "all", type = c("stab", "HC", "coef"),
         ncomp = 2, biom.opt = biom.options(), scale.p = "auto",
         \dots)
\method{coef}{BMark}(object, \dots)
\method{print}{BMark}(x, \dots)
\method{summary}{BMark}(object, \dots)
}
\arguments{
  \item{X}{Data matrix. Usually the number of columns (variables) is
    (much) larger than the number of rows (samples).}
  \item{Y}{Class indication. For classification with two or more factors
    a factor; a numeric vector will be interpreted as a regression
    situation, which can only be tackled by \code{fmethod = "lasso"}.}
  \item{fmethod}{Modelling method(s) employed. The default is to use
    \code{"all"}, which will test all methods in the current
    \code{biom.options$fmethods} list. Note that from version 0.4.0,
    \code{"plsda"} and \code{"pclda"} are no longer in the list of
    methods - they have been replaced by \code{"pls"} and \code{"pcr"},
    respectively. For compatibility reasons, using the old terms will
    not lead to an error but only a warning.} 
  \item{type}{Whether to use coefficient size as a criterion
    (\code{"coef"}), \code{"stab"} or \code{"HC"}.}
  \item{ncomp}{Number of latent variables to use in PCR and PLS (VIP)
    modelling. In function \code{get.biom} this may be a vector; in all
    other functions it should be one number. Default: 2.}
  \item{biom.opt}{Options for the biomarker selection - a list with
    several named elements. See \code{\link{biom.options}}.}
  \item{scale.p}{Scaling. This is performed individually in every
    crossvalidation iteration, and can have a profound effect on the
    results. Default: "auto" (autoscaling). Other possible choices:
    "none" for no scaling, "pareto" for pareto scaling, "log" and "sqrt" for log
    and square root scaling, respectively.}
  \item{object, x}{A BMark object.}
  \item{\dots}{Further arguments for modelling functions. Often used to
    catch unused arguments.}
}
\value{Function \code{get.biom} returns an object of class "BMark", a
  list containing an element 
  for every \code{fmethod} that is selected, as well as an element
  \code{info}. The individual elements contain information depending on
  the type chosen: for \code{type == "coef"}, the only element returned
  is a matrix containing coefficient sizes. For \code{type == "HC"}
  and \code{type == "stab"}, a list is returned containing elements
  \code{biom.indices}, and either \code{pvals} (for \code{type == "HC"})
  or \code{fraction.selected} (for \code{type == "stab"}).
  Element \code{biom.indices} contains the indices of
  the selected variables, and can be extracted using function
  \code{selection}. Element \code{pvals} contains the p values
  used to perform HC thresholding; these are presented in the original
  order of the variables, and can be obtained directly from e.g. t
  statistics, or from permutation sampling. Element
  \code{fraction.selected} indicates in what fraction of the
  stability selection iterations a particular variable has been
  selected. The more often it has been selected, the more stable it is
  as a biomarker. Generic function \code{coef.biom} extracts model
  coefficients, p values or stability fractions for types \code{"coef"},
  \code{"HC"} and \code{"stab"}, respectively.}
\author{Ron Wehrens}
\seealso{\code{\link{biom.options}}, \code{\link{get.segments}},
  \code{\link{selection}}, \code{\link{scalefun}}}
\examples{
## Real apple data (small set)
data(spikedApples)
apple.coef <- get.biom(X = spikedApples$dataMatrix,
                       Y = factor(rep(1:2, each = 10)),
                       ncomp = 2:3, type = "coef")
coef.sizes <- coef(apple.coef) 
sapply(coef.sizes, range)

## stability-based selection
set.seed(17)
apple.stab <- get.biom(X = spikedApples$dataMatrix,
                       Y = factor(rep(1:2, each = 10)),
                       ncomp = 2:3, type = "stab")
selected.variables <- selection(apple.stab)
unlist(sapply(selected.variables, function(x) sapply(x, length)))
## Ranging from more than 70 for pcr, approx 40 for pls and student t,
## to 0-29 for the lasso
unlist(sapply(selected.variables,
              function(x) lapply(x, function(xx, y) sum(xx \%in\% y),
              spikedApples$biom)))
## TPs (stab): all find 5/5, except pcr.2 and the lasso with values for lambda
## larger than 0.0484

unlist(sapply(selected.variables,
              function(x) lapply(x, function(xx, y) sum(!(xx \%in\% y)),
              spikedApples$biom)))
## FPs (stab): PCR finds most FPs (approx. 60), other latent-variable
## methods approx 40, lasso allows for the optimal selection around 
## lambda = 0.0702

## regression example
data(gasoline) ## from the pls package
gasoline.stab <- get.biom(gasoline$NIR, gasoline$octane,
                          fmethod = c("pcr", "pls", "lasso"), type = "stab")


\dontrun{
## Same for HC-based selection
## Warning: takes a long time!
apple.HC <- get.biom(X = spikedApples$dataMatrix,
                     Y = factor(rep(1:2, each = 10)),
                     ncomp = 2:3, type = "HC")
sapply(apple.HC[names(apple.HC) != "info"],
       function(x, y) sum(x$biom.indices \%in\% y),
       spikedApples$biom)
sapply(apple.HC[names(apple.HC) != "info"],
       function(x, y) sum(!(x$biom.indices \%in\% y)),
       spikedApples$biom)
}
}
\keyword{multivariate}
\keyword{models}
\keyword{htest}
