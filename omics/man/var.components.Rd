\name{var.components}
\alias{var.components}
\alias{var.components.merMod}
\title{Extract Variance Components}
\description{
    Extracts variance components from a linear mixed-effects model.
}
\usage{
    \method{var.components}{merMod}(model)
}
\arguments{
    \item{model}{a linear mixed-effects model estimated using
                 \code{\link{lmer}}.}
}
\value{
    A data frame with elements:
    \item{group}{the name of the random effect.}
    \item{var.name}{the name of the variable.}
    \item{var}{the estimated variance.}
    \item{var.prop}{the estimated proportion of variance.}
}
\seealso{
    \code{\link{mlmer}}
}
