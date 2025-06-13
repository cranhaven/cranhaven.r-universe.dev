\name{ranef.ranks}
\alias{ranef.ranks}
\alias{ranef.ranks.merMod}
\title{Extract Random Effect Rank Distributions}
\description{
    Extracts random effect rank distributions from a linear mixed-effects model.
}
\usage{
    \method{ranef.ranks}{merMod}(model, groups)
}
\arguments{
    \item{model}{a linear mixed-effects model estimated using
                 \code{\link{lmer}}.}
    \item{groups}{a character vector of random effects of interest.}
}
\value{
    If \code{which} is missing, a list of rank distribution vectors for all
    random intercepts in the model.
    Otherwise, a list of rank distribution vectors for random intercepts
    specified by \code{which}.
}
\seealso{
    \code{\link{mlmer}}
}
