\name{allCombinations}
\alias{allCombinations}
\title{
Create formulae for all combinations of covariates
}
\description{
Create formulae for all combinations of covariates, currently main effects only.
}
\usage{
allCombinations(response = "", covars, formulae = TRUE)
}
\arguments{
  \item{response}{
a character vector of length 1 specifying the response variable.
}
  \item{covars}{
a character vector specifying the covariates/predictors.
}
  \item{formulae}{
if TRUE, only the formulae are returned; otherwise a TRUE/FALSE matrix is returned, with the formulae as row names.
}
}
\value{
If \code{formulae = TRUE}, returns a character vector with elements corresponding to the formulae for all possible combinations of main effects.

Otherwise, returns a TRUE/FALSE matrix indicating which covariates are included in each model with the model formulae as the row names.
}

\author{
Mike Meredith
}

\examples{
longNames <- colnames(swiss)
# these would produce formulae too long for the console.
names(swiss) <- abbreviate(longNames)
vars <- colnames(swiss)
vars

# Get the formulae for all combinations of covars:
formulae <- allCombinations(vars[1], vars[-1])
formulae[1:10]

# Run all the models with 'lm', put results into a list:
# lms <- lapply(formulae, lm, data=swiss) # This works, but the call is a mess!
lms <- vector('list', 32)
for(i in 1:32)
  lms[[i]] <- lm(formulae[i], data=swiss)
names(lms) <- formulae

# Extract AICs and look at top model:
AICs <- sapply(lms, AIC)
head(sort(AICs))
lms[[which.min(AICs)]]

# Do a nice table of results:
DeltaAIC <- AICs - min(AICs)
AICllh <- exp(-DeltaAIC/2)
AICwt <- AICllh / sum(AICllh)
order <- order(AICs)
head(round(cbind(AIC=AICs, DeltaAIC, AICllh, AICwt)[order, ], 3))

# Get AIC weights for each of the covars:
is.in <- allCombinations(vars[1], vars[-1], form=FALSE)
head(is.in)   # shows which covars are in each model
covarWts <- AICwt \%*\% is.in
round(sort(covarWts[1, ], dec=TRUE), 3)
  # the [1, ] is needed because \%*\% returns a 1-row matrix; 'sort' will coerce
  #   that to a vector but strips out the names in the process.
}
