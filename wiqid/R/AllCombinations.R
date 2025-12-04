# Function to generate formulae for all combinations of covariates.

allCombinations <- function(response="", covars, formulae=TRUE)  {
  # response : a character vector of length 1 specifying the response variable
  # covars : a character vector specifying the covariates/predictors
  # formulae : if TRUE, only the formulae are returned; otherwise a TRUE/FALSE matrix
  #   is returned, with the formulae as row names.
  if(!is.character(response) || !is.character(covars))
    stop("'response' and 'covars' must be character vectors")
  if(length(response) > 1)
    stop("Only one response variable possible.")
  if(any(response == covars))
    stop("The response cannot also be a covariate.")
  covars <- unique(covars)

  ncovs <- length(covars)
  tfmat <- matrix(FALSE, 2^ncovs, ncovs)
  for(i in 1:ncovs)
    tfmat[, i] <- rep(c(FALSE, TRUE), each=2^(i-1))
  # Sort (should this be optional?)
  if(ncovs > 1)
    tfmat <- tfmat[order(rowSums(tfmat)), ]
  RHS <- apply(tfmat, 1, function(x) paste(covars[x], collapse=" + "))
  RHS[1] <- "1"
  forms <- paste(response, RHS, sep=" ~ ")
  if(formulae) {
    return(forms)
  } else {
    colnames(tfmat) <- covars
    rownames(tfmat) <- forms
    return(tfmat)
  }
}

if(FALSE) {
longNames <- colnames(swiss)
longNames # Eeek! All too long.
names(swiss) <- abbreviate(longNames)
vars <- colnames(swiss)
vars # ok

# Get the formulae for all combinations of covars:
formulae <- allCombinations(vars[1], vars[-1])
formulae
class(formulae) # actually character, but coerced to formula as needed. 

# Run all the models with 'lm', put results into a list:
# lms <- lapply(formulae, lm, data=swiss) # This works, but the call is a mess!
lms <- vector('list', 32)
for(i in 1:32)
  lms[[i]] <- lm(formulae[i], data=swiss)
names(lms) <- formulae # uses fact that 'formulae' is actually character.

# Extract AICs and look at top model:
AICs <- sapply(lms, AIC)
sort(AICs)
lms[[which.min(AICs)]]

# Do a nice table of results:
DeltaAIC <- AICs - min(AICs)
AICllh <- exp(-DeltaAIC/2)
AICwt <- AICllh / sum(AICllh)
order <- order(AICs)
round(cbind(AIC=AICs, DeltaAIC, AICllh, AICwt)[order, ], 3)

# Get AIC weights for each of the covars:
is.in <- allCombinations(vars[1], vars[-1], form=FALSE)
is.in   # shows which covars are in each model
covarWts <- AICwt %*% is.in
round(sort(covarWts[1, ], dec=TRUE), 3)
  # the [1, ] is needed because %*% returns a 1-row matrix; 'sort' will coerce
  #   that to a vector but strips out the names in the process.
}