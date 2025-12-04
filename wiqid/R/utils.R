
# This file contains utilities used in several places in the code
#   and NOT exported:

# modelMatrix : wrapper for model.matrix, changes "(Intercept)" to "Intrcpt"
# getVar0, getFittedVar : get variance for fitted values
# getScaling, doScaling, scaleToMatch : functions to deal with scaling
# signifish : an alternative to signif (added 10-02-2015)
# fixCI : Calculate critical values for CI.
# getMARKci : Calculate MARK-style confidence intervals for N
# stdModel : Regularize a list of formulae, ensuring it is a named list of one-sided formulae.
# stddata : Convert a data frame of site and survey data into a list and standardise
# selectCovars : Pull the covars needed for a model matrix into a specific data frame
# matchStart : has its own file

# fixNames : removed, use 'make.names(., unique=TRUE) instead (2019-04-17)
# AICtable moved to file AICc.R
# logSumExp etc are now in file UnderOverflow.R
# Functions to convert parameters of distributions (eg mean and sd to shape and rate)
#   are in converters.R
# Variants of the t-distribution are in TDist.R
# ...............................................................................

# Produces a  model matrix but the intercept column is named "Intrcpt" without
#   parentheses
modelMatrix <- function(formula, data, ...) {
  mm <- model.matrix(formula, data, ...)
  colnames(mm)[colnames(mm) == "(Intercept)"] <- "Intrcpt"
  return(mm)
}

# ...............................................................................

# Functions to calculate the variance of fitted values from model matrix and var-covar matrix.
# added 2017-10-16
# Output of getFittedVar is equivalent to
#   diag(MM %*% varcov %*% t(MM))
# but does not require calculation of the full matrix, which can be huge.

getVar0 <- function(x, vcv)
  x %*% vcv %*% x
# x : one row of a model matrix
# vcv : variance-covariance matrix

getFittedVar <- function(MM, vcv)
  apply(MM, 1, getVar0, vcv=vcv)
# MM : a model matrix
# vcv : variance-covariance matrix

# ...............................................................................

# Functions to deal with scaling, can be used with s/lapply

getScaling <- function(x, scaleBy)
  c(mean(x, na.rm=TRUE), sd(x, na.rm=TRUE) / scaleBy)

doScaling <- function(x, scaleBy)
  if(is.numeric(x)) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE) * scaleBy else x

# This takes a whole data frame
scaleToMatch <- function(target, scaling) {
  for(i in seq_along(target)) {
    if(is.numeric(target[[i]])) {
      pos <- match(names(target)[i], names(scaling))
      if(!is.na(pos)) {
        sc <- scaling[[pos]]
        target[[i]] <- (target[[i]] - sc[1]) / sc[2]
      }
    }
  }
  return(target)
}

# ...............................................................................

# A more sensible version of signif
signifish <- function(x, digits=3)
  ifelse(x < 10^digits, signif(x, digits=digits), round(x))

# ...............................................................................

# Deal with confidence interval specification:
fixCI <- function(ci) {
  if(ci > 1 | ci < 0.5)
    stop("ci must be between 0.5 and 1")
  alf <- (1 - ci[1]) / 2
  return(qnorm(c(alf, 1 - alf)))
}
# .....................................................................

# Function to calculate the MARK-style confidence intervals for N
# See help for Closed Captures

getMARKci <- function(beta, SE.beta, ci) {
  f0.hat <- exp(beta)
  crit <- qnorm((1 - ci[1]) / 2, lower.tail=FALSE)
  C <- exp(crit * sqrt(log(1 + SE.beta^2))) # See the Burnham et al reference, p212!
  return(c(f0.hat, f0.hat/C, f0.hat*C))
}
# .........................................................................

## Regularize a list of formulae, ensuring it is a named list of one-sided formulae.
# New version:
stdModel <- function (model1, defaultModel) {
  if(is.null(model1))
    return(defaultModel)
  if(inherits(model1, "formula"))
    model1 <- list(model1)
  if(!is.list(model1))
    stop("The 'model' argument must be a formula or a list of formulae.")
  LHS <- function (form) {
      trms <- as.character (form)
      if (length(trms)==2) '' else trms[2]
  }
  RHS <- function (form) {
      trms <- as.character (form)
      if (length(trms)==3) as.formula(paste(trms[c(1,3)], collapse=" ")) else form
  }
  lhs <- sapply(model1, LHS)
  temp <- lapply(model1, RHS)
  if (is.null(names(model1))) {
    names(temp) <- lhs
  } else {
    names(temp) <- ifelse(names(model1) == '', lhs, names(model1))
  }
  newModel <- replace (defaultModel, names(temp), temp)
  return(newModel)
}
# .............................................................................

## Convert a data frame of site and survey data into a list
# ** Site covars will each have a single column in the data frame,
# ** survey covars will have a column for each survey occasion, and
# column names end with the number of the occasion, eg, temperature
# will be in columns named "temp1", "temp2", etc.

stddata <- function(df, nocc=NULL, scaleBy=1)  {
  if (is.null(df))
    return(NULL)
  if(!is.data.frame(df))
    stop("The 'data' argument must be a data frame.")
  dataList <- as.list(df)
  ## Group variables spread over > 1 column into a single vector
  if (!is.null(nocc)) {
    nocc <- sort(nocc, decreasing=TRUE) # start with biggest
    for (this.nocc in nocc)  {
      # look for names ending with number of occasions
      nam <- names(df)
      clue <- paste0(this.nocc, "$", collapse="")
      clueDo <- grep(clue, nam)
      if(length(clueDo) > 0) {
        for(i in clueDo) {
          # get stem, generate set of names
          stem <- sub(clue, "", nam[i])
          subnames <- paste0(stem, 1:this.nocc)
          subtable <- df[, subnames]
          # check that there's a column for each occasion
          if(ncol(subtable) != this.nocc)
            stop("Survey covariates must have 1 column for each survey occasion.")
          # check that all have same class
          classes <- sapply(subtable, class)
          if(length(unique(classes)) != 1)
            stop("All columns of the survey covariates must have the same class (all factor or all numeric).")
          # remove original columns from the list:
          dataList <- replace(dataList, subnames, NULL)
          # convert to a matrix, then a vector;
          #   fortunately this also converts factors to character
          tmp <- as.vector(as.matrix(subtable))
          if(is.character(tmp))
            tmp <- as.factor(tmp) # convert to factor AFTER combining columns
          dataList <- c(dataList, list(tmp))
          names(dataList)[length(dataList)] <- stem
        }
      }
    }
  }
  ## Standardize numeric variables to mean = 0 and sd = scaleBy
  if (!is.null(scaleBy)) {
    doScale <- function(x) {
      if (is.numeric(x))
        x <- standardize(x) * scaleBy
      return(x)
    }
    dataList <- lapply(dataList, doScale)
  }
  return(dataList)
} # ...........................................................................

# Pull the covars needed for a model matrix into a specific data frame
selectCovars <- function(formula, dataList, minrows)  {
  wanted <- rownames(attr(terms(formula), "factors"))
  found <- wanted %in% names(dataList)
  wanted <- wanted[found]
  if (length(wanted) > 0)  {
    df <- as.data.frame(dataList[wanted])
    df <- cbind(df, .dummy = rep(NA, minrows))
  } else {
    df <- data.frame(.dummy = rep(NA, minrows))
  }
  stopifnot(nrow(df) %% minrows == 0)
  return(df)
} # ........................................................




