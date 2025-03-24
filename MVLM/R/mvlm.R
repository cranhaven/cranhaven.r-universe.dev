#' Conduct multivariate multiple regression and MANOVA with analytic p-values
#'
#' \code{mvlm} is used to fit linear models with a multivariate outcome. It uses
#' the asymptotic null distribution of the multivariate linear model test
#' statistic to compute p-values (McArtor et al., under review). It therefore
#' alleviates the need to use approximate p-values based Wilks' Lambda, Pillai's
#' Trace, the Hotelling-Lawley Trace, and Roy's Greatest Root.
#'
#' Importantly, the outcome of \code{formula} must be a \code{matrix}, and the
#' object passed to \code{data} must be a data frame containing all of the
#' variables that are named as predictors in \code{formula}.
#'
#' The conditional effects of variables of type \code{factor} or \code{ordered}
#' in \code{data} are computed based on the type of contrasts specified by
#' \code{contr.factor} and \code{contr.ordered}. If \code{data} contains an
#' (ordered or unordered) factor with \code{k} levels, a \code{k-1} degree of
#' freedom test will be conducted corresponding to that factor and the specified
#' contrast structure. If, instead, the user wants to assess \code{k-1} separate
#' single DF tests that comprise this omnibus effect (similar to the approach
#' taken by \code{lm}), then the appropriate model matrix should be formed in
#' advance and passed to \code{mvlm} directly in the \code{data} parameter. See
#' the package vigentte for an example by calling
#' \code{vignette('mvlm-vignette')}.
#'
#' @param formula An object of class \code{formula} where the outcome (e.g. the
#' Y in the following formula: Y ~ x1 + x2) is a \code{n x q matrix}, where
#' \code{q} is the number of outcome variables being regressed onto the set
#' of predictors included in the formula.
#' @param data Mandatory \code{data.frame} containing all of the predictors
#' passed to \code{formula}.
#' @param n.cores Number of cores to use in parallelization through the
#' \code{parallel} pacakge.
#' @param start.acc Starting accuracy of the Davies (1980) algorithm
#' implemented in the \code{\link{davies}} function in the \code{CompQuadForm}
#' package (Duchesne &  De Micheaux, 2010) that \code{mvlm} uses to compute
#' multivariate linear model p-values.
#' @param contr.factor The type of contrasts used to test unordered categorical
#' variables that have type \code{factor}. Must be a string taking one of the
#' following values: \code{("contr.sum", "contr.treatment", "contr.helmert")}.
#' @param contr.ordered The type of contrasts used to test ordered categorical
#' variables that have type \code{ordered}. Must be a string taking one of the
#' following values: \code{("contr.poly", "contr.sum", "contr.treatment",
#' "contr.helmert")}.
#'
#' @return An object with nine elements and a summary function. Calling
#' \code{summary(mvlm.res)} produces a data frame comprised of:
#' \item{Statistic}{Value of the corresponding test statistic.}
#' \item{Numer DF}{Numerator degrees of freedom for each test statistic.}
#' \item{Pseudo R2}{Size of the corresponding (omnibus or conditional) effect
#' on the multivariate outcome. Note that the intercept term does not have
#' an estimated effect size.}
#' \item{p-value}{The p-value for each (omnibus or conditional) effect.}
#' In addition to the information in the three columns comprising
#' \code{summary(mvlm.res)}, the \code{mvlm.res} object also contains:
#'
#' \item{p.prec}{A data.frame reporting the precision of each p-value.
#' These are the maximum error bound of the p-values reported by the
#' \code{davies} function in \code{CompQuadForm}.}
#' \item{y.rsq}{A matrix containing in its first row the overall variance
#' explained by the model for variable comprising Y (columns). The remaining
#' rows list the variance of each outcome that is explained by the conditional
#' effect of each predictor.}
#' \item{beta.hat}{Estimated regression coefficients.}
#' \item{adj.n}{Adjusted sample size used to determine whether or not the
#' asmptotic properties of the model are likely to hold. See McArtor et al.
#' (under review) for more detail.}
#' \item{data}{Original input data and the \code{model.matrix} used to fit the
#' model.}
#' \item{formula}{The formula passed to \code{mvlm}.}
#'
#' Note that the printed output of \code{summary(res)} will truncate p-values
#' to the smallest trustworthy values, but the object returned by
#' \code{summary(mvlm.res)} will contain the p-values as computed. If the error
#' bound of the Davies algorithm is larger than the p-value, the only conclusion
#' that can be drawn with certainty is that the p-value is smaller than (or
#'  equal to) the error bound.
#'
#' @author Daniel B. McArtor (dmcartor@nd.edu) [aut, cre]
#'
#' @references Davies, R. B. (1980). The Distribution of a Linear Combination of
#'  chi-square Random Variables. Journal of the Royal Statistical Society.
#'  Series C (Applied Statistics), 29(3), 323-333.
#'
#'  Duchesne, P., & De Micheaux, P.L. (2010). Computing the distribution of
#'  quadratic forms: Further comparisons between the Liu-Tang-Zhang
#'  approximation and exact methods. Computational Statistics and Data
#'  Analysis, 54(4), 858-862.
#'
#'  McArtor, D. B., Grasman, R. P. P. P., Lubke, G. H., & Bergeman, C. S.
#'  (under review). A new approach to conducting linear model hypothesis tests
#'  with a multivariate outcome.
#'
#' @examples
#'data(mvlmdata)
#'
#'Y <- as.matrix(Y.mvlm)
#'
#'# Main effects model
#'mvlm.res <- mvlm(Y ~ Cont + Cat + Ord, data = X.mvlm)
#'summary(mvlm.res)
#'
#'# Include two-way interactions
#'mvlm.res.int <- mvlm(Y ~ .^2, data = X.mvlm)
#'summary(mvlm.res.int)
#'
#' @importFrom CompQuadForm davies
#' @importFrom parallel mclapply
#' @export
mvlm <- function(formula, data,
                 n.cores = 1,
                 start.acc = 1e-20,
                 contr.factor = 'contr.sum',
                 contr.ordered = 'contr.poly'){

  # ============================================================================
  # Input management
  # ============================================================================

  # ----------------------------------------------------------------------------
  # Get the data
  # ----------------------------------------------------------------------------
  X <- data
  rm(data)
  dat <- stats::model.frame(formula, data = X, na.action = NULL)
  Y <- as.matrix(dat[1])
  p <- ncol(X)
  q <- ncol(Y)
  rm(dat)

  # ----------------------------------------------------------------------------
  # Handle missingness
  # ----------------------------------------------------------------------------
  X.use <- X
  Y.use <- Y
  which.na <- unique(c(which(rowSums(is.na(X)) > 0),
                       which(rowSums(is.na(Y)) > 0)))
  if(length(which.na) > 0){
    X.use <- X.use[-which.na,]
    Y.use <- Y.use[-which.na,]
    warning(paste(length(which.na), 'observations removed due to missingness.'))
  }

  # Record sample size
  n <- nrow(X.use)

  # ----------------------------------------------------------------------------
  # Handle variable names
  # ----------------------------------------------------------------------------

  # Get Y names
  ynames <- colnames(Y)
  if(all(is.null(colnames(Y)))){
    ynames <- paste0('Y', 1:q)
  }

  # Get X names that were passed to mvlm()
  X <- as.data.frame(X)
  xnames <- names(X)

  # Get the formula to expand using stats::model.matrix()
  fmla <- paste(formula)
  fmla <- stats::as.formula(paste('~', fmla[3]))

  # ----------------------------------------------------------------------------
  # Handle potential contrast coding of factors
  # ----------------------------------------------------------------------------

  contr.list <- lapply(1:p, FUN = function(k){
    # Default: no contrast for quantitaive predictor
    contr.type <- NULL

    # Sum contrasts for unordered categorical predictor
    if(is.factor(X[,k])){
      contr.type <- contr.factor
    }

    # Polynomial contrasts for ordered categorical predictor
    if(is.ordered(X[,k])){
      contr.type <- contr.ordered
    }

    return(contr.type)
  })

  names(contr.list) <- xnames
  contr.list <- contr.list[!unlist(lapply(contr.list, is.null))]

  # ----------------------------------------------------------------------------
  # Get full design matrix
  # ----------------------------------------------------------------------------
  X.full <- stats::model.matrix(fmla, data = X.use, contrasts = contr.list)
  term.inds <- attr(X.full, 'assign')
  term.names <-  c('(Intercept)',
                   attr(stats::terms(fmla, data = X.use), 'term.labels'))

  # Record number of predictor variables after contrast coding
  p <- ncol(X.full) - 1
  px <- length(term.names)

  # ============================================================================
  # Get eigenvalues of SSCP and create function to get p-values
  # ============================================================================

  # The overall SSCP
  mean.Y <- matrix(apply(Y.use, 2, mean), nrow = n, ncol = q, byrow = T)
  sscp.Y <- crossprod(Y.use)
  sscp.mean.Y <- crossprod(mean.Y)
  sscp <- sscp.Y - sscp.mean.Y

  # Eigenvalues
  lambda <- eigen(sscp, only.values = T)$values

  # Compute adjusted sample size
  n.tilde <- (n-p-1) * lambda[1] / sum(lambda)

  # Function to compute p-values
  pmvlm <- function(f, lambda, k, p, n,
                    lim = 50000, acc = start.acc){

    # Remove df from test statistic
    f <- f/(n-p-1)*k

    gamma <- c(lambda,  -f * lambda)

    nu <- c(rep(k, length(lambda)), rep(n-p-1, length(lambda)))

    # Call the Davies function at zero using the given weights and df, along
    # with the starting values of the metaparameters of the algorithm
    pv <- CompQuadForm::davies(0, lambda = gamma, h = nu, lim = lim, acc = acc)

    # Check error status. If there was an error, return entire davies object
    if(pv$ifault != 0){
      #     warning('Error in davies() procedure, please check results.')
      return(pv)
    }

    # If the p-value is below zero, interpret as an error and break
    if(pv$Qq < 0){
      #     warning('Error in davies() procedure, please check results.')
      return(pv)
    }

    # If there was no error, return only the p-value
    if(pv$ifault == 0){
      return(pv$Qq)
    }
  }

  # ============================================================================
  # Compute test statistics and r-squares
  # ============================================================================

  # Overall Hat matrix
  H <- tcrossprod(tcrossprod(
    X.full, solve(crossprod(X.full))), X.full)

  # Error SSCP
  sscp.e <- crossprod(Y.use, diag(n) - H) %*% Y.use

  # Denominator of each test statistic
  mse <- sum(diag(sscp.e)) / (n-p-1)

  # --- Omnibus Test Statistic --- #
  sscp.r <- (crossprod(Y.use, H) %*% Y.use) - sscp.mean.Y
  f.omni <- (sum(diag(sscp.r)) / p) / mse

  # --- Omnibus R-Square --- #
  rsq.omni <- sscp.r / sscp
  rsq.omni <- c('overall' = sum(diag(sscp.r))/sum(diag(sscp)), diag(rsq.omni))

  # --- Conditional test statistics and r-squares for each predictor --- #
  res.x <- matrix(
    unlist(
      parallel::mclapply(
        unique(term.inds), mc.cores = n.cores, FUN = function(tn){

          # Make adjusted design matrix
          pred.inds <- which(term.inds == tn)
          X.tmp <- X.full[,-pred.inds]
          k <- length(pred.inds)

          # Hat matrix
          H1 <- tcrossprod(tcrossprod(X.tmp, solve(crossprod(X.tmp))), X.tmp)

          # Compute test statistic
          sscp.r.tmp <- crossprod(Y.use, H - H1) %*% Y.use
          f.xx <- (sum(diag(sscp.r.tmp)) / k) / mse

          # Compute r-squares
          rsq.x <- sscp.r.tmp / sscp
          rsq.x <- c('overall' =
                       sum(diag(sscp.r.tmp))/sum(diag(sscp)), diag(rsq.x))

          # Return output
          return(c('df' = k, 'f' = f.xx, rsq.x))

        })), nrow = px, byrow = T)
  colnames(res.x) <- c('df', 'f', 'rsq', paste0('rsq', 1:q))
  df <- res.x[,1]
  f.x <- res.x[,2]
  rsq.x <- res.x[,-(1:2)]

  # ============================================================================
  # Compute p-values
  # ============================================================================

  # --- Omnibus Test --- #
  acc.omni <- start.acc
  pv.omni <- pmvlm(f = f.omni, lambda = lambda, k = p, p = p, n = n,
                   acc = acc.omni)

  # If the davies procedure threw an error, decrease the accuracy
  while(length(pv.omni) > 1){
    acc.omni <- acc.omni * 10
    pv.omni <- pmvlm(f = f.omni, lambda = lambda, k = p, p = p, n = n,
                     acc = acc.omni)
  }

  # --- Conditional tests --- #
  pv.x <- matrix(
    unlist(parallel::mclapply(1:px, mc.cores = n.cores, FUN = function(l){

      ff <- f.x[l]
      dff <- df[l]

      acc.x <- start.acc
      pv.x <- pmvlm(f = ff, lambda = lambda, k = dff, p = p, n = n, acc = acc.x)

      # If the davies procedure threw an error, decrease the accuracy
      while(length(pv.x) > 1){
        acc.x <- acc.x * 10
        pv.x <- pmvlm(f = ff, lambda = lambda, k = dff, p = p, n = n,
                      acc = acc.x)
      }

      return(c(pv.x, acc.x))
    })), ncol = 2, byrow = T)

  acc.x <- pv.x[,2]
  pv.x <- pv.x[,1]

  # ============================================================================
  # Get estimated regression coefficients;
  # format test stats, p-values, p-value accuracy, and R-square for output
  # ============================================================================

  beta.hat <- as.matrix(stats::coef(stats::lm(formula, data = X,
                                              contrasts = contr.list)))
  colnames(beta.hat) <- ynames

  stat <- c(f.omni, f.x)
  df <- c(p, df)
  pv <- c(pv.omni, pv.x)
  pv.acc <- c(acc.omni, acc.x)
  names(stat) <- names(pv) <- names(pv.acc) <- names(df) <-
    c('Omnibus Effect', term.names)

  pseudo.rsq <- c(rsq.omni[1], rsq.x[-1,1])
  names(pseudo.rsq) <- c('Omnibus Effect', term.names[-1])

  y.rsq <- rbind(rsq.omni[-1], rsq.x[-1,-1,drop=F])
  dimnames(y.rsq) <- list(c('Omnibus Effect', term.names[-1]),  c(ynames))

  # ============================================================================
  # Output results
  # ============================================================================
  out <- list('stat' = as.matrix(stat),
              'df' = as.matrix(df),
              'pv' = as.matrix(pv),
              'p.prec' = as.matrix(pv.acc),
              'pseudo.rsq' = as.matrix(pseudo.rsq),
              'y.rsq' = as.matrix(y.rsq),
              'beta.hat' = as.matrix(beta.hat),
              'adj.n' = n.tilde,
              'data' = list('X' = X, 'Y' = Y, model.matrix = X.full),
              'formula' = formula)

  class(out) <- c('mvlm', class(out))

  if(n.tilde < 75){
    warning(
      paste0('Adjusted sample size = ', round(n.tilde), '\n',
             'Asymptotic properties of the null distribution may not hold.\n',
             'This can result in overly conservative p-values.\n',
             'Increased sample size is recommended.'))
  }

  return(out)
}


#' Print mvlm Object
#'
#' \code{print} method for class \code{mvlm}
#'
#' @param x Output from \code{mvlm}
#' @param ... Further arguments passed to or from other methods.
#'
#' @return
#' \item{p-value}{Analytical p-values for the omnibus test and each predictor}
#'
#' @author Daniel B. McArtor (dmcartor@nd.edu) [aut, cre]
#'
#'
#' @export
print.mvlm <- function(x, ...){
  pv.name <- 'p-value'
  # If it's analytic, we can only say it's below davies error
  out <- rep(NA, nrow(x$pv))
  for(i in 1:length(out)){
    out[i] <- format.pval(x$pv[i,1], eps = x$p.prec[i,1])
  }
  out <- data.frame(out,row.names = rownames(x$pv))
  names(out) <- pv.name
  print(out)
}


#' Summarizing mvlm Results
#'
#' \code{summary} method for class \code{mvlm}
#'
#' @param object Output from \code{mvlm}
#' @param ... Further arguments passed to or from other methods.
#'
#' @return Calling
#' \code{summary(mvlm.res)} produces a data frame comprised of:
#' \item{Statistic}{Value of the corresponding test statistic.}
#' \item{Numer DF}{Numerator degrees of freedom for each test statistic.}
#' \item{Pseudo R2}{Size of the corresponding (omnibus or conditional) effect
#' on the multivariate outcome. Note that the intercept term does not have
#' an estimated effect size.}
#' \item{p-value}{The p-value for each (omnibus or conditional) effect.}
#' In addition to the information in the three columns comprising
#' \code{summary(mvlm.res)}, the \code{mvlm.res} object also contains:
#'
#' \item{p.prec}{A data.frame reporting the precision of each p-value.
#' These are the maximum error bound of the p-values reported by the
#' \code{davies} function in \code{CompQuadForm}.}
#' \item{y.rsq}{A matrix containing in its first row the overall variance
#' explained by the model for variable comprising Y (columns). The remaining
#' rows list the variance of each outcome that is explained by the conditional
#' effect of each predictor.}
#' \item{beta.hat}{Estimated regression coefficients.}
#' \item{adj.n}{Adjusted sample size used to determine whether or not the
#' asmptotic properties of the model are likely to hold. See McArtor et al.
#' (under review) for more detail.}
#' \item{data}{Original input data and the \code{model.matrix} used to fit the
#' model.}
#'
#' Note that the printed output of \code{summary(res)} will truncate p-values
#' to the smallest trustworthy values, but the object returned by
#' \code{summary(mvlm.res)} will contain the p-values as computed. If the error
#' bound of the Davies algorithm is larger than the p-value, the only conclusion
#' that can be drawn with certainty is that the p-value is smaller than (or
#'  equal to) the error bound.
#'
#' @author Daniel B. McArtor (dmcartor@nd.edu) [aut, cre]
#'
#' @references Davies, R. B. (1980). The Distribution of a Linear Combination of
#'  chi-square Random Variables. Journal of the Royal Statistical Society.
#'  Series C (Applied Statistics), 29(3), 323-333.
#'
#'  Duchesne, P., & De Micheaux, P.L. (2010). Computing the distribution of
#'  quadratic forms: Further comparisons between the Liu-Tang-Zhang
#'  approximation and exact methods. Computational Statistics and Data
#'  Analysis, 54(4), 858-862.
#'
#'  McArtor, D. B., Grasman, R. P. P. P., Lubke, G. H., & Bergeman, C. S.
#'  (under review). A new approach to conducting linear model hypothesis tests
#'  with a multivariate outcome.
#'
#' @examples
#'data(mvlmdata)
#'
#'Y <- as.matrix(Y.mvlm)
#'
#'# Main effects model
#'mvlm.res <- mvlm(Y ~ Cont + Cat + Ord, data = X.mvlm)
#'summary(mvlm.res)
#'
#'# Include two-way interactions
#'mvlm.res.int <- mvlm(Y ~ .^2, data = X.mvlm)
#'summary(mvlm.res.int)
#'
#' @export
summary.mvlm <- function(object, ...){
  pp <- unlist(object$pv)

  pr2.out <- c(object$pseudo.rsq[1], NA, object$pseudo.rsq[-1])
  out.res <- list('statistic' = object$stat,
                  'numerDF' = object$df,
                  'pseudoR2' = pr2.out,
                  'pvalues' = object$pv,
                  'p.prec' = object$p.prec)
  class(out.res) <- 'summary.mvlm'
  return(out.res)
}

#' @export
print.summary.mvlm <- function(x, ...){

  print.res <- data.frame('Statistic' = x$statistic,
                          'Numer DF' = x$numerDF,
                          'Pseudo R-Square' = x$pseudoR2,
                          'p-value' = x$pvalues)

  # Format p-values for printing
  pp <- x$pvalues
  pv.print <- rep(NA, length(pp))
  for(i in 1:length(pv.print)){
    pv.print[i] <- format.pval(pp[i], eps = x$p.prec[i,1])
  }
  print.res[,4] <- pv.print

  # Format significant digits on R-Square and test statistic
  print.res[,1] <- format(print.res[,1],
                          trim = T,
                          digits = 4,
                          justify = 'right',
                          width = 10)

  print.res[,3] <- format(print.res[,3],
                          trim = T,
                          digits = 4,
                          justify = 'right',
                          width = 10)

  # Clear effect size from intercept
  print.res[2,3] <- ''

  # Add significance codes to p-values
  print.res <- data.frame(print.res, NA)
  names(print.res)[5] <- ''
  for(l in 1:nrow(print.res)){
    if(pp[l] > 0.1){
      print.res[l,5] <- '   '
    }
    if((pp[l] <= 0.1) & (pp[l] > 0.05)){
      print.res[l,5] <- '.  '
    }
    if((pp[l] <= 0.05) & (pp[l] > 0.01)){
      print.res[l,5] <- '*  '
    }
    if((pp[l] <= 0.01) & (pp[l] > 0.001)){
      print.res[l,5] <- '** '
    }
    if(pp[l] <= 0.001){
      print.res[l,5] <- '***'
    }
  }

  print(print.res)
  cat('---', fill = T)
  cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")

}

#' Extract mvlm Fitted Values
#'
#' \code{fitted} method for class \code{mvlm}.
#'
#' @param object Output from \code{mvlm}
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A data frame of fitted values with the same dimension as the outcome
#' data passed to \code{mvlm}
#'
#' @author Daniel B. McArtor (dmcartor@nd.edu) [aut, cre]
#'
#' @examples
#'data(mvlmdata)
#'Y <- as.matrix(Y.mvlm)
#'mvlm.res <- mvlm(Y ~ Cont + Cat + Ord, data = X.mvlm)
#'Y.hat <- fitted(mvlm.res)
#'
#' @export
fitted.mvlm <- function(object, ...){
  XX <- object$data$X
  YY <- object$data$Y
  MM <- object$data$model.matrix
  con.list <- attr(MM, 'contrasts')
  fmla <- paste(object$formula)
  fmla <- stats::as.formula(paste('~', fmla[3]))

  BB <- object$beta.hat

  # Don't let the missing observations be removed - keep NA's in the output
  na.x <- which(rowSums(is.na(XX)) > 0)
  # replace predicors with a placeholder - the predicted values will be NA'ed
  if(length(na.x) > 0){
    XX[na.x,] <- XX[(1:nrow(XX)[-na.x])[1],]
  }

  XX <- stats::model.matrix(fmla, data = XX, contrasts.arg = con.list)
  Y.hat <- XX %*% BB

  # Maually recode the entries that should be NA
  if(length(na.x) > 0){
    Y.hat[na.x,] <- NA
  }

  # Return
  return(Y.hat)
}


#' Extract mvlm Residuals
#'
#' \code{residuals} method for class \code{mvlm}.
#'
#' @param object Output from \code{mvlm}
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A data frame of residuals with the same dimension as the outcome
#' data passed to \code{mvlm}
#'
#' @author Daniel B. McArtor (dmcartor@nd.edu) [aut, cre]
#'
#' @examples
#'data(mvlmdata)
#'Y <- as.matrix(Y.mvlm)
#'mvlm.res <- mvlm(Y ~ Cont + Cat + Ord, data = X.mvlm)
#'Y.resid <- resid(mvlm.res)
#'
#' @export
residuals.mvlm <- function(object, ...){
  XX <- object$data$X
  YY <- object$data$Y
  MM <- object$data$model.matrix
  con.list <- attr(MM, 'contrasts')
  fmla <- paste(object$formula)
  fmla <- stats::as.formula(paste('~', fmla[3]))

  BB <- object$beta.hat

  # Don't let the missing observations be removed - keep NA's in the output
  na.x <- which(rowSums(is.na(XX)) > 0)
  # replace predicors with a placeholder - the predicted values will be NA'ed
  if(length(na.x) > 0){
    XX[na.x,] <- XX[(1:nrow(XX)[-na.x])[1],]
  }

  XX <- stats::model.matrix(fmla, data = XX, contrasts.arg = con.list)
  Y.hat <- XX %*% BB

  # Maually recode the entries that should be NA
  if(length(na.x) > 0){
    Y.hat[na.x,] <- NA
  }
  Y.resid <- YY - Y.hat

  # Return
  return(Y.resid)
}


#' mvlm Model Predictions
#'
#' \code{predict} method for class \code{mvlm}. To predict using new data, the
#' predictor data frame passed to \code{newdata} must have the same number of
#' columns as the data used to fit the model, and the names of each variable
#' must match the names of the original data.
#'
#' @param object Output from \code{mvlm}
#' @param newdata Data frame of observations on the predictors used to fit the
#' model.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A data frame of predicted values
#'
#' @author Daniel B. McArtor (dmcartor@nd.edu) [aut, cre]
#'
#' @examples
#'data(mvlmdata)
#'Y.train <- as.matrix(Y.mvlm[1:150,])
#'X.train <- X.mvlm[1:150,]
#'
#'mvlm.res <- mvlm(Y.train ~ ., data = X.train)
#'
#'X.test <- X.mvlm[151:200,]
#'Y.predict <- predict(mvlm.res, newdata = X.test)
#'
#' @export
predict.mvlm <- function(object, newdata, ...){
  fmla <- paste(object$formula)
  fmla <- stats::as.formula(paste('~', fmla[3]))

  XX <- data.frame(newdata)
  MM <- object$data$model.matrix
  con.list <- attr(MM, 'contrasts')
  BB <- object$beta.hat

  # Use the original variable names
  ynames <- colnames(object$data$Y)
  xnames <- names(object$data$X)

  # Here I can make sure the names on newdata match the names on X
  if(any(xnames != names(XX))){
    stop('The variables comprising newdata must be the same as the predictors
               passed to the origial call to mvlm().')
  }



  # Don't let the missing observations be removed - keep NA's in the output
  na.x <- which(rowSums(is.na(XX)) > 0)
  if(length(na.x) > 0){
    XX[na.x,] <- XX[(1:nrow(XX)[-na.x])[1],]
  }

  XX <- stats::model.matrix(fmla, data = XX, contrasts.arg = con.list)
  Y.hat <- XX %*% BB
  colnames(Y.hat) <- ynames

  # Maually recode the entries that should be NA
  if(length(na.x) > 0){
    Y.hat[na.x,] <- NA
  }

  return(Y.hat)
}


#' Simulated predictor data to illustrate the mvlm package.
#'
#' See package vignette by calling \code{vignette('mvlm-vignette')}.
"X.mvlm"


#' Simulated outcome data to illustrate the mvlm package.
#'
#' See package vignette by calling \code{vignette('mvlm-vignette')}.
"Y.mvlm"
