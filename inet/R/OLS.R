# jonashaslbeck@gmail.com; Jan 09, 2022
# Standard OLS + Bonferroni-Holm correction

OLS <- function(data,
                pbar = TRUE,
                correction = TRUE,
                ci.level = .95,
                rulereg = "and") {

  # ----- Basic Info -----
  X <- data
  p <- ncol(X)
  alpha <- 1 - ci.level

  # ----- Input Checks -----

  input_checks(data)

  # ----- Scale Data -----
  X <- apply(X, 2, scale)


  # ----- Make Storage -----

  # The below is adapted from code of Lourens
  lower.bounds <- array(NA,dim=c(p,p))
  upper.bounds <- array(NA,dim=c(p,p))
  pval.corr <- array(NA,dim=c(p,p))
  bhat <- array(NA,dim=c(p,p))
  ints <- rep(NA, p) # for intercepts

  # ----- Nodewise Regression -----

  # Set up progress bar
  if(pbar) pb <- txtProgressBar(min = 0, max=p, initial=0, char="-", style = 3)

  # Loop over Nodes

  for(i in 1:p) {

    X <- as.matrix(X)
    y <- X[,i]
    x <- X[,-i]

    # -- Linear regression --
    out <- lm(y~x)
    coefs <- coef(out)
    ints[i] <-  coefs[1] # save intercept
    bhat[i, -i] <- coefs[-1] # save coefficients

    # -- Correct p-values --
    p_vals <- (summary(out))$coefficients[-1, 4]  # get p-values

    # Apply Holm correction
    if(correction) {
      p_vals_adj <- p.adjust(p_vals, method = "holm", n=length(p_vals))
      p_vals <- p_vals_adj
    }
    pval.corr[i, -i] <- p_vals

    # -- Compute CIs --
    CIs <- confint(out, level = ci.level)[-1, ]
    lower.bounds[i, -i] <- CIs[, 1]
    upper.bounds[i, -i] <- CIs[, 2]

    if(pbar) setTxtProgressBar(pb, i)

  } # end for: p


  # ----- Assess Significance -----

  signif <- pval.corr < alpha


  # ----- Apply AND/OR-rule -----

  # combine from-to and to-from, either and-rule or or-rule
  signif.and <- array(0,dim=c(p,p))
  signif.or <- array(0,dim=c(p,p))
  for(i in 1:p){
    for(j in 1:p){
      if(i != j) signif.and[i,j] <- ifelse(signif[i,j]==signif[j,i] & signif[i,j]==1, 1, 0)
      if(i != j) signif.or[i,j] <- ifelse(signif[i,j]==1 | signif[j,i]==1, 1, 0)
    }
  }

  if(rulereg == "and") signif.fin <- signif.and else signif.fin <- signif.or

  # Apply AND-rule to estimates and CIs
  # bhat_sym <- (bhat + t(bhat)) / 2
  lower.bounds <- (lower.bounds + t(lower.bounds)) / 2
  upper.bounds <- (upper.bounds + t(upper.bounds)) / 2

  # Subset significant estimates
  bhat_sym_sig <- bhat
  bhat_sym_sig[signif.fin==0] <- 0

  # ----- Return Results -----

  outlist <- list("est" = bhat,
                  "est.signf" = bhat_sym_sig,
                  "signif" = signif.fin,
                  "ci.lower" = lower.bounds,
                  "ci.upper" = upper.bounds,
                  "ints" = ints)

  class(outlist) <- c('inet', 'list')

  return(outlist)

} # eoF
