# jonashaslbeck@gmail.com; Dec 7, 2021
# With code adapted from Lourens Waldorp

lasso_ms <- function(data,
                     B = 50,
                     fraction = 0.5,
                     ci.level = 0.95,
                     correction = TRUE, # if true, apply Holm-Bonferroni correction
                     pbar = TRUE,
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
  bhat <- array(NA,dim=c(p,p))
  pval.corr <- array(NA,dim=c(p,p))

  # ----- Nodewise Regression -----

  # Set up progress bar
  if(pbar) pb <- txtProgressBar(min = 0, max=p, initial=0, char="-", style = 3)

  # Loop over Nodes

  for(i in 1:p) {

    X <- as.matrix(X)

    y <- X[,i]
    x <- X[,-i]

    out <- hdi::multi.split(x = x, y = y,
                            B = B,
                            fraction = fraction,
                            ci.level = ci.level)

    # Get confidence intervals
    lower.bounds[i,-i] <- out$lci # lower bound
    upper.bounds[i,-i] <- out$uci # lower bound

    # Get corrected p-values
    pval.corr[i, -i] <- out$pval.corr

    if(pbar) setTxtProgressBar(pb, i)

  } # end for: p

  # Replace -Inf by 0, no non-zero interval
  lower.bounds[which(lower.bounds == -Inf)] <- 0
  upper.bounds[which(upper.bounds == Inf)] <- 0

  # ----- Assess Significance -----

  if(correction) {
    signif <- array(0, dim=c(p,p))
    for(i in 1:p){
      for(j in 1:p){
        signif[i,j] <- pval.corr[i,j] < alpha
      }
    }
  } else {
    # Alternatively, we could compute this from the pval-matrix
    signif <- array(0, dim=c(p,p))
    for(i in 1:p){
      for(j in 1:p){
        signif[i,j] <- ifelse(lower.bounds[i,j] > 0 & upper.bounds[i,j] > 0
                              | lower.bounds[i,j] < 0 & upper.bounds[i,j] < 0, 1, 0)
      }
    }
  }


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

  # Compute Point estimates (by taking mean of CIs, which are symmetric)
  bhat <- (lower.bounds + upper.bounds) / 2

  # Subset significant estiamates
  bhat_sym_sig <- bhat
  bhat_sym_sig[signif.fin==0] <- 0



  # ----- Return Results -----

  outlist <- list("est" = bhat,
                  "est.signf" = bhat_sym_sig,
                  "signif" = signif.fin,
                  "ci.lower" = lower.bounds,
                  "ci.upper" = upper.bounds)

  class(outlist) <- c('inet', 'list')

  return(outlist)

} # eoF












