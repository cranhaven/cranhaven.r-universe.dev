# jonashaslbeck@gmail.com; Jan 9, 2022
# Standard lasso for GGM estimation with cv

lasso <- function(data,
                  pbar = TRUE,
                  nfolds = 10,
                  rulereg = "and") {

  # ----- Basic Info -----
  X <- data
  p <- ncol(X)

  # ----- Input Checks -----

  input_checks(data)

  # ----- Scale Data -----
  X <- apply(X, 2, scale)

  # ----- Make Storage -----


  # The below is adapted from code of Lourens
  bhat <- signif <- array(NA,dim=c(p,p))
  ints <- rep(NA, p) # for intercepts

  # ----- Nodewise Regression -----

  # Set up progress bar
  if(pbar) pb <- txtProgressBar(min = 0, max=p, initial=0, char="-", style = 3)

  # Loop over Nodes

  for(i in 1:p) {

    X <- as.matrix(X)

    y <- X[,i]
    x <- X[,-i]

    out <- glmnet::cv.glmnet(x = x,
                             y = y,
                             nfolds = nfolds)

    # Save estimates
    coefs_i <- coef(out, s = "lambda.min")
    ints[i] <- coefs_i[1]
    bhat[i, -i] <- coefs_i[-1] # delete intercept

      if(pbar) setTxtProgressBar(pb, i)

  } # end for: p

  # Binarize
  signif <- bhat != 0

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

  # Subset significant estiamates
  bhat_sym_sig <- bhat
  bhat_sym_sig[signif.fin==0] <- 0

  # ----- Return Results -----

  outlist <- list("est" = bhat_sym_sig,
                  "select" = signif.fin,
                  "ints" = ints)

  class(outlist) <- c('qunet', 'list', "lasso")

  return(outlist)

} # eoF
