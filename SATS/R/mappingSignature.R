
MappingSignature <- function(W_hat, W_ref=NULL, niter=100, cutoff.I2=0.1, min.repeats=80, COSMICv="v3.4"){
  ## Description
  # This function finds a subset of TMB-based catalog SBS signatures whose linear combination approximate 
  # {\it de novo} SBS signatures detected by signeR
  
  ## Arguments
  # W_hat: de novo signatures from signeR 
  # W_ref: TMB-based catalog signatures (SimData$W_TMB)
  
  # Check for errors with inputs
  check_mat_df(W_hat, "W_hat")
  if (!is.null(W_ref)) check_mat_df(W_ref, "W_ref")
  check_number(niter, "niter", min=1)
  check_number(cutoff.I2, "cutoff.I2", pos=TRUE)
  check_number(min.repeats, "min.repeats", min=1, max=niter)
  check_COSMICv(COSMICv)
  SimData <- NULL

  if (is.null(W_ref)) W_ref <- get_W_ref(COSMICv)

  ## Value
  # selected TMB-based catalog signatures with coefficient I^2 greater than cutoff.I2 
  #   in more than min.repeats repeats
  
  signeR_W_norm <- apply(as.matrix(W_hat), 2, function(x) x/sum(x))
  r <- ncol(signeR_W_norm)
  n_ref <- dim(W_ref)[2]
  
  ## glment X and y
  X <- kronecker(diag(1, r), as.matrix(W_ref))
  y <- as.vector(signeR_W_norm)
  
  reg.sig.all <- NULL
  sig_found   <- NULL
  singeR      <- NULL
  for(rep in 1:niter){
    cvfit <- cv.glmnet(X, y, lower.limits = 0, intercept = FALSE) 
    out   <- coef(cvfit, s = "lambda.min")
    dimnames(out)[[1]][-1] <- rep(colnames(W_ref),r) 
    reg <- data.frame(SBS = rownames(out)[-1], coeff = out[-1,1], singeR = rep(1:r, each = n_ref))
    #reg.sig <- reg %>% arrange(singeR, desc(coeff)) %>% group_by(singeR) %>% mutate(cum_sum = cumsum(coeff)) %>% filter(coeff > cutoff.I2)
    reg.sig <- filt1(reg, cutoff.I2)

    if(dim(reg.sig)[1]!=0){
      reg.sig.all <- rbind(reg.sig.all, cbind(rep,sort(unique(reg.sig$SBS))))
    }
  }
  if (!is.null(reg.sig.all)) {
    tab             <- table(reg.sig.all[,2])
    sig_found       <- data.frame(Reference = names(tab), freq = as.numeric(tab))
    tmp             <- sig_found[, "freq", drop=TRUE] >= min.repeats
    tmp[is.na(tmp)] <- FALSE
    sig_found       <- sig_found[tmp, , drop=FALSE] 
  }
  return(sig_found)
}

filt1 <- function(reg, cutoff.I2) {

  ret   <- as.data.frame(reg, stringsAsFactors=FALSE)
  ord   <- order(ret[, "coeff", drop=TRUE], decreasing=TRUE)
  ret   <- ret[ord, , drop=FALSE]
  ord   <- order(ret[, "singeR", drop=TRUE], decreasing=FALSE)
  ret   <- ret[ord, , drop=FALSE]
  grps  <- ret[, "singeR", drop=TRUE]
  ugrps <- unique(grps)
  ngrps <- length(ugrps)
  ret[, "cum_sum"] <- NA
  for (i in 1:ngrps) {
    tmp                 <- grps %in% ugrps[i]
    ret[tmp, "cum_sum"] <- cumsum(ret[tmp, "coeff", drop=TRUE])
  }  
  tmp <- ret[, "coeff", drop=TRUE] > cutoff.I2
  tmp[is.na(tmp)] <- FALSE
  ret <- ret[tmp, , drop=FALSE]
  ret

}

get_W_ref <- function(COSMICv) {

  # Function to load data

  #"SBS_refSigs"  "DBS_refSigs"  "TMB_DBS_v3.2" "TMB_SBS_v3.2" "TMB_DBS_v3.4"
  # "TMB_SBS_v3.4"
  
  RefTMB       <- NULL
  dir          <- system.file("data", package="SATS", mustWork=TRUE)
  f            <- file.path(dir, "RefTMB.rda")
  tmp          <- load(f)
  SBS_refSigs  <- NULL 
  DBS_refSigs  <- NULL 
  TMB_DBS_v3.2 <- NULL
  TMB_DBS_v3.4 <- NULL

  if (COSMICv == "v3.2") {
    nm <- "TMB_SBS_v3.2"
  } else {
    nm <- "TMB_SBS_v3.4"
  }
  ret <- RefTMB[[nm, exact=TRUE]]
  if (is.null(ret)) stop("ERROR loading data")
    
  
  ret
}
