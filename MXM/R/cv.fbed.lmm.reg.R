cv.fbed.lmm.reg <- function(target, dataset, id, prior = NULL, kfolds = 10, folds = NULL, 
                            alphas = c(0.01, 0.05), ks = 0:2) {
  
  if ( is.null(alphas) )   alphas <- c(0.01, 0.05)
  if ( is.null(ks) )  ks <- 0:2
  las <- length(alphas)
  lks <- length(ks)
  id <- as.numeric(id)
  
  groups <- 1:length( table(id) ) 
  if ( is.null(folds) ) {
    folds <- MXM::generatefolds(groups, nfolds = kfolds, stratified = FALSE, seed = FALSE)
  }
  kfolds <- length(folds)
  vars <- array(0, dim = c(las, lks, kfolds) )
  cv <- array( dim = c(las, lks, kfolds) )
  rownames(vars) <- paste("alpha=", alphas, sep = "")   ;    rownames(cv) <- paste("alpha=", alphas, sep = "")
  colnames(vars) <- paste("K=", ks, sep = "")           ;    colnames(cv) <-  paste("K=", ks, sep = "")
  
  tic <- proc.time()
  
  for ( i in 1:kfolds ) {
    
    poia <- which( id == folds[[ i ]] )
    train_set <- dataset[ -poia, ] #Set the training set
    train_target <- target[ -poia ]
    train_id <- id[ -poia ] 
    train_prior <- prior[ -poia, ] #Set the training set
    
    test_set <- dataset[ poia, ] #Set the validation set
    test_target <- target[ poia ]
    test_id <- id[ poia ] 
    test_prior <- prior[ -poia, ] #Set the training set
    fbed_ini <- NULL

    for ( vim in 1:length(alphas) ) {
      
      results <- MXM::fbed.glmm.reg(train_target, train_set, id = train_id, prior = train_prior, ini = fbed_ini, threshold = alphas[vim], K = ks) 
      fbed_ini <- results$res$univ
      res <- results$mod
      ep <- numeric(lks)
      aa <- rep(NA, lks)
      
      if ( length(res) > 0 ) {
        for ( j in 1:length(res) ) {
          ep[j] <- dim(res[[ j ]])[1]
          vars[vim, , i] <- ep
          ma <- Rfast::rint.reg( train_target, cbind(train_set[, res[[ j ]][, 1] ], train_prior), train_id )
          er <- test_target - as.vector( cbind(1, test_set[, res[[ j ]][, 1] ], test_prior) %*% ma$be )
          W <- matrix( ma$info[2]/ma$info[3], nrow = length(test_id), ncol = length(test_id) )
          diag(W) <- diag(W) + 1
          aa[j] <- Rfast::rowmeans( er %*% solve(W, er) )
          cv[vim, , i] <- aa
        }  ##  end  for ( j in 1:length(res) ) {
      }  ##  end  if ( length(res) > 0 )  {  
    }  ##  end  for ( vim in 1:length(alphas) ) {
  }  ##  end  for ( i in 1:kfolds ) {
 
  runtime <- proc.time() - tic
  
  perf <- apply(cv, 1:2, mean, na.rm = TRUE)
  poia <- which( perf == min(perf,na.rm = TRUE),arr.ind = TRUE )
  best <- c( alphas[ poia[1] ], ks[ poia[2] ] )
  names(best) <- c("alpha", "K")
  list(vars = vars, cv = cv, perf = perf, best = best, runtime = runtime)
  
}

