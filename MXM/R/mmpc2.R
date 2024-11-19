mmpc2 <- function(target, dataset, prior = NULL, max_k = 3, threshold = 0.05, 
                  test = "testIndLogistic", ini = NULL, wei = NULL, ncores = 1, backward = FALSE) {
  
  runtime <- proc.time()

 #check for NA values in the dataset and replace them with the variable median or the mode
  if ( any(is.na(dataset)) ) {
    #dataset = as.matrix(dataset);
    warning("The dataset contains missing values (NA) and they were replaced automatically by the variable (column) median (for numeric) or by the most frequent level (mode) if the variable is factor")
    if ( is.matrix(dataset) )  {
      dataset <- apply( dataset, 2, function(x){ x[which(is.na(x))] = median(x, na.rm = TRUE) ; return(x) } ) 
    } else {
      poia <- unique( which( is.na(dataset), arr.ind = TRUE )[, 2] )
      for ( i in poia )  {
        xi <- dataset[, i]
        if ( is.numeric(xi) ) {                    
          xi[ which( is.na(xi) ) ] <- median(xi, na.rm = TRUE) 
        } else if ( is.factor( xi ) ) {
          xi[ which( is.na(xi) ) ] <- levels(xi)[ which.max( as.vector( table(xi) ) )]
        }
        dataset[, i] <- xi
      }
    }
  }

 av_tests <- c("testIndReg", "testIndBeta", "censIndCR", "censIndWR", "testIndClogit", "testIndOrdinal",
              "testIndLogistic", "testIndPois", "testIndNB", "testIndBinom", "auto", "testIndZIP", 
              "testIndRQ", "testIndGamma", "testIndNormLog", "testIndTobit", "testIndQPois", "censIndCR", 
              "censIndWR", "censIndER", "censIndLLR", "testdIndQBinom", "testIndMMReg", "testIndMultinom", 
              "testIndIGreg", "testIndSPML", NULL);
 
 ci_test <- test
   
 if ( ( ci_test == "testIndLogistic" | ci_test == "testIndPois" |  ci_test == "testIndQPois")  &  is.matrix(dataset) ) {
 
 if ( ci_test == "testIndLogistic" ) {
   runtime <- proc.time()
   ep <- Rfast2::mmpc2(y = target, x = dataset, max_k = max_k, threshold = threshold, test = "logistic", parallel = (ncores > 1) )
   kapa_pval <- ep$kapa_pval
   len <- length(kapa_pval)
   if (len > 0 ) {
     for ( i in 1:length(kapa_pval) ) {
       if ( !is.null(kapa_pval[[ i ]]) )    kapa_pval[[ i ]] <- t( kapa_pval[[ i ]] )
     }
     names(kapa_pval) <- paste("kappa=", 1:max_k, sep = "")
   }
   runtime <- proc.time() - runtime
   runtime[3] <- ep$runtime
   sela <- as.numeric(ep$selectedVars)
   
   if ( backward  & length( sela ) > 0  ) {
     tic <- proc.time()
     pv <- pval[sela]
     sela <- sela[ order(pv) ]
     bc <- MXM::mmpcbackphase(target, dataset[, sela, drop = FALSE], test = test, wei = wei, max_k = max_k, threshold = threshold)
     met <- bc$met
     sela <- sela[met]
     pval[sela] <- bc$pvalues
     n.tests <- n.tests + bc$counter
     runtime <- runtime + proc.time() - tic
   } 
   
   res <- list(selectedVars = as.numeric(ep$selectedVars), pvalues = as.numeric(ep$pvalues), uni = as.numeric(ep$univ), 
               kapa_pval = kapa_pval, max_k = ep$max_k, threshold = ep$threshold, n.tests = ep$n.tests, runtime = runtime, test = ci_test)
   
 } else if ( ci_test == "testIndPois" ) {  
   runtime <- proc.time()
   ep <- Rfast2::mmpc2(y = target, x = dataset, max_k = max_k, threshold = threshold, test = "poisson", parallel = (ncores > 1) )
   kapa_pval <- ep$kapa_pval
   len <- length(kapa_pval)
   if (len > 0 ) {
     for ( i in 1:length(kapa_pval) ) {
       if ( !is.null(kapa_pval[[ i ]]) )    kapa_pval[[ i ]] <- t( kapa_pval[[ i ]] )
     }
     names(kapa_pval) <- paste("kappa=", 1:max_k, sep = "")
   }
   runtime <- proc.time() - runtime
   runtime[3] <- ep$runtime
   sela <- as.numeric(ep$selectedVars)
   
   if ( backward  & length( sela ) > 0  ) {
     tic <- proc.time()
     pv <- pval[sela]
     sela <- sela[ order(pv) ]
     bc <- MXM::mmpcbackphase(target, dataset[, sela, drop = FALSE], test = test, wei = wei, max_k = max_k, threshold = threshold)
     met <- bc$met
     sela <- sela[met]
     pval[sela] <- bc$pvalues
     n.tests <- n.tests + bc$counter
     runtime <- runtime + proc.time() - tic
   } 
   
   res <- list(selectedVars = as.numeric(ep$selectedVars), pvalues = as.numeric(ep$pvalues), uni = as.numeric(ep$univ), 
               kapa_pval = kapa_pval, max_k = ep$max_k, threshold = ep$threshold, n.tests = ep$n.tests, runtime = runtime, test = ci_test)
   
 } else if ( ci_test == "testIndQPois" ) {  
   runtime <- proc.time()
   ep <- Rfast2::mmpc2(y = target, x = dataset, max_k = max_k, threshold = threshold, test = "qpoisson", parallel = (ncores > 1) )
   kapa_pval <- ep$kapa_pval
   len <- length(kapa_pval)
   if (len > 0 ) {
     for ( i in 1:length(kapa_pval) ) {
       if ( !is.null(kapa_pval[[ i ]]) )    kapa_pval[[ i ]] <- t( kapa_pval[[ i ]] )
     }
     names(kapa_pval) <- paste("kappa=", 1:max_k, sep = "")
   }
   runtime <- proc.time() - runtime
   runtime[3] <- ep$runtime
   sela <- as.numeric(ep$selectedVars)
 }
 
   if ( backward  & length( sela ) > 0  ) {
     tic <- proc.time()
     pv <- pval[sela]
     sela <- sela[ order(pv) ]
     bc <- MXM::mmpcbackphase(target, dataset[, sela, drop = FALSE], test = test, wei = wei, max_k = max_k, threshold = threshold)
     met <- bc$met
     sela <- sela[met]
     pval[sela] <- bc$pvalues
     n.tests <- n.tests + bc$counter
     runtime <- runtime + proc.time() - tic
   } 
   
   res <- list(selectedVars = sela, pvalues = as.numeric(ep$pvalues), uni = as.numeric(ep$univ), 
               kapa_pval = kapa_pval, max_k = ep$max_k, threshold = ep$threshold, n.tests = ep$n.tests, runtime = runtime, test = ci_test)
   
 } else {  ## end if if ( ( ci_test == "testIndLogistic" | ci_test == "testIndPois" |  ci_test == "testIndQPois")  &  is.matrix(dataset) ) { 
 
   test <- match.arg(test, av_tests, TRUE)
   
   if ( test == "testIndSpearman" )  {
     target <- rank(target)
     dataset <- Rfast::colRanks(dataset)  
     test <- testIndSpearman;  ## Spearman is Pearson on the ranks of the data
   } else if ( test == "testIndMVreg" ) {
     if ( min(target) > 0 & sd( Rfast::rowsums(target) ) == 0 )  target = log( target[, -1]/target[, 1] ) 
     test <- testIndMVreg;
     
   } else if ( test == "testIndSPML" ) {
     test <- testIndSPML
     if ( !is.matrix(target) )   target <- cbind( cos(target), sin(target) )
     
   } else test <- test.maker(test)
   #more tests here
   
  dataset <- as.data.frame(dataset)  
  n.tests <- 0
  alpha <- log(threshold)
  kapa_pval <- NULL
  
  varsize <- dim(dataset)[2]
  if ( ( typeof(max_k) != "double" ) | max_k < 1 )   stop('invalid max_k option');
  if ( max_k > varsize )   max_k <- varsize;
  if ( (typeof(threshold) != "double") | threshold <= 0 | threshold > 1 )   stop('invalid threshold option');
 
  priorindex <- NULL
  if ( !is.null(prior) )   {
    dataset <- cbind(dataset, prior)
    priorindex <- c( c(varsize + 1): dim(dataset)[2] ) 
  }  
   
  if ( is.null(ini) ) {
    if ( is.null(prior) ) {
      mod <- MXM::univregs(target, dataset, test = test, wei = wei, ncores = ncores) 
    } else  mod <- MXM::cond.regs(target, dataset, xIndex = 1:varsize, csIndex = priorindex, 
                                  test = test, wei = wei, ncores = ncores) 
    pval <- mod$pvalue
    univ <- mod$pvalue
    n.tests <- dim(dataset)[2]
  } else pval <- ini$pvalue
  vars <- which(pval < alpha) 
  if ( length(vars) > 0 ) {
    sela <- which.min(pval) 
  } else  sela <- vars
  vars <- setdiff(vars, sela)
  
  ## 1 selected
  if ( length(vars) > 0  &  max_k >= 1 ) {
    a <- paste("kappa=", 1:max_k, sep = "")
    kapa_pval <- sapply(a, function(x) NULL)
    pval2 <- MXM::cond.regs(target, dataset, xIndex = vars, csIndex = c(sela, priorindex), test = test, wei = wei, ncores = 1)$pvalue
    kapa_pval[[ 1 ]] <- rbind(pval2[vars], vars, sela)
    n.tests <- n.tests + length(vars)
    pval[vars] <- pmax(pval[vars], pval2[vars]) 
    ide <- which(pval[vars] < alpha)
    vars <- vars[ide]
    sel <- which.min(pval[vars])
    sela <- c(sela, vars[sel] )
    vars <- setdiff(vars, vars[sel])  
  }  ## end  if ( length(vars) > 0  &  max_k >= 1 ) {

  ## 2 selected
  if ( length(vars) > 0  &  max_k >= 1 ) {
    pval2 <- MXM::cond.regs(target, dataset, xIndex = vars, csIndex = c(sela[2], priorindex), test = test, wei = wei, ncores = 1)$pvalue
    kapa_pval[[ 1 ]] <- cbind( kapa_pval[[ 1 ]], rbind(pval2[vars], vars, sela[2]) )
    n.tests <- n.tests + length(vars)
    pval[vars] <- pmax(pval[vars], pval2[vars]) 
    ide <- which(pval[vars] < alpha)
    vars <- vars[ide]
    if ( length(vars) > 0  &  max_k >= 2 ) {
      pval2 <- MXM::cond.regs(target, dataset, xIndex = vars, csIndex = c(sela, priorindex), test = test, wei = wei, ncores = 1)$pvalue
      kapa_pval[[ 2 ]] <- rbind( pval2[vars], vars, matrix( rep(sela, length(vars)), ncol = length(vars) ) )
      pval[vars] <- pmax(pval[vars], pval2[vars]) 
      ide <- which(pval[vars] < alpha)
      vars <- vars[ide]
    }  ## end  if ( max_k >= 2 ) {
    sel <- which.min(pval[vars])
    sela <- c(sela, vars[sel] )
    vars <- setdiff(vars, vars[sel])    
    dm <- length(sela)
  }  ## end  if ( length(vars) > 0  &  max_k >= 1 ) {

  ## 3 selected  
  if ( length(vars) > 0  &  max_k >= 1 ) {
    if ( max_k >= 1 ) {
      pval2 <- MXM::cond.regs(target, dataset, xIndex = vars, csIndex = c(sela[3], priorindex), test = test, wei = wei, ncores = 1)$pvalue
      kapa_pval[[ 1 ]] <- cbind( kapa_pval[[ 1 ]], rbind(pval2[vars], vars, sela[3]) )
      n.tests <- n.tests + length(vars)
      pval[vars] <- pmax(pval[vars], pval2[vars]) 
      ide <- which(pval[vars] < alpha)
      vars <- vars[ide]
    }  ## end  if ( max_k >= 1 ) {
    if ( length(vars) > 0  &  max_k >= 2 ) {
      cand <- Rfast::comb_n(sela, 2)[, - 1]
      pval2 <- MXM::cond.regs(target, dataset, xIndex = vars, csIndex = c(cand[, 1], priorindex), test = test, wei = wei, ncores = 1)$pvalue
      kapa_pval[[ 2 ]] <- cbind( kapa_pval[[ 2 ]], rbind( pval2[vars], vars, matrix( rep(cand[, 1], length(vars)), ncol = length(vars) ) ) )
      n.tests <- n.tests + length(vars)
      pval[vars] <- pmax(pval[vars], pval2[vars]) 
      ide <- which(pval[vars] < alpha)
      vars <- vars[ide]
      pval2 <- MXM::cond.regs(target, dataset, xIndex = vars, csIndex = c(cand[, 2], priorindex), test = test, wei = wei, ncores = 1)$pvalue
      kapa_pval[[ 2 ]] <- cbind( kapa_pval[[ 2 ]], rbind( pval2[vars], vars, matrix( rep(cand[, 2], length(vars)), ncol = length(vars) ) ) )
      n.tests <- n.tests + length(vars)
      pval[vars] <- pmax(pval[vars], pval2[vars]) 
      ide <- which(pval[vars] < alpha)
      vars <- vars[ide]
    }  ## end  if ( max_k >= 2 ) {
    if ( length(vars) > 0  &  max_k >= 3 ) {
      pval2 <- MXM::cond.regs(target, dataset, xIndex = vars, csIndex = c(sela, priorindex), test = test, wei = wei, ncores = 1)$pvalue
      kapa_pval[[ 3 ]] <- rbind( pval2[vars], vars, matrix( rep(sela, length(vars)), ncol = length(vars) ) )
      n.tests <- n.tests + length(vars)
      pval[vars] <- pmax(pval[vars], pval2[vars]) 
      ide <- which(pval[vars] < alpha)
      vars <- vars[ide]
    }  
    sel <- which.min(pval[vars])
    sela <- c(sela, vars[sel] )
    vars <- setdiff(vars, vars[sel])  
  }  ## end  if ( length(vars) > 0  &  max_k >= 1 ) {  
  
  ## 4 selected
  while ( length(vars) > 0  &  max_k >= 1 ) {
    dm <- length(sela)
    pval2 <- MXM::cond.regs(target, dataset, xIndex = vars, csIndex = c(sela[dm], priorindex), test = test, wei = wei, ncores = 1)$pvalue
    kapa_pval[[ 1 ]] <- cbind( kapa_pval[[ 1 ]], rbind(pval2[vars], vars, sela[dm]) )
    n.tests <- n.tests + length(vars)
    pval[vars] <- pmax(pval[vars], pval2[vars]) 
    ide <- which(pval[vars] < alpha)
    vars <- vars[ide]
    if ( length(vars) > 0  &  max_k >= 2 ) {
      for ( i in 2:min( max_k, length(sela) ) ) {  
        cand <- Rfast::comb_n(sort(sela), i)
        j <- 1
        while ( length(vars) > 0  &  j <= dim(cand)[2] ) {
          pval2 <- MXM::cond.regs(target, dataset, xIndex = vars, csIndex = c(cand[, j], priorindex), test = test, wei = wei, ncores = 1)$pvalue
          kapa_pval[[ i ]] <- cbind( kapa_pval[[ i ]], rbind( pval2[vars], vars, matrix( rep(cand[, j], length(vars)), ncol = length(vars) ) ) )
          n.tests <- n.tests + length(vars)
          pval[vars] <- pmax(pval[vars], pval2[vars]) 
          ide <- which(pval[vars] < alpha)
          vars <- vars[ide]
          j <- j + 1
        }  ## end  while ( length(vars) > 0  &  j <= dim(cand)[2] ) {
      }  ## end  for ( i in 2:max_k ) {  
      sel <- which.min(pval[vars])
      sela <- c(sela, vars[sel] )
      vars <- setdiff(vars, vars[sel]) 
    }  ## end  if ( length(vars) > 0  &  max_k >= 2 ) {
  } ## end  while ( length(vars) > 0  &  max_k >= 1 ) {
    
  if ( backward  & length( sela ) > 0  ) {
    pv <- pval[sela]
    sela <- sela[ order(pv) ]
    bc <- MXM::mmpcbackphase(target, dataset[, sela, drop = FALSE], test = test, wei = wei, max_k = max_k, threshold = threshold)
    met <- bc$met
    sela <- sela[met]
    pval[sela] <- bc$pvalues[met]
    n.tests <- n.tests + bc$counter
  }  ## end  if ( backward  & length( sela ) > 0  ) { 
  
  runtime <- proc.time() - runtime 
  if ( length(kapa_pval) > 0 ) {
    for ( i in 1:length(kapa_pval) ) {
      if ( length(kapa_pval[[ i ]]) > 0 ) {
        rownames(kapa_pval[[ i ]]) <- c("log_pvalue", "variable", paste("cond_var=", 1:i, sep = "") )
      } else  kapa_pval[[ length(kapa_pval)]] <- NULL  
    }  
  }  ##  end  if ( length(kapa_pval) > 0 )     
  res <- list(selectedVars = sela, pvalues = pval, uni = univ, kapa_pval = kapa_pval, max_k = max_k, threshold = alpha, 
      n.tests = n.tests, runtime = runtime, test = ci_test)
 }

 res
}





