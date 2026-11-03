mmpc.gee2 <- function(target, reps = NULL, group, dataset, prior = NULL, max_k = 3, threshold = 0.05, test = NULL, 
                      ini = NULL, wei = NULL, correl = "exchangeable", se = "jack", ncores = 1) {
  
  runtime <- proc.time()

  #check for NA values in the dataset and replace them with the variable median or the mode
  if ( any(is.na(dataset)) ) {
    #dataset = as.matrix(dataset);
    warning("The dataset contains missing values (NA) and they were replaced automatically by the variable (column) median (for numeric) or by the most frequent level (mode) if the variable is factor")
    dataset <- apply( dataset, 2, function(x){ x[which(is.na(x))] = median(x, na.rm = TRUE) ; return(x) } ) 
  }
  
  la <- length( unique( as.numeric(target) ) )
  #auto detect independence test in case of not defined by the user and the test is null or auto
  if ( is.null(test) )  {
    if ( la > 2 & sum(target - round(target) ) != 0  ) {
      test <- "testIndGEEReg"
    } else if (la == 2) {
      test <- "testIndGEELogistic"
    } else if ( la > 2  &  sum( target - round(target) ) == 0 ) {
      test <- "testIndGEEPois"
    } 
  }  
  #available conditional independence tests
  av_tests = c("testIndGEEeg", "testIndGEELogistic", "testIndGEEPois", "testIndGEEGamma", 
               "testIndGEENormLog", NULL);
  
  ci_test <- test
  if ( length(test) == 1 ) {   #avoid vectors, matrices etc
    test <- match.arg(test, av_tests, TRUE);
    #convert to closure type
    if ( test == "testIndGEEReg" ) {
      test <- testIndGEEReg;
    } else if ( test == "testIndGEELogistic" ) {
      test <- testIndGEELogistic;
    } else if ( test == "testIndGEEPois" ) {
      test <- testIndGEEPois;
    } else if ( test == "testIndGEEGamma" ) {
      test <- testIndGEEGamma;
    } else if ( test == "testIndGEENormLog" ) {
      test <- testIndGEENormLog;
    }
  } else   stop('invalid test option');
  
  n.tests <- 0
  alpha <- log(threshold)
  
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
      mod <- MXM::gee.univregs(target = target, reps = reps, id = group, dataset = dataset, targetID = -1, test = test, 
                               wei = wei, correl = correl, se = se, ncores = ncores)
    } else mod <- MXM::gee.condregs(target = target, reps = reps, id = group, dataset = dataset, xIndex = 1:varsize, 
                                    csIndex = priorindex, test = test, wei = wei, correl = correl, se = se, ncores = ncores)
      
    
    pval <- mod$pvalue
    n.tests <- varsize
  } else pval <- ini$pvalue
  vars <- which(pval < alpha)  
  sela <- which.min(pval)
  vars <- setdiff(vars, sela)
  
  ## 1 selected
  if ( length(vars) > 0  &  max_k >= 1 ) {
    a <- paste("kappa=", 1:max_k, sep = "")
    kapa_pval <- sapply(a, function(x) NULL)
    pval2 <- MXM::gee.condregs(target = target, reps = reps, id = group, dataset = dataset, xIndex = vars, 
                               csIndex = c(sela, priorindex), test = test, wei = wei, correl = correl, se = se, ncores = 1)$pvalue 
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
    pval2 <- MXM::gee.condregs(target = target, reps = reps, id = group, dataset = dataset, xIndex = vars, 
                          csIndex = c(sela[2], priorindex), test = test, wei = wei, correl = correl, se = se, ncores = 1)$pvalue 
    kapa_pval[[ 1 ]] <- cbind( kapa_pval[[ 1 ]], rbind(pval2[vars], vars, sela[2]) )
    n.tests <- n.tests + length(vars)
    pval[vars] <- pmax(pval[vars], pval2[vars]) 
    ide <- which(pval[vars] < alpha)
    vars <- vars[ide]
    if ( length(vars) > 0  &  max_k >= 2 ) {
      pval2 <- MXM::gee.condregs(target = target, reps = reps, id = group, dataset = dataset, xIndex = vars, 
                                 csIndex = c(sela, priorindex), test = test, wei = wei, correl = correl, se = se, ncores = 1)$pvalue 
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
      pval2 <- MXM::gee.condregs(target = target, reps = reps, id = group, dataset = dataset, xIndex = vars, 
                            csIndex = c(sela[3], priorindex), test = test, wei = wei, correl = correl, se = se, ncores = 1)$pvalue 
      kapa_pval[[ 1 ]] <- cbind( kapa_pval[[ 1 ]], rbind(pval2[vars], vars, sela[3]) )
      n.tests <- n.tests + length(vars)
      pval[vars] <- pmax(pval[vars], pval2[vars]) 
      ide <- which(pval[vars] < alpha)
      vars <- vars[ide]
    }  ## end  if ( max_k >= 1 ) {
    if ( length(vars) > 0  &  max_k >= 2 ) {
      cand <- Rfast::comb_n(sela, 2)[, - 1]
      pval2 <- MXM::gee.condregs(target = target, reps = reps, id = group, dataset = dataset, xIndex = vars, 
                                 csIndex = c(cand[, 1], priorindex), test = test, wei = wei, correl = correl, se = se, ncores = 1)$pvalue 
      kapa_pval[[ 2 ]] <- cbind( kapa_pval[[ 2 ]], rbind( pval2[vars], vars, matrix( rep(cand[, 1], length(vars)), ncol = length(vars) ) ) )
      n.tests <- n.tests + length(vars)
      pval[vars] <- pmax(pval[vars], pval2[vars]) 
      ide <- which(pval[vars] < alpha)
      vars <- vars[ide]
      pval2 <- MXM::gee.condregs(target = target, reps = reps, id = group, dataset = dataset, xIndex = vars, 
                                 csIndex = c(cand[, 2], priorindex), test = test, wei = wei, correl = correl, se = se, ncores = 1)$pvalue 
      kapa_pval[[ 2 ]] <- cbind( kapa_pval[[ 2 ]], rbind( pval2[vars], vars, matrix( rep(cand[, 2], length(vars)), ncol = length(vars) ) ) )
      n.tests <- n.tests + length(vars)
      pval[vars] <- pmax(pval[vars], pval2[vars]) 
      ide <- which(pval[vars] < alpha)
      vars <- vars[ide]
    }  ## end  if ( max_k >= 2 ) {
    if ( length(vars) > 0  &  max_k >= 3 ) {
      pval2 <- MXM::gee.condregs(target = target, reps = reps, id = group, dataset = dataset, xIndex = vars, 
                                 csIndex = c(sela, priorindex), test = test, wei = wei, correl = correl, se = se, ncores = 1)$pvalue 
      kapa_pval[[ 3 ]] <- rbind( pval2[vars], vars, matrix( rep(sela, length(vars)), ncol = length(vars) ) )
      n.tests <- n.tests + length(vars)
      pval[vars] <- pmax(pval[vars], pval2[vars]) 
      ide <- which(pval[vars] < alpha)
      vars <- vars[ide]
    }  ## end  if ( max_k >= 3 ) {
    sel <- which.min(pval[vars])
    sela <- c(sela, vars[sel] )
    vars <- setdiff(vars, vars[sel])  
  }  ## end  if ( length(vars) > 0  &  max_k >= 2 ) {  
  
  ## 4 selected
  while ( length(vars) > 0  &  max_k >= 1 ) {
    dm <- length(sela)
    pval2 <- MXM::gee.condregs(target = target, reps = reps, id = group, dataset = dataset, xIndex = vars, 
                               csIndex = c(sela[dm], priorindex), test = test, wei = wei, correl = correl, se = se, ncores = 1)$pvalue 
    kapa_pval[[ 1 ]] <- cbind( kapa_pval[[ 1 ]], rbind(pval2[vars], vars, sela[dm]) )
    n.tests <- n.tests + length(vars)
    pval[vars] <- pmax(pval[vars], pval2[vars]) 
    ide <- which(pval[vars] < alpha)
    vars <- vars[ide]
    if ( length(vars) > 0  &  max_k >= 2 ) {
      for ( i in 2:min( max_k, length(sela) ) ) {  
        cand <- Rfast::comb_n(sort(sela), i)
        j <- 1
        while ( length(vars) > 0  &  j < dim(cand)[2] ) {
          pval2 <- MXM::gee.condregs(target = target, reps = reps, id = group, dataset = dataset, xIndex = vars, 
                                     csIndex = c(cand[, j], priorindex), test = test, wei = wei, correl = correl, se = se, ncores = 1)$pvalue 
          kapa_pval[[ i ]] <- cbind( kapa_pval[[ i ]], rbind( pval2[vars], vars, matrix( rep(cand[, j], length(vars)), ncol = length(vars) ) ) )
          n.tests <- n.tests + length(vars)
          pval[vars] <- pmax(pval[vars], pval2[vars]) 
          ide <- which(pval[vars] < alpha)
          vars <- vars[ide]
          j <- j + 1
        }  ## end  for ( j in 1:dim(cand)[2] ) {
      }  ## end  for ( i in 2:max_k ) {  
    }  ## end  if ( max_k >= 2 ) {
    sel <- which.min(pval[vars])
    sela <- c(sela, vars[sel] )
    vars <- setdiff(vars, vars[sel])  
  } ## end  while ( length(vars) > 0  &  max_k >= 1 ) {
  
  runtime <- proc.time() - runtime
  # if ( backward  & length( sela ) > 0  ) {
  #   tic <- proc.time()
  #   pv <- pval[sela]
  #   sela <- sela[ order(pv) ]
  #   bc <- mmpcbackphase(target, dataset[, sela, drop = FALSE], test = test, wei = wei, max_k = max_k, threshold = threshold)
  #   met <- bc$met
  #   sela <- sela[met]
  #   pvalues[sela] <- bc$pvalues
  #   n.tests <- n.tests + bc$counter
  #   runtime <- runtime + proc.time() - tic
  # }  
  list(selectedVars = sela, pvalues = pval, univ = mod$pvalue, kapa_pval = kapa_pval, max_k = max_k, threshold = alpha, 
       n.tests = n.tests, runtime = runtime, test = ci_test, correl = correl, se = se)
}





