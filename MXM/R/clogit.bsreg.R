clogit.bsreg <- function(target, dataset, threshold = 0.05, wei = NULL) {
  
  threshold <- log(threshold)
  id <- target[, 2] #the patient id
  case <- as.logical(target[, 1])
  
  dm <- dim(dataset)
  if ( is.null(dm) ) {
    n <- length(dataset)
    p <- 1
  } else {
    n <- dm[1]  ## sample size 
    p <- dm[2]  ## number of variables
  }  
  if ( p > n ) {
    res <- paste("The number of variables is hiher than the sample size. 
                 No backward procedure was attempted")
  } else {

    runtime <- proc.time()
    dataset <- as.data.frame(dataset)
    if ( p == 1 ) {
      ini <- survival::clogit( case~. + strata(id),  data = dataset )
      stat <- 2 * abs( diff(ini$loglik) )
      dof <- length( ini$coefficients )
    } else {
      ini <- survival::clogit( case ~. + strata(id),  data = dataset )
      dofini <- length( coef(ini) )
      stat <- dof <- numeric(p)
      for (i in 1:p) {
        mod <- survival::clogit( case ~. + strata(id),  data = dataset[, -i, drop = FALSE] )
        stat[i] <- 2 * ( logLik(ini) - logLik(mod) )
        dof[i] <- dofini - length( mod$coefficients ) 
      }
    }
    mat <- cbind(1:p, pchisq( stat, dof, lower.tail = FALSE, log.p = TRUE), stat )
    colnames(mat) <- c("variable", "log.p-values", "statistic" )
    rownames(mat) <- 1:p 
    
    sel <- which.max( mat[, 2] )
    info <- matrix( c(0, -10, -10) , ncol = 3 )
    
    if ( mat[sel, 2] < threshold ) {
      res <- list(mat = mat, final = ini  ) 
      
    } else {
      
      info[1, ] <- mat[sel, ]
      mat <- mat[-sel, , drop = FALSE] 
      dat <- dataset[, -sel, drop = FALSE] 
    } 
    
    i <- 1  
    
        while ( info[i, 2] > threshold  &  NCOL(dat) > 0 )  {   
          i <- i + 1
          k <- p - i + 1
          ini <- survival::clogit( case ~ . + strata(id), data = dat )
          
          if ( k == 1 ) {
            mod <- survival::clogit( case ~ . + strata(id),  data = dataset )
            stat <- 2 * abs( diff(mod$loglik) )
            dof <- length( ini$coefficients )
            pval <- pchisq( stat, dof, lower.tail = FALSE, log.p = TRUE)
            
            if (pval > threshold ) {
              final <- "No variables were selected"
              info <- rbind(info, c(mat[, 1], pval, stat) )
              dat <- dataset[, -info[, 1], drop = FALSE ]
              mat <- NULL
            } else {
              info <- rbind(info, c(0, -10, -10)) 
              final <- ini
              mat[, 2:3] <- c(pval, stat)
            }
            
          } else {
            
            stat <- dof <- numeric(k)
            
            for (j in 1:k) {
              mod <- survival::clogit( case ~ . + strata(id),  data = dat[, -j, drop = FALSE] )
              stat[j] <- 2 * ( logLik(ini) - logLik(mod) )
              dof[j] <- length( ini$coefficients ) - length( mod$coefficients ) 
            }
            mat[, 2:3] <- cbind( pchisq( stat, dof, lower.tail = FALSE, log.p = TRUE), stat )
            sel <- which.max( mat[, 2] )
            
            if ( mat[sel, 2] < threshold ) {
              final <- ini
              info <- rbind(info, c(0, -10, -10) )
              
            } else {
              info <- rbind(info, mat[sel, ] )
              mat <- mat[-sel, , drop = FALSE] 
              dat <- dataset[, -info[, 1], drop = FALSE ]
            }
          }  
        }  ## end while

    info <- info[ info[, 1] > 0, , drop = FALSE ]
    runtime <- proc.time() - runtime
    res <- list(runtime = runtime, info = info, mat = mat, ci_test = "testIndClogit", final = final ) 
  }  ## end if ( p > n ) 
  res
}    






