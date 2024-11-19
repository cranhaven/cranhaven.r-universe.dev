cor.bsreg <- function(target, dataset, threshold = 0.05) {

  tic <- proc.time() 
  threshold <- log(threshold)
  dm <- dim(dataset)
  if ( is.null(dm) ) {
    n <- length(target)
    p <- 1
  } else {
    n <- dm[1]  ## sample size 
    p <- dm[2]  ## number of variables
  }  
  if ( p > n ) {
    res <- paste("The number of variables is hiher than the sample size. 
                 No backward procedure was attempted")
  } else {

    dataset <- as.data.frame(dataset)
    ci_test <- "testIndFisher"  
    mat <- cor.drop1(target, dataset, logged = TRUE)
    mat <- cbind(1:p, mat[, 2], mat[, 1] )
    colnames(mat) <- c("variable", "log.p-values", "statistic" )
    rownames(mat) <- 1:p 
    sel <- which.max( mat[, 2] )  
    info <- matrix( c(0, -10, -10) , ncol = 3 )
    sela <- sel 
    
    if ( mat[sel, 2] < threshold ) {
      runtime <- proc.time() - tic
      res <- list(runtime = runtime, info = matrix(0, 0, 3), mat = mat, ci_test = ci_test, final = lm(target ~., dataset) ) 
          
    } else {
      info[1, ] <- mat[sel, ]
      mat <- mat[-sel, , drop = FALSE] 
      dat <- dataset[, -sel, drop = FALSE] 
      final <- "No variables were selected"
    
      i <- 1  
          
        while ( info[i, 2] > threshold  &  dim(dat)[2] > 0 )  {   
          i <- i + 1
          k <- p - i + 1
          
          mod <- cor.drop1(target, dat, logged = TRUE)
          stat <- mod[, 1]
          pval <- mod[, 2]
          ini <- lm(target ~., dat)        
          if ( k == 1 ) {
            if (pval > threshold ) {
              final <- "No variables were selected"
              info <- rbind(info, c(mat[, 1], pval, stat) )
              dat <- dataset[, -info[, 1], drop = FALSE ]
              mat <- NULL
            } else {
              info <- rbind( info, c(0, -10, -10) ) 
              final <- ini
              mat[, 2:3] <- c(pval, stat)
            }  
          } else {
            mat[, 2:3] <- cbind( pval, stat )
            sel <- which.max( mat[, 2] )
            if ( mat[sel, 2] < threshold ) {
              final <- ini
              info <- rbind(info, c(0, -10, -10) )
            } else {
              info <- rbind(info, mat[sel, ] )
              mat <- mat[-sel, , drop = FALSE] 
              dat <- dataset[, -info[, 1], drop = FALSE] 
            }
          }
        }

        runtime <- proc.time() - tic		
        info <- info[ info[, 1] > 0, , drop = FALSE]
        res <- list(runtime = runtime, info = info, mat = mat, ci_test = ci_test, final = final ) 
          
    }  ## end if ( mat[sel, 2] < threshold ) 
    
  }  ## end if ( p > n )   
  res
}    






