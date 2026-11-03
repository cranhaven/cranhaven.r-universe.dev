glm.bsreg2 <- function(target, dataset, threshold = 0.05, wei = NULL, test = NULL) {
  
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

      runtime <- proc.time()
      dataset <- as.data.frame(dataset)
      ci_test <- test
      
      if ( test == "testIndGamma" )  {
        type <- Gamma(link = log)
      } else if ( test == "testIndNormLog" )  {
        type <- gaussian(link = log)
      } else if ( test == "testIndQPois" )  {
        type <- quasipoisson(link = log)
      } else if ( test == "testIndQBinom" )  {
        type <- quasibinomial(link = logit)
      }
	  poio <- 4  
      if ( test == "testIndQBinom"  |  test == "testIndQPois" )  poio <- 3 
      ini <- glm( target ~.,  data = dataset, family = type, weights = wei, y = FALSE, model = FALSE )
      dofini <- length( ini$coefficients )
      tab <- drop1( ini, test = "F" )
      dof <- tab[-1, 1]
      stat <- tab[-1, poio]
        
      mat <- cbind(1:p, pf( stat, dofini - dof, n - dof, lower.tail = FALSE, log.p = TRUE), stat )
      colnames(mat) <- c("variable", "log.p-values", "statistic" )
      rownames(mat) <- 1:p 
        
      sel <- which.max( mat[, 2] )
      info <- matrix( c(0, -10, -10) , ncol = 3 )
      sela <- sel 
        
        if ( mat[sel, 2] < threshold ) {
          runtime <- proc.time() - runtime
          res <- list(runtime = runtime, info = matrix(0, 0, 3), mat = mat, ci_test = ci_test, final = ini ) 
          
        } else {
          
          info[1, ] <- mat[sel, ]
          mat <- mat[-sel, , drop = FALSE] 
          dat <- dataset[, -sel, drop = FALSE] 

          i <- 1  
        
            while ( info[i, 2] > threshold  &  dim(dat)[2] > 0 )  {   
              i <- i + 1
              k <- p - i + 1
              ini <- glm( target ~., data = dat, family = type, weights = wei, y = FALSE, model = FALSE )
              dofini <- length(ini$coefficients)
              if ( k == 1 ) {
                mod <- glm(target ~ 1, data = dat, family = type, weights = wei)
                dof <- length( mod$coefficients ) 
                stat <- (mod$deviance - ini$deviance) / (dofini - dof) / summary(mod)[[ 14 ]]
                pval <- pf( stat, dofini - dof, n - dof, lower.tail = FALSE, log.p = TRUE)
                
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
                
                tab <- drop1( ini, test = "F" )
                dof <- tab[-1, 1]
                stat <- tab[-1, poio]
                mat[, 2:3] <- cbind( pf(stat, dof, n - (dofini - dof), lower.tail = FALSE, log.p = TRUE), stat )
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

        runtime <- proc.time() - runtime		
        info <- info[ info[, 1] > 0, , drop = FALSE]
        res <- list( runtime = runtime, info = info, mat = mat, ci_test = ci_test, final = final ) 
          
    }  
  }  
  res

}    






