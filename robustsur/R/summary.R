## prepare summary results that belong to the whole system
summary.surerob <- function(object, residCov=TRUE, equations=TRUE, ...) {

   # if( is.null( useDfSys ) ) {
   #    useDfSys <- length( coef( object ) ) != object$rank
   #       # TRUE if there are restrictions imposed
   # }

   # number of equations
   nEq <- length( object$eq )
   # number of observations per equation
   nObsEq <- rep( NA, nEq )
   for( i in 1:nEq ) {
      nObsEq[ i ] <- length( residuals( object$eq[[ i ]], na.rm = TRUE ) )
   }
   # total number of observations
   nObs <- sum( nObsEq )

   # preparing objects that will be returned
   result <- list()
   result$method <- object$method
   result$residuals <- residuals( object )
   result$residCovEst <- object$residCovEst
   result$residCov <- object$residCov
   if( !is.null( result$residCovEst ) ) {
      dimnames( result$residCovEst ) <- dimnames( result$residCov )
   }
   result$residCor <- cor( residuals( object ), use = "na.or.complete" )
   dimnames( result$residCor ) <- dimnames( result$residCov )
   result$detResidCov <- det( object$residCov, tol = object$control$solvetol )
   result$rweights <- object$rweights

   # now prepare summury results for the individual equations
   result$eq <- list()
   for( i in 1:length( object$eq ) ) {
       result$eq[[i]] <- summary(object$eq[[i]])
       result$eq[[i]]$ssr <- sum( residuals( object$eq[[i]], na.rm = TRUE )^2 )
       result$eq[[i]]$eqnNo <- object$eq[[i]]$eqnNo
       result$eq[[i]]$eqnLabel <- object$eq[[i]]$eqnLabel
   }

   # coefficients, standard errors, ... 
   p <- object$rank
   df <- object$df.residual
   cf.names <- c( "Estimate", "Std. Error", "t value", "Pr(>|t|)" )
   if(p > 0){
      n <- p + df
      se <- sqrt(diag(object$coefCov))
      est <- object$coefficients
      tval <- est/se
      if(!is.null(result$rweights)){
         result$residual_weighted <- result$residuals * sqrt(object$rweights)
      }
      result$df <- c(p, df)
      result$coefficients <- cbind(est, se, tval, 2*pnorm(abs(tval), df, lower.tail = FALSE))
      dimnames(result$coefficients) <- list(names(est), cf.names)
      
      resid <- result$residuals
      pred <- object$fitted.values
      resp <- object$y
      wgt <- object$rweights
      psi <- object$control$psi
      correc <- switch(psi,
                       bisquare = 1.207617,
                       welsh    = 1.224617, # 1.2246131
                       optimal  = 1.068939,
                       hampel   = 1.166891,
                       lqq      = 1.159232,
                       stop('unsupported psi function -- should not happen'))
      resp.mean <-  sum(wgt * resp)/sum(wgt) 
      yMy <- sum(wgt * (resp - resp.mean)^2)
      rMr <- sum(wgt * resid^2)
      result$ssr_weighted <- rMr
      
      result$r.squared <- r2correc <- (yMy - rMr) / (yMy + rMr * (correc - 1))
      result$adj.r.squared <- 1 - (1 - r2correc) * ((n - 1) / df)
      result$coefCov <- object$coefCov
   } 
   # else { ## p = 0: "null model"
   #    result <- object
   #    result$df <- c(0L, df, length(is.na(coef(object))))
   #    result$coefficients <- matrix(result$coefficients[0L], 0L, 4L, dimnames = list(NULL, cf.names))
   #    result$r.squared <- result$adj.r.squared <- 0
   #    result$coefCov <- object$coefCocov
   # }

   result$printResidCov  <- residCov
   result$printEquations <- equations
   result$control <- object$control
   result$call <- object$call 
   class( result ) <- "summary.surerob"
   return( result )
}

## print summary results of the whole system
print.summary.surerob <- function(x,
      digits = max( 3, getOption("digits") - 1 ),
      residCov = x$printResidCov, equations = x$printEquations, ...) {

  table <- NULL
  labels <- NULL

  cat("\n")
  cat("robustsur results \n")
  cat("method: ")
  cat( paste( x$method, "\n\n"))
  

   table.sys <- cbind( round( sum( x$df ),         digits ),
                       round( x$df[2],             digits ),
                       round( x$ssr_weighted, digits ),
                       round( x$detResidCov,       digits ),
                       round( x$r.squared,     digits ),
                       round( x$adj.r.squared,     digits ))
   rownames( table.sys ) <- c( "system" )
   colnames( table.sys ) <- c( "N", "DF", "SSR", "detRCov", "R2", "Adjusted R2")
   print( table.sys, quote = FALSE, right = TRUE, digits = digits )

   cat("\n")

  for(i in 1:length( x$eq ) ) {
    row <- NULL
    row <- cbind( round( sum( x$eq[[i]]$df ),  digits ),
                  round( x$eq[[i]]$df[2], digits ),
                  round( x$eq[[i]]$ssr,   digits ),
                  round( x$eq[[i]]$r.squared,     digits ),
                  round( x$eq[[i]]$adj.r.squared, digits ))
    table  <- rbind( table, row )
    labels <- rbind( labels, x$eq[[i]]$eqnLabel )
  }
  rownames(table) <- c( labels )
  colnames(table) <- c("N","DF", "SSR", "R2", "Adj R2" )
  print(table, quote = FALSE, right = TRUE, digits = digits )

  cat("\n")

   if( residCov ){
      if(!is.null(x$residCovEst)) {
         cat("The covariance matrix of the residuals used for estimation\n")
         print( x$residCovEst, digits = digits )
         cat("\n")
         if( min(eigen( x$residCov, only.values=TRUE)$values) < 0 ) {
            cat("warning: this covariance matrix is NOT positive semidefinit!\n")
            cat("\n")
         }
      }

      cat("The covariance matrix of the residuals\n")
      print( x$residCov, digits = digits )
      cat("\n")

      cat("The correlations of the residuals\n")
      print( x$residCor, digits = digits )
      cat("\n")
   }

   if( equations ){
      ## now print the individual equations
      for(i in 1:length( x$eq ) ) {
         cat("\n")
         cat( x$method, " estimates for '", x$eq[[i]]$eqnLabel,
              "' (equation ", x$eq[[i]]$eqnNo, ")\n", sep = "" )
         
         cat("Model Formula: ")
         print( formula( x$eq[[i]]$terms ) )
         print( x$eq[[i]], digits = digits, showAlgo = FALSE )
      }
   } else {
      cat( "\nCoefficients:\n" )
      printCoefmat( coef( x ), digits = digits )
   }
  invisible( x )
}
