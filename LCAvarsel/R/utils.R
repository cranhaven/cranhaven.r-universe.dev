maxG <- function(Y, Gvec)
{
  # show warnings
  warn <- getOption("warn")
  if( warn < 0 ) options("warn" = 0)
    
  # N <- nrow(x)
  M <- ncol(Y)
  N <- sum( !duplicated(Y) )
  #
  nLev <- sapply( Y, function(v) length(table(v)) )
  npar <- sum( max(Gvec)*(nLev - 1) ) + max(Gvec) - 1
  Prod <- prod(nLev)
  check <- Prod > max(Gvec)*( sum(nLev) - M + 1 ) & N > npar
  if (check) {
    Gmax <- max(Gvec)
  } else {
    if ( N <= npar ) {
      set <- N > Gvec*sum(nLev - 1) + (Gvec - 1)
      if ( all(!set) ) {
        warning("no LCA model is identifiable for such value(s) of G")
        return(NULL)
      }
      Gmax <- max( Gvec[set] )
    } else {
      Gmax <- floor( Prod / (sum(nLev) - M + 1) )
    }
  }
  return( if ( Gmax > max(Gvec) ) min(Gvec):max(Gvec) else min(Gvec):Gmax )
}



compareCluster <- function(class1, class2)
{
  if ( length(class1) != length(class2) )
    stop( "arguments must be vectors of the same length" )
  tab <- as.matrix( table(class1, class2) )
  n <- sum(tab)
  dm <- dim(tab)
  
  n11 <- sum( choose(tab, 2) )      # comembership
  n01 <- sum( choose(rowSums(tab), 2) ) - n11
  n10 <- sum( choose(colSums(tab), 2) ) - n11
  n00 <- choose( sum(tab), 2 ) - n11 - n10 - n01
  
  Jacc <- (n11)/(n11 + n01 + n10)
  RI <- (n11 + n00)/(n11 + n01 + n10 + n00)
  ARI <- (n11 - (n11 + n01) * (n11 + n10)/(n11 + n01 + n10 + n00))/
    ( (n11 + n01 + n11 + n10)/2 - (n11 + n01) * (n11 + n10)/(n11 + n01 + n10 + n00) )
  
  p <- prop.table(tab)
  p1 <- rowSums(tab)/n
  p2 <- colSums(tab)/n
  p1 <- p1[p1 > 0]
  p2 <- p2[p2 > 0]
  h1 <- -sum( p1 * log(p1) )
  h2 <- -sum( p2 * log(p2) )
  icc <- 0
  for ( i in 1:dm[1] ) {   # not very elegant
    for ( j in 1:dm[2] ) {
      if( p[i,j] > 0 ) icc <- icc + p[i,j] * log( p[i,j]/(p1[i]*p2[j]) )
    }
  }
  VI <- h1 + h2 - 2*icc
  if ( VI < sqrt(.Machine$double.eps) ) VI <- 0
  names(VI) <- NULL
  return( list(tab = tab, jaccard = Jacc, randIndex = RI, adjRandIndex = ARI, varInfo = VI) )
}



monitor <- function(info, iter)
{
  n <- nrow(info) - 1
  info[,4] <- suppressWarnings( 
    as.numeric( format(info[,4], digits = 0, nsmall = 2) ) 
  )    # NA
  # if ( any(is.na(info[,4])) ){           #### problem with spacing, to be fixed
  #   set <- which( is.na(info[,4]) )
  #   na <- rep("", n+1)
  #   na[set] <- "Step not performed"
  #   info <- cbind(info, na)
  # }
  cp <- capture.output( print(info, row.names = FALSE) )
  tmp <- paste0(cp, "\n")
  if (iter == 1) cat( paste(tmp[1], collapse = "") )
  cat( paste(tmp[iter+2], collapse = "") )
  flush.console()
}



# monitorGA <- function(object, digits = getOption("digits"), ...)
# {
#   fitness <- na.exclude(object@fitness)
#   sumryStat <- c( mean(fitness), max(fitness) )
#   sumryStat <- format( sumryStat, digits = digits )
#   replicate( 1, GA::clearConsoleLine() )
#   # cat(paste("\rGA | iter =", object@iter, "\n"))
#   # cat(paste("Mean =", sumryStat[1], "| Best =", sumryStat[2], "\n"))
#   cat( paste("\rGA search | Iter =", object@iter,
#              "    Mean =", sumryStat[1], "| Best =", sumryStat[2], "\n") )
#   flush.console()
# }
monitorGA <- function(object, digits = getOption("digits"), ...)
{
  if ( object@iter%%10 == 0 ){
    # print every 10 iterations
    fitness <- na.exclude(object@fitness)
    sumryStat <- c( mean(fitness), max(fitness) )
    sumryStat <- format( sumryStat, digits = digits )
    mx <- which.max(object@fitness)
    sol <- as.logical( object@population[mx,] )
    cat( paste("\n GA search | Iter =", object@iter,
               "    Mean =", sumryStat[1], "| Best =", sumryStat[2], "\n") )
    cat( "   Clustering set:", object@names[sol], "\n" )
  }
}
