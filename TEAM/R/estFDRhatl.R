#' Calculate FDR
#'
#' Step-down multiple-testing procedure
#'
#' @param min.x lower limit of searching range
#' @param max.x upper limit of searching range
#' @param c.prev Previous c.hat from layer l-1
#' @param n.l Vector of number of pooled observations in layer l bins
#' @param x.l Vector of case counts in each bin at layer l
#' @param theta0 Nominal boundary level for binomial parameter at layer 1
#' @param l Layer
#'
#' @return NULL
#'
est.FDR.hat.l <- function(min.x,max.x,c.prev,n.l,x.l,theta0,l){

  num = 0 #for ### ##printing only
  denom = 0
  if(is.na(min.x)) return(1) #need to add condition to break out of function if last value of x.vec is reached

  if(min.x + 1 > max.x){
    FDP.est = 0
  }
  else{
    tmp = seq(from=min.x+1,to=max.x) #corresponds to x.l > 2^(l-1)*n*theta0, # > xvec[j]
    if(l==1){
      num = sum(dbinom(tmp,n.l,theta0))
      denom = max(sum(x.l>min.x),1)
      FDP.est = length(x.l)*num/denom
    }
    else{
      f1 <- function(z){
        #sums over the dbinoms for vector of a_{1,r}'s...
        #need row products
        return(ifelse(!is.matrix(z),
                      sum(prod(dbinom(x=z,size=n.l/2,prob=theta0)),na.rm = TRUE),
                      sum(apply(dbinom(x=z,size=n.l/2,prob=theta0),1,prod),na.rm = TRUE)))
      }
      f2 <- function(w){
        #sum over the values of c_k's
        val = ifelse(is.list(valid.counts(w,c.prev=c.prev)),
                     sum(sapply(valid.counts(w,c.prev=c.prev),f1),na.rm=TRUE),
                     ifelse(is.null(valid.counts(w,c.prev=c.prev)),0,
                            f1(valid.counts(w,c.prev=c.prev))))
        return(val)
      }

      #numerator
      num = f2(tmp)/pbinom(q=c.prev,size=n.l/2,prob=theta0)^2

      #denominator
      denom = max(sum(x.l>min.x),1)
      FDP.est = length(x.l)*num/denom
    }
  }
  #print(paste("num:",num,"denom:",denom,"m.l:",length(x.l),"FDP:",FDP.est))
  return(FDP.est)
}
