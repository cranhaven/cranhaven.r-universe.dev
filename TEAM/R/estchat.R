#' Estimate threshold function
#'
#' Estimate threshold to control FDR in multiple testing procedure
#'
#' @param l Layer
#' @param n Number of pooled case and control observations in each layer 1 bin
#' @param theta0 Nominal boundary level for binomial parameter at layer 1
#' @param x.l Vector of case counts in each bin
#' @param c.hats Previous c.hats calculated from layers 1 to l-1
#' @param alpha Nominal FDR level
#' @param m.l Number of leaf hypotheses at layer l
#'
#' @return NULL
#'
est.c.hat <- function(l,n,theta0,x.l,c.hats,alpha,m.l){

  a.l = floor(2^(l-1)*n*theta0+2^((l-1)/2)*sqrt(2*theta0*(1-theta0)*n*log(m.l)))

  filter1 = which(x.l < 2^(l-1)*n*theta0)
  filter2 = which(x.l > a.l)
  c.vec = sort(x.l[-c(filter1,filter2)],decreasing = TRUE)

  x.indx = 0
  emp.fdr = 0


  if(length(c.vec)>0){
    while(all(emp.fdr <= alpha,x.indx < length(c.vec))){
      x.indx = x.indx + 1
      emp.fdr = est.FDR.hat.l(min.x=c.vec[x.indx],
                              max.x=max(n,2*c.hats[l-1]),#a.l,
                              c.prev=ifelse(length(c.hats[l-1])>0,c.hats[l-1],NULL),
                              n.l=n*2^(l-1),
                              x.l=x.l,
                              theta0=theta0,
                              l=l)
    }
    c.hat = ifelse(is.na(c.vec[x.indx-1]) || length(c.vec[x.indx-1])==0,
                   a.l,
                   c.vec[x.indx-1])
  }else{
    c.hat = a.l
  }

  return(c.hat)
}

