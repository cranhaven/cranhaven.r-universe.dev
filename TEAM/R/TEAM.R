#' Testing on an Aggregation Tree Method
#'
#' This function performs multiple testing embedded in a hierarchical structure in order to identify local differences between two independent distributions (e.g. case and control).
#'
#' @import stats ks ggplot2 plyr
#'
#' @param x1 Numeric vector of N1 control observations
#' @param x2 Numeric vector of N2 case observations
#' @param theta0 Nominal boundary level for binomial parameter - default is N2/(N1+N2)
#' @param K log2 number of bins
#' @param alpha Nominal false discovery rate (FDR) level
#' @param L Number of layers in the aggregation tree
#'
#' @return List containing the discoveries (S.list) in each layer and the estimated layer-specific thresholds (c.hats)
#'
#' @references Pura J. Chan C. Xie J. Multiple Testing Embedded in an Aggregation Tree to Identify where Two Distributions Differ. \url{https://arxiv.org/abs/1906.07757}
#'
#' @examples
#' set.seed(1)
#' # Simulate local shift difference for each population from mixture of normals
#' N1 <- N2 <- 1e6
#' require(ks) #loads rnorm.mixt function
#' #Controls
#' x1 <- rnorm.mixt(N1,mus=c(0.2,0.89),sigmas=c(0.04,0.01),props=c(0.97,0.03))
#' #Cases
#' x2 <- rnorm.mixt(N2,mus=c(0.2,0.88),sigmas=c(0.04,0.01),props=c(0.97,0.03))
#' res <- TEAM(x1,x2,K=14,alpha=0.05,L=3)
#' #Discoveries in each layer - Each element is an growing set of
#' #indices captured at each layer
#' res$S.list
#' #Map back final discoveries in layer 3 to corresponding regions
#' levels(res$dat$quant)[res$S.list[[3]]]
#'
#' @export TEAM
TEAM <- function(x1,x2,theta0=length(x2)/length(c(x1,x2)),K=14,alpha=0.05,L=3){

  N2 = length(x2)
  N1 = length(x1)

  dat = data.frame(X=c(x2,x1),lab=rep(c(1,0),times=c(N2,N1)))

  N = N2+N1

  #number of bins
  m = 2^K

  dat$quant = with(dat, cut_number(dat$X, n = m))
  dat$index = factor(dat$quant, levels = levels(dat$quant),
                     labels = 1:m)

  #number of pooled obs in each bin
  n = N/m

  #Global/Initial variables
  x.1 = aggregate(lab~index,dat,sum)$lab
  c.hats = vector("integer",L) #set of critical values
  S = NULL #set of rejections regions
  m.excl = NULL #set of excluded regions
  S.list=vector("list",L)
  curr.indx.vec = seq(m)

  #Loop through resolutions


  for (l in seq(L)){
    #print(paste("l:",l))
    if (l==1){

      c.hat.l = est.c.hat(l=1,n=round(n),
                          theta0=theta0, x.l=x.1,
                          c.hats=NULL,alpha=alpha,m.l=m)

      #identify indices of rejected regions - discoveries
      S = c(curr.indx.vec[which(x.1 > c.hat.l)],S)
      S.list[[l]] = unique(unlist(c(S.list,list(S))))
    }
    else{# l > 1

      #map indices in layer l to 1
      indx.list = splitNoOverlap(curr.indx.vec,2^(l-1))$kept #splitWithOverlap(curr.indx.vec,l,log2(l))#splitNoOverlap(curr.indx.vec,l)#
      m.excl = splitNoOverlap(curr.indx.vec,2^(l-1))$removed#unique(c(m.excl,splitNoOverlap(curr.indx.vec,2^(l-1))$removed))
      #Don't throw away m.excl, compare it to t.hat.l

      m.l = length(indx.list)#+1

      #estimate counts from data
      x.l=chunk.sum(x.1[unlist(indx.list)],2^(l-1))

      #estimate c.hat[l] and replace previous initial value

      c.hat.l = est.c.hat(l=l,n=round(n),
                          theta0=theta0,x.l=x.l,
                          c.hats=c.hats,alpha=alpha,m.l=m.l)

      #identify indices of rejected regions - need to map to indices of l = 1
      #index of rejected counts corresponding to layer l>1
      rej.x = which(x.l > c.hat.l)
      rej.xtra = ifelse(sum(x.1[m.excl])>c.hat.l,m.excl,integer(0))

      if (length(rej.x)==0){
        S = S
      } else{
        #indices corresponding to layer 1
        ind.rej = unique(c(unlist(indx.list[rej.x],rej.xtra)))
        S = c(ind.rej,S)
      }
      S.list[[l]] = unique(unlist(c(S.list,list(S))))
    }

    #append c hat vector
    c.hats[l] = c.hat.l

    #update current regions
    curr.indx.vec = setdiff(seq(m),S)
  }

  return(list("dat"=dat,"n"=round(n), "m"=m,
              "m.excl"=unique(m.excl),"S.list"=S.list,
              "c.hats"=c.hats))
}





