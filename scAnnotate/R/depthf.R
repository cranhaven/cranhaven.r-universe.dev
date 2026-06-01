#' @title depthf
#'
#' @description Calculate the nonparametric depth function h(x*) for a specific gene in a given cell population
#'
#' @param dat A vector of nonzero counts gene expression from training data for a specific gene in a given cell population
#' @param x A vector of gene expression from test data for a given gene
#' @param nn Number of nonzero counts for that specific gene in a given cell population
#'
#' @return A vector contain depth function h(x*) of each test data for a specific gene in a given cell population
#' @noRd
#'
depthf <- function(dat,x,nn) {
  # if only 1 nonzero count, set h(x*) = delta
  # delta: A value to add to depth function for 0 case (1 nonzero obs)
  delta=0.01
  if(nn==1){
    hx=rep(delta,length(x))
  }
  if(nn!=1){
    rank=matrix(NA,nrow=length(x),ncol = 2)
    colnames(rank)=c("rank","r_rank")
    # Depth function: determine rank of x*
    rank[,1]=colSums(t(outer(x,dat,"-"))>0,na.rm = TRUE)+1
    rank[,2]=(nn+1)-rank[,1]+1
    rank_min=apply(rank,1,min)

    hx = ((1/(nn+1)) * rank_min) + delta
  }

  return(hx)
}
