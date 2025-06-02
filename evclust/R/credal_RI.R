#' Credal Rand indices
#'
#' \code{credal_RI} computes generalizations of the Rand index to compare credal partitions, as defined 
#' in Denoeux et al (2018).
#'
#' In Denoeux et al. (2018), two generalizations of the Rand index for comparing credal partitions
#' are defined: one is based on distances between mass function, the other one is based on distances.
#' In the latter case, two distances are proposed: Jousselme's distance and the L1 distance between 
#' belief functions. These three indices can be computed by function \code{credal_RI}.
#'
#'
#' @param P1 Relational representation of the first credal partition such as generated
#' by function \code{pairwise_mass}
#' @param P2 Relational representation of the second credal partition such as generated
#' by function \code{pairwise_mass}
#' @param type "c" for degree of conflict (default), "j" for Jousselme's distance and "b" for 
#' belief distance.
#'
#' @return The credal Rand index
#' 
#' @export
#'
#' @seealso  \code{\link{nonspecificity}}, \code{\link{pairwise_mass}}
#'
#' @references
#'  T. Denoeux, S. Li and S. Sriboonchitta. Evaluating and Comparing Soft Partitions: an 
#'  Approach Based on Dempster-Shafer Theory. IEEE Transactions on Fuzzy Systems, 
#'  26(3):1231-1244, 2018.
#' 
#' @examples
#' ## Butterfly data
#' data(butterfly)
#' clus1<-kevclus(butterfly,c=2) 
#' P1<-pairwise_mass(clus1)
#' clus2<-ecm(butterfly,c=2)
#' P2<-pairwise_mass(clus2)
#' RI1<-credal_RI(P1,P2,"c")
#' RI2<-credal_RI(P1,P2,"j")
#' RI3<-credal_RI(P1,P2,"b")
#' print(c(RI1,RI2,RI3))
#' 

credal_RI<- function(P1,P2,type="c"){
  if(type=="c"){
    RI<-1-mean(P1$Me+P2$Me-P1$Me*P2$Me + P1$M1*P2$M0 + P1$M0*P2$M1)
  }else if(type=="j"){
    D<-mapply(jousselme_dist,P1$Me,P1$M1,P1$M0,P2$Me,P2$M1,P2$M0)
    RI<-1-mean(D)
  } else {
    D<-mapply(Bel_dist,P1$Me,P1$M1,P1$M0,P2$Me,P2$M1,P2$M0)
    RI<-1-mean(D)
  }
  return(RI)
}