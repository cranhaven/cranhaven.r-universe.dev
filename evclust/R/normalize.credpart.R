#' Normalization of a credal partition
#'
#' \code{normalize.credpart} normalizes a credal partition (a \code{"credpart"} object).
#'
#' The function implements two normalization methods: Dempster's normalization (the mass of each focal set
#' is divided by one minus the mass on the empty set), and yager's normalization (the mass of the empty set
#' is transfered to the whole frame).
#'
#' @param clus An object of class \code{"credpart"}, encoding a credal partition.
#' @param method Normalization method ("d" for Dempster or "y" for Yager).
#'
#' @return The normalized credal partition (a \code{"credpart"} object).
#'
#' @export
#'
#' @seealso \code{\link{extractMass}}, \code{\link{plot.credpart}}, \code{\link{summary.credpart}}.
#'
#' @references
#' T. Denoeux and O. Kanjanatarakul. Beyond Fuzzy, Possibilistic and Rough: An
#' Investigation of Belief Functions in Clustering. 8th International conference on soft
#' methods in probability and statistics, Rome, 12-14 September, 2016.
#'
#' @examples
#'data(butterfly)
#'clus<-kevclus(butterfly,c=2)
#'print(clus$mass)
#'clus1<-normalize.credpart(clus,"d") # Dempster normalization
#'print(clus1$mass)
#'clus2<-normalize.credpart(clus,"y") # Yager normalization
#'print(clus2$mass)
#'
normalize.credpart<- function(clus,method="d"){
  if(all(clus$F[1,]==0)){
    f<-nrow(clus$F)
    conf<-clus$mass[,1]
    F.n<-clus$F[2:f,]
    if(method=="d"){ # Dempster normalization
      C<- 1/(1-conf)
      mass.n<- C*clus$mass[,2:f]
    } else{ # Yager normalization
      if(all(clus$F[f,]==1)){
        mass.n<- cbind(clus$mass[,2:(f-1)],clus$mass[,f]+clus$mass[,1])
      }else{
        n<-ncol(F.n)
        F.n<-rbind(F.n,rep(1,n))
        mass.n<- cbind(clus$mass[,2:f],clus$mass[,1])
      }
    }
    clus.n<-extractMass(mass.n,F.n,method='normalize_credalpart',crit=NULL)
  } else clus.n<-clus
  return(clus.n)
}
