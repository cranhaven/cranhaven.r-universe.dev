#' Expansion of must-link and cannot-link constraints
#'
#'\code{expandlink} returns an expanded set of must-link and cannot-link constraints using the
#'k nearest neighbors of each observation.
#'
#' Using the algorithm described in Li et al (2018), \code{expandlink} generates new must-link and
#' cannot-link constraints from existing ones, using the k nearest neighbors of each observations. The
#' extended constraint list can be used by constrained clusetring algorithms such as \cite{cecm} and
#' \code{kcevclus}. 
#'
#' @param link A list with two attributes: a matrix ML containing nbML x 2 must-link constraints
#' and a matrix CL containing nbCL x 2 cannot-link constraints.
#' @param ind An n*k matrix containing the k nearest neighbor indices.
#' @param distan An n*k matrix containing the k nearest neighbor distances.
#'
#' @return A list with two attributes:
#'  \describe{
#'   \item{ML}{The new matrix of must-link constraints.}
#'   \item{CL}{The new matrix of cannot-link constraints.}
#'  }
#'
#'@references F. Li, S. Li and T. Denoeux. k-CEVCLUS: Constrained evidential clustering of 
#'large dissimilarity data. Knowledge-Based Systems (142):29-44, 2018.
#'
#'@author Feng Li and Thierry Denoeux.
#'
#' @export
#'
#' @seealso \code{\link{kcevclus}},\code{\link{cecm}},\code{\link{create_MLCL}},
#' \code{\link{bananas}}
#' 
#' @examples 
#'\dontrun{
#' data<-bananas(200)
#' link<-create_MLCL(data$y,10)
#' nml<-nrow(link$ML)
#' plot(data$x,col=data$y)
#' for(k in 1:nml) lines(data$x[link$ML[k,],1],data$x[link$ML[k,],2],lwd=2,col="red")
#' ncl<-nrow(link$CL)
#' for(k in 1:ncl) lines(data$x[link$CL[k,],1],data$x[link$CL[k,],2],lwd=2,col="blue")
#' library(FNN)
#' nn<-get.knn(data$x,5)
#' link1<-expandlink(link,ind=nn$nn.index,distan=nn$nn.dist)
#' nml<-nrow(link1$ML)
#' for(k in 1:nml) lines(data$x[link1$ML[k,],1],data$x[link1$ML[k,],2],lwd=1,lty=2,col="red")
#' ncl<-nrow(link1$CL)
#' for(k in 1:ncl) lines(data$x[link1$CL[k,],1],data$x[link1$CL[k,],2],lwd=1,lty=2,col="blue")
#' }
#'

expandlink<-function(link,ind,distan){
  k<-ncol(ind)
  
  #-----
  expandone<-function(linkone,ind,distan){
    ML<-linkone
    ML1NN<-ind[ML[1],]
    ML2NN<-ind[ML[2],]
    newML1<-c(ML[1],ML1NN)
    newML2<-c(ML[2],ML2NN)
    newML<-cbind(rep(newML1,each=length(newML1)),rep(newML2,length(newML2)))
    #################leave the original one out
    original<-which(apply(newML,1,function(x){all(sort(x)==sort(ML))}))
    newML0<-newML[-original,]
    ########################################delete the link where one point is linked to itself
    #if(any(newML0[,1]==newML0[,2])){
    #  same<-which(newML0[,1]==newML0[,2])
    #  newML0<-newML0[-same,]
    #}
    ##################################the distance between the original link and the expanded links
    d<-rep(0,nrow(newML0))
    for(o in 1:nrow(newML0)){
      if(any(newML0[o,]==ML[1])){
        d[o]<-distan[ML[2],which(ML2NN==newML0[o,2])]
      }else if(any(newML0[o,]==ML[2])){
        d[o]<-distan[ML[1],which(ML1NN==newML0[o,1])]
      }else{d[o]<-distan[ML[1],which(ML1NN==newML0[o,1])]+distan[ML[2],which(ML2NN==newML0[o,2])]}
    }
    return(list(link=newML0,distance=d))
  } # end of function expandone
  
  if((nrow(link$ML)+nrow(link$CL))==0){
    ML<-matrix(0,0,2)
    CL<-matrix(0,0,2)
    expandlink<-list(ML=ML,CL=CL)
  }else{
    expandML<-NULL
    for(i in 1:nrow(link$ML)){
      res<-expandone(link$ML[i,],ind,distan)
      ###########################get the first k links
      firstk<-order(res$distance)
      expandML<-rbind(expandML,res$link[firstk[1:k],])
    } # end for
    expandCL<-NULL
    for(i in 1:nrow(link$CL)){
      res<-expandone(link$CL[i,],ind,distan)
      ###########################get the first k links
      firstk<-order(res$distance)
      expandCL<-rbind(expandCL,res$link[firstk[1:k],])
    } #end for
    expandlink<-list(ML=rbind(link$ML,expandML),CL=rbind(link$CL,expandCL))
  } # end else
  return(expandlink)
}
