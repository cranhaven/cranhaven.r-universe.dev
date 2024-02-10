#' @title Statistical Matching using Optimal transport
#' @name otmatch
#' @description 
#' 
#' This function computes the statistical matching between two complex survey samples with weighting schemes. The function uses the function \code{\link[transport:transport]{transport}} of the package \code{transport}.
#'  
#' @param X1 A matrix, the matching variables of sample 1.
#' @param id1 A character or numeric vector that contains the labels of the units in sample 1.
#' @param X2 A matrix, the matching variables of sample 2.
#' @param id2 A character or numeric vector that contains the labels of the units in sample 1.
#' @param w1 A numeric vector that contains the weights of the sample 1, harmonized by the function \code{\link{harmonize}}.
#' @param w2 A numeric vector that contains the weights of the sample 2, harmonized by the function \code{\link{harmonize}}.
#' @param dist_method A string that specified the  distance used by the function \code{\link[proxy:dist]{dist}} of the package \code{proxy}. Default \code{"Euclidean"}.
#' @param transport_method A string that specified the  distance used by the function \code{\link[transport:transport]{transport}} of the package \code{transport}. Default \code{"shortsimplex"}.
#' @param EPS an numeric scalar to determine if the value is rounded to 0.  
#'
#' @details All details of the method can be seen in : Raphaël Jauslin and Yves Tillé (2021) <arXiv:2105.08379>.
#'
#' @return A data.frame that contains the matching. The first two columns contain the unit identities of the two samples. The third column is the final weights. All remaining columns are the matching variables.
#' @export
#'
#' @importFrom transport transport
#' @importFrom proxy dist
#' @importFrom sampling srswor
#'
#' @examples
#' 
#' #--- SET UP
#' N=1000
#' p=5
#' X=array(rnorm(N*p),c(N,p))
#' EPS= 1e-9
#' 
#' n1=100
#' n2=200
#' 
#' s1 = sampling::srswor(n1,N)
#' s2 = sampling::srswor(n2,N)
#' 
#' 
#' id1=(1:N)[s1==1]
#' id2=(1:N)[s2==1]
#' 
#' d1=rep(N/n1,n1)
#' d2=rep(N/n2,n2)
#' 
#' X1=X[s1==1,]
#' X2=X[s2==1,]
#' 
#' #--- HARMONIZATION
#' 
#' re=harmonize(X1,d1,id1,X2,d2,id2)
#' w1=re$w1
#' w2=re$w2
#' 
#' #--- STATISTICAL MATCHING WITH OT
#' 
#' object = otmatch(X1,id1,X2,id2,w1,w2)
#' 
#' 
#' round(colSums(object$weight*object[,4:ncol(object)]),3)
#' round(colSums(w1*X1),3)
#' round(colSums(w2*X2),3)
otmatch <- function(X1,
                    id1,
                    X2,
                    id2,
                    w1,
                    w2,
                    dist_method = "Euclidean",
                    transport_method = "shortsimplex",
                    EPS = 1e-9){
  
  # distance
  D <- proxy::dist(X1,X2,method = dist_method)
  
  # to find units that are in intersection between the two sample
  inter <- base::intersect(id1,id2)
  inter1 <- do.call(c,lapply(inter,function(x){which(x == id1)}))
  inter2 <- do.call(c,lapply(inter,function(x){which(x == id2)}))
  
  # select the minimum weights between the two sample
  wtmp <- pmin(w1[inter1],w2[inter2])
  
  # change the weight of the intersected value to be the weight - min(w), some weights are put equal to 0.
  ww1 <- w1
  ww2 <- w2 
  ww1[inter1] <- ww1[inter1] - wtmp
  ww2[inter2] <- ww2[inter2] - wtmp
  
  # weights that are put equal to 0, we keep only weights that are greater than 0
  w01 <- ww1>EPS
  w02 <- ww2>EPS
  
  # keep track of the right indices (some weights are put to 0 and then the indices have changes)
  id1_tmp <- id1[w01]
  id2_tmp <- id2[w02]
  
  # keep weights larger than 0
  www1 <- ww1[w01]
  www2 <- ww2[w02]
  
  # adapt distance to optimal transport
  DD <- D[w01,w02]
  
  # optimal transport
  # cat("begin transport : \n\n");start_time <- Sys.time()
  # require(lpSolve)
  # lp.transport(DD, "min", row.signs = rep ("==", length(www1)),
  #              row.rhs = www1,
  #              col.signs= rep ("==", length(www2)),
  #              col.rhs = www2,
  #              integers = NULL)
  
  res <- transport::transport.default(www1,www2,DD,method = transport_method)
  # end_time <- Sys.time()
  # cat("end transport : TIME : ", end_time - start_time,"\n\n")
  
  U <- cbind(id1 = id1_tmp[res[,1]],
             id2 = id2_tmp[res[,2]],
             weight = res[,3] ,
             X1[w01,][res[,1],],
             X2[w02,][res[,2],])
  
  # for common unit 
  D1 <- data.frame(w1,id1,X1 = X1)
  D2 <- data.frame(w2,id2,X2 = X2)
  V <- merge(D1,D2,by.x="id1",by.y="id2")
  V <- cbind(id1=V$id1,id2=V$id1,weight=pmin(V$w1,V$w2),V[,-which(names(V)%in% c("w1","w2","id1"))])
  
  colnames(U)[4:ncol(U)] <- colnames(V)[4:ncol(V)]
  
  conc <- rbind(V,U)
  conc <- conc[order(conc[,1]),]
  
  out <- conc
  # class(out) <- "match"
  
  return(out)
  
}





