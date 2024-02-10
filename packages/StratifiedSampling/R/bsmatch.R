#' @title Statistical matching using optimal transport and balanced sampling
#' @name bsmatch
#' @description 
#' 
#' We propose a method based on the output of the function \code{\link{otmatch}}. The method consists of choosing a unit from sample 2 to assign to a particular unit from sample 1.
#' 
#' @param object A data.frame, output from the function \code{\link{otmatch}}.
#' @param Z2 A optional matrix, if we want to add some variables for the stratified balanced sampling step.
#'
#' @details All details of the method can be seen in the manuscript: Raphaël Jauslin and Yves Tillé (2021) <arXiv:2105.08379>.
#'
#' @return A list of two objects, A data.frame that contains the matching and the normalized weights. The first two columns of the data.frame contain the unit identities of the two samples. The third column are the final weights. All remaining columns are the matching variables.
#' @export
#' 
#' 
#' @importFrom sampling srswor
#' 
#' @seealso \code{\link{otmatch}}, \code{\link{stratifiedcube}}
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
#' s1=sampling::srswor(n1,N)
#' s2=sampling::srswor(n2,N)
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
#' #--- BALANCED SAMPLING
#' 
#' out <- bsmatch(object)
#' 
#' 
bsmatch <- function(object,
                    Z2){
  
  
  # split weight in order to 
  q_l <- split(object$weight,f = object$id1)
  q <- as.numeric(do.call(c,lapply(q_l, function(x){x/sum(x)})))
  
  # if Z data are added to balanced smapling
  if(missing(Z2)){
    Z = object$weight*(object[,which(do.call(rbind,strsplit(colnames(object),"[.]"))[,1] == "X2")])  
  }else{
    Z = object$weight*(object[,which(do.call(rbind,strsplit(colnames(object),"[.]"))[,1] == "X2")])  
    Z = cbind(object$weight*sampling::disjunctive(Z2[as.character(object$id2),]),Z)
  }
  
  # cleaning strata and define data for balanced sampling
  strata <- sampling::cleanstrata(object$id1)
  XXX <- Z
  qqq <- q
  

  s <- stratifiedcube(X = XXX,
                      strata=strata,
                      pik=qqq)
  s <- round(s,4)
  ss <- s
  
  out <- object
  out <- list(object = object[ss == 1,],q = q[ss == 1])
  
  return(out)
}




