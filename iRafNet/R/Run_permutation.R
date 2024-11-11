#' Run iRafNet for M permuted data sets.
#'
#' MAIN FUNCTION -- > iRafNet_permutation
#' 
#' INPUT
#' 
#' X            list object containing expression data for each class
#' W            (p x p) matrix of sampling weights
#' ntree        number of trees
#' mtry         number of variables to be sampled at each node
#' genes.name   vector containing gene names 
#' M            total number of permutations
#'  
#' OUTPUT: importance score of gene-gene interactions for permuted data.
#'   
#'
#' @export 
#"Run_permutation" <-  function(X, ...)UseMethod("iRafNet")

"iRafNet_permutation" <-
  function(X,W, ntree,mtry,genes.name,perm) {
    
    p<-dim(X)[2]
    imp<-matrix(0,p,p)
    n<-dim(X)[1]
    index<-seq(1,p)
    vec1<-matrix(rep(genes.name,p),p,p)
    vec2<-t(vec1); vec1<-c(vec1); vec2<-c(vec2)
    
    set.seed(perm)
    label<-sample(n)
    
    for (j in 1:p){ 
      y<-X[label,j];  # -- permute samples of target gene
      
      weights.rf<-as.matrix(W[,j]); 
      weights.rf[j]<-0
      weights.rf<-weights.rf/sum(weights.rf);
      
      w.sorted<-sort(weights.rf,decreasing = FALSE,index.return=T)
      index<-w.sorted$ix
      x.sorted<-X[,index]
      w.sorted<-w.sorted$x
      
      rout<-irafnet_onetarget(x=x.sorted,y=as.double(y),importance=TRUE,mtry=round(sqrt(p-1)),ntree=1000,
                              sw=as.double(w.sorted))
      
      imp[index,j]<-c(importance(rout))
    }
    
    
    # --- Return importance score for each regulation
    imp<-c(imp);
    out<-cbind(as.character(vec1),as.character(vec2),as.data.frame(imp),stringsAsFactors=FALSE)
    out<-out[vec1!=vec2,]
    i<-sort(out[,3],decreasing=TRUE,index=TRUE)
    out<-out[i$ix,]
    return(out[,3])
    
  }


# -- Main Function
"Run_permutation" <-
  function(X, W, ntree,mtry,genes.name,M) {
    p<-length(genes.name)
    perm<-matrix(0,(p^2-p),M)
    for (j in 1:M)  perm[,j]<-as.matrix(iRafNet_permutation(X,W, ntree,mtry,genes.name,j))  
    return(perm)
  }
    