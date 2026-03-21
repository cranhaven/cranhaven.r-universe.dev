
setGeneric("qkspeclust", function(x, ...) standardGeneric("qkspeclust"))

setMethod("qkspeclust", signature(x = "qkspecc"), function(x, clustmethod = "kmeans", Nocent=NULL, iterations=NULL, hmethod=NULL, eps = NULL, MinPts = NULL)
 {
   if(clustmethod == "kmeans"){
     res <- kmeans(x@ymatrix, Nocent, iterations)
     clust(x) <- res$cluster
     return(x)
    }
    else if(clustmethod =="hclust"){
       res <- hclust(dist(x@ymatrix),method = hmethod)
       clust(x) <- cutree(res,k = Nocent)
       return(x)
     }
    else if(clustmethod =="dbscan"){
       res <- fpc::dbscan(x@ymatrix, eps, MinPts)
       clust(x) <- res$cluster
       return(x)
    }
 })


