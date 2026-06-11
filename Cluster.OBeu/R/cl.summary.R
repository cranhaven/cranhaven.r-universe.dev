#' @title
#' Extract the proposed clustering method and the number of clusters from clvalid method
#' 
#' @description 
#' Extract the most frequent 
#' 
#' @usage cl.summary(clv)
#' @param clv A clValid object
#' 
#' @details This function returns the proposed method or number of clusters or both according to the majority clustering indices of a clValid process
#'  
#' @return A value that indicates the proposed method and number of clusters.
#' 
#' @author Kleanthis Koupidis
#' 
#' @rdname cl.summary
#' @export
#' 

cl.summary<-function(clv) {
  
  if(!inherits(clv,"clValid")) stop("clValid object should be provided")
  
  df <- t(as.data.frame(clv@measures))
  df <- as.data.frame(df)
  tab <- data.frame( 
    index = c(rep("APN", length(rownames(df[which(df$APN == min(df$APN)),]))),
              rep("AD", length(rownames(df[which(df$AD == min(df$AD)),]))),
              rep("ADM", length(rownames(df[which(df$ADM == min(df$ADM)),]))),
              rep("FOM", length(rownames(df[which(df$FOM == min(df$FOM)),]))),
              rep("Connectivity", length(rownames(df[which(df$Connectivity == max(df$Connectivity)),]))),
              rep("Dunn", length(rownames(df[which(df$Dunn == max(df$Dunn)),]))),
              rep("Silhouette", length(rownames(df[which(df$Silhouette == max(df$Silhouette)),])))),
    
    model = c(rownames(df[which(df$APN == min(df$APN)),]),
              rownames(df[which(df$AD == min(df$AD)),]),
              rownames(df[which(df$ADM == min(df$ADM)),]),
              rownames(df[which(df$FOM == min(df$FOM)),]),
              rownames(df[which(df$Connectivity == max(df$Connectivity)),]),
              rownames(df[which(df$Dunn == max(df$Dunn)),]),
              rownames(df[which(df$Silhouette == max(df$Silhouette)),])),
    
    value = c(rep(min(df$APN), length(rownames(df[which(df$APN == min(df$APN)),]))),
              rep(min(df$AD), length(rownames(df[which(df$AD == min(df$AD)),]))),
              rep(min(df$ADM), length(rownames(df[which(df$ADM == min(df$ADM)),]))),
              rep(min(df$FOM), length(rownames(df[which(df$FOM == min(df$FOM)),]))),
              rep(max(df$Connectivity), length(rownames(df[which(df$Connectivity == max(df$Connectivity)),]))),
              rep(max(df$Dunn), length(rownames(df[which(df$Dunn == max(df$Dunn)),]))),
              rep(max(df$Silhouette), length(rownames(df[which(df$Silhouette == max(df$Silhouette)),])))))
  
  #tab$model = gsub("[0-9]\\.", "", tab$model)
  cl_meth <- stringr::str_split(tab$model,"\\.", simplify = TRUE)
  tab$model <- cl_meth[,2]
  tab$clusters <- as.numeric(cl_meth[,1])
  cl.algorithm <- table(tab[,2])
  clusters <- table(tab[,4])
  max.meth <- names(which(cl.algorithm == max(cl.algorithm)))
  max.clusters <- names(which(clusters == max(clusters)))
  
  if (length(max.meth) <= 1) {
    j <- max.meth
  } else {
    j <- sample(max.meth, 1)
  }
  
  if (length(max.clusters) <= 1) {
    i <- max.clusters
  } else {
    i <- sample(max.clusters, 1)
  }
  
  cl.nb.meth <- data.frame(nb.clust = as.integer(max.clusters), method.cluster = max.meth)
  
  return(cl.nb.meth)
}
