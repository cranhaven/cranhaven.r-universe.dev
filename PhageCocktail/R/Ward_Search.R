# WardClustering_Search

WardClustering_Search<-function(matrix_pb,new_matrix,MaxPhage,MaxBacteria,bacteria_number,phage_names,phage_number,list_bacteria_removed,limit,list_new_phage,phage_new_name,file_name,FUN){
  Temperature=nestedness(matrix_pb, null.models = FALSE, n.nulls = 1000, popsize = 30, n.ind = 7, n.gen = 2000, binmatnestout=FALSE)$temperature
  counter_1=0
  for(i in 1:bacteria_number){
    for(p in 1:phage_number){
      counter_1=counter_1+matrix_pb[i,p]
    }
  }
  Fill=100*counter_1/(bacteria_number*phage_number);

  Phi=as.integer(log2(((bacteria_number*Temperature)/Fill)+2));

  if (MaxPhage < Phi){
    Phi=MaxPhage}
  if(limit<Phi){Phi<-limit}
  if(Phi==0){return(0)}


  # Dissimilarity matrix

  d <- dist(t(new_matrix), method = "euclidean")
  # Number of clusters using "Within cluster Sums of Squares"
  NumClusters=fviz_nbclust(t(new_matrix), FUNcluster = hcut, method = "wss")
  NumCluster_n<-elbow_point(c(NumClusters$data$clusters),c(NumClusters$data$y))$x
  if (Phi>NumCluster_n){
    NumCluster_n<-Phi
  }

  # Agglomerative hierarchical clustering using Ward
  Clusters <- hclust(d, method = "ward.D2")
  index_tree<-cutree(Clusters,NumCluster_n)
  cluster_divided<-list()
  for (l in 1:NumCluster_n){
    cluster1<-c()
    for (i in 1:length(list_new_phage)){
      if (index_tree[i]==l){
        cluster1<-c(cluster1,i)
      }
    }
    cluster_divided[[l]]<-cluster1
  }

  Phages=c()
  phage_name_clustering=c()
  for (c in 1:NumCluster_n){
    BestPhage<-SelectBestPhage(unlist(as.matrix(cluster_divided)[c,]), new_matrix,MaxBacteria  )
    BestPhage<-as.integer(BestPhage)
    Phages=c(Phages, BestPhage)
    phage_name_clustering=c(phage_name_clustering,phage_new_name[BestPhage])
  }

  new_matrix<-new_matrix[,Phages]

  MaxPhage<-NumCluster_n

  ResultClusteringExhaustive<-ExhaustiveSearch(MaxPhage,MaxBacteria,new_matrix,phage_name_clustering,limit,file_name,FUN)


  return(c(ResultClusteringExhaustive))

}
