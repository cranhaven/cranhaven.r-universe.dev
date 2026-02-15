sil_width<-function(cluster_which,dist_mat){
  cluster_which<-as.data.frame(cluster_which)
  cluster_which[,2]<-as.numeric(cluster_which[,2])
  dist_mat-dist_mat
  i=1
  j=2
  ai_list<-unlist(lapply(1:nrow(cluster_which), function(i) {
    xs<-cluster_which[(cluster_which[,2]==cluster_which[i,2] ),]
    xs<-xs[xs[,1]!=cluster_which[i,1],]
    ai<-mean(dist_mat[i,xs[,1]])
  }))

  number_of_k<-max(unique(cluster_which[,2]))

  di_mat<-do.call(cbind,lapply(1:nrow(cluster_which), function(i){
    unlist(lapply(1:number_of_k, function(j){
      if(cluster_which[i,2]==j){
        return(NA)
      }else{
        bs<-cluster_which[(cluster_which[,2]==j ),]
        return(mean(dist_mat[i,bs[,1]]))
      }
    }))
  }))

  bi_list<-unlist(lapply(1:nrow(cluster_which), function(i){
    min(di_mat[,i],na.rm = T)
  }))

  si<-unlist(lapply(1:length(bi_list), function(i){
    (bi_list[i]-ai_list[i])/max(ai_list[i],bi_list[i])
  }))

  return(si)
}




#
# cluster_which<-as.data.frame(cluster_which)
# cluster_which[,2]<-as.numeric(cluster_which[,2])
# dist_mat<-as.matrix(daisy(t(y)))
# i=1
# j=2
# ai_list<-unlist(lapply(1:nrow(cluster_which), function(i) {
#   xs<-cluster_which[(cluster_which[,2]==cluster_which[i,2] ),]
#   xs<-xs[xs[,1]!=cluster_which[i,1],]
#   ai<-mean(dist_mat[i,xs[,1]])
# }))
#
# number_of_k<-max(unique(cluster_which[,2]))
#
# di_mat<-do.call(cbind,lapply(1:nrow(cluster_which), function(i){
#   unlist(lapply(1:number_of_k, function(j){
#     if(cluster_which[i,2]==j){
#       return(NA)
#     }else{
#       bs<-cluster_which[(cluster_which[,2]==j ),]
#       return(mean(dist_mat[i,bs[,1]]))
#       }
#   }))
# }))
#
# bi_list<-unlist(lapply(1:nrow(cluster_which), function(i){
#   min(di_mat[,i],na.rm = T)
# }))
#
# si<-unlist(lapply(1:length(bi_list), function(i){
#   (bi_list[i]-ai_list[i])/max(ai_list[i],bi_list[i])
# }))
#
#
