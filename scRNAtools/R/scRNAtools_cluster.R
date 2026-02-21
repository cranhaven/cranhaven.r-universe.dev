scRNAtools_cluster <-
function(example1,k){
ConsensusClusterPlus::ConsensusClusterPlus
d1<-example1
d=d1[,-1]
rownames(d)<-d1[,1]
mads=apply(d[,-1],1,mad)
d = sweep(d,1, apply(d,1,median,na.rm=T))
title=tempdir()
d<-as.matrix(d)
results = ConsensusClusterPlus(d,maxK=k,reps=50,pItem=0.8,pFeature=1,clusterAlg="hc",distance="pearson",seed=1262118388.71279,plot="png")
}
