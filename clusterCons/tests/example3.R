###############################################################################
# clusterCons - Consensus clustering functions for R
# 
# Author: Dr. T. Ian Simpson
# Affiliation : University of Edinburgh
# E-mail : ian.simpson@ed.ac.uk
#
# Example script number 3 - more advanced use with data from Golub et al. 1999
###############################################################################

#perform some test analyses using clusterCons
library(clusterCons);

#load in some real gene expression data

#simulated class data (true number of classes = 3, 200 diagnostic genes with 10 different expression profiles)
data('sim_class');

#perform the re-sampling with (note the transpose of the data matrix as we want to cluster by class not gene) 
cmr <- cluscomp(data.frame(t(sim_class)),algorithms=list('kmeans','pam','agnes'),merge=0,clmin=2,clmax=6,reps=20);

#show the result list
summary(cmr);

#explore the cluster robustness for all k values
for(i in 1:length(cmr)){
  print(names(cmr)[i],q=F);
  print(clrob(cmr[[i]]),q=F);
}

#when k=3 show the cluster robustness
for(i in 1:length(cmr)){
  if(cmr[[i]]@k==3){
    print(names(cmr)[i],q=F);
    print(clrob(cmr[[i]]),q=F);
  }
}

#when k=4 and algo is kmeans find the membership robustness values
mr <- memrob(cmr$e1_kmeans_k3);

#show what this object holds
summary(mr);

#show the membership robustness for cluster1
mr$cluster1;

#show the whole membership matrix
mr$resultmatrix;

#EXTRA ANALYSIS

#calculating area under curve (AUC)

#we can calculate the AUC for individual consensus matrices (note you must pass the consensus matrix itself @cm)
aci <- auc(cmr$e3_agnes_k5@cm);

#or for the entire result set to assess performance over clusters between algorithms and/or experimental conditions
ac <-aucs(cmr);

#basic AUC plot
aucplot(ac);

#we can also calculate the change in AUC by cluster number, deltak
dk <- deltak(ac);

#basic delta-K plot
dkplot(dk)