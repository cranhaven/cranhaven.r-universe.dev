###############################################################################
# clusterCons - Consensus clustering functions for R
# 
# Author: Dr. T. Ian Simpson
# Affiliation : University of Edinburgh
# E-mail : ian.simpson@ed.ac.uk
#
# Example script number 1 - basic use with simulated data
#
###############################################################################

#perform some test analyses using clusterCons
library(clusterCons);

#PART ONE - simulated profile data (true k=4)
#load up the data
data('sim_profile');

#perform the re-sampling with five different algorithms
cmr <- cluscomp(sim_profile,algorithms=list('kmeans','pam','agnes'),merge=0,clmin=2,clmax=6,reps=5);

#show the result list
summary(cmr);

#explore the cluster robustness for all k values
for(i in 1:length(cmr)){
	print(names(cmr)[i],q=F);
	print(clrob(cmr[[i]]),q=F);
}

#when k=4 show the cluster robustness
for(i in 1:length(cmr)){
	if(cmr[[i]]@k==4){
		print(names(cmr)[i],q=F);
		print(clrob(cmr[[i]]),q=F);
	}
}

#when k=4 and algo is kmeans find the membership robustness values
mr <- memrob(cmr$e3_agnes_k4);

#show what this object holds
summary(mr);

#show the membership robustness for cluster1
mr$cluster1;

#show the whole membership matrix
mr$resultmatrix;

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