###############################################################################
# clusterCons - Consensus clustering functions for R
# 
# Author: Dr. T. Ian Simpson
# Affiliation : University of Edinburgh
# E-mail : ian.simpson@ed.ac.uk
#
# Example script number 2 - more advanced use with data from Golub et al. 1999
###############################################################################

#perform some test analyses using clusterCons
library(clusterCons);

#load in some real gene expression data
data('golub');

#call the cluscomp method
cmr <- cluscomp(data.frame(t(golub)),algorithms=list('kmeans','pam'),merge=0,clmin=2,clmax=5,reps=10)

#exploring the cmr
summary(cmr);
summary(cmr$e1_kmeans_k3);
getClass('consmatrix');

#lets look at a heat map
cm <- cmr$e1_kmeans_k3;
heatmap(cm@cm);

#get cluster robustness
cr <- clrob(cm);

#get member robustness
mr <- memrob(cm);

#lets expore the mr list
summary(mr);

#get the member robustness for the first cluster
mr$cluster1;

#now lets move to looking at some comparisons
#calculate the areas under the curves
ac <- aucs(cmr);

#plot out the auc curves
aucplot(ac);

#now lets calculate the deltak
dk <- deltak(ac);

#plot out the results to find optimal class number
dkplot(dk)