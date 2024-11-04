\name{clusterCons-package}
\alias{clusterCons-package}
\alias{clusterCons}
\docType{package}
\title{Calculate consensus clustering results from re-sampled clustering experiments with the option of using multiple algorithms and parameters}
\description{
clusterCons is a package containing functions that generate robustness measures for clusters and cluster membership based on generating consensus 
matrices from bootstrapped clustering experiments in which a random proportion of rows of the data set are used in each individual clustering.
This allows the user to prioritise clusters and the members of clusters based on their consistency in this regime. The functions allow the user to
select several algorithms to use in the re-sampling scheme and with any of the parameters that the algorithm would normally take.
}
\details{
\tabular{ll}{
Package: \tab clusterCons\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2010-10-12\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
Depends: \tab methods,cluster,lattice,RColorBrewer,grid,apcluster\cr
Extends: \tab cluster\cr
Suggests: \tab latticeExtra\cr
}

The user should first prepare an entirely numeric \code{data.frame} in which the conditions to be clustered are the column names and the unique ids
of the entities are the row names. Compatibility of the resulting data.fram can be checked by using the \code{data_check} function. 
}
%main
\section{Functions to run the consensus clustering and retrieve robustness information}{
\code{cluscomp} - generate consensus matrices from re-sampled clustering experiments with the option of multiple algorithms and parameters\cr
\code{clrob} - calculate the robustness of the clusters from the consensus matrix\cr
\code{memrob} - calculate the cluster membership robustness from the consensus matrix\cr
}
%algorithms
\section{Internal functions to call the individual clustering algorithms}{
\code{agnes_clmem} - wrapper for the \code{agnes} function of package cluster\cr
\code{diana_clmem} - wrapper for the \code{diana} function of package cluster\cr
\code{hclust_clmem} - wrapper for the \code{hclust} function of package cluster\cr
\code{kmeans_clmem} - wrapper for the \code{kmeans} function of package cluster\cr
\code{pam_clmem} - wrapper for the \code{pam} function of package cluster\cr
\code{apcluster_clmem} - wrapper for the \code{apclusterK} function of package apcluster\cr
}
%analysis curves
\section{Functions to calculate AUC related metrics}{
\code{auc} - calculates the area under the curve for a series of clustering experiments with the same cluster number\cr
\code{aucs} - calculates the areas under the curves of a series of clustering experiments over a range of cluster numbers\cr
\code{deltak} - calculates the change in the area under the curve\cr
}

%checking and class definition
\section{Functions to check data and object validity}{
\code{data_check} - check that the provided \code{data.frame} is formatted correctly\cr
\code{expSetProcess} - extracts the data set from an object of class expressionSet\cr  
\code{validConsMatrixObject} - check the validity of a \code{consmatrix} object\cr
\code{validMergeMatrixObject} - check the validity of a \code{mergematrix} object\cr
\code{validMemRobListObject} - check the validity of a membership robustness list object\cr
\code{validMemRobMatrixObject} - check the validity of a membership robustness matrix object\cr
\code{validAUCObject} - check the validity of an \code{"auc"} class object\cr
\code{validDkObject} - check the validity of an \code{"dk"} class object\cr
}

%plotting
\section{Functions to plot out performance curves}{
\code{aucplot} - plot area under the curve (AUC) plots from consensus clustering results\cr
\code{dkplot} - plot change in AUC by cluster number (delta-K plot)\cr
\code{expressionPlot} - plot the original data partitioned by cluster membership\cr
\code{membBoxPlot} - plot a box and whisker plot of the membership robustness for each cluster\cr
}

\references{
Merged consensus clustering to assess and improve class discovery with microarray data. Simpson TI, Armstrong JD and Jarman AP. BMC Bioinformatics 2010, 11:590.\cr

Consensus clustering: A resampling-based method for class discovery and visualization of gene expression microarray data. Monti, S., Tamayo, P., Mesirov, J. and Golub, T. Machine Learning, 52, July 2003.\cr
}

\author{
Dr. T. Ian Simpson
\email{ian.simpson@ed.ac.uk}
}

\section{Keywords}{
cluster
}

\section{See Also}{
\code{\link{cluster}},\code{\link{lattice}},\code{\link{apcluster}}
}

\section{Examples}{
#load data
data(sim_profile);

#perform consensus clustering
cmr <- cluscomp(sim_profile,algo=list('agnes','pam','kmeans'),clmin=2,clmax=7,rep=10,merge=1);

#see the consensus and merge matrices
summary(cmr);

#fetch the cluster robustness for agnes consensus clustering with k=3
clrob(cmr$e1_agnes_k3);

#show the membership robustness for cluster 1
memrob(cmr$e1_agnes_k3)$cluster1

#show the same, but for the merge against the k=3 agnes clustering structure
#note we provide the reference matrix (which is the original cluster membership for agnes where k=3)
clrob(cmr$merge_k3,cmr$e1_agnes_k3@rm);
memrob(cmr$merge_k3,cmr$e1_agnes_k3@rm)$cluster1;

#calculate the AUCs
acs <- aucs(cmr);

#plot the AUC curves
aucplot(acs);

#calculate the delta-Ks
dks <- deltak(acs);

#plot the delta-K curves
dkplot(dks);

#plot the expression profiles
expressionPlot(sim_profile,cmr$e1_agnes_k3);

#plot the bwplot of membership robustness for the same
membBoxPlot(memrob(cmr$e1_agnes_k3));
}
