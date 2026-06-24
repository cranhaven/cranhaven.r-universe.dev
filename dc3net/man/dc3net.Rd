\name{dc3net}
\alias{dc3net}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Differential C3NET
}
\description{
The dc3net is an R package that infers direct physical interactions of differential gene networks from gene expression datasets of multiple conditions. 
}
\usage{
dc3net(dataT = c(), dataC = c(), probes = c(), genes = c(), method = "cutoff", 
methodValue = 0, itNum = 1, rankDif = 100, percentDif = 0.6, rankdCom = 10, 
percentCom = 0.85, probFiltered = FALSE, visualization = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataT}{
Raw microarray gene expression data set or mutual information matrix, e.g. Tumor dataset or tumor mim.
}
  \item{dataC}{
Raw microarray gene expression data set or mutual information matrix, e.g. Normal dataset or normal mim.
}
  \item{probes}{
A vector of probe names. The size of the vector should be equal to the number of rows in your data sets. 
}
  \item{genes}{
A vector of gene names. The size of the vector should be equal to the number of rows in your data sets. 
}
  \item{method}{
Significance test for elimination of nonsignificant edges. The available options for method parameter are "cutoff", "justp", "rank", "holm", "hochberg", "hommel", "bonferroni", "BH" and "BY". The last six parameter, starting from holm are multiple testing correction methods. methodValue and itnum parameters are dependent to method parameter.
}
  \item{methodValue}{
If the method is "cutoff", methodValue can be zero or predefined cut-off value. Zero means that mean of upper triangle will be taken as cutoff. If the method is "justp", methodValue and itnum (iteration number) parameters are need to be adjusted. For the "justp" method, methodValue corresponds to alpha value, e.g. 0.05. If the method is "rank", methodValue is the rank number. Rank corresponds to the number of interactions that will be taken as significant starting from the highest MI value. If the selected method is one of the MTC method, then methodValue and itnum parameters are need to be adjusted as it were in "justp" method. 
}
  \item{itNum}{
Iteration number for "justp" and MTC methods.
}
  \item{rankDif}{
Predefined rank cut-off parameter for inferring differential network.
}
  \item{percentDif}{
Predefined rate parameter for inferring differential network.
}
  \item{rankdCom}{
Predefined rank cut-off parameter for inferring common network.
}
  \item{percentCom}{
Predefined rate parameter for inferring common network.
}
  \item{probFiltered}{
If probes of the same gene are filtered or not. Setting this option FALSE eliminates the interactions between the probes of the same genes.
}
  \item{visualization}{
Enables plotting the inferred networks. The visualization parameter takes three values, "0" for disabling the plot, "1" for plotting the differential network and "2" for plotting the common network. 
}
}
\details{
The required inputs of the package are two different gene expression data sets, probe names and gene names. Users can also use pre-computed test and control mutual information (adjacency) matrices as input. Otherwise, the algorithm takes the two data sets and generates the matrices itself. The data sets need to be normalized together (e.g. using RMA) before using in dc3net. If the input data sets are precomputed mutual information matrices, then the algorithm skip this preprocessing step. The MI matrices are square adjacency matrices where the MI value corresponds to the weight of interaction for each gene pair. The diagonals are set to zero to ignore self-interactions. The next step is computing row wise ranked versions of these MI matrices in descending order. Here, rank 1 corresponds to the highest mutual information value in a row of the matrix. This ranked matrices will be used in comparing and filtering the networks at the comparison step. Then C3NET is applied to the test and control MI matrices to infer gene networks of direct physical interactions of test and control datasets independently. 

In the comparison step of DC3NET, there are four conditions that all must be validated at the same time for an edge to be included in test difnet. Suppose that we check the potential interaction geneA to geneB to be included in difnet or not. As we stated above, we have been computed row wise ranked versions of the MI matrices in descending order. So we know the rank of interaction geneA to geneB in control MI matrix. The first parameter of DC3NET, rankdif, is the predefined cutoff parameter that checks the interaction between geneA and geneB is one of the top ranked interactions in control MI matrix or not. If the rank of geneA and geneB in the ranked control MI matrix is greater than the predefined cutoff parameter, rankdif, then the first condition becomes valid for deciding it as a difnet interaction. rankdif parameter can be adjusted to any value between 1 and number of rows of control MI matrix. If user wants a stricter difnet, then rankdif parameter needs to be adjusted to a greater value.  The second condition is the change in MI value of interaction from geneA to geneB in the control MI matrix. Here, algorithm uses MIdif value as the cutoff parameter. MIdif is defined as percentdif times the maximum MI value of the row of geneA in the control MI matrix. Default value for the percentdif parameter is 0.6. Depends on strictness of the differential network, user can increase or decrease the second cutoff parameter. The previous two conditions compared the interaction of geneA to geneB but we also need to compare the interaction of geneB to geneA. So the algorithm validates the first and second conditions also for the interaction of geneB to geneA. In this example, if four of the conditions are validated, then DC3NET infer this interaction as in test difnet and continue to perform same filtering process for all gene pairs in test network.

Lets now start to describe the way that the algorithm infers the common network. Common network can be inferred by looking for all the same interactions between test and control network. However, this is a very strict way of inferring common network. So alternatively, one may consider the ranks and MI value decreases in the other data set. More broadly, users may follow the manner of the difnet process described above but change the comparison parameter, rankdif, from greater to less and for the percentdif from less to greater. Additionally, at this time, we only look at one of the two conditions, rather than all the four conditions together, from geneA to geneB or geneB to geneA. In the package, rankdcom, and percentcom parameters correspond to rank difference and MIdif for common network. Finally, DC3NET infers test difnet and common network and save it to the output folder of the package. If the visualization function is turned on, then the package plots the selected inferred network.
}
\value{
\code{dc3net} returns an environment res that contains the new inferred networks and mutual information matrices. Users can access to output "Differential Network" and "Common Network" by using res$DifNet and res$CommonNet, respectively. Users can also access to computed mutual information matrices of test and control data by using res$mimT and res$mimC. Thus, users can use this precomputed mutual information matrices on the next run of the package to save time.

Accessing Differential Network Table:
networks <- dc3net(tumorData, normalData, probes, geneNames)
networks$DifNet

}
\references{
Altay, G. et. al. "Genome-wide differential gene network analysis software and an application in LnCap prostate cancer", bioRxiv, 2017.
Altay, G., Asim, M., Markowetz, F., Neal, D.E. (2011) Differential C3NET reveals disease netwokrs of direct physical interactions. BMC Bioinformatics. 12: 296.
Altay,G., Altay,N., Neal,D. (2013) Global assessment of network inference algorithms based on available literature of gene/protein interactions. Turkish Journal of Biology. 37:547-555.
Castro MA, Wang X, Fletcher MN, Meyer KB and Markowetz F (2012). RedeR: R/Bioconductor package for representing modular structures, nested networks and multiple levels of hierarchical associations. Genome Biology, 13(4), pp. R29.
}
\author{
  Gokmen Altay
}

\seealso{
\code{\link{dc3netdif}}, \code{\link{dc3netcom}}, \code{\link{maxofMIM}}, \code{\link{orderList}}, \code{\link{probSameFilt}}, \code{\link{rankList}}, \code{\link{uniqNet}}
}
\examples{
  
  data(tumorData)
  
  data(normalData)

  data(geneNames)
  
  probes <- rownames(tumorData)
  
  networks <- dc3net(tumorData, normalData, probes, geneNames, visualization=0) 
  
  networks$DifNet ### Differential Network
  
  networks$CommonNet ### Common Network
  
}
\keyword{misc}
