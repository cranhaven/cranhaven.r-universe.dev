\name{repfdr}
\alias{repfdr}
\alias{manhattan}
\title{
Bayes and local Bayes false discovery rate  estimation for replicability analysis
}
\description{
Estimate Bayes and local Bayes false discovery rates (FDRs) from multiple studies, for replicability analysis and for meta-analysis, as presented in Heller and Yekutieli (see reference below). 
} 
\usage{
repfdr(pdf.binned.z, binned.z.mat,
       non.null = c("replication", "meta-analysis",
       "user.defined"),
       non.null.rows = NULL,Pi.previous.result = NULL,
       control = em.control(), clusters = NULL, 
       clusters.ldr.report=NULL, clusters.verbose=T)
}
\arguments{
  \item{pdf.binned.z}{
A 3-dimensional array which contains for each study (first dimension), the probabilities of a z-score to fall in the bin (second dimension), under each hypothesis status (third dimension). The third dimension can be of size 2 or 3, depending on the number of association states: if the association can be either null or non-null (e.g. only in one direction), the dimension is 2; if the association can be either null, or positive, or negative, the dimension is 3.  
Element \code{[[1]]} in the output of \code{\link{ztobins}}. 
}
  \item{binned.z.mat}{
A matrix of the bin numbers for each of the z-scores (rows) in each study (columns).
Element \code{[[2]]} in the output of \code{\link{ztobins}}.
}
  \item{non.null}{
Indicates the desired analysis: \code{replication}, \code{meta-analysis} or \code{user.defined}. When \code{user.defined} is selected \code{non.null.rows} must be specified. 
}
  \item{non.null.rows}{
Vector of row indices in \code{H} (see \code{\link{hconfigs}}), indicating which vectors of association status should be considered as non-null in the analysis. \code{H} is the output of hconfigs(dim(pdf.binned.z)[1], dim(pdf.binned.z)[3]), i.e. the matrix with rows indicating the possible vectors of association status, where \code{dim(pdf.binned.z)[1]} is the number of studies and \code{dim(pdf.binned.z)[3]} is the number of association states in each study (2 or 3). 
}
  \item{Pi.previous.result}{
An optional Vector of probabilities for each association status. If \code{NULL}, then the probabilities are estimated with the EM algorithm. An estimation result from a previous run of \code{\link{repfdr}} or \code{\link{piem}} can be supplied to shorten the run-time of the function, see Example section. 
}
  \item{control}{
List of control parameters to pass to the EM algorithm. See \code{\link{em.control}}.
}
\item{clusters}{ Used for performing analysis in each cluster, and than aggregating results together.Default value is \code{NULL} (no clusters in data). To use clusters, argument must be vector of integer, filled with number from 1 to wanted number of clusters. Vector is of length of number of studies, where \code{clusters[i]} is the cluster membership of the ith study.
  NULL
}
\item{clusters.ldr.report}{
  Sets whether local fdr values (available through the function \code{ldr} for non clustered data) should be displayed with the output. Default value is \code{NULL} (no ldr values reported). Other options are \code{'ALL'} (all ldr valeus are reported) or a vector of integers for the indices of the SNPs to be reported)
}
\item{clusters.verbose}{
  if set to \code{TRUE}, messages will be printed to screen regarding the state of the calculation (which cluster is currently being processes, and aggregation procecdure by SNP). Default is \code{FALSE}
}
}
\details{
For \code{N} studies, each examining the same \code{M} features, the binned z-scores and the (estimated) probabilities under the null and non-null states in each study are given as input. 
These inputs can be produced from the z-scores using the function \code{\link{ztobins}}.

The function calls \code{\link{piem}}  for the computation of the probabilities for each vector of association status. The number of probabilies estimated is  \code{x^N}, where \code{x=2,3} is the number of possible association states in each study.    


The function calls \code{\link{ldr}} for the computation of the conditional probability of each of the vectors of association status in the null set given the binned z-scores. The null set contains the rows in \code{\link{hconfigs}(N,x)} that:  are excluded from \code{non.null.rows} if \code{non.null} is \code{user.defined};  that are non-zero if \code{non.null} is \code{meta-analysis}; that contain at most one 1 if \code{non.null} is \code{replication} and \code{x=2}; that contain at most one 1 or one -1 if \code{non.null} is \code{replication} and \code{x=3}. 

The local Bayes FDR is estimated to be the sum of conditional probabilities in the null set for each feature. The empirical Bayes FDR is the average of all local Bayes FDRs that are at most the value of the local Bayes FDR for each feature. The list of discoveries at level q are all features with empirical Bayes FDR at most q.

If many studies are available, one may not be able to compute RepFDR directly at the original data. If however, different groups of studies are known to be independent (e.g., if  a SNP is non null for studies 1,2 is independent of the SNP being non null in studies 3,4) one may Run RepFDR in each cluster seperatly and then aggregate the results. This is done by providing a vector for the \code{clusters} argument, with an integer value stating the cluster membership for each study.
See the values section below for the results returned from this function, when partitioning the data to clusters.
See vignette('RepFDR') for a complete example, on how to run RepFDR in clusters.
}
\value{
  \item{mat}{An \code{Mx2} Matrix with a row for each feature (\code{M} rows) and two columns, the estimated local Bayes FDR (fdr) and the estimated Bayes FDR (Fdr).}
  \item{Pi}{Vector of the estimated probabilities for each of the \code{x^N} possible vectors of association status. If \code{clusters} is not \code{NULL}, A matrix with number of rows being \code{choose(2+nr_studies,2)} (for \code{n.association} being 3) or \code{choose(1+nr_studies,1)} (for \code{n.association} being 3). The last column represents an aggregated probability over all combintaions for row.}
\item{repfdr.mat.percluster}{
Returned if \code{clusters} is not \code{NULL}. A list of the \code{mat} values returned from the RepFDR analysis, per cluster.
}
\item{repfdr.Pi.percluster}{
Returned if \code{clusters} is not \code{NULL}. A list of the \code{Pi} values returned from the RepFDR analysis, per cluster.
}
\item{ldr}{
Returned if \code{clusters.ldr.report} is not \code{NULL}. A matrix with number of rows being \code{choose(2+nr_studies,2)} (for \code{n.association} being 3) or \code{choose(1+nr_studies,1)} (for \code{n.association} being 3). Each row holds the local fdr values for a combination of non null values, for the reported SNPs. The first three columns count the number of studies for each reported state (number of null studies, number of non null studies). Other values in the row give the local fdr value, per reported SNP, for the specificed system of hypotheses.
}
}
\references{
Heller, R., & Yekutieli, D. (2014). Replicability analysis for genome-wide association studies. \emph{The Annals of Applied Statistics}, 8(1), 481-498.

Heller, R., Yaacoby, S., & Yekutieli, D. (2014). repfdr: a tool for replicability analysis for genome-wide association studies. \emph{Bioinformatics}, btu434.
}

\author{
Ruth Heller, Shachar Kaufman, Shay Yaacoby, Barak Brill, Daniel Yekutieli.
}

\examples{

#### Example 1: a simulation; each feature in each study has two association states,
####            null and positive, prior is known
#This example generates the Z scores for two studies, with 0.05 probability to have
# non - null signal in each study.
# The prior matrix is being pregenerated to show the optimal values.
# if this matrix was not supplied, the repfdr method would estimate it
# using an EM algorithm. See the next examples for estimating the prior as well using repfdr.

set.seed(1)
n = 2 #two studies
m=10000 # ten thounsand, SNPs
H_Study_1 = rbinom(m,1,prob = 0.05) #signal of 1, for SNPS with association in the first study
H_Study_2 = rbinom(m,1,prob = 0.05) #signal of 1, for SNPS with association in the second study
Zmat = matrix(rnorm(n*m),nrow = m) #generate matrix

#insert signal (mean shift of 3) for the first study
Zmat[which(H_Study_1==1),1] = Zmat[which(H_Study_1==1),1]  + 4 

#insert signal to the second study
Zmat[which(H_Study_2==1),2] = Zmat[which(H_Study_2==1),2]  + 4 

#estimate densities via ztobins:
ztobins_res = ztobins(Zmat,n.association.status = 2,plot.diagnostics = FALSE,n.bin= 100) 

#writing out the prior explicitly. If this was not supplied, 
#the repfdr would try to estimate this prior from the data.
Precomputed_Pi = matrix(NA,ncol = 3,nrow = 4)
Precomputed_Pi[,1] = c(0,1,0,1)
Precomputed_Pi[,2] = c(0,0,1,1)
Precomputed_Pi[,3] = c(0.95^2,0.95*0.05,0.95*0.05,0.05^2)
colnames(Precomputed_Pi) = c('Study 1','Study 2','Pi')

#run repfdr
repfdr_res = repfdr(ztobins_res$pdf.binned.z,
                    ztobins_res$binned.z.mat,
                    non.null = 'replication',
                    Pi.previous.result = Precomputed_Pi)

#The precomputed prior matrix. if this would not 
repfdr_res$Pi

#local fdr0 and Fdr for each SNP
head(repfdr_res$mat)

Non_Null = which(H_Study_1 ==1 & H_Study_2 == 1)
Reported = which(repfdr_res$mat[,2] <= 0.05)
TP = length(intersect(Reported,  Non_Null))
TP
FP = length(Reported) - TP
FP
FN = length(Non_Null - TP)
FN


#### Example 2: a simulation; each feature in each study has two association states,
####            null and positive, prior is estimated
\dontrun{
# a) Replicablity analysis:
data(binned_zmat_sim) # this loads the binned z-scores as well as the (estimated) probabilities
# in each bin for each state 
output.rep <- repfdr(pbz_sim, bz_sim, "replication")
BayesFdr.rep <- output.rep$mat[,"Fdr"]
Rej <- (BayesFdr.rep <= 0.05)
sum(Rej)

# which of the tests are true replicability findings? (we know this since the data was simulated)
data(hmat_sim)
true.rep   <- apply(hmat_sim,1,function(y){ sum(y==1)>1 })


# Compute the false discovery proportion (FDP) for replicability:
sum(Rej * !true.rep) / sum(true.rep)

# we can use the previously calculated Pi for further computations (e.g meta-analysis):
Pi_sim <- output.rep$Pi

# b) meta-analysis:
output.meta <- repfdr(pbz_sim, bz_sim, "meta-analysis", Pi.previous.result = Pi_sim)

BayesFdr.meta <- output.meta$mat[,"Fdr"]
Rej <- (BayesFdr.meta <= 0.05)
sum(Rej)

# which of the tests are true association findings? (we know this since the data was simulated)
true.assoc <- rowSums(hmat_sim) >= 1

# Compute the false discovery proportion (FDP) for association:
sum(Rej * !true.assoc) / sum(true.assoc) 

}

\dontrun{
#### Example 3: SNPs data; each SNP in each study has three association states,
####            negative, null, or positive: 

# load the bins of the z-scores and their probabilities.
download.file('http://www.math.tau.ac.il/~ruheller/repfdr_RData/binned_zmat.RData',
  destfile = "binned_zmat.RData")
load(file = "binned_zmat.RData")
# can also be generated from SNPlocation - see ztobins documentation.

# load the prior probabilities for each association status vector.
data(Pi)
Pi # the proportions vector was computed using piem()
   # with the following command: Pi <- piem(pbz, bz)$last.iteration

# a) replicablity analysis:
output.rep <- repfdr(pbz, bz, "replication",Pi.previous.result=Pi)
BayesFdr.rep <- output.rep$mat[,"Fdr"]
Rej <- sum(BayesFdr.rep <= 0.05)
sum(Rej)

# The posterior probabilities for the first five features with Bayes FDR at most 0.05:
post <- ldr(pbz,bz[order(BayesFdr.rep)[1:5],],Pi)
round(post,4)

# posteriors for a subset of the association status vectors can also be reported:
H <- hconfigs( dim(bz)[2], 3)
h.replicability = apply(H, 1, function(y) {sum(y == 1)> 1 | sum(y == -1) >1})
post <- ldr(pbz,bz[order(BayesFdr.rep)[1:5],],Pi,h.vecs= which(h.replicability==1))
round(post,4)

# b) meta-analysis:
output.meta <- repfdr(pbz, bz, "meta-analysis", Pi.previous.result = Pi) 
BayesFdr.meta <- output.meta$mat[,"Fdr"]
Rej <- sum(BayesFdr.meta <= 0.05)
sum(Rej)
}

## manhattan plot (ploting can take a while):
# code for manhattan plot by Stephen Turner (see copyrights at the source code manhattan.r)

\dontrun{ 
  data(SNPlocations)
  par(mfrow=c(2,1))
  # Replication 
  manhattan(dataframe=cbind(SNPlocations,P=BayesFdr.rep),ymax=10.5,pch=20,
            limitchromosomes=1:4,suggestiveline=-log(0.05,10),genomewideline=F,cex=0.25,
            annotate=SNPlocations$SNP[BayesFdr.rep<=0.05],main="Replication")
  # Association
  manhattan(dataframe=cbind(SNPlocations,P=BayesFdr.meta),ymax=10.5,cex=0.25,
            limitchromosomes=1:4,suggestiveline=-log(0.05,10),genomewideline=F,pch=20,
            annotate=SNPlocations$SNP[BayesFdr.rep<=0.05],main="Meta-analysis")
  par(mfrow=c(1,1))
}
}
\keyword{Replicability}
\keyword{GWAS}