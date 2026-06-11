\name{GLP}
\alias{GLP}
\alias{graph.clust.test}

\title{
A function to perform K-sample test using GLP algorithm
}
\description{
This function performs the GLP multivariate K-sample learning.
}
\usage{
GLP(X,y,m.max=4,components=NULL,alpha=0.05,c.poly=0.5,clust.alg='kmeans',perm=0,
	combine.criterion='pvalue',multiple.comparison=TRUE,
	compress.algorithm=FALSE,nbasis=8, return.LPT=FALSE,return.clust=FALSE)
}

\arguments{
  \item{X}{ A \eqn{n}-by-\eqn{d} matrix of the observations, the observations should be grouped by their respective classes.}
  \item{y}{ A length \eqn{n} vector indicating the sample class.}
  \item{m.max}{ An integer, maximum order of LP component to investigate, default: 4. }
  \item{components}{A vector specifying which components to test. If provided with any value other than NULL, the test will only examine the components mentioned in this argument, ignoring the m.max settings.}
  \item{alpha}{ Numeric, confidence level \eqn{\alpha} , default: 0.05.}
  \item{c.poly}{ Numeric, parameter for polynomial kernel, default: 0.5. }
  \item{perm}{ Number of permutations for approximating p-value, set to 0 to use asymptotic p-value. }
  \item{combine.criterion}{How to obtain the overall testing result based on the component-wise results; 'pvalue' uses Fisher's method to combine the p-values from each component; 'kernel' computes an overall kernel \eqn{W} based on the significant components and run the LP graph test on the \eqn{W}.}
  \item{multiple.comparison}{Set to TRUE to use adjustment for multiple comparisons when determining which components are significant.}
  \item{compress.algorithm}{Use the smooth compression of Laplacian spectra for testing the null hypothesis. Recommended for large \eqn{n}. }
  \item{nbasis}{Number of bases used for approximation when \code{compress.algorithm=TRUE}.}
  \item{clust.alg}{ \code{"mclust"} or \code{"kmeans"}; algorithm used for clustering in graph community detection. }
  \item{return.LPT}{ logical, whether or not to return the data driven covariate matrix, default: FALSE.}
  \item{return.clust}{ logical, whether or not to return the class labels assigned by graph community detection, default: FALSE.}
}

\value{
 A list containing the following items:
   \item{GLP}{Overall GLP statistics.}
   \item{pval}{Overall P-value.}
   \item{table}{The GLP component table indicating the significance of each component.}
   \item{components}{significant eLP components for the data set.}
   \item{LPT}{(optional) matrix of data driven covariates.}
   \item{clust}{(optional) class labels assigned by graph community detection.}
}

\references{
Mukhopadhyay, S. and Wang, K. (2020), "A Nonparametric Approach to High-dimensional K-sample Comparison Problem", arXiv:1810.01724.

Mukhopadhyay, S. and Wang, K. (2020).  "Towards a unified statistical theory of spectralgraph analysis", arXiv:1901.07090,
}
\author{ Mukhopadhyay, S. and Wang, K.}

\examples{


  ##1.muiltivariate normal distribution with only mean difference:
  ##generate data, n1=n2=10, dimension 25
   X1<-matrix(rnorm(250,mean=0,sd=1),10,25)
   X2<-matrix(rnorm(250,mean=0.5,sd=1),10,25)
   y<-c(rep(1,10),rep(2,10))
   X<-rbind(X1,X2)
  ##GLP test:
   locdiff.test<-GLP(X,y,m.max=4)

  \dontrun{
  ##2.Leukemia data example
   data(leukemia)
   attach(leukemia)
   leukemia.test<-GLP(X,class,components=1:4)
  ##confirmatory results:
   leukemia.test$GLP  # overall statistic
   #[1] 0.2092378
   leukemia.test$pval # overall p-value
   #[1] 0.0001038647
  ##exploratory outputs:
   leukemia.test$table  # rows as shown in Table 3 of reference
   #     component    comp.GLP       pvalue
   #[1,]         1 0.209237826 0.0001038647
   #[2,]         2 0.022145514 0.2066876581
   #[3,]         3 0.002025545 0.7025436476
   #[4,]         4 0.033361702 0.1211769396
  }
}


\keyword{ GLP main function }