\name{progenyClust}
\alias{progenyClust}
\alias{summary.progenyClust}
\alias{print.summary.progenyClust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Progeny Clustering
}
\description{
Select the optimal number for clustering using Progeny Clustering.
}
\usage{
progenyClust(data, FUNclust = kmeans, method = "gap", score.invert = F, ncluster = 2:10, 
size = 10, iteration = 100, repeats = 1, nrandom = 10, ...)

\method{summary}{progenyClust}(object,...)

}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  data matrix or data frame for clustering: each row correpsonds to a sample or observation, whereas each column corresponds to a feature or variable.
}
  \item{FUNclust}{
  clustering function: accepts data as its first argument and the number for clustering as the second argument; returns a list containing a component called 'cluster' which is a vector of integers recording the clustering assignment for all samples. The default function is kmeans.
}
  \item{method}{
  character string indicating the criterion used to pick the optimal cluster number.\cr
  'gap': the default value, selecting the cluster number that has the biggest or smallest (when score.invert=TRUE) gap from its neighboring numbrs. The optimal cluster number is picked based on the input data only, and is not compared against any random datasets, thus is quick to compute. Note that this method does not evaluate the minimum and maximum cluster numbers.\cr
  'score': selects the cluster number that has the highest or lowest (when score.invert=TRUE) score when comparing against scores generated from random datasets. Due to the repeats on progeny clustering on random datasets, this method is slower to compute.\cr
  'both': uses and outputs results from both the 'gap' and 'score' criteria.
}
  \item{score.invert}{
logical flag: specifies whether the score should be inverted. The default score is the ratio of true classification probabilities over false classification probilities. The inverted score is the ratio of false classification over true classification probilities, which can prevent the algorithm from generating infinite score values in cases of perfect clustering. When score.invert=TRUE, the optimla cluster number is picked based on the lowest score.  
}
  \item{ncluster}{
sequence of integers specifying candidate cluster numbers for evaluation: ncluster needs to be continuous if the method 'gap' is chosen.
}
  \item{size}{
integer specifying the number of progenies generated from each cluster. Default value is 10.
}
  \item{iteration}{
integer specifying the number of times the algorithm samples progenies and evalutes similarity among progenies. Default value is 100.
}
  \item{repeats}{
integer specifying the number of times the algorithm should be run: needs to be greater than 0. Values greater than 1 output standard deviations of the scores, which are plotted as error bars in print(...,errorbar=T,...) function. Default value is 1.
}
  \item{nrandom}{
integer specifying the number of random datasets used to generate reference scores when using method 'score'. Default value is 10. 
}
  \item{object}{
    the S3 object of class "progenyClust".
  }
  \item{\dots}{
  additional arguments for FUNclust in progenyClust(...).
}
}

\value{
progenyClust returns an object of class "progenyClust" which has a plot and summary method. It is a list with the following components:
%%  If it is a LIST, use
    \item{cluster}{matrix of clustering memberships for all samples under given cluster numbers: each row corresponds to a sample; each column corresponds to a given cluster number.}
    \item{score}{matrix of stability scores from clustering the input data under given cluster numbers: each column corresponds to a given cluster number; each row corresponds to a repeat, the number of which is defined by 'repeats' in the input argument.}
    \item{random.score}{matrix of stability scores from clustering random datasets under given cluster numbers: each column corresponds to a given cluster number; each row corresponds to a random dataset, the number of which is defined by 'nrandom' in the input argument.}
    \item{random.score}{matrix of stability scores from clustering random datasets under given cluster numbers: each column corresponds to a given cluster number; each row corresponds to a random dataset, the number of which is defined by 'nrandom' in the input argument.}
    \item{mean.gap}{vector of mean stability scores based on the 'gap' criterion when the input argument 'method' is set to be 'gap' or 'both'.}
    \item{mean.score}{vector of mean stability scores based on the 'score' criterion when the input argument 'method' is set to be 'score' or 'both'.}
    \item{sd.gap}{vector of standard deviations of stability scores for each given cluster number based on the 'gap' criterion, when the input argument 'method' is set to be 'gap' or 'both'.}
    \item{sd.score}{vector of standard deviations of stability scores for each given cluster number based on the 'score' criterion, when the input argument 'method' is set to be 'score' or 'both'.}
    \item{call}{the call with arguments specified.}
    \item{ncluster}{the specified value of input argument 'ncluster'.}
    \item{method}{the specified value of input argument 'method'.}
    \item{score.invert}{the specified value of input argument 'score.invert'.}
%% ...
}
\references{
Hu, C.W., et al. "Progeny Clustering: A Method to Identify Biological Phenotypes." Scientific reports 5 (2015). \cr
http://www.nature.com/articles/srep12894
}
\author{
C.W. Hu, Rice University
}

\examples{
# a 3-cluster 2-dimensional example dataset
data('test')

# default progeny clsutering
progenyClust(test,ncluster=2:5)->pc

summary(pc)
plot(pc)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ cluster }
% __ONLY ONE__ keyword per line
