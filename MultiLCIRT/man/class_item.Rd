\name{class_item}
\alias{class_item}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Hierarchical classification of test items}
\description{It performs a hierarchical classification of a set of test items on the basis of 
the responses provided by a sample of subjects. The classification is based on a
sequence of likelihood ratio tests between pairs of multidimensional models suitably formulated.}
\usage{class_item(S, yv, k, link = 1, disc = 0, difl = 0, fort = FALSE,
           disp = FALSE, tol = 10^-10)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S}{matrix of all response sequences observed at least once in the sample and listed row-by-row
           (use 999 for missing response)}
  \item{yv}{vector of the frequencies of every response configuration in \code{S}}
  \item{k}{number of ability levels (or latent classes)}
  \item{link}{type of link function (1 = global logits, 2 = local logits);
              with global logits the Graded Response model
              results; with local logits the  Partial Credit results (with dichotomous responses, global logits
              is the same as using local logits resulting in the Rasch or the 2PL model depending on the value
              assigned to disc)}
  \item{disc}{indicator of constraints on the discriminating indices (0 = all equal to one, 1 = free)}
  \item{difl}{indicator of constraints on the difficulty levels (0 = free, 1 = rating scale parametrization)}
  \item{fort}{to use fortran routines when possible}
  \item{disp}{to display the likelihood evolution step by step}
  \item{tol}{tolerance level for convergence}
}

%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}

\value{
\item{merge}{input for the dendrogram represented by the \code{R} function \code{plot}}
\item{height}{input for the dendrogram represented by the \code{R} function \code{plot}}
\item{lk}{maximum log-likelihood of the model resulting from each aggregation}
\item{np}{number of free parameters of the model resulting from each aggregation}
\item{lk0}{maximum log-likelihood of the latent class model}
\item{np0}{number of free parameters of the latent class model}
\item{groups}{list of groups resulting (step-by-step) from the hierarchical clustering}
\item{dend}{hclust object to represent the histogram}
\item{call}{command used to call the function}
}

\references{
Bartolucci, F. (2007), A class of multidimensional IRT models for testing unidimensionality and clustering
items, \emph{Psychometrika}, \bold{72}, 141-157.

Bacci, S., Bartolucci, F. and Gnaldi, M. (2012), A class of Multidimensional Latent Class IRT models for 
ordinal polytomous item responses, \emph{Technical report}, http://arxiv.org/abs/1201.4667.}

\author{Francesco Bartolucci, Silvia Bacci, Michela Gnaldi - University of Perugia (IT)}

%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{
\dontrun{
## Model-based hierarchical classification of items from simulated data
# Setup
r = 6  # number of items
n = 1000  # sample size
bev = rep(0,r) 
k = r/2
multi = rbind(1:(r/2),(r/2+1):r)
L = chol(matrix(c(1,0.6,0.6,1),2,2))
data = matrix(0,n,r)
model = 1 
# Create data
Th = matrix(rnorm(2*n),n,2)%*%L   
for(i in 1:n) for(j in 1:r){
	if(j<=r/2){
    	pc = exp(Th[i,1]-bev[j]); pc = pc/(1+pc)
	}else{
		pc = exp(Th[i,2]-bev[j]); pc = pc/(1+pc)
    }
    data[i,j] = runif(1)<pc
}
# Aggregate data
out = aggr_data(data)
S = out$data_dis
yv = out$freq
# Create dendrogram for items classification, by assuming k=3 latent
# classes and a Rasch parameterization
out = class_item(S,yv,k=3,link=1)
summary(out)
plot(out$dend)
}

\dontrun{
## Model-based hierarchical classification of NAEP items
# Aggregate data
data(naep)
X = as.matrix(naep)
out = aggr_data(X)
S = out$data_dis
yv = out$freq
# Create dendrogram for items classification, by assuming k=4 latent
# classes and a Rasch parameterization
out = class_item(S,yv,k=4,link=1)   
summary(out)
plot(out$dend)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classification}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
