\name{simGenoFunc}
\alias{simGenoFunc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulate Genotype Data from a Mixture of 3 Bayesian Hierarchical Models
}
\description{
Simulate Genotype Data from a Mixture of 3 Bayesian Hierarchical Models.
The minor allele frequency (MAF) of cases has the same prior
as that of controls. 
}
\usage{
simGenoFunc(nCases = 100, 
            nControls = 100, 
            nSNPs = 1000, 
            alpha.p = 2, 
            beta.p = 5, 
            pi.p = 0.1, 
            alpha0 = 2, 
            beta0 = 5, 
            pi0 = 0.8, 
            alpha.n = 2, 
            beta.n = 5, 
            pi.n = 0.1, 
            low = 0.02, 
            upp = 0.5, 
            verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nCases}{
integer. Number of cases.
}
  \item{nControls}{
integer. Number of controls.
}
  \item{nSNPs}{
integer. Number of SNPs.
}
  \item{alpha.p}{
numeric. The first shape parameter of Beta prior in cluster \eqn{+}.
}
  \item{beta.p}{
numeric. The second shape parameter of Beta prior in cluster \eqn{+}.
}
  \item{pi.p}{
numeric. Mixture proportion for cluster \eqn{+}.
}
  \item{alpha0}{
numeric. The first shape parameter of Beta prior in cluster \eqn{0}.
}
  \item{beta0}{
numeric. The second shape parameter of Beta prior in cluster \eqn{0}.
}
  \item{pi0}{
numeric. Mixture proportion for cluster \eqn{0}.
}
  \item{alpha.n}{
numeric. The first shape parameter of Beta prior in cluster \eqn{-}.
}
  \item{beta.n}{
numeric. The second shape parameter of Beta prior in cluster \eqn{-}.
}
  \item{pi.n}{
numeric. Mixture proportion for cluster \eqn{-}.
}
  \item{low}{
numeric. A small positive value. If a MAF generated from half-flat shape
bivariate prior is smaller than \code{low}, we will delete the SNP to be generated.
}
  \item{upp}{
numeric. A positive value. If a MAF generated from half-flat shape
bivariate prior is greater than \code{upp}, we will delete the SNP to be generated.
}
  \item{verbose}{
logical. Indicating if intermediate results or final results should be output 
to output screen.
}
}
\details{
In this simulation, we generate additive-coded genotypes for 3 clusters of SNPs
 based on a mixture of 3 Bayesian hierarchical models. 

In cluster \eqn{+}, the minor allele frequency
(MAF) \eqn{\theta_{x+}} of cases is greater than the MAF \eqn{\theta_{y+}} of
controls.

In cluster \eqn{0}, the MAF \eqn{\theta_{0}} of cases is equal to 
the MAF of controls.

In cluster \eqn{-}, the MAF \eqn{\theta_{x-}} of cases is smaller than 
the MAF \eqn{\theta_{y-}} of controls.

The proportions of the 3 clusters of SNPs are \eqn{\pi_{+}},
\eqn{\pi_{0}}, and \eqn{\pi_{-}}, respectively.


We assume a \dQuote{half-flat shape} bivariate prior for the MAF in
cluster \eqn{+}
\deqn{2h_{+}\left(\theta_{x+}\right)h_{+}\left(\theta_{y+}\right)
I\left(\theta_{x+}>\theta_{y+}\right),
}
where \eqn{I(a)} is hte indicator function taking value \eqn{1} if the event 
\eqn{a} is true, and value \eqn{0} otherwise.
The function \eqn{h_{+}} is the probability density function of the 
beta distribution \eqn{Beta\left(\alpha_{+}, \beta_{+}\right)}.

We assume \eqn{\theta_{0}} has the beta prior \eqn{Beta(\alpha_0, \beta_0)}. 

We also assume a \dQuote{half-flat shape} bivariate prior for the MAF in
cluster \eqn{-}
\deqn{2h_{-}\left(\theta_{x-}\right)h_{-}\left(\theta_{y-}\right)
I\left(\theta_{x-}>\theta_{y-}\right).
}
The function \eqn{h_{-}} is the probability density function of the 
beta distribution \eqn{Beta\left(\alpha_{-}, \beta_{-}\right)}.

Given a SNP, we assume Hardy-Weinberg equilibrium holds for its genotypes. 
That is, given MAF \eqn{\theta}, the probabilities of genotypes are
\deqn{Pr(geno=2) = \theta^2}
\deqn{Pr(geno=1) = 2\theta\left(1-\theta\right)}
\deqn{Pr(geno=0) = \left(1-\theta\right)^2}

We also assume the genotypes \eqn{0} (wild-type), \eqn{1} (heterozygote), and 
\eqn{2} (mutation) follows a multinomial distribution
\eqn{Multinomial\left\{1, \left[
\theta^2, 2\theta\left(1-\theta\right),
\left(1-\theta\right)^2
\right]\right\}}

Note that when we generate MAFs from the half-flat shape bivariate priors, 
we might get very small MAFs or get MAFs \eqn{>0.5}. In these cased,
we then delete this SNP.

So the final number of SNPs generated might be less than the initially-set 
number of SNPs.

}
\value{
An ExpressionSet object stores genotype data.
}
\references{
  Yan X, Xing L, Su J, Zhang X, Qiu W. 
  Model-based clustering for identifying disease-associated SNPs in case-control genome-wide association studies. 
  Scientific Reports 9, Article number: 13686 (2019) https://www.nature.com/articles/s41598-019-50229-6.
}
\author{
Yan Xu <yanxu@uvic.ca>, 
Li Xing <sfulxing@gmail.com>, 
Jessica Su <rejas@channing.harvard.edu>, 
Xuekui Zhang <xuekui@uvic.ca>, 
Weiliang Qiu <Weiliang.Qiu@gmail.com>
}
%\note{
%%  ~~further notes~~
%}

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
set.seed(2) 

esSim = simGenoFunc(
  nCases = 100,
  nControls = 100,
  nSNPs = 500,
  alpha.p = 2, beta.p = 5, pi.p = 0.1,
  alpha0 = 2, beta0 = 5, pi0 = 0.8,
  alpha.n = 2, beta.n = 5, pi.n = 0.1,
  low = 0.02, upp = 0.5, verbose = FALSE
)

print(esSim)
pDat = pData(esSim)
print(pDat[1:2,])
print(table(pDat$memSubjs))

fDat = fData(esSim)
print(fDat[1:2,])
print(table(fDat$memGenes))
print(table(fDat$memGenes2))
}
\keyword{ method }% use one of  RShowDoc("KEYWORDS")
