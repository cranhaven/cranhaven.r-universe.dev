\name{esSimDiffPriors}
\alias{esSimDiffPriors}
\docType{data}
\title{
An ExpressionSet Object Storing Simulated Genotype Data
}
\description{
An ExpressionSet object storing simulated genotype data.
The minor allele frequency (MAF) of cases has different prior
than that of controls. 
}
\usage{data("esSimDiffPriors")}
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
\deqn{2h_{x+}\left(\theta_{x+}\right)h_{y+}\left(\theta_{y+}\right)
I\left(\theta_{x+}>\theta_{y+}\right),
}
where \eqn{I(a)} is hte indicator function taking value \eqn{1} if the event 
\eqn{a} is true, and value \eqn{0} otherwise.
The function \eqn{h_{x+}} is the probability density function of the 
beta distribution \eqn{Beta\left(\alpha_{x+}, \beta_{x+}\right)}.
The function \eqn{h_{y+}} is the probability density function of the 
beta distribution \eqn{Beta\left(\alpha_{y+}, \beta_{y+}\right)}.

We assume \eqn{\theta_{0}} has the beta prior \eqn{Beta(\alpha_0, \beta_0)}. 

We also assume a \dQuote{half-flat shape} bivariate prior for the MAF in
cluster \eqn{-}
\deqn{2h_{x-}\left(\theta_{x-}\right)h_{y-}\left(\theta_{y-}\right)
I\left(\theta_{x-}>\theta_{y-}\right).
}
The function \eqn{h_{x-}} is the probability density function of the 
beta distribution \eqn{Beta\left(\alpha_{x-}, \beta_{x-}\right)}.
The function \eqn{h_{y-}} is the probability density function of the 
beta distribution \eqn{Beta\left(\alpha_{y-}, \beta_{y-}\right)}.

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

We set the number of cases as \eqn{100}, the number of controls as 
\eqn{100},
and the number of SNPs as \eqn{1000}.

The hyperparameters are
\eqn{\alpha_{x+}=2}, \eqn{\beta_{x+}=3}, 
\eqn{\alpha_{y+}=2}, \eqn{\beta_{y+}=8}, 
\eqn{\pi_{+}=0.1},

\eqn{\alpha_{0}=2}, \eqn{\beta_{0}=5}, \eqn{\pi_{0}=0.8},

\eqn{\alpha_{x-}=2}, \eqn{\beta_{x-}=8}, 
\eqn{\alpha_{y-}=2}, \eqn{\beta_{y-}=3}, 
\eqn{\pi_{-}=0.1}.

Note that when we generate MAFs from the half-flat shape bivariate priors, 
we might get very small MAFs or get MAFs \eqn{>0.5}. In these cased,
we then delete this SNP.

So the final number of SNPs generated might be less than the initially-set 
number \eqn{1000} of SNPs.

For the dataset stored in \code{esSim}, there are \eqn{838} SNPs.
\eqn{64} SNPs are in cluster {-}, \eqn{708} SNPs are in cluster \eqn{0},
and \eqn{66} SNPs are in cluster \eqn{+}.

}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
\references{
  Yan X, Xing L, Su J, Zhang X, Qiu W. 
  Model-based clustering for identifying disease-associated SNPs in case-control genome-wide association studies. 
  Scientific Reports 9, Article number: 13686 (2019) https://www.nature.com/articles/s41598-019-50229-6.
}
\examples{
data(esSimDiffPriors)
print(esSimDiffPriors)

pDat=pData(esSimDiffPriors)
print(pDat[1:2,])
print(table(pDat$memSubjs))

fDat=fData(esSimDiffPriors)
print(fDat[1:2,])
print(table(fDat$memGenes))
print(table(fDat$memGenes2))
}
\keyword{datasets}
