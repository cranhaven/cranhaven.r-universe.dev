\name{bbm_data_generate}
\alias{bbm_data_generate}
\title{
bbm_data_generate
}
\description{
This is to generate the simulation data based on Beta-bionomial mixture model
}
\usage{
bbm_data_generate(S=3, G=50, K=3, prob=rep(1,times=3),
                              alpha_band=c(2,6),
                              beta_band=c(2,6),
                              nb_mu=100,nb_size=0.2, plotf = FALSE, 
                              max_cor=0.5)
}                              
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S}{ Number of samples in the simulated data
%%     ~~Describe \code{S} here~~
}
  \item{G}{ Number of sites in the simulated data
%%     ~~Describe \code{G} here~~
}
  \item{K}{ Number of clusters that exist in the simulated data
  %%     ~~Describe \code{K} 
}
  \item{prob}{ the cluster weight for each cluster
  %%     ~~Describe \code{prob} prob should be a vector, which has the length of K
}
  \item{alpha_band}{ the region used to generate the parameter of beta distribution alpha
  %%     ~~Describe \code{alpha_band} 
}
  \item{beta_band}{ the region used to generate the parameter of beta distribution beta
  %%     ~~Describe \code{beta_band} 
}
  \item{nb_mu}{ alternative parametrization via mean for Negative Binomial distribution
  %%     ~~Describe \code{nb_mu} 
}
  \item{nb_size}{ target for number of successful trials, or dispersion parameter (the shape parameter of the gamma mixing distribution) for Negative binomial distrition. Must be strictly positive, need not be integer.
  %%     ~~Describe \code{nb_size} 
}
  \item{plotf}{ option for whether plot the generated data according to clusters or not
  %%     ~~Describe \code{plotf} 
}
  \item{max_cor}{ The maximized correlation allowed for the simulated data, which used to guarantee the data is not highly correlated.
  %%     ~~Describe \code{max_cor} 
}

}
\value{
The function returns simulation data generated based on beta binomial mixture model
}

\details{
The Dirichlet Process based beta-binomial mixture model clustering

}

\references{
Reference coming soon!
}
\author{ Lin Zhang, PhD  <lin.zhang@cumt.edu.cn>
%%  ~~who you are~~
}

\examples{
set.seed(123455)
S <- 4
G <- 100
K <- 3
nb_mu <- 100
nb_size <- 0.8
prob <- c(1,1,1)
mat <- bbm_data_generate(S=S,G=G,K=K,prob=prob,alpha_band=c(2,6),beta_band=c(2,6),
                         nb_mu=nb_mu,nb_size=nb_size, plotf = TRUE, max_cor=0.5) 
table(mat$gamma)
pie(mat$gamma)
id <- order(mat$gamma);
c <- mat$gamma[id]
mat_ratio <- (mat$k+1)/(mat$n+1);
heatmap(mat_ratio[id,], Rowv = NA, Colv = NA, scale="none", RowSideColors=as.character(c), 
        xlab = "4 samples", ylab="100 RNA methylation sites")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ statistical Inference }
