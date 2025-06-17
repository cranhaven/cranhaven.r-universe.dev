\name{dpbbm_mc_iterations}
\alias{dpbbm_mc_iterations}
\title{
dpbbm_mc_iterations
}
\description{
This is the Markov Chain Monte Carlo iterations for DPBBM
}
\usage{
dpbbm_mc_iterations(x, size.x, m = 1, max_iter = 2000, 
			a = 0.1, b = 1, tau = 1, 
			sig_alpha = 25/9, sig_beta = 25/9, 
			tau.method = "auto", debug = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a matrix of k for clustering, referring to IP reads in m6A seq data 
%%     ~~Describe \code{mat_1} here~~
}
  \item{size.x}{ a matrix of n for clustering, referring to the summation of IP reads and input reads in m6A seq data 
%%     ~~Describe \code{mat_2} here~~
}
  \item{m}{ a value indicating the auxiliary clusters used in DPBBM
  %%     ~~Describe \code{m} During auxiliary 
}
  \item{max_iter}{ maximized iterations in DPBBM
  %%     ~~Describe \code{max_iter} Maximized iterations allowed for DPBBM, max_iter should be larger than 1000 since we need 1000 burn in iterations
}
  \item{a}{ Hyperparameter a for tau
  %%     ~~Describe \code{a} During auxiliary 
}
  \item{b}{ Hyperparameter b for tau
  %%     ~~Describe \code{b} During auxiliary 
}
  \item{tau}{ Prior for tau
  %%     ~~Describe \code{tau} If tau.method is "auto", tau will be updated in each iteration based on hyperparameters. If tau.method is "stable", tau will be fixed as the exact value.
}
  \item{sig_alpha}{ variation for parameter alpha of beta distribution
  %%     ~~Describe \code{sig_alpha} During auxiliary 
}
  \item{sig_beta}{ variation for parameter beta of beta distribution
  %%     ~~Describe \code{sig_beta} During auxiliary 
}
  \item{tau.method}{ tau.method should be set to "auto" or "stable", refer to tau for detail description.
  %%     ~~Describe \code{tau.method} During auxiliary 
}
  \item{debug}{ whether DPBBM print the debug info or not. Default: FALSE
  %%     ~~Describe \code{debug} here~~
}
}
\value{
The function returns the cluster label withdrawn by DPBBM
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
# generate a simulated dataset
set.seed(123455)
S <- 4
G <- 100
K <- 3
nb_mu <- 100
nb_size <- 0.8
prob <- c(1,1,1)
mat <- bbm_data_generate(S=S,G=G,K=K,prob=prob,alpha_band=c(2,6),beta_band=c(2,6),
                     nb_mu=nb_mu,nb_size=nb_size, plotf = FALSE, max_cor=0.5) 
# check generated data
id <- order(mat$gamma);
c <- mat$gamma[id]
mat_ratio <- (mat$k+1)/(mat$n+1);
heatmap(mat_ratio[id,], Rowv = NA, Colv = NA, scale="none", RowSideColors=as.character(c), 
        xlab = "4 samples", ylab="100 RNA methylation sites")

## Run the DPBBM result. This step takes a really long time.
## You are suggested to check the pre-prepared example for a quick start
F=system.file("extdata", "DPBBM_example.html", package="DPBBM")
browseURL(url=F)

## Alternatively
# cluster_label <- dpbbm_mc_iterations(mat$k, mat$n)
# # Show the clustering result. 
# table(cluster_label)
# pie(table(mat$gamma))
# 
# # Compare the clustering result with the true clustering IDs.
# id <- order(mat$gamma);
# c <- cluster_label
# r <- rainbow(3, start = 0, end = 0.3)
# mat_ratio <- (mat$k+1)/(mat$n+1);
# heatmap(mat_ratio[id,], Rowv = NA, Colv = NA, scale="none",
#         RowSideColors = as.character(cluster_label[id]), 
#         margins = c(3,25))
}
\keyword{ statistical Inference }
