\name{self_blockrand}
\alias{self_blockrand}
\title{Block Randomization}
\usage{

self_blockrand(N=N,trt_levels=trt_levels,blocksize=blocksize,rand_ratio=rand_ratio)


}

\arguments{
  \item{N}{Total number of patients }


  \item{trt_levels}{treatment levels in vector. for example, c(0,1) for control vs treatment trial.}
  \item{blocksize}{Randomization block size}
   \item{rand_ratio}{Randomization ratio between control and treatment}

}


\description{
Block randomization
}
\examples{

self_blockrand(N=100,trt_levels=c(0,1),blocksize=2,rand_ratio=c(1,1))

}
