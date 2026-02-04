\name{randomize_trt}
\alias{randomize_trt}
\title{Generate Block Randomized Treatment Label Based on Covariates Matrix}
\usage{

randomize_trt(cov_mat=cov_mat,blocksize=blocksize,trtHR=trtHR,rand_ratio=c(1,1))


}

\arguments{
  \item{cov_mat}{Covariates matrix. }

  \item{blocksize}{Randomization block size}
  \item{trtHR}{Hazard ratio between treatment arms.}
   \item{rand_ratio}{Randomization ratio between control and treatment}

}


\description{
Generate block randomized treatment label based on covariates matrix
}

