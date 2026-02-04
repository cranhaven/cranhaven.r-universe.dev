\name{randomize_trt2}
\alias{randomize_trt2}
\title{Generate Block Randomized Treatment Label Based on Covariates Matrix for Two Arm Trial}
\usage{

randomize_trt2(cov_mat=cov_mat,blocksize=blocksize,rand_ratio=c(1,1))


}

\arguments{
  \item{cov_mat}{Covariates matrix. }

  \item{blocksize}{Randomization block size}

   \item{rand_ratio}{Randomization ratio between control and treatment}

}


\description{
Generate block randomized treatment label based on covariates matrix for two arm trial.
}

