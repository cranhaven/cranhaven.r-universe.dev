\name{decoding}

\alias{decoding}

\title{Perform local and global decoding}

\description{Function that performs local and global decoding (Viterbi) from the output of \code{est_lm_basic}, \code{est_lm_cov_latent}, \code{est_lm_cov_manifest}, and \code{est_lm_mixed}.  \cr \cr \bold{The function is no longer maintained. Please look at} \code{\link{lmestDecoding}} \bold{function}}

\usage{
decoding(est, Y, X1 = NULL, X2 = NULL, fort = TRUE)
}

\arguments{
  \item{est }{output from \code{est_lm_basic}, \code{est_lm_cov_latent}, \code{est_lm_cov_manifest}, or \code{est_lm_mixed}}
  \item{Y }{single vector or matrix of responses}
  \item{X1 }{matrix of covariates on the initial probabilities (\code{est_lm_cov_latent}) or on the responses (\code{est_lm_cov_manifest})}
  \item{X2 }{array of covariates on the transition probabilites}
  \item{fort }{to use Fortran routines}
}

\value{
  \item{Ul }{matrix of local decoded states corresponding to each row of Y}
  \item{Ug }{matrix of global decoded states corresponding to each row of Y}
}

\references{
Viterbi A. (1967) Error Bounds for Convolutional Codes and an Asymptotically Optimum Decoding Algorithm. \emph{IEEE Transactions on Information Theory}, \bold{13}, 260-269.

Juan B., Rabiner L. (1991) Hidden Markov Models for Speech Recognition. \emph{Technometrics}, \bold{33}, 251-272.
}

\author{Francesco Bartolucci, Silvia Pandolfi, University of Perugia (IT), http://www.stat.unipg.it/bartolucci}

\examples{
\dontrun{
# example for the output from est_lm_basic

data(data_drug)
data_drug <- as.matrix(data_drug)
S <- data_drug[,1:5]-1
yv <- data_drug[,6]
n <- sum(yv)

# fit the Basic LM model

k <- 3
est <- est_lm_basic(S, yv, k, mod = 1)

# decoding for a single sequence

out1 <- decoding(est, S[1,])

# decoding for all sequences

out2 <- decoding(est, S)


# example for the output from est_lm_cov_latent with difflogit parametrization
data(data_SRHS_long)
dataSRHS <- data_SRHS_long[1:1600,]

TT <- 8
head(dataSRHS)
res <- long2matrices(dataSRHS$id, X = cbind(dataSRHS$gender-1,
dataSRHS$race == 2 | dataSRHS$race == 3, dataSRHS$education == 4,
dataSRHS$education == 5, dataSRHS$age-50,(dataSRHS$age-50)^2/100),
Y= dataSRHS$srhs)

# matrix of responses (with ordered categories from 0 to 4)
S <- 5-res$YY

# matrix of covariates (for the first and the following occasions)
# colums are: gender,race,educational level (2 columns),age,age^2)
X1 <- res$XX[,1,]
X2 <- res$XX[,2:TT,]

# estimate the model
est <- est_lm_cov_latent(S, X1, X2, k = 2, output = TRUE, param = "difflogit")
# decoding for a single sequence
out1 <- decoding(est, S[1,,], X1[1,], X2[1,,])
# decoding for all sequences
out2 <- decoding(est, S, X1, X2)
}
}
