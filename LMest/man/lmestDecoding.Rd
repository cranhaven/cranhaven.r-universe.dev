\name{lmestDecoding}
\alias{lmestDecoding.LMbasic}
\alias{lmestDecoding.LMmixed}
\alias{lmestDecoding.LMmanifest}
\alias{lmestDecoding.LMlatent}
\alias{lmestDecoding.LMbasiccont}
\alias{lmestDecoding}

\title{
Perform local and global decoding
}
\description{
Function that performs local and global decoding (Viterbi algorithm) from the output of \code{lmest}, \code{lmestCont}, and \code{lmestMixed}.
}
\usage{
lmestDecoding(est, sequence = NULL, fort = TRUE, ...)
\method{lmestDecoding}{LMbasic}(est, sequence = NULL,fort = TRUE, ...)
\method{lmestDecoding}{LMmanifest}(est, sequence = NULL, fort = TRUE, ...)
\method{lmestDecoding}{LMlatent}(est, sequence = NULL, fort = TRUE,...)
\method{lmestDecoding}{LMbasiccont}(est, sequence = NULL, fort = TRUE,...)
\method{lmestDecoding}{LMmixed}(est, sequence = NULL, fort = TRUE,...)
}
\arguments{
  \item{est}{an object obtained from a call to \code{\link{lmest}}, \code{\link{lmestCont}}, and \code{\link{lmestMixed}}}
  \item{sequence}{an integer vector indicating the units for the decoding. If \code{NULL} the whole observations are considered. (By default is set to NULL)}
    \item{fort}{to use fortran routines when possible}
  \item{\dots}{further arguments}
}

\value{
  \item{Ul }{matrix of local decoded states corresponding to each row of Y}
  \item{Ug }{matrix of global decoded states corresponding to each row of Y}
}
\references{
Viterbi A. (1967) Error Bounds for Convolutional Codes and an Asymptotically Optimum Decoding Algorithm. \emph{IEEE Transactions on Information Theory}, \bold{13}, 260-269.

Juan B., Rabiner L. (1991) Hidden Markov Models for Speech Recognition. \emph{Technometrics}, \bold{33}, 251-272.
}

\author{Francesco Bartolucci,
  Silvia Pandolfi,
  Fulvia Pennoni,
  Alessio Farcomeni,
  Alessio Serafini
}


\examples{

# Decoding for basic LM model

data("data_drug")
long <- data_drug[,-6]-1
long <- data.frame(id = 1:nrow(long),long)
long <- reshape(long,direction = "long",
                idvar = "id",
                varying = list(2:ncol(long)))

est <- lmest(index = c("id","time"),
             k = 3, 
             data = long,
             weights = data_drug[,6], 
             modBasic = 1)

# Decoding for a single sequence

out1 <- lmestDecoding(est, sequence = 1)

out2 <- lmestDecoding(est, sequence = 1:4)

# Decoding for all sequences

out3 <- lmestDecoding(est)

\dontrun{
# Decoding for LM model  with covariates on the initial and transition probabilities

data("data_SRHS_long")

SRHS <- data_SRHS_long[1:2400,]

# Categories rescaled to vary from 0 (“poor”) to 4 (“excellent”)

SRHS$srhs <- 5 - SRHS$srhs

est2 <- lmest(responsesFormula = srhs ~ NULL,
              latentFormula =  ~
              I(gender - 1) +
              I( 0 + (race == 2) + (race == 3)) +
              I(0 + (education == 4)) +
              I(0 + (education == 5)) +
              I(age - 50) + I((age-50)^2/100),
              index = c("id","t"),
              data = SRHS,
              k = 2,
              paramLatent = "difflogit",
              output = TRUE)

# Decoding for a single sequence

out3 <- lmestDecoding(est2, sequence = 1)

# Decoding for the first three sequences

out4 <- lmestDecoding(est2, sequence = 1:3)

# Decoding for all sequences

out5 <- lmestDecoding(est2)
}

}

