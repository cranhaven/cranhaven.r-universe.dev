\name{CTN}
\alias{CTN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{convert genetic data (nucleotides) to numerical values
%%  ~~function to do ... ~~
}
\description{This function converts genetic data (nucleotides) to numeric data.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CTN(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{x should be a dataset in fasta format
%%     ~~Describe \code{data} here~~
}
}

\details{
R is more efficient with numerical data and storage of data via numerical values takes
less memory. Genetic data consists of nucleotide data {A,T,C,G} and are usually saved
in Fasta format. After downloading the data from one of the bioinformatics repositories
and importing it to R, this function converts the data to numerical values.
}

\examples{
### import fasta data to R.
##x.dna0 <- read.fasta("dna.fasta")
###  convert data to numerical values
##x.dna<-CTN(x.dna0)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
