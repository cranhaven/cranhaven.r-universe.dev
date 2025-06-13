\name{simSBS}
\alias{simSBS}
\title{
Simulate single base substitutions
}
\description{
Given the number of genomes to be created, generate single base substitutions in those genomes from simulated mutational processes, by referring to a human reference genome.
}
\usage{
simSBS(nSigs = NULL, nGenomes = NULL, refGenome = NULL,
        similarity = 0.6, noise = 0,
        presetSigs = NULL, chrs = NULL, nMutPerGenome = NULL,
        sigPrevalence = NULL, chrDistribution = NULL,
        parallel = TRUE, saveDir = './')
}
\arguments{
  \item{nSigs}{
    \strong{Required}. The number of mutational processes to be created.
  }
  \item{nGenomes}{
    \strong{Required}. The number of genomes in which to simulate single base substitutions.
  }
  \item{refGenome}{
    \strong{Required}. A \code{\link{BSgenome}} object of human reference genome.
  }
  \item{similarity}{
    Optional. Limit the similarity between any two mutational proccesses. 0 indicates no similarity while 1 indicates the opposite. Lower similarity may require more time to simulate.
  }
  \item{noise}{
    Optional. The value should between 0 and 1, indicating the amount of random mutations (noise) added to each simulated genome. 0 indicates no noise while 1 indicates the amount of noise is equal to the amount of mutation.
  }
  \item{presetSigs}{
    Optional. Use user defined mutational processes to simulate mutations in the genome. It should be a 96-by-n matrix, where 96 denotes the number of mutation motifs while n denotes the number of mutational processes. If presetSigs is given, nSigs = n.
  }
  \item{chrs}{
    Optional. On wich chromosome(s) mutations simulated from. Default is \code{c(1:22, 'X', 'Y')}. This argument accepts a vector that indicates chromosomes, which should be a vector created by manual input or, for example, using R code like \code{c(1:22, 'X', 'Y', 'M')}, where 'X', 'Y', 'M' are case sensitive (upper case) and indicate chromosome X, Y and mitochondrial chromosome. Incompatible input could cause fatal errors cause of unidentifiable chromosome name.
  }
  \item{nMutPerGenome}{
    Optional. \emph{NULL} or a numerical vector whose length equals nGenomes. Number of mutations on each genome to simulate. If not defined, Default will use the \href{https://doi.org/10.1101/112367}{distribution} of number of single base substitutions in all WGS projects of \href{https://dcc.icgc.org}{ICGC} release 23.
  }
  \item{sigPrevalence}{
    Optional. Acceptable values are either \emph{NULL} or \emph{a numerical vector}. The prevalence of mutational processes in wild. The default uses known prevalances of 21 processes from \href{http://dx.doi.org/10.1038/nature12477}{Alexandrov et al.}'s work.
  }
  \item{chrDistribution}{
    Optional. \emph{NULL} or \emph{a numerical vector} are acceptable. The percentage of mutations assigned to each chromosome in a genome. The default uses the distribution of length of chromosomes (chr1 to chr22 and chrX and chrY). If a numerical vector was given, its length should equal the length of \emph{chrs} and values should sum up to 1.
  }
  \item{parallel}{
    Optional. \code{TRUE} or \code{FALSE}. Whether enable or disable parallel computing ability.
  }
  \item{saveDir}{
    Optional. The directory where to save simulation output. Default is the current working directory. Other paths should also be relative to the current working directory.
  }
}
\value{
  If succeed, the return value is 1. Simulation results are saved in saveDir.
}
\examples{
if(require(BSgenome.Hsapiens.UCSC.hg38)){
  simSBS(nSigs = 2, nGenomes = 2,
    refGenome = BSgenome.Hsapiens.UCSC.hg38::BSgenome.Hsapiens.UCSC.hg38,
    nMutPerGenome = sample(10:50, 2),
    parallel = FALSE)
}else{
  message('Cannot proceed withoud a valid reference genome.')
}
}
\keyword{simulate}
