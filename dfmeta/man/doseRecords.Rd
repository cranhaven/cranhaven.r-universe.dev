\name{doseRecords}
\alias{doseRecords}

\title{
Random Effect Analysis
}

\description{
This function make a random effects analysis that can be used after in the random effects model based algorithm. 
}

\usage{
doseRecords(data)
}

\arguments{
  \item{data}{
a data frame containing information for each patient at each simulation and trial.
}
}

\references{
Zohar, S. , Katsahian, S. and O'Quigley, J. (2011), An approach to meta analysis of dose finding studies. Statist. Med., <doi:10.1002/sim.4121>.
}

\author{
Artemis Toumazi <artemis.toumazi@gmail.com>, Sarah Zohar <sarah.zohar@inserm.fr>, Anand N. Vidyashankar <avidyash@gmu.edu>, Jie Xu <jxu13@gmu.edu> and Moreno Ursino <moreno.ursino@inserm.fr>
}

\seealso{
\code{\link{MA_estimates}}, \code{\link{glimem}}
}

\examples{
  ################################################################
  ### Give a toxicity data for each simulation and each trial. ###
  ################################################################
  
  ### Using a toxicity data for each simulation and each trial including in the dfmeta package.
  data("Toxdata") 
  dim(Toxdata)
  colnames(Toxdata)
  
  ## NOTE: The toxicity dataset must have the same structure as below. ##
  ## Check it before running the VarWT function! ##
  str(Toxdata)
  
  analyf0 <- Toxdata[order(Toxdata$simulation), ]


  ####################################################
  ## Taking the records from random effect analysis ##
  ####################################################
  rec <- doseRecords(analyf0)
}

