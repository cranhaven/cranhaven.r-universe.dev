\name{findFirstLast}
\alias{findFirstLast}

\title{
The first or the last occurence of a unique value.
}

\description{
This function can be used to find the first or the last occurence of each unique value in a column.
}

\usage{
findFirstLast(myDF, findFirst = TRUE)
}

\arguments{
  \item{myDF}{
a data.frame or a matrix.
}
  \item{findFirst}{
a logical variable indicating the first or the last occurence.
}
}

\details{
By default, it finds the first occurence (i.e. findFirst = TRUE). If instead, we want to find the last, we must set findFirst = FALSE.
}

\author{
Artemis Toumazi <artemis.toumazi@gmail.com>, Sarah Zohar <sarah.zohar@inserm.fr>, Anand N. Vidyashankar <avidyash@gmu.edu>, Jie Xu <jxu13@gmu.edu> and Moreno Ursino <moreno.ursino@inserm.fr>
}

\seealso{
\code{\link{VarWT}}
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
  
  analyf0 <- Toxdata[order(Toxdata$simulation), ] # sort the imported data by simulation.
  
  ## Using the function findFirstLast to find when starts the first trial. ##
  first.trial = findFirstLast(analyf0)$Trial
}
