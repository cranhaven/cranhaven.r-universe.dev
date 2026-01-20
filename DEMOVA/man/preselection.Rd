\name{preselection}
\alias{preselection}
\title{
Suppression of missing or constant descriptors
}
\description{
Remove descriptors with missing values and a variance lower than 0.001. 
}
\usage{
preselection(desc)
}

\arguments{
  \item{desc}{
Dataframe containing the names of desciptors and their values
}
}

\value{
return a dataframe without the removed variables
}

\examples{
## The input file should be with the following form
## id_molecule propriete x1 x2 x3 ... # Header line
## molecule1 1 0.02 500 ...
## molecule2 5 0.06 600 ...

# nom<-"NameOfInputFile.csv"
# data<-read.csv(nom,header = TRUE , sep=" ") 
# dim<-dim(data)
# mydesc<-data[,3:dim[2]]
# id<-data[,1]
# y<-data[,2] 

# d<-preselection(mydesc)

}
