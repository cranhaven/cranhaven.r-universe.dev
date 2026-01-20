\name{graphe_3Sets}
\alias{graphe_3Sets}
\title{
Predictions for the external validation set and graph
}
\description{
Calulate the predicted values for the external validation set and trace the graph experimental values vs predicted values for training, test and external validation sets. 
}
\usage{
graphe_3Sets(fit, mydata, mynewdata, mynewdata2, n)
}

\arguments{
  \item{fit}{
Multi linear regression between property and selected descriptors (lm object)
}
  \item{mydata}{
Dataframe containing names and values of response and descriptors
}
  \item{mynewdata}{
Dataframe containing property and selected descriptors values for the test set
}
  \item{mynewdata2}{
Dataframe containing property and selected descriptors values for the external validation set
}
  \item{n}{
Numbers of selected descriptors of the regression (determined using select_MLR)
}
}

\value{
\item{Rext,Rext2 }{return a list containing the value of the determination coefficient of the test set and of the external validation set}
\item{Graphe_3sets.tiff }{Image representing experimental values vs predicted values for the all three sets}
}

\examples{
# This function have to be run last!

## "Test_set.csv" should be with the following form
## ID property SelectedDesc1 SelectedDesc2 ... 

# new_nom<-'Test_set.csv'
# newdata<-read.csv(new_nom,header=TRUE , sep=" ")
# mynewdata=newdata[,2:dim[2]]


## "External_set.csv" should be with the following form
## ID property SelectedDesc1 SelectedDesc2 ... 

# new_nom2<-'External_set.csv'
# newdata2<-read.csv(new_nom2,header=TRUE , sep=" ")
# mynewdata2=newdata2[,2:dim[2]]

#graphe_3Sets(fit,mynewdata,mynewdata2,dim(MLR)[2])
}
