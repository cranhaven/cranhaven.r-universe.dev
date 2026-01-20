\name{prediction}
\alias{prediction}
\title{
Predictions for the test set and graph
}
\description{
Calulate the predicted values for the test set and trace the graph experimental values vs predicted values for both training and test sets. This function also give the R2 test coefficent.
}
\usage{
prediction(fit, mydata, mynewdata, n)
}

\arguments{
  \item{fit}{
Multi linear regression between property and selected descriptors
}
  \item{mydata}{
Dataframe containing names and values of response and descriptors
}
  \item{mynewdata}{
Dataframe containing property and selected descriptors values for the test set
}
  \item{n}{
Numbers of selected descriptors of the regression (determined using Select_MLR)
}
}


\value{
\item{Exp.vs.Pred.tiff }{Image representing experimental values vs predicted values for the both sets}
\item{Rext}{return the value of the determination coefficient of the test set}
}


\examples{
# This function have to be run after choise of the model.

## "Test_set.csv" should be with the following form
## ID property SelectedDesc1 SelectedDesc2 ... 

#new_nom<-'Test_set.csv'
#newdata<-read.csv(new_nom,header=TRUE , sep=" ")
#mynewdata=newdata[,2:dim[2]]

#prediction(fit,mynewdata,dim(MLR)[2])
}
