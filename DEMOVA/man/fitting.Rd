\name{fitting}
\alias{fitting}
\title{
Performance of selected model
}
\description{
Perform a multi linear regression between property and previously selected descriptors (using select_MLR function). \cr
Calculate R2 coefficient and the predicted values from the MLR. Trace the graph experimental values vs predicted values.}
\usage{
fitting(mydata, n, property)
}

\arguments{
  \item{mydata}{
Dataframe containing names and values of response and descriptors
}
  \item{n}{
Number of selected descriptors of the regression (determined using select_MLR function)
}
  \item{property}{
Name of the studied proterty
}
}

\value{
\item{prediction_TrainSet_Y.csv }{ File containing prediction obtained using the fitting}
\item{Y_TrainingSet.tiff }{ Image representing experimental values vs predicted values for the training set}
\item{fit}{ lm object return by the function}
}


\examples{
# First run select_MLR to define n
# y<-data[,2]
# mydata<-cbind(y,MLR)
# fit<-fitting(data,dim(MLR)[2],"Name of property")
}
