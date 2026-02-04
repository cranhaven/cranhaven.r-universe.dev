\name{dummy_convert}
\alias{dummy_convert}
\title{Convert Categorical Data to Dummy Variables}
\usage{
dummy_convert(dataset,column_names)


}

\arguments{
  \item{dataset}{Data frame that contains the categorical columns}


  \item{column_names}{A list of column names that need to be converted to dummy variables.}

}


\description{
Convert Categorical Data to Dummy Variables
}
\examples{

x<-data.frame(trt=as.factor(rbinom(100,1,0.5)), Gender=as.factor(rbinom(100,1,0.5)))

dummy_convert(x,c("trt","Gender"))

}
