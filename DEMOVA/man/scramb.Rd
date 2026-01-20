\name{scramb}
\alias{scramb}
\title{
scrambling
}
\description{
Perform the y-scrambling method that consit to permute y values and try to develop new models. They have to be unperformants in order to validate the original one. The graph R2 vs r(y,yrandom) is created. 

}
\usage{
scramb(mydata, k, n, cercle = FALSE)
}

\arguments{
  \item{mydata}{
Dataframe containing names and values of response and descriptors
}
  \item{k}{
Number of random run
}
  \item{n}{
Number of selected descriptors of the regression (determined using Select_MLR)
}
  \item{cercle}{
Value is TRUE or FALSE (by default) . If it TRUE it's draw a circle around the point representinf the original model
}
}


\value{
Return a list of 
\item{mean}{ Mean of R^2 new model }
\item{sd}{ RStandard deviation of R^2 new model}

And also
\item{Scramb.tiff }{Description of 'comp1'}
\item{Scramb.csv }{Description of 'comp2'}
}



\references{
Tropsha, A.; Gramatica, P.; Gombar, V. K. The Importance of Being Earnest: Validation Is the
Absolute Essential for Successful Application and Interpretation of QSPR Models. Qsar \&
Combinatorial Science 2003, 22, 69-77.   \cr
Rucker, C.; Rucker, G.; Meringer, M. y-Randomization and Its Variants in QSPR/QSAR. J.
Chem. Inf. Model. 2007, 47, 2345-2357.\cr
Lindgren, F.; Hansen, B.; Karcher, W.; Sjostrom, M.; Eriksson, L. Model Validation by
Permutation Tests: Applications to Variable Selection. Journal of Chemometrics 1996, 10, 521-532.
}

\examples{
# First run Select_MLR to define n

# scramb(mydata,1000,nom,dim(MLR)[2])
}

