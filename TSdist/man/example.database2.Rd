\name{example.database2}
\alias{example.database2}


\docType{data}
\title{Example synthetic database with series belonging to different classes.}
\description{
Example synthetic database with series belonging to 6 different classes.}

\usage{
data(example.database2);
}

\format{
 \code{example.database2} a list conformed of the following two elements:
 
 \itemize{
\item{data} {The 100 time series are stored in a numeric matrix, row-wise.}
\item{classes} {A numerical vector of length 100 that takes values in \{1,2,3,4,5,6\}. Each element in the vector represents the class of one of the series.}
}

}


\details{
 
  \code{example.database2} is a database conformed of 100 series of length 100 obtained from 6 different classes. Each class is represented by the following function: 
  
  
The class to which each series belongs is given in the \code{classes} vector.

\itemize{
\item{Class 1: random function}

\deqn{f1(t)=80+r(t)+n(t)}

\item{Class 2: periodic function}

{\deqn{f2(t)=80+15\sin(\frac{2\pi t + sh}{T})+n(t)}}

\item{Class 3: increasing linear trend}

{\deqn{f3(t)=f_3(t)=80+0.4t+n(t)+sh}}

\item{Class 4: decreasing linear trend}

{\deqn{f4(t)=80-0.4t+n(t)+sh}}

\item{Class 5: piecewise linear function which takes a value of \eqn{80+n(t)} for the first L/2+sh of the series and a value of  \eqn{90+n(t)} for the rest of the points.}

\item{Class 6: piecewise linear function which takes a value of \eqn{90+n(t)} for the first L/2+sh of the series and a value of  \eqn{80+n(t)} for the rest of the points.}


\eqn{r(t)} is a random value issued from a \eqn{N(0,3)} distribution, \eqn{L} is the length of the series, 100 in this case, and \eqn{T} is the period and is defined as a third of the length of the series. \eqn{n(t)} is a random noise obtained from a \eqn{N(0,2.8)} distribution.. Finally, \eqn{sh} is an integer value that takes a random value between \eqn{(-7,7)} and shifts the series sh positions to the right or left, depending on the sign. 

}
}

\examples{

data(example.database2);


##  The "data" element of the list contains the time series, set in a row-wise format.

plot(example.database2$data)[1,]

##  The "classes" element in example.database2 contains the classes of the series: 

example.database2$classes
}
\keyword{datasets}
