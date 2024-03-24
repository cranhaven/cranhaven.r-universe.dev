\name{GRSVc}
\alias{GRSVc}
\encoding{UTF-8}
\title{Generalized RSV (Raffinetti, Siletti and Vernizzi, 2015) curve of maximum inequality for negative attributes
}

\description{
computes the x-axis and y-axis values of the generalized RSV curve of maximum inequality for negative attributes.
}

\usage{
GRSVc(z,w=rep(1,length(z)),plot=FALSE)
}

\arguments{
  \item{z}{a vector of attributes containing negative elements}
  \item{w}{a vector containing the weights associated with the elements of the attribute vector}
  \item{plot}{logical. If TRUE the generalized RSV curve of maximum inequality is plotted}
}

\details{
\code{GRSVc(z,w)} provides the points of the generalized RSV curve of maximum inequality.
}

\value{
A list of class \code{GRSVc} with the following components:
\item{Generalized RSV (maximum inequality) x-axis points}{vector with the x-axis values of the generalized RSV curve of maximum inequality}
\item{Generalized RSV (maximum inequality) y-axis points}{vector with the y-axis values of the generalized RSV curve of maximum inequality.}
}

\references{
E. Raffinetti, E. Siletti, A. Vernizzi (2014), Inequality measures and the issue of negative income. Italian Statistical Society Conference (SIS), Book of Short Papers: "SIS2014. 47th Scientific Meeting of the Italian Statistical Society", CUEC (Cooperativa Universitaria Editrice Cagliaritana), 11-13 June 2014
\cr
\cr E. Raffinetti, E. Siletti, A. Vernizzi (2015), On the Gini coefficient normalization when incomes with negative values are considered, Statistical Methods & Applications, 24(3), 507-521
}

\author{Emanuela Raffinetti, Fabio Aimar}

\note{
If the vector \code{w} contains unitary elements, the plot of the generalized RSV curve of maximum inequality is obtained as \code{GRSVc(z,plot=TRUE)}.}
 
\seealso{
\code{\link[ineq]{ineq}}, \code{\link[IC2]{IC2}}
}

\examples{
# generate the vector of attributes with even negative elements
z<-c(-7,-15,11,-10,2,4,40)
# plot the generalized RSV curve of maximum inequality
GRSVc(z,plot=TRUE) 

# generate the vector of attributes with even negative elements
z<-c(10,-25,-12,3,2,-5,45)
# generate the vector of non-unitary weights
w<-c(1.5,2.2,1.9,3.8,4.7,5,2.3)
# plot the generalized RSV curve of maximum inequality
GRSVc(z,w,plot=TRUE) 

data(BI2012)
# define the vector of non-unitary weights
w<-BI2012$weight 

# select the vector of incomes (e.g., the incomes from transfers YTA)
z<-BI2012$YTA
# plot the generalized RSV curve of maximum inequality
GRSVc(z,w,plot=TRUE) 

# select the vector of incomes (e.g., the incomes from financial capital gain YCF)
z<-BI2012$YCF
# plot the generalized RSV curve of maximum inequality
GRSVc(z,w,plot=TRUE) 
}
