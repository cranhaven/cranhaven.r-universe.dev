\name{RSVc}
\alias{RSVc}
\encoding{UTF-8}
\title{Ordinary (empirical) RSV (Raffinetti, Siletti and Vernizzi, 2015) curve of maximum inequality for negative attributes
}

\description{
computes the x-axis and y-axis values of the ordinary (empirical) RSV curve of maximum inequality for negative attributes.
}

\usage{
RSVc(z,w=rep(1,length(z)),plot=FALSE)
}

\arguments{ 
  \item{z}{a vector of attributes containing negative elements}
  \item{w}{a vector containing the weights associated with the elements of the attribute vector}
  \item{plot}{logical. If TRUE the ordinary (empirical) RSV curve of maximum inequality is plotted}
}
\details{
\code{RSVc(z,w)} provides the points of the ordinary (empirical) RSV curve of maximum inequality.
}

\value{
A list of class \code{RSVc} with the following components:
\item{RSV (maximum inequality) x-axis points}{vector with the x-axis values of the ordinary (empirical) RSV curve of maximum inequality}
\item{RSV (maximum inequality) y-axis points}{vector with the y-axis values of the ordinary (empirical) RSV curve of maximum inequality.}
}

\references{
E. Raffinetti, E. Siletti, A. Vernizzi (2014), Inequality measures and the issue of negative income. Italian Statistical Society Conference (SIS), Book of Short Papers: "SIS 2014. 47th Scientific Meeting of the Italian Statistical Society", CUEC (Cooperativa Universitaria Editrice Cagliaritana), 11-13 June 2014
\cr
\cr E. Raffinetti, E. Siletti, A. Vernizzi (2015), On the Gini coefficient normalization when incomes with negative values are considered, Statistical Methods & Applications, 24(3), 507-521
}

\author{Emanuela Raffinetti, Fabio Aimar}

\note{
If the vector \code{w} contains unitary elements, the plot of the ordinary (empirical) RSV curve of maximum inequality is obtained as \code{RSVc(z,plot=TRUE)}.}

\seealso{
\code{\link[ineq]{ineq}}, \code{\link[IC2]{IC2}}
}

\examples{
# generate the vector of attributes with even negative elements
z<-c(-8,-11,9,-12,7,6,35)
# plot the ordinary (empirical) RSV curve of maximum inequality
RSVc(z,plot=TRUE) 

# generate the vector of attributes with even negative elements
z<-c(12,-21,-10,6,1,-3,40)
# generate the vector of non-unitary weights
w<-c(1.2,2.3,1.6,3.5,4.7,4,2.2)
# plot the ordinary (empirical) RSV curve of maximum inequality
RSVc(z,w,plot=TRUE) 

data(BI2012)
# define the vector of non-unitary weights
w<-BI2012$weight 

# select the vector of incomes (e.g., the incomes from transfers YTA)
z<-BI2012$YTA
# plot the ordinary (empirical) RSV curve of maximum inequality
RSVc(z,w,plot=TRUE) 

# select the vector of incomes (e.g., the incomes from financial capital gain YCF)
z<-BI2012$YCF
# plot the ordinary (empirical) RSV curve of maximum inequality
RSVc(z,w,plot=TRUE) 
}