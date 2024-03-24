\name{PGini_RSV}
\alias{PGini_RSV}
%- Also NEED an '\alias' for EACH other topic documented here. 
\title{
The pseudo-Gini (concentration) coefficient adjusted for negative attributes (Raffinetti, Siletti and Vernizzi, 2016)
}

\description{
computes the pseudo-Gini (concentration) coefficient adjusted for negative data.
}

\usage{
PGini_RSV(y,s,w)
}

\arguments{
  \item{y}{a vector of attributes usually corresponding to the vector of total incomes}
  \item{s}{a vector of attributes containing even negative elements, usually corresponding to one of the income sources composing the total income}
  \item{w}{a vector containing the weights associated with the elements of the two attribute vectors}
}

\details{
\code{PGini_RSV(y,s,w)} is the pseudo-Gini (concentration) coefficient for negative income data proposed by Raffinetti, Siletti and Vernizzi (2016) and based on the 
Raffinetti, Siletti and Vernizzi (2015) normalization. It provides a value always included into the close range [-1,+1]. The pseudo-Gini (concentration) coefficient is 
built by ordering the values of vector s according to the values of vector y sorted in non-decreasing order.}

\value{
the value of the pseudo-Gini (concentration) coefficient adjusted for negative attributes.
}

\references{
E. Raffinetti, E. Siletti, A. Vernizzi (2016), Analyzing the effects of negative and non-negative values on income inequality. Evidence from the Survey of Household Income and
Wealth of the Bank of Italy (2012), Social Indicators Research (published on line http://link.springer.com/article/10.1007%2Fs11205-016-1354-x)
}

\author{Emanuela Raffinetti, Fabio Aimar}

\note{If the vector \code{w} contains unitary elements, compute \code{PGini_RSV} as \code{PGini_RSV(y,s)}.}

\seealso{
\code{\link[IC2]{IC2}}}

\examples{
# generate the vector of total incomes 
y<-c(-7,-15,11,-10,2,4,40)
# generate the vector of one of the income sources composing the total income
s<-c(-3,6,-5,9,5,-1,-15)
# generate the vector of weights
w<-c(2.5,1.1,3.6,4.4,0.8,2.7,1.9)
# compute the pseudo-Gini (concentration) coefficient 
PGini_RSV(y,s,w)

data(BI2012)
# define the vector of weights
w<-BI2012$weight 
# select the vector of the total net income Y
y<-BI2012$Y

# select the vector of the income source (e.g., the incomes from transfers YTA)
s<-BI2012$YTA
# compute the pseudo-Gini (concentration) coefficient  
PGini_RSV(y,s,w)

# select the vector of the income source (e.g., the incomes from financial capital gain YCF)
s<-BI2012$YCF
# compute the pseudo-Gini (concentration) coefficient 
PGini_RSV(y,s,w)
}


