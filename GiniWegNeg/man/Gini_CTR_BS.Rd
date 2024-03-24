\name{Gini_CTR_BS}
\alias{Gini_CTR_BS} 
\encoding{UTF-8}
\title{The Gini coefficient adjusted for negative attributes (Chen, Tsaur and Rhai, 1982, Berebbi and Silber, 1985)}

\description{
computes the Gini coefficient adjusted for negative (even weighted) data.
}

\usage{
Gini_CTR_BS(y,w)
}

\arguments{
  \item{y}{a vector of attributes containing even negative elements}
  \item{w}{a vector containing the weights associated with the elements of the attribute vector}
}
\details{
\code{Gini_CTR_BS(y,w)} is the Gini coefficient for negative data proposed by Chen, Tsaur and Rhai (1982) and subsequenty improved by Berebbi and Silber (1985). 
It is based on a normalization factor adding the part of the concentration area lying below the x-axis and provides a value always included into the close range [0,1].}

\value{
the value of the Gini coefficient adjusted for negative attributes.
}

\references{
C.N. Chen, T.W. Tsaur, T.S. Rhai (1982), The Gini coefficient and negative income, Oxford Economic Papers 34, 473–478
\cr
\cr Z.M. Berrebi, J. Silber (1985), The Gini coefficient and negative income: a comment, Oxford Economic Papers 37, 525–526
}

\author{Emanuela Raffinetti, Fabio Aimar}

\note{If the vector \code{w} contains unitary elements, compute \code{Gini_CTR_BS} as \code{Gini_CTR_BS(y)}.}

\seealso{
\code{\link[ineq]{ineq}}, \code{\link[IC2]{IC2}}
}

\examples{
# generate the vector of attributes with even negative elements
y<-c(-7,-15,11,-10,2,4,40)
# generate the vector of weights
w<-c(2.5,1.1,3.6,4.4,0.8,2.7,1.9)
# compute the Gini coefficient adjusted for negative values
Gini_CTR_BS(y,w)

data(BI2012)
# define the vector of weights
w<-BI2012$weight 

# select the vector of incomes (e.g., the incomes from transfers YTA)
y<-BI2012$YTA
# compute the Gini coefficient adjusted for negative values
Gini_CTR_BS(y,w)

# select the vector of incomes (e.g., the incomes from financial capital gain YCF)
y<-BI2012$YCF
# compute the Gini coefficient adjusted for negative values
Gini_CTR_BS(y,w)
}

