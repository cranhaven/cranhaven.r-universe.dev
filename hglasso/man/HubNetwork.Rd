\name{HubNetwork}
\alias{HubNetwork}
\title{
Hub network generation
}
\description{
Generate an inverse covariance matrix, covariance matrix, or binary network with hub structure
}
\usage{
HubNetwork(p, sparsity, hubnumber, hubsparsity, type = "Gaussian")
}

\arguments{
  \item{p}{
The number of features
}
  \item{sparsity}{
Sparsity of the network
}
  \item{hubnumber}{
The number of hubs in the network
}
  \item{hubsparsity}{
Sparsity level within each hub
}
  \item{type}{
Type of network. The default value type="Gaussian" generates an inverse covariance matrix.  type="covariance" generates a covariance matrix with hubs.  type="binary" generates a binary network with hubs.  
}
}

\value{
\item{Theta}{Theta is the generated inverse covariance matrix, covariance matrix, or binary network.}
\item{hubcol}{hubcol contains indices for features that are hubs.}
}
\references{
Tan et al. (2014). Learning graphical models with hubs. To appear in Journal of Machine Learning Research.  arXiv.org/pdf/1402.7349.pdf.
}
\author{
Kean Ming Tan}
\examples{
# Generate inverse covariance matrix with 5 hubs
# 30\% of the elements within a hub are zero 
# 95\% of the elements that are not within hub nodes are zero
p <- 100
Theta <- HubNetwork(p,0.95,5,0.3)$Theta

# Generate covariance matrix with 5 hubs with similar structure
Sigma <- HubNetwork(p,0.95,5,0.3,type="covariance")$Theta

# Generate binary network with 2 hubs with p=10
Theta <- HubNetwork(p=10,0.95,2,0.3,type="binary")$Theta
}


