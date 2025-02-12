
\name{R-squared pruning}
\alias{Prune}
\title{R-squared pruning}
\usage{
Prune(X, alpha = 0.95, 
      pos = NULL, d.max = NULL, 
      centered = FALSE, scaled = FALSE,
      verbose = FALSE) 
}
\arguments{
\item{X}{(numeric matrix) A matrix with observations in rows and features (e.g., SNPs) in columns}

\item{alpha}{(numeric) R-squared threshold used to determine connected sets}

\item{pos}{(numeric vector) Optional vector with positions (e.g., bp) of features}

\item{d.max}{(numeric) Maximum distance that connected sets are apart}

\item{centered}{\code{TRUE} or \code{FALSE} whether columns in \code{X} are centered with mean zero}

\item{scaled}{\code{TRUE} or \code{FALSE} whether columns in \code{X} are scaled with unit standard deviation}

\item{verbose}{\code{TRUE} or \code{FALSE} to whether show progress}
}
\value{
Returns a list object that contains the elements:
\itemize{
  \item \code{prune.in}: (vector) indices of selected (unconnected) features.
  \item \code{prune.out}: (vector) indices of dropped out features.
}
}
\description{
Pruning features using an R-squared threshold and maximum distance 
}
\details{
The algorithm identifies sets of connected features as those that share an 
\ifelse{html}{\out{R<sup>2</sup> > &alpha;}}{\eqn{R^2>\alpha}{R^2 > alpha}} and retains
only one feature (first appearance) for each set.

The sets can be limited to lie within a distance less or equal to a \code{d.max} value.
}
\examples{
  require(SFSI)
  data(wheatHTP)
  
  index = c(154:156,201:205,306:312,381:387,540:544)
  X = M[,index]          # Subset markers
  colnames(X) = 1:ncol(X)
  
  # See connected sets using R^2=0.8
  R2thr = 0.8
  R2 = cor(X)^2
  nw1 = net(R2, delta=R2thr) 
  plot(nw1, show.names=TRUE)

  # Get pruned features
  res = Prune(X, alpha=R2thr)

  # See selected (unconnected) features
  nw2 = net(R2[res$prune.in,res$prune.in], delta=R2thr) 
  nw2$xy = nw1$xy[res$prune.in,]
  plot(nw2, show.names=TRUE)

}
