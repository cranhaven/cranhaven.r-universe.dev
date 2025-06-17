\name{panel.cor.pval}
\alias{panel.cor.pval}
\title{
accessory function for pairs() to display pvalue of the Pearson correlation
}
\description{
works as upper.panel or lower.panel argument of pairs() (package graphics). Displays pvalues better than 0.1.  
}
\usage{
panel.cor.pval(x, y, digits = 2, cex.cor, p.cut=0.1)
}
\arguments{
  \item{x}{
x element of the pairs() matrix
}
  \item{y}{
y element of the pairs() matrix
}
  \item{digits}{
number of non-zero digits to leave at the end
}
  \item{cex.cor}{
scaling factor for displayed text
}
  \item{p.cut}{
p-value cutoff
}
}
\references{
cannibalized from an example in ?pairs (package graphics)
}
