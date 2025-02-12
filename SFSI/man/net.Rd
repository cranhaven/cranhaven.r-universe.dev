\name{Graphical Network from a numeric matrix}
\alias{net}
\title{Graphical Network}
\usage{
net(object, K = NULL,
    nsup = NULL, p.radius = 1.7,
    delta = .Machine$double.eps)
}
\arguments{
\item{object}{Either a numeric matrix \code{X} or an object of the 'SGP' class. When the object is of the 'SGP' class the regression coefficients are used as \code{X}}

\item{K}{(numeric matrix) Kinship relationship matrix among nodes}

\item{nsup}{(numeric) For a SGP, average number of training individuals contributing to the prediction (with non-zero regression coefficient) of testing individuals. Default \code{nsup = NULL} will use the value of \code{nsup} that yielded the optimal accuracy}

\item{p.radius}{(numeric) For a multi-trait SGP, a factor (x-folds radius) to separate each trait from the origin}

\item{delta}{(numeric) Minumum value to determine nodes to be connected. Default is the machine precision (numerical zero)}
}
\value{
Returns a plottable object of the class 'net' that can be visualized using 'plot' method
}
\description{
Obtain a Graphical Network representation from a matrix where nodes are subjects in the rows and columns, and edges are obtained from the matrix entries 
}
\details{
For a numeric matrix \ifelse{html}{\out{<b>X</b>=\{x<sub>ij</sub>\}}}{\eqn{\textbf{X}=\{x_{ij}\}}{X=\{x_ij\}}} with \code{m} rows and \code{n} columns, a graphical network with \code{m} + \code{n} nodes is obtained by defining edges connecting subjects in rows with those in columns. An edge between subject in row \code{i} and subject in column \code{j} is
determined if the corresponding (absolute) entry matrix is larger than certain value, i.e., \ifelse{html}{\out{|x<sub>ij</sub>|&gt;&delta;}}{\eqn{|x_{ij}|>\delta}{|x_ij|>delta}}.

For a symmetric matrix, only \code{m}=\code{n} nodes are considered with edges determined by the above diagonal entries of the matrix.

Nodes and edges are plotted in the cartesian plane according to the Fruchterman-Reingold algorithm. When a matrix \ifelse{html}{\out{<b>K</b>}}{\eqn{\textbf{K}}{K}} is provided, nodes are plotted according to the top 2 eigenvectors from the spectral value decomposition of \ifelse{html}{\out{<b>K</b> = <b>U D U'</b>}}{\eqn{\textbf{K} = \textbf{U D U'}}{K = U D U'}}.

When the object is a 'SGP' class object the edges are taken from the regression coefficients (associated to a specific \code{nsup} value) are used as matrix \ifelse{html}{\out{<b>X</b>}}{\eqn{\textbf{X}}{X}} with testing subjects in rows and training subjects in columns.  
}
\examples{
  require(SFSI)
  data(wheatHTP)
  
  #--------------------------------------
  # Net for an SGP object
  #--------------------------------------
  index = which(Y$trial \%in\% 1:6)     # Use only a subset of data
  Y = Y[index,]
  M = scale(M[index,])/sqrt(ncol(M))  # Subset and scale markers
  G = tcrossprod(M)                   # Genomic relationship matrix
  y = as.vector(scale(Y[,"E1"]))      # Scale response variable
  
  # Training and testing sets
  tst = which(Y$trial \%in\% 2)
  trn = seq_along(y)[-tst]

  fm = SGP(y, K=G, trn=trn, tst=tst)
  
  nt = net(fm)          # Get the net
  plot(nt)              # Plot the net
  plot(nt, i=c(1,5))    # Show the first and fifth tst elements
  plot(net(fm, nsup=10), show.names=c(TRUE,TRUE,FALSE))
  \donttest{
  #--------------------------------------
  # Net for a numeric matrix
  #--------------------------------------
  B = as.matrix(coef(fm, nsup=10))
  plot(net(B), curve=TRUE, set.size=c(3.5,1.5,1))
  
  #--------------------------------------
  # Net for a symmetric numeric matrix
  #--------------------------------------
  X = X_E1[,seq(1,ncol(X_E1),by=5)]
  R2 = cor(X)^2  # An R2 matrix
  plot(net(R2, delta=0.9))
  }
}

