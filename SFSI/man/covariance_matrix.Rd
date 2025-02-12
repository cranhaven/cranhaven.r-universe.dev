
\name{2. Covariance matrix transformations}
\alias{cov2dist}
\alias{cov2cor2}
\title{Conversion of a covariance matrix to a distance/correlation matrix}
\usage{
cov2dist(A, a = 1, inplace = FALSE)

cov2cor2(A, a = 1, inplace = FALSE)

}
\arguments{
\item{A}{(numeric matrix) Variance-covariance matrix}

\item{inplace}{\code{TRUE} or \code{FALSE} to whether operate directly on the input matrix.
When \code{TRUE} no result is produced but the input \code{A} is modified. Default \code{inplace=FALSE}}

\item{a}{(numeric) A number to multiply the whole resulting matrix by. Default \code{a = 1}}

}
\value{
Function 'cov2dist' returns a matrix containing the Euclidean distances. Function 'cov2cor2' returns a correlation matrix
}
\description{
Transformation into correlation matrix or distance matrix from a covariance matrix
}
\details{
For any variables
\ifelse{html}{\out{X<sub>i</sub>}}{\eqn{X_i}{X_i}} and
\ifelse{html}{\out{X<sub>j</sub>}}{\eqn{X_j}{X_j}} with mean zero and with sample vectors
\ifelse{html}{\out{<b>x</b><sub>i</sub> = (x<sub>i1</sub>,...,x<sub>in</sub>)'}}{\eqn{\textbf{x}_i=(x_{i1},...,x_{in})'}{x_i=(x_i1,...,x_in)'}} and
\ifelse{html}{\out{<b>x</b><sub>j</sub> = (x<sub>j1</sub>,...,x<sub>jn</sub>)'}}{\eqn{\textbf{x}_j=(x_{j1},...,x_{jn})'}{x_j=(x_j1,...,x_jn)'}}
, their (sample) variances are equal (up-to a constant) to their cross-products, this is,
\ifelse{html}{\out{var(X<sub>i</sub>) = <b>x</b>'<sub>i</sub><b>x</b><sub>i</sub>}}{\eqn{var(X_i)=\textbf{x}_i'\textbf{x}_i}{var(X_i) = x'_i x_i}} and
\ifelse{html}{\out{var(X<sub>j</sub>) = <b>x</b>'<sub>j</sub><b>x</b><sub>j</sub>}}{\eqn{var(X_j)=\textbf{x}_j'\textbf{x}_j}{var(X_j) = x'_j x_j}}.
Likewise, the covariance is
\ifelse{html}{\out{cov(X<sub>i</sub>,X<sub>j</sub>) = <b>x</b>'<sub>i</sub><b>x</b><sub>j</sub>}}{\eqn{cov(X_i,X_j)=\textbf{x}_i'\textbf{x}_j}{cov(X_i,X_j) = x'_i x_j}}.

\ifelse{html}{\out{<b>Distance.</b>}}{\eqn{\textbf{Distance.}}{Distance.}}
The Euclidean distance
\ifelse{html}{\out{d(X<sub>i</sub>,X<sub>j</sub>)}}{\eqn{d(X_i,X_j)}{d(X_i,X_j)}}
between the variables expressed in terms of cross-products is

\ifelse{html}{\out{<p style='text-align:center'>d(X<sub>i</sub>,X<sub>j</sub>) = (<b>x</b>'<sub>i</sub><b>x</b><sub>i</sub> + <b>x</b>'<sub>j</sub><b>x</b><sub>j</sub> - 2<b>x</b>'<sub>i</sub><b>x</b><sub>j</sub>)<sup>1/2</sup></p>}}{\deqn{d(X_i,X_j) = \sqrt{\textbf{x}_i'\textbf{x}_i + \textbf{x}_j'\textbf{x}_j - 2\textbf{x}_i'\textbf{x}_j}}{d(X_i,X_j) = sqrt(x'_i x_i + x'_j x_j - 2x'_i x_j)}}

Therefore, the output distance matrix will contain as off-diagonal entries

\ifelse{html}{\out{<p style='text-align:center'>d(X<sub>i</sub>,X<sub>j</sub>) = (var(X<sub>i</sub>) + var(X<sub>j</sub>) - 2cov(X<sub>i</sub>,X<sub>j</sub>))<sup>1/2</sup></p>}}{\deqn{d(X_i,X_j) = \sqrt{var(X_i)+var(X_j)- 2cov(X_i,X_j)}}{d(X_i,X_j) = sqrt(var(X_i) + var(X_j) - 2cov(X_i,X_j))}}

while in the diagonal, the distance between one variable with itself is
\ifelse{html}{\out{d(X<sub>i</sub>,X<sub>i</sub>) = 0}}{\eqn{d(X_i,X_i)=0}{d(X_i,X_i) = 0}}

\ifelse{html}{\out{<b>Correlation.</b>}}{\eqn{\textbf{Correlation.}}{Correlation.}}
The correlation between the variables is obtained from variances and covariances as

\ifelse{html}{\out{<p style='text-align:center'>cor(X<sub>i</sub>,X<sub>j</sub>) = cov(X<sub>i</sub>,X<sub>j</sub>)/(sd(X<sub>i</sub>)sd(X<sub>j</sub>))</p>}}{\deqn{cor(X_i,X_j) = cov(X_i,X_j)/(sd(X_i)sd(X_j))}{cor(X_i,X_j) = cov(X_i,X_j)/(sd(X_i)sd(X_j))}}

where \ifelse{html}{\out{sd(X<sub>i</sub>)=sqrt(var(X<sub>i</sub>))}}{\eqn{sd(X_i)=\sqrt{var(X_i)}}{sd(X_i) = sqrt(var(X_i))}}; while in the diagonal, the correlation between one variable with itself is
\ifelse{html}{\out{cor(X<sub>i</sub>,X<sub>i</sub>) = 1}}{\eqn{cor(X_i,X_i)=1}{cor(X_i,X_i) = 1}}

Variances are obtained from the diagonal values while covariances are obtained from the out-diagonal.

}
\examples{
  require(SFSI)
  data(wheatHTP)
  
  X = scale(M)[1:100,]/sqrt(ncol(M))
  A = tcrossprod(X)        # A 100x100 covariance matrix 
  
  # Covariance matrix to distance matrix
  D = cov2dist(A)
  # (it must equal (but faster) to:)
  D0 = as.matrix(dist(X))
  max(D-D0)
  
  # Covariance to a correlation matrix
  R = cov2cor2(A)
  # (it must equal (but faster) to:)
  R0 = cov2cor(A)
  max(R-R0)
  
  # Inplace calculation
  A[1:5,1:5]
  cov2dist(A, inplace=TRUE)
  A[1:5,1:5]  # notice that A was modified
  
}
