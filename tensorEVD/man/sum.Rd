
\name{Weighted sum}
\alias{Sum}
\title{Weighted sum}
\usage{
Sum(a = 1, A, b = 1, B, IDrowA, IDrowB,
    IDcolA = NULL, IDcolB = NULL,
    make.dimnames = FALSE, drop = TRUE, 
    inplace = FALSE)

}
\arguments{
\item{a}{(numeric) A constant to multiply the first matrix by}

\item{A}{(numeric) Numeric matrix}

\item{b}{(numeric) A constant to multiply the second matrix by}

\item{B}{(numeric) Numeric matrix}

\item{IDrowA}{(integer/character) Vector of length \ifelse{html}{\out{<i>m</i>}}{\eqn{m}{m}} with either indices or row names mapping from rows of \code{A} into the resulting Hadamard product. If 'missing', it is assumed to be equal to \code{1,...,nrow(A)}}

\item{IDrowB}{(integer/character) Vector of length \ifelse{html}{\out{<i>m</i>}}{\eqn{m}{m}} with either indices or row names mapping from rows of \code{B} into the resulting Hadamard product. If 'missing', it is assumed to be equal to \code{1,...,nrow(B)}}

\item{IDcolA}{(integer/character) (Optional) Similar to \code{IDrowA}, vector of length \ifelse{html}{\out{<i>n</i>}}{\eqn{n}{n}} for columns. If \code{NULL}, it is assumed to be equal to \code{IDrowA} if 
\ifelse{html}{\out{<i>m=n</i>}}{\eqn{m=n}{m=n}}}

\item{IDcolB}{(integer/character) (Optional) Similar to \code{IDrowB}, vector of length \ifelse{html}{\out{<i>n</i>}}{\eqn{n}{n}} for columns. If \code{NULL}, it is assumed to be equal to \code{IDrowB} if
\ifelse{html}{\out{<i>m=n</i>}}{\eqn{m=n}{m=n}}}

\item{drop}{Either \code{TRUE} or \code{FALSE} to whether return a uni-dimensional vector when output is a matrix with either 1 row or 1 column as per the \code{rows} and \code{cols} arguments}

\item{make.dimnames}{\code{TRUE} or \code{FALSE} to whether add \code{rownames} and \code{colnames} attributes to the output}

\item{inplace}{\code{TRUE} or \code{FALSE} to whether operate directly on one input matrix (\code{A} or \code{B}) when this is used as is (i.e., is not indexed; therefore, needs to be of appropiate dimensions) in the Hadamard. When \code{TRUE} the output will be overwritten on the same address occupied by the non-indexed matrix. Default \code{inplace=FALSE}}
}
\value{
Returns a matrix containing the Hadamard product.
}
\description{
Computes the Hadamard product between two matrices
}
\details{
Computes the \ifelse{html}{\out{<i>m</i> &times; <i>n</i>}}{\eqn{m\times n}{m x n}} weighted sum matrix between matrices 
\ifelse{html}{\out{<b>A</b>}}{\eqn{\textbf{A}}{A}} and \ifelse{html}{\out{<b>B</b>}}{\eqn{\textbf{B}}{B}},

\ifelse{html}{\out{<p style='text-align:center'><i>a</i>(<b>R</b><sub>1</sub> <b>A</b> <b>C'</b><sub>1</sub>) + <i>b</i>(<b>R</b><sub>2</sub> <b>B</b> <b>C'</b><sub>2</sub>)</p>}}{\deqn{a(\textbf{R}_{1}\textbf{A}\textbf{C}'_{1}) + b(\textbf{R}_{2}\textbf{B}\textbf{C}'_{2})}{a(R1 A C1') + b(R2 B C2')}}

where
\ifelse{html}{\out{<b>R</b><sub>1</sub>}}{\eqn{\textbf{R}_{1}}{R1}} and
\ifelse{html}{\out{<b>R</b><sub>2</sub>}}{\eqn{\textbf{R}_{2}}{R2}} are incidence matrices mapping from rows of the resulting sum to rows of \ifelse{html}{\out{<b>A</b>}}{\eqn{\textbf{A}}{A}} and \ifelse{html}{\out{<b>B</b>}}{\eqn{\textbf{B}}{B}}, respectively; and 
\ifelse{html}{\out{<b>C</b><sub>1</sub>}}{\eqn{\textbf{C}_{1}}{C1}} and
\ifelse{html}{\out{<b>C</b><sub>2</sub>}}{\eqn{\textbf{C}_{2}}{C2}} are incidence matrices mapping from columns of the resulting sum to columns of \ifelse{html}{\out{<b>A</b>}}{\eqn{\textbf{A}}{A}} and \ifelse{html}{\out{<b>B</b>}}{\eqn{\textbf{B}}{B}}, respectively.

Matrix \ifelse{html}{\out{<b>R</b><sub>1</sub> <b>A</b> <b>C'</b><sub>1</sub>}}{\eqn{\textbf{R}_{1}\textbf{A}\textbf{C}'_{1}}{R1 A C1'}}
can be obtained by matrix indexing as \code{A[IDrowA,IDcolA]}, where \code{IDrowA} and \code{IDcolA} are integer vectors whose entries are, respectively, the row and column number of 
\ifelse{html}{\out{<b>A</b>}}{\eqn{\textbf{A}}{A}} that are mapped at each row of 
\ifelse{html}{\out{<b>R</b><sub>1</sub>}}{\eqn{\textbf{R}_{1}}{R1}} and 
\ifelse{html}{\out{<b>C</b><sub>1</sub>}}{\eqn{\textbf{C}_{1}}{C1}}, respectively.
Likewise, matrix 
\ifelse{html}{\out{<b>R</b><sub>2</sub> <b>B</b> <b>C'</b><sub>2</sub>}}{\eqn{\textbf{R}_{2}\textbf{B}\textbf{C}'_{2}}{R2 B C2'}}
can be obtained as \code{B[IDrowB,IDcolB]}, where \code{IDrowB} and \code{IDcolB} are integer vectors whose entries are, respectively, the row and column number of 
\ifelse{html}{\out{<b>B</b>}}{\eqn{\textbf{B}}{B}} that are mapped at each row of 
\ifelse{html}{\out{<b>R</b><sub>2</sub>}}{\eqn{\textbf{R}_{2}}{R2}} and 
\ifelse{html}{\out{<b>C</b><sub>2</sub>}}{\eqn{\textbf{C}_{2}}{C2}}, respectively. Therefore, the weighted sum can be obtained directly as

\ifelse{html}{\out{<p style='text-align:center;font-family:courier'>a*A[IDrowA,IDcolA] + b*B[IDrowB,IDcolB]</p>}}{\deqn{\code{a*A[IDrowA,IDcolA] + b*B[IDrowB,IDcolB]}}{a*A[IDrowA,IDcolA] + b*B[IDrowB,IDcolB]}}

The function computes the Hadamard product directly from \ifelse{html}{\out{<b>A</b>}}{\eqn{\textbf{A}}{A}} and \ifelse{html}{\out{<b>B</b>}}{\eqn{\textbf{B}}{B}} without forming \ifelse{html}{\out{<b>R</b><sub>1</sub> <b>A</b> <b>C'</b><sub>1</sub>}}{\eqn{\textbf{R}_{1}\textbf{A}\textbf{C}'_{1}}{R1 A C1'}} or 
\ifelse{html}{\out{<b>R</b><sub>2</sub> <b>B</b> <b>C'</b><sub>2</sub>}}{\eqn{\textbf{R}_{2}\textbf{B}\textbf{C}'_{2}}{R2 B C2'}}
matrices. The result can be multiplied by a constant \ifelse{html}{\out{<i>a</i>}}{\eqn{a}{a}}.

}
\examples{
  require(tensorEVD)
  
  # Generate rectangular matrices A (nrowA x ncolA) and B (nrowB x ncolB)
  nA = c(10,15)
  nB = c(12,8)
  A = matrix(rnorm(nA[1]*nA[2]), nrow=nA[1])
  B = matrix(rnorm(nB[1]*nB[2]), nrow=nB[1])
  
  # Define IDs for a Hadamard of size n1 x n2
  n = c(1000,500)
  IDrowA = sample(nA[1], n[1], replace=TRUE)
  IDrowB = sample(nB[1], n[1], replace=TRUE)
  IDcolA = sample(nA[2], n[2], replace=TRUE)
  IDcolB = sample(nB[2], n[2], replace=TRUE)
  
  a = rnorm(1)
  b = rnorm(1)
  
  K1 = Sum(a, A, b, B, IDrowA, IDrowB, IDcolA, IDcolB)
  
  # (it must equal to:)
  K2 = a*A[IDrowA,IDcolA] + b*B[IDrowB,IDcolB]
  all.equal(K1,K2)
}
