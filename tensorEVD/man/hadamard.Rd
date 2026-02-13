
\name{Hadamard product}
\alias{Hadamard}
\title{Hadamard product}
\usage{
Hadamard(A, B, IDrowA, IDrowB,
         IDcolA = NULL, IDcolB = NULL,
         a = 1, make.dimnames = FALSE,
         drop = TRUE, inplace = FALSE)

}
\arguments{
\item{A}{(numeric) Numeric matrix}

\item{B}{(numeric) Numeric matrix}

\item{IDrowA}{(integer/character) Vector of length \ifelse{html}{\out{<i>m</i>}}{\eqn{m}{m}} with either indices or row names mapping from rows of \code{A} into the resulting Hadamard product. If 'missing', it is assumed to be equal to \code{1,...,nrow(A)}}

\item{IDrowB}{(integer/character) Vector of length \ifelse{html}{\out{<i>m</i>}}{\eqn{m}{m}} with either indices or row names mapping from rows of \code{B} into the resulting Hadamard product. If 'missing', it is assumed to be equal to \code{1,...,nrow(B)}}

\item{IDcolA}{(integer/character) (Optional) Similar to \code{IDrowA}, vector of length \ifelse{html}{\out{<i>n</i>}}{\eqn{n}{n}} for columns. If \code{NULL}, it is assumed to be equal to \code{IDrowA} if 
\ifelse{html}{\out{<i>m=n</i>}}{\eqn{m=n}{m=n}}}

\item{IDcolB}{(integer/character) (Optional) Similar to \code{IDrowB}, vector of length \ifelse{html}{\out{<i>n</i>}}{\eqn{n}{n}} for columns. If \code{NULL}, it is assumed to be equal to \code{IDrowB} if
\ifelse{html}{\out{<i>m=n</i>}}{\eqn{m=n}{m=n}}}

\item{a}{(numeric) A constant to multiply the resulting Hadamard product by}

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
Computes the \ifelse{html}{\out{<i>m</i> &times; <i>n</i>}}{\eqn{m\times n}{m x n}} Hadamard product (aka element-wise or entry-wise product) matrix between matrices 
\ifelse{html}{\out{<b>A</b>}}{\eqn{\textbf{A}}{A}} and \ifelse{html}{\out{<b>B</b>}}{\eqn{\textbf{B}}{B}},

\ifelse{html}{\out{<p style='text-align:center'>(<b>R</b><sub>1</sub> <b>A</b> <b>C'</b><sub>1</sub>) &odot; (<b>R</b><sub>2</sub> <b>B</b> <b>C'</b><sub>2</sub>)</p>}}{\deqn{(\textbf{R}_{1}\textbf{A}\textbf{C}'_{1})\odot(\textbf{R}_{2}\textbf{B}\textbf{C}'_{2})}{Hadamard(R1 A C1', R2 B C2')}}

where
\ifelse{html}{\out{<b>R</b><sub>1</sub>}}{\eqn{\textbf{R}_{1}}{R1}} and
\ifelse{html}{\out{<b>R</b><sub>2</sub>}}{\eqn{\textbf{R}_{2}}{R2}} are incidence matrices mapping from rows of the resulting Hadamard to rows of \ifelse{html}{\out{<b>A</b>}}{\eqn{\textbf{A}}{A}} and \ifelse{html}{\out{<b>B</b>}}{\eqn{\textbf{B}}{B}}, respectively; and 
\ifelse{html}{\out{<b>C</b><sub>1</sub>}}{\eqn{\textbf{C}_{1}}{C1}} and
\ifelse{html}{\out{<b>C</b><sub>2</sub>}}{\eqn{\textbf{C}_{2}}{C2}} are incidence matrices mapping from columns of the resulting Hadamard to columns of \ifelse{html}{\out{<b>A</b>}}{\eqn{\textbf{A}}{A}} and \ifelse{html}{\out{<b>B</b>}}{\eqn{\textbf{B}}{B}}, respectively.

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
\ifelse{html}{\out{<b>C</b><sub>2</sub>}}{\eqn{\textbf{C}_{2}}{C2}}, respectively. Therefore, the Hadamard product can be obtained directly as

\ifelse{html}{\out{<p style='text-align:center;font-family:courier'>A[IDrowA,IDcolA]*B[IDrowB,IDcolB]</p>}}{\deqn{\code{A[IDrowA,IDcolA]*B[IDrowB,IDcolB]}}{A[IDrowA,IDcolA]*B[IDrowB,IDcolB]}}

The function computes the Hadamard product directly from \ifelse{html}{\out{<b>A</b>}}{\eqn{\textbf{A}}{A}} and \ifelse{html}{\out{<b>B</b>}}{\eqn{\textbf{B}}{B}} without forming \ifelse{html}{\out{<b>R</b><sub>1</sub> <b>A</b> <b>C'</b><sub>1</sub>}}{\eqn{\textbf{R}_{1}\textbf{A}\textbf{C}'_{1}}{R1 A C1'}} or 
\ifelse{html}{\out{<b>R</b><sub>2</sub> <b>B</b> <b>C'</b><sub>2</sub>}}{\eqn{\textbf{R}_{2}\textbf{B}\textbf{C}'_{2}}{R2 B C2'}}
matrices. The result can be multiplied by a constant \ifelse{html}{\out{<i>a</i>}}{\eqn{a}{a}}.

}
\examples{
  require(tensorEVD)
  
  # (a) Example 1. Indexing using row/column names
  # Generate rectangular matrices A (nrowA x ncolA) and B (nrowB x ncolB)
  nA = c(10,15)
  nB = c(12,8)
  A = matrix(rnorm(nA[1]*nA[2]), nrow=nA[1])
  B = matrix(rnorm(nB[1]*nB[2]), nrow=nB[1])
  dimnames(A) = list(paste0("row",seq(nA[1])), paste0("col",seq(nA[2])))
  dimnames(B) = list(paste0("row",seq(nB[1])), paste0("col",seq(nB[2])))
  
  # Define IDs for a Hadamard of size n1 x n2
  n = c(1000,500)
  IDrowA = sample(rownames(A), n[1], replace=TRUE)
  IDrowB = sample(rownames(B), n[1], replace=TRUE)
  IDcolA = sample(colnames(A), n[2], replace=TRUE)
  IDcolB = sample(colnames(B), n[2], replace=TRUE)
  
  K1 = Hadamard(A, B, IDrowA, IDrowB, IDcolA, IDcolB, make.dimnames=TRUE)
  
  # (it must equal to:)
  K2 = A[IDrowA,IDcolA]*B[IDrowB,IDcolB]
  dimnames(K2) = list(paste0(IDrowA,":",IDrowB), paste0(IDcolA,":",IDcolB))
  all.equal(K1,K2)
  
  # (b) Example 2. Indexing using integers
  # Generate squared symmetric matrices A and B 
  nA = 20
  nB = 15
  A = tcrossprod(matrix(rnorm(nA*nA), nrow=nA))
  B = tcrossprod(matrix(rnorm(nB*nB), nrow=nB))
  
  # Define IDs for a Hadamard of size n x n
  n = 1000
  IDA = sample(seq(nA), n, replace=TRUE)
  IDB = sample(seq(nB), n, replace=TRUE)
  
  K1 = Hadamard(A, B, IDA, IDB)
  
  # (it must equal to:)
  K2 = A[IDA,IDA]*B[IDB,IDB]
  all.equal(K1,K2)
  
  # (c) Inplace calculation
  # overwrite the output at the same address as the input:
  IDB = sample(seq(nB), nA, replace=TRUE)
  
  K1 = A[]                     # copy of A to be used as input
  add  = pryr::address(K1)     # address of K on entry
  K1 = Hadamard(K1, B, IDrowB=IDB)
  pryr::address(K1) == add     # on exit, K was moved to a different address
  
  K2 = A[]   
  add  = pryr::address(K2)
  K2 = Hadamard(K2, B, IDrowB=IDB, inplace=TRUE)
  pryr::address(K2) == add     # on exit, K remains at the same address
  all.equal(K1,K2)
}
