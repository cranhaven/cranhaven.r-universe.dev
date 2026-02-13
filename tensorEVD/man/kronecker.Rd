
\name{Kronecker product}
\alias{Kronecker}
\title{Kronecker product}
\usage{
Kronecker(A, B, rows = NULL, cols = NULL, a = 1,
          make.dimnames = FALSE, drop = TRUE,
          inplace = FALSE) 

}
\arguments{
\item{A}{(numeric) Left numeric matrix}

\item{B}{(numeric) Right numeric matrix}

\item{rows}{(integer) Index which rows of the Kronecker are to be returned. They must range from 1 to \code{nrow(A)*nrow(B)}. Default \code{rows=NULL} will return all the rows}

\item{cols}{(integer) Index which columns of the Kronecker are to be returned. They must range from 1 to \code{ncol(A)*ncol(B)}. Default \code{cols=NULL} return all the columns}

\item{a}{(numeric) A constant to multiply the resulting Kronecker product by}

\item{drop}{Either \code{TRUE} or \code{FALSE} to whether return a uni-dimensional vector when output is a matrix with either 1 row or 1 column as per the \code{rows} and \code{cols} arguments}

\item{make.dimnames}{\code{TRUE} or \code{FALSE} to whether add \code{rownames} and \code{colnames} attributes to the output}

\item{inplace}{\code{TRUE} or \code{FALSE} to whether operate directly on one input matrix (\code{A} or \code{B}) when the other one is a scalar. This is possible only when \code{rows=NULL} and \code{cols=NULL}. When \code{TRUE} the output will be overwritten on the same address occupied by the input that is not scalar. Default \code{inplace=FALSE}}
}
\value{
Returns the Kronecker product matrix. It can be a sub-matrix of it as per the \code{rows} and \code{cols} arguments.
}
\description{
Computes the direct Kronecker product between two matrices
}
\details{
For any two matrices
\ifelse{html}{\out{<b>A</b>=\{a<sub>ij</sub>\}}}{\eqn{\textbf{A}=\{a_{ij}\}}{A=\{a_ij\}}} of dimensions 
\ifelse{html}{\out{<i>m</i> &times; <i>n</i>}}{\eqn{m\times n}{m x n}} and
\ifelse{html}{\out{<b>B</b>=\{b<sub>ij</sub>\}}}{\eqn{\textbf{B}=\{b_{ij}\}}{B=\{b_ij\}}} of dimensions
\ifelse{html}{\out{<i>p</i> &times; <i>q</i>}}{\eqn{p\times q}{p x q}}, 
the direct Kronecker product between them is a matrix defined as the block matrix

\ifelse{html}{\out{<p style='text-align:center'><b>A</b>&otimes;<b>B</b> = \{a<sub>ij</sub><b>B</b>\}</p>}}{\deqn{\textbf{A}\otimes\textbf{B}=\{a_{ij}\textbf{B}\}}{Kronecker(A,B) = \{a_ij B\}}}

which is of dimensions \ifelse{html}{\out{<i>mp</i> &times; <i>nq</i>}}{\eqn{mp\times nq}{mp x nq}}.

A sub-matrix formed by selecting specific rows and columns from the Kronecker can be obtained by pre- and post- multiplication with incidence matrices

\ifelse{html}{\out{<p style='text-align:center'><b>R</b> (<b>A</b>&otimes;<b>B</b>) <b>C'</b></p>}}{\deqn{\textbf{R}(\textbf{A}\otimes\textbf{B})\textbf{C}'}{R (Kronecker(A,B)) C'}}

where
\ifelse{html}{\out{<b>R</b>}}{\eqn{\textbf{R}}{R}} is an incidence matrix mapping from rows of the resulting sub-matrix to rows of the Kronecker product, and 
\ifelse{html}{\out{<b>C</b>}}{\eqn{\textbf{C}}{C}} is an incidence matrix mapping from columns of the resulting sub-matrix to columns of the Kronecker product.
This sub-matrix of the Kronecker can be obtained by matrix indexing as

\ifelse{html}{\out{<p style='text-align:center;font-family:courier'>Kronecker(A,B)[rows,cols]</p>}}{\deqn{\code{Kronecker(A,B)[rows,cols]}}{Kronecker(A,B)[rows,cols]}}

where \code{rows} and \code{cols} are integer vectors whose entries are, respectively, the row and column number of the Kronecker that are mapped at each row of \ifelse{html}{\out{<b>R</b>}}{\eqn{\textbf{R}}{R}} and \ifelse{html}{\out{<b>C</b>}}{\eqn{\textbf{C}}{C}}.

The function computes this sub-matrix of the Kronecker product directly from \ifelse{html}{\out{<b>A</b>}}{\eqn{\textbf{A}}{A}} and \ifelse{html}{\out{<b>B</b>}}{\eqn{\textbf{B}}{B}} without forming the whole Kronecker product. This is very useful if a relatively small number of row/columns are to be selected. 

}
\examples{
  require(tensorEVD)
  
  # (a) Kronecker product of 2 vectors
  A = rnorm(3)
  B = rnorm(2) 
  (K1 = Kronecker(A, B))
  # it must equal when using from the R-base package:
  (K2 = kronecker(A, B))
  
  # (b) Kronecker product of 2 matrices
  A = matrix(rnorm(12), ncol=3)
  B = matrix(rnorm(4), ncol=2)
  K1 = Kronecker(A, B)
  # (it must equal (but faster) to:)
  K2 = kronecker(A, B)
  all.equal(K1,K2)
  \donttest{
  # (c) Subsetting rows/columns from the Kronecker
  A = matrix(rnorm(100*150), ncol=150)
  B = matrix(rnorm(100*120), ncol=120)
  rows = c(1,3,5,7)
  cols = c(10,20,30,50)
  K1 = Kronecker(A, B, rows=rows, cols=cols)
  # (it must equal (but faster) to:)
  K2 = Kronecker(A, B)[rows,cols]
  all.equal(K1,K2)
  
  # (d) Inplace calculation
  # overwrite the output at the same address as the input:
  K1 = A[]                     # copy of A to be used as input
  add  = pryr::address(K1)     # address of K on entry
  K1 = Kronecker(K1, B=0.5)
  pryr::address(K1) == add     # on exit, K was moved to a different address
  
  K2 = A[]   
  add  = pryr::address(K2)
  K2 = Kronecker(K2, B=0.5, inplace=TRUE)
  pryr::address(K2) == add     # on exit, K remains at the same address
  all.equal(K1,K2)
  }
}
