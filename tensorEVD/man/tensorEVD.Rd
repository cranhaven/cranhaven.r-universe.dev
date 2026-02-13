
\name{Tensor EVD}
\alias{tensorEVD}
\title{Tensor EVD}
\usage{
tensorEVD(K1, K2, ID1, ID2, alpha = 1.0,
          EVD1 = NULL, EVD2 = NULL,
          d.min = .Machine$double.eps, 
          make.dimnames = FALSE, verbose = FALSE)
}
\arguments{
\item{K1, K2}{(numeric) Covariance structure matrices}

\item{ID1}{(character/integer) Vector of length \ifelse{html}{\out{<i>n</i>}}{\eqn{n}{n}} with either names or indices mapping from rows/columns of \code{K1} into the resulting tensor product}

\item{ID2}{(character/integer) Vector of length \ifelse{html}{\out{<i>n</i>}}{\eqn{n}{n}} with either names or indices mapping from rows/columns of \code{K2} into the resulting tensor product}

\item{alpha}{(numeric) Proportion of variance of the tensor product to be explained by the tensor eigenvectors}

\item{EVD1}{(list) (Optional) Eigenvectors and eigenvalues of \code{K1} as produced by the \code{eigen} function}

\item{EVD2}{(list) (Optional) Eigenvectors and eigenvalues of \code{K2} as produced by the \code{eigen} function}

\item{d.min}{(numeric) Tensor eigenvalue threshold. Default is a numeric zero. Only eigenvectors with eigenvalue passing this threshold are returned}

\item{make.dimnames}{\code{TRUE} or \code{FALSE} to whether add \code{rownames} and \code{colnames} attributes to the output}

\item{verbose}{\code{TRUE} or \code{FALSE} to whether show progress}
}
\value{
Returns a list object that contains the elements:
\itemize{
  \item \code{values}: (vector) resulting tensor eigenvalues.
  \item \code{vectors}: (matrix) resulting tensor eigenvectors.
  \item \code{totalVar}: (numeric) total variance of the tensor matrix product.
}
}
\description{
Fast eigen value decomposition (EVD) of the Hadamard product of two matrices
}
\details{
Let the \ifelse{html}{\out{<i>n</i> &times; <i>n</i>}}{\eqn{n\times n}{n x n}} matrix \ifelse{html}{\out{<b>K</b>}}{\eqn{\textbf{K}}{K}} to be the Hadamard product (aka element-wise or entry-wise product) involving two smaller matrices 
\ifelse{html}{\out{<b>K</b><sub>1</sub>}}{\eqn{\textbf{K}_{1}}{K1}} and
\ifelse{html}{\out{<b>K</b><sub>2</sub>}}{\eqn{\textbf{K}_{2}}{K2}} of dimensions 
\ifelse{html}{\out{<i>n</i><sub>1</sub>}}{\eqn{n_{1}}{n1}} and \ifelse{html}{\out{<i>n</i><sub>2</sub>}}{\eqn{n_{2}}{n2}}, respectively,

\ifelse{html}{\out{<p style='text-align:center'><b>K</b> = (<b>Z</b><sub>1</sub> <b>K</b><sub>1</sub> <b>Z'</b><sub>1</sub>) &odot; (<b>Z</b><sub>2</sub> <b>K</b><sub>2</sub> <b>Z'</b><sub>2</sub>)</p>}}{\deqn{\textbf{K}=(\textbf{Z}_{1}\textbf{K}_{1}\textbf{Z}'_{1})\odot(\textbf{Z}_{2}\textbf{K}_{2}\textbf{Z}'_{2})}{K = Hadamard(Z1 K1 Z1', Z2 K2 Z2')}}

where
\ifelse{html}{\out{<b>Z</b><sub>1</sub>}}{\eqn{\textbf{Z}_{1}}{Z1}} and
\ifelse{html}{\out{<b>Z</b><sub>2</sub>}}{\eqn{\textbf{Z}_{2}}{Z2}} are incidence matrices mapping from rows (and columns) of the resulting Hadamard to rows (and columns) of \ifelse{html}{\out{<b>K</b><sub>1</sub>}}{\eqn{\textbf{K}_{1}}{K1}} and
\ifelse{html}{\out{<b>K</b><sub>2</sub>}}{\eqn{\textbf{K}_{2}}{K2}}, respectively. 

Let the eigenvalue decomposition (EVD) of \ifelse{html}{\out{<b>K</b><sub>1</sub>}}{\eqn{\textbf{K}_{1}}{K1}} and
\ifelse{html}{\out{<b>K</b><sub>2</sub>}}{\eqn{\textbf{K}_{2}}{K2}} to be
\ifelse{html}{\out{<b>K</b><sub>1</sub> = <b>V</b><sub>1</sub> <b>D</b><sub>1</sub> <b>V'</b><sub>1</sub>}}{\eqn{\textbf{K}_{1}=\textbf{V}_{1}\textbf{D}_{1}\textbf{V}'_{1}}{K1 = V1 D1 V1'}} and 
\ifelse{html}{\out{<b>K</b><sub>2</sub> = <b>V</b><sub>2</sub> <b>D</b><sub>2</sub> <b>V'</b><sub>2</sub>}}{\eqn{\textbf{K}_{2}=\textbf{V}_{2}\textbf{D}_{2}\textbf{V}'_{2}}{K2 = V2 D2 V2'}}. 
Using properties of the Hadamard and Kronecker products, an EVD of the Hadamard product
\ifelse{html}{\out{<b>K</b>}}{\eqn{\textbf{K}}{K}} can be approximated using the EVD of 
\ifelse{html}{\out{<b>K</b><sub>1</sub>}}{\eqn{\textbf{K}_{1}}{K1}} and 
\ifelse{html}{\out{<b>K</b><sub>2</sub>}}{\eqn{\textbf{K}_{2}}{K2}} as

\ifelse{html}{\out{<p style='text-align:center'><b>K = V D V'</b></p>}}{\deqn{\textbf{K}=\textbf{V}\textbf{D}\textbf{V}'}{K = V D V'}}

where \ifelse{html}{\out{<b>D</b> = <b>D</b><sub>1</sub>&otimes;<b>D</b><sub>2</sub>}}{\eqn{\textbf{D}=\textbf{D}_1\otimes\textbf{D}_2}{D = Kronecker(D1,D2)}} is a diagonal matrix containing
\ifelse{html}{\out{<i>N</i> = <i>n</i><sub>1</sub> &times; <i>n</i><sub>2</sub>}}{\eqn{N=n_{1}\times n_{2}}{N = n1 x n2}} tensor eigenvalues 
\ifelse{html}{\out{d<sub>1</sub> &ge; ... &ge; d<sub>N</sub> &ge; 0}}{\eqn{d_1\ge...\ge d_N \ge 0}{d_1 >= ...>= d_N >= 0}} and
\ifelse{html}{\out{<b>V</b> = (<b>Z</b><sub>1</sub>&Star;<b>Z</b><sub>2</sub>)(<b>V</b><sub>1</sub>&otimes;<b>V</b><sub>2</sub>) = [<b>v</b><sub>1</sub>,...,<b>v</b><sub>N</sub>]}}{\eqn{\textbf{V}=(\textbf{Z}_1\star\textbf{Z}_2)(\textbf{V}_1\otimes\textbf{V}_2)=[\textbf{v}_1,...,\textbf{v}_N]}{V = (Z1*Z2)(Kronecker(V1,V2)) = [v_1,...,v_N]}} is matrix containing \ifelse{html}{\out{<i>N</i>}}{\eqn{N}{N}} tensor eigenvectors
\ifelse{html}{\out{<b>v</b><sub>k</sub>}}{\eqn{\textbf{v}_{k}}{v_k}}; here the term 
\ifelse{html}{\out{<b>Z</b><sub>1</sub>&Star;<b>Z</b><sub>2</sub>}}{\eqn{\textbf{Z}_1\star\textbf{Z}_2}{Z1*Z2}} is the 
"face-splitting product" (aka "transposed Khatri–Rao product") of matrices 
\ifelse{html}{\out{<b>Z</b><sub>1</sub>}}{\eqn{\textbf{Z}_{1}}{Z1}} and
\ifelse{html}{\out{<b>Z</b><sub>2</sub>}}{\eqn{\textbf{Z}_{2}}{Z2}}.

Each tensor eigenvector \ifelse{html}{\out{<i>k</i>}}{\eqn{k}{k}} is derived separately as a Hadamard product using the corresponding 
\ifelse{html}{\out{<i>i(k)</i>}}{\eqn{i(k)}{i(k)}} and \ifelse{html}{\out{<i>j(k)</i>}}{\eqn{j(k)}{j(k)}} eigenvectors 
\ifelse{html}{\out{<b>v</b><sub>1i(k)</sub>}}{\eqn{\textbf{v}_{1i(k)}}{v_1i(k)}} and
\ifelse{html}{\out{<b>v</b><sub>2j(k)</sub>}}{\eqn{\textbf{v}_{2j(k)}}{v_2j(k)}} from 
\ifelse{html}{\out{<b>V</b><sub>1</sub>}}{\eqn{\textbf{V}_{1}}{V1}} and 
\ifelse{html}{\out{<b>V</b><sub>2</sub>}}{\eqn{\textbf{V}_{2}}{V2}}, respectively, this is

\ifelse{html}{\out{<p style='text-align:center'><b>v</b><sub>k</sub> = (<b>Z</b><sub>1</sub><b>v</b><sub>1i(k)</sub>)&odot;(<b>Z</b><sub>2</sub><b>v</b><sub>2j(k)</sub>)</p>}}{\deqn{\textbf{v}_{k}=(\textbf{Z}_{1}\textbf{v}_{1i(k)})\odot(\textbf{Z}_{2}\textbf{v}_{2j(k)})}{v_k = Hadamard(Z1 v_1i(k), Z2 v_2j(k))}}

The \code{tensorEVD} function derives each of these eigenvectors \ifelse{html}{\out{<b>v</b><sub>k</sub>}}{\eqn{\textbf{v}_{k}}{v_k}} by matrix indexing using integer vectors \code{ID1} and \code{ID2}. The entries of these vectors are the row (and column) number of 
\ifelse{html}{\out{<b>K</b><sub>1</sub>}}{\eqn{\textbf{K}_{1}}{K1}} and 
\ifelse{html}{\out{<b>K</b><sub>2</sub>}}{\eqn{\textbf{K}_{2}}{K2}} that are mapped at each row of \ifelse{html}{\out{<b>Z</b><sub>1</sub>}}{\eqn{\textbf{Z}_{1}}{Z1}} and
\ifelse{html}{\out{<b>Z</b><sub>2</sub>}}{\eqn{\textbf{Z}_{2}}{Z2}}, respectively.

}
\examples{
  require(tensorEVD)
  set.seed(195021)
  
  # Generate matrices K1 and K2 of dimensions n1 and n2
  n1 = 10; n2 = 15
  K1 = crossprod(matrix(rnorm(n1*(n1+10)), ncol=n1))
  K2 = crossprod(matrix(rnorm(n2*(n2+10)), ncol=n2))

  # (a) Example 1. Full design (Kronecker product)
  ID1 = rep(seq(n1), each=n2)
  ID2 = rep(seq(n2), times=n1)

  # Direct EVD of the Hadamard product
  K = K1[ID1,ID1]*K2[ID2,ID2]
  EVD0 = eigen(K)

  # Tensor EVD using K1 and K2
  EVD = tensorEVD(K1, K2, ID1, ID2)

  # Eigenvectors and eigenvalues are numerically equal
  all.equal(EVD0$values, EVD$values)
  all.equal(abs(EVD0$vectors), abs(EVD$vectors)) 

  # (b) If a proportion of variance explained is specified, 
  # only the eigenvectors needed to explain such proportion are derived
  alpha = 0.95
  EVD = tensorEVD(K1, K2, ID1, ID2, alpha=alpha)
  dim(EVD$vectors)

  # For the direct EVD
  varexp = cumsum(EVD0$values/sum(EVD0$values))
  index = 1:which.min(abs(varexp-alpha))
  dim(EVD0$vectors[,index])

  # (c) Example 2. Incomplete design (Hadamard product)
  # Eigenvectors and eigenvalues are no longer equivalent
  n = n1*n2   # Sample size n
  ID1 = sample(seq(n1), n, replace=TRUE) # Randomly sample of ID1
  ID2 = sample(seq(n2), n, replace=TRUE) # Randomly sample of ID2

  K = K1[ID1,ID1]*K2[ID2,ID2]
  EVD0 = eigen(K)
  EVD = tensorEVD(K1, K2, ID1, ID2)

  all.equal(EVD0$values, EVD$values)
  all.equal(abs(EVD0$vectors), abs(EVD$vectors)) 
  
  # However, the sum of the eigenvalues is equal to the trace(K)
  c(sum(EVD0$values), sum(EVD$values), sum(diag(K)))

  # And provide the same approximation for K
  K01 = EVD0$vectors\%*\%diag(EVD0$values)\%*\%t(EVD0$vectors)
  K02 = EVD$vectors\%*\%diag(EVD$values)\%*\%t(EVD$vectors)
  c(all.equal(K,K01), all.equal(K,K02))

  # When n is different from N=n1xn2, both methods provide different 
  # number or eigenvectors/eigenvalues. The eigen function provides 
  # a number of eigenvectors equal to the minimum between n and N
  # for the tensorEVD, this number is always N

  # (d) Sample size n being half of n1 x n2
  n = n1*n2/2
  ID1 = sample(seq(n1), n, replace=TRUE)
  ID2 = sample(seq(n2), n, replace=TRUE)

  K = K1[ID1,ID1]*K2[ID2,ID2]
  EVD0 = eigen(K)
  EVD = tensorEVD(K1, K2, ID1, ID2)

  c(eigen=sum(EVD0$values>1E-10), tensorEVD=sum(EVD$values>1E-10))

  # (e) Sample size n being twice n1 x n2
  n = n1*n2*2
  ID1 = sample(seq(n1), n, replace=TRUE)
  ID2 = sample(seq(n2), n, replace=TRUE)

  K = K1[ID1,ID1]*K2[ID2,ID2]
  EVD0 = eigen(K)
  EVD = tensorEVD(K1, K2, ID1, ID2)

  c(eigen=sum(EVD0$values>1E-10), tensorEVD=sum(EVD$values>1E-10))
}

