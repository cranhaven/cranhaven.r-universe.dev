\name{as.inputoutput}
\alias{as.inputoutput}

\title{Creating an Input-Output Object}
\description{Creates a list of class \code{InputOutput} for easier use of the other functions within \code{ioanalysis}.
  The Leontief inverse and Ghoshian inverse are calculated.
  A little work now to save a bunch of work in the future.
  For most functions in the package, this is a prerequisite.
  At a minimum, \code{Z}, \code{X}, and \code{RS_label} must be provided.
  See \code{Usage} for details.


  Caution: Inverting large matrices will take a long time. R does a computation roughly every 8e-10 second. The number of computations per matrix inversion is n^3 where n is the dimension of the square matrix. For n = 5000 it should take 100 seconds.}
%%%%%%%%%%%
%% Usage %%
%%%%%%%%%%%
\usage{
as.inputoutput(Z, RS_label, f, f_label, E, E_label, X, V, V_label, M, M_label, 
               fV, fV_label, P, P_label, A, B, L, G)
}
\arguments{
Let n = #sectors*#regions, l = # of labels, m = arbitrary length, r = #regions
  \item{Z}{Required. A nxn matrix of intermediate transactions between sectors and regions. It should be in units of currency, kg, etc.}
  \item{RS_label}{Required. A nx2 "column" matrix of the regions in column 1 and sector in column 2. Other functions use those locations to correctly identify elements in the matices. If there is only one region, it still needs to be specified in column 1.}
  \item{f}{Not required. A nxm matrix of final demand. Exports SHOULD NOT be included in this matrix. Instead, put exports in the \code{E} matrix. However, net exports should stay. }
  \item{f_label}{Not required. A 2xn "row" matrix of the region and accounts to help identify the elements of \code{f}. The first row should be regions and the second should be regional account labels.}
  \item{E}{Not required. A nxr matrix of exports. Multiple columns per region is accepted.}
  \item{E_label}{Not required. A 2xn "row" matrix of the region and type of export to help identify the elements of \code{E}. }
  \item{X}{Required. A 1xn vector of total production for each sector across all regions. \code{RS_label} identifies the objects}
  \item{V}{Not required. A nxm matrix of value added. Imports SHOULD NOT be included in this matrix. Instead, put exports in the \code{M} matrix.}
  \item{V_label}{Not required. A mx1 "column" matrix where the only column is the type of value added. This helps identify the rows of value added. \code{RS_label} identifies the columns.}
  \item{M}{Not required. A mxn matrix of import. Multiple types of imports is accepted.}
  \item{M_label}{Not required. A mx1 "column" matrix to identify the rows of imports. \code{RS_label} identifies the columns.}
\item{fV}{Not Required. The matrix of final demand's value added}
\item{fV_label}{Not Required. Column matrix to identify the row elements of \code{fV}}
\item{P}{Not Required. The matrix of intermediate transactions in physical units}
\item{P_label}{Not Required. A nx2 matrix to identify the regions and sectors of \code{P}}
  \item{A}{Not required. A nxn matrix of technical input coefficients. If not provided, \code{A} is calculated for you.}
  \item{B}{Not required. A nxn matrix of technical output coefficients. If not provided, \code{B} is calculated for you.}
  \item{L}{Not required. The Leontief inverse. If not provided, \code{L} is calculated for you.}
  \item{G}{Not required. The Ghoshian inverse. If not provided, \code{G} is calculated for you.}
}
\details{
If the \code{A} matrix is not provided, it is calculated as follows: \deqn{a_{ij} = z_{ij}/x_j}
If the \code{B} matrix is not provided, it is calculated as follows: \deqn{b_{ij} = z_{ij}/x_i}
If the \code{L} matrix is not provided, it is calculated as follows: \deqn{L = (I-A)^{-1}}
If the \code{G} matrix is not provided, it is calculated as follows: \deqn{G = (I-B)^{-1}}
}
%%%%%%%%%%%%
%% Output %%
%%%%%%%%%%%%
\value{
\code{as.inputouput} retuns an object of \code{\link{class} "InputOutput"}. Once created, it is sufficient to provide this object in all further functions in the \code{ioanalysis} package.
\item{Z}{Intermediate Transactions Matrix}
\item{RS_label}{Column matrix of labels for the region and sectors used to identify elements in \code{A, Z, X, L, ...}}
\item{f}{Final Demand}
\item{f_label}{Row matrix of labels for accounts for \code{f}}
\item{E}{Exports}
\item{E_label}{Row matrix of labels for exports by sector and region for \code{E}}
\item{X}{Total Production}
\item{V}{Value added}
\item{V_label}{Column matrix of labels for types of value added for \code{V}}
\item{M}{Imports}
\item{M_label}{Colum matrix of labels for type of imports for \code{M}}
\item{fV}{The matrix of final demand's value added}
\item{fV_label}{Column matrix to identify the row elements of \code{fV}}
\item{A}{Technical Input Coefficients}
\item{B}{Technical Input Coefficients}
\item{L}{Leontief inverse}
\item{G}{Ghoshian inverse}
}

\references{
Nazara, Suahasil & Guo, Dong & Hewings, Geoffrey J.D., & Dridi, Chokri, 2003. \emph{PyIO. Input-Output Analysis with Python}. REAL Discussion Paper 03-t-23. University of Illinois at Urbana-Champaign. (\url{http://www.real.illinois.edu/d-paper/03/03-t-23.pdf})

}
\author{
John J. P. wade
}

\note{
Currently, there is no use for an intermediate transaction matrix in physical units (P). If you wish to carry this with the matrix then you can create the \code{InputOutput} object and add to it by using \code{io$P <- P}.
}

\examples{
# In toy,FullIOTable it is a full matrix of characters: a pseudo worst case scenario
data(toy.FullIOTable)
Z <- matrix(as.numeric(toy.FullIOTable[3:12, 3:12]), ncol = 10)
f <- matrix(as.numeric(toy.FullIOTable[3:12, c(13:15, 17:19)]), nrow = dim(Z)[1])
E <- matrix(as.numeric(toy.FullIOTable[3:12, c(16, 20)]), nrow = 10)
X <- matrix(as.numeric(toy.FullIOTable[3:12, 21]), ncol = 1)
V <- matrix(as.numeric(toy.FullIOTable[13:15, 3:12]), ncol = 10)
M <- as.numeric(toy.FullIOTable[16, 3:12])
fV <- matrix(as.numeric(toy.FullIOTable[15:16, c(13:15,17:19)]), nrow = 2)

# Note toy.FullIOTable is a matrix of characters: non-numeric
toy.IO <- as.inputoutput(Z = Z, RS_label = toy.FullIOTable[3:12, 1:2],
                         f = f, f_label = toy.FullIOTable[1:2, c(13:15, 17:19)],
                         E = E, E_label = toy.FullIOTable[1:2, c(16, 20)],
                         X = X,
                         V = V, V_label = toy.FullIOTable[13:15, 2],
                         M = M, M_label = toy.FullIOTable[16,2],
                         fV = fV, fV_label = toy.FullIOTable[15:16, 2])

# Notice we do not need to supply the matrix of technical coefficients (A)
}
