\name{trim.matrix}

\alias{trim.matrix}

\title{Given an ill-conditioned square matrix, deletes rows/columns
until a well-conditioned submatrix is obtained.}

\description{
This function seeks to deal with ill-conditioned matrices, for which the search algorithms of optimal k-variable subsets could encounter numerical problems. Given a square matrix \code{mat} which is assumed positive semi-definite, the function checks whether it has reciprocal of the 2-norm condition number (i.e., the ratio of the smallest to the largest eigenvalue) smaller than \code{tolval}. If not, the matrix is considered well-conditioned and remains unchanged. If the ratio of the smallest to largest eigenvalue is smaller than \code{tolval}, an iterative process is begun, which deletes rows/columns (using Jolliffe's method for subset selections described on pg. 138 of the Reference below) until a principal submatrix with reciprocal of the condition number larger than \code{tolval} is obtained.
}

\usage{trim.matrix(mat,tolval=10*.Machine$double.eps)}

\arguments{

  \item{mat}{a symmetric matrix, assumed positive semi-definite.}

  \item{tolval}{the tolerance value for the reciprocal condition
number of matrix \emph{mat}.}   
}

\details{
For the given matrix \code{mat}, eigenvalues are computed. If the
ratio of the smallest to the largest eigenvalue is less than \code{tolval}, matrix \code{mat} remains unchanged and the function stops. Otherwise, an iterative process is begun, in which the eigenvector associated with the smallest eigenvalue is considered and its largest (in absolute value) element is identified. The corresponding row/column are deleted from matrix \code{mat} and the eigendecomposition of the resulting submatrix is computed. This iterative process stops when the ratio of the smallest to largest eigenvalue is not smaller than \code{tolval}.

The function checks whether the input matrix is square, but not whether it is positive semi-definite. This \code{trim.matrix} function can be used to delete rows/columns of square matrices, until only non-negative eigenvalues appear. 
}

\value{Output is a list with four items:

    \item{trimmedmat}{is a principal submatrix of the original matrix,
with the ratio of its smallest to largest eigenvalues no smaller than
\code{tolval}. This matrix can be used as input for the search
algorithms in this package.}

    \item{numbers.discarded}{is a list of the integer numbers of the original variables that were discarded.}

    \item{names.discarded}{is a list of the original column numbers of the variables that were discarded.}

    \item{size}{is the size of the output matrix.}
}

\note{
When the \code{\link{trim.matrix}} function is used to produce a well-conditioned matrix for use with the \code{\link{anneal}}, \code{\link{genetic}}, \code{\link{improve}} or \code{\link{eleaps}} functions, care must be taken in interpreting the output of those functions. In those search functions, the selected variable subsets are specified by variable numbers, and those variable numbers indicate the position of the variables in the input matrix. Hence, if a trimmed matrix is supplied to functions \code{anneal}, \code{genetic}, \code{improve} or \code{eleaps}, variable numbers refer \emph{to the trimmed matrix}.
}

\references{
Jolliffe, I.T. (2002) \emph{Principal Component Analysis, second edition}, Springer Series in Statistics.
}

\examples{

# a trivial example, for illustration of use: creating an extra column,
# as the sum of columns in the "iris" data, and then using the function
# trim.matrix to exclude it from the data's correlation matrix

data(iris)
lindepir<-cbind(apply(iris[,-5],1,sum),iris[,-5])
colnames(lindepir)[1]<-"Sum"
cor(lindepir)

##                    Sum Sepal.Length Sepal.Width Petal.Length Petal.Width
##Sum           1.0000000    0.9409143  -0.2230928    0.9713793   0.9538850
##Sepal.Length  0.9409143    1.0000000  -0.1175698    0.8717538   0.8179411
##Sepal.Width  -0.2230928   -0.1175698   1.0000000   -0.4284401  -0.3661259
##Petal.Length  0.9713793    0.8717538  -0.4284401    1.0000000   0.9628654
##Petal.Width   0.9538850    0.8179411  -0.3661259    0.9628654   1.0000000

trim.matrix(cor(lindepir))

##$trimmedmat
##             Sepal.Length Sepal.Width Petal.Length Petal.Width
##Sepal.Length    1.0000000  -0.1175698    0.8717538   0.8179411
##Sepal.Width    -0.1175698   1.0000000   -0.4284401  -0.3661259
##Petal.Length    0.8717538  -0.4284401    1.0000000   0.9628654
##Petal.Width     0.8179411  -0.3661259    0.9628654   1.0000000
##
##$numbers.discarded
##[1] 1
##
##$names.discarded
##[1] "Sum"
##
##$size
##[1] 4

data(swiss)
lindepsw<-cbind(apply(swiss,1,sum),swiss)
colnames(lindepsw)[1]<-"Sum"
trim.matrix(cor(lindepsw))

##$lowrankmat
##                  Fertility Agriculture examination   Education   Catholic
##Fertility         1.0000000  0.35307918  -0.6458827 -0.66378886  0.4636847
##Agriculture       0.3530792  1.00000000  -0.6865422 -0.63952252  0.4010951
##Examination      -0.6458827 -0.68654221   1.0000000  0.69841530 -0.5727418
##Education        -0.6637889 -0.63952252   0.6984153  1.00000000 -0.1538589
##Catholic          0.4636847  0.40109505  -0.5727418 -0.15385892  1.0000000
##Infant.Mortality  0.4165560 -0.06085861  -0.1140216 -0.09932185  0.1754959
##                 Infant.Mortality
##Fertility              0.41655603
##Agriculture           -0.06085861
##Examination           -0.11402160
##Education             -0.09932185
##Catholic               0.17549591
##Infant.Mortality       1.00000000
##
##$numbers.discarded
##[1] 1
##
##$names.discarded
##[1] "Sum"
##
##$size
##[1] 6

}

\keyword{manip}
