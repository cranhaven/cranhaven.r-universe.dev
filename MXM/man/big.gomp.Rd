\name{Generic orthogonal matching pursuit(gOMP) for big data}
\alias{big.gomp}
\alias{big.gomp.path}
\title{
Generic orthogonal matching pursuit(gOMP) for big data
}

\description{
Generic orthogonal matching pursuit(gOMP) for big data.
}

\usage{
big.gomp(target = NULL, dataset, tol = qchisq(0.95, 1) + log(dim(x)[1]), 
test = "testIndFisher", method = "ar2")

big.gomp.path(target = NULL, dataset, tol = c(8, 9, 10), 
test = "testIndFisher", method = "ar2")
}

\arguments{
\item{target}{
The target (response) variable. If NULL, then it must inside the dataset. You might have the target variable though outside the 
big data file. This is like in the case of the regular gomp, a Surv object, a factor or a continuous numerical vector for all 
other cases. The default value is NULL.
}
\item{dataset}{
The big.matrix oject. If target is NULL, the first column must be the target, the response variable and all the others are the 
predictor variables. In the case of survival data, the first two columns are used to form the response variable.
}
\item{tol}{
The tolerance value to terminate the algorithm. This is the change in the criterion value between two successive steps. 
The default value is the 95\% quantile of the \eqn{\chi^2} distribution with 1 degree of freedom. For test = "testIndFisher" 
the BIC is already calculated. 

In the case of "big.gomp.path" this is a vector of values. For each tolerance value the result of the gOMP is returned. 
It returns the whole path of solutions. 
}
\item{test}{
This denotes the parametric model to be used each time. It depends upon the nature of the target variable. 
The possible values are "testIndFisher" (or "testIndReg" for the same purpose), "testIndLogistic", 
"testIndPois", "testIndQPois", "testIndQbinom", "testIndNormLog", "testIndNB", "testIndGamma", "testIndMMReg", 
"testIndRQ", "testIndOrdinal", "testIndTobit", "censIndCR" and "censIndWR". 
}
\item{method}{
This is only for the "testIndFisher". You can either specify, "ar2" for the adjusted R-square or "sse" for the sum of squares 
of errors. The tolerance value in both cases must a number between 0 and 1. That will denote a percentage. If the percentage 
increase or decrease is less than the nubmer the algorithm stops. An alternative is "BIC" for BIC and the tolerance values are 
like in all other regression models.
}
}

\details{
The data (matrix) which will be read and compressed into a big.matrix object must be of type "numeric". We tested it and it 
works with "integer" as well. But, in general, bear in mind that only matrices will be read. We have not tested with data.frame 
for example. Whatsoever, in the help page of the package "bigmemory" this is mentioned: Any non-numeric entry will be ignored 
and replaced with NA, so reading something that traditionally would be a data.frame won't cause an error. A warning is issued. 
In all cases, the object size is always 696 bytes!
}

\value{
A list including:
\item{runtime}{
The runtime of the algorithm
}
\item{phi}{
The \eqn{phi} coefficient, returned in the quasi binomial (testIndQBinom), quasi Poisson (testIndQPois), Gamma (testIndGamma) 
and Gaussian with log link (testIndNormLog). In all other cases this is NULL.
}
\item{res}{
For the case of "big.gomp" a matrix with two columns. The selected variable(s) and the criterion value at every step. 
For the case of "gomp.path" a matrix with many columns. Every column contains the selected variables for each tolerance value, 
starting from the smallest value (which selected most variables). The final column is the deviance of the model at each step.
}
}

\references{
Pati Y. C., Rezaiifar R. & Krishnaprasad P. S. (1993). Orthogonal matching pursuit: Recursive function approximation with 
applications to wavelet decomposition. In Signals, Systems and Computers. 1993 Conference Record of The Twenty-Seventh 
Asilomar Conference on. IEEE.

Davis G. (1994). Adaptive Nonlinear Approximations. PhD thesis. 
http://www.geoffdavis.net/papers/dissertation.pdf

Mallat S. G. & Zhang Z. (1993). Matching pursuits with time-frequency dictionaries. IEEE Transactions on signal processing, 
41(12), 3397-3415.
https://www.di.ens.fr/~mallat/papiers/MallatPursuit93.pdf

Gharavi-Alkhansari M., & Huang T. S. (1998, May). A fast orthogonal matching pursuit algorithm. In Acoustics, Speech and Signal 
Processing, 1998. 
Proceedings of the 1998 IEEE International Conference on (Vol. 3, pp. 1389-1392). IEEE.

Chen S., Billings S. A., & Luo W. (1989). Orthogonal least squares methods and their application to non-linear system identification. 
International Journal of control, 50(5), 1873-1896.

Lozano A., Swirszcz G., & Abe N. (2011). Group orthogonal matching pursuit for logistic regression. In Proceedings of the Fourteenth 
International Conference on Artificial Intelligence and Statistics.

Razavi S. A. Ollila E., & Koivunen V. (2012). Robust greedy algorithms for compressed sensing. In Signal Processing 
Conference (EUSIPCO), 2012 Proceedings of the 20th European. IEEE.

Mazin Abdulrasool Hameed (2012). Comparative analysis of orthogonal matching pursuit and least angle regression. 
MSc thesis, Michigan State University.
https://www.google.gr/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&ved=0ahUKEwik9P3Yto7XAhUiCZoKHQ8XDr8QFgglMAA&url=https%3A%2F%2Fd.lib.msu.edu%2Fetd%2F1711%2Fdatastream%2FOBJ%2Fdownload%2FComparative_analysis_of_orthogonal_matching_pursuit_and_least_angle_regression.pdf&usg=AOvVaw2fRcSemcbteyWUiAKYi-8B 

%Tsagis M. (2018). Guide on performing feature selection with the R package MXM.
%\url{http://mensxmachina.org/site/wp-content/uploads/2018/04/Guide-on-performing-feature-selection-with-the-R-package-MXM.pdf}

Tsagris, M., Papadovasilakis, Z., Lakiotaki, K., & Tsamardinos, I. (2022). 
The \eqn{\gamma}-OMP algorithm for feature selection with application to gene expression data. 
IEEE/ACM Transactions on Computational Biology and Bioinformatics, 19(2): 1214-1224.
}

\author{
Michail Tsagris

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}. For more information see the "bigmemory" package.
}

\seealso{
\code{ \link{gomp}, \link{read.big.data} }
}

\examples{
\dontrun{
dataset <- matrix( runif(10^6 * 50, 1, 100), ncol = 50 )
write.csv(data, "dataset.csv", sep = ",")
a <- read.big.data("dataset.csv")
mod <- big.gomp(dataset = a, test = "testIndFisher", tol = 0.01)
}
}

