\name{MatTrans.EM}
\alias{MatTrans.EM}
% Also NEED an '\alias' for EACH other topic documented here.
\title{ EM algorithm for matrix clustering}
\description{
Runs the EM algorithm for matrix clustering}
\usage{
MatTrans.EM(Y, initial = NULL, la = NULL, nu = NULL, 
model = NULL, trans = "None", la.type = 0, 
row.skew = TRUE, col.skew = TRUE, tol = 1e-05, 
short.iter = NULL, long.iter = 1000, all.models = TRUE, 
size.control = 0, silent = TRUE)
}
% maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y }{ dataset of random matrices (p x T x n), n random matrices of dimensionality (p x T)}
  \item{initial }{ initialization parameters provided by function MatTrans.init()}
  \item{la }{ initial skewness for rows (K x p)}
  \item{nu }{ initial skewness for columns (K x T)}
   \item{model }{ parsimonious model type, if null, then all 210 models are run}
  \item{trans }{ transformation method: None (Gaussian models), Power, Manly}
  \item{la.type }{ lambda type 0 or 1, 0: unrestricted, 1: same lambda across all variables}
  \item{row.skew }{ if skewness for rows are fitted: TRUE or FALSE}
  \item{col.skew }{ if skewness for columns are fitted: TRUE or FALSE}
  \item{tol }{ tolerance level}
  \item{short.iter }{ number of short EM iterations; if not specified, just run long EM}
  \item{long.iter }{ number of long EM iterations}
  \item{all.models }{ if true, run long EM for all models; otherwise just the best model returned by short EM in terms of BIC }
  \item{size.control }{ minimum size of clusters allowed for controlling spurious solutions}
  \item{silent }{ whether to produce output of steps or not}
}
\details{
Runs the EM algorithm for modeling and clustering matrices for a provided dataset. Both matrix Gaussian mixture, matrix Power mixture and matrix Manly transformation mixture can be employed. The user should use the MatTrans.init() function to get initial parameters and input them as 'initial'. In the case when transformation parameters are not provided but 'trans' is specified to be 'Power' or 'Manly', 'la' and 'nu' take value of 0.5. 'model' can be specified as 'X-XXX-XX'. The first digit 'X' stands for the mean structure. It is either 'G': general mean  or 'A': additive mean. The second 'XXX' specifies the variance-covariance Sigma. There are 14 options including EII, VII, EEI, VEI, EVI, VVI, EEE, EVE, VEE, VVE, EEV, VEV, EVV and VVV with detailed explanation as follows:
"EII" spherical, equal volume
"VII" spherical, unequal volume
"EEI" diagonal, equal volume and shape
"VEI" diagonal, varying volume, equal shape
"EVI" diagonal, equal volume, varying shape
"VVI" diagonal, varying volume and shape
"EEE" ellipsoidal, equal volume, shape, and orientation
"EVE" ellipsoidal, equal volume and orientation (*)
"VEE" ellipsoidal, equal shape and orientation (*)
"VVE" ellipsoidal, equal orientation (*)
"EEV" ellipsoidal, equal volume and equal shape
"VEV" ellipsoidal, equal shape
"EVV" ellipsoidal, equal volume (*)
"VVV" ellipsoidal, varying volume, shape, and orientation
 The last 2-digit 'XX' specifies the variance-covariance Psi. There are 8 options including II, EI, VI, EE, VE, EV, VV, AR. The user can specify the 'model' to be for example 'X-VVV-EV', then both 'G' and 'A' mean structures will be fitted while Sigma and Psi are fixed at 'VVV' and 'EV', respectively. Similarly, 'model' can be specified as 'G-XXX-EV' or 'G-VVV-XX' for selection of Sigma and Psi structures.
}
\value{No return value, called for side effects}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ EM algorithm }% __ONLY ONE__ keyword per line
