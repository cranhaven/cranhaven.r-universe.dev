
\name{6. BLUP estimation from Linear Mixed Model}
\alias{fitBLUP}
\title{Fitting a Linear Mixed model to calculate BLUP}
\usage{
fitBLUP(y, X = NULL, Z = NULL, K = NULL, trn = NULL,
        EVD = NULL, varU = NULL, varE = NULL,
        ID_geno = NULL, ID_trait = NULL, intercept = TRUE,
        BLUP = TRUE, method = c("REML","ML"),
        interval = c(1E-9,1E9), tol = 1E-8, maxiter = 1000,
        n.regions = 10, verbose = TRUE)
            
}
\arguments{
\item{y}{(numeric vector) Response variable. It can be a matrix with each column representing a different response variable}

\item{X}{(numeric matrix) Design matrix for fixed effects. When \code{X=NULL} a vector of ones is constructed only for the intercept (default)}

\item{Z}{(numeric matrix) Design matrix for random effects. When \code{Z=NULL} an identity matrix is considered (default) thus \ifelse{html}{\out{<b>G</b> = <b>K</b>}}{\eqn{\textbf{G = K}}{G = K}}; otherwise \ifelse{html}{\out{<b>G</b> = <b>Z K Z'</b>}}{\eqn{\textbf{G} = \textbf{Z K Z'}}{G = Z K Z'}} is used}

\item{K}{(numeric matrix) Kinship relationship matrix}

\item{trn}{(integer vector) Which elements from vector \code{y} are to be used for training. When \code{trn = NULL}, non-NA entries in vector \code{y} will be used as training set}

\item{EVD}{(list) Eigenvectors and eigenvalues from eigenvalue decomposition (EVD) of \ifelse{html}{\out{<b>G</b>}}{\eqn{\textbf{G}}{G}} corresponding to training data}

\item{ID_geno}{(character/integer) For within-trait analysis only, vector with either names or indices mapping entries of the vector \code{y} to rows/columns of matrix \code{G}}

\item{ID_trait}{(character/integer) For within-trait analysis only, vector with either names or indices mapping entries of the vector \code{y} to different traits}

\item{varU, varE}{(numeric) Genetic and residual variances. When both \code{varU} and \code{varE} are not \code{NULL} they are not calculated; otherwise, the likelihood function (REML or ML) is optimized to search for the genetic/residual variances ratio}

\item{intercept}{\code{TRUE} or \code{FALSE} to whether fit an intercept. When \code{FALSE}, the model assumes a null intercept}

\item{BLUP}{\code{TRUE} or \code{FALSE} to whether return the random effects estimates}

\item{method}{(character) Either 'REML' (Restricted Maximum Likelihood) or 'ML' (Maximum Likelihood)}

\item{tol}{(numeric) Maximum error between two consecutive solutions (convergence tolerance) when finding the root of the log-likelihood's first derivative}

\item{maxiter}{(integer) Maximum number of iterations to run before convergence is reached}

\item{interval}{(numeric vector) Range of values in which the root is searched}

\item{n.regions}{(numeric) Number of regions in which the searched 'interval' is divided for local optimization}

\item{verbose}{\code{TRUE} or \code{FALSE} to whether show progress}
}
\value{
Returns a list object that contains the elements:
\itemize{
  \item \code{b}: (vector) fixed effects solutions (including the intercept).
  \item \code{u}: (vector) total genetic values (\ifelse{html}{\out{<b>u</b> = <b>Z g</b>}}{\eqn{\textbf{u}=\textbf{Z}\textbf{g}}{u = Z g}}).
  
  \item \code{g}: (vector) genetic effects solutions.
  \item \code{varU}: random effect variance.
  \item \code{varE}: residual variance.
  \item \code{h2}: heritability.
  \item \code{convergence}: (logical) whether Brent's method converged.
  \item \code{method}: either 'REML' or 'ML' method used.
}
}
\description{
Solves the Linear Mixed Model and calculates the Best Linear Unbiased Predictor (BLUP)
}
\details{
The basic linear mixed model that relates phenotypes with genetic values is of the form

\ifelse{html}{\out{<p style='text-align:center'><b>y</b> = <b>X b</b> + <b>Z g</b> + <b>e</b></p>}}{\deqn{\textbf{y}=\textbf{X}\textbf{b}+\textbf{Z}\textbf{g}+\textbf{e}}{y = X b + Z g + e}}

where
\ifelse{html}{\out{<b>y</b>}}{\eqn{\textbf{y}}{y}} is a vector with the response,
\ifelse{html}{\out{<b>b</b>}}{\eqn{\textbf{b}}{b}} is the vector of fixed effects,
\ifelse{html}{\out{<b>u</b>}}{\eqn{\textbf{u}}{u}} is the vector of the (random) genetic effects of the genotypes,
\ifelse{html}{\out{<b>e</b>}}{\eqn{\textbf{e}}{e}} is the vector of environmental residuals (random error), and
\ifelse{html}{\out{<b>X</b>}}{\eqn{\textbf{X}}{X}} and \ifelse{html}{\out{<b>Z</b>}}{\eqn{\textbf{Z}}{Z}} are design matrices for the fixed and genetic effects, respectively. Genetic effects are assumed to follow a Normal distribution as
\ifelse{html}{\out{<b>g</b> ~ N(<b>0</b>,&sigma;<sup>2</sup><sub>u</sub><b>K</b>)}}{\eqn{\textbf{g}\sim N(\textbf{0},\sigma^2_u\textbf{K})}{g ~ N(0,sigma^2_u K)}}, and the error terms are assumed
\ifelse{html}{\out{<b>e</b> ~ N(<b>0</b>,&sigma;<sup>2</sup><sub>e</sub><b>I</b>)}}{\eqn{\textbf{e}\sim N(\textbf{0},\sigma^2_e\textbf{I})}{e ~ N(0,sigma^2_e I)}}.

The vector of total genetic values
\ifelse{html}{\out{<b>u</b> = <b>Z g</b>}}{\eqn{\textbf{u}=\textbf{Z}\textbf{g}}{u = Z g}} will therefore follow 
\ifelse{html}{\out{<b>u</b> ~ N(<b>0</b>,&sigma;<sup>2</sup><sub>u</sub><b>G</b>)}}{\eqn{\textbf{u}\sim N(\textbf{0},\sigma^2_u\textbf{G})}{g ~ N(0,sigma^2_u G)}} where
\ifelse{html}{\out{<b>G</b> = <b>Z K Z'</b>}}{\eqn{\textbf{G}=\textbf{Z}\textbf{K}\textbf{Z}'}{G = Z K Z'}}.
In the un-replicated case, \ifelse{html}{\out{<b>Z</b> = <b>I</b>}}{\eqn{\textbf{Z}=\textbf{I}}{Z = I}} is an identity matrix, and hence 
\ifelse{html}{\out{<b>u</b> = <b>g</b>}}{\eqn{\textbf{u}=\textbf{g}}{u = g}} and
\ifelse{html}{\out{<b>G</b> = <b>K</b>}}{\eqn{\textbf{G}=\textbf{K}}{G = K}}.

The predicted values \ifelse{html}{\out{<b>u</b><sub>trn</sub> = \{u<sub>i</sub>\}}}{\eqn{\textbf{u}_{trn}=\{u_i\}}{u_trn = {u_i}}},
\ifelse{html}{\out{i = 1,2,...,n<sub>trn</sub>}}{\eqn{i=1,2,...,n_{trn}}{i = 1,2,...,n_trn}}, corresponding to observed data (training set) are derived as

\ifelse{html}{\out{<p style='text-align:center'><b>u</b><sub>trn</sub> = <b>B</b> (<b>y</b><sub>trn</sub> - <b>X</b><sub>trn</sub><b>b</b>)</p>}}{\deqn{\textbf{u}_{trn}=\textbf{B}(\textbf{y}_{trn}-\textbf{X}_{trn}\textbf{b})}{u_trn = B (y_trn - X_trn*b)}}

where \ifelse{html}{\out{<b>B</b>}}{\eqn{\textbf{B}}{B}}
is a matrix of weights given by

\ifelse{html}{\out{<p style='text-align:center'><b>B</b> = &sigma;<sup>2</sup><sub>u</sub><b>G</b><sub>trn</sub> (&sigma;<sup>2</sup><sub>u</sub><b>G</b><sub>trn</sub> + &sigma;<sup>2</sup><sub>e</sub><b>I</b>)<sup>-1</sup></p>}}{\deqn{\textbf{B}=\sigma_u^2\textbf{G}_{trn}(\sigma_u^2\textbf{G}_{trn} + \sigma_e^2\textbf{I})^{-1}}{B = sigma^2_u G[trn](sigma^2_u G[trn] + sigma^2_e I)^-1}}

where \ifelse{html}{\out{<b>G</b><sub>trn</sub>}}{\eqn{\textbf{G}_{trn}}{G[trn]}}
is the sub-matrix corresponding to the training set. This matrix can be rewritten as

\ifelse{html}{\out{<p style='text-align:center'><b>B</b> = <b>G</b><sub>trn</sub> (<b>G</b><sub>trn</sub> + &theta;<b>I</b>)<sup>-1</sup></p>}}{\deqn{\textbf{B}=\textbf{G}_{trn}(\textbf{G}_{trn} + \theta\textbf{I})^{-1}}{B = G[trn](G[trn] + theta I)^-1}}

where \ifelse{html}{\out{&theta; = &sigma;<sup>2</sup><sub>e</sub>/&sigma;<sup>2</sup><sub>u</sub>}}{\eqn{\theta=\sigma_e^2/\sigma_u^2}{theta = sigma^2_e/sigma^2_u}} is the residual/genetic variances ratio representing a ridge-like shrinkage parameter.

The matrix \ifelse{html}{\out{<b>H</b> = <b>G</b><sub>trn</sub> + &theta;<b>I</b>}}{\eqn{\textbf{H}=\textbf{G}_{trn} + \theta\textbf{I}}{H = G[trn] + theta I}}
in the above equation can be used to obtain predictions corresponding to un-observed data (testing set),
\ifelse{html}{\out{<b>u</b><sub>tst</sub> = \{u<sub>i</sub>\}}}{\eqn{\textbf{u}_{tst}=\{u_i\}}{u_tst = {u_i}}},
\ifelse{html}{\out{i = 1,2,...,n<sub>tst</sub>}}{\eqn{i=1,2,...,n_{tst}}{i = 1,2,...,n_tst}}, by  

\ifelse{html}{\out{<p style='text-align:center'><b>B</b> = <b>G</b><sub>tst,trn</sub> (<b>G</b><sub>trn</sub> + &theta;<b>I</b>)<sup>-1</sup></p>}}{\deqn{\textbf{B}=\textbf{G}_{tst,trn}(\textbf{G}_{trn} + \theta\textbf{I})^{-1}}{B = G[tst,trn](G[trn] + theta I)^-1}}

where
\ifelse{html}{\out{<b>G</b><sub>tst,trn</sub>}}{\eqn{\textbf{G}_{tst,trn}}{G[tst,trn]}}
is the sub-matrix of \ifelse{html}{\out{<b>G</b>}}{\eqn{\textbf{G}}{G}} corresponding to the testing set (in rows) and training set (in columns).  

Solutions are found using the GEMMA (Genome-wide Efficient Mixed Model Analysis) approach (Zhou & Stephens, 2012). First, the Brent's method is implemented to solve for the genetic/residual variances ratio (i.e., \ifelse{html}{\out{1/&theta;}}{\eqn{1/\theta}{1/theta}}) from the first derivative of the log-likelihood (either REML or ML). Then, variances \ifelse{html}{\out{&sigma;<sup>2</sup><sub>u</sub>}}{\eqn{\sigma_u^2}{sigma^2_u}} and \ifelse{html}{\out{&sigma;<sup>2</sup><sub>e</sub>}}{\eqn{\sigma_e^2}{sigma^2_e}} are calculated. Finally, \ifelse{html}{\out{<b>b</b>}}{\eqn{\textbf{b}}{b}} is obtained using Generalized Least Squares.
}
\examples{
  require(SFSI)
  data(wheatHTP)
  
  index = which(Y$trial \%in\% 1:20)    # Use only a subset of data
  Y = Y[index,]
  M = scale(M[index,])/sqrt(ncol(M))  # Subset and scale markers
  G = tcrossprod(M)                   # Genomic relationship matrix
  Y0 = scale(as.matrix(Y[,4:6]))      # Response variable
  
  #---------------------------------------------------
  # Single-trait model
  #---------------------------------------------------
  y = Y0[,1]     
  
  # Training and testing sets
  tst = which(Y$trial \%in\% 1:3)
  trn = seq_along(y)[-tst]
  
  # Kinship-based model
  fm1 = fitBLUP(y, K=G, trn=trn)
  
  head(fm1$g)                  # Genetic effects
  plot(y[tst],fm1$yHat[tst])   # Predicted vs observed values in testing set
  cor(y[tst],fm1$yHat[tst])    # Prediction accuracy in testing set
  cor(y[trn],fm1$yHat[trn])    # Prediction accuracy in training set
  fm1$varU                     # Genetic variance
  fm1$varE                     # Residual variance
  fm1$h2                       # Heritability
  fm1$b                        # Fixed effects
  \donttest{
  # Markers-based model
  fm2 = fitBLUP(y, Z=M, trn=trn)
  head(fm2$g)                   # Marker effects
  all.equal(fm1$yHat, fm2$yHat)
  fm2$varU                      # Genetic variance
  fm2$varU*sum(apply(M,2,var))
  
  #---------------------------------------------------
  # Multiple response variables
  #---------------------------------------------------
  ID_geno = as.vector(row(Y0))
  ID_trait = as.vector(col(Y0))
  y = as.vector(Y0)
  
  # Training and testing sets
  tst = c(which(ID_trait==1)[Y$trial \%in\% 1:3],
          which(ID_trait==2)[Y$trial \%in\% 1:3],
          which(ID_trait==3)[Y$trial \%in\% 1:3])
  trn = seq_along(y)[-tst]
  
  # Across traits model
  fm3 = fitBLUP(y, K=G, ID_geno=ID_geno, trn=trn)
  plot(fm1$yHat,fm3$yHat[ID_trait==1])  # different from the single-trait model
  
  # Within traits model: pass an index for traits
  fm4 = fitBLUP(y, K=G, ID_geno=ID_geno, ID_trait=ID_trait, trn=trn)
  plot(fm1$yHat,fm4$yHat[ID_trait==1])  # equal to the single-trait model
  }
}
\references{
Zhou X, Stephens M (2012). Genome-wide efficient mixed-model analysis for association studies. \emph{Nature Genetics}, \bold{44}(7), 821-824
}
