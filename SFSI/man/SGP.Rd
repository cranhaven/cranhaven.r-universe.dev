
\name{5. Sparse Genomic Prediction (SGP)}
\alias{SGP}
\alias{SGP.CV}
\title{Sparse Genomic Prediction}
\usage{
SGP(y = NULL, X = NULL, b = NULL, Z = NULL, K = NULL,
    trn = NULL, tst = NULL, varU = NULL, varE = NULL,
    ID_geno = NULL, ID_trait = NULL, intercept = TRUE,
    alpha = 1, lambda = NULL, nlambda = 100,
    lambda.min = .Machine$double.eps^0.5,
    common.lambda = TRUE, subset = NULL, tol = 1E-4,
    maxiter = 500, method = c("REML","ML"), tag = NULL,
    save.at = NULL, precision.format = c("single","double"),
    mc.cores = 1L, verbose = 2)
    
SGP.CV(y, X = NULL, b = NULL, Z = NULL, K,
       trn = NULL, varU = NULL, varE = NULL,
       ID_geno = NULL, ID_trait = NULL,
       intercept = TRUE, alpha = 1, lambda = NULL,
       nlambda = 100, lambda.min = .Machine$double.eps^0.5,
       common.lambda = TRUE, nfolds = 5, nCV = 1L,
       folds = NULL, seed = NULL, subset = NULL, tol = 1E-4,
       maxiter = 500, method = c("REML","ML"), tag = NULL,
       save.at = NULL, mc.cores = 1L, verbose = TRUE)
}
\arguments{
\item{y}{(numeric vector) Response variable. It can be a matrix with each column representing a different response variable. If it is passed to the 'SGP' function, predicted values for testing data are computed using phenotypes from training data}

\item{X}{(numeric matrix) Design matrix for fixed effects. When \code{X = NULL}, a vector of ones is constructed only for the intercept (default)}

\item{b}{(numeric vector) Fixed effects. When \code{b = NULL}, only the intercept is estimated from training data using generalized least squares (default)}

\item{K}{(numeric matrix) Kinship relationship matrix}

\item{Z}{(numeric matrix) Design matrix for random effects. When \code{Z = NULL} an identity matrix is considered (default) thus \ifelse{html}{\out{<b>G</b> = <b>K</b>}}{\eqn{\textbf{G = K}}{G = K}}; otherwise \ifelse{html}{\out{<b>G</b> = <b>Z K Z'</b>}}{\eqn{\textbf{G} = \textbf{Z K Z'}}{G = Z K Z'}} is used}

\item{varU, varE}{(numeric) Genetic and residual variances. When either \code{varU = NULL} or \code{varE = NULL} (default), they are calculated from training data using the function 'fitBLUP' (see \code{help(fitBLUP)}) for single-trait analysis. For multi-trait analysis, unstructured matrices are calculated using the function 'getGenCov' in a pairwise fashion}

\item{ID_geno}{(character/integer) For multi-trait analysis only, vector with either names or indices mapping entries of the vector \code{y} to rows/columns of matrix \code{G}}

\item{ID_trait}{(character/integer) For multi-trait analysis only, vector with either names or indices mapping entries of the vector \code{y} to rows/columns of matrices \code{varU} and \code{varE}}

\item{intercept}{\code{TRUE} or \code{FALSE} to whether fit an intercept. When \code{FALSE}, the model assumes a null intercept}

\item{trn, tst}{(integer vector) Which elements from vector \code{y} are in training and testing sets, respectively. When both \code{trn = NULL} and \code{tst = NULL}, non-NA entries in vector \code{y} will be used as training set}

\item{subset}{(integer vector) \eqn{c(m,M)} to fit the model only for the \ifelse{html}{\out{m<sup>th</sup>}}{\eqn{m^{th}}{m^{th}}}
subset out of \eqn{M} subsets that the testing set will be divided into. Results can be automatically saved as per the \code{save.at} argument and can be retrieved later using function 'read_SGP' (see \code{help(read_SGP)}). In cross-validation, it should has format \eqn{c(fold,CV)} to fit the model for a given fold within partition}

\item{alpha}{(numeric) Value between 0 and 1 for the weights given to the L1 and L2-penalties}

\item{lambda}{(numeric vector) Penalization parameter sequence. Default is \code{lambda = NULL}, in this case a decreasing grid of \code{nlambda} lambdas will be generated
starting from a maximum equal to

\ifelse{html}{\out{<p style='text-align:center;font-family:courier'>max(abs(G[trn,tst])/alpha)</p>}}{\deqn{\code{max(abs(G[trn,tst])/alpha)}}{max(abs(G[trn,tst])/alpha)}}

to a minimum equal to zero. If \code{alpha = 0} the grid is generated starting from a maximum equal to 5}

\item{nlambda}{(integer) Number of lambdas generated when \code{lambda = NULL}}

\item{lambda.min}{(numeric) Minimum value of lambda in the generated grid when \code{lambda = NULL}}

\item{nfolds}{(integer/character) Either 2,3,5,10 or 'n' indicating the number of non-overlapping folds in which the data is split into to do cross-validation. When \code{nfolds = 'n'} a leave-one-out CV is performed}

\item{seed}{(numeric vector) Seed to fix randomization when creating folds for cross-validation. If it is a vector, a number equal to its length of CV repetitions are performed}

\item{nCV}{(integer) Number of CV repetitions to be performed. Default is \code{nCV = 1}}

\item{folds}{(integer matrix) A matrix with \code{nTRN} rows and \code{nCV} columns where each column represents a different partition with \code{nfolds} folds. It can be created using the function 'get_folds'}

\item{common.lambda}{\code{TRUE} or \code{FALSE} to whether computing the coefficients for a grid of lambdas common to all individuals in testing set
 or for a grid of lambdas specific to each individual in testing set. Default is \code{common.lambda = TRUE}}

\item{mc.cores}{(integer) Number of cores used. When \code{mc.cores} > 1, the analysis is run in parallel for each testing set individual. Default is \code{mc.cores = 1}}

\item{tol}{(numeric) Maximum error between two consecutive solutions of the CD algorithm to declare convergence}

\item{maxiter}{(integer) Maximum number of iterations to run the CD algorithm at each lambda step before convergence is reached}

\item{save.at}{(character) Path where files (regression coefficients and output object) are to be saved (this may include a prefix added to the files). Default \code{save.at = NULL} will no save any results and they are returned in the output object. No regression coefficients are saved for function 'SGP.CV'}

\item{precision.format}{(character) Either 'single' or 'double' for numeric precision and memory occupancy (4 or 8 bytes, respectively) of the regression coefficients. This is only used when \code{save.at} is not \code{NULL}}

\item{method}{(character) Either 'REML' (Restricted Maximum Likelihood) or 'ML' (Maximum Likelihood) to calculate variance components as per the function 'fitBLUP'}

\item{tag}{(character) Name given to the output for tagging purposes. Default \code{tag = NULL} will give the name of the method used}

\item{verbose}{(integer) If greater than zero analysis details will be printed}
}
\value{
Function 'SGP' returns a list object of the class 'SGP' for which methods \code{coef}, \code{predict}, \code{plot}, and \code{summary} exist. Functions 'net.plot' and 'path.plot' can be also used. It contains the elements:
\itemize{
  \item \code{b}: (vector) fixed effects solutions (including the intercept).
  \item \code{Xb}: (vector) total fixed values (\ifelse{html}{\out{<b>X b</b>}}{\eqn{\textbf{X}\textbf{b}}{X b}}).
  \item \code{u}: (matrix) total genetic values (\ifelse{html}{\out{<b>u</b> = <b>Z g</b>}}{\eqn{\textbf{u}=\textbf{Z}\textbf{g}}{u = Z g}})
  for testing individuals (in rows) associated to each value of lambda (in columns).
  \item \code{varU}, \code{varE}, \code{h2}: variance components solutions.
  \item \code{alpha}: value for the elastic-net weights used.
  \item \code{lambda}: (matrix) sequence of values of lambda used (in columns) for each testing individual (in rows).
  \item \code{nsup}: (matrix) number of non-zero predictors at each solution given by lambda for each testing individual (in rows).
  \item \code{file_beta}: path where regression coefficients are saved.
}

Function 'SGP.CV' returns a list object of length \code{nCV} of the class 'SGP'. Optimal cross-validated penalization values can be obtained using the\code{summary} method. Method \code{plot} is also available.

}
\description{
Computes the entire Elastic-Net solution for the regression coefficients of a Selection Index for a grid of values of the penalization parameter.

An optimal penalization can be chosen using cross-validation (CV) within a specific training set.
}
\details{
The basic linear mixed model that relates phenotypes with genetic values is of the form

\ifelse{html}{\out{<p style='text-align:center'><b>y</b> = <b>X b</b> + <b>Z g</b> + <b>e</b></p>}}{\deqn{\textbf{y}=\textbf{X}\textbf{b}+\textbf{Z}\textbf{g}+\textbf{e}}{y = X b + Z g + e}}

where
\ifelse{html}{\out{<b>y</b>}}{\eqn{\textbf{y}}{y}} is a vector with the response,
\ifelse{html}{\out{<b>b</b>}}{\eqn{\textbf{b}}{b}} is the vector of fixed effects,
\ifelse{html}{\out{<b>g</b>}}{\eqn{\textbf{g}}{g}} is the vector of the genetic values of the genotypes,
\ifelse{html}{\out{<b>e</b>}}{\eqn{\textbf{e}}{e}} is the vector of environmental residuals, and
\ifelse{html}{\out{<b>X</b>}}{\eqn{\textbf{X}}{X}} and \ifelse{html}{\out{<b>Z</b>}}{\eqn{\textbf{Z}}{Z}} are design matrices for the fixed and genetic effects, respectively. Genetic effects are assumed to follow a Normal distribution as
\ifelse{html}{\out{<b>g</b> ~ N(<b>0</b>,&sigma;<sup>2</sup><sub>u</sub><b>K</b>)}}{\eqn{\textbf{g}\sim N(\textbf{0},\sigma^2_u\textbf{K})}{g ~ N(0,sigma^2_u K)}}, and environmental terms are assumed
\ifelse{html}{\out{<b>e</b> ~ N(<b>0</b>,&sigma;<sup>2</sup><sub>e</sub><b>I</b>)}}{\eqn{\textbf{e}\sim N(\textbf{0},\sigma^2_e\textbf{I})}{e ~ N(0,sigma^2_e I)}}.

The resulting vector of genetic values
\ifelse{html}{\out{<b>u</b> = <b>Z g</b>}}{\eqn{\textbf{u}=\textbf{Z}\textbf{g}}{u = Z g}} will therefore follow 
\ifelse{html}{\out{<b>u</b> ~ N(<b>0</b>,&sigma;<sup>2</sup><sub>u</sub><b>G</b>)}}{\eqn{\textbf{u}\sim N(\textbf{0},\sigma^2_u\textbf{G})}{u ~ N(0,sigma^2_u G)}} where
\ifelse{html}{\out{<b>G</b> = <b>Z K Z'</b>}}{\eqn{\textbf{G}=\textbf{Z}\textbf{K}\textbf{Z}'}{G = Z K Z'}}.
In the un-replicated case, \ifelse{html}{\out{<b>Z</b> = <b>I</b>}}{\eqn{\textbf{Z}=\textbf{I}}{Z = I}} is an identity matrix, and hence 
\ifelse{html}{\out{<b>u</b> = <b>g</b>}}{\eqn{\textbf{u}=\textbf{g}}{u = g}} and
\ifelse{html}{\out{<b>G</b> = <b>K</b>}}{\eqn{\textbf{G}=\textbf{K}}{G = K}}.

The values \ifelse{html}{\out{<b>u</b><sub>tst</sub> = \{u<sub>i</sub>\}}}{\eqn{\textbf{u}_{tst}=\{u_i\}}{u_tst = {u_i}}},
\ifelse{html}{\out{i = 1,2,...,n<sub>tst</sub>}}{\eqn{i=1,2,...,n_{tst}}{i = 1,2,...,n_tst}}, for a testing set are estimated individual-wise using (as predictors) all available observations in a training set as

\ifelse{html}{\out{<p style='text-align:center'>u<sub>i</sub> = <b>&beta;</b>'<sub>i</sub> (<b>y</b><sub>trn</sub> - <b>X</b><sub>trn</sub><b>b</b>)</p>}}{\deqn{u_i=\boldsymbol{\beta}'_i(\textbf{y}_{trn}-\textbf{X}_{trn}\textbf{b})}{u_i = beta'_i (y_trn - X_trn*b)}}

where \ifelse{html}{\out{<b>&beta;</b><sub>i</sub>}}{\eqn{\boldsymbol{\beta}_i}{beta_i}}
is a vector of weights that are found separately for each individual in the testing set, by minimizing the penalized mean squared error function

\ifelse{html}{\out{<p style='text-align:center'>-&sigma;<sup>2</sup><sub>u</sub><b>G</b>'<sub>trn,tst(i)</sub> <b>&beta;</b><sub>i</sub> + 1/2 <b>&beta;</b>'<sub>i</sub>(&sigma;<sup>2</sup><sub>u</sub><b>G</b><sub>trn</sub> + &sigma;<sup>2</sup><sub>e</sub><b>I</b>)<b>&beta;</b><sub>i</sub> + &lambda; J(<b>&beta;</b><sub>i</sub>)</p>}}{\deqn{-\sigma^2_u\textbf{G}_{trn,tst(i)}' \boldsymbol{\beta}_i + 1/2 \boldsymbol{\beta}_i'(\sigma^2_u\textbf{G}_{trn} + \sigma^2_e\textbf{I}) \boldsymbol{\beta}_i + \lambda J(\boldsymbol{\beta}_i)}{-sigma^2_u G'[trn,tst(i)] beta_i + 1/2 beta_i'(sigma^2_u G[trn] + sigma^2_e I)beta_i + lambda J(beta_i)}}

where
\ifelse{html}{\out{<b>G</b><sub>trn,tst(i)</sub>}}{\eqn{\textbf{G}_{trn,tst(i)}}{G[trn,tst(i)}}
is the \ifelse{html}{\out{i<sup>th</sup>}}{\eqn{i^{th}}{i^th}} column of the
sub-matrix of \ifelse{html}{\out{<b>G</b>}}{\eqn{\textbf{G}}{G}} whose rows correspond to the training set and columns to the testing set;
\ifelse{html}{\out{<b>G</b><sub>trn</sub>}}{\eqn{\textbf{G}_{trn}}{G[trn]}}
is the sub-matrix corresponding to the training set; \ifelse{html}{\out{&lambda;}}{\eqn{\lambda}{lambda}} is the penalization parameter; and
\ifelse{html}{\out{J(<b>&beta;</b><sub>i</sub>)}}{\eqn{J(\boldsymbol{\beta}_i)}{J(beta_i)}}
is a penalty function given by

\ifelse{html}{\out{<p style='text-align:center'>1/2(1-&alpha;)||<b>&beta;</b><sub>i</sub>||<sub>2</sub><sup>2</sup> + &alpha;||<b>&beta;</b><sub>i</sub>||<sub>1</sub></p>}}{\deqn{1/2(1-\alpha)||\boldsymbol{\beta}_i||_2^2 + \alpha||\boldsymbol{\beta}_i||_1}{1/2(1-alpha)||beta_i||_2^2 + alpha||beta_i||_1}}

where \ifelse{html}{\out{0 &le; &alpha; &le; 1}}{\eqn{0\leq\alpha\leq 1}{0 <= alpha <= 1}}, and
\ifelse{html}{\out{||<b>&beta;</b><sub>i</sub>||<sub>1</sub> = &sum;<sub>j=1</sub>|&beta;<sub>ij</sub>|}}{\eqn{||\boldsymbol{\beta}_i||_1=\sum_{j=1}^{n_{trn}}|\beta_{ij}|}{||beta_i||_1 = sum(|beta_ij|)}} and
\ifelse{html}{\out{||<b>&beta;</b><sub>i</sub>||<sub>2</sub><sup>2</sup> = &sum;<sub>j=1</sub>&beta;<sub>ij</sub><sup>2</sup>}}{\eqn{||\boldsymbol{\beta}_i||_2^2=\sum_{j=1}^{n_{trn}}\beta_{ij}^2}{||beta_i||_2^2 = sum(beta_ij^2)}} are the L1 and (squared) L2-norms, respectively.

Function 'SGP' calculates each individual solution using the function 'solveEN' (via the Coordinate Descent algorithm, see \code{help(solveEN)}) by setting the argument \code{Sigma} equal to
\ifelse{html}{\out{&sigma;<sup>2</sup><sub>u</sub><b>G</b><sub>trn</sub> + &sigma;<sup>2</sup><sub>e</sub><b>I</b>}}{\eqn{\sigma^2_u\textbf{G}_{trn}+\sigma^2_e\textbf{I}}{sigma^2_u G[trn] + sigma^2_e I}}
and \code{Gamma} equal to
\ifelse{html}{\out{&sigma;<sup>2</sup><sub>u</sub><b>G</b><sub>trn,tst(i)</sub>}}{\eqn{\sigma^2_u\textbf{G}_{trn,tst(i)}}{sigma^2_u G[trn,tst(i)]}}.

Function 'SGP.CV' performs cross-validation within the training data specified in argument \code{trn}. Training data is divided into \eqn{k}{k} folds and the SGP is sequentially derived for (all individuals in) one fold (as testing set) using information from the remaining folds (as training set).
}
\examples{
  require(SFSI)
  data(wheatHTP)
  
  index = which(Y$trial \%in\% 1:12)    # Use only a subset of data
  Y = Y[index,]
  M = scale(M[index,])/sqrt(ncol(M))  # Subset and scale markers
  G = tcrossprod(M)                   # Genomic relationship matrix
  Y0 = scale(as.matrix(Y[,4:6]))      # Response variable
  
  #---------------------------------------------------
  # Single-trait model
  #---------------------------------------------------
  y = Y0[,1]    
  
  # Training and testing sets
  tst = which(Y$trial \%in\% 2:3)
  trn = seq_along(y)[-tst]
  
  # Sparse genomic prediction
  fm1 = SGP(y, K=G, trn=trn, tst=tst)
  
  uHat = predict(fm1)        # Predicted values for each testing element
  out = summary(fm1)         # Useful function to get results
  corTST = out$accuracy      # Testing set accuracy (correlation cor(y,yHat))
  out$optCOR                 # SGP with maximum accuracy
  B = coef(fm1)              # Regression coefficients for all tst
  B = coef(fm1, iy=1)        # Coefficients for first tst (tst[1])
  B = coef(fm1, ilambda=10)  # Coefficients associated to the 10th lambda
  B = coef(fm1, nsup=10)     # Coefficients for which nsup=10
  
  plot(fm1)                  # Penalization vs accuracy plot
  plot(fm1, y.stat="MSE", ylab='Mean Square Error', xlab='Sparsity')     
  
  varU = fm1$varU
  varE = fm1$varE
  b = fm1$b
  \donttest{
  #---------------------------------------------------
  # Predicting a testing set using a value of lambda
  # obtained from cross-validation in a traning set
  #---------------------------------------------------
  # Run a cross validation in training set
  fm2 = SGP.CV(y, K=G, varU=varU, varE=varE, b=b, trn=trn, nfolds=5, tag="1 5CV")
  lambda = summary(fm2)$optCOR["lambda"]

  # Fit the index with the obtained lambda
  fm3 = SGP(y, K=G, varU=varU, varE=varE, b=b, trn=trn, tst=tst, lambda=lambda)
  summary(fm3)$accuracy        # Testing set accuracy

  # Compare the accuracy with that of the non-sparse index (G-BLUP)
  summary(fm1)$accuracy[fm1$nlambda,1] # we take the last one
  
  #---------------------------------------------------
  # Multi-trait SGP
  #---------------------------------------------------
  ID_geno = as.vector(row(Y0))
  ID_trait = as.vector(col(Y0))
  y = as.vector(Y0)
  
  # Training and testing sets
  tst = c(which(ID_trait==1)[Y$trial \%in\% 2:3],
          which(ID_trait==2)[Y$trial \%in\% 2],
          which(ID_trait==3)[Y$trial \%in\% 3])
  trn = seq_along(y)[-tst]
  
  fmMT = SGP(y=y, K=G, ID_geno=ID_geno, ID_trait=ID_trait,
             trn=trn, tst=tst)
  
  multitrait.plot(fmMT)
  }
}
