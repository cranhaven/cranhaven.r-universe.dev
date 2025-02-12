
\name{7. Genetic covariances computation}
\alias{getGenCov}
\title{Pairwise Genetic Covariance}
\usage{
getGenCov(y, X = NULL, Z = NULL, K = NULL, trn = NULL,
          EVD = NULL, ID_geno, ID_trait, scale = TRUE,
          pairwise = TRUE, verbose = TRUE, ...)
          
}
\arguments{
\item{y}{(numeric matrix) Response variable vector. It can be a matrix with each column representing a different response variable}

\item{X}{(numeric matrix) Design matrix for fixed effects. When \code{X=NULL} a vector of ones is constructed only for the intercept (default)}

\item{Z}{(numeric matrix) Design matrix for random effects. When \code{Z=NULL} an identity matrix is considered (default) thus \ifelse{html}{\out{<b>G</b> = <b>K</b>}}{\eqn{\textbf{G = K}}{G = K}}; otherwise \ifelse{html}{\out{<b>G</b> = <b>Z K Z'</b>}}{\eqn{\textbf{G} = \textbf{Z K Z'}}{G = Z K Z'}} is used}

\item{K}{(numeric matrix) Kinship relationship matrix}

\item{trn}{(integer vector) Which elements from vector \code{y} are to be used for training. When \code{trn = NULL}, non-NA entries in vector \code{y} will be used as training set}

\item{EVD}{(list) Eigenvectors and eigenvalues from eigenvalue decomposition (EVD) of \ifelse{html}{\out{<b>G</b>}}{\eqn{\textbf{G}}{G}} corresponding to training data}

\item{ID_geno}{(character/integer) Vector with either names or indices mapping entries of the vector \code{y} to rows/columns of matrix \code{G}}

\item{ID_trait}{(character/integer) Vector with either names or indices mapping entries of the vector \code{y} to different traits}

\item{scale}{\code{TRUE} or \code{FALSE} to scale each column of \code{y} by their corresponding standard deviations so the resulting variables will have unit variance}

\item{pairwise}{\code{TRUE} or \code{FALSE} to calculate pairwise genetic covariances for all columns in \code{y}. When \code{pairwise = FALSE} (default) covariances of the first column in \code{y} with the remaining columns (2,...,\code{ncol(y)}) are calculated}

\item{verbose}{\code{TRUE} or \code{FALSE} to whether show progress}

\item{...}{Other arguments passed to the function 'fitBLUP'} 
}
\value{
Returns a list object that contains the elements:
\itemize{
  \item \code{varU}: (vector) genetic variances.
  \item \code{varE}: (vector) error variances.
  \item \code{covU}: (vector) genetic covariances between response variable 1 and the rest.
  \item \code{covE}: (vector) environmental covariances between response variable 1 and the rest.
}
When \code{pairwise = TRUE}, \code{varU} and \code{varE} are matrices containing all variances (diagonal) and pairwise covariances (off diagonal)
}
\description{
Calculation of genetic and environmental (unstructured) covariances matrices from pairwise models of variables with the same experimental design
}
\details{
Assumes that both \ifelse{html}{\out{<b>y</b><sub>1</sub>}}{\eqn{\textbf{y}_1}{y_1}} and
\ifelse{html}{\out{<b>y</b><sub>2</sub>}}{\eqn{\textbf{y}_2}{y_2}} follow the basic linear mixed model that relates phenotypes with genetic values of the form

\ifelse{html}{\out{<div style='text-align:center'><b>y</b><sub>1</sub> = <b>X b</b><sub>1</sub> + <b>Z g</b><sub>1</sub> + <b>e</b><sub>1</sub></div>}}{\deqn{\textbf{y}_1=\textbf{X}\textbf{b}_1+\textbf{Z}\textbf{g}_1+\textbf{e}_1}{y = X b_1 + Z g_1 + e_1}}

\ifelse{html}{\out{<div style='text-align:center'><b>y</b><sub>2</sub> = <b>X b</b><sub>2</sub> + <b>Z g</b><sub>2</sub> + <b>e</b><sub>2</sub></div>}}{\deqn{\textbf{y}_2=\textbf{X}\textbf{b}_2+\textbf{Z}\textbf{g}_2+\textbf{e}_2}{y = X b_2 + Z g_2 + e_2}}

where
\ifelse{html}{\out{<b>b</b><sub>1</sub>}}{\eqn{\textbf{b}_1}{b_1}} and 
\ifelse{html}{\out{<b>b</b><sub>2</sub>}}{\eqn{\textbf{b}_2}{b_2}} are the specific fixed effects,
\ifelse{html}{\out{<b>g</b><sub>1</sub>}}{\eqn{\textbf{g}_1}{g_1}} and 
\ifelse{html}{\out{<b>g</b><sub>2</sub>}}{\eqn{\textbf{g}_2}{g_2}} are the specific genetic effects of the genotypes,
\ifelse{html}{\out{<b>e</b><sub>1</sub>}}{\eqn{\textbf{e}_1}{e_1}} and 
\ifelse{html}{\out{<b>e</b><sub>2</sub>}}{\eqn{\textbf{e}_2}{e_2}} are the vectors of specific environmental residuals, and
\ifelse{html}{\out{<b>X</b>}}{\eqn{\textbf{X}}{X}} and \ifelse{html}{\out{<b>Z</b>}}{\eqn{\textbf{Z}}{Z}} are common design matrices for the fixed and genetic effects, respectively. Genetic effects are assumed to follow a Normal distribution as
\ifelse{html}{\out{<b>g</b><sub>1</sub> ~ N(<b>0</b>,&sigma;<sup>2</sup><sub>u1</sub><b>K</b>)}}{\eqn{\textbf{g}_1\sim N(\textbf{0},\sigma^2_{u_1}\textbf{K})}{g_1 ~ N(0,sigma^2_u1 K)}} and 
\ifelse{html}{\out{<b>g</b><sub>2</sub> ~ N(<b>0</b>,&sigma;<sup>2</sup><sub>u2</sub><b>K</b>)}}{\eqn{\textbf{g}_2\sim N(\textbf{0},\sigma^2_{u_2}\textbf{K})}{g_2 ~ N(0,sigma^2_u2 K)}}, and environmental terms are assumed
\ifelse{html}{\out{<b>e</b><sub>1</sub> ~ N(<b>0</b>,&sigma;<sup>2</sup><sub>e1</sub><b>I</b>)}}{\eqn{\textbf{e}_1\sim N(\textbf{0},\sigma^2_{e_1}\textbf{I})}{e_1 ~ N(0,sigma^2_e1 I)}} and
\ifelse{html}{\out{<b>e</b><sub>2</sub> ~ N(<b>0</b>,&sigma;<sup>2</sup><sub>e2</sub><b>I</b>)}}{\eqn{\textbf{e}_2\sim N(\textbf{0},\sigma^2_{e_2}\textbf{I})}{u_2 ~ N(0,sigma^2_e2 I)}}.

The genetic covariance 
\ifelse{html}{\out{&sigma;<sup>2</sup><sub>u1,u2</sub>}}{\eqn{\sigma^2_{u_1 u_2}}{sigma^2_u1u2)}}
is estimated from the formula for the variance for the sum of two variables as 

\ifelse{html}{\out{<div style='text-align:center'>&sigma;<sup>2</sup><sub>u1,u2</sub> = 1/2(&sigma;<sup>2</sup><sub>u3</sub> - &sigma;<sup>2</sup><sub>u1</sub> - &sigma;<sup>2</sup><sub>u2</sub>)</div>}}{\deqn{\sigma^2_{u_1 u_2}=\frac{1}{2}(\sigma^2_{u_3}-\sigma^2_{u_1}-\sigma^2_{u_2})}{sigma^2_u1u2 = 1/2 (sigma^2_u3 - sigma^2_u1 - sigma^2_u2)}}

where \ifelse{html}{\out{&sigma;<sup>2</sup><sub>u3</sub>}}{\eqn{\sigma^2_{u_3}}{sigma^2_u3)}}
is the genetic variance of the variable 
\ifelse{html}{\out{<b>y</b><sub>3</sub> = <b>y</b><sub>1</sub> + <b>y</b><sub>2</sub>}}{\eqn{\textbf{y}_3=\textbf{y}_1+\textbf{y}_2}{y_3=y_1+y_2}} that also follows the same model as for 
\ifelse{html}{\out{<b>y</b><sub>1</sub>}}{\eqn{\textbf{y}_1}{y_1}} and
\ifelse{html}{\out{<b>y</b><sub>2</sub>}}{\eqn{\textbf{y}_2}{y_2}}.

Likewise, the environmental covariance 
\ifelse{html}{\out{&sigma;<sup>2</sup><sub>e1,e2</sub>}}{\eqn{\sigma^2_{e_1 e_2}}{sigma^2_e1e2)}}
is estimated as 

\ifelse{html}{\out{<div style='text-align:center'>&sigma;<sup>2</sup><sub>e1,e2</sub> = 1/2(&sigma;<sup>2</sup><sub>e3</sub> - &sigma;<sup>2</sup><sub>e1</sub> - &sigma;<sup>2</sup><sub>e2</sub>)</div>}}{\deqn{\sigma^2_{e_1 e_2}=\frac{1}{2}(\sigma^2_{e_3}-\sigma^2_{e_1}-\sigma^2_{e_2})}{sigma^2_e1e2 = 1/2 (sigma^2_e3 - sigma^2_e1 - sigma^2_e2)}}

where \ifelse{html}{\out{&sigma;<sup>2</sup><sub>e3</sub>}}{\eqn{\sigma^2_{e_3}}{sigma^2_e3)}}
is the error variance of the variable 
\ifelse{html}{\out{<b>y</b><sub>3</sub>}}{\eqn{\textbf{y}_3}{y_3}}.

Solutions are found using the function 'fitBLUP' (see \code{help(fitBLUP)}) to sequentialy fit mixed models for all the variables \ifelse{html}{\out{<b>y</b><sub>1</sub>}}{\eqn{\textbf{y}_1}{y_1}}, \ifelse{html}{\out{<b>y</b><sub>2</sub>}}{\eqn{\textbf{y}_2}{y_2}} and
\ifelse{html}{\out{<b>y</b><sub>3</sub>}}{\eqn{\textbf{y}_3}{y_3}}.
}
\examples{
  require(SFSI)
  data(wheatHTP)
  
  index = which(Y$trial \%in\% 1:10)    # Use only a subset of data
  Y = Y[index,]
  M = scale(M[index,])/sqrt(ncol(M))  # Subset and scale markers
  G = tcrossprod(M)                   # Genomic relationship matrix
  Y0 = scale(as.matrix(Y[,4:7]))      # Response variable
  
  ID_geno = as.vector(row(Y0))
  ID_trait = as.vector(col(Y0))
  y = as.vector(Y0)
  
  # Pairwise covariance for all columns in y
  fm = getGenCov(y=y, K=G, ID_geno=ID_geno, ID_trait=ID_trait)
  fm$varU           # Genetic variances
  fm$varE           # Residual variances
  fm$varU+fm$varE   # Phenotypic covariance
  var(Y0)           # Sample phenotypic covariance
  
  # Covariances between the first and the rest: y[,1] and y[,2:4]
  fm = getGenCov(y=y, K=G, ID_geno=ID_geno, ID_trait=ID_trait, pairwise=FALSE)
  fm$covU           # Genetic covariance between y[,1] and y[,2:4]
  \donttest{
  # Containing some missing values:
  # The model is estimated from common training data
  YNA = Y0
  YNA[Y$trial \%in\% 1:2, 1] = NA
  YNA[Y$trial \%in\% 2:3, 3] = NA
  y = as.vector(YNA)
  
  fm = getGenCov(y=y, K=G, ID_geno=ID_geno, ID_trait=ID_trait)
  fm$varU 
  fm$varE
  }
}
