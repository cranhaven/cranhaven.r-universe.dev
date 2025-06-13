\name{LPS}
\alias{LPS}
\alias{LPS-class}
\title{
  Linear Predictor Score fitting
}

\description{
  This function trains a Linear Predictor Score model, given pre-computed coefficients. It uses data with known classes to fit the model.
  
  It has numerous way to be called, and all the arguments are not mandatory. See the 'Examples' section.
}

\usage{
  LPS(data, coeff, response, k, threshold, formula, method = "fdr", ...)
}

\arguments{
  \item{data}{
    Continuous data used to retrieve classes, as a \code{data.frame} or \code{matrix}, with samples in rows and features (genes) in columns. Rows and columns should be named. \strong{Some precautions must be taken concerning data normalization, see the corresponding section below}.
}
  \item{coeff}{
    Pre-computed coefficients for the model, as returned by \code{\link{LPS.coeff}} (see there for format details).
}
  \item{response}{
    Already known classes for the samples provided in \code{data}, preferably as a two-level \code{factor}. Can be missing if a \code{formula} with a response element is provided, but this argument precedes.
}
  \item{k}{
    Single \code{integer} value, amount of features to include in the model, in decreasing order of coefficient. Can be missing if \code{threshold} or \code{formula} are provided, but this argument precedes other both of them.
}
  \item{threshold}{
    Single \code{numeric} value, p-value threshold to apply for feature selection. Can be missing if \code{k} or \code{formula} are provided, but \code{k} precedes on it and it precedes on \code{formula}.
}
  \item{formula}{
    A \code{formula} object, describing the model to fit (several templates are handled, see 'Examples'). The formula response element (before the "~" sign) can replace the \code{response} argument if it is not provided. The variables (after the "~" sign) can be a single integer (standing for the \code{k} argument), a single numeric (standing for the \code{threshold} argument) or a sum of feature names to use directly. "." is also handled in the usual way (all \code{data} columns), and "1" is a more efficient way to refer to all numeric columns of \code{data}.
}
  \item{method}{
    Single character value, to be passed to \code{\link{p.adjust}} when \code{threshold} is provided.
}
  \item{\dots}{
    Further arguments are passed to \code{\link{model.frame}} if \code{response} is missing (thus defined via \code{formula}). \code{subset} and \code{na.action} may be particularly useful for cross-validation schemes, see \code{\link{model.frame.default}} for details. \code{subset} is always handled but masked in "..." for compatibility reasons.
}
}

\value{
  An object of (S3) class "LPS" :
  \item{coeff}{Named numeric vector, the coefficients used in the model.}
  \item{classes}{Character vector, the labels of the two groups to be predicted.}
  \item{scores}{List of two numeric vectors, training dataset scores sorted by group.}
  \item{means}{Numeric vector, score means of each group in the training dataset.}
  \item{sds}{Numeric vector, score \code{\link{sd}} of each group in the training dataset.}
  \item{ovl}{Numeric value, overlapping coefficient as returned by \code{\link{OVL}}.}
  \item{k}{Integer value, amount of features selected in the model (if relevant).}
  \item{p.threshold}{Numeric value, threshold used for feature selection (if relevant).}
  \item{p.method}{Character value, p-value correction used for feature selection (if relevant).}
}

\references{
  Radmacher MD, McShane LM, Simon R. \cite{A paradigm for class prediction using gene expression profiles.} J Comput Biol. 2002;9(3):505-11.

  Wright G, Tan B, Rosenwald A, Hurt EH, Wiestner A, Staudt LM. \cite{A gene expression-based method to diagnose clinically distinct subgroups of diffuse large B cell lymphoma.} Proc Natl Acad Sci U S A. 2003 Aug 19;100(17):9991-6.
  
  Bohers E, Mareschal S, Bouzelfen A, Marchand V, Ruminy P, Maingonnat C, Menard AL, Etancelin P, Bertrand P, Dubois S, Alcantara M, Bastard C, Tilly H, Jardin F. \cite{Targetable activating mutations are very frequent in GCB and ABC diffuse large B-cell lymphoma.} Genes Chromosomes Cancer. 2014 Feb;53(2):144-53.  
}

\author{
  Sylvain Mareschal
}

\section{Normalization}{
  As expression values are directly used in the score, gene centering and scaling are strongly recommended. For Affymetrix raw expression values (strictly positive, linear and absolute), Wright et al. suggests a multiplicative centering on a median of 1000 followed by a log2 transformation. For log-ratio, gene centering and scaling should not be necessary, as they are naturally 0-centered.
}

\section{Time efficiency}{
  Using a numeric matrix as \code{data} and a factor as \code{response} is the fastest way to compute coefficients, if time consumption matters (as in cross-validation schemes). \code{formula} is there only for consistency with R modeling functions, and to provide \code{response}, \code{k} or \code{threshold} in a single way.
}

\seealso{
  \code{\link{LPS.coeff}}
}

\examples{
  # Data with features in columns
  data(rosenwald)
  group <- rosenwald.cli$group
  expr <- t(rosenwald.expr)
  
  # NA imputation (feature's mean to minimize impact)
  f <- function(x) { x[ is.na(x) ] <- round(mean(x, na.rm=TRUE), 3); x }
  expr <- apply(expr, 2, f)
  
  # Coefficients
  coeff <- LPS.coeff(data=expr, response=group)
  
  
  # 10 best features (straightforward)
  m <- LPS(data=expr, coeff=coeff, response=group, k=10)
  
  # 10 best features (formula)
  ### 'k' MUST be an integer, or will be understood as a 'threshold'
  ### Numbers are "numeric", enforce integer with "L" or "as.integer"
  m <- LPS(data=as.data.frame(expr), coeff=coeff, formula=group~10L)
  k <- as.integer(10)
  m <- LPS(data=as.data.frame(expr), coeff=coeff, formula=group~k)
  
  # FDR threshold
  thr <- 0.01
  m <- LPS(data=expr, coeff=coeff, response=group, threshold=thr)
  m <- LPS(data=as.data.frame(expr), coeff=coeff, formula=group~0.01)
  m <- LPS(data=as.data.frame(expr), coeff=coeff, formula=group~thr)
  
  # Custom model
  m <- LPS(data=expr, coeff=coeff[ c("27481","17013") ,], response=group, k=2)
  m <- LPS(data=as.data.frame(expr), coeff=coeff, formula=group~`27481`+`17013`)
  ### Notice backticks in formula for syntactically invalid names
  
  # Complete model
  m <- LPS(data=expr, coeff=coeff, response=group, k=ncol(expr))
  m <- LPS(data=expr, coeff=coeff, response=group, threshold=1)
  ### m <- LPS(data=as.data.frame(expr), coeff=coeff, formula=group~.)
  ### The last is correct but (really) slow on large datasets
}
