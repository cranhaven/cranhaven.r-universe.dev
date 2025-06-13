\name{LPS.coeff}
\alias{LPS.coeff}
\title{
  Linear Predictor Score coefficient computation
}

\description{
  As Linear Predictor Score coefficients are genuinely t statistics, this function provides a faster implementation for large datasets than using \code{\link{t.test}}.
}

\usage{
  LPS.coeff(data, response, formula = ~1, type = c("t", "limma"),
    p.value = TRUE, log = FALSE, weighted = FALSE, ...)
}

\arguments{
  \item{data}{
    Continuous data used to retrieve classes, as a \code{data.frame} or \code{matrix}, with samples in rows and features (genes) in columns. Rows and columns should be named. \code{NA} values are silently ignored. \strong{Some precautions must be taken concerning data normalization, see the corresponding section in \code{\link{LPS}} manual page}.
}
  \item{response}{
    Already known classes for the samples provided in \code{data}, preferably as a two-level \code{factor}. Can be missing if a \code{formula} with a response element is provided, but this argument precedes.
}
  \item{formula}{
    A \code{formula} object, describing the features to consider in \code{data}. The formula response element (before the "~" sign) can replace the \code{response} argument if it is not provided. The features can be enumerated in the variable section of the formula (after the "~" sign). "." is also handled in the usual way (all \code{data} columns), and "1" is a more efficient way to refer to all numeric columns of \code{data}.
}
  \item{type}{
    Single character value, "t" to compute genuine t statistics (unequal variances and unpaired samples) or "limma" to use the lmFit() and eBayes() t statistics from this microarray oriented Bioconductor package.
}
  \item{p.value}{
    Single logical value, whether to compute (two-sided) p-values or not.
}
  \item{log}{
    Single logical value, whether to log-transform t or not (sign will be preserved). Original description of the LPS does not include log-transformation, but it may be useful to not over-weight discriminant genes in large series. Values between -1 and 1 are transformed to 0 to avoid sign shifting, as it generally comes with non significant p-values.
}
  \item{weighted}{
    Single logical value, whether to divide t (or log-transformed t) by gene mean or not. We recommend to normalize data only by samples and use \code{weighted = TRUE} to include gene centering in the model, rather than centering and scaling genes by normalizing independantly each series as Wright et al. did.
}
  \item{\dots}{
    Further arguments are passed to \code{\link{model.frame}} if \code{response} is missing (thus defined via \code{formula}). \code{subset} and \code{na.action} may be particularly useful for cross-validation schemes, see \code{\link{model.frame.default}} for details. \code{subset} is always handled but masked in "..." for compatibility reasons.
}
}

\value{
  Always returns a row named numeric matrix, with a "t" column holding statistics computed. If \code{p.value} is TRUE, a second "p.value" column is added.
}

\references{
  \url{http://www.bioconductor.org/packages/release/bioc/html/limma.html}
}

\author{
  Sylvain Mareschal
}

\note{
  Using a numeric matrix as \code{data} and a factor as \code{response} is the fastest way to compute coefficients, if time consumption matters (as in cross-validation schemes). \code{formula} was added only for consistency with other R modeling functions, and eventually to subset features to compute coefficients for.
}

\seealso{
  \code{\link{LPS}}
}

\examples{
  # Data with features in columns
  data(rosenwald)
  group <- rosenwald.cli$group
  expr <- t(rosenwald.expr)
  
  
  # All features, all samples
  k <- LPS.coeff(data=expr, response=group)
  k <- LPS.coeff(formula=group~1, data=as.data.frame(expr))
  ### LPS.coeff(formula=group~., data=as.data.frame(expr), na.action=na.pass)
  ### The last is correct but (really) slow on large datasets
  
  # Feature subset, all samples
  k <- LPS.coeff(data=expr[, c("27481","17013") ], response=group)
  k <- LPS.coeff(formula=group~`27481`+`17013`, data=as.data.frame(expr))
  ### Notice backticks in formula for syntactically invalid names
  
  # All features, sample subset
  training <- rosenwald.cli$set == "Training"
  ### training <- sample.int(nrow(expr), 10)
  ### training <- which(rosenwald.cli$set == "Training")
  ### training <- rownames(subset(rosenwald.cli, set == "Training"))
  k <- LPS.coeff(data=expr, response=group, subset=training)
  k <- LPS.coeff(formula=group~1, data=as.data.frame(expr), subset=training)

  # NA handling by model.frame()
  k <- LPS.coeff(formula=group~1, data=as.data.frame(expr), na.action=na.omit)
}
