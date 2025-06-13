\name{predict.LPS}
\alias{predict.LPS}
\title{
  Predict method for LPS objects
}

\description{
  This function allow predictions to be made from a fitted \code{\link{LPS}} model and a new dataset.
  
  It can also plot a gene expression heatmap to visualize results of the prediction.
}

\usage{
  \method{predict}{LPS}(object, newdata, type=c("class", "probability", "score"),
    method = c("Wright", "Radmacher", "exact"), threshold = 0.9, na.rm = TRUE,
    subset = NULL, col.lines = "#FFFFFF", col.classes = c("#FFCC00", "#1144CC"),
    plot = FALSE, side = NULL, cex.col = NA, cex.row = NA, mai.left = NA,
    mai.bottom = NA, mai.right = 1, mai.top = 0.1, side.height = 1, side.col = NULL,
    col.heatmap = heat(), zlim = "0 centered", norm = c("rows", "columns", "none"),
    norm.robust = FALSE, customLayout = FALSE, getLayout = FALSE, ...)
}

\arguments{
  \item{object}{
    An object of class \code{"LPS"}, as returned by \code{\link{LPS}}.
}
  \item{newdata}{
    Continuous data used to retrieve classes, as a \code{data.frame} or \code{matrix}, with samples in rows and features (genes) in columns. Rows and columns should be named. It can also be a named numeric vector of already computed scores. \strong{Some precautions must be taken concerning data normalization, see the corresponding section in \code{\link{LPS}} manual page}.
}
  \item{type}{
    Single character value, return type of the predictions to be made ("class", "probability" or "score"). See 'Value' section.
}
  \item{method}{
    Single character value, the method to use to make predictions ("Wright", "Radmacher" or "exact"). See 'Details' section.
}
  \item{threshold}{
    Threshold to use for class prediction. "Wright" method was designed with 0.9, "Radmacher" method makes no use of the threshold.
}
  \item{na.rm}{
    Single logical value, if TRUE samples with one or many \code{NA} features will be scored too (concerned feature is removed for the concerned sample, which might be discutable).
}
  \item{subset}{
    A subsetting vector to apply on \code{newdata} rows. See \code{\link{[}} for handled values.
}
  \item{col.lines}{
    If \code{graph} is TRUE, a single character value to be used for line drawing on the heatmap.
}
  \item{col.classes}{
    If \code{graph} is TRUE, a character vector of two values giving to each class a distinct color.
}
  \item{plot}{
    To be passed to \code{\link{heat.map}}.
}
  \item{side}{
    To be passed to \code{\link{heat.map}}.
}
  \item{cex.col}{
    To be passed to \code{\link{heat.map}}.
}
  \item{cex.row}{
    To be passed to \code{\link{heat.map}}.
}
  \item{mai.left}{
    To be passed to \code{\link{heat.map}}.
}
  \item{mai.bottom}{
    To be passed to \code{\link{heat.map}}.
}
  \item{mai.right}{
    To be passed to \code{\link{heat.map}} (used to plot score coefficients).
}
  \item{mai.top}{
    To be passed to \code{\link{heat.map}}.
}
  \item{side.height}{
    To be passed to \code{\link{heat.map}}.
}
  \item{side.col}{
    To be passed to \code{\link{heat.map}}.
}
  \item{col.heatmap}{
    To be passed to \code{\link{heat.map}}.
}
  \item{zlim}{
    To be passed to \code{\link{heat.map}}.
}
  \item{norm}{
    To be passed to \code{\link{heat.map}}.
}
  \item{norm.robust}{
    To be passed to \code{\link{heat.map}}.
}
  \item{customLayout}{
    To be passed to \code{\link{heat.map}}.
}
  \item{getLayout}{
    To be passed to \code{\link{heat.map}}.
}
  \item{\dots}{
    Ignored, just there to match the \code{\link{predict}} generic function.
}
}

\details{
  The "Compound covariate predictor" from Radmacher et al. (\code{method} = "Radmacher") simply assign each sample to the closest group (comparing the sample score to the mean scores of each group in the training dataset).
  
  The "Linear Predictor Score" from Wright et al. (\code{method} = "Wright") modelizes scores in each training sub-group with a distinct gaussian distribution, and computes the probability for a sample to be in one of them or the other using a bayesian rule.
  
  The "exact" mode is still under development and should not be used.
}

\value{
  For a "class" \code{type}, returns a character vector with group assignment for each new sample (possibly \code{NA}), named according to \code{data} row names.
  
  For a "probability" \code{type}, returns a numeric matrix with two columns (probabilities to be in each group) and a row for each new sample, row named according to \code{data} row names and column named according to the group labels.
  
  For a "score" \code{type}, returns a numeric vector with LPS score for each new sample, named according to \code{data} row names. Notice the score is the same for all \code{method}s.
  
  If \code{plot} is \code{TRUE}, returns the list returned by \code{\link{heat.map}}, with data described above in the first unammed element.
}

\references{
  Radmacher MD, McShane LM, Simon R. \cite{A paradigm for class prediction using gene expression profiles.} J Comput Biol. 2002;9(3):505-11.

  Wright G, Tan B, Rosenwald A, Hurt EH, Wiestner A, Staudt LM. \cite{A gene expression-based method to diagnose clinically distinct subgroups of diffuse large B cell lymphoma.} Proc Natl Acad Sci U S A. 2003 Aug 19;100(17):9991-6.
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{LPS}}
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
  
  # 10 best features model
  m <- LPS(data=expr, coeff=coeff, response=group, k=10)
  
  
  # Class prediction plot
  predict(m, expr, plot=TRUE)
  
  # Wright et al. class prediction
  table(
    group,
    prediction = predict(m, expr),
    exclude = NULL
  )
  
  # More stringent threshold
  table(
    group,
    prediction = predict(m, expr, threshold=0.99),
    exclude = NULL
  )
  
  # Radmacher et al. class prediction
  table(
    group,
    prediction = predict(m, expr, method="Radmacher"),
    exclude = NULL
  )
  
  # Probabilities
  predict(m, expr, type="probability", method="Wright")
  predict(m, expr, type="probability", method="Radmacher")
  predict(m, expr, type="probability", method="exact")
  
  # Probability plot
  predict(m, expr, type="probability", plot=TRUE)
  
  # Annotated probability plot
  side <- data.frame(group, row.names=rownames(expr))
  predict(m, expr, side=side, type="probability", plot=TRUE)
  
  # Score plot
  predict(m, expr, type="score", plot=TRUE)
}

