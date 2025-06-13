\name{surv.scale}
\alias{surv.scale}
\title{
  Plots a survival color scale, for legend
}

\description{
  This function plots a color scale using a custom color palette, to legend \code{\link{surv.colors}} annotations.
}

\usage{
  surv.scale(time, event, eventColors = c("#000000", "#CCCCCC"),
    censColors = c("#FFFFEE", "#FFDD00"))
}

\arguments{
  \item{time}{
    Numeric vector, the follow-up times of each individual (see \code{\link{Surv}} in the \code{survival} package).
}
  \item{event}{
    Logical vector, whether an event (death, relapse ...) occured at the end of each individual follow-up or not (see \code{\link{Surv}} in the \code{survival} package).
}
  \item{eventColors}{
    Character vector of length 2, the boundaries of the color scale to generate for individuals with events.
}
  \item{censColors}{
    Character vector of length 2, the boundaries of the color scale to generate for right-censored individuals.
}
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{surv.colors}}, survival::\code{\link{Surv}}
}

\examples{
  # Rosenwald's dataset (hand-picked prognostic probes)
  data(rosenwald)
  probes <- c("30580", "16006", "32315", "16978", "26588")
  expr <- t(rosenwald.expr[ probes ,])
  
  # NA imputation (feature's mean to minimize impact)
  f <- function(x) { x[ is.na(x) ] <- round(mean(x, na.rm=TRUE), 3); x }
  expr <- apply(expr, 2, f)
  
  # Survival colors
  surv <- with(rosenwald.cli, surv.colors(time=follow.up, event=status=="Dead"))
  
  # Annotated clustering
  side <- data.frame(OS=surv, row.names=rownames(rosenwald.cli))
  clusterize(expr, side=side)
  
  # Color scale legend
  with(rosenwald.cli, surv.scale(time=follow.up, event=status=="Dead"))
}

