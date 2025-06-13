\name{surv.colors}
\alias{surv.colors}
\title{
  Produces visual representation of survival data
}

\description{
  This function generates color shades for each individual, according to their respective right-censored survival data (event occurred or not, after which follow-up time). This can prove useful to annotate heat maps with survival data.
  
  Two color scales are used, one for right-censored individuals (lost of sight before the event occurs, yellow with default colors) and an other for individual with observed events (death, relapse ... black in default colors). Shades are generated according to their impact : fast events and long follow-ups without event have strong colors, while late events and short follow-up without event are light-colored.
}

\usage{
  surv.colors(time, event, eventColors = c("#000000", "#CCCCCC"),
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

\value{
  Returns a character vector, named according to \code{time} names.
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{surv.scale}}, \code{\link{heat.map}}
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
  
  # Color scale legend
  with(rosenwald.cli, surv.scale(time=follow.up, event=status=="Dead"))
  
  # Annotated clustering
  side <- data.frame(OS=surv, row.names=rownames(rosenwald.cli))
  clusterize(expr, side=side)
}

