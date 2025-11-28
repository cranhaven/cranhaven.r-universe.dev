\name{Boxplots}
\alias{Boxplots}

\title{Grouped boxplots}
\description{Boxplots for every scaled variable grouped by factor}

\usage{Boxplots(vars, groups, boxcols=Pastels, legpos="topleft", srt=45, adj=1,
 slty=3, yticks=FALSE, ymarks=FALSE, ...)}

\arguments{
\item{vars}{data frame consists of variables to plot}
\item{groups}{grouping factor}
\item{boxcols}{colors of character boxes, default is 'Pastels', i.e. c("white", "lightblue", "mistyrose", "lightcyan", "lavender", "cornsilk")}
\item{legpos}{where to place automatic legend, default is 'topleft', for no legend use 'legpos=NA'}
\item{slty}{line type to delimit groups of boxes}
\item{srt, adj, yticks, ymarks}{regular 'plot()' arguments}
\item{...}{additional arguments to 'boxplot()'}
}

\details{There are many ways to represent groups in data. One is trellis plots. 'Boxplots()' make grouped plots which fit the plot box linearly and therefore easy to compare. So the main idea for grouped plots is to make comparison easier.

Please note that because characters within group are likely of different nature, they are scaled. Consequently, tick marks are removed as they have no sense.

Alternatives: trellis designs.
}

\value{For the efficiency reasons, the function does not return anything.}

% \references{}

\author{Alexey Shipunov}

\seealso{\code{\link{boxplot}}, \code{\link{Linechart}}, \code{\link{Dotchart3}}}

\examples{
Trees <- trees
Trees[, 4] <- sample(letters[1:3], nrow(Trees), replace=TRUE)
Boxplots(Trees[, 1:3], factor(Trees[, 4]), srt=0, adj=c(.5, 1)) # horizontal labels

sp <- Recode(eq_s$N.POP, eq_l$N.POP, eq_l$SPECIES)
eq <- cbind(sp=as.factor(sp), eq_s[, -1])
eq3 <- eq[eq$sp \%in\% levels(eq$sp)[1:3], ]
Boxplots(eq3[, 2:9], eq3[, 1], boxcols=grey(1:3/3), slty=0) # no border lines
}

\keyword{hplot}
