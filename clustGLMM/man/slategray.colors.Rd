\name{slategray.colors}
\alias{slategray.colors}
\alias{slategrey.colors}
\title{
Slategray Color Palette
}
\description{
Creates a vector of \code{n} slategray colors.
}
\usage{
slategray.colors(n, start = 0.3, end = 0.9)
slategrey.colors(n, start = 0.3, end = 0.9)
}
\arguments{
  \item{n}{
the number of slategray colors to be in the palette.
}
  \item{start}{
starting slategray level in the palette (should be between 0 and 1 where zero indicates "slategray4" and one indicates "slategray1").
}
  \item{end}{
ending gray level in the palette.
}
}
\details{
The function \code{slategray.colors} chooses a series of \code{n} slategray levels between \code{start} and \code{end} by interpolating 0=\code{slategray4}, 1/3=\code{slategray3}, 2/3=\code{slategray2}, \code{slategray1} = 1. The returned palette contains the corresponding gray colors. This palette is used in \code{\link{plot_cat_vs_x_grouped}} for levels of group 0.

\code{slategrey.colors} is an alias for \code{slategray.colors}.
}
\value{
A vector of \code{n} slategray colors.
}
\author{
Jan Vávra
}
\examples{
slategray.colors(5, start = 0.3, end = 0.9)
slategrey.colors(5, start = 0.1, end = 0.7)
}
