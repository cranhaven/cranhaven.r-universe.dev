\name{plaid.grid}
\alias{plaid.grid}

\title{
Parameter Grid for BCPlaid Biclustering
}
\description{
Generates a list containing parameter settings for the ensemble algorithm. 
}

\usage{
plaid.grid(method = "BCPlaid", cluster = "b", fit.model = y ~ m + a + b, 
  background = TRUE, background.layer = NA, background.df = 1, 
  row.release = c(0.5, 0.6, 0.7), col.release = c(0.5, 0.6, 0.7), 
  shuffle = 3, back.fit = 0, max.layers = 20, iter.startup = 5, 
  iter.layer = 10, verbose = FALSE)
}

\arguments{
  \item{method}{Here BCPlaid, to perform Plaid algorithm}
  \item{cluster}{'r', 'c' or 'b', to cluster rows, columns or both (default 'b')}
  \item{fit.model}{Model (formula) to fit each layer. Usually, a linear model is used, that
   estimates three parameters: m (constant for all elements in the bicluster),
   a(contant for all rows in the bicluster) and b (constant for all columns).
   Thus, default is: y ~ m + a + b.}
  \item{background}{If 'TRUE' the method will consider that a background layer
  (constant for all rows and columns) is present in the data matrix.}
  \item{background.layer}{If background='TRUE' a own background layer
  (Matrix with dimension of x) can be specified.}
  \item{background.df}{Degrees of Freedom of backround layer if background.layer is specified.}
\item{shuffle}{Before a layer is added, it's statistical significance is compared
    against a number of layers obtained by random defined by this parameter. Default is
    3, higher numbers could affect time performance. }
  \item{iter.startup}{Number of iterations to find starting values }
  \item{iter.layer}{Number of iterations to find each layer }
  \item{back.fit}{After a layer is added, additional iterations can be done to
    refine the fitting of the layer (default set to 0)}
  \item{row.release}{Scalar in [0,1](with interval recommended [0.5-0.7]) used as threshold to prune rows in the layers
    depending on row homogeneity}
  \item{col.release}{As above, with columns }
  \item{max.layers}{Maximum number of layer to include in the model }
  \item{verbose}{If 'TRUE' prints extra information on progress.}
}

\value{
A list containing parameter settings
}

\author{Sebastian Kaiser 
\email{sebastian.kaiser@stat.uni-muenchen.de}
}

\seealso{
\code{\link{ensemble}}, \code{\link{BCPlaid}}
}
\examples{
plaid.grid()
}
\keyword{ cluster }