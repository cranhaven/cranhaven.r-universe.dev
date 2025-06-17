\name{immigration_perceptions}
\alias{immigration_perceptions}
\title{
Sample STM Model
}
\description{
This is an example of a fitted STM object, alongside the raw document data used to fit the model. The model has 20 topics, 341 documents and a 455 word dictionary. For background about the underlying dataset, see \code{\link[stm]{gadarian}}.
}

\usage{data(immigration_perceptions)}

\format{
  A list with the following elements:
  \describe{
    \item{\code{model}}{The STM model object.}
    \item{\code{raw_documents}}{A character vector containing the raw documents.}
    \item{\code{documents_matrix}}{Processed documents in bag-of-words matrix representation.}
  }
}

\seealso{
   \code{\link[stm]{gadarian}}
}