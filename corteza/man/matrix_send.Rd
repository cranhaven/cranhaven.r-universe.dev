% tinyrox says don't edit this manually, but it can't stop you!
\name{matrix_send}
\alias{matrix_send}
\title{Deprecated: use bot_send()}
\usage{
matrix_send(text, room_id = NULL, msgtype = "m.text", markdown = FALSE)
}
\arguments{
\item{text}{Character. Plain text body.}

\item{room_id}{Character. Matrix room id. Defaults to \code{cfg$room_id}
from the saved Matrix config (see \code{\link{bot_configure}}).}

\item{msgtype}{Character. Matrix msgtype, default "m.text".}

\item{markdown}{Logical. If TRUE, also send Matrix custom HTML derived
from a conservative markdown subset.}
}
\value{
See \code{\link{bot_send}}.
}
\description{
Deprecated: use bot_send()
}
\examples{
\dontrun{
matrix_send("hello")  # use bot_send()
}
}
\seealso{
\code{\link{bot_send}}
}
