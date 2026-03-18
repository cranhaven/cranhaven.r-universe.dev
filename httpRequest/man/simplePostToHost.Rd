\name{simplePostToHost}
\alias{simplePostToHost}
\title{Sends Simple Post Request to HTTP Server (host)}
\description{
  Sends Post Request to HTTP Server
}
\usage{
        simplePostToHost(host, path, datatosend, referer, contenttype,
                                port=80, maxlen=131063L)
}
\arguments{
  \item{host}{The host to which to send the Request eg.: www.spiegel.de, localhost}
  \item{path}{The path to the file eg. \samp{/cgi-bin/paramlist.cgi} or \samp{/index.html}}
  \item{datatosend}{key value pairs pairs separated by \& \code{"pid=14\&pollvotenumber=2\n4"}}
  \item{referer}{something like www.myhome.org}
  \item{contenttype}{default: application/x-www-form-urlencoded}
  \item{port}{its 80 or 8080 most frequently}
  \item{maxlen}{maximum blocksize read at once}
}
\value{The document which the server returns as a string.}
\details{
It is a simple http client. So it does not take care of special chars e.g. whitspaces.
For details see e.g. perl HTTP::Request::Common documentation
and\cr
\code{http://www.w3.org/Protocols/rfc1341/0_TableOfContents.html}
The output is gathered in several blocks, if necessary. The
parameter maxlen is used to determine the last data block by
its length. The first block with less than maxlen bytes is
considered the last one.
}
\author{E.W.Wolski wolski@molgen.mpg.de, Andreas Westfeld}
\seealso{getToHost, postToHost}
\examples{
host <- "api.scb.se"
path <- "/OV0104/v1/doris/en/ssd"
data <- '{"format":"json"}'
simplePostToHost(host, path, data, contenttype="text/json")
}
