\name{getToHost2}
\alias{getToHost2}
\title{Sends Get Request to HTTP Server (host) with raw contents}
\description{
  Sends Get Request to HTTP Server (experimental version of getToHost based on raw data type)
}
\usage{
getToHost2(host,path,referer,port=80)
}
\arguments{
\item{host}{The host to which to send the Request e. g.: www.spiegel.de, 127.0.0.1}
\item{path}{The path to the file eg. /cgi/getpasswords.pl?test=best or /index.html}
\item{referer}{something like www.myhome.org}
\item{port}{its 80 or 8080 most frequently}
}
\value{The document which the server returns as a string.}
\details{
It is a simple http client. So it does not take care of special chars e.g. whitspaces.
For details see e.g. perl HTTP::Request::Common documentation
and\cr
\code{http://www.w3.org/Protocols/rfc1341/0_TableOfContents.html}
}
\author{E.W.Wolski wolski@molgen.mpg.de}
\seealso{getToHost}
\examples{
#check first if www.molgen.mpg.de is running.
#port <- 80
#getToHost2("www.molgen.mpg.de",
#        "/~wolski/test.php4?test=test1&test2=test2&test3=3",
#        "www.test.pl", port=port)
}
