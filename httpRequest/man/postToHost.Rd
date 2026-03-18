\name{postToHost}
\alias{postToHost}
\title{Sends Multiform Post Request to HTTP Server}
\description{
  Sends Multiform Post Request to HTTP Server
}
\usage{
postToHost(host, path, data.to.send, referer, port=80, ua, accept,
accept.language, accept.encoding, accept.charset, contenttype, cookie)
}

\arguments{
        \item{host}{The host to which to send the Request e. g.: www.spiegel.de, 127.0.0.1}
        \item{path}{The path to the file e. g. /cgi/getpasswords.pl or /index.html, default "/"}
        \item{data.to.send}{is a list of name value pairs e. g. list(name=value, name1=value1, name2=value2).  The value may be a list("filename"=X, "object"=Y), where Y is a vector of type raw and X is a name to be posted as the filename of Y.}
        \item{referer}{something like www.myhome.org, default: NULL (not present in header)}
        \item{port}{port to connect to, default 80}
        \item{ua}{Identifier of the user agent, default "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.1.5) Gecko/20070719 Iceweasel/2.0.0.5 (Debian-2.0.0.5-2)"}
        \item{accept}{document types that are accepted by the client, default:\cr
"text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,\cr
image/png,*/*;q=0.5"}
        \item{accept.language}{languages that are preferred by the client, default: "de,de-de;q=0.8,en-us;q=0.5,en;q=0.3"}
        \item{accept.encoding}{compressions that are accepted by the client, default: "gzip,deflate"}
        \item{accept.charset}{charset accepted by the client, default: "ISO-8859-1,utf-8;q=0.7,*;q=0.7"}
        \item{contenttype}{type of data that is sent to the server, default: "application/octet-stream"}
        \item{cookie}{e. g., "SessionID=1008XVG0F1F5D8H94294967295", default: NULL (not present in header)}
}
\value{document which the server returns.}
\details{
It is a simple http client. So it does not take care of special chars e.g. whitspaces.
For details see e.g. perl HTTP::Request::Common documentation
and\cr
\code{http://www.w3.org/Protocols/rfc1341/0_TableOfContents.html}
}
\author{E.W.Wolski wolski@molgen.mpg.de,
Andreas Westfeld andreas.westfeld@htw-dresden.de}
\seealso{getToHost, simplePostToHost}
\examples{

#to test uncomment. First check that the host is running.
#port <- 80
#test2 <- list(
#	"fruit"="apple",
#	"dat_defs"="20021204/F113213.dat",
#	"myimage"=list(
#			"filename"="myimage.raw",
#			"object"=as.raw(as.vector(mymatrix))
#		),
#	"upsa"="test"
#)
#postToHost("www.molgen.mpg.de", "/~wolski/test.php4", test2,
#        referer="www.test.de", port=port)
}

