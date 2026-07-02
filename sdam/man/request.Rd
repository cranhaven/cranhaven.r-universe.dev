\name{request}
\alias{request}
\alias{sddk}
\alias{SDDK}
\title{
Perform an HTTP request
}
\description{
A function to perform an HTTP request to \emph{\url{https://sciencedata.dk}} or other server.
}
\usage{
request(file, URL = "https://sciencedata.dk", method = c("GET", "POST", "PUT", "DELETE"), 
        anonymous = FALSE, cred = NULL, path = "/files", subdomain = NULL, force = FALSE, 
        rm.file, ...)
}

\arguments{
  \item{file}{
   the requested file
}
  \item{URL}{
   protocol and domain of the url
}
  \item{method}{
   the http verb for the object
}
  \item{anonymous}{
    unauthenticated user?
}
  \item{cred}{
   vector with username and password as authentication credentials
}
  \item{path}{
   path to add to the url (optional)
}
  \item{subdomain}{
   subdomain to add to the url (optional)
}
  \item{force}{
   force remote file overwriting? (optional and logical)
}
  \item{rm.file}{
   remove file in local machine? (optional and logical)
}
  \item{\dots}{
   further parameters if required
}
}
\details{
\code{request} is an HTTP request, first aimed to interact with DEiC's (Danish e-Infrastructure Cooperation) RESTful APIs at 
\emph{\url{https://sciencedata.dk}}; however, it is possible to specify the URL path and subdomain if necessary. 

DEiC's \emph{\url{https://sciencedata.dk}} servers have different types of folders and both \emph{personal} and \emph{shared} 
folders require authentication with credentials.

The \emph{path} to the shared folders where the files are located must be specified with the \code{path} argument. 
However, for personal folders the \code{file} argument that includes the path information. Many times, DEiC's \emph{\url{https://sciencedata.dk}} places the data on a \emph{subdomain}, and for some methods like \code{PUT} it is required to specify the subdomain as well.

When a file already exists on the remote server, there is a prompt question for overwriting the file when the \code{PUT} method is invoked, and by activating argument \code{force} we can prevent confirmation and replace the file.
Method \code{POST} is not yet supported.

In case that accessing the server requires basic authentication, then package "\code{\link{tcltk}}" is required to input the credentials with a widget prompt. 
However, there is a \code{cred} argument for performing a basic authentication without a prompt, and public folders 
can be accessed without credentials with the option of \code{anonymous} user.
}
\value{
Depends on the method, an action on the server site. 

A \emph{Response} message is returned when the method is \code{PUT} with the URL and items \code{Date}, \code{Status}, \code{Content-Type}. 
}
\references{
https://sciencedata.dk/sites/developer/ (retrieved on January 2020)

\url{https://sciencedata.dk}

\url{https://www.deic.dk/}
}
\author{
Antonio Rivero Ostoic
}
\note{
This function requires "\code{\link[httr]{httr}}", and aliases for \code{request} are \code{sddk()} and \code{SDDK()}.
}

%\section{Warning}{
%}

\seealso{
\code{\link[httr]{httr}}, \code{\link{tcltk}}
}

\examples{
## get a public file from remote server as anonymous user
\dontrun{
request("filename.extension", method="GET", anonymous=TRUE)}

## put a file in remote server
\dontrun{
request("filename.extension", method="PUT")}

## put an existing file in remote server and force overwriting
\dontrun{
request("filename.extension", method="PUT", force=TRUE)}

## put an existing file in remote server and remove file from local machine
\dontrun{
request("filename.extension", method="PUT", rm.file=TRUE)}


## remove a file in remote server
\dontrun{
request("filename.extension", method="DELETE")}

}
\keyword{ IO }
\keyword{ utilities }
