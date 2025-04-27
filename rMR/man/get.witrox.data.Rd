\name{get.witrox.data}
\alias{get.witrox.data}

\title{
Load Data from 'AutoResp' Software Generated .txt Files
}
\description{
Allows user to import data from Loligo (R) Systems' 'Autoresp' software-generated text files into a R data.frame (class \code{data.frame})
}
\usage{
get.witrox.data(data.name, lines.skip, delimit = "tab", 
choose.names = F, chosen.names = NULL, 
format)
}

\arguments{
  \item{data.name}{
Full data file name as character string.
  }
  \item{lines.skip}{
The lines in the header to be skipped. If \code{choose.names = FALSE}, then skip all lines up to the column names. If \code{choose.names =TRUE}, skip all lines including column names.
}
  \item{delimit}{
Choose the delimiter. Defaults to tab delimited. Can take values of \code{"tab"}, \code{"space"}, or \code{"comma"}. If importing from an excel file, save the file as a .csv file, then use the delimiter argument \code{"comma"}
}
  \item{choose.names}{
logical: if \code{FALSE}, then names are automatically derived from the names of the text file. Sometimes, this can be a problem if there are tabs or commas included in odd places in the column name line of the text file. If \code{TRUE}, user must specify a vector of column names--see \code{lines.skip} and \code{chosen.names}. 
}
  \item{chosen.names}{
If \code{choose.names = TRUE}, chosen.names must be a vector of character strings for use as \code{column.names}.
}
  \item{format}{
This is the format that the date-time column is formatted in by auto resp--This must be the FIRST COLUMN. The default format is \code{"\%d/\%m/\%Y \%I:\%M:\%S \%p"}. Another common format is \code{"\%d/\%m/\%Y/\%I:\%M:\%S \%p"}. See \code{\link{strptime}} for more directions on formatting the date and time.
}
}
\value{
Returns an object of class data.frame, with \code{std.time} as the last column, which is in the default standard \code{POSIXct} date-time format.
}
\author{
Tyler L. Moulton
}
\seealso{
\code{\link{strptime}}, 
\code{\link{as.POSIXct}},
}
\examples{
# Requires a text file. Download fish_MR.txt from github repository and
# accompanying readme file at:
# https://github.com/tyler-l-moulton/rMR

}
