\name{loadcsv_multi}
\alias{loadcsv_multi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
read multiple csv files into named data frames
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Reads multiple files in table format and creates a data frame from them, with cases corresponding to lines and variables to fields in the file.
}
\usage{
loadcsv_multi(directory = NULL,
                          extension = "CSV",
                          encoding = "Latin-1",
                          stringsAsFactors = FALSE,
                          header = TRUE,
                          quote = "\"",
                          fill = TRUE,
                          comment.char = "")


}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{directory}{
%%     ~~Describe \code{x} here~~
a directory to load the files from, if NULL then a manual choice is provided on windows OS.
}
  \item{extension}{
  logical. if TRUE .txt files will be loaded as tables instead of .csv.
  }
  \item{encoding}{
  character. files encoding. default is Latin-1
  }
  \item{stringsAsFactors}{
  logical: should character vectors be converted to factors? Note that this is overridden by as.is and colClasses, both of which allow finer control.
  }
  \item{header}{
  a logical value indicating whether the files contain the names of the variables as its first line. If missing, the value is determined from the file format: header is set to TRUE if and only if the first row contains one fewer field than the number of columns.
  }
  \item{quote}{
  the set of quoting characters. To disable quoting altogether, use quote = "". See scan for the behavior on quotes embedded in quotes. Quoting is only considered for columns read as character, which is all of them unless colClasses is specified.
  }
  \item{fill}{
  logical. If TRUE then in case the rows have unequal length, blank fields are implicitly added.
  }
  \item{comment.char}{
  character: a character vector of length one containing a single character or an empty string. Use "" to turn off the interpretation of comments altogether.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
loadcsv_multi is used for uncompressed files in a single folder.it can be used either by entering the local directory the files are in, or just running it with no arguments for manual folder selection on windows OS.
It receives some arguments from read.csv and they are listed in the arguments section.
loadcsvfromZIP is used for comma separated tables inside of a .zip file.
loadZIPcsvfromURL is used for comma separated tables inside of a .zip file on the internet, no download needed.
}
\value{
A \link[base]{data.frame} containing a representation of the data in the file.
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\link[easycsv]{loadZIPcsvfromURL}
\link[easycsv]{loadcsvfromZIP}
}
\examples{
  require(easycsv)
  directory = getwd()
  table1 <- data.frame(matrix(1:9, nrow = 3))
  write.csv(table1, file = file.path(directory,"/table1.csv"))
  write.csv(table1, file = file.path(directory,"/table2.txt"))
  loadcsv_multi(directory, extension = "BOTH")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~utilities }
\keyword{ ~misc }
