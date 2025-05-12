\name{loadZIPcsvfromURL}
\alias{loadZIPcsvfromURL}
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

loadZIPcsvfromURL(urlAddress = NULL,
                              txt = FALSE ,
                           encoding = "Latin-1",
                           stringsAsFactors = FALSE,
                           header = TRUE,
                           quote = "\"",
                           fill = TRUE,
                           comment.char = "")

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{urlAddress}{
  a URL address of the zipped file, if NULL then Israeli GTFS will be downloaded and added to env.
  }
  \item{txt}{
  logical. if TRUE .txt files will be loaded as tables instead of .csv
  }
  \item{encoding}{
  character. files encoding.default is Latin-1
  }
  \item{stringsAsFactors}{
  logical: should character vectors be converted to factors? Note that this is overridden by as.is and colClasses, both of which allow finer control.
  }
  \item{header}{
  a logical value indicating whether the files contain the names of the variables as its first line. If missing, the value is determined from the file format: header is set to TRUE if and only if the first row contains one fewer field than the number of columns.
  }
  \item{quote}{
  the set of quoting characters. To disable quoting altogether, use quote = "". See scan for the behaviour on quotes embedded in quotes. Quoting is only considered for columns read as character, which is all of them unless colClasses is specified.
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
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A \link[base]{data.frame} containing a representation of the data in the file.
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\link[easycsv]{loadcsvfromZIP}
\link[easycsv]{loadcsv_multi}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~utilities }
\keyword{ ~misc }
