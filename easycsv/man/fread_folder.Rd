\name{fread_folder}
\alias{fread_folder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
read multiple csv files into named data frames
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Reads multiple files in table format using \link[data.table]{fread}'s speed and creates a data frame from them, with cases corresponding to lines and variables to fields in the file.
}
\usage{
fread_folder(directory = NULL,
                        extension = "CSV",
                        sep = "auto",
                        nrows = -1L,
                        header = "auto",
                        na.strings = "NA",
                        stringsAsFactors = FALSE,
                        verbose=getOption("datatable.verbose"),
                        skip = 0L,
                        drop = NULL,
                        colClasses = NULL,
                        integer64=getOption("datatable.integer64"),# default:"integer64"
                        dec = if (sep!=".") "." else ",",
                        check.names = FALSE,
                        encoding = "unknown",
                        quote = "\"",
                        strip.white = TRUE,
                        fill = FALSE,
                        blank.lines.skip = FALSE,
                        key = NULL,
                        Names=NULL,
                        prefix=NULL,
                        showProgress = interactive(),
                        data.table=TRUE
                        )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{directory}{
  a directory to load the files from, if NULL then a manual choice is provided on windows OS.
  }
  \item{extension}{
  "TXT" for tables in '.txt' files, "CSV" for tables in '.csv' files, "BOTH" for both file endings.

}
  \item{sep}{
  The separator between columns. Defaults to the first character in the set [,\verb{\t} |;:] that exists on line autostart outside quoted ("") regions, and separates the rows above autostart into a consistent number of fields, too.
  }
  \item{nrows}{
  The number of rows to read, by default -1 means all. Unlike read.table, it doesn't help speed to set this to the number of rows in the file (or an estimate), since the number of rows is automatically determined and is already fast. Only set nrows if you require the first 10 rows, for example. 'nrows=0' is a special case that just returns the column names and types; e.g., a dry run for a large file or to quickly check format consistency of a set of files before starting to read any.
  }
  \item{header}{
  Does the first data line contain column names? Defaults according to whether every non-empty field on the first data line is type character. If so, or TRUE is supplied, any empty column names are given a default name.
  }
  \item{na.strings}{
  A character vector of strings which are to be interpreted as NA values. By default ",," for columns read as type character is read as a blank string ("") and ",NA," is read as NA. Typical alternatives might be na.strings=NULL (no coercion to NA at all!) or perhaps na.strings=c("NA","N/A","null")
  }
  \item{stringsAsFactors}{
  Convert all character columns to factors?
  }
  \item{verbose}{
  Be chatty and report timings?
  }
  \item{skip}{
  If 0 (default) use the procedure described below starting on line autostart to find the first data row. skip>0 means ignore autostart and take line skip+1 as the first data row (or column names according to header="auto"|TRUE|FALSE as usual). skip="string" searches for "string" in the file (e.g. a substring of the column names row) and starts on that line (inspired by read.xls in package gdata).
  }
  \item{drop}{
  Vector of column names or numbers to drop, keep the rest.
  }
  \item{colClasses}{
  A character vector of classes (named or unnamed), as read.csv. Or a named list of vectors of column names or numbers, see examples. colClasses in fread is intended for rare overrides, not for routine use. fread will only promote a column to a higher type if colClasses requests it. It won't downgrade a column to a lower type since NAs would result. You have to coerce such columns afterwards yourself, if you really require data loss.
  }
  \item{integer64}{
  "integer64" (default) reads columns detected as containing integers larger than 2^31 as type bit64::integer64. Alternatively, "double"|"numeric" reads as base::read.csv does; i.e., possibly with loss of precision and if so silently. Or, "character".
  }
  \item{dec}{
  The decimal separator as in base::read.csv. If not "." (default) then usually ",". See details.
  }
  \item{check.names}{
  default is FALSE. If TRUE then the names of the variables in the data.table are checked to ensure that they are syntactically valid variable names. If necessary they are adjusted (by \link[base]{make.names}) so that they are, and also to ensure that there are no duplicates.
  }
  \item{encoding}{
  default is "unknown". Other possible options are "UTF-8" and "Latin-1". Note: it is not used to re-encode the input, rather enables handling of encoded strings in their native encoding.
  }
  \item{quote}{
  By default ("\""), if a field starts with a doublequote, fread handles embedded quotes robustly as explained under Details. If it fails, then another attempt is made to read the field as is, i.e., as if quotes are disabled. By setting quote="", the field is always read as if quotes are disabled.
  }
  \item{strip.white}{
  default is TRUE. Strips leading and trailing whitespaces of unquoted fields. If FALSE, only header trailing spaces are removed.
  }
  \item{fill}{
  logical (default is FALSE). If TRUE then in case the rows have unequal length, blank fields are implicitly filled.
  }
  \item{blank.lines.skip}{
  logical, default is FALSE. If TRUE blank lines in the input are ignored.
  }
  \item{key}{
  Character vector of one or more column names which is passed to setkey. It may be a single comma separated string such as key="x,y,z", or a vector of names such as key=c("x","y","z"). Only valid when argument data.table=TRUE
  }
  \item{Names}{
  A character vector of names for the tables to be read, note that the table will be read and listed by an alphabetical order, use with caution.
  }
  \item{prefix}{
  A character string to be prefixed to each table name.
  }
  \item{showProgress}{
  TRUE displays progress on the console using \verb{\r}. It is produced in fread's C code where the very nice (but R level) txtProgressBar and tkProgressBar are not easily available.
  }
  \item{data.table}{
  logical. TRUE returns a data.table. FALSE returns a data.frame.
  }
}
\details{
Similar to \link[easycsv]{loadcsv_multi} can read multiple tables from either '.txt' or '.csv' files, uses \link[data.table]{fread} for additional speed. Takes arguments that respond to \link[data.table]{fread}'s arguments.
}
\value{
A \link[base]{data.frame} containing a representation of the data in the file.
}
\note{
  This function alone requires \link[data.table]{fread}, it is not installed by default with easycsv, because of that.
  If you use "BOTH" option with 'txt' make sure your '.txt' and '.csv' files have different names.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\link[easycsv]{loadZIPcsvfromURL}
\link[easycsv]{loadcsvfromZIP}
\link[easycsv]{loadcsv_multi}
\link[data.table]{fread}
}
\examples{
  require(easycsv)
  require("data.table")
  directory = getwd()
  write.csv(data.frame(matrix(1:9, nrow = 3)), file = file.path(directory,"/table1.csv"))
  write.csv(data.frame(matrix(1:9, nrow = 3)), file = file.path(directory,"/table2.csv"))
  write.csv(data.frame(matrix(1:9, nrow = 3)), file = file.path(directory,"/table3.txt"))
  write.csv(data.frame(matrix(1:9, nrow = 3)), file = file.path(directory,"/table4.txt"))
  fread_folder(directory, extension = "BOTH")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~utilities }
\keyword{ ~misc }
