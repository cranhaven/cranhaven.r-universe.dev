\name{RSDA2SymbolicDA}
\alias{RSDA2SymbolicDA}
\title{Read a Symbolic Table from }
\usage{
RSDA2SymbolicDA(rsda.object=NULL,from.csv=F,file=NULL
, header = TRUE, sep, dec, row.names = NULL)
}
\arguments{

\item{rsda.object}{object of class "symb.data.table" from (former) RSDA package)}

\item{from.csv}{object of class "symb.data.table" from (former) RSDA package)}

\item{file}{optional, The name of the CSV file in RSDA format (see details)}

\item{header}{As in R function read.table}

\item{sep}{As in R function read.table}

\item{dec}{As in R function read.table}

\item{row.names}{As in R function read.table}
}
\value{
Return a symbolic data table in form of SymbolicDA "symbolic" class type object.
}
\description{
It reads a symbolic data table from a CSV file or converts RSDA object to SymbolicDA "symbolic" class type object
}
\details{
(as in (former) RSDA package)
The labels $C means that follows a continuous variable, $I means an interval variable, $H
means a histogram variables and $S means set variable. In the first row each labels should
be follow of a name to variable and to the case of histogram a set variables types the names
of the modalities (categories) . In data rows for continuous variables we have just one
value, for interval variables we have the minimum and the maximum of the interval,
for histogram variables we have the number of modalities and then the probability
of each modality and for set variables we have the cardinality of the set and next
the elements of the set.

The format is the CSV file should be like:

  $C   F1 $I F2 F2 $H F3  M1  M2  M3 $S F4 E1 E2 E3 E4 \cr

Case1 $C  2.8 $I  1  2 $H  3 0.1 0.7 0.2 $S  4  e  g  k  i\cr

Case2 $C  1.4 $I  3  9 $H  3 0.6 0.3 0.1 $S  4  a  b  c  d\cr

Case3 $C  3.2 $I -1  4 $H  3 0.2 0.2 0.6 $S  4  2  1  b  c\cr

Case4 $C -2.1 $I  0  2 $H  3 0.9 0.0 0.1 $S  4  3  4  c  a\cr

Case5 $C -3.0 $I -4 -2 $H  3 0.6 0.0 0.4 $S  4  e  i  g  k\cr

The internal format is:\cr
  $N\cr
[1] 5\cr
$M\cr
[1] 4\cr
$sym.obj.names\cr
[1] 'Case1' 'Case2' 'Case3' 'Case4' 'Case5'\cr
$sym.var.names\cr
[1] 'F1' 'F2' 'F3' 'F4'\cr
$sym.var.types\cr
[1] '$C' '$I' '$H' '$S'\cr
$sym.var.length\cr
[1] 1 2 3 4\cr
$sym.var.starts\cr
[1]  2  4  8 13\cr
$meta\cr
$C   F1 $I F2 F2 $H F3  M1  M2  M3 $S F4 E1 E2 E3 E4\cr
Case1 $C  2.8 $I  1  2 $H  3 0.1 0.7 0.2 $S  4  e  g  k  i\cr
Case2 $C  1.4 $I  3  9 $H  3 0.6 0.3 0.1 $S  4  a  b  c  d\cr
Case3 $C  3.2 $I -1  4 $H  3 0.2 0.2 0.6 $S  4  2  1  b  c\cr
Case4 $C -2.1 $I  0  2 $H  3 0.9 0.0 0.1 $S  4  3  4  c  a\cr
Case5 $C -3.0 $I -4 -2 $H  3 0.6 0.0 0.4 $S  4  e  i  g  k\cr
$data\cr
F1 F2 F2.1  M1  M2  M3 E1 E2 E3 E4\cr
Case1  2.8  1    2 0.1 0.7 0.2  e  g  k  i\cr
Case2  1.4  3    9 0.6 0.3 0.1  a  b  c  d\cr
Case3  3.2 -1    4 0.2 0.2 0.6  2  1  b  c\cr
Case4 -2.1  0    2 0.9 0.0 0.1  3  4  c  a\cr
Case5 -3.0 -4   -2 0.6 0.0 0.4  e  i  g  k\cr
}
\examples{
# Example will be available in next version of package, thank You for your patience :-)
}
\references{
Bock H.H., Diday E. (eds.) (2000), \emph{Analysis of Symbolic Data. Explanatory methods for extracting statistical information from complex data}, Springer-Verlag, Berlin.
}
\seealso{
display.sym.table
}
\author{
Andrzej Dudek

With ideas from  RSDA package by Oldemar Rodriguez Rojas 
}
\keyword{Symbolic}
\keyword{Table}
\concept{SDA}
\concept{SymbolicDA}
\concept{RSDA}
