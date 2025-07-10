\name{RRprofReport}
\alias{RRprofReport}
\title{
RRprofReport
}
\description{
Generate the report based on the output of the R profiler.
}
\usage{
RRprofReport(file.name = "RRprof.out", notepad.path = 
"C:/Program Files/Notepad++/notepad++.exe",reportname = "my_report")
}
\arguments{
  \item{file.name}{
Name of a file produced by RRprofStart()
}
  \item{notepad.path}{
Path where notepad++.exe is
}
  \item{reportname}{
Name of the html file to be generated
}
}
\details{
This function generates a profiling report as a html file in the working directory.

The report consists of two main groups of tables: a summary of the called functions with the time spent for each of them and a second group of tables with the time spent on each line of code for each function. Furthermore, it includes a graph of the different hierarchical relationships between the called functions generated using functions in the package 'proftools'.

In the RStudio environment, this report is shown in the viewer pane. In addition, the markers panel indicates the lines of code where more time was spent to ease the navigation across the source code by simply clicking on them.

When it is opened in a browser, a convenient implemented feature is that the line numbers of the functions are clickable. If Notepad++ is installed, once a line number is clicked, the corresponding file is opened with the cursor on the selected line.
}
\author{
Fernando de Villar and Angel Rubio
}
\note{
It is advisable to open the Report using Internet Explorer, because other browsers can block the clickable line numbers feature
}


\seealso{
\code{\link[GUIProfiler]{RRprofStop}}, \code{\link[GUIProfiler]{RRprofStart}}, \code{\link{Rprof}}
}
\examples{
temp<-tempdir()
# Definition of two functions
normal.solve <- function(A,b) {
  Output <- solve(crossprod(A), t(A)\%*\%b)
}

chol.solve <- function(A,b) {
  L <- chol(crossprod(A))
  Output1 <- backsolve(L, t(A)\%*\%b, transpose=TRUE)
  Output2 <- backsolve(L, Output1)
}

compareMethods <- function() {
  library(MASS)
  # Call the functions
  source(paste(temp,"/normal.solve.R",sep=""))
  source(paste(temp,"/chol.solve.R",sep=""))
  # Solving a big system of equations
  nrows <- 1000
  ncols <- 500
  A <- matrix(rnorm(nrows*ncols),nrows,ncols)
  b <- rnorm(nrows)
  # Testing different possibilities
  Sol1 <- qr.solve(A,b) # Using QR factorization
  Sol2 <- coefficients(lm.fit(A,b)) # lm.fit, based on QR but with some overhead
  Sol3 <- ginv(A) \%*\% b # Using the pseudoinverse based on SVD
  Sol4 <- normal.solve(A,b) # Using a function based on the normal equations.
  Sol5 <- chol.solve(A,b) # Using a function based on the Choleski factorization.
}

# Dump these functions to three different files

dump("normal.solve",file=paste(temp,"/normal.solve.R",sep=""))
dump("chol.solve",file=paste(temp,"/chol.solve.R",sep=""))
dump("compareMethods",file=paste(temp,"/compareMethods.R",sep=""))
source(paste(temp,"/compareMethods.R",sep=""))

# Profile the code

RRprofStart()
compareMethods()
RRprofStop()
# Uncomment to open the report
#RRprofReport()

}