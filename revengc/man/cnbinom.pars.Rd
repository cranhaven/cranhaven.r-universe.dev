\name{cnbinom.pars}
\alias{cnbinom.pars}
\title{Estimation of negative binomial parameters}
\usage{
cnbinom.pars(censoredtable)
}
\arguments{
\item{censoredtable}{A frequency table.  A data.frame and matrix are acceptable classes.  See Details section for formatting.}
}

\value{
The cnbinom.pars function outputs a list consisting of the estimated average (mu) and dispersion (r) parameter. 
}

\description{
A univariate censored frequency table is fit to a negative binomial distribution using a likelihood function customized to handle left, right, and interval censored data.  The output is a list containing the average and dispersion parameter that maximizes the custom function.  
}

\details{

The censored table for the cnbinom.pars function has restrictions. The univariate frequency table, which can be a data.frame or matrix class, must have two columns and n number of rows.  The categories must be in the first column with frequencies or probabilities in the second column.  Row names should never be placed in this table (the default row names should always be 1:n).  Column names can be any character string.  The only symbols accepted for censored data are listed below.  Note, less than or equal to (<= and LE) is not equivalent to less than (< and L) and greater than or equal to (>=, +, and GE) is not equivalent to greater than (> and G).  Also, calculations use closed intervals.     

\itemize{
\item left censoring: <, L, <=, LE
\item interval censoring: - or I (symbol has to be placed in the middle of the two category values)
\item right censoring: >, >=, +, G, GE
\item uncensored: no symbol (only provide category value)
}

Below are three correctly formatted tables.  

\tabular{cc}{
    Category\tab Frequency\cr
    <=6\tab 11800 \cr
    7-12\tab 57100 \cr
    13-19\tab 14800 \cr
    20+\tab 3900 \cr
  }
  
  \tabular{cc}{
    Category\tab Frequency\cr
    LE6\tab 11800 \cr
    7I12\tab 57100 \cr
    13I19\tab 14800 \cr
    GE20\tab 3900 \cr
  }
  
    \tabular{cc}{
    Category\tab Frequency\cr
    <7\tab 11800 \cr
    7I12\tab 57100 \cr
    13-19\tab 14800 \cr
    >=20\tab 3900 \cr
  }

}

\examples{

# create frequency table that follows a Poisson distribution
set.seed(123)
testdata1<-data.frame(table(rpois(100000,lambda = 10)))

# negative binomial converges in distribution to the Poisson 
## when dispersion->infinity
cnbinom.pars(testdata1)

# censor table of testdata1 (lambda = 10)
testdata2<-cbind(as.character(c("<=5", "6-10", "11-15", "16-20", ">20")), 
  c(6718,51329,37041,4732, 180))
cnbinom.pars(testdata2)

# create frequency table that follows a negative binomial distribution
# different ways to parameterize the negative binomial distribution
mu = 10
r = 2
p = r/(r+mu)
set.seed(123)
testdata3<-data.frame(table(rnbinom(100000, size = r, mu = mu)))
cnbinom.pars(testdata3)
set.seed(123)
testdata4<-data.frame(table(rnbinom(100000, size = r, p = p)))
cnbinom.pars(testdata4)

# censor table with mu = 10 and r = 2 
testdata5<-cbind(as.character(c("<5", "5-15", "16I26", "27-37", "38I48", ">48")), 
  c(26130,53979,15899,3267, 593, 132))
cnbinom.pars(testdata5)
}


\keyword{negative binomial}
\keyword{univariate table}
\keyword{frequency table}
\keyword{count data}
\keyword{censored}
\keyword{mu}
\keyword{dispersion}
