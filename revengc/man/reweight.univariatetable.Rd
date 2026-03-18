\name{reweight.univariatetable}
\alias{reweight.univariatetable}
\title{Reweighting a univariate table}
\usage{
reweight.univariatetable(observed.table, estimated.table)
}


\arguments{
\item{observed.table}{A data.frame or matrix with two columns.  See Details section for formatting.}

\item{estimated.table}{A numeric vector with uncensored row names. }
}

\value{
A censored univariate table (observed.table) of probabilities is reweighted to match probabilities in an uncensored numeric vector (estimated.table). If observed.table consist of frequencies, the reweight.contingencytable() function changes the frequencies to probabilities.    
}

\description{
This function is used in the main function: rec. 
}


\details{

\strong{Format for observed.table:}\cr
This univariate frequency table, which can be a data.frame or matrix class, must have two columns and n number of rows.  The categories must be in the first column with frequencies or probabilities in the second column.  Row names should never be placed in this table (the default row names should always be 1:n).  Column names can be any character string.  The only symbols accepted for censored data are listed below.  Note, less than or equal to (<= and LE) is not equivalent to less than (< and L) and greater than or equal to (>=, +, and GE) is not equivalent to greater than (> and G).  Also, calculations use closed intervals.    

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
# use preloaded univariate table for observed table 
observed.table<-univariatetable.csv

# estimate parameters from observed table
pars=cnbinom.pars(observed.table)
mu=pars$Average
r = pars$Dispersion

# create estimated table 
# truncated negative binomial probabilities
# uncensored range is from 1:15 
library(truncdist)
estimated.table<-dtrunc(1:15, size = r, mu = mu, spec = "nbinom", a = 1-1, b = 15)
names(estimated.table)<-1:15

# reweight observed table to estimated table
results<-reweight.univariatetable(observed.table, estimated.table)

# check results for >=9 
reweightedresults<-sum(results[9:15])
# observed.table$V2[5] = 4.1
observedresults<-observed.table$V2[5]/sum(observed.table$V2)  
# matching probabilities
all.equal(reweightedresults, observedresults)
}



