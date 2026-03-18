\name{seedmatrix}
\alias{seedmatrix}
\title{An uncensored seed matrix from censored contingency table}
\usage{
seedmatrix(censoredtable, Xlowerbound, Xupperbound, Ylowerbound, Yupperbound)
}
\arguments{
\item{censoredtable}{A censored contingency table.  A data.frame and matrix are accepted table classes.  See Details section for formatting. }

\item{Xlowerbound}{A numeric class value to represent a lower bound for the row values.  The value must strictly be >= 0 and cannot be greater than the lowest row category value (e.g. the lower bound cannot be 6 if a table has '< 5' as a row category value)}

\item{Xupperbound}{A numeric class value to represent an upper bound for the row values.  The value cannot be less than the highest row category value (e.g. the upper bound cannot be 90 if a table has '> 100' as a row category value).}

\item{Ylowerbound}{Same description as Xlowerbound but this argument is for the columns in contingency table).}

\item{Yupperbound}{Same description as Xupperbound but this argument is for the columns in contingency table).}
}

\value{
The output is a list of two tables: 'Exact' and 'Probabilities'.  Both tables have rows ranging from  Xlowerbound, Xlowerbound + 1,..., Xupperbound and columns ranging from Ylowerbound, Ylowerbound + 1,..., Yupperbound.  Also, the marginals are not placed in either table.  The 'Exact' table uses the cross tabulation probabilities corresponding to the censored categories and repeats these values to the newly created and compatible uncensored cross tabulations (frequencies are changed to probabilities).  The 'Probabilities' table takes the the cells of the 'Exact' table are divides by sum(Exact).  The rec() function uses the 'Probabilities' table.       

}

\description{
To implement the mipfp R package in our 'rec' function, an initial N-dimensional array (called a seed) needs to be provided.  This function creates a seed matrix for an uncensored contingency table (Xlowerbound:Xupperbound, Ylowerbound:Yupperbound) when provided a censored contingency table.     
}


\details{
\strong{Table Format:}\cr
The only symbols accepted for censored data are listed below.  Note, less than or equal to (<= and LE) is not equivalent to less than (< and L) and greater than or equal to (>=, +, and GE) is not equivalent to greater than (> and G).  Also, calculations use closed intervals. 

\itemize{
\item left censoring: <, L, <=, LE
\item interval censoring: - or I (symbol has to be placed in the middle of the two category values)
\item right censoring: >, >=, +, G, GE
\item uncensored: no symbol (only provide category value)
}

The column names should be the Y category values. The first column should be the X category values and the row names can be arbitrary. The inside of the table are X * Y cross tabulation, which are either positive frequency values or probabilities. The row and column marginal totals corresponding to their X and Y category values need to be placed in this table. The top left, top right, and bottom left corners of the table should be NA or blank.  The bottom right corner can be a total cross tabulation sum value, NA, or blank. The table below is a formatted example. 

  \tabular{ccccc}{
    NA \tab <20 \tab 20-30 \tab >30\tab NA\cr
    <5 \tab 18 \tab 19 \tab 8\tab 45\cr
    5-9 \tab 13 \tab 8 \tab 12\tab 33\cr
    >=10 \tab 7 \tab 5 \tab 10\tab 21\cr
    NA \tab 38 \tab 32 \tab 31\tab NA\cr
  }
}


\examples{
# create a censored contingency table
contingencytable<-matrix(1:9, 
                         nrow = 3, ncol = 3)
rowmarginal<-apply(contingencytable,1,sum)
contingencytable<-cbind(contingencytable, rowmarginal)
colmarginal<-apply(contingencytable,2,sum)
contingencytable<-rbind(contingencytable, colmarginal)
row.names(contingencytable)[row.names(contingencytable)=="colmarginal"]<-""
contingencytable<-data.frame(c("<5", "5I9", ">9", NA), contingencytable)
colnames(contingencytable)<-c(NA,"<=19","20-30",">=31", NA)

# look at the seed for this example (probabilities)
seed.output<-seedmatrix(contingencytable, 1, 
                        15, 15, 35)$Probabilities

}

