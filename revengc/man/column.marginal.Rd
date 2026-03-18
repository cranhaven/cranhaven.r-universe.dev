\name{column.marginal}
\alias{column.marginal}
\title{Column marginal table from contingency table}
\usage{
column.marginal(contingencytable)
}

\arguments{
\item{contingencytable}{ A censored contingency table.  Accepted classes are a data.frame or matrix.  See Details section below formatting.}
}

\value{A univariate frequency table (data.frame) created from the column marginals. }

\description{This function inputs a contingency table and produces a univariate frequency table from the column marginals.}

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
# contingencytable.csv is a preloaded contingency table example
column.marginal(contingencytable.csv)
}
