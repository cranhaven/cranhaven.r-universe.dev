\name{reweight.contingencytable}
\alias{reweight.contingencytable}
\title{Reweighting a contingency table}
\usage{
reweight.contingencytable(observed.table, estimated.table)
}


\arguments{
\item{observed.table}{A censored contingency table. See Details section below formatting this data.frame. }

\item{estimated.table}{A data.frame with uncensored row names and column names. }


}

\value{
Interior probability cells of a censored contingency table (observed.table) is reweighted to match interior probability cells of an uncensored contingency table (estimated.table).  If observed.table consist of frequencies, the reweight.contingencytable() function changes frequencies to probabilities.    
}

\description{
This function is used in the main function: rec. 
}


\details{

\strong{Format for observed.table:}\cr
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
## going through the coding step of rec ##

# first create contingency table 
contingencytable<-matrix(1:9, 
                         nrow = 3, ncol = 3)
rowmarginal<-apply(contingencytable,1,sum)
contingencytable<-cbind(contingencytable, rowmarginal)
colmarginal<-apply(contingencytable,2,sum)
contingencytable<-rbind(contingencytable, colmarginal)
row.names(contingencytable)[row.names(contingencytable)=="colmarginal"]<-""
contingencytable<-data.frame(c("<5", "5I9", ">9", NA), contingencytable)
colnames(contingencytable)<-c(NA,"<=19","20-30",">=31", NA)

# provided upper and lower bound values for table
# X=row and Y=column
Xlowerbound=1
Xupperbound=15
Ylowerbound=15
Yupperbound=35

# table of row marginals provides average x and phi x
row.marginal.table<-row.marginal(contingencytable)
x<-cnbinom.pars(row.marginal.table)
# table of column marginals provides average y and phi y 
column.marginal.table<-column.marginal(contingencytable)
y<-cnbinom.pars(column.marginal.table)

# create row and column ranges   
rowrange<-Xlowerbound:Xupperbound
colrange<-Ylowerbound:Yupperbound

library(truncdist)
# new uncensored row marginal table = truncated negative binomial distribution
# rowrange = X is distributed given a < X <= b
uncensored.row.margin<-dtrunc(rowrange, mu=x$Average, size = x$Dispersion, 
                              a = Xlowerbound-1, b = Xupperbound, spec = "nbinom")
# new uncensored column margin table = = truncated negative binomial distribution
# colrange = Y is distributed given a < Y <= b
uncensored.column.margin<-dtrunc(colrange, mu=y$Average, size = y$Dispersion,
                                 a = Ylowerbound-1, b = Yupperbound, spec = "nbinom")

# sum of truncated distributions equal 1
sum(uncensored.row.margin)
sum(uncensored.column.margin)

# look at the seed for this example (probabilities)
seed.output<-seedmatrix(contingencytable, Xlowerbound, 
                        Xupperbound, Ylowerbound, Yupperbound)$Probabilities

# run mipfp
# store the new margins in a list
tgt.data<-list(uncensored.row.margin, uncensored.column.margin)
# list of dimensions of each marginal constrain
tgt.list<-list(1,2)
# calling the estimated function
## seed has to be in array format for mipfp package
## ipfp is the selected seed.estimation.method
## $p.hat gives probabilies = x.hat/sum(x.hat)
library(mipfp)
final1<-Estimate(array(seed.output,dim=c(length(Xlowerbound:Xupperbound), 
                 length(Ylowerbound:Yupperbound))), tgt.list, tgt.data, method="ipfp")$x.hat

# filling in names of updated seed  
final1<-data.frame(final1)
row.names(final1)<-Xlowerbound:Xupperbound
names(final1)<-Ylowerbound:Yupperbound

# reweight estimates to known censored interior cell probabilities 
final1<-reweight.contingencytable(observed.table = contingencytable, estimated.table = final1)

# see that they sum to one
sum(final1)

# rec function outputs the same table
# default of rec seed.estimation.method is ipfp
# default of rec seed.matrix is the output of the seedmatrix() function
final2<-rec(X= contingencytable,
            Xlowerbound = 1,
            Xupperbound = 15,
            Ylowerbound = 15,
            Yupperbound = 35)

# check that both data.frame results have same values
all(final1 == final2$Probability.Estimates)
}



