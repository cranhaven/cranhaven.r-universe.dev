\name{createGroupset}
\alias{createGroupset}
\title{
Create a group set (groups) of variables
}
\description{
Create a group set (groups) of variables for categorical co-data (factor, character or boolean input), or for continuous co-data (numeric). Continuous co-data is discretised in non-overlapping groups.
}
\usage{
createGroupset(values,index=NULL,grsize=NULL,ngroup=10,
                decreasing=TRUE,uniform=FALSE,minGroupSize = 50)
}
\arguments{
  \item{values}{
Factor, character or boolean vector for categorical co-data, or numeric vector for continuous co-data values.
}
  \item{index}{
%%     ~~Describe \code{index} here~~
Index of the covariates corresponding to the values supplied. Useful if part of the co-data is missing/seperated and only the non-missing/remaining part should be discretised.
}

  \item{grsize}{
Numeric. Size of the groups. Only relevant when \code{values} is a numeric vector and \code{uniform=TRUE}.
}
\item{ngroup}{
Numeric. Number of the groups to create. Only relevant when \code{values} is a numeric vector and 
\code{grsize} is NOT specified.
}
  \item{decreasing}{
Boolean. If \code{TRUE} then \code{values} is sorted in decreasing order.
}

\item{uniform}{
Boolean. If \code{TRUE} the group sizes are as equal as possible.
}

\item{minGroupSize}{
Numeric. Minimum group size. Only relevant when \code{values} is a numeric vector and \code{uniform=FALSE}.
}
}

\details{
This function is derived from \code{CreatePartition} from the \code{GRridge}-package, available on Bioconductor. Note that the function name and some variable names have been adapted to match terminology used in other functions in the \code{ecpc}-package.

A convenience function to create group sets of variables from external information that is stored in \code{values}. If \code{values} is a factor then the levels of the factor define the groups. 
If \code{values} is a character vector then the unique names in the character vector define the groups.
If \code{values} is a Boolean vector then the group set consists of two groups for True and False.
If \code{values} is a numeric vector, then groups contain the variables corresponding to 
\code{grsize} consecutive values of \code{values}. Alternatively, the group size 
is determined automatically from \code{ngroup}. If \code{uniform=FALSE}, a group with rank $r$ is  
of approximate size \code{mingr*(r^f)}, where \code{f>1} is determined such that the total number of groups equals \code{ngroup}.
Such unequal group sizes enable the use of fewer groups (and hence faster computations) while still maintaining a 
good `resolution' for the extreme values in \code{values}. About \code{decreasing}: if smaller values
mean `less relevant' (e.g. test statistics, absolute regression coefficients) use \code{decreasing=TRUE}, else use \code{decreasing=FALSE}, e.g. for p-values. If \code{index} is defined, then the group set will use these variable indices corresponding to the values. Useful if the group set should be made for a subset of all variables.
}
\value{
A list with elements that contain the indices of the variables belonging to each of the groups. 
}

\author{
Mark A. van de Wiel
}

\seealso{
Instead of discretising continuous co-data in a a fixed number of groups, they may be discretised adaptively to learn a discretisation that fits the data well, see: \code{\link{splitMedian}}. 
}

\examples{
#SOME EXAMPLES ON SMALL NR OF VARIABLES

#EXAMPLE 1: group set based on known gene signature (boolean vector)
genset <- sapply(1:100,function(x) paste("Gene",x))
signature <- sapply(seq(1,100,by=2),function(x) paste("Gene",x))
SignatureGroupset <- createGroupset(genset\%in\%signature) #boolean vector

#EXAMPLE 2: group set based on factor variable
Genetype <- factor(sapply(rep(1:4,25),function(x) paste("Type",x)))
TypeGroupset <- createGroupset(Genetype)

#EXAMPLE 3: group set based on continuous variable, e.g. p-value
pvals <- rbeta(100,1,4)

#Creating a group set of 10 equally-sized groups, corresponding to increasing p-values.
PvGroupset <- createGroupset(pvals, decreasing=FALSE,uniform=TRUE,ngroup=10)

#Alternatively, create a group set of 5 unequally-sized groups,
#with minimal size at least 10. Group size
#increases with less relevant p-values.
# Recommended when nr of variables is large.
PvGroupset2 <- createGroupset(pvals, decreasing=FALSE,uniform=FALSE,
                              ngroup=5,minGroupSize=10)

#EXAMPLE 4: group set based on subset of variables,
#e.g. p-values only available for 50 genes. 
genset <- sapply(1:100,function(x) paste("Gene",x))
subsetgenes <- sort(sapply(sample(1:100,50),function(x) paste("Gene",x)))
index <- which(genset\%in\%subsetgenes)

pvals50 <- rbeta(50,1,6)

#Returns the group set for the subset based on the indices of 
#the variables in entire genset. 

PvGroupsetSubset <- createGroupset(pvals50, index=index,
                                   decreasing=FALSE,uniform=TRUE, ngroup=5)
#append list with group containing the covariate indices for missing p-values
PvGroupsetSubset <- c(PvGroupsetSubset,
                      list("missing"=which(!(genset\%in\%subsetgenes))))

#EXAMPLE 5: COMBINING GROUP SETS

#Combines group sets into one list with named components. 
#This can be used as input for the ecpc() function.

GroupsetsAll <- list(signature=SignatureGroupset, type = TypeGroupset,
                     pval = PvGroupset, pvalsubset=PvGroupsetSubset)
               
#NOTE: if one aims to use one group set only, then this should also be
# provided in a list as input for the ecpc() function.

GroupsetsOne <- list(signature=SignatureGroupset)

}