\name{RECODE}
\alias{RECODE}
\title{Recode values in a vector}
\description{Options for changing the numeric values in a vector to new numeric values
}
\usage{RECODE(data, old = NULL, new = NULL, type = 'reverse', max_value = NULL,
       real_min = NULL, real_max = NULL, new_min = NULL, new_max = NULL) 
}
\arguments{
 \item{data}{
 \code{}A numeric vector, typically consisting of item responses.}

 \item{old}{
 \code{(optional)} A vector of the values in data to be recoded, e.g., old = c(1,2,3,4).}
                  
 \item{new}{
 \code{(optional)} A vector of the values that should replace the old values, e.g., new = c(0,0,1,1).}
                  
 \item{type}{
 \code{(optional)} The type of recoding if "old" and "new" are not specified. The options are
 	'reverse' (for reverse coding) and 'new_range' (for changing the metric/range).}
                  
 \item{max_value}{
 \code{(optional)} For type = 'reverse' coding only. It is the maximum possible value for data (an item). 
 This option is included for when max(data) is not the maximum possible value for an item (e.g., when
 the highest response option was never used).}
                  
 \item{real_min}{
 \code{(optional)} For type = 'new_range' coding only. The minimum possible value for data.}
                  
 \item{real_max}{
 \code{(optional)} For type = 'new_range' coding only. The maximum possible value for data.}
                  
 \item{new_min}{
 \code{(optional)} For type = 'new_range' coding only. The desired, new minimum possible value for data.}
                  
 \item{new_max}{
 \code{(optional)} For type = 'new_range' coding only. The desired, new maximum possible value for data.}
}
\details{
When 'old' and 'new' are specified, the data values in the 'old' vector are replaced with the values in the
same ordinal position in the 'new' vector, e.g., occurrences of the second value in 'old'
in data are replaced with the second value in 'new' in data.	
		
Regarding the type = 'new_range' option: Sometimes the items in a pool have different response option 
ranges, e.g., some on a 5-point scale and others on a 6-point scale. The type = 'new_range' option 
changes the metric/range of a specified item to a desired metric, e.g., so that scales scores 
based on all of the items in the pool can be computed. This alters item scores and the new 
item values may not be integers. Specifically, for each item response, the percent value on 
the real/used item is computed. Then the corresponding value on the desired new item metric 
for the same percentage is found.
}
\value{The recoded data values}
\author{Brian P. O'Connor }
\examples{
data <- c(1,2,3,4,1,2,3,4)	
print(RECODE(data, old = c(1,2,3,4), new = c(1,1,2,2)) )

print(RECODE(data, type = 'reverse'))	
\donttest{
# reversing coding the third item (Q3) of the Rosenberg Self-Esteem scale data
data_RSE_rev <- RECODE(data_RSE_not_recoded[,'Q3'], type = 'reverse')
table(data_RSE_rev); table(data_RSE_not_recoded[,'Q3'])

# changing the third item (Q3) responses for the Rosenberg Self-Esteem scale data
# from 0-to-4 to 1-to-5
data_RSE_rev <- RECODE(data_RSE_not_recoded[,'Q3'], old = c(0,1,2,3,4), new = c(1,2,3,4,5))
table(data_RSE_rev); table(data_RSE_not_recoded[,'Q3'])

# changing the metric/range of the third item (Q3) responses for the 
# Rosenberg Self-Esteem scale data
data_RSE_rev <- RECODE(data_RSE_not_recoded[,'Q3'], type = 'new_range', 
                 real_min = 1, real_max = 4, new_min = 1, new_max = 5 ) 
table(data_RSE_rev); table(data_RSE_not_recoded[,'Q3'])
}
}
