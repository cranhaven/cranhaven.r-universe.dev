##
## S3 method to design matrix and response variable or data.frame objects
##

TukeyC.default <- function(x,
                           ...)
{

  stop(paste("class", 
             class(x), 
             "objects are not valid for TukeyC" ))

}
