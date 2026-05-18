##'
##' @title Results
##'
##' @description function which generates a matrix from summary(Model),
##' where 'Model' is an 'lm' object. Is used by marima, in case there can
##' be one or more non-identifiable (ar-)parameters when estimating
##' the lm-object.
##'
##' @param Model = an 'lm' object (with NA's for non-identifiable
##' ar.parameters in the 'lm' specification, if so).
##' 
##' @return Results = matrix with 4 columns containing the same information
##' as summary(Model),
##' but with "NA" rows replaced by rows = c(0, 0, 0, NaN).
##'
##' @export
##' 

Results <- function( Model = NULL)
{
# function to eliminate NA from estimation results in Model (a marima object)

Results <- summary(Model)[[4]]
mc <- Model$coefficients
t <- which(is.na(mc))
terms <- names(Model$coefficients)
L <- length(t)
if(L > 0)
{
extra <- matrix(0,nrow=L, ncol=4)
extra[1:L, 4] <- NaN
rownames(extra) <- terms[t]
Results <- rbind(Results, extra)
Results <- Results[terms, ]
}
return( Results )
}
