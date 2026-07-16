HDoutliers <-
function( data, maxrows=10000, radius=NULL, alpha=.05, transform = TRUE) 
{
 
# MCA for categorical variables; unitize data

 data <- if (transform) dataTrans(data) else as.matrix(data)

 if (any(is.na(data))) stop("missing values not allowed")

 members <- getHDmembers( data, radius=radius, maxrows = maxrows)

 getHDoutliers( data, members, alpha=alpha, transform=FALSE)
}
