Pull <- function(df, ...) if (!is.data.frame(df)) stop("Not a data frame") else df[with(df, ...), ]
