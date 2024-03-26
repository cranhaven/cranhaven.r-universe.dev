#' Indicator matrix
#'
#' Convert values of categorical variables into indicator matrix
#'
#' @param x A data frame of categorical data coded in numbers.
#' @return Dummy_variables
#'
#' @keywords Indicator
#'
#' @examples
#' ## Long form
#' data(PTSD)
#' PTSD <- as.data.frame(PTSD)
#' # Transform a string or continuous class variable into factor
#' PTSD[,2:4] <- apply(PTSD[,2:4], 2, function(x) ifelse(x >= 3, 1, 0))
#' PTSD[,5] <-  ifelse(PTSD[,5] >= 6 , 1, 0)
#' PTSD <- data.frame(lapply(PTSD[,-1], function(x) as.factor(x)))
#' indicator(PTSD)
#'
#'
#' ## Wide form
#' data(Depression)
#' str(Depression)
#' indicator(Depression[,-1])
#'
#'
#'
#' @importFrom utils combn
#' @export indicator
#'

indicator = function(x){

  if( class(x) != "data.frame" ) stop("Data should be data.frame")

  if( any( apply(x, 2, function(x) length(table(x)) > 100 ) ) ) stop(paste0(""))

  if( any( apply(x, 2, function(x) length(table(x)) > 10 ) ) ) warning(paste0("\nPlease check the format of column ",

                                                              which(apply(x, 2, function(x) length(table(x)) > 10 )) ) )
  check.df.levels <- function(df){

    sapply(df, function(x) {

      if( !is.factor(x) ) stop("A variable is not a factor. You have to convert it to factor")

      if( !is.factor(x) ) return(0)

      if( is.factor(x) ){

        return( nlevels(droplevels(x)) )

        if( nlevels(droplevels(x)) < 2 ) stop("A variable have to consists of two or more categories to be converted into a dummy variable")

         }

       }) %>% unlist
  }

  check.df.levels(x)

  colnames.old <- colnames(x)

  gsub("[^[:alnum:][:blank:]+?&/\\-]", "" , colnames(x) )

  if( any( grepl("[0-9]|[^[:alnum:][:blank:]+?&/\\-]", substring(colnames(x), 1, 1)) ) ){

    colnames(x) <- ifelse( grepl("[0-9]|[^[:alnum:][:blank:]+?&/\\-]", substring(colnames(x), 1, 1)), paste0("T_", colnames(x)), colnames(x) )

    }

  x <- as.data.frame( apply(x, 2, as.factor), stringAsFactors=TRUE )

  indicator.mat <- NULL

  for (i in 1:ncol(x)){

    var <- colnames(x)[i]

    tmp.data <- paste0("x[", i ,"]")

    indicator.mat <- cbind( indicator.mat, eval(parse(text=paste("model.matrix(~ ", var, " -1 , ", tmp.data, ")"  ) ) ) )

    }

  indicator.mat <- as.data.frame( apply( indicator.mat, 2, as.integer ) )

  indicator.mat.colnames <- NULL

  for( i in 1:ncol(x)){

    indicator.mat.colnames <- c(indicator.mat.colnames, paste( colnames.old[i], names( table( x[, i] ) ), sep="." ) )

    }

  colnames(indicator.mat) <- indicator.mat.colnames

  return(indicator.mat)

}
