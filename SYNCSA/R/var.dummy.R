#' @title Generate dummy variable
#'
#' @description Function to expand factor variables in dummy variables in a data.frame. See details.
#'
#' @details Variables in the data.frame of class factor is expanded in dummy variables. Each
#' level of factor produce a new column in the dataframe, with presence (1) or
#' absense (0) of level. The name of columns is a combination of orginal variable name
#' plus the level separate by underscore ( _ ). Ordered factor and character class are
#' not expanded.
#'
#' @encoding UTF-8
#' @param data A data.frame.
#' @return \item{data}{The data with all variables of class factor expanded.} \item{together}{A list
#' with sugestion to group of traits that are analysed together.}
#' @author Vanderlei Julio Debastiani <vanderleidebastiani@@yahoo.com.br>
#' @seealso  \code{\link{syncsa}}, \code{\link{organize.syncsa}}, \code{\link{var.type}}
#' @keywords SYNCSA
#' @export
var.dummy<-function(data)
{
  if(!inherits(data, "data.frame")){
    stop("data must be a data.frame")
  }
  colnames(data) <- colnames(data, do.NULL = FALSE, prefix = "var")
  type <- var.type(data)
  n <- nrow(data)
  RES <- as.data.frame(rep(NA, n))
  rownames(RES) <- rownames(data)
  m <- 1
  l <- 1
  together <- list()
  for(i in 1:ncol(data)){
    if(type[i] == "f"){
      var_temp <- table(1:n, as.factor(data[,i]))
      colnames(var_temp) <- paste(colnames(data)[i], colnames(var_temp), sep = "_")
      together[[l]] <- colnames(var_temp)
      l <- l+1
      for(j in 1:ncol(var_temp)){
        m <- m+1
        RES[,m] <- as.numeric(var_temp[, j, drop = FALSE])
        colnames(RES)[m] <- colnames(var_temp)[j]
      }
    }else{
      m <- m+1
      RES[, m] <- data[, i, drop = FALSE]
    }
  }
  RES <- RES[, -1, drop = FALSE]
  return(list(data = RES, together = together))
}
