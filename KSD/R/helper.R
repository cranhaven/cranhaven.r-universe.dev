# Repeat a given vector row-wise
#
# Takes in any vector and repeats it to make a matrix.
#
# @param x   A vector of any size
# @param n   The number of times vector will be repeated
# @return The matrix created by repeating x by n times.
#
# @examples
# x <- c(1,2,3)
# rep.row(x,4)

rep.row<-function(x,n){
      matrix(rep(x,each=n),nrow=n)
}

# Repeat a given vector column-wise
#
# Takes in any vector and repeats it to make a matrix.
#
# @param x   A vector of any size
# @param n   The number of times vector will be repeated
# @return The matrix created by repeating x by n times.
#
# @examples
# x <- c(1,2,3)
# rep.col(x,4)

rep.col<-function(x,n){
      matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

# Repeat a given matrix in each dimension (up to 2d)
#
# Takes in any matrix and repeats it to make a matrix.
# Equivalent of Matlab's repmat function
#
# @param X   A matrix of any size
# @param a   The number of times X will be repeated row-wise
# @param b   The number of times X will be repeated column-wise
# @return The matrix created by repeating X 'a' times row-wise and 'b' times columnwise.
#
# @examples
# X <- matrix(c(1,2,3,4,5,6,7,8),ncol=2)
# result <- repmat(X,2,3)


repmat <- function(X,a=1,b=1){
      rows <- dim(X)[1]
      cols <- dim(X)[2]
      if(is.null(cols))
            cols <- 1
      rowRep <- matrix(rep(t(X),a),ncol = cols, byrow = TRUE)
      newX <- matrix(rep(rowRep, b),ncol=cols*b)
      return(newX)
}

# Cleans up workspace
# Removes all current variables

cleanup <- function(){
      rm(list=ls())
}


# Returns size of x
#
# Returns a list, which stores the dimension of the matrix.
#
# @param x   A matrix of data
# @return    List where
# n = number of rows of x;
# dim = number of columns of x
#
# @examples
# x <- matrix(c(1,2,3,4,5,6,7,8),ncol=2)
# dim <- getDim(x)
getDim <- function(x){
      if(is.array(x)){
            n <- dim(x)[1]; dimen <- dim(x)[2]
      }else{
            x <- array(x)
            n <- dim(x)[1]; dimen <- 1
      }

      result <- list("n" = n, "dim" = dimen)
      return(result)
}

# Score function for gamma distribution
gamma_score <- function(x, shape, rate=1,scale=1/rate){
      return ((shape-1)/x - 1/scale)
}

# Function that can be used to retain legend of a plot
# http://www.sthda.com/
get_legend<-function(myggplot){
      tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(myggplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      return(legend)
}

# Function that rounds up given number to three significant digits
custround <- function(x){
      round(x,3)
}

