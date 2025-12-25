summary.pls <- function(object, ...)
{
  data <- object$data
  
  if(data$SiMflag == 0)
  {
    cat("\n Single Index Model")
    cat("\n")
  }
  else
  {
    cat("\n Partial Linear Single-index Model ")
    cat("\n")
  }
  
  n <- nrow(data$y)
  
  if(is.null(data$x))
  {
    dx <- 0
  }
  else
  {
    dx <- ncol(data$x)
  }
  
  dz <- ncol(data$z)
  d <- dx + dz
  
  
  
  cat(paste("\n Regression Data: ",as.character(n)," training data points, in ",
            as.character(d)," variable(s)"),sep="")
  cat("\n")
  
  if( is.null(colnames(data$z)) )
  {
    colnames(data$z) = 1:dz
  }
  
  cat('\n         ')
  cat(paste(colnames(data$z), collapse= "\t"))
  cat("\n Alpha: ") 
  cat(paste(round(object$zeta[1:dz],4),collapse= "\t"))
  cat("\n")
  
  if(!is.null(data$x))
  {
    if( is.null(colnames(data$x)) )
    {
      colnames(data$x) <- 1:dx
    }
    
    cat('\n        ')
    cat(paste(colnames(data$x), collapse= "\t"))
    cat("\n Beta: ") 
    cat(paste( round(object$zeta[(dz+1):(dz+dx)],4),collapse= "\t"))
    cat("\n")
  }
  
  cat('\n\n')
  cat(paste(' Mean Square Error: ',round(object$mse,6),sep=""))
  cat('\n')
  cat( paste(' R-squared: ',
             round(object$r_square,4),sep=""))
  cat("\n")
}

print.pls <- function(x, ...)
{
  data <- x$data
  
  if(data$SiMflag == 0)
  {
    cat("\n Single Index Model")
    cat("\n")
  }
  else
  {
    cat("\n Partial Linear Single-index Model ")
    cat("\n")
  }
  
  n <- nrow(data$y)
  
  if(is.null(data$x))
  {
    dx <- 0
  }
  else
  {
    dx <- ncol(data$x)
  }
  
  dz <- ncol(data$z)
  d <- dx + dz
  
  
  
  cat(paste("\n Regression Data: ",as.character(n)," training data points, in ",
      as.character(d)," variable(s)"),sep="")
  cat("\n")
  
  if( is.null(colnames(data$z)) )
  {
    colnames(data$z) = 1:dz
  }
  
  cat('\n         ')
  cat(paste(colnames(data$z), collapse= "\t"))
  cat("\n Alpha: ") 
  cat(paste(round(x$zeta[1:dz],4),collapse= "\t"))
  cat("\n")
  
  if(!is.null(data$x))
  {
    if( is.null(colnames(data$x)) )
    {
      colnames(data$x) <- 1:dx
    }
    
    cat('\n        ')
    cat(paste(colnames(data$x), collapse= "\t"))
    cat("\n Beta: ") 
    cat(paste( round(x$zeta[(dz+1):(dz+dx)],4),collapse= "\t"))
    cat("\n")
  }
  
  cat('\n\n')
  cat(paste(' Mean Square Error: ',round(x$mse,6),sep=""))
  cat('\n')
  cat( paste(' R-squared: ',
       round(x$r_square,4),sep=""))
  cat("\n")
}

fitted.pls <- function(object, ...)
{
  pr <- object$y_hat
  return(pr)
}