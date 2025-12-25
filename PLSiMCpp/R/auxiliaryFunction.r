deal_formula = function(formula){
  tf <- as.character(formula)  
  tf <- tf[length(tf)]
  
  eval(parse(text=paste("c(",
                        ifelse(length(as.character(formula)) == 3,
                               'strsplit(as.character(formula)[2]," *[+] *"),',""),
                        'strsplit(strsplit(tf," *[|] *")[[1]]," *[+] *"))')))
}

.reshapeMatrix=function(x,n)
{
  return(matrix(rep.int(x,n),nrow(x),n))
}


.assertion_for_variables=function(data)
{
  if( is.null(data$y) )
  {
    stop("Y should not be NULL")
  }
  
  if( (!is.matrix(data$y))&(!is.data.frame(data$y)) )
  {
    stop("Y should be a matrix or dataframe")
  }
  
  
  
  if( is.null(data$z) )
  {
    stop("Z should not be NULL. If Z is NULL, please utilize linear models,
         such as lm() function")
  }
  
  if((!is.matrix(data$z))&(!is.data.frame(data$z)))
  {
    stop("Z should be a matrix or dataframe")
  }  
  
  
  if(!is.null(data$x))
  {
    if( (!is.matrix(data$x))&(!is.data.frame(data$x)) )
    {
      stop("X should be a matrix or dataframe")
    }
  }
  
  
  if( !is.null(data$x) )
  {
    if(nrow(data$y) !=  nrow(data$x) )
    {
      stop("The sample size of Y is not equal to that of X")
    }
  }
  
  
  if( nrow(data$y) !=  nrow(data$z) )
  {
    stop("The sample size of Y is not equal to that of Z")
  }
  
  return(TRUE)
  
}

.r_square=function(y,y_bar)
{
  y_ = mean(y)
  
  numerator = ( t(y - y_)%*%(y_bar - y_) )^2
  
  rs = numerator/(t(y - y_)%*%(y - y_)  *  t(y_bar - y_)%*%(y_bar - y_) )
  
  return(rs)
}

