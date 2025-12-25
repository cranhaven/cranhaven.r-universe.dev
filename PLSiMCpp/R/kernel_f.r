
.kernel_f = function(x,y,h=1,method="epa")
{
  if(method == "epa")
  {
    k = .epa.ker(x-y,h)
  }
  else if(method == "nor")
  {
    k = .nor.ker(x-y,h)
  }
  else
  {
    cat( blue$bold('\n Please select a kernel function from Epanechnikov (')
         %+% black$bold('epa')
         %+% blue$bold(') and Gaussian (')
         %+% black$bold('nor')
         %+% blue$bold('). \n \n')
    )


    return(NULL)
  }


  return(k)
}


.epa.ker = function(x,h=1)
{
  return( 3/4*(1-x^2)*(abs(x)<=h) )
}


.nor.ker = function(x,h=1)
{
  return( 1/sqrt(2*pi) * exp^(-x^2/2) )
}



