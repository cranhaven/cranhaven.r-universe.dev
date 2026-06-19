#' @title Check if the matrix is a density of a copula
#' @description Return a logical value
#' @param D.ini a matrix with the initial values for the density copula.package: the name of the package for numerical optimization.
#' @param k positive integer indicating the number of subintervals for the U2 variable.
#' @param m positive integer indicating the number of subintervals for the U1 variable.
#' @examples


validate.density<- function(D.ini,k,m)
	{
	if(round(apply(D.ini,1,sum)[1],0) == m & round(apply(D.ini,2,sum)[1],0) == k)
		{
		return(TRUE)
		}else
		{
		return(FALSE)
		}
	

	}











