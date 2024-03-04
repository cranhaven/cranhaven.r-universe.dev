#' Calculate ellipse 'confidence region'
#' 
#' Based on the mean and standard deviation of the elevation and rotation estimates this function
#' will determine the region over which a given proportion of points would be expected to fall.
#' 
#' @param means A numeric vector of length 2 giving the mean for the x direction and the mean for the y direction.
#' @param sd A numeric vector of length 2 giving the standard deviation for the x direction and the y direction.
#' @param alpha A value between 0 and 1 that gives the confidence level.
#' @details Based on the mean and standard deviation of the x and y direction the limits of an ellipse are determined.
#' 
#' @return A list of length 2 containing:
#' \item{x}{The x axis coordinates of the ellipse}
#' \item{y}{The y axis coordinates of the ellipse}
#' @keywords internal
#' @export
#' 

calculateEllipse <- function(means, 
                             sd, 
                             alpha = 0.05){
	
	if(!is.vector(means, mode = "numeric") | length(means)!= 2){
		stop("means must be a length 2 numeric vector.")
	}
	if(!is.vector(sd, mode = "numeric") | length(sd)!= 2){
		stop("sd must be a length 2 numeric vector.")
	}
	if(!is.vector(alpha, mode = "numeric") | length(alpha)!= 1){
		stop("alpha must be a length 1 numeric vector.")
	}
	if(alpha > 1 | alpha < 0){
		stop("alpha must take a value between 0 and 1")
	}

	p <- (1-alpha) + (alpha/2)

	a <- qnorm(p)*sd[1]
	b <- qnorm(p)*sd[2]

	t <- seq(0, 2*pi, by=pi/100)

	xt <- means[1] + a*cos(t)
  yt <- means[2] + b*sin(t)

	return(list(x = xt, y = yt))
}

