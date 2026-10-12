
#'Plot of standardized residuals vs. leverage with boundaries for unusual cases
#'
#' @param mod  a regression model from lm( )
#'
#' @return A plot showing standardized residuals versus leverage values with boundaries for unusual cases
#'
#' @description
#' This function produces an plot of standardized residuals versus leverage values for a regression model.
#' Horizontal boundaries identify mild or more extreme standardized residuals.
#' Vertical boundaries identify mild and more severe high leverage points.
#' Curved boundaries identify mild and more severe values of Cook's D.
#'
#' @details
#' The plot shows standardized residuals (vertical) versus leverage values (horizontal) for all cases in a regression model.
#'
#' Horizontal (blue) boundaries mark standardized residuals beyond  +/- 2 (mild) and +/- 3 (more severe).
#'
#' Vertical (green) boundaries mark leverage points beyond 2(k+1)/4 (mild) and 3(k+1)/n (more severe), where k= number of predictors.
#'
#' Curved (red) boundaries for mark influential points beyond 0.5 (mild) and 1.0 (more severe) using Cook's D.
#'
#' Unusual points are labeled with a case number.
#'
#' @examples
#' data(AccordPrice)
#' mod1=lm(Price~Age,data=AccordPrice)
#' cooksplot(mod1)
#'
#' @export
#
cooksplot=function(mod){
  x=NULL     #just to keep R's package check happy
	k=length(mod$coeff)-1
	n=(mod$df)+k+1
	StandardizedResiduals=rstudent(mod)
	Leverage=hatvalues(mod)
	h=max(abs(StandardizedResiduals))
	plot(StandardizedResiduals~Leverage,xlim=c(0.0000001,max(Leverage)),ylim=c(-h,h))
	abline(v=(2*(k+1))/n,lty=2,col="green")
	abline(v=(3*(k+1))/n,lty=5,col="green")
	abline(h=2,lty=2,col="blue")
	abline(h=3,lty=5,col="blue")
	abline(h=-2,lty=2,col="blue")
	abline(h=-3,lty=5,col="blue")
	curve(sqrt((0.5)*((1-x)/x)*(k+1)),lty=2,col="red",add=TRUE)
	curve(-1*sqrt((0.5)*((1-x)/x)*(k+1)),lty=2,col="red",add=TRUE)
	curve(sqrt(((1-x)/x)*(k+1)),lty=5,col="red",add=TRUE)
	curve(-1*sqrt(((1-x)/x)*(k+1)),lty=5,col="red",add=TRUE)
	#mtext(0.5,side=4,col="blue")
	condition=(abs(StandardizedResiduals)>2)+(Leverage>(2*(k+1))/n)
	for(i in 1:n){
		if(condition[i]>0)text(Leverage[i],StandardizedResiduals[i],label=i,pos=2, col="blue")
		}

	}




