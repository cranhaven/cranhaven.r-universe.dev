
#' highCor returns ordered index-pairs of high to low correlation
#'
#' This function uses the correlation matrix C of a sample X, orders
#' all values from the upper triangle of C (excluding the diagonal)
#' from highest to lowest correlation value and returns the indices as
#' a data.frame.
#'
#' When truncated, the result can be used to plot only pairs with high
#' correlation.
#'
#' @param C the correlation matrix of a sample, no attributes need be
#'     present other than dim.
#' @return data.frame with columns i and j, representing the rows and columns of high to low correlation pairs.
#' @export
#' @examples
#' A <- matrix(
#'   c(
#'      1,  -1, 0.1,
#'     -1,   1, 0.4,
#'    0.1, 0.4,   1
#'   ),3,3
#' )
#' print(highCor(A))
highCor <- function(C){
	stopifnot(is.matrix(C))
	if (diff(dim(C))!=0){
		## accidentally used a sample as argument rather than correlation?
		warning("Argument C seems to be a sample rather than its correlation matrix; calculating correlation now.")
		C <-cor(C)
	}
	U <- abs(C * upper.tri(C,diag=FALSE))
	o <- order(as.numeric(U),decreasing=TRUE)
	i <- ((o-1) %% NROW(U))+1
	j <- ((o-1) %/% NCOL(U))+1
	return(data.frame(i=i,j=j))
}

#' showPosterior makes a pairs plot for a sample
#'
#' This function will display the difference between the posterior and
#' prior by plotting the posterior as shaded density plots and the
#' prior as contour lines of level sets. If the two are identical, the
#' lines will be invisible as they blend into the density
#' plot. Otherwise the contour lines will show up as a distinct
#' feature.
#'
#' @param posterior a matrix, each row is a sample member
#' @param prior a matrix of the same size as the posterior
#' @param ... passed to `graphics::pairs()`
#' @return pairs plot object
#' @export
#' @examples
#'   rprior <- rNormalPrior(c(-1,0,1),c(1,2,3))
#'   A <- matrix(rnorm(9),3,3)
#'   A <- (A + t(A))^2/norm(A)^2
#'   X <- rprior(30)
#'   Z <- X %*% A
#'   colnames(Z) <- letters[seq(3)]
#'   colnames(X) <- letters[seq(3)]
#'   ## make a plot:
#'   if (interactive()) showPosterior(Z,X) # this can take a while
showPosterior <- function(posterior, prior,...){
# upper and lower panel defaults
	n_posterior <- NROW(posterior)
	n_prior <- NROW(prior)
	lp <- function(x,y,...){
		rx <- range(x)
		ry <- range(y)
		C <- cor(head(x,n_posterior),head(y,n_posterior)) # in [-1,1]
		i <- 1+round((1+C)*50)            # 1+C in [0,2], (1+C)*50 in [0,100], 1+round((1+C)*50) in [1,101]
		col <- colorspace::diverging_hcl(101,rev=FALSE,palette="Blue Red") # maps to possible correlation values
		usr <- par("usr")
		rect(usr[1], usr[3], usr[2], usr[4], col = col[i], border = NA)
		text(mean(rx),mean(ry),sprintf("%.3f",C),cex=2+round(abs(C)*2))
	}
	up <- function(x,y,...){
		m <- mean(x)
		Qx <- quantile(x,probs=c(0.01,0.99))
		Qy <- quantile(y,probs=c(0.01,0.99))
		dx <- 0.01*diff(Qx)
		dy <- 0.01*diff(Qy)
		LIM <- c(min(Qx)-dx,max(Qx)+dx,min(Qy)-dy,max(Qy)+dy)
		k1 <- MASS::kde2d(
			head(x,n_posterior),
			head(y,n_posterior),
			n=200,
			lims=LIM
		)
		k2 <- MASS::kde2d(
			tail(x,n_prior),
			tail(y,n_prior),
			n=200,
			lims=LIM
		)
		C <- contourLines(k2)
		colPatch   <- c("#FFFFFF",colorspace::sequential_hcl(length(C),rev=TRUE,palette="Blues 3"))
		colContour <- c("#FFFFFF",colorspace::sequential_hcl(length(C),rev=TRUE,palette="Blues 3"))
		image(k1$x,k1$y,k1$z,add=TRUE,col=colPatch)
		for (i in seq_along(C)){
			lines(C[[i]],col=colContour[i])
		}
		usr <- par("usr")
		polygon(
			c(usr[1],usr[1],usr[2],usr[2]),
			c(usr[1],usr[3],usr[3],usr[2]),
			col=rgb(0.2,1,0.2,0.05),
			border=NA
		)
	}
	return(
		pairs(
			rbind(posterior,prior),
			upper.panel=up,
			lower.panel=lp,
			...
		)
	)
}


#' plots a sample in parallel coordinates
#'
#' This function makes a plot that is quite similar to parallel
#' coordinates. It includes information about the prior as error-bars,
#' centered around the prior's median.
#'
#' @param posterior a matrix, with N rows (sample-members), and M columns
#'     (different model parameters). The columns must be named.
#' @param prior a data.frame with at least $median, and $stdv
#'     columns. This data.frame may also include the fields: `color`,
#'     and `colorOutline` to change the prior error-bars.
#' @param color the color of the sample lines, should have some
#'     transparency.
#' @param ... parameters are passed to matplot.
#' @export
#' @return produces a plot
#' @examples
#' rprior <- rNormalPrior(c(-1,0,1),c(1,2,3))
#' A <- matrix(rnorm(9),3,3)
#' A <- (A + t(A))^2/norm(A)^2
#' X <- rprior(1000)
#' Z <- X %*% A
#' colnames(Z) <- letters[seq(3)]
#' pr <- data.frame(median=apply(X,2,median),stdv=apply(X,2,sd))
#' pcDist(Z,pr)
pcDist <- function(posterior,prior,color=rgb(0.5,0.5,0.5,0.05),...){
	if (is.matrix(prior)){
		X <- prior
		prior <- data.frame(median=apply(X,2,median),stdv=apply(X,2,sd))
	}
	pI <- seq(NCOL(posterior))
	stopifnot(all(c("median","stdv") %in% names(prior)))
	m <- prior$median
	sd <- prior$stdv
	u <- m+sd
	l <- m-sd
	names(pI) <- colnames(posterior)

	matplot(pI,t(posterior),type="l",lty=1,col=color,xlab=NA,axes = FALSE,lwd=3,...)
	if ("color" %in% names(prior)){
		pColor <- prior$color
	} else {
		pColor <- "green2"
	}
	if ("colorOutline" %in% names(prior)){
		colorOutline <- prior$colorOutline
	} else {
		pColorOutline <- "black"
	}
	arrows(pI,m,pI,u,angle=90,col=pColorOutline,lwd=2)
	arrows(pI,m,pI,l,angle=90,col=pColorOutline,lwd=2)
	lines(pI,posterior[1,],col="red3",lw=2)
	arrows(pI,m,pI,u,angle=90,col=pColor)
	arrows(pI,m,pI,l,angle=90,col=pColor)
	axis(2)
	axis(1,at=pI,labels=colnames(posterior),xpd=TRUE)
}
