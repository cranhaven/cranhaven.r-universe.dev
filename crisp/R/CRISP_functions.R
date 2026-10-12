##########################################################################################################################################
##########################################################################################################################################

#Functions for crisp R package to perform methods described in "Convex Regression with Interpretable Sharp Partitions"

##########################################################################################################################################
##########################################################################################################################################

##########################################################################################################################################
#### MAIN CRISP FUNCTIONS
### functions in this section:
### crisp.onelambda: fits CRISP for a single lambda value
### crisp.helper: the workhorse function for fitting CRISP, calls crisp.onelambda for each value of lambda
### crisp: this is the function you should call to fit CRISP, it will in turn call the two functions above as needed
##########################################################################################################################################

crisp.onelambda = function(y, X, lambda, rho=0.1, e_abs=10^-4, e_rel=10^-3, varyrho=TRUE, initial.m=NULL, initial.u=NULL, initial.z=NULL, Q, A, z.shrink=NULL) {

	n = sqrt(ncol(A))

	#shrink Q and A if !is.null(z.shrink)
	if (!is.null(z.shrink)==T) {
		blocks = get.blocks(z=z.shrink, n=n)
		Q = sapply(blocks,colReduce,matrix=Q,simplify=T)
		A = sapply(blocks,colReduce,matrix=A,simplify=T)
	}

	converge = FALSE
	if (is.null(initial.m)) m = matrix(0, nrow=ncol(A), ncol=1) else m = initial.m
	if (is.null(initial.z)) z = matrix(0, nrow=nrow(A), ncol=1) else z = initial.z
	if (is.null(initial.u)) u = matrix(0, nrow=nrow(A), ncol=1) else u = initial.u
	n.iter = 0
	rho.old = rho

	indices = seq(1,nrow(A),by=n)

	#matrices used later
	tmp = crossprod(Q) + rho * crossprod(A)
	crossprodQ = crossprod(Q)
	crossprodQy = crossprod(Q,y)
	QRmat = qr(tmp)

	while (converge==F) {
		#step 1: update m
		if (rho.old!=rho) {
			tmp = crossprodQ + rho * crossprod(A)
			QRmat = qr(tmp)
		}
		m = qr.coef(QRmat, crossprodQy + rho * crossprod(A, z-u))
		Am = A %*% m

		#step 2: update z
		z.old = z
		z = matrix(as.vector(sapply(indices,	 function(index, n, vec, lambda, rho)
			update.l2(vec[index:(index+n-1)], lambda, rho), lambda=lambda, rho=rho, n=n, vec= Am+u)),ncol=1)

		#step 3: update u
		u = Am + u - z

		#check convergence
		n.iter = n.iter + 1
		#primal (r) and dual (s) residuals
		r = Am - z
		s = rho * crossprod(A, z.old - z)
		r.norm = sqrt(sum(r^2)); s.norm = sqrt(sum(s^2))
		#thresholds
		e_primal = sqrt(nrow(A)) * e_abs + e_rel * max(c(sqrt(sum((Am)^2)),sqrt(sum(z^2))))
		e_dual = n * e_abs + e_rel * sqrt(sum((rho*crossprod(A, u))^2))
		if ((r.norm <= e_primal) & (s.norm <= e_dual) & n.iter>2) converge = TRUE

		#update rho (and u) when allowing rho to vary
		rho.old = rho

		if (varyrho==T) {
			if (r.norm > (10*s.norm)) {
				rho = 2*rho; u = u/2
			} else if (s.norm > (10*r.norm)) {
				rho = rho/2; u = 2*u
			}
		}
	}

	obj = calc.obj(y=y, X=X, m=m, lambda=lambda, Q=Q, A=A)

	if (!is.null(z.shrink)==T) {
		m.shrunk = m
		m = rep(NA, n^2)
		for (i in 1:length(blocks)) m[blocks[[i]]] = m.shrunk[i]
	}

	return(list(M=matrix(m, nrow=n), z=z, u=u, n.iter=n.iter, obj.value=obj, y=y, X=X, lambda=lambda, rho=rho, e_abs=e_abs, e_rel=e_rel, z.shrink=z.shrink))
}

crisp.helper = function(y, X, lambda.seq, rho=0.1, e_abs=10^-4, e_rel=10^-3, varyrho=TRUE, z.shrink=NULL, initial.M.list=NULL, initial.u.mat=NULL, initial.z.mat=NULL, A, Q) {

	#make sure lambda.seq is decreasing
	lambda.seq = sort(lambda.seq, decreasing=TRUE)

	#initialize
	M.hat.list = vector("list",length(lambda.seq))
	z.hat.mat <- u.hat.mat <- matrix(NA, nrow=nrow(A), ncol=length(lambda.seq))
	n.iter.vec <- obj.vec <- rep(NA, length(lambda.seq))

	#fit model
	for (i in 1:length(lambda.seq)) {

		if (is.null(z.shrink)) {
			if (i==1) {
				out = crisp.onelambda(y=y, X=X, lambda=lambda.seq[i], rho=rho, e_abs=e_abs, e_rel=e_rel, varyrho=varyrho,
					initial.m=NULL, initial.z=NULL, initial.u=NULL, Q=Q, A=A)
			} else {
				out = crisp.onelambda(y=y, X=X, lambda=lambda.seq[i], rho=out$rho, e_abs=e_abs, e_rel=e_rel, varyrho=varyrho,
				initial.m=as.vector(M.hat.list[[i-1]]), initial.z=z.hat.mat[,i-1], initial.u=u.hat.mat[,i-1], Q=Q, A=A)
			}
		} else {
			blocks = get.blocks(z=z.shrink[,i], n=sqrt(ncol(A)))
			initial.m = matrix(NA, nrow=length(blocks), ncol=1)
			for (j in 1:length(blocks)) initial.m[j] = as.vector(initial.M.list[[i]])[blocks[[j]][1]]
			out = crisp.onelambda(y=y, X=X, lambda=lambda.seq[i], rho=rho, e_abs=e_abs, e_rel=e_rel, varyrho=varyrho,
				initial.m=initial.m, initial.z=initial.z.mat[,i], initial.u=initial.u.mat[,i], Q=Q, A=A, z.shrink=z.shrink[,i])
		}

		M.hat.list[[i]] = out$M
		z.hat.mat[,i] = as.vector(out$z)
		u.hat.mat[,i] = as.vector(out$u)
		n.iter.vec[i] = out$n.iter; obj.vec[i] = out$obj.value
	}

	return(list(M.hat.list=M.hat.list,z.hat.mat=z.hat.mat,u.hat.mat=u.hat.mat,n.iter.vec=n.iter.vec, obj.vec=obj.vec, y=y, X=X, lambda.seq=lambda.seq, rho=rho, e_abs=e_abs, e_rel=e_rel, z.shrink=z.shrink))
}

#' Convex Regression with Interpretable Sharp Partitions (CRISP).
#'
#' This function implements CRISP, which considers the problem of predicting an outcome variable on the basis of two covariates, using an interpretable yet non-additive model.
#' CRISP partitions the covariate space into blocks in a data-adaptive way, and fits a mean model within each block. Unlike other partitioning methods,
#' CRISP is fit using a non-greedy approach by solving a convex optimization problem, resulting in low-variance fits.
#' More details are provided in Petersen, A., Simon, N., and Witten, D. (2016). Convex Regression with Interpretable Sharp Partitions. Journal of Machine Learning Research, 17(94): 1-31 <http://jmlr.org/papers/volume17/15-344/15-344.pdf>.
#'
#' @param y An n-vector containing the response.
#' @param X An n x 2 matrix with each column containing a covariate.
#' @param q The desired granularity of the CRISP fit, \code{M.hat}, which will be a \code{q} by \code{q} matrix. \code{M.hat}
#' is a mean matrix whose element \code{M.hat[i,j]} contains the mean for pairs of covariate values within a quantile range
#' of the observed predictors \code{X[,1]} and \code{X[,2]}. For example, \code{M.hat[1,2]} represents the
#' mean of the observations with the first covariate value less than the \code{1/q}-quantile of \code{X[,1]},
#' and the second covariate value between the \code{1/q}- and \code{2/q}-quantiles of \code{X[,2]}.
#' If left \code{NULL}, then \code{q=n} is used when n<100, and \code{q=100} is used when n>=100.
#' We recommend using \code{q<=100} as higher values take longer to fit and provide an unneeded amount of granularity.
#' @param lambda.min.ratio The smallest value for \code{lambda.seq}, as a fraction of the maximum lambda value, which is the data-derived
#' smallest value for which the fit is a constant value. The default is 0.01.
#' @param n.lambda The number of lambda values to consider - the default is 50.
#' @param lambda.seq A user-supplied sequence of positive lambda values to consider. The typical usage is to calculate
#' \code{lambda.seq} using \code{lambda.min.ratio} and \code{n.lambda}, but providing \code{lambda.seq} overrides this. If provided,
#' \code{lambda.seq} should be a decreasing sequence of values, since CRISP relies on warm starts for speed.
#' Thus fitting the model for a whole sequence of lambda values is often faster than fitting for a single lambda value.
#' @param e_abs,e_rel Values used in the stopping criterion for our ADMM algorithm, and discussed in Appendix C.2 of the CRISP paper.
#' @param rho The penalty parameter for our ADMM algorithm. The default is 0.1.
#' @param varyrho Should \code{rho} be varied from iteration to iteration? This is discussed in Appendix C.3 of the CRISP paper.
#' @param double.run The initial complete run of our ADMM algorithm will yield sparsity in z_{1i} and z_{2i}, but not
#' necessarily exact equality of the rows and columns of \code{M.hat}. If \code{double.run} is \code{TRUE}, then the algorithm
#' is run a second time to obtain \code{M.hat} with exact equality of the appropriate rows and columns. This issue
#' is discussed further in Appendix C.4 of the CRISP paper.
#'
#' @return An object of class \code{crisp}, which can be summarized using \code{\link{summary}}, plotted using \code{\link{plot}}, and used to predict outcome values for new covariates using \code{\link{predict}}.
#' \itemize{
#' \item{\code{M.hat.list}: }{A list of length \code{n.lambda} giving \code{M.hat} for each value of \code{lambda.seq}.}
#' \item{\code{num.blocks}: }{A vector of length \code{n.lambda} giving the number of blocks in \code{M.hat} for each value of \code{lambda.seq}.}
#' \item{\code{obj.vec}: }{A vector of length \code{n.lambda} giving the value of the objective of Eqn (4) in the CRISP paper for each value of \code{lambda.seq}.}
#' \item{Other elements: }{As specified by the user.}
#' }
#'
#'
#' @seealso \code{\link{crispCV}}, \code{\link{plot}}, \code{\link{summary}}, \code{\link{predict}}
#'
#' @import Matrix
#' @examples
#' \dontrun{
#' #See ?'crisp-package' for a full example of how to use this package
#'
#' #generate data (using a very small 'n' for illustration purposes)
#' set.seed(1)
#' data <- sim.data(n = 15, scenario = 2)
#'
#' #fit model for a range of tuning parameters, i.e., lambda values
#' #lambda sequence is chosen automatically if not specified
#' crisp.out <- crisp(X = data$X, y = data$y)
#' }
#' @export
crisp = function(y, X, q=NULL, lambda.min.ratio = 0.01, n.lambda = 50, lambda.seq = NULL, rho=0.1, e_abs=10^-4, e_rel=10^-3, varyrho=TRUE, double.run=FALSE) {

  call = match.call()

  n = length(y)
  if (is.null(q)) {
    if (n<100) q = n  else q = 100
  }
  if (is.null(lambda.seq)) {
    max.lam = max.lambda(X=X, y=y)
    lambda.seq = exp(seq(log(max.lam), log(max.lam * lambda.min.ratio), len = n.lambda))
  }

	#checks
	if (length(y)!=nrow(X)) stop("The length of 'y' must equal the number of rows of 'x'")
	if (length(lambda.seq)==1) stop("Provide a sequence of decreasing values for 'lambda.seq'")
	if (min(lambda.seq)<=0) stop("Values in 'lambda.seq' must be positive")
	if (e_abs<=0 | e_rel<=0) stop("Values for 'e_abs' and 'e_rel' must be positive")
  if (!is.logical(varyrho)) stop("Specify TRUE or FALSE for 'varyrho'")
  if (!is.logical(double.run)) stop("Specify TRUE or FALSE for 'double.run'")

	if (q!=n) {
		n = q
		q.seq = c(0, 1:(q-1)/q, 1)
		block.X1 = findInterval(X[,1], stats::quantile(X[,1], q.seq, type=8), all.inside=T)
		block.X2 = findInterval(X[,2], stats::quantile(X[,2], q.seq, type=8), all.inside=T)
		blocks = block.X1 + n * (block.X2 - 1)
		Q = sparseMatrix(i=1:nrow(X), j=blocks, dims=c(nrow(X),n^2))
		Q = methods::as(Q, "dgCMatrix")
	} else {
		Q = get.Q(n=n, X=X, sparse=T)
	}

	A = get.A(n=n, sparse=T)

	out.unshrunk = crisp.helper(y=y, X=X, lambda.seq=lambda.seq, rho=rho, e_abs=e_abs, e_rel=e_rel, varyrho=varyrho, A=A, Q=Q)
	#n.iter1 = out.unshrunk$n.iter.vec

	if (double.run==TRUE) {

		out.shrunk = crisp.helper(y=y, X=X, lambda.seq=lambda.seq, rho=rho, e_abs=e_abs, e_rel=e_rel,
			varyrho=varyrho, z.shrink=out.unshrunk$z.hat.mat, initial.M.list=out.unshrunk$M.hat.list,
			initial.u.mat=out.unshrunk$u.hat.mat, initial.z.mat=out.unshrunk$z.hat.mat, A=A, Q=Q)

		num.blocks = apply(out.shrunk$z.hat.mat, 2, function(z, n) length(get.blocks(z=z, n=n)), n=n)
		M.hat.list = out.shrunk$M.hat.list
		obj.vec = out.shrunk$obj.vec

	} else {

		num.blocks = apply(out.unshrunk$z.hat.mat, 2, function(z, n) length(get.blocks(z=z, n=n)), n=n)
		M.hat.list = out.unshrunk$M.hat.list
		obj.vec = out.unshrunk$obj.vec
	}

	fit = list(lambda.seq=lambda.seq, M.hat.list=M.hat.list, num.blocks=num.blocks, obj.vec=obj.vec, y=y, X=X, rho=rho, e_abs=e_abs, e_rel=e_rel, varyrho=varyrho, double.run=double.run, call=call)
	class(fit) = "crisp"
	return(fit)
}

##########################################################################################################################################
#### METHODS FOR CRISP OBJECT
##########################################################################################################################################

get.cell = function(block.X1, block.X2, ntilde) {

	index = block.X1 + ntilde * (block.X2 - 1)
	return(index)
}

#' Predicts Observations for a New Covariate Matrix using Fit from \code{\link{crisp}} or \code{\link{crispCV}}.
#'
#' This function makes predictions for a specified covariate matrix for a fit of the class \code{crispCV}, or class \code{crisp} with a user-specified tuning parameter.
#'
#' @param object An object of class \code{crisp} or \code{crispCV}, which result from running the \code{\link{crisp}} or \code{\link{crispCV}} functions, respectively.
#' @param new.X The covariate matrix for which to make predictions.
#' @param lambda.index The index for the desired value of lambda, i.e., \code{object$lambda.seq[lambda.index]}.
#' @param ... Additional arguments to be passed, which are ignored in this function.
#' @return A vector containing the fitted y values for \code{new.X}.
#'
#' @details The ith prediction is made to be the value of \code{object$M.hat.list[[lambda.index]]} corresponding to the pair of covariates closest (in Euclidean distance) to \code{new.X[i,]}.
#'
#' @import Matrix
#' @examples
#' \dontrun{
#' #See ?'crisp-package' for a full example of how to use this package
#'
#' #generate data (using a very small 'n' for illustration purposes)
#' set.seed(1)
#' data <- sim.data(n = 15, scenario = 2)
#'
#' #fit model for a range of tuning parameters, i.e., lambda values
#' #lambda sequence is chosen automatically if not specified
#' crisp.out <- crisp(X = data$X, y = data$y)
#' #or fit model and select lambda using 2-fold cross-validation
#' #note: use larger 'n.fold' (e.g., 10) in practice
#' crispCV.out <- crispCV(X = data$X, y = data$y, n.fold = 2)
#'
#' #we can make predictions for a covariate matrix with new observations
#' #new.X with 20 observations
#' new.data <- sim.data(n = 20, scenario = 2)
#' new.X <- new.data$X
#' #these will give the same predictions:
#' yhat1 <- predict(crisp.out, new.X = new.X, lambda.index = crispCV.out$index.cv)
#' yhat2 <- predict(crispCV.out, new.X = new.X)
#' }
#' @name predict
NULL

#' @rdname predict
#' @method predict crisp
#' @export
predict.crisp = function(object, new.X, lambda.index, ...) {

  if (class(object)!="crisp") stop("The class of 'object' must be 'crisp'")
  if (ncol(new.X)!=2) stop("There should be 2 columns in 'new.X'")
  if (lambda.index<1 | lambda.index>length(object$lambda.seq)) stop(paste0("Specify 'lambda.index' in 1,...,",length(object$lambda.seq)))

	x1.sort = sort(object$X[,1]); x2.sort = sort(object$X[,2])

	ntilde = nrow(object$M.hat.list[[1]])
	q.seq = c(0, 1:(ntilde-1)/ntilde, 1)
	block.X1 = findInterval(new.X[,1], stats::quantile(x1.sort, q.seq, type=8), all.inside=T)
	block.X2 = findInterval(new.X[,2], stats::quantile(x2.sort, q.seq, type=8), all.inside=T)

	closest = sapply(1:nrow(new.X), function(block.X1, block.X2, ntilde, i) get.cell(block.X1[i], block.X2[i], ntilde), ntilde=ntilde, block.X1=block.X1, block.X2=block.X2)
	y.hat.new = as.vector(object$M.hat.list[[lambda.index]])[closest]

	return(y.hat.new)
}

#' @rdname predict
#' @method predict crispCV
#' @export
predict.crispCV = function(object, new.X, ...) {

  if (class(object)!="crispCV") stop("The class of 'object' must be 'crispCV'")
  if (ncol(new.X)!=2) stop("There should be 2 columns in 'new.X'")

  y.hat.new = predict.crisp(object$crisp.out, new.X, object$index.cv)
  return(y.hat.new)
}

col2hex = function(color) {
  rgb.col = grDevices::col2rgb(color)[,1]
  hex.col = grDevices::rgb(rgb.col[1], rgb.col[2], rgb.col[3], max=255)
  return(hex.col)
}

#' Plots Fit from \code{\link{crisp}} or \code{\link{crispCV}}.
#'
#' This function plots fit of the class \code{crispCV}, or class \code{crisp} with a user-specified tuning parameter.
#'
#' @param x An object of class \code{crisp} or \code{crispCV}, which result from running the \code{\link{crisp}} or \code{\link{crispCV}} functions, respectively.
#' @param lambda.index The index for the desired value of lambda, i.e., \code{x$lambda.seq[lambda.index]}.
#' @param title The title of the plot. By default, the value of lambda is noted.
#' @param x1lab The axis label for the first covariate. By default, it is "X1".
#' @param x2lab The axis label for the second covariate. By default, it is "X2".
#' @param min,max The minimum and maximum y-values, respectively, to use when plotting the fit. By default, they are chosen to be the minimum and maximum of all of the fits, i.e., the minimum and maximum of \code{unlist(x$M.hat.list)}.
#' @param cex.axis The magnification to be used for axis annotation relative to the current setting of \code{cex}.
#' @param cex.lab The magnification to be used for x and y labels relative to the current setting of \code{cex}.
#' @param color1,color2,color3 The colors to use to create the color gradient for plotting the response values. At least the first two must be specified, or the defaults of \code{"seagreen1"}, \code{"steelblue1"}, and \code{"darkorchid4"} will be used.
#' @param ... Additional arguments to be passed, which are ignored in this function.
#' @return None.
#'
#' @examples
#' \dontrun{
#' #See ?'crisp-package' for a full example of how to use this package
#'
#' #generate data (using a very small 'n' for illustration purposes)
#' set.seed(1)
#' data <- sim.data(n = 15, scenario = 2)
#'
#' #fit model for a range of tuning parameters, i.e., lambda values
#' #lambda sequence is chosen automatically if not specified
#' crisp.out <- crisp(X = data$X, y = data$y)
#' #or fit model and select lambda using 2-fold cross-validation
#' #note: use larger 'n.fold' (e.g., 10) in practice
#' crispCV.out <- crispCV(X = data$X, y = data$y, n.fold = 2)
#'
#' #plot the estimated relationships between two predictors and outcome
#' #do this for a specific fit
#' plot(crisp.out, lambda.index = 25)
#' #or for the fit chosen using cross-validation
#' plot(crispCV.out)
#' }
#' @name plot
NULL

#' @rdname plot
#' @method plot crisp
#' @export
plot.crisp = function(x, lambda.index, title=NULL, x1lab=NULL, x2lab=NULL, min=NULL, max=NULL, cex.axis=1, cex.lab=1, color1="seagreen1", color2="steelblue1", color3="darkorchid4", ...) {

  if (class(x)!="crisp") stop("The class of 'x' must be 'crisp'")
  if (lambda.index<1 | lambda.index>length(x$lambda.seq)) stop(paste0("Specify 'lambda.index' in 1,...,",length(x$lambda.seq)))

  graphics::layout(matrix(c(1,1,1,1,2), nrow=1))
  #grid of X values to predict on
  dim = 50
  x1.sort = seq(min(x$X[,1]), max(x$X[,1]), len=dim)
  x2.sort = seq(min(x$X[,2]), max(x$X[,2]), len=dim)
  grid.complete = cbind(rep(x1.sort,dim),rep(x2.sort,each=dim))

  #predict for grid of X values
  M.fill = predict.crisp(x, new.X=grid.complete, lambda.index=lambda.index)
  if (!is.null(min)) {
    if (min>min(M.fill)) {
      warning("The value for 'min' provided is larger than the minimum of the data, so 'min' was reset.")
      min = min(M.fill)
    }
  }
  if (!is.null(max)) {
    if (max<max(M.fill)) {
      warning("The value for 'max' provided is smaller than the maximum of the data, so 'max' was reset.")
      max = max(M.fill)
    }
  }

  if (is.null(x1lab)) x1lab = expression(X[1])
  if (is.null(x2lab)) x2lab = expression(X[2])
  if (is.null(min)) min = min(unlist(x$M.hat.list))
  if (is.null(max)) max = max(unlist(x$M.hat.list))
  val = signif(x$lambda.seq[lambda.index], 3)
  if (is.null(title)) title = bquote(lambda == .(val))
  if (is.null(color3)) colors = c(col2hex(color1), col2hex(color2)) else colors = c(col2hex(color1), col2hex(color2), col2hex(color3))
  pal = grDevices::colorRampPalette(colors)
  col = pal(1001)

  #normalize
  M.fill.norm = (M.fill - min) / (max - min)

  margin.x1 = abs(grid.complete[1,1] - grid.complete[2,1])/2
  margin.x2 = abs(grid.complete[1,2] - grid.complete[1+sqrt(nrow(grid.complete)),2])/2

  #plot x1 as y (with ylim reversed) and x2 as x
  graphics::plot(1,type="n",xlim=c(min(grid.complete[,2]),max(grid.complete[,2])),ylim=c(max(grid.complete[,1]),min(grid.complete[,1])),
       axes=T,xlab=x2lab,ylab=x1lab,main=title,cex.axis=cex.axis,cex.lab=cex.lab)
  graphics::box()
  for (i in 1:length(M.fill)) {
    color = col[1001-round(M.fill.norm[i],3)*1000]
    graphics::rect(xleft=grid.complete[i,2]-margin.x2, xright=grid.complete[i,2]+margin.x2,
         ybottom=grid.complete[i,1]+margin.x1, ytop=grid.complete[i,1]-margin.x1, border=NA, col=color)
  }
  default = graphics::par()$mar
  graphics::par(mar=c(0.2,0.2,0.2,2))
  legend_image = grDevices::as.raster(matrix(col),ncol=1)
  graphics::plot(c(0,1.5),c(0,1),type="n",axes=F,xlab="",ylab="",main="",cex.main=2, cex.lab=1.5, cex.axis=1.5)
  graphics::text(x=1.25, y=seq(0,1,l=5), labels=round(seq(min,max,l=5),2), cex=1)
  graphics::rasterImage(legend_image,0,0,1,1)
  graphics::par(mar=default)
}

#' @rdname plot
#' @method plot crispCV
#' @export
plot.crispCV = function(x, title=NULL, x1lab=NULL, x2lab=NULL, min=NULL, max=NULL, cex.axis=1, cex.lab=1, color1="seagreen1", color2="steelblue1", color3="darkorchid4", ...) {

  if (class(x)!="crispCV") stop("The class of 'x' must be 'crispCV'")

  plot.crisp(x$crisp.out, lambda.index=x$index.cv, title=title, x1lab=x1lab, x2lab=x2lab, min=min, max=max, cex.axis=cex.axis, cex.lab=cex.lab, color1=color1, color2=color2, color3=color3)

}

#' Plots Cross-Validation Curve for \code{\link{crispCV}}.
#'
#' This function plots the cross-validation curve for a series of models fit using \code{\link{crispCV}}. The cross-validation error with +/-1 standard error is plotted for each value of lambda considered in the call to \code{\link{crispCV}} with a dotted vertical line indicating the chosen lambda.
#'
#' @param x An object of class \code{cvError}, which results from calling \code{\link{summary}} on an object of class \code{\link{crispCV}}.
#' @param showSE A logical value indicating whether the standard error of the curve should be plotted.
#' @param ... Additional arguments to be passed, which are ignored in this function.
#' @return None.
#'
#' @examples
#' \dontrun{
#' #See ?'crisp-package' for a full example of how to use this package
#'
#' #generate data (using a very small 'n' for illustration purposes)
#' set.seed(1)
#' data <- sim.data(n = 15, scenario = 2)
#'
#' #fit model and select lambda using 2-fold cross-validation
#' #note: use larger 'n.fold' (e.g., 10) in practice
#' crispCV.out <- crispCV(X = data$X, y = data$y, n.fold = 2)
#'
#' #plot the cross-validation error
#' plot(summary(crispCV.out))
#' }
#' @method plot cvError
#' @export
plot.cvError = function(x, showSE=T, ...) {

  if (class(x)!="cvError") stop("The class of 'x' must be 'cvError'. Standard usage is to call 'plot(summary(crispCV.out))' where 'crispCV.out' is the object returned from calling 'crispCV()'")
  if (!is.logical(showSE)) stop("Specify TRUE or FALSE for 'showSE'")

  min.y = min(x$mean.cv.error - x$se.cv.error); max.y = max(x$mean.cv.error + x$se.cv.error)

  graphics::par(mfrow=c(1,1))
  graphics::plot(x=1,type="n",xlim=c(min(log(x$crisp.out$lambda.seq)),max(log(x$crisp.out$lambda.seq))),ylim=c(min.y,max.y),ylab="Cross-Validation Error",xlab=expression(paste("log ",lambda)))

  graphics::points(log(x$crisp.out$lambda.seq), x$mean.cv.error, cex=1.5, col="red",pch=16)

  if (showSE==T) graphics::arrows(x0=log(x$crisp.out$lambda.seq),x1=log(x$crisp.out$lambda.seq),y0=x$mean.cv.error-x$se.cv.error,y1=x$mean.cv.error+x$se.cv.error,angle=90,col="red",length=0.05,code=3,lwd=1.6)

  graphics::abline(v=log(x$crisp.out$lambda.seq[x$index.cv]),lty=2)
}

#' Summarizes Fit from \code{\link{crisp}} or \code{\link{crispCV}}.
#'
#' This function summarizes fit of the class \code{crispCV} or \code{crisp}.
#'
#' @param object An object of class \code{crisp} or \code{crispCV}, which result from running the \code{\link{crisp}} or \code{\link{crispCV}} functions, respectively.
#' @param lambda.index The index for the desired value of lambda, i.e., \code{object$lambda.seq[lambda.index]}. By default, fits for all values of lambda are summarized.
#' @param ... Additional arguments to be passed, which are ignored in this function.
#' @return None.
#'
#' @examples
#' \dontrun{
#' #See ?'crisp-package' for a full example of how to use this package
#' #generate data (using a very small 'n' for illustration purposes)
#' set.seed(1)
#' data <- sim.data(n = 15, scenario = 2)
#'
#' #fit model for a range of tuning parameters, i.e., lambda values
#' #lambda sequence is chosen automatically if not specified
#' crisp.out <- crisp(X = data$X, y = data$y)
#' #or fit model and select lambda using 2-fold cross-validation
#' #note: use larger 'n.fold' (e.g., 10) in practice
#' crispCV.out <- crispCV(X = data$X, y = data$y, n.fold = 2)
#'
#' #summarize all of the fits
#' summary(crisp.out)
#' #or just summarize a single fit
#' #we examine the fit with an index of 25. that is, lambda of
#' crisp.out$lambda.seq[25]
#' summary(crisp.out, lambda.index = 25)
#' #lastly, we can summarize the fit chosen using cross-validation
#' summary(crispCV.out)
#' }
#' @name summary
NULL

#' @rdname summary
#' @method summary crisp
#' @export
summary.crisp = function(object, lambda.index=NULL, ...) {
  cat("Call: \n")
  print(object$call)
  args = match.call()
  if (class(object)!="crisp") stop("Provide 'object' of class 'crisp'")

  if (is.null(lambda.index)) { #summarize fits for all lambda values
    cat(paste0("\nCRISP was fit for a sequence of ",length(object$lambda.seq)," lambda values.\n\n"))

    sparsity.mat = cbind(round(unique(object$lambda.seq),3), object$num.blocks)
    colnames(sparsity.mat) = c("Lambda", "Number of blocks")
    cat("The number of blocks in the fit for each tuning parameter value:\n")
    print(sparsity.mat, max=1000)

  } else { #summarize a specific fit
    if (lambda.index<1 | lambda.index>length(object$lambda.seq)) stop(paste0("Specify 'lambda.index' in 1,...,",length(object$lambda.seq)))

    cat("\nThe requested CRISP fit corresponds to:\n")
    cat(paste("lambda = ",round(object$lambda.seq[lambda.index],3),sep=""))
    cat(paste("\n\nThe number of blocks in the fit was ",object$num.blocks[lambda.index],".",sep=""))

    args.t = as.character(args)
    cat(paste("\n\nUse 'plot(",args.t[[2]],",",args.t[[3]],")' to plot the fit.\n",sep=""))
  }
}

#' @rdname summary
#' @method summary crispCV
#' @export
summary.crispCV = function(object, ...) {

  if (class(object)!="crispCV") stop("The class of 'object' must be 'crispCV'")

  cat("Call: \n")
  print(object$call)
  args = match.call()

  cat("\nCRISP was fit for a sequence of lambda values:\n\n")
  cat(round(object$crisp.out$lambda.seq,3))

  cat(paste("\n\nCross-validation with K=",object$n.fold," folds was used to choose lambda.",sep=""))
  if (object$within1SE==T) {
    cat("\nLambda was chosen to be the largest value with CV error within one standard error of the minimum CV error.\n")
    cat(paste("\nThe chosen lambda was ",round(object$lambda.cv,3),".",sep=""))
    cat(paste("\nThe number of blocks in the fit was ",object$crisp.out$num.blocks[object$index.cv],".",sep=""))
  } else {
    cat("\nLambda was chosen to be the value with the minimum CV error.\n")
    cat(paste("\nThe chosen lambda was ",round(object$lambda.cv,3),".\n",sep=""))
    cat(paste("\nThe number of blocks in the fit was ",object$crisp.out$num.blocks[object$index.cv],".",sep=""))
  }
  cat(paste("\n\nUse 'plot(",args$object,")' to plot the fit, and 'plot(summary(",args$object,"))' to plot the CV error curve.\n",sep=""))

  cverror = object
  class(cverror) = "cvError"
  invisible(cverror)
}

##########################################################################################################################################
#### OTHER FUNCTIONS
##########################################################################################################################################

mse = function(y, y.hat) {
	sum((y - y.hat)^2)/length(y)
}

max.lambda = function(X, y, A=NULL, Q=NULL) {

	n = length(y)
	if (is.null(A)) A = get.A(n=n, sparse=F)
	if (is.null(Q)) Q = get.Q(n=n, X=X, sparse=F)
	indices = seq(1,2*n*(n-1),by=n)

	stack = rbind(A,Q)
	M = -cbind(matrix(0,nrow=n,ncol=2*n*(n-1)),diag(n)) + Q %*% MASS::ginv(stack)
	w = MASS::ginv(M) %*% y
	u_A = (cbind(diag(2*n*(n-1)), matrix(0,nrow=2*n*(n-1),ncol=n)) - A %*% MASS::ginv(stack)) %*% w
	lambda = max(sapply(indices, function(index, y) sqrt(sum((y[index:(index+n-1)])^2)), y=u_A))

	return(lambda)
}

get.A = function(n, sparse=T) {

	index1 = rep(1:(n-1),each=n)
	index2 = rep(1:n,(n-1))
	A.i = c((index1-1)*n+index2, n*(n-1)+(index1-1)*n+index2,
		(index1-1)*n+index2, n*(n-1)+(index1-1)*n+index2)
	A.j = c((index2-1)*n+index1, (index1-1)*n+index2,
		(index2-1)*n+index1+1, index1*n+index2)
	A.x = c(rep(1,2*n*(n-1)), rep(-1,2*n*(n-1)))

	if (sparse==T) {
		A = sparseMatrix(i=A.i, j=A.j, x=A.x, dims=c(2*n*(n-1),n^2))
	} else if (sparse==F) {
		A = matrix(0, nrow=2*n*(n-1), ncol=n^2)
		for (l in 1:length(A.i)) A[A.i[l],A.j[l]] = A.x[l]
	}
	return(A)
}

get.Q = function(n, X, sparse=T) {

	Q.i = 1:n; Q.j = rank(X[,1]) + (rank(X[,2])-1)*n
	if (sparse==T) {
		Q = sparseMatrix(i=Q.i, j=Q.j, dims=c(n,n^2))
		Q = methods::as(Q, "dgCMatrix")
	} else if (sparse==F) {
		Q = matrix(0, nrow=n, ncol=n^2)
		for (l in Q.i) Q[Q.i[l],Q.j[l]] = 1
	}
	return(Q)
}

get.blocks = function(z,n) {

	indices = seq(1,n*(n-1),by=n)

	rows.same = as.vector(sapply(indices, function(index, vec, n)
		ifelse(sum((vec[index:(index+n-1)])^2)==0,1,0),
		vec=z[1:(n*(n-1))], n=n))

	cols.same = as.vector(sapply(indices, function(index, vec, n)
		ifelse(sum((vec[index:(index+n-1)])^2)==0,1,0),
		vec=z[(1+n*(n-1)):(2*n*(n-1))], n=n))

	blocks = vector("list")
	elements = matrix(1:(n^2),nrow=n)
	col = 1

	while (col<=n) {

		#check which columns are the same
		k_c = 0
		while(cols.same[col+k_c]!=0 & (col+k_c)<n) k_c = k_c + 1

		row = 1

		while (row<=n) {

			#check which rows are the same
			k_r = 0
			while(rows.same[row+k_r]!=0 & (row+k_r)<n) k_r = k_r + 1

			#add to 'blocks'
			blocks[[length(blocks)+1]] = as.vector(elements[row:(row+k_r),col:(col+k_c)])

			#adjust row counter
			row = row + k_r + 1
		}

		#adjust column counter
		col = col + k_c + 1
	}

	return(blocks)
}

colReduce = function(matrix,indices) {
	if(length(indices)==1) {
		col = matrix[,indices]
	} else col = rowSums(matrix[,indices])
	return(col)
}

update.l2 = function(vec, lambda, rho) {
	norm.vec = sqrt(sum(vec^2))
	z_chunk = vec * max(c(1 - lambda/(rho*norm.vec), 0))
	return(z_chunk)
}

calc.obj = function(y, X, m, lambda, Q=NULL, A=NULL) {

	n = length(y)
	indices = seq(1,2*n*(n-1),by=n)

	if (is.null(Q)) Q = get.Q(n=n, X=X, sparse=TRUE)
	if (is.null(A)) A = get.A(n=n, sparse=TRUE)

	penalty = Reduce('+',sapply(indices, function(index, vec) sqrt(sum((vec[index:(index+n-1)])^2)), vec=A%*%m))
	obj = 0.5 * sum((y - Q %*% m)^2) + lambda * penalty

	return(obj)
}

##########################################################################################################################################
#### FUNCTIONS TO GENERATE DATA
##########################################################################################################################################

mean.model = function(x1,x2,scenario) {

	if (scenario==1) {
		#additive model
		if (x1*x2 > 0) mean = 2*sign(x1) else mean = 0
	} else if (scenario==2) {
		#interaction model
		if (x1*x2 > 0) mean = -2 else mean = 2
		mean = mean/sqrt(2)
	} else if (scenario==3) {
		#tetris model
		if (x1<(-5/6)) {
			if (x2<(-1.25)) mean = -3 else mean = 1
		} else if (x1<5/6) {
			if (x2<0) mean = -2 else mean =2
		} else {
			if (x2<1.25) mean = -1 else mean = 3
		}
		mean = mean/sqrt(5/3)
	} else if (scenario==4) {
		#smooth model
		d = 3
		mean  = 10/(((x1-2.5)/d)^2 + ((x2-2.5)/d)^2 + 1) + 10/(((x1+2.5)/d)^2 + ((x2+2.5)/d)^2 + 1)
		mean = mean - 8.476032; mean = mean/ sqrt(1.936208/2)
	} else if (scenario==5) {
		if ((x1<(-0.83) & x2<(-0.83)) |  (x1>(-0.83) & x2>(-0.83))) mean = -2 else mean = 2
		mean = mean + 0.2048
		mean = mean/sqrt(3.958057/2)
	}
	return(mean)
}

#' Simulate Data to Use with \code{\link{crisp}}.
#'
#' This function generates data according to the simulation scenarios considered in Section 3 of the CRISP paper (and plotted in Figure 2 of the paper).
#'
#' @param n The number of observations.
#' @param scenario The simulation scenario to use. Options are 1 (additive model), 2 (interaction model), 3 ('tetris' model), or 4 (smooth model), which correspond to the simulation scenarios of Section 3 of the CRISP paper. Each scenario has two covariates.
#' @param noise The standard deviation of the normally-distributed noise that is added to the signal.
#' @param X The \code{n} x 2 covariate matrix, which is automatically generated if not specified.
#'
#' @return A list containing:
#' \itemize{
#' \item{\code{X}: }{An \code{n} x 2 covariate matrix.}
#' \item{\code{y}: }{An \code{n}-vector containing the response values.}
#' \item{Other elements: }{As specified by the user.}
#' }
#'
#' @seealso \code{\link{crisp}}, \code{\link{crispCV}}
#'
#' @examples
#' #See ?'crisp-package' for a full example of how to use this package
#'
#' #generate data (using a very small 'n' for illustration purposes)
#' set.seed(1)
#' data <- sim.data(n = 15, scenario = 2)
#'
#' #plot the mean model for the scenario from which we generated data
#' plot(data)
#' @export
sim.data = function(n, scenario, noise=1, X=NULL) {

  #checks
  if (!(scenario %in% 1:4)) stop("Specify 1, 2, 3, or 4 for 'scenario'")
  if (!is.null(X)) {
    if (ncol(X)!=2) stop("There should be 2 columns in 'X'")
    if (nrow(X)!=n) stop("There should be 'n' rows in 'X'")
  }

	if (is.null(X)) X = matrix(stats::runif(n=n*2,min=-2.5,max=2.5), nrow=n, ncol=2)
	y = sapply(1:nrow(X),function(index,matrix,scenario) mean.model(matrix[index,1],matrix[index,2],scenario),matrix=X,scenario=scenario) + stats::rnorm(n,sd=noise)

	if (noise!=0) print("See example in '?sim.data' for code to plot the mean model.")

	out = vector("list")
	out$X = X
	out$y = y
	out$scenario = scenario
	out$noise = noise
	class(out) = "sim.data"
	return(out)
}

#' Plot Mean Model for Data.
#'
#' This function plots the mean model for the scenario from which data was generated using \code{\link{sim.data}}.
#'
#' @param x An object of class \code{sim.data}, which results from running the \code{\link{sim.data}} function.
#' @param ... Additional arguments to be passed, which are ignored in this function.
#' @return None.
#'
#' @seealso \code{\link{sim.data}}
#'
#' @examples
#' #See ?'crisp-package' for a full example of how to use this package
#'
#' #generate data (using a very small 'n' for illustration purposes)
#' set.seed(1)
#' data <- sim.data(n = 15, scenario = 2)
#'
#' #plot the mean model for the scenario from which we generated data
#' plot(data)
#' @method plot sim.data
#' @export
plot.sim.data = function(x, ...) {
  if (class(x)!="sim.data") stop("The class of 'x' must be 'sim.data'")
  #mean model
  dim = 50
  grid.complete = cbind(rep(seq(-2.5, 2.5, len=dim), dim), rep(seq(-2.5, 2.5, len=dim), each=dim))
  mean.model = sim.data(n = nrow(grid.complete), scenario = x$scenario, noise = 0, X = grid.complete)
  M.fill = mean.model$y

  color1 = "seagreen1"
  color2 = "steelblue1"
  color3 = "darkorchid4"
  graphics::layout(matrix(c(1,1,1,1,2), nrow=1))

  x1lab = expression(X[1])
  x2lab = expression(X[2])
  min = -5
  max = 5
  title = paste0("Mean Model for Scenario ", x$scenario)
  colors = c(col2hex(color1), col2hex(color2), col2hex(color3))
  pal = grDevices::colorRampPalette(colors)
  col = pal(1001)

  #normalize
  M.fill.norm = (M.fill - min) / (max - min)

  margin.x1 = abs(grid.complete[1,1] - grid.complete[2,1])/2
  margin.x2 = abs(grid.complete[1,2] - grid.complete[1+sqrt(nrow(grid.complete)),2])/2

  #plot x1 as y (with ylim reversed) and x2 as x
  graphics::plot(1,type="n",xlim=c(min(grid.complete[,2]),max(grid.complete[,2])),ylim=c(max(grid.complete[,1]),min(grid.complete[,1])),
                 axes=T,xlab=x2lab,ylab=x1lab,main=title,cex.axis=1,cex.lab=1)
  graphics::box()
  for (i in 1:length(M.fill)) {
    color = col[1001-round(M.fill.norm[i],3)*1000]
    graphics::rect(xleft=grid.complete[i,2]-margin.x2, xright=grid.complete[i,2]+margin.x2,
                   ybottom=grid.complete[i,1]+margin.x1, ytop=grid.complete[i,1]-margin.x1, border=NA, col=color)
  }
  default = graphics::par()$mar
  graphics::par(mar=c(0.2,0.2,0.2,2))
  legend_image = grDevices::as.raster(matrix(col),ncol=1)
  graphics::plot(c(0,1.5),c(0,1),type="n",axes=F,xlab="",ylab="",main="",cex.main=2, cex.lab=1.5, cex.axis=1.5)
  graphics::text(x=1.25, y=seq(0,1,l=5), labels=round(seq(min,max,l=5),2), cex=1)
  graphics::rasterImage(legend_image,0,0,1,1)
  graphics::par(mar=default)
}

##########################################################################################################################################
#### FUNCTIONS TO FIT CRISP WITH TUNING PARAMETER CHOSEN VIA CROSS-VALIDATION
##########################################################################################################################################

crispCV.helper = function(tuning.vec, y, X, train, q, rho, e_abs, e_rel, varyrho) {

  out = crisp(y=y[train], X=X[train,,drop=F], lambda.seq = tuning.vec, rho=rho, e_abs=e_abs, e_rel=e_rel, varyrho=varyrho, double.run=FALSE, q=q)
  fitted = matrix(NA, nrow=length(y), ncol=length(tuning.vec))

  for (i in 1:length(tuning.vec)) fitted[,i] = predict.crisp(out, new.X=X, lambda.index=i)

  error.vec = apply(fitted[-train,,drop=F], 2, mse, y=y[-train])
  return(error.vec)
}


#' CRISP with Tuning Parameter Selection via Cross-Validation.
#'
#' This function implements CRISP, which considers the problem of predicting an outcome variable on the basis of two covariates, using an interpretable yet non-additive model.
#' CRISP partitions the covariate space into blocks in a data-adaptive way, and fits a mean model within each block. Unlike other partitioning methods,
#' CRISP is fit using a non-greedy approach by solving a convex optimization problem, resulting in low-variance fits. This function differs
#' from the \code{\link{crisp}} function in that the tuning parameter, lambda, is automatically selected using K-fold cross-validation.
#' More details are provided in Petersen, A., Simon, N., and Witten, D. (2016). Convex Regression with Interpretable Sharp Partitions. Journal of Machine Learning Research, 17(94): 1-31 <http://jmlr.org/papers/volume17/15-344/15-344.pdf>.
#'
#' @param y An n-vector containing the response.
#' @param X An n x 2 matrix with each column containing a covariate.
#' @param lambda.min.ratio The smallest value for \code{lambda.seq}, as a fraction of the maximum lambda value, which is the data-derived
#' smallest value for which the fit is a constant value. The default is 0.01.
#' @param q The desired granularity of the CRISP fit, \code{M.hat}, which will be a \code{q} by \code{q} matrix. \code{M.hat}
#' is a mean matrix whose element \code{M.hat[i,j]} contains the mean for pairs of covariate values within a quantile range
#' of the observed predictors \code{X[,1]} and \code{X[,2]}. For example, \code{M.hat[1,2]} represents the
#' mean of the observations with the first covariate value less than the \code{1/q}-quantile of \code{X[,1]},
#' and the second covariate value between the \code{1/q}- and \code{2/q}-quantiles of \code{X[,2]}.
#' If left \code{NULL}, then \code{q=n} is used when n<100, and \code{q=100} is used when n>=100.
#' We recommend using \code{q<=100} as higher values take longer to fit and provide an unneeded amount of granularity.
#' @param n.lambda The number of lambda values to consider - the default is 50.
#' @param lambda.seq A user-supplied sequence of positive lambda values to consider. The typical usage is to calculate
#' \code{lambda.seq} using \code{lambda.min.ratio} and \code{n.lambda}, but providing \code{lambda.seq} overrides this. If provided,
#' \code{lambda.seq} should be a decreasing sequence of values, since CRISP relies on warm starts for speed.
#' Thus fitting the model for a whole sequence of lambda values is often faster than fitting for a single lambda value.
#' @param fold User-supplied fold numbers for cross-validation. If supplied, \code{fold} should be an n-vector with entries in 1,...,K when doing K-fold cross-validation. The default is to choose \code{fold} using \code{n.fold}.
#' @param n.fold The number of folds, K, to use for the K-fold cross-validation selection of the tuning parameter, lambda. The default is 10 - specification of \code{fold} overrides use of \code{n.fold}.
#' @param seed An optional number used with \code{set.seed()} at the beginning of the function. This is only relevant if \code{fold} is not specified by the user.
#' @param within1SE Logical value indicating how cross-validated tuning parameters should be chosen. If \code{within1SE=TRUE}, lambda is chosen to be the value corresponding to the most sparse model with cross-validation error within one standard error of the minimum cross-validation error. If \code{within1SE=FALSE}, lambda is chosen to be the value corresponding to the minimum cross-validation error.
#' @param e_abs,e_rel Values used in the stopping criterion for our ADMM algorithm, and discussed in Appendix C.2 of the CRISP paper.
#' @param rho The penalty parameter for our ADMM algorithm. The default is 0.1.
#' @param varyrho Should \code{rho} be varied from iteration to iteration? This is discussed in Appendix C.3 of the CRISP paper.
#' @param double.run The initial complete run of our ADMM algorithm will yield sparsity in z_{1i} and z_{2i}, but not
#' necessarily exact equality of the rows and columns of \code{M.hat}. If \code{double.run} is \code{TRUE}, then the algorithm
#' is run a second time to obtain \code{M.hat} with exact equality of the appropriate rows and columns. This issue
#' is discussed further in Appendix C.4 of the CRISP paper.
#'
#' @return An object of class \code{crispCV}, which can be summarized using \code{\link{summary}}, plotted using \code{\link{plot}}, and used to predict outcome values for new covariates using \code{\link{predict}}.
#' \itemize{
#' \item{\code{lambda.cv}: }{Optimal lambda value chosen by K-fold cross-validation.}
#' \item{\code{index.cv}: }{The index of the model corresponding to the chosen tuning parameter, \code{lambda.cv}. That is, \code{lambda.cv=crisp.out$lambda.seq[index.cv]}.}
#' \item{\code{crisp.out}: }{An object of class \code{crisp} returned by \code{\link{crisp}}.}
#' \item{\code{mean.cv.error}: }{An m-vector containing cross-validation error where m is the length of \code{lambda.seq}. Note that \code{mean.cv.error[i]} contains the cross-validation error for the tuning parameter \code{crisp.out$lambda.seq[i]}.}
#' \item{\code{se.cv.error}: }{An m-vector containing cross-validation standard error where m is the length of \code{lambda.seq}. Note that \code{se.cv.error[i]} contains the standard error of the cross-validation error for the tuning parameter \code{crisp.out$lambda.seq[i]}.}
#' \item{Other elements: }{As specified by the user.}
#' }
#'
#'
#' @seealso \code{\link{crisp}}, \code{\link{plot}}, \code{\link{summary}}, \code{\link{predict}}, \code{\link{plot.cvError}}
#'
#' @import Matrix
#' @examples
#' \dontrun{
#' #See ?'crisp-package' for a full example of how to use this package
#'
#' #generate data (using a very small 'n' for illustration purposes)
#' set.seed(1)
#' data <- sim.data(n = 15, scenario = 2)
#'
#' #fit model and select lambda using 2-fold cross-validation
#' #note: use larger 'n.fold' (e.g., 10) in practice
#' crispCV.out <- crispCV(X = data$X, y = data$y, n.fold = 2)
#' }
#' @export
crispCV = function(y, X, q = NULL, lambda.min.ratio = 0.01, n.lambda = 50, lambda.seq = NULL, fold = NULL, n.fold = NULL, seed = NULL, within1SE = FALSE, rho = 0.1, e_abs = 10^-4, e_rel = 10^-3, varyrho = TRUE, double.run = FALSE) {

  call = match.call()

  n = length(y)
  if (is.null(q)) {
    if (n<100) q = n  else q = 100
  }
  if (is.null(lambda.seq)) {
    max.lam = max.lambda(X=X, y=y)
    lambda.seq = exp(seq(log(max.lam), log(max.lam * lambda.min.ratio), len = n.lambda))
  }
  #checks
  if (length(y)!=nrow(X)) stop("The length of 'y' must equal the number of rows of 'x'")
  if (length(lambda.seq)==1) stop("Provide a sequence of decreasing values for 'lambda.seq'")
  if (min(lambda.seq)<=0) stop("Values in 'lambda.seq' must be positive")
  if (e_abs<=0 | e_rel<=0) stop("Values for 'e_abs' and 'e_rel' must be positive")
  if (!is.null(n.fold)) if ((n.fold < 1 | n.fold > n)) stop("'n.fold' must be between 1 and the length of 'y'")
  if (!is.null(fold) & length(fold) != n) stop("The length of 'fold' must equal the length of 'y'")
  if (!is.logical(varyrho)) stop("Specify TRUE or FALSE for 'varyrho'")
  if (!is.logical(double.run)) stop("Specify TRUE or FALSE for 'double.run'")
  if (!is.logical(within1SE)) stop("Specify TRUE or FALSE for 'within1SE'")

  if (!is.null(seed)) set.seed(seed)
  if (is.null(fold) & !is.null(n.fold)) {
    fold = sample(rep(1:n.fold, ceiling(n/n.fold))[1:n], n)
  } else if (is.null(fold) & is.null(n.fold)) {
    if (n>=10) fold = sample(rep(1:10, ceiling(n/10))[1:n], n) else fold = sample(1:n)
  }
  n.fold = length(unique(fold))

  cv.error = matrix(NA, nrow=n.fold, ncol=length(lambda.seq))
  for (k in 1:n.fold) {
    print(paste0("Fold ", k, " of ", n.fold))
    cv.error[k,] = crispCV.helper(y=y, X=X, train=which(fold!=k), tuning.vec=lambda.seq, q=q, rho=rho, e_abs=e_abs, e_rel=e_rel, varyrho=varyrho)
  }

  mean.cv.error = apply(cv.error,2,mean)
  se.cv.error = apply(cv.error,2,stats::sd)/sqrt(nrow(cv.error))

  if (within1SE==FALSE) {
    index.cv = min(which(mean.cv.error==min(mean.cv.error)))
  } else {
    index.cv = min(which(mean.cv.error==min(mean.cv.error)))
    error.cutoff = mean.cv.error[index.cv] + se.cv.error[index.cv]
    index.cv = min(which(mean.cv.error <= error.cutoff))
  }

  fit = list()
  fit$mean.cv.error = mean.cv.error
  fit$se.cv.error = se.cv.error
  fit$lambda.cv = lambda.seq[index.cv]
  fit$index.cv = index.cv
  fit$crisp.out = crisp(y=y, X=X, lambda.seq=lambda.seq, rho=rho, e_abs=e_abs, e_rel=e_rel, varyrho=varyrho, double.run=double.run, q=q)
  fit$fold = fold
  fit$n.fold = n.fold
  fit$within1SE = within1SE
  fit$call = 	call
  class(fit) = "crispCV"
  return(fit)
}

