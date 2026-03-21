
## Wraps around tsne in package tsne_0.1-3, 
## to carry out modifications to the qKernels and cnd kernels
##


setGeneric("qtSNE",function(x, ...) standardGeneric("qtSNE"))

setMethod("qtSNE", signature(x = "matrix"),
function(x,kernel = "rbfbase", qpar = list(sigma = 0.1, q = 0.9), initial_config = NULL, no_dims=2, initial_dims=30, perplexity=30, max_iter = 1300, min_cost=0, epoch_callback=NULL, epoch=100, na.action = na.omit, ...)
{
  x <- na.action(x)
  x = as.matrix(x)
  #x = x - min(x)
  #x = x/max(x)
  initial_dims = min(initial_dims,ncol(x))
  n = nrow(x)
  ret <- new("qtSNE")

  if (!is(kernel,"qkernel") && !is(kernel,"cndkernel"))
    {
      if(is(kernel,"function")) kernel <- deparse(substitute(kernel))
      kernel <- do.call(kernel, qpar)
    }
  if(!is(kernel,"qkernel") && !is(kernel,"cndkernel")) stop("kernel must inherit from class 'qkernel' or 'cndkernel'")
  if(is(kernel,"cndkernel")){
    ## Compute conditionally negative definite kernel matrix
    H <- cndkernmatrix(kernel,x)  }
  else
    if (is(kernel,"qkernel")) H <- qkernmatrix(kernel, x)
#----------------------------------------------------------#

	momentum = .5
	final_momentum = .8
	mom_switch_iter = 250
	stop_lying_iter = 300
	epsilon = 50   #500
  eps = .Machine$double.eps # typical machine precision
  min_gain = .01
	initial_P_gain = 4

	if (!is.null(initial_config) && is.matrix(initial_config)) {
		if (nrow(initial_config) != n | ncol(initial_config) != no_dims){
			stop('initial_config argument does not match necessary configuration for x')
		}
		ydata = initial_config
		initial_P_gain = 1

	} else {
		ydata = matrix(rnorm(no_dims * n),n)
	}

#---------------------------------------------------------------#

	P = .d2p(H,perplexity, 1e-5)$P

	P = .5 * (P + t(P))
	P[P < eps]<- eps
	P = P/sum(P)

	P <- P * initial_P_gain
	grads <- matrix(0,nrow(ydata),ncol(ydata))
	incs <-  matrix(0,nrow(ydata),ncol(ydata))
	gains <- matrix(1,nrow(ydata),ncol(ydata))

	for (iter in 1:max_iter){
		if (iter %% epoch == 0) { # epoch
			cost <-  sum(apply(P * log((P+eps)/(Q+eps)), 1, sum))
			message("Epoch: Iteration #",iter," error is: ",cost)
			if (cost < min_cost) break
			if (!is.null(epoch_callback)) epoch_callback(ydata)
		}
    # Compute joint probability that point i and j are neighbors
	  sum_ydata = apply(ydata^2, 1, sum)
	  num =  1/(1 + sum_ydata + sweep(-2 * ydata %*% t(ydata),2, -t(sum_ydata)))
	  diag(num)=0
	  Q = num / sum(num)
	  if (any(is.nan(num))) message ('NaN in grad. descent')
	  Q[Q < eps] = eps
	  stiffnesses = 4 * (P-Q) * num
	  for (i in 1:n){
	    grads[i,] = apply(sweep(-ydata, 2, -ydata[i,]) * stiffnesses[,i],2,sum)
	  }

	  gains = ((gains + .2) * abs(sign(grads) != sign(incs)) +
	             gains * .8 * abs(sign(grads) == sign(incs)))

	  gains[gains < min_gain] = min_gain
	  incs = momentum * incs - epsilon * (gains * grads)
	  ydata = ydata + incs
	  ydata = sweep(ydata,2,apply(ydata,2,mean))
		if (iter == mom_switch_iter) momentum = final_momentum
		if (iter == stop_lying_iter) P = P/4
	}
	dimRed(ret) <- ydata
	# xmatrix(ret) <- x
	kcall(ret) <- match.call()
	cndkernf(ret) <- kernel
	return(ret)
})

#----------------------------------------------------------#

setMethod("qtSNE", signature(x = "cndkernmatrix"),
function(x, initial_config = NULL, no_dims=2, initial_dims=30, perplexity=30, max_iter = 1000, min_cost=0, epoch_callback=NULL,epoch=100)
{


  n = nrow(x)
  ret <- new("qtSNE")

  if(!is(x,"cndkernmatrix")) stop("x must inherit from class 'cndkernmatrix'")
#----------------------------------------------------------#

	momentum = .5
	final_momentum = .8
	mom_switch_iter = 250
	stop_lying_iter = 200
	epsilon = 50   #500
	eps = .Machine$double.eps # typical machine precision
  min_gain = .01
	initial_P_gain = 4

	if (!is.null(initial_config) && is.matrix(initial_config)) {
		if (nrow(initial_config) != n | ncol(initial_config) != no_dims){
			stop('initial_config argument does not match necessary configuration for x')
		}
		ydata = initial_config
		initial_P_gain = 1

	} else {
		ydata = matrix(rnorm(no_dims * n),n)
	}

#---------------------------------------------------------------#

	P = .d2p(x,perplexity, 1e-5)$P
	P = .5 * (P + t(P))
	P[P < eps]<-eps
	P = P/sum(P)

	P <- P * initial_P_gain
	grads <- matrix(0,nrow(ydata),ncol(ydata))
	incs <-  matrix(0,nrow(ydata),ncol(ydata))
	gains <- matrix(1,nrow(ydata),ncol(ydata))

	for (iter in 1:max_iter){
		if (iter %% epoch == 0) { # epoch
			cost <-  sum(apply(P * log((P+eps)/(Q+eps)), 1, sum))
			message("Epoch: Iteration #",iter," error is: ",cost)
			if (cost < min_cost) break
			if (!is.null(epoch_callback)) epoch_callback(ydata)
		}
    # Compute joint probability that point i and j are neighbors
	  sum_ydata = apply(ydata^2, 1, sum)
	  num =  1/(1 + sum_ydata + sweep(-2 * ydata %*% t(ydata),2, -t(sum_ydata)))
	  diag(num)=0
	  Q = num / sum(num)
	  if (any(is.nan(num))) message ('NaN in grad. descent')
	  Q[Q < eps] = eps
	  stiffnesses = 4 * (P-Q) * num
	  for (i in 1:n){
	    grads[i,] = apply(sweep(-ydata, 2, -ydata[i,]) * stiffnesses[,i],2,sum)
	  }

	  gains = ((gains + .2) * abs(sign(grads) != sign(incs)) +
	             gains * .8 * abs(sign(grads) == sign(incs)))

	  gains[gains < min_gain] = min_gain
	  incs = momentum * incs - epsilon * (gains * grads)
	  ydata = ydata + incs
	  ydata = sweep(ydata,2,apply(ydata,2,mean))
		if (iter == mom_switch_iter) momentum = final_momentum
		if (iter == stop_lying_iter) P = P/4
	}
	dimRed(ret) <- ydata
	# xmatrix(ret) <- x
	kcall(ret) <- match.call()
	cndkernf(ret) <- "cndkernel"
	return(ret)
})

#----------------------------------------------------------#
setMethod("qtSNE", signature(x = "qkernmatrix"),
function(x,initial_config = NULL, no_dims=2, initial_dims=30, perplexity=30, max_iter = 1000, min_cost=0, epoch_callback=NULL, epoch=100)
{

  n = nrow(x)
  ret <- new("qtSNE")

  if(!is(x,"qkernmatrix")) stop("x must inherit from class 'qkernmatrix'")
#----------------------------------------------------------#

	momentum = .5
	final_momentum = .8
	mom_switch_iter = 250
	stop_lying_iter = 200
	epsilon = 50   #500
	eps = .Machine$double.eps # typical machine precision
  min_gain = .01
	initial_P_gain = 4

	if (!is.null(initial_config) && is.matrix(initial_config)) {
		if (nrow(initial_config) != n | ncol(initial_config) != no_dims){
			stop('initial_config argument does not match necessary configuration for x')
		}
		ydata = initial_config
		initial_P_gain = 1

	} else {
		ydata = matrix(rnorm(no_dims * n),n)
	}

#---------------------------------------------------------------#

	P = .d2p(x,perplexity, 1e-5)$P

	P = .5 * (P + t(P))
	P[P < eps]<-eps
	P = P/sum(P)

	P <- P * initial_P_gain
	grads <- matrix(0,nrow(ydata),ncol(ydata))
	incs <-  matrix(0,nrow(ydata),ncol(ydata))
	gains <- matrix(1,nrow(ydata),ncol(ydata))

	for (iter in 1:max_iter){
		if (iter %% epoch == 0) { # epoch
			cost <-  sum(apply(P * log((P+eps)/(Q+eps)), 1, sum))
			message("Epoch: Iteration #",iter," error is: ",cost)
			if (cost < min_cost) break
			if (!is.null(epoch_callback)) epoch_callback(ydata)
		}
    # Compute joint probability that point i and j are neighbors
	  sum_ydata = apply(ydata^2, 1, sum)
	  num =  1/(1 + sum_ydata + sweep(-2 * ydata %*% t(ydata),2, -t(sum_ydata)))
	  diag(num)=0
	  Q = num / sum(num)
	  if (any(is.nan(num))) message ('NaN in grad. descent')
	  Q[Q < eps] = eps
	  stiffnesses = 4 * (P-Q) * num
	  for (i in 1:n){
	    grads[i,] = apply(sweep(-ydata, 2, -ydata[i,]) * stiffnesses[,i],2,sum)
	  }

	  gains = ((gains + .2) * abs(sign(grads) != sign(incs)) +
	             gains * .8 * abs(sign(grads) == sign(incs)))

	  gains[gains < min_gain] = min_gain
	  incs = momentum * incs - epsilon * (gains * grads)
	  ydata = ydata + incs
	  ydata = sweep(ydata,2,apply(ydata,2,mean))
		if (iter == mom_switch_iter) momentum = final_momentum
		if (iter == stop_lying_iter) P = P/4
	}
	dimRed(ret) <- ydata
	# xmatrix(ret) <- x
	kcall(ret) <- match.call()
	cndkernf(ret) <- "qkernel"
	return(ret)
})



###---------------------------------------------------------------#
.Hbeta <-function(D, beta){
  P = exp(-D * beta)
  sumP = sum(P)
  if (sumP == 0){
    H = 0
    P = D * 0
  } else {
    H = log(sumP) + beta * sum(D %*% P) /sumP
    P = P/sumP
  }
  r = {}
  r$H = H
  r$P = P
  r
}

.d2p <-function(D,perplexity = 15,tol = 1e-5){

  n <- nrow(D)
  D = as.matrix(D)
  P = matrix(0, n, n )
  beta = rep(1, n)
  logU = log(perplexity)

  for (i in 1:n){
    betamin = -Inf
    betamax = Inf
    Di = D[i, -i]
    hbeta = .Hbeta(Di, beta[i])
    H = hbeta$H;
    thisP = hbeta$P
    Hdiff = H - logU;
    tries = 0;

    while(abs(Hdiff) > tol && tries < 50){
      if (Hdiff > 0){
        betamin = beta[i]
        if (is.infinite(betamax)) beta[i] = beta[i] * 2
        else beta[i] = (beta[i] + betamax)/2
      } else{
        betamax = beta[i]
        if (is.infinite(betamin))  beta[i] = beta[i]/ 2
        else beta[i] = ( beta[i] + betamin) / 2
      }

      hbeta = .Hbeta(Di, beta[i])
      H = hbeta$H
      thisP = hbeta$P
      Hdiff = H - logU
      tries = tries + 1
    }
    P[i,-i]  = thisP
  }

  r = {}
  r$P = P
  r$beta = beta
  sigma = sqrt(1/beta)

  message('sigma summary: ', paste(names(summary(sigma)),':',summary(sigma),'|',collapse=''))

  r
}

