##
"tps" <- function(formula=formula(data),
                  data=parent.frame(),
                  nn0, nn1,
                  group,
                  contrasts=NULL,
                  method="PL",
                  cohort=TRUE,
                  alpha=1)
{
  call <- match.call()
  m    <- match.call(expand.dots = FALSE)
  m$method <- m$contrasts <- m$nn0 <- m$nn1 <- m$group <- m$cohort <- m$
  alpha  <- NULL
  m[[1]] <- as.name("model.frame")
  m      <- eval(m, parent.frame())
  Terms  <- attr(m, "terms")
  a      <- attributes(m)
  Y      <- model.extract(m, "response")
  X      <- model.matrix(Terms, m, contrasts)

  ## Potential Errors
  if(length(nn0) != length(nn1)) 					 stop("nn0 and nn1 should be of same length")
  if(length(nn0) != length(unique(group))) stop("Number of strata defined by group should be same as length of nn0")
  if(length(group) != nrow(X))						 stop("Group and x are not compatible")
  if((any(nn1 == 0)) || (any(nn0 == 0)))   stop("Zero cell frequency at phase I")

  ## method
  imeth <- charmatch(method, c("PL", "WL", "ML"), nomatch = 0)
  methodName <- switch(imeth + 1,
                        stop("Method doesn't exist"),
                        "PL",
                        "WL",
                        "ML")
  ## data
  if(is.matrix(Y) && (ncol(Y) > 1))
  {
    case <- as.vector(Y[, 1])
    N    <- as.vector(Y[, 1] + Y[, 2])
  }
  else
  {
    case <- as.vector(Y)
    N    <- rep(1, length(case))
  }
	
	## evaluation
	z   <- call(methodName, nn0 = nn0, nn1 = nn1, x = X, N = N, case = case, group = group, cohort = cohort, alpha = alpha)
	#out <- eval(z, local = sys.parent())
	out <- eval(z)
	names(out$coef) <- dimnames(X)[[2]]
	if(!is.null(out$cove)) dimnames(out$cove) <- list(dimnames(X)[[2]], dimnames(X)[[2]])
	if(!is.null(out$covm)) dimnames(out$covm) <- list(dimnames(X)[[2]], dimnames(X)[[2]])
	out$method <- method
	class(out) <- "tps"
	##
	return(out)
}


"WL" <- function(nn0, nn1, x, N, case, group, cohort, alpha)
{
	# S function for the weighted likelihood or Horvitz-Thompson type of estimator.        
	nntot0 <- sum(nn0)	# Phase I control Total
	nntot1 <- sum(nn1)	# Phase I case Total
  u <- sort(unique(group))
  strt.id <- outer(group,u,FUN="==")
  strt.id <- matrix(as.numeric(strt.id),nrow=length(group),ncol=length(u))
  n1 <- apply(strt.id * case, 2, sum)	      # n1= Phase II case sample sizes
	n0 <- apply(strt.id * (N - case), 2, sum) # n0 = phase II control sample sizes   
	if((any(n0 == 0)) || (any(n1 == 0)))
		stop("Zero cell frequency at phase II")
	strt.id <- rbind(strt.id, strt.id)
	N <- c(case, N - case)
	case <- c(rep(1, length(case)), rep(0, length(case)))
	group <- c(group, group)
	x <- rbind(x, x)
	nstrta <- length(nn0)
	ofs <- 0
	if(!cohort)
		ofs <- log(sum(nn1)/sum(nn0)) - log(alpha)
	ofs <- rep(ofs, length(case))
	pw0 <- nn0[group]/n0[group]
	pw1 <- nn1[group]/n1[group]
	pw <- case * pw1 + (1 - case) * pw0
	Nw <- N*pw
	lp <- rep(0, length(case))
	z <- rep(0, length(case))
	z[case == 1] <- Nw[case == 1]
	m <- glm(cbind(z, Nw - z) ~ -1 + x + offset(ofs), family = binomial, x=TRUE, 
	         control = glm.control(epsilon = 9.9999999999999995e-07, maxit = 20))
  #  weights=pw, start = lp, 
	uj0 <-  - t(m$x) %*% (strt.id * (1 - case) * m$fitted.values * N)	
	#   Within group sum of scores for controls
	uj1 <- t(m$x) %*% (strt.id * case * (1 - m$fitted.values) * N)	
	#     Within group sum of scores for cases
	g0 <- t(m$x * as.vector((1 - case) * m$fitted.values^2 * N * pw0^2)) %*% m$x           ## CHANGED 29TH OCT 2007
	g1 <- t(m$x * as.vector(case * (1 - m$fitted.values)^2 * N * pw1^2)) %*% m$x
	identity <- diag(rep(1, nstrta))
	if((any(n1 == 0)) || (any(n0 == 0)))
		stop("No cell should be empty in phase II")
  ##
  b0temp <- as.vector((nn0 * (nn0 - n0))/n0^3)                                           ## CHANGED 30TH OCT 2007
	b0 <- diag(b0temp, nrow=length(b0temp))
  ##
  b1temp <- as.vector((nn1 * (nn1 - n1))/n1^3)                                           ## CHANGED 30TH OCT 2007
	b1 <- diag(b1temp, nrow=length(b1temp))
  ##
  bb0temp <- as.vector((n0/(nntot0 * nn0)))                                              ## CHANGED 30TH OCT 2007
	bb0 <- diag(bb0temp, nrow=length(bb0temp))
  ##
  bb1temp <- as.vector((n1/(nntot1 * nn1)))                                              ## CHANGED 30TH OCT 2007
	bb1 <- diag(bb1temp, nrow=length(bb1temp))
	##
  cov <- summary(m)$cov.unscaled
	cove <- g1 + g0 - uj0 %*% b0 %*% t(uj0) - uj1 %*% b1 %*% t(uj1)
	if(!cohort)
		cove <- cove - uj0 %*% bb0 %*% t(uj0) - uj1 %*% bb1 %*% t(uj1)
	cove <- cov %*% cove %*% cov
	z <- list(coef = m$coef, cove = cove, fail = FALSE)
	z
}


"PL" <- function(nn0, nn1, x, N, case, group, cohort, alpha)
{
# S-function for "Pseudo Likelihood"  method  as developed by Breslow and Cain (1988)
	nntot0 <- sum(nn0)	# Phase I control Total
	nntot1 <- sum(nn1)	# Phase I case Total
	nstrata <- length(nn0)
  u <- sort(unique(group))
  strt.id <- outer(group,u,FUN="==")
  strt.id <- matrix(as.numeric(strt.id),nrow=length(group),ncol=length(u))
  n1 <- apply(strt.id * case, 2, sum)       # n1 = Phase II case sample sizes
	n0 <- apply(strt.id * (N - case), 2, sum) # n0 = phase II control sample sizes   
	if((any(n0 == 0)) || (any(n1 == 0)))
		stop("Zero cell frequency  at phase II")
	ofs <- log(n1/n0) - log(nn1/nn0)
	if(!cohort)
		ofs <- ofs + log(nntot1/nntot0) - log(alpha)
	ofs <- ofs[group]
	lp <-  - ofs
	pw <- rep(1, length(case))	
	#  Fitting model using standard GLM procedure
	m <- glm(cbind(case, N - case) ~ -1 + x + offset(ofs), family = binomial, weights = pw, x = TRUE, control = glm.control(epsilon = 9.9999999999999995e-07, maxit = 20))
	fv <- m$fitted.values
	nhat0 <- apply(strt.id * (1 - fv) * N, 2, sum)	
	#  Within group sum of fitted values for controls
	nhat1 <- apply(strt.id * fv * N, 2, sum)	
	# Within group sum of fited values
	# Adjusted covariance Matrix
	uj0 <-  - t(m$x) %*% (strt.id * (N - case) * m$fitted.values)	
	#   Within group sum of scores for controls
	uj1 <-  - t(m$x) %*% (strt.id * case * (1 - m$fitted.values))	
	#     Within group sum of scores for cases
	g0 <- t(m$x * (N - case) * fv^2) %*% m$x
	g1 <- t(m$x * case * (1 - fv)^2) %*% m$x
	a <- t(m$x) %*% (strt.id * (1 - fv) * fv * N)
	zz <- matrix(1, nrow = nstrata, ncol = nstrata)
	identity <- diag(rep(1, nstrata))
	b0 <- identity/as.vector(n0)
	b1 <- identity/as.vector(n1)
	bb0 <- (identity/as.vector(nn0))
	bb1 <- (identity/as.vector(nn1))
	if(!cohort)
	{
		bb0 <- bb0 - (zz/nntot0)
		bb1 <- bb1 - (zz/nntot1)
	}
	aba <- a %*% bb0 %*% t(a) + a %*% bb1 %*% t(a)
	info <- t(m$x) %*% diag(m$weight) %*% m$x
	info2 <- solve(summary(m)$cov.unscaled)
	ghat <- info - a %*% b0 %*% t(a) - a %*% b1 %*% t(a)
	ghate <- g1 + g0 - uj0 %*% b0 %*% t(uj0) - uj1 %*% b1 %*% t(uj1)
	cov <- summary(m)$cov.unscaled
	cove <- cov %*% (ghate + aba) %*% cov	
	# Empirical variance-covariance matrix
	cove <- cov %*% (ghate + aba) %*% cov	
	# Model based variance-covariance matrix
	covm <- cov %*% (ghat + aba) %*% cov
	return(list(coef = m$coef, covm = covm, cove = cove, fail = FALSE))
}


"ML"<- function(nn0, nn1, x, N, case, group, cohort, alpha, maxiter = 100)
{
# S function to solve the concentrated lagrangian equations developed by 
# Breslow and Holubkov (1997). This function implements a modified
# Newton-Rhapson  algorithm to solve the equations. Gradients as computed by
# the authors were used to implement the NR algorithm.
	iter <- 0
	converge <- FALSE
	rerror <- 100
	nntot0 <- sum(nn0)	# Phase I control Total
	nntot1 <- sum(nn1)	# Phase I case Total
	nn <- nn0 + nn1
	nntot <- nntot0 + nntot1
	nstrata <- length(nn0)
	nobs <- length(case)
	ncovs <- ncol(x)
	stpmax <- 1 * (ncovs + nstrata)
 ca<-case
 co<-N-case
 u <- sort(unique(group))
 strt.id <- outer(group,u,FUN="==")
 strt.id <- matrix(as.numeric(strt.id),nrow=length(group),ncol=length(u))
	ee1 <- cbind(matrix(1, nstrata, 1), matrix(0, nstrata, nstrata))
	ee2 <- cbind(matrix(0, nobs, 1), strt.id)
	ee <- rbind(ee1, ee2)
 n1 <- apply(strt.id * case, 2, sum)
 # n1= Phase II case sample sizes
	n0 <- apply(strt.id * (N - case), 2, sum)
	#  n0 = phase II control sample sizes   
	if((any(n0 == 0)) || (any(n1 == 0)))
		stop("Zero cell frequency at phase II")
	n <- n0 + n1
	n1a <- c(nntot1, n1)
	n0a <- c(nntot0, n0)
	grpa <- c(rep(1, nstrata), (group + 1))	# Augmented group indicator
	na <- n0a + n1a
	yy <- c(nn1, case)
	identity <- diag(rep(1, nstrata))
	x0 <- cbind(identity, matrix(0, nrow = nstrata, ncol = ncovs))
	x <- cbind( - strt.id, x)	# Augmented covariate matrix
	xx <- rbind(x0, x)
	ofs <- log(n1a/n0a)
	ofs[1] <- ofs[1] - log(alpha)
	if(cohort)
		ofs[1] <- ofs[1] - log(nntot1/nntot0) + log(alpha)
	ofs <- ofs[grpa]
	repp <- c(nn1 + nn0, N)	# Augmented binomial denominator
	m <- glm(cbind(yy, repp - yy) ~ -1 + xx + offset(ofs), family = binomial, x = TRUE, control = glm.control(epsilon = 1e-10, maxit = 100))
	gamm.schill <- as.vector(m$coef)
	gamm0 <- gamm <- gamm.schill	# Initialize by Schill's estimates
	errcode <- 0
	while((iter < maxiter) && (rerror > 1e-10))
	{
		#   cat("No of iterations=", iter, "\n")
		l <- lagrange(gamm0, nstrata, nn1, nn0, n1a, n0a, grpa, repp, xx, ofs, yy)
		h <- lagrad(gamm0, nstrata, nobs, ncovs, nn1, nn0, n1a, n0a, ee, repp, grpa, n0, n1, xx, ofs)
 	p <-  - solve(h) %*% l	# Newton's direction
		sp2 <- sqrt(sum(p^2))

		if(sp2 > stpmax)
			p <- p * (stpmax/sp2)	
		# Scale if the attempted stepis too big
		gamm <- lnsrch(gamm0, 0.5 * sum(l^2), t(h) %*% l, p, nstrata, nn1, nn0, n1a, n0a, grpa, repp, xx, ofs, yy)	
		# Search for new value along the line of Newton's direction
		rerror <- max(abs(gamm - gamm0)/pmax(abs(gamm0), 0.10000000000000001))
		gamm0 <- gamm  

   iter <- iter + 1
	}
	if(iter < maxiter) converge <- TRUE	#cat("No of iterations=", iter, "\n")
	h <- lagrad(gamm0, nstrata, nobs, ncovs, nn1, nn0, n1a, n0a, ee, repp, grpa, n0, n1, xx, ofs)
	fvv <- xx %*% gamm + ofs
	fvv <- as.vector(exp(fvv)/(1 + exp(fvv)))
	t1 <- nn1 - nn * fvv[1:nstrata]
	mu <- 1 - t1/n1
	t1 <- c(0, t1)
	mu <- c(1, mu)
	qn <- repp * n0a[grpa]
	qd1 <- n0a[grpa]
	qd2 <- (1 - mu[grpa]) * (n1a[grpa] - na[grpa] * fvv)
	q <- qn/(qd1 + qd2)
	# 
	# Verify constraints: equations (8) and (9) of Breslow and Holubkov
	#qq <- q/na[grpa]
	#h0 <- round(as.vector(t(qq)%*%ee), 10)
	#h1 <- as.vector(round(t(n1a - na*t(ee)%*%(fvv*qq)),digits=4))
	#print("Check that q's sum to 1 within strata")
	#print(h0)
	#print("Check that constraints for i=1 hold")
	#print(h1)
	##
	#fail <- FALSE
	#if(sum(h0 != 1) > 0) fail <- TRUE
	#if(sum(h1 != 0) > 0) fail <- TRUE
	#if(fail == TRUE) cat("\n WARNING: ML estimates don't satisfy appropriate constraints\n")
	##
	gg <- t(xx) %*% ((1 - fvv) * fvv * q * ee)
	t00 <- (1 - fvv)^2 * q * ee
	d00 <- rep.int(1, nrow(t00)) %*% t00
	t01 <- (1 - fvv) * fvv * q * ee
	d01 <- rep.int(1, nrow(t01)) %*% t01
	t11 <- fvv * fvv * q * ee
	d11 <- rep.int(1, nrow(t11)) %*% t11
	d00 <- diag(as.vector(d00))
	d01 <- diag(as.vector(d01))
	d11 <- diag(as.vector(d11))
	tinfo <- (1 - fvv) * fvv * q * xx
	info <- t(xx) %*% tinfo
	cove <- solve(info)
	gig <- t(gg) %*% cove %*% gg
	zz1 <- cbind((gig + d00), ( - gig + d01))
	zz2 <- cbind(( - gig + d01), (gig + d11))
	zz <- rbind(zz1, zz2)
	zz <- solve(zz)
	gg <- cbind(gg,  - gg)
	gig <- cove %*% gg %*% zz %*% t(gg) %*% cove
	cov1 <- cove - gig	
	# Model covariance matrix using Atchinson and Silvey formula 
	# Computation of empirical variance-covariance matrix using within group scores
	ta <- n0a * (n1a - t1)
	tb <- na * t1
	g <- (ta[grpa] * fvv)/(ta[grpa] + tb[grpa] * (1 - fvv))
	uj0 <-  - t(m$x) %*% (ee * (repp - yy) * g)
	uj1 <- t(m$x) %*% (ee * yy * (1 - g))
	gg <- t(m$x * yy * (1 - g)^2) %*% m$x + t(m$x * (repp - yy) * g^2) %*% m$x
	#u1j0 <- -t(xx)%*%(ee*(repp-yy)*g)        
	#u1j1 <- t(xx)%*%(ee*yy*(1-g))
	#print("uj0:")
	#print(uj0)
	#print("uj1:")
	#print(uj1)
	# gg <- t(xx*((1-g)^2*yy + g^2*(repp-yy)))%*%xx
	gig <- gg - t(t(uj0)/n0a) %*% t(uj0) - t(t(uj1)/n1a) %*% t(uj1)	
	#gig <- gg - t( t(u1j0) / n0a)  %*% t(u1j0) -  t( t(u1j1) / n1a ) %*% t(u1j1)
	#print("gig")
	#print(gig)
	h <- solve(h)
	cov2 <- t(h) %*% gig %*% h	
	# Adjusting term for asymptotic variance-covariance matrix for prospective first stage sampling
	adjust <- (1/nntot0) + (1/nntot1)
	adjust <- matrix(adjust, nrow = (nstrata + 1), ncol = (nstrata + 1))
	if(cohort)
	{
		cov1[1:(nstrata + 1), 1:(nstrata + 1)] <- cov1[1:(nstrata + 1), 1:(nstrata + 1)] + adjust
		cov2[1:(nstrata + 1), 1:(nstrata + 1)] <- cov2[1:(nstrata + 1), 1:(nstrata + 1)] + adjust
	}
	cov1 <- cov1[(nstrata + 1):(nstrata + ncovs), (nstrata + 1):(nstrata + ncovs)]	
	# Exctract the covariance matrix for the regression coefficients
	cov2 <- cov2[(nstrata + 1):(nstrata + ncovs), (nstrata + 1):(nstrata + ncovs)]	
	# Exctract the covariance matrix for the regression coefficients
	# jc <- jc[(nstrata + 1):(nstrata + ncovs), (nstrata + 1):(nstrata + ncovs)]	
	delta <- gamm[1:nstrata]
	b <- gamm[(nstrata + 1):(nstrata + ncovs)]
	q <- q/na[grpa]
	
	#validation of constraints, calculate h_ij from eq. (9) in Breslow and Holubkov
	xx2<-xx[(nstrata+1):nrow(xx),]
	pred<-xx2%*%gamm
 help<-rep(0,nstrata)
	for (i in 1:nstrata)
	{
	   k<-xx2[,i]
    help[i]<--sum(k)
	} 
	help<-as.vector(help)
	#help<-nrow(xx2)/nstrata
 n1_new<-rep(n1,help)
 n0_new<-rep(n0,help)
 p1<-n1_new*exp(pred)/(n0_new+n1_new*exp(pred))
 xx3<-xx[1:nstrata,]
 pred2<-xx3%*%gamm
 nntot0_new<-rep(nntot0,nstrata)
 nntot1_new<-rep(nntot1,nstrata)
 if(cohort)
 {
 P<-exp(pred2)/(1+exp(pred2))
 }
 else 
 { 
 P<-nntot1_new*exp(pred2)/(nntot0_new+nntot1_new*exp(pred2))
 }
 T<-nn1-(nn1+nn0)*P
 T_new<-rep(T,help)
 q<-N/(n0_new+n1_new)*n1_new*n0_new/(n0_new*n1_new+T_new*(n1_new-(n1_new+n0_new)*p1))
 p0<-1-p1
 h1<-rep(0,nstrata)
 h0<-rep(0,nstrata)
 start<-1
 for (i in 1:nstrata)
 {
   sum_1<-0
   sum_0<-0
   end<-start+help[i]-1   
     sum_1<-t(p1[start:end])%*%q[start:end]
     sum_0<-t(p0[start:end])%*%q[start:end]
   h1[i]<-n1[i]-(n1[i]+n0[i])*sum_1
   h0[i]<-n0[i]-(n1[i]+n0[i])*sum_0
   start<-start+help[i]
 }
 
#	out <- list(coef = b, covm = cov1, cove = cov2, delta = delta, mu = mu, h0=h0, h1=h1, k=k)
	fail <- FALSE
	if(sum(round(h0, 10) != 0) > 0) fail <- TRUE
	if(sum(round(h1, 10) != 0) > 0) fail <- TRUE
	if(fail == TRUE) cat("\n WARNING: ML estimates don't satisfy appropriate constraints\n\n")
	out <- list(coef = b, covm = cov1, cove = cov2, delta = delta, mu = mu, fail = fail)
	out
}


"summary.tps"<- function(object, ...)
{
	# produces summary from an object of the class "tps"
	coef <- object$coef
	method <- object$method
	if(method == "WL")
	{
		see <- sqrt(diag(object$cove))
		tee <- coef/see
		pee <- 2 * (1 - pnorm(abs(tee)))
		coefficients <- matrix(0, nrow = length(coef), ncol = 4)
		dimnames(coefficients) <- list(names(coef), c("Value", "Emp SE", "Emp t", "Emp p"))
		coefficients[, 1] <- coef
		coefficients[, 2] <- see
		coefficients[, 3] <- tee
		coefficients[, 4] <- pee
	}
	else
	{
		se  <- sqrt(diag(object$covm))
		see <- sqrt(diag(object$cove))
		te  <- coef/se
		tee <- coef/see
		pe  <- 2 * (1 - pnorm(abs(te)))
		pee <- 2 * (1 - pnorm(abs(tee)))
		coefficients <- matrix(0, nrow = length(coef), ncol = 7)
		dimnames(coefficients) <- list(names(coef), c("Value", "Mod SE", "Mod t", "Mod p", "Emp SE", "Emp t", "Emp p"))
		coefficients[, 1] <- coef
		coefficients[, 2] <- se
		coefficients[, 3] <- te
		coefficients[, 4] <- pe
		coefficients[, 5] <- see
		coefficients[, 6] <- tee
		coefficients[, 7] <- pee
	}
	structure(list(coefficients = coefficients), class = "summary.tps")
}

#function to compute the lagrangian equations
lagrange <- function(gamm0, nstrata , nn1 , nn0, n1a , n0a , grpa ,repp , xx , ofs , yy)
{
	fv <- xx%*%gamm0 + ofs 
	fv <- exp(fv)
	fv <- fv/(1+fv)
	r  <- fv[1:nstrata]
	r  <- nn1 - (nn1 + nn0)*r
	r  <- c(0,r)
	r1 <- (n1a-r)*n0a
	r2 <- (n0a+n1a)*r
	g  <- r1[grpa]*fv
	g  <- g/(r1[grpa] + r2[grpa]*(1-fv))
	g  <- t(xx)%*%((1-g)*yy - g*(repp-yy)) 
	g
}


#function to compute the gradients
lagrad <- function(gamm0, nstrata, nobs , ncovs ,nn1 , nn0 , n1a , n0a ,ee ,repp , grpa , n0 , n1 , xx , ofs)
{
	fv  <- xx%*%gamm0 + ofs
	fv  <- exp(fv)
	fv  <- fv/(1+fv)
	xxx <- xx[(nstrata+1):(nstrata+nobs),]
	grp <- grpa[(nstrata+1):(nstrata+nobs)]-1
	rep <- repp[(nstrata+1):(nstrata+nobs)]
	e   <- ee[(nstrata+1):(nstrata+nobs),2:(nstrata+1)]
	pp  <- fv[1:nstrata]
	g   <- fv[(nstrata+1):(nstrata+nobs)]
	r   <- nn1 - (nn1+nn0)*pp
	r0  <- r*(n0+n1)
	r1  <- n0*n1*(nn0+nn1)*(n0+n1)*pp*(1-pp)
	r2  <- n0*(n1-r)

	d <- r1[grp]*g*(1-g)*rep/(r2[grp]+r0[grp]*(1-g))^2
  d <- as.vector(d)                                       ## ADDITIONAL 29TH OCT 2007

	de <- d*e
	g  <- -t(xxx)%*%de
	g  <- cbind(g,matrix(0,(nstrata+ncovs),ncovs))
	r  <- c(0,r)
	r1 <- n0a*(n1a-r)
	r2 <- (n0a+n1a)*r
	r0 <- n0a*n1a*(n1a-r)*(n0a+r)
	fv <- r0[grpa]*fv*(1-fv)*repp/(r1[grpa]+r2[grpa]*(1-fv))^2
	fv <- t(xx)%*%(hdp(fv,xx))
	g <- g - fv
	g
}

#function to compute the norm of the lagrangian equations
s2 <- function(gamm,nstrata , nn1 , nn0 , n1a , n0a , grpa , repp , xx , ofs , yy)
{
	l <- lagrange(gamm,nstrata , nn1 , nn0 , n1a , n0a , grpa , repp , xx , ofs , yy)
  sum(l^2)
}

#function to compute horizontal direct product of two matrices
hdp <- function(x,y)
{
	x <- as.matrix(x)
	y <- as.matrix(y)
	if(nrow(x) != nrow(y))stop("row dimensions are not same")
	z <- x[,rep(1:ncol(x),rep(ncol(y),ncol(x)))]*y[,rep(1:ncol(y),ncol(x))]
	z
}

#function to conduct line search for modified Newton method of ML
lnsrch <- function(gamm0,f0,g0,p,nstrata,nn1,nn0,n1a,n0a,grpa,repp,xx,ofs,yy)
{
	scale <- 1.0
	rel.error <- max(abs(p)/pmax(gamm0,0.1))
	if(rel.error < 1e-07)
		gamm <- gamm0
	else
	{
		gamm <- gamm0 + p
		l <- lagrange(gamm,nstrata,nn1,nn0,n1a,n0a,grpa,repp,xx,ofs,yy)
		f <- 0.5*sum(l^2)
		slope <- sum(g0*p)
		# backtrack if the function does not decrease sufficiently
		if(f > f0+0.0001*slope)
		{
			scale <- -slope/(2*(f-f0-slope))
			scale <- min(scale,0.5)
			scale <- max(0.05,scale)
    }
		gamm <- gamm0 + scale*p
	}
	gamm
}
