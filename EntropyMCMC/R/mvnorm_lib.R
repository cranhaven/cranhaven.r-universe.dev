# Collection of utility functions, using fts imported from the mixtools package,
# which provide alternative and more efficient versions 


ellipse <- function(mu, sigma, alpha=.05, npoints=250,
                    newplot=FALSE, draw=TRUE, ...) {
  es <- eigen(sigma)
  e1 <- es$vec%*%diag(sqrt(es$val))
  r1 <- sqrt(qchisq(1-alpha,2))
  theta <- seq(0,2*pi,len=npoints)
  v1 <- cbind(r1*cos(theta),r1*sin(theta))
  pts=t(mu-(e1%*%t(v1)))
  if (newplot && draw) {
    plot(pts, ...)
  } else if (!newplot && draw) {
    lines(pts, ...)
  }
  invisible(pts)
}

######## Weighted KDE - multidim #########
# version vectorized on dmvnorm argument, 
# using the symmetry in u,x 
# (partially vectorized version)
mvwkde <- function(x,u=x,w=rep(1,dim(x)[1]),
				bw=bw.nrd0(as.vector(x))){
t <- dim(u)[1]; n <- dim(x)[1]; d <- ncol(x)
w <- w/sum(w)     # normalized weights
sig <- bw^2*diag(d) # covariance matrix var=bw^2 (checked)
f <- rep(0,t) # storing wkde(u)
for (j in 1:t){ # for each point u_j
	G <- mixtools::dmvnorm(x, mu=u[j,], sigma=sig)
	f[j] <- sum(w*G)
	}
f
}

######## KDE - multidim #########
# same as mvwkde but for default weights 1/n
# version vectorized on dmvnorm argument, 
# using the symmetry in u,x 
# (partially vectorized version)
# bw should be a matrix (d,d) 
mvkde <- function(x, u=x, bw=NULL){
t <- dim(u)[1]; n <- dim(x)[1]; d <- ncol(x)
if (is.null(bw)) {
	bw=bw.nrd0(as.vector(x)) # poor for d large
	sig <- bw^2*diag(d) # covariance matrix var=bw^2 
	}
	else sig <- bw
f <- rep(0,t) # storing fhat(u)
for (j in 1:t){ # for each point u_j
	G <- mixtools::dmvnorm(x, mu=u[j,], sigma=sig)
	f[j] <- sum(G)/n
	}
f
}
