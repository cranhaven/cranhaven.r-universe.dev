maxent.LP.par <- function(L2.LP.par){
#############################################
## INPUT: L2 representation of LP Means
## OUTPUT:  Maximum Entropy representation of LP Means
##
## The functional form for the MoM using significant LP Means
	maxent.c.sig <- function(u, c.vec, c.ind, sig.ind, mval){
		l.u.mat <- gLP.basis(u, c(1,1), m = mval, con.prior = "Beta")
		den.mat <- cbind(1,l.u.mat[,sig.ind])
		out <- exp(den.mat%*%c.vec)*l.u.mat[,c.ind]
		return(out)
		}
## The functional form for MoM for the constant term
	maxent.c0 <- function(u, c.vec, sig.ind, mval){
		leg.u.mat <- gLP.basis(u, c(1,1), m = mval, con.prior = "Beta")
		leg.upd <- leg.u.mat[,sig.ind]
		fun.mat <- cbind(1,leg.upd)
		out <- exp(fun.mat%*%c.vec)
		return(out)
		}
#### Main function
	m.val <- length(L2.LP.par)
	ME.LP.par <- L2.LP.par
	B <- 1000
	u.grid <- seq(1/B, 1- 1/B, length.out = B)
	c.sig.LP.par <- as.vector(c(1, L2.LP.par[abs(L2.LP.par)>0]))
	#names(c.sig.LP.par)[1] <- "c0"
	c.sig.ind <- which(abs(L2.LP.par)>0)
	###Setup system of equations
	maxent.sys.eq <- function(c.par){
		y <- numeric(length(c.sig.ind)+1)
		for(i in 1:(length(y)-1)){
			y[i] <- sum(maxent.c.sig(u = u.grid, c.vec = c.par, c.ind = c.sig.ind[i], 
							  mval = m.val, sig.ind = c.sig.ind))/ B - L2.LP.par[c.sig.ind[i]]
			}
			y[length(y)] <- sum(maxent.c0(u = u.grid, c.vec = c.par, 
										mval = m.val, sig.ind = c.sig.ind))/B - 1
			y
			}
		maxent.c <- nleqslv(c.sig.LP.par, maxent.sys.eq)$x
		ME.LP.par[c.sig.ind] <- maxent.c[-1] 
		ME.LP.par <- c(maxent.c[1], ME.LP.par)
		names(ME.LP.par) <- paste("LP(ME)", 0:(length(ME.LP.par)-1), sep = "")
		return(ME.LP.par)
		}