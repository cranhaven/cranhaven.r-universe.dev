pooledROC.BB <-
function(y0, y1, p = seq(0,1,l=101), B = 5000) {
	n1 <- length(y1)
	n0 <- length(y0)

	weights.h <- matrix(0, nrow = n0, ncol = B)
    weights.d <- matrix(0, nrow = n1, ncol = B)

	np <- length(p)
	u <- matrix(0,nrow = n1, ncol = B)
	for(l in 1:B){
	  q <- rexp(n0,1)
	  weights.h[,l] <- q/sum(q)
	  for(j in 1:n1){
	    u[j,l]<- sum(weights.h[,l]*(y0>y1[j]))  
	  }  
	}
	rocbbpool <- matrix(0, nrow = np, ncol = B)
	aucbbpool <- numeric(B)
	for(l in 1:B) {
	  q1 <- rexp(n1,1)
	  weights.d[,l] <- q1/sum(q1)  
	  for(j in 1:np){
	    rocbbpool[j,l] <- sum(weights.d[,l]*(u[,l]<=p[j]))   
	  }
	  aucbbpool[l] <- simpson(rocbbpool[,l], p)
	}
	poolROC <- matrix(0, ncol = 3, nrow = np, dimnames = list(1:np, c("est","ql", "qh")))
	poolROC[,1] <- apply(rocbbpool,1,mean)
	poolROC[,2] <- apply(rocbbpool,1,ql)
	poolROC[,3] <- apply(rocbbpool,1,qh)

	res <- list()
	res$call <- match.call()
	res$marker <- list(h = y0, d = y1)
	res$p <- p
	res$ROC <- poolROC
	AUC <- c(mean(aucbbpool), quantile(aucbbpool,c(0.025,0.975)))
	names(AUC) <- c("est","ql", "qh")
	res$AUC <- AUC
	res$weights <- list(h = weights.h, d = weights.d)
	class(res) <- c("AROC","pooledROC.BB")
	res
}
