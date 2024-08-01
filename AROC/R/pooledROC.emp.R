pooledROC.emp <-
function(y0, y1, p = seq(0,1,l=101), B = 500, method = c("ncoutcome","coutcome")) {
	method <- match.arg(method)
	n1 <- length(y1)
	n0 <- length(y0)
	np <- length(p)
	compute.ROC <- function(y0, y1, p = seq(0,1,l=101)) {
		F1emp <- ecdf(y1)
		rocemp <- 1 - F1emp(quantile(y0,1-p))
		aucemp <- simpson(rocemp, p)
		res <- list()
		res$p <- p
		res$ROC <- rocemp
		res$AUC <- aucemp
		res

	}
	res <- compute.ROC(y0, y1, p = p)
	rocemp <- res$ROC
	aucemp <- res$AUC
	if(B > 0) {
		rocempb <- matrix(0,nrow = np, ncol = B)
		aucempb <- numeric(B)
		data.original <- data.frame(y = c(y0, y1), group = c(rep(0,n0), rep(1,n1)))
		for(l in 1:B){
			data.boot <- bootstrap.sample(data.original, "group", method = method)
			y0b <- data.boot$y[data.boot$group == 0]
			y1b <- data.boot$y[data.boot$group == 1]
			res.boot <- compute.ROC(y0b, y1b, p = p)
			rocempb[,l] <- res.boot$ROC
			aucempb[l]  <- res.boot$AUC
		}
	}
	columns <-switch(as.character(B>0),"TRUE" = 1:3,"FALSE"=1)			    		
	col.names <-c("est","ql", "qh")[columns]

	poolROC <- matrix(0, ncol = length(columns), nrow = np, dimnames = list(1:np, col.names))
	poolROC[,1] <- rocemp
	AUC <- aucemp
	if(B > 0) {
		poolROC[,2] <- apply(rocempb,1,ql)
		poolROC[,3] <- apply(rocempb,1,qh)
		AUC <- c(AUC, quantile(aucempb,c(0.025,0.975)))
	}
	names(AUC) <- col.names

	res <- list()
	res$call <- match.call()
	res$marker <- list(h = y0, d = y1)
	res$p <- p
	res$ROC <- poolROC
	res$AUC <- AUC
	class(res) <- c("AROC","pooledROC.emp")
	res
}
