
fertilizers <- function() {
	f <- system.file("extdata/fertilizers.csv", package="Rquefts")
	utils::read.csv(f)
}



nutrientRates <- function(supply, treatment) {
#	result <- matrix(nrow=ncol(supply)-1, ncol=ncol(treatment)-1)
	NPK <- supply[, c("N", "P", "K")] * treatment / 100
	apply(NPK, 2, sum)
}




# minimize total cost of a fertilizer treatment
# based on a function by Pieter Pypers
fertApp <- function(nutrients, fertilizers, price, exact=TRUE, retCost=FALSE){

	stopifnot(length(price) == nrow(fertilizers))
	name <- fertilizers$name
	supply <- t(as.matrix(fertilizers[,-1,drop=FALSE]))
	treatment <- as.matrix(nutrients)

	result <- matrix(nrow=ncol(supply), ncol=nrow(treatment))
	colnames(result) <- rownames(nutrients)
	if (!any(is.na(price))) {
		treatment <- treatment * 100
		for (i in 1:nrow(treatment)) {
			if (exact) {
				solution <- limSolve::linp(E=supply, F=treatment[i,], Cost=price)
			} else {
				solution <- limSolve::linp(G=supply, H=treatment[i,], Cost=price)		
			}
			if (solution$IsError) {
				result[,i] <- NA
			} else { 	
				result[,i] <- solution$X
			}
		}
	}
	if (retCost) {
		r <- colSums(result * price)
	} else {
		r <- data.frame(name, result)
		#r[apply(r[,-1,drop=FALSE], 1, function(i) !all(i==0)), ]	
	}
	r
}

