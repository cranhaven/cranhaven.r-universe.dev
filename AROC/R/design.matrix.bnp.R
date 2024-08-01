design.matrix.bnp <-
function(formula, data) {
	iform <- interpret.AROCformula(formula, data)
	data.cov <- iform$data.cov
	X <- NULL
	Xterms <- list()
	paracoeff <- TRUE # First "coefficients is parametric"
	if(iform$npartial == 0) { # Only the intercept
        X <- matrix(1, ncol = 1, nrow = nrow(data))
        colnames(X) <- "(Intercept)"
        res <- list()
        res$X <- X
        res$terms <- NULL
        res$iformula <- iform
    } else {
		for(i in 1:iform$npartial) {
			if(any(iform$II[,i] == -1)) {
				if(iform$h[i] == 0) { # Linear and factor
					mf <- model.frame(paste0("~", iform$II[2,i]), data.cov, drop.unused.levels = TRUE)
					mt <- terms(mf)             
                    MM <- model.matrix(mt, mf)[,-1, drop = FALSE] # Here we delete the intercept
                    paracoeff <- c(paracoeff, rep(TRUE, ncol(MM)))
                    X <- cbind(X, MM)
                    attr(mt, "contrast") <- attr(MM,"contrast")
                    attr(mt, "xlev") <- .getXlevels(mt, mf)
                    Xterms[[i]] <- mt
				} else {
					Bs <- bbase.os(data.cov[,iform$II[2,i]], K = iform$K[[i]], intercept = FALSE)
					colnames(Bs) <- paste0(iform$partial[i],".", 1:ncol(Bs))
					paracoeff <- c(paracoeff, rep(FALSE, ncol(Bs)))
					Xterms[[i]] <- Bs                    
					X <- cbind(X, Bs)
				}
			} else { # Factor by curve
				Bs <- bbase.interaction.factor.by.curve.os(data.cov[,iform$II[2,i]], data.cov[,iform$II[1,i]], K = iform$K[[i]])
				colnames(Bs) <- paste0(iform$partial[i],".", 1:ncol(Bs))
				paracoeff <- c(paracoeff, rep(FALSE, ncol(Bs)))
                Xterms[[i]] <- Bs
                X <- cbind(X, Bs)
			}
		}
		# Add the intercept
        names.X <- colnames(X)
        X <- cbind(1, X)
        colnames(X) <- c("(Intercept)", names.X)
        res <- list()
        res$X <- X
        res$terms <- Xterms
        res$iformula <- iform
	}
	res$paracoeff <- paracoeff
	class(res) <- "design.matrix.bnp"
	res
}
