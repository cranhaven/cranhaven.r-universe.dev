DETREND_SJ <-
function(x, tt = 'linear', bp = c()) {

        
        if (!is.numeric(x) && !is.complex(x))
                stop("'x' must be a numeric or complex vector or matrix.")
        trendType <- pmatch(tt, c('constant', 'linear'), nomatch = 0)
        
        if (is.vector(x))
                x <- as.matrix(x)
        n <- nrow(x)

        if (length(bp) > 0 && !all(bp %in% 1:n))
                stop("Breakpoints 'bp' must elements of 1:length(x).")
        
        if (trendType == 1) {  # 'constant'
                if (!is.null(bp))
                        warning("Breakpoints not used for 'constant' trend type.")
                y <- x - matrix(1, n, 1) %*% apply(x, 2, mean)
                
        } else if (trendType == 2) {  # 'linear'
                
                if(length(bp)==0) {
                        bp <- 1
                }
                
		bp <- sort(unique(c(0, c(bp), n-1)))
		lb <- length(bp) - 1

		a <- cbind(matrix(0, n, lb), matrix(1, n, 1))
		for (kb in 1:lb) {
		    m <- n - bp[kb]
		    a[(1:m) + bp[kb], kb] <- as.matrix(1:m)/m
		}
		#print(a)
		#y <- x - a %*% qr.solve(a, x)
		y <- x - a %*% mldivide(a, x)

		# ---- SJ's added section, start-----
		# x is observation matrix in the matrix form: x=A*p so p=A\x
		# A is the design matrix
		# p is the un-known parameters vector

		A <- cbind(matrix(0, n, lb), matrix(1, n, 1))
		for (k in 1:lb){
		    A[(bp[k]+1):bp[k+1],k] <- 1:(bp[k+1]-bp[k])
		    A[(bp[k+1]+1):n,k] <- A[bp[k+1],k]
		}


                z <- qr(A, LAPACK = TRUE)
                Q <- qr.Q(z)
                R <- qr.R(z)


		sgn <- sign(diag(R))
		#new R and Q to avoid minus
		R <- diag(sgn) %*% R
		Q <- Q %*% diag(sgn)

		#p <- R\(Q'*x)# first coefficient is slope of the first segment, and so on, last one is the intercept of the first segment
		#r <- x - A*p# residuals same as y = x - a*(a\x) in line 52

                #p <- qr.solve(R, (t(Q)%*%x) ) # changed last

                b2 <- (t(Q)%*%x)
		p <- mldivide(R, b2)

		param_no <- length(p)#  number of unknowm paramters in the LS fit, same as lbp in line 43

		# S is a structure containing three elements: the triangular factor from a
		# QR decomposition of the Vandermonde matrix, the degrees of freedom and
		# the norm of the residuals.

		# df = max(0,length(x)-param_no);# d.f. for large data

		#H <- A %*% pinv( t(A) %*% A) %*% t(A)
		SVD <- svd(A)
		H <- tcrossprod(SVD$u)

                dfSJ <- abs(length(x) - 1.25 * sum(diag(H)) + 0.5)
                normr <- max(svd(y)$d)
		# ---- SJ's added section , End

                
        } else {
                stop("Trend type 'tt' must be 'constant' or 'linear'.")
        }
        
        
        DTSJ.values <- list(
                "y" = y,                
                "param_no" = param_no,
                "p" = p,
                "R" = R,
                "dfSJ" = dfSJ,
                "normr" = normr
        )
        
        class(DTSJ.values) <- "DTSJ"
        return(DTSJ.values)
}
