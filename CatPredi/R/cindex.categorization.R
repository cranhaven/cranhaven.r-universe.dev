cindex.categorization <-
function (x, y) {
	if (!is.Surv(y))
		y <- Surv(y)
	
	i <- is.na(x) | is.na(y)
	if (any(i)) {
		x <- x[!i]
		y <- y[!i, ]
	}
	k <- concordancefit(y, x)$count
	cindex <- (k[2] + (k[3]+k[4]+k[5])/2)/sum(k[1:5])
	cindex
}
