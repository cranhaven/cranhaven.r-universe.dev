determinejob <- function(r,n){
	stopifnot(r <= n*(n-1)/2 + n)
	deduct <- n
	subtractions <- 0
	while(r > 0){
		r <- r - deduct
		subtractions = subtractions + 1
		deduct = deduct - 1
	}
	A <- subtractions
	B <- r + n
	return(c(A,B))
}