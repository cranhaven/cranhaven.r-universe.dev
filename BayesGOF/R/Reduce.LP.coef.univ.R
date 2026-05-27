Reduce.LP.coef.univ <-
function(wght.mat, LP.par, leg.mat, freq.vec = NULL, k.tot = NULL){
	L.mat <- matrix(0, nrow = length(LP.par), ncol = dim(wght.mat)[2])
	L.mat <- apply(wght.mat, 2, function(x) EXP.score(LP.par, x, leg.mat))
	if(!is.null(freq.vec) && !is.null(k.tot)){
		new.c.vec <- as.vector( (L.mat %*% as.vector(freq.vec) ) )/k.tot
		}else{
		if(!is.null(ncol(L.mat))){
			new.c.vec <- rowMeans(L.mat)
			} else {
			new.c.vec <- mean(L.mat)
			}
		}	
	return(new.c.vec)
}