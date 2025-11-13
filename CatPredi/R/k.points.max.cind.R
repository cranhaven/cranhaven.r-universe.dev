k.points.max.cind <-
function(formula, cat.var, data, range, k, l.s.points = 100 , min.p.cat=1) {

	point=NULL
	mat=matrix(ncol=2,nrow=k)
	colnames(mat)<- c("point","c-index")

	for (i in 1:k){
		cinds <- select.cutpoint.cind(formula = formula, cat.var = cat.var, data = data, range = range, point = point, l.s.points = l.s.points, min.p.cat = min.p.cat)
		pos <- which(cinds[,2]==max(cinds[,2],na.rm=TRUE))
		if(length(pos)> 1 & i > 1) {
			if (cinds[pos[1],1] <= mat[i-1,1]) {
				pos <- max(pos)
			} else {
				pos = min(pos)
			}
		} else {
			pos = pos[1]
		}
		mat[i,1] <- cinds[pos,1]
		mat[i,2] <- cinds[pos,2]
		point <- c(point,mat[i,1])
	}
	mat
}
