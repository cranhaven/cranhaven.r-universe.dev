k.points.max.cpe <-
function(formula, cat.var, data, range, k, l.s.points = 100 , min.p.cat=1) {
	point=NULL
	mat=matrix(ncol=2,nrow=k)
	colnames(mat)<- c("point","cpe")
	for (i in 1:k){
		cpes <- select.cutpoint.cpe(formula = formula, cat.var = cat.var, data = data, range = range, point = point, l.s.points = l.s.points, min.p.cat = min.p.cat)
		pos <- which(cpes[,2]==max(cpes[,2],na.rm=TRUE))
		if(length(pos)> 1 & i > 1) {
			if (cpes[pos[1],1] <= mat[i-1,1]) {
				pos <- max(pos)
			} else {
				pos = min(pos)
			}
		} else {
			pos = pos[1]
		}
		mat[i,1] <- cpes[pos,1]
		mat[i,2] <- cpes[pos,2]
		point <- c(point,mat[i,1])
	}
	mat
}
