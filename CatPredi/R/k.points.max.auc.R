k.points.max.auc <-
function(formula, cat.var, data, range, k = 1, l.s.points = 100, min.p.cat = 1) {

	points <- NULL
	mat <-  matrix(ncol=2,nrow=k)
	colnames(mat)<- c("point","auc")

	for (i in 1:k){
		aucs <- select.cutpoint.auc(formula = formula, cat.var = cat.var, data = data, range = range, points = points, l.s.points = l.s.points, min.p.cat = min.p.cat)
		pos <- which(aucs[,2]==max(aucs[,2],na.rm=TRUE))
		if(length(pos)> 1 & i > 1) {
		  if (aucs[pos[1],1] <= mat[i-1,1]) {
			  pos <- max(pos)
		  } else {
		  pos = min(pos)
		  }
		} else {
			  pos = pos[1]
		}
		mat[i,1] <- aucs[pos,1]
		mat[i,2] <- aucs[pos,2]
		points <- c(points,mat[i,1])
	}
	mat
}
