npn = function(x, npn.func = "shrinkage"){
	gcinfo(FALSE)
	n = nrow(x)
  	p = ncol(x)
  	x.col = colnames(x)
  	x.row = rownames(x)
  	
	if(npn.func == "shrinkage"){
		
		x = qnorm(apply(x,2,rank)/(n+1))
		x = x/sd(x[,1])
   		colnames(x) = x.col
		rownames(x) = x.row
	}
	
	if(npn.func == "skeptic"){
		x = 2*sin(pi/6*cor(x,method="spearman"))
  		colnames(x) = x.col
		rownames(x) = x.col
	}
	return(x)
}