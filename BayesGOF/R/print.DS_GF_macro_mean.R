print.DS_GF_macro_mean <-
function(x, ...){
	mean.mat <- matrix(0, nrow = 1, ncol = 3)
	mean.mat[,1] <- x$model.mean-x$mean.sd
	mean.mat[,2] <- x$model.mean
	mean.mat[,3] <- x$model.mean+x$mean.sd
	colnames(mean.mat) <- c("1SD Lower Limit","Mean", "1SD Upper Limit")
	print(round(mean.mat,4))
}

