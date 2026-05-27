print.DS_GF_macro_mode <-
function(x, ...){
	mode.mat <- matrix(0, nrow = length(x$model.modes), ncol = 3)
	for(i in 1:length(x$model.modes)){
		mode.mat[i,1] <- x$model.modes[i]-x$mode.sd[i]
		mode.mat[i,2] <- x$model.modes[i]
		mode.mat[i,3] <- x$model.modes[i]+x$mode.sd[i]
		}
	colnames(mode.mat) <- c("1SD Lower Limit","Mode", "1SD Upper Limit")
	print(round(mode.mat,4))
}
