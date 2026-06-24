
t_col <- function(color, percent = 50, name = NULL) {
	rgb.val <- col2rgb(color)

	t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3], maxColorValue = 255,
             alpha = (100 - percent) * 255 / 100,
             names = name)


	invisible(t.col)
}

MatTrans.plot <- function(X, model = NULL, xlab = "", ylab = "", rownames = NULL, colnames = NULL, lwd.obs = 0.8, lwd.mean = 2, line.cols = NULL, ...){

	if(is.null(model)){
		stop("Please specify which model to plot...\n")
	}
	else{
		cat("Mean profile plot is provided for the best model...\n")

		K <- max(model$best.result[[1]]$id)
		p <- dim(X)[1]
		T <- dim(X)[2]
		n <- dim(X)[3]
		if(is.null(line.cols)){
			line.cols <- seq(1, p)
		}
		#par(mfrow = c(1,K))

		for(k in 1:K){

			plot(c(0,1), c(min(X),max(X)), type = "n", xlab = xlab, ylab = ylab,  xaxt='n', main = paste("Cluster", k))
			if(is.null(colnames)){
				colnames <- NULL
				for(t in 1:T){
					colnames <- c(colnames, paste("Col", t, sep = ""))
				}
			}
			axis(1, at = seq(0, 1, length.out = T), labels = colnames)
			box()

			for(j in 1:p){
				if(model$trans == "None"){
					values <- model$best.result[[1]]$Mu[j,,k]
				}
				else if(model$trans == "Power"){
					ind <- model$best.result[[1]]$Mu[j,,k] > 0 
					values <- rep(NA, T)
					values[ind] <- (model$best.result[[1]]$Mu[j,,k][ind] * model$best.result[[1]]$LA[j,,k][ind] + 1)^(1/model$best.result[[1]]$LA[j,,k][ind]) - 1
					values[!ind] <- 1-(model$best.result[[1]]$Mu[j,,k][!ind] * (model$best.result[[1]]$LA[j,,k][!ind]-2) + 1)^(1/(2-model$best.result[[1]]$LA[j,,k][!ind]))
				}
				else if(model$trans == "Manly"){
					values <- log(model$best.result[[1]]$Mu[j,,k]* model$best.result[[1]]$LA[j,,k] + 1) / model$best.result[[1]]$LA[j,,k]
				}

				for(i in 1:n){
					if(model$best.result[[1]]$id[i]==k){
						lines(seq(0, 1, length.out = T), X[j,,i], col = t_col(line.cols[j], percent = 80), lwd = lwd.obs, ...)	
					}
				}
				lines(seq(0, 1, length.out = T), values, col = line.cols[j], lwd = lwd.mean, ...)
	
			}
			
		}

		if(is.null(rownames)){
			rownames <- NULL
			for(j in 1:p){
				rownames <- c(rownames, paste("Row", j, sep = ""))
			}
		}

		legend(0.3, min(X) + 1/5*(max(X) - min(X)), legend=rownames, col=line.cols, lty = rep(1, p), cex = 1)
		#oldpar <- par(no.readonly = TRUE) # code line i
		#on.exit(par(oldpar)) # code line i + 1
	}
}

