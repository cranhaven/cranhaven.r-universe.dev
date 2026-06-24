
eucl.dist <- function(X, Y){

	sqrt(sum(X - Y)^2)

}

Trans.bic <- function(logl, n, M){
	return(-2 * logl + M * log(n))
}

# EM algorithm
MatTrans.init <- function(Y, K, n.start = 10, scale = 1){

	Y <- Y/scale	
	A <- dim(Y)
	p <- A[1]
	T <- A[2]
	n <- A[3]

	if(n < 1) stop("Wrong number of observations n...\n")
	if(p < 1) stop("Wrong dimensionality p...\n")
	if(T < 1) stop("Wrong dimensionality T...\n")


	init <- list()
	i <- 0
	
	Psi.inv <- array(NA, c(T, T, K))
	Sigma.inv <- array(NA, c(p, p, K))
	detPsi <- rep(NA, K)
	detS <- rep(NA, K)

	
	repeat{

		s <- sample(1:n,K)
		
		mat.Y <- t(apply(Y, 3, as.matrix))

		iif_1 <- 0

		if(p>1){iif_1 <- 1}
		if(T>1){iif_1 <- 1}

		if(iif_1 == 1){
			centers <- mat.Y[s,]
		}
		else{
			centers <- mat.Y[s]
		}

		D <- NULL
		if(K == 1) {D <- cbind(D, apply(mat.Y, 1, eucl.dist, centers))}
		else{

			iif_2 <- 0

			if(p>1){iif_2 <- 1}
			if(T>1){iif_2 <- 1}

			if(iif_2 == 1){
				for (k in 1:K) D <- cbind(D, apply(mat.Y, 1, eucl.dist, centers[k,]))
			}
			else{
				for (k in 1:K) D <- cbind(D, t(mat.Y - centers[k])^2)
			}
		}
		PI <- as.matrix(D == apply(D, 1, min)) * 1
		W.result <- NULL

		iif <- 0

		for (k in 1:K){
			index <- PI[,k] == 1


			iif3 <- 0

			if((p>1) && (T>1)){
				iif3 <- 1
			}
			
			iif4 <- 0
			
			if((p==1) && (T>1)){iif4 <- 1}
			
			
			B <- NULL
			C <- NULL
		
			if(iif3 == 1){
				var.est <- apply(Y[,,index], 2, as.matrix, byrow = TRUE)
				var.est <- var(var.est)
				C <- try(Psi.inv[,,k] <- solve(var.est))
				detPsi[k] <- 1/det(Psi.inv[,,k])
				var.est <- apply(Y[,,index], 1, as.matrix, byrow = TRUE)
				var.est <- var(var.est)
				B <- try(Sigma.inv[,,k] <- solve(var.est))
				detS[k] <- 1/det(Sigma.inv[,,k])

			}
			if(iif4 == 1){
				var.est <- var(t(Y[,,index]))
				C <- try(Psi.inv[,,k] <- solve(var.est))
				detPsi[k] <- 1/(Psi.inv[,,k])
				var.est <- var(as.vector(Y[,,index]))
				B <- try(Sigma.inv[,,k] <- solve(var.est))
				detS[k] <- 1/det(Sigma.inv[,,k])
			}
			if(T == 1){
				var.est <- var(as.vector(Y[,,index]))
				C <- try(Psi.inv[,,k] <- solve(var.est))
				detPsi[k] <- 1/(Psi.inv[,,k])
				var.est <- var(t(Y[,,index]))
				B <- try(Sigma.inv[,,k] <- solve(var.est))
				detS[k] <- 1/det(Sigma.inv[,,k])

			}
			
			if(inherits(C, "try-error")){
				
				iif <- 1
			}	
			if(inherits(B, "try-error")){
				
				iif <- 1
			}
		}
		if(iif == 0){
			W.result$y <- as.vector(Y)				
			W.result$gamma1 <- PI
			W.result$invS1 <- Sigma.inv
			W.result$tau <- rep(0, K)
			W.result$Mu1 <- rep(0, p*T*K)
			W.result$invPsi1 <- Psi.inv
			W.result$detS <- detS
			W.result$detPsi <- detPsi
			W.result$scale <- scale				
	
			i <- i + 1
			init[[i]] <- W.result	

		}
		if (i == n.start) break

	}
	return(init)

}



code.convert <- function(code){
  Mu <- substr(code,1,1)
  Sigma <- substr(code,3,5)
  Psi <- substr(code,7,8)
  
  Mu.num <- ifelse(Mu == "A", 1,
	   ifelse(Mu == "G", 2, 
	   ifelse(Mu == "X", 0, -1)))

  Sigma.num <- ifelse(Sigma == "EII", 1,
               ifelse(Sigma == "VII", 2,
               ifelse(Sigma == "EEI", 3,
               ifelse(Sigma == "VEI", 4,
               ifelse(Sigma == "EVI", 5,
               ifelse(Sigma == "VVI", 6,
               ifelse(Sigma == "EEE", 7,
               ifelse(Sigma == "VEE", 8,
               ifelse(Sigma == "EVE", 9,
               ifelse(Sigma == "VVE", 10,
               ifelse(Sigma == "EEV", 11,
               ifelse(Sigma == "VEV", 12,
               ifelse(Sigma == "EVV", 13,
               ifelse(Sigma == "VVV", 14,
 	       ifelse(Sigma == "XXX", 0, -1)))))))))))))))
  
  Psi.num <- ifelse(Psi == "II", 1,
             ifelse(Psi == "EI", 2,
             ifelse(Psi == "VI", 3,
             ifelse(Psi == "EE", 4,
             ifelse(Psi == "VE", 5,
             ifelse(Psi == "EV", 6,
             ifelse(Psi == "VV", 7,              
	     ifelse(Psi == "AR", 8, 
	     ifelse(Psi == "XX", 0, -1)))))))))
  
  list(Mu = Mu, Sigma = Sigma, Psi = Psi, Mu.num = Mu.num, Sigma.num = Sigma.num, Psi.num = Psi.num)
}


code.back <- function(num){


  Mu.num <- num[1]
  Sigma.num <- num[2]
  Psi.num <- num[3]
 
  Mu <- ifelse(Mu.num == 1, "A",
	ifelse(Mu.num == 2, "G", NULL))
	
 
  Sigma <- ifelse(Sigma.num == 1, "EII",
           ifelse(Sigma.num == 2, "VII", 
           ifelse(Sigma.num == 3, "EEI", 
           ifelse(Sigma.num == 4, "VEI", 
           ifelse(Sigma.num == 5, "EVI", 
           ifelse(Sigma.num == 6, "VVI", 
           ifelse(Sigma.num == 7, "EEE", 
           ifelse(Sigma.num == 8, "VEE", 
           ifelse(Sigma.num == 9, "EVE", 
           ifelse(Sigma.num == 10, "VVE", 
           ifelse(Sigma.num == 11, "EEV", 
           ifelse(Sigma.num == 12, "VEV", 
           ifelse(Sigma.num == 13, "EVV", 
           ifelse(Sigma.num == 14, "VVV", NULL))))))))))))))
  
  Psi <- ifelse(Psi.num == 1, "II", 
         ifelse(Psi.num == 2, "EI", 
         ifelse(Psi.num == 3, "VI", 
         ifelse(Psi.num == 4, "EE", 
         ifelse(Psi.num == 5, "VE", 
         ifelse(Psi.num == 6, "EV", 
         ifelse(Psi.num == 7, "VV", 
         ifelse(Psi.num == 8, "AR", NULL))))))))
  
  list(Mu = Mu, Sigma = Sigma, Psi = Psi, Mu.num = Mu.num, Sigma.num = Sigma.num, Psi.num = Psi.num)
}





MatTrans.EM.orig <- function(Y, initial = NULL, la = NULL, nu = NULL, model = NULL, trans = "None", la.type = 0, row.skew = TRUE, col.skew = TRUE, tol = 1e-05, max.iter = 1000, size.control = 0, silent = TRUE, check = TRUE){

	A <- dim(Y)
	p <- A[1]
	T <- A[2]
	n <- A[3]
	

	if(n < 1) stop("Wrong number of observations n...\n")
	if(p < 1) stop("Wrong dimensionality p...\n")
	if(T < 1) stop("Wrong dimensionality T...\n")

	if(!is.null(initial)){
		
		if(length(initial) < 1) stop("Wrong initialization...\n")

		K <- length(initial[[1]]$tau)

		if(trans != "None"){

			if((row.skew == FALSE) && (col.skew == FALSE)){
				trans <- "None"
				message("No row skewness or column skewness: trans method is set to 'None' \n")
			}

			else if(row.skew == FALSE){
				if(trans == "Power"){
					la <- matrix(1, K, p)
					message("No row skewness lambda -- 1 \n")
				}
				else if(trans == "Manly"){
					la <- matrix(0, K, p)
					message("No row skewness lambda -- 0 \n")
				}
			}
			else if(col.skew == FALSE){
				if(trans == "Power"){
					nu <- matrix(1, K, T)
					message("No column skewness nu -- 1 \n")
				}
				else if(trans == "Manly"){
					nu <- matrix(0, K, T)
					message("No column skewness nu -- 0 \n")
				}
			}
		}



		if(is.null(la)){
			if(trans != "None"){
				la <- matrix(0.5, K, p)
				message("Initial lambda -- 0.5 \n")
			}
		}
		if(is.null(nu)){
			if(trans != "None"){
				nu <- matrix(0.5, K, T)
				message("Initial nu -- 0.5 \n")
			}
		}
		if(la.type == 0){
			if(trans != "None"){
				if(row.skew){
					message("Unrestricted lambda type \n")
				}
			}
		}
		if(la.type == 1){
			if(trans != "None"){
				if(row.skew){
					message("Lambda same across all variables \n")
				}
			}
		}



		if(is.null(model)){
			index <- matrix(NA, 3, 14*8*2)
			Model <- rep(NA, 14*8*2)
			iter <- 0
			for(Mu.type in 1:2){
				for(Sigma.type in 1:14){
					for(Psi.type in 1:8){
																	iter <- iter + 1
						Model[iter] <- paste(code.back(c(Mu.type, Sigma.type, Psi.type))$Mu, "-", code.back(c(Mu.type, Sigma.type, Psi.type))$Sigma, "-", code.back(c(Mu.type, Sigma.type, Psi.type))$Psi, sep="")
								
						index[,iter] <- c(Mu.type, Sigma.type, Psi.type)
					}
				}
			}
		}

		else{
			
			index <- NULL
			Model <- NULL



			for(ij in 1:length(model)){
				
				code1 <- code.convert(model[ij])$Mu.num
				code2 <- code.convert(model[ij])$Sigma.num
				code3<- code.convert(model[ij])$Psi.num
			
				#stop(code3,"\n")
				index.temp <- c(code1, code2, code3)
				
				#stop(index.temp, "\n")

 				if(any(index.temp == -1)){
					message("model code is not identifiable...\n")
				}
				else if(any(index.temp == 0)){

					if(code1 == 0){
						code1 <- c(1,2)
					}
					if(code2 == 0){
						code2 <- seq(1,14)
					}
					if(code3 == 0){
						code3 <- seq(1,8)
					}

					
					index.temp <- t(expand.grid(x = code1, y = code2, z = code3))
					for(ij2 in 1:dim(index.temp)[2]){
						Model <- c(Model, paste(code.back(index.temp[,ij2])$Mu, "-", code.back(index.temp[,ij2])$Sigma, "-", code.back(index.temp[,ij2])$Psi, sep=""))
					}
						

				}
				else{
					Model <- c(Model, paste(code.back(index.temp)$Mu, "-", code.back(index.temp)$Sigma, "-", code.back(index.temp)$Psi, sep=""))
				}

				index <- cbind(index, index.temp)

				#stop(index, "\n")

			}

			
		}



		loglik <- rep(-Inf, dim(index)[2])
		bic <- rep(Inf, dim(index)[2]) 

				
		if(trans == "Power"){
			trans.type <- 1
			message("Power transformation \n")
			if(initial[[1]]$scale != 1){
				message("Models are fitted to the scaled data. Results are not scaled back. \n" )
				for(i in 1:length(initial)){
					initial[[i]]$scale <- 1
				}
			}
		}
		else if(trans == "Manly"){

			trans.type <- 2
			message("Manly transformation \n")
			if(initial[[1]]$scale != 1){
				stop("Models are fitted to the scaled data. Results are not scaled back. \n" )
				for(i in 1:length(initial)){
					initial[[i]]$scale <- 1
				}
			}

		}
		else if(trans == "None"){
			trans.type <- 0
			message("None -- no transformation \n")
			if(row.skew || col.skew){
				message("trans method is set to 'None' -- row.skew and col.skew are set to FALSE \n") 
			}
			if(initial[[1]]$scale != 1){
				message("Models are fitted to the scaled data. Log-likelihood and BIC values are scaled back. \n" )
			}

			
		}

		result <- list()



		for(i in 1:length(initial)){
			if(silent != TRUE){
				message("Initialization", i, "\n")
			}
	
			ll <- rep(0, 3)
			misc_double <- c(tol, 0.0, 0.0)
			conv <- rep(0, 3)
			id <- rep(0, n)

			r <- NULL

			for(iter in 1:dim(index)[2]){

				
				if(trans == "Power"){
					trans.type <- 1
					
				}
				else if(trans == "Manly"){

					trans.type <- 2
					

				}
				else if(trans == "None"){
					trans.type <- 0
					
				}

				#stop("trans.type", trans.type, "\n")
				misc_int <- c(p, T, n, K, max.iter, index[1,iter], index[2,iter], index[3,iter], la.type, trans.type)

				#stop(la, "la", "\n")

				try0 <- try(temp <- .C("run_Mat_Trans_Full", y = as.double(initial[[i]]$y), misc_int = as.integer(misc_int), misc_double = as.double(misc_double), tau = as.double(initial[[i]]$tau), la1 = as.double(as.vector(la)), nu1 = as.double(as.vector(nu)), Mu1 = as.double(initial[[i]]$Mu1), invS1 = as.double(initial[[i]]$invS1), invPsi1 = as.double(initial[[i]]$invPsi1), detS = as.double(initial[[i]]$detS), detPsi = as.double(initial[[i]]$detPsi), gamma1 = as.double(initial[[i]]$gamma1), id = as.integer(id), ll = as.double(ll), conv = as.integer(conv), scale = as.double(initial[[i]]$scale), PACKAGE = "MatTransMix"), silent = TRUE)
				
					 

				try1 <- try(invS <- array(temp$invS1, dim = c(p, p, K)))
				try2 <- try(invPsi <- array(temp$invPsi1, dim = c(T, T, K)))
				
				try3 <- try(Sigma <- array(apply(invS, 3, solve), dim = c(p,p,K)))
				try4 <- try(Psi <- array(apply(invPsi, 3, solve), dim = c(T,T,K)))

				iif_0 <- 0
		
				if(inherits(try0, "try-error")){	
					iif_0 <- 1
				}
		
				if(inherits(try1, "try-error")){
					iif_0 <- 1
				}
				if(inherits(try2, "try-error")){
					iif_0 <- 1
				}

				if (iif_0 != 1){

					if(silent != TRUE){
																	message("Model", Model[iter], "BIC", temp$ll[2],"\n")
																											}
					if(!is.na(temp$ll[1])){	
					if(temp$ll[1] > loglik[iter]){
					if((!inherits(try3, "try-error")) && (!inherits(try4, "try-error"))){
						if(check == TRUE){
							check1 <- all(table(temp$id) > size.control)
							check2 <- length(table(temp$id))==K
							check3 <- temp$conv[2] != 1
							if(check1 && check2 && check3){

								loglik[iter] <- temp$ll[1]
								bic[iter] <- temp$ll[2]				
								r <- NULL	
								r$tau <- temp$tau
								if(trans.type != 0){
									r$la <- matrix(temp$la1, nrow = K)
									r$nu <- matrix(temp$nu1, nrow = K)
									r$LA <- array(NA, dim = c(p, T, K))
									for(k in 1:K){
									for(j in 1:p){
									for(t in 1:T){
																				r$LA[j,t,k] <- r$la[k,j] + r$nu[k,t]
									}
									}
									}
								}
						
								else{
									r$la <- NA
									r$nu <- NA
									r$LA <- NA

								}
								r$Sigma <- Sigma
								r$Psi <- Psi
								r$detS <- temp$detS
								r$detPsi <- temp$detPsi
								r$Mu <- array(temp$Mu1, dim = c(p, T, K))
								r$gamma <- matrix(temp$gamma1, nrow = n)
								r$iter <- temp$conv[1]
								r$pars <- temp$conv[3]

								r$id <- temp$id
								r$flag <- temp$conv[2]
								r$ll <- temp$ll[1]
								r$bic <- temp$ll[2]
							
								result[[iter]] <- r
							}
						}
						else{
							loglik[iter] <- temp$ll[1]	
							bic[iter] <- temp$ll[2]
											
							r <- NULL
						
							r$tau <- temp$tau
							if(trans.type != 0){
								r$la <- matrix(temp$la1, nrow = K)
								r$nu <- matrix(temp$nu1, nrow = K)
								r$LA <- array(NA, dim = c(p, T, K))
								for(k in 1:K){
								for(j in 1:p){
								for(t in 1:T){
								r$LA[j,t,k] <- r$la[k,j] + r$nu[k,t]
								}
								}
								}
							}
							else{
								r$la <- NA
								r$nu <- NA
								r$LA <- NA

							}
							r$Sigma <- Sigma
							r$Psi <- Psi
							r$detS <- temp$detS
							r$detPsi <- temp$detPsi
							r$Mu <- array(temp$Mu1, dim = c(p, T, K))
							r$gamma <- matrix(temp$gamma1, nrow = n)
							r$iter <- temp$conv[1]
							r$pars <- temp$conv[3]

							r$id <- temp$id
							r$flag <- temp$conv[2]
							r$ll <- temp$ll[1]
							r$bic <- temp$ll[2]
							
							result[[iter]] <- r


						}
											
						if(trans.type == 2){

							adj <- cdf.adjust(K, r$Mu, r$Sigma, r$Psi, r$la, r$nu)

							
							ll.new <- 0

							for(obs in 1:n){
								dens <- 0
								for(k in 1:K){
									dens <- dens + r$tau[k] * manly.dens(as.matrix(Y[,,obs], p, T), r$Mu[,,k], r$detS[k], r$detPsi[k], invS[,,k], invPsi[,,k], r$LA[,,k]) /adj[k]
									
								}
								ll.new <- ll.new + log(dens)
							}

							
							result[[iter]]$ll <- ll.new
							M <- (r$bic + 2*r$ll) / log(n)
							result[[iter]]$bic <- M *log(n) -2 *result[[iter]]$ll

							loglik[iter] <- ll.new	
							bic[iter] <- result[[iter]]$bic					
						}


						
					}	
					}
					}
					

				}

			}


		}				


		find.min <- which.min(bic)
		best.result <- result[find.min]
		best.model <- Model[find.min]
		best.loglik <- loglik[find.min]
		best.bic <- bic[find.min]


		
		ret <- list(scale = initial[[1]]$scale, result = result, model = Model, loglik = loglik, bic = bic, best.result = best.result, best.model = best.model, best.loglik = best.loglik, best.bic = best.bic, trans = trans)
	
		class(ret) <- "MatTransMix"
	

		return(ret)

	}
	else{
		message("Use MatTrans.init() to get initialization...\n")

	}


}




MatTrans.EM <- function(Y, initial = NULL, la = NULL, nu = NULL, model = NULL, trans = "None", la.type = 0, row.skew = TRUE, col.skew = TRUE, tol = 1e-05, short.iter = NULL, long.iter = 1000, all.models = TRUE, size.control = 0, silent = TRUE){

	if(is.null(short.iter)){

		ret <- MatTrans.EM.orig(Y, initial = initial, la = la, nu = nu, model = model, trans = trans, la.type = la.type, row.skew = row.skew, col.skew = col.skew, tol = tol, max.iter = long.iter, size.control = size.control, silent = silent, check = TRUE)	
		class(ret) <- "MatTransMix"

	}
	else{
		message("Short EM has been started", "\n")

		shortEM <- MatTrans.EM.orig(Y, initial = initial, la = la, nu = nu, model = model, trans = trans, la.type = la.type, row.skew = row.skew, col.skew = col.skew, tol = tol, max.iter = short.iter, size.control = size.control, silent = silent, check = FALSE)


		message("Long EM has been started", "\n")

		if(!all.models){

			message("For the best model:", shortEM$best.model, "\n")
			
			init <- list()

			W.result <- NULL
			W.result$y <- initial[[1]]$y				
			W.result$gamma1 <- shortEM$best.result[[1]]$gamma
			W.result$invS1 <- apply(shortEM$best.result[[1]]$Sigma, 3, solve)
			W.result$tau <-  shortEM$best.result[[1]]$tau
			W.result$Mu1 <-  shortEM$best.result[[1]]$Mu
			W.result$invPsi1 <- apply(shortEM$best.result[[1]]$Psi, 3, solve)
			W.result$detS <- shortEM$best.result[[1]]$detS
			W.result$detPsi <- shortEM$best.result[[1]]$detPsi
			W.result$scale <- shortEM$scale
	
			init[[1]] <- W.result	
		
			if(trans != "None"){

				longEM <- MatTrans.EM.orig(Y, initial = init, la = shortEM$best.result[[1]]$la, nu = shortEM$best.result[[1]]$nu, model = shortEM$best.model, trans = shortEM$trans, la.type = la.type, row.skew = row.skew, col.skew = col.skew, tol = tol, max.iter = long.iter, size.control = size.control, silent = silent, check = TRUE)
			}
			else{

				longEM <- MatTrans.EM.orig(Y, initial = init, la = NULL, nu = NULL, model = shortEM$best.model, trans = shortEM$trans, la.type = la.type, row.skew = row.skew, col.skew = col.skew, tol = tol, max.iter = long.iter, size.control = size.control, silent = silent, check = TRUE)

			}
			ret <- longEM
	
			class(ret) <- "MatTransMix"
		}
		else{

			message("For all models:", shortEM$model, "\n")

			longEM <- list()

			longEM$model <- shortEM$model

			longEM$trans <- shortEM$trans

			longEM$result <- list()


			loglik <- rep(-Inf, length(shortEM$model))
			bic <- rep(Inf, length(shortEM$model)) 


			for(i in 1:length(shortEM$model)){

				init <- list()

				W.result <- NULL
				W.result$y <- initial[[1]]$y				
				W.result$gamma1 <- shortEM$result[[i]]$gamma
				W.result$invS1 <- apply(shortEM$result[[i]]$Sigma, 3, solve)
				W.result$tau <-  shortEM$result[[i]]$tau
				W.result$Mu1 <-  shortEM$result[[i]]$Mu
				W.result$invPsi1 <- apply(shortEM$result[[i]]$Psi, 3, solve)
				W.result$detS <- shortEM$result[[i]]$detS
				W.result$detPsi <- shortEM$result[[i]]$detPsi
				W.result$scale <- shortEM$scale				
	
				#stop(W.result$gamma1, "\n")
				init[[1]] <- W.result	
				
				if(trans != "None"){

					try0 <- try(longEM$result[[i]] <- MatTrans.EM.orig(Y, initial = init, la = shortEM$result[[i]]$la, nu = shortEM$result[[i]]$nu, model = shortEM$model[i], trans = shortEM$trans, la.type = la.type, row.skew = row.skew, col.skew = col.skew, tol = tol, max.iter = long.iter, size.control = size.control, silent = silent, check = TRUE)$result[[1]])
					

				}
				else{

					try0 <- try(longEM$result[[i]] <- MatTrans.EM.orig(Y, initial = init, la = NULL, nu = NULL, model = shortEM$model[i], trans = shortEM$trans, la.type = la.type, row.skew = row.skew, col.skew = col.skew, tol = tol, max.iter = long.iter, size.control = size.control, silent = silent, check = TRUE)$result[[1]])


				}
				try0 <- try(loglik[i] <- longEM$result[[i]]$ll)
				try0 <- try(bic[i] <- longEM$result[[i]]$bic)


			}



			find.min <- which.min(bic)
			best.result <- longEM$result[find.min]
			best.model <- longEM$model[find.min]
			best.loglik <- loglik[find.min]
			best.bic <- bic[find.min]


			ret <- list(scale = shortEM$scale, result = longEM$result, model = longEM$model, loglik = loglik, bic = bic, best.result = best.result, best.model = best.model, best.loglik = best.loglik, best.bic = best.bic, trans = trans)

			class(ret) <- "MatTransMix"


		}
	}

	return(ret)


}


Manly.transX <- function(X, LA){

	y <- X


	y <- X * LA
	y <- exp(y) - 1
	y <- y / LA

	return(y)

}




manly.dens <- function(X, Mu, detS, detPsi, invS, invPsi, LA){
	p <- dim(X)[1]
	T <- dim(X)[2]
	y <- Manly.transX(X, LA)

	maha <- invS %*% (y - Mu) %*% invPsi %*% t(y - Mu)

	trace <- sum(diag(maha))		

	res <- 1.0 / (2*pi)^(p*T/2) / detS^(T/2) / detPsi^(p/2) * exp(-1.0 / 2.0 * trace) * exp(sum(LA * X));

	return(res)
}




cdf.adjust <- function(K, Mu, Sigma, Psi, la, nu){
  p <- ncol(la)
  q <- ncol(nu)
  vec.Mu <- matrix(NA, nrow = K, ncol = (p*q))
  covar <- array(NA, dim = c((p*q), (p*q), K))
  skew <- matrix(NA, nrow = K, ncol = (p*q))
  cdfAdjust <- rep(NA, K)
  for (k in 1:K){
    vec.Mu[k, ] <- as.vector(Mu[ , , k])
    covar[ , , k] <- Psi[ , , k]%x%Sigma[ , , k]
    ctr <- 0
    for (j in 1:q){
      for (i in 1:p){
        ctr <- ctr + 1
        skew[k,ctr] <- la[k,i] + nu[k,j]
      }
    }
    cdf <- pmvnorm(upper = (-1/skew[k,]), mean = vec.Mu[k,], sigma = covar[ , , k])
    cdfAdjust[k] <- ifelse(cdf > 0.5, cdf, (1-cdf))
  }
  return(cdfAdjust)
}

