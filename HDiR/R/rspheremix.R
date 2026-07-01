rspheremix<-function (n, model = NULL){
 if (!is.numeric(n))
        stop("argument 'n' must be numeric")
 if (is.null(model))
        stop("no model specified")
     if (!is.null(model)) {
        if (!is.numeric(model))
            stop("argument 'model' must be numeric")
        if (!any(model == 1:9))
            stop("value specified for argument 'model' is not valid")
        if (model == 1) {
		k <-10
		prob <- 1
		mu <- matrix(c(0,0,1),nrow=1)
		mu <- mu / sqrt(rowSums(mu^2))
        }else if(model == 2){
		k <- c(1,1)
		prob <- c(1/2,1/2)
		mu <- rbind(c(0,0,1),c(0,0,-1))
		mu <- mu / sqrt(rowSums(mu^2))
	  }else if(model == 3){
		k <- c(10,1)
		prob <- c(1/2,1/2)
		mu <- rbind(c(0,0,1),c(0,0,-1))
		mu <- mu / sqrt(rowSums(mu^2))
        }else if(model == 4){
		k <- c(10,10)
		prob <- c(1/2,1/2)
		mu <- rbind(c(0,0,1),c(0,0.5,.5))
		mu <- mu / sqrt(rowSums(mu^2) )
        }else if(model == 5){
		k <- c(10,10)
		prob <- c(.4,.6)
		mu <- rbind(c(0,0,1),c(0,0.5,.5))
		mu <- mu / sqrt(rowSums(mu^2))
        }else if(model == 6){
		k <- c(10,5)
		prob <- c(.2,.8)
		mu <- rbind(c(0,0,1),c(0,0.5,.5))
		mu <- mu / sqrt(rowSums(mu^2))
        }else if(model == 7){
		k <- c(5,5,5)
		prob <- c(1/3,1/3,1/3)
		mu <- rbind(c(0,0,1),c(0,1,0),c(1,0,0))
		mu <- mu / sqrt(rowSums(mu^2) )
        }else if(model == 8){
		k <- c(5,5,5)
		prob <- c(2/3,1/6,1/6)
		mu <- rbind(c(0,0,1),c(0,1,0),c(1,0,0))
		mu <- mu / sqrt(rowSums(mu^2))
        }else if(model == 9){
		k <- c(10,10,10)
		prob <- c(1/3,1/3,1/3)
		mu <- rbind(c(0,0,1),c(0,0.5,0.5),c(0,1,0))
		mu <- mu / sqrt(rowSums(mu^2))
	  }
}
return(rmovMF(n,k*mu,alpha=prob))
}


