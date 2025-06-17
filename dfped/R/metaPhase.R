#' @import stats4
#' @import ggplot2
#' @import rstan
#' @import methods
#' @import stats
#' @import graphics
#' @import grDevices
#' @export
metaPhase <-
function(dataTox, doses, nbSimu){
    
    lvcrm 	<- function(a = 0.5){-sum(y1p*log(x1p^a) + (1 - y1p)*log(1 - x1p^a))}
    lprime 	<- function(a){
        res <- rep(0, length(dr))
        for(i in 1:length(dr)){
            if(ni[i] > 0){
                res[i] <- ((ti[i]/ni[i])*((log(dr[i])*dr[i]^a)/dr[i]^a)) + ((1-(ti[i]/ni[i]))*((-log(dr[i])*dr[i]^a)/(1-dr[i]^a)))
            }
        }
        return(res)
    }
    weight <- function(a, ti, ni, dr, w){sum(w*lprime(a))}
    weight1 <- function(a, w, ri, dr){sum(w*((ri*(log(dr)*dr^a)/dr^a) + ((1 - ri)*(-(log(dr)*dr^a)/(1 - dr^a)))))}
    
    pcs_ret	<- matrix(NA, nrow=1, ncol=4)
    dose_ret <- matrix(NA, nrow=1, ncol=4)
    
    fnbTox <- function(dose, dataTox){sum(dataTox[dataTox$doses == dose, "nbTox"])}
    fnbPatients <- function(dataTox, dose){sum(dataTox[dataTox$doses == dose, "nbPatients"])}
    
    # The initial parameters
    dr <- c(0.05, 0.15, 0.3, 0.45)
    n <- sum(dataTox$nbPatients)										# The number of patients
    ti <- round(sapply(doses, fnbTox, dataTox = dataTox), 0) 			# A DLT observed
    ni <- round(sapply(doses, fnbPatients, dataTox = dataTox), 0)
    realite <- ti/ni
    obj <- 0.30
    
    # realite <- work.sim[1, ]                 		# vector of the probabilities of differents doses for the simulation  
    nd 	<- length(dr)				                # the number of possible doses
    y <- 0				                    		# a vector of the responses
    repet <- nbSimu				                  	# the number of the simulations
    graine <- 15					                # graine for the simulations 
    cohort <- 3
    cdeb <-  rep(3, trunc(n/3)*3)					# the size of the cohorts for the schema up-and-down. 

    # The variables for the simulations 
    r1		<- 	matrix(NA, nrow=n, ncol=length(dr))
    simuld	<- 	bonval <- nbna <- matrix(0, repet,2)
    indice	<- 	c(1 : (n+1))
    maxv1	<- 	rep(NA, n)
    ref		<- 	c(1:length(dr))
    rsim	<- 	matrix(NA, nrow=repet, ncol= length(dr))
    dsujsim	<- 	matrix(NA, nrow=repet, ncol= (n + 1))
    dsim	<- 	rep(NA, repet)
    verif	<- 	0
    
    
    estimated_alpha <- c()
    estimated_proba <- c()
    
    # The initiation graine 
    set.seed(graine)
    
    for (k in 1:repet){
        print(k)
        # The random of the responses before the start of the trial
        tirage <- t(matrix(rbinom(n*nd, 1, realite), nd, n))
        
        # Reinitialisation
        y <- d <- x1p <- y1p <- maxv1 <- rep(0, n + 1)
        dsuiv <- delta <- j <- 0
        r1	<- matrix(NA, nrow = n + 1, ncol = length(dr))
        
        # The dose first step
        d_deb <- c(rep(dr, each = cohort), rep(dr[4], 4))
        compteur <- 0
        
        # Escalade standard 
        while (sum(y) == 0 || sum(y) == compteur){
            if(compteur < length(d_deb) && compteur < n){
                d[(compteur + 1)] <- d_deb[(compteur + 1)]
                y[(compteur + 1)] <- tirage[(compteur + 1), which(d_deb[(compteur + 1)] == dr)]
                dsujsim[k,(compteur + 1)] <- which(d_deb[(compteur + 1)] == dr)
                compteur <- compteur + 1
                
                if(compteur == 1 && sum(y) == 3){
                    rsim[k,] <- NA
                    dsim[k] <- dsujsim[k, (compteur)]
                    verif <- verif + 1
                    j <- n + 1
                    break
                }
            }else{
                rsim[k,] <- NA
                dsim[k] <- dsujsim[k, (compteur)]
                verif <- verif + 1
                break
            }
        }
        
        # LCRM 
        j <- c(indice[d == 0])
        if(j > 0 && j <= (n) && sum(y) > 0){
            for (i in j){
                x1p <- c(d[1 : (i - 1)])
                y1p <- c(y[1 : (i - 1)])
                maxv1[(i - 1)] <- attr(mle(lvcrm), "details")$par
                r1[(i - 1), ] <- dr^maxv1[(i - 1)]
                delta <- abs(obj - r1[(i - 1),])
                dsuiv <- ref[delta == min(delta)]
                d[i] <- dr[dsuiv]
                dsujsim[k, i] <- dsuiv
                if(i <= n){
                    y[i] <- tirage[i, dsuiv]
                }
            }
            rsim[k,] <- r1[n, ]
            dsim[k]	<- dsuiv
        }else{
            rsim[k,] <- NA
            dsim[k] <- dsujsim[k, (compteur)]
            verif <- verif + 1
        }
    }
    
    # Calcul PCS
    dsim1 <- factor(dsim, levels = seq(1, length(dr)))
    pcs_ret[1, ] <- table(dsim1) / repet
    
    dsujsim1 <- factor(dsujsim[ ,1 : n], levels=seq(1, length(dr)))
    dose_ret[1, ] <- round(table(dsujsim1) / (repet*n), 2)
    res1 <- uniroot(weight1, interval = c(0.1, 64), tol = 0.01, w = dose_ret[1, ], ri = realite, dr = dr)$root
    
    estimated_alpha <- res1
    estimated_proba <- dr^res1
    
    return(list(estimated_alpha, estimated_proba))
}
