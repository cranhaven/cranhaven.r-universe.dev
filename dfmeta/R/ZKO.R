#' @import stats4
#' @export
ZKO <-
function (dataTox, doses, target = 0.3, dr, nbSimu){

    lvcrm <- function(a = 0.5){
        -sum(y1p * log(x1p^a) + (1 - y1p) * log(1 - x1p^a))
    }
  
    lprime <- function(a){
        res <- rep(0, length(dr))
        for (i in 1:length(dr)){
            if (ni[i] > 0){
                res[i] <- ((ti[i]/ni[i]) * ((log(dr[i]) * dr[i]^a)/dr[i]^a)) + 
                ((1 - (ti[i]/ni[i])) * ((-log(dr[i]) * dr[i]^a)/(1 - dr[i]^a)))
            }
        }
        return(res)
    }
  
    weight <- function(a, ti, ni, dr, w){
        sum(w * lprime(a))
    }
  
    weight1 <- function(a, w, ri, dr){
        sum(w * ((ri * (log(dr) * dr^a)/dr^a) + ((1 - ri) * (-(log(dr) * dr^a)/(1 - dr^a)))))
    }

    pcs_ret <- matrix(NA, nrow = 1, ncol = length(dr))
    dose_ret <- matrix(NA, nrow = 1, ncol = length(dr))

    fnbTox <- function(dose, dataTox){
        sum(dataTox[dataTox$doses == dose, "nbTox"])
    }

    fnbPatients <- function(dataTox, dose){
        sum(dataTox[dataTox$doses == dose, "nbPatients"])
    }

    dr <- dr
    n <- sum(dataTox$nbPatients)
    ti <- round(sapply(doses, fnbTox, dataTox = dataTox), 0)    # vriskei to sum twn nbTox gia kathe dose level.
    ni <- round(sapply(doses, fnbPatients, dataTox = dataTox),  # vriskei to sum twn nbPatients gia kathe dose level. 
                0)

    realite <- ti/ni
    obj <- target
    nd <- length(dr)
    y <- 0
    repet <- nbSimu
    graine <- 15
    cohort <- 3
    cdeb <- rep(3, trunc(n/3) * 3)
    r1 <- matrix(NA, nrow = n, ncol = length(dr))
    simuld <- bonval <- nbna <- matrix(0, repet, 2)
    indice <- c(1:(n + 1))
    maxv1 <- rep(NA, n)
    ref <- c(1:length(dr))
    rsim <- matrix(NA, nrow = repet, ncol = length(dr))
    dsujsim <- matrix(NA, nrow = repet, ncol = (n + 1))
    dsim <- rep(NA, repet)
    verif <- 0
    estimated_alpha <- c()
    estimated_proba <- c()

    set.seed(graine)
    for (k in 1:repet){
        print(k)
        tirage <- t(matrix(rbinom(n * nd, 1, realite), nd, n))
        y <- d <- x1p <- y1p <- maxv1 <- rep(0, n + 1)
        dsuiv <- delta <- j <- 0
        r1 <- matrix(NA, nrow = n + 1, ncol = length(dr))
        d_deb <- c(rep(dr, each = cohort), rep(dr[length(dr)], length(dr)))
        compteur <- 0
        while (sum(y) == 0 || sum(y) == compteur){
            if (compteur < length(d_deb) && compteur < n){
                d[(compteur + 1)] <- d_deb[(compteur + 1)]
                y[(compteur + 1)] <- tirage[(compteur + 1), which(d_deb[(compteur + 1)] == dr)]
                dsujsim[k, (compteur + 1)] <- which(d_deb[(compteur + 1)] == dr)
                compteur <- compteur + 1
                if (compteur == 1 && sum(y) == 3){
                    rsim[k, ] <- NA
                    dsim[k] <- dsujsim[k, (compteur)]
                    verif <- verif + 1
                    j <- n + 1
                    break
                }
            }
            else{
            rsim[k, ] <- NA
            dsim[k] <- dsujsim[k, (compteur)]
            verif <- verif + 1
            break
            }
        }

        j <- c(indice[d == 0])
        if (j > 0 && j <= (n) && sum(y) > 0){
        for (i in j){
            x1p <- c(d[1:(i - 1)])
            y1p <- c(y[1:(i - 1)])
            maxv1[(i - 1)] <- attr(mle(lvcrm), "details")$par
            r1[(i - 1), ] <- dr^maxv1[(i - 1)]
            delta <- abs(obj - r1[(i - 1), ])
            dsuiv <- ref[delta == min(delta)]
            d[i] <- dr[dsuiv]
            dsujsim[k, i] <- dsuiv
            if (i <= n){
                y[i] <- tirage[i, dsuiv]
            }
        }
          rsim[k, ] <- r1[n, ]
          dsim[k] <- dsuiv
        }
        else{
          rsim[k, ] <- NA
          dsim[k] <- dsujsim[k, (compteur)]
          verif <- verif + 1
        }
    }
    
    dsim1 <- factor(dsim, levels = seq(1, length(dr)))
    pcs_ret[1, ] <- table(dsim1)/repet
    dsujsim1 <- factor(dsujsim[, 1:n], levels = seq(1, length(dr)))
    dose_ret[1, ] <- round(table(dsujsim1)/(repet * n), 2)
    weights <- pcs_ret
    res1 <- uniroot(weight1, interval = c(0.1, 64), tol = 0.01, w = dose_ret[1, ], ri = realite, dr = dr)$root
    estimated_alpha <- res1
    estimated_proba <- dr^res1

    # return(list(weights = weights, estimAlpha = estimated_alpha, estimProba = estimated_proba))

    new("ZKO", dataTox = dataTox, target = obj, doses = doses, ti = ti, ni = ni, realite = realite, relfreq=dose_ret,
        estimAlpha=estimated_alpha, estimProb=estimated_proba)
}
