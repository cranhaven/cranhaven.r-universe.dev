
#parse coefficients
merlin_xb <- function(gml){

    if (gml$Nlevels) {

        # update vcv * nodes matrices for each level
        sigma <- list()
        nmat  <- list()

        ind = 1
        for (l in 1:gml$Nlevels) {
            Nres  = gml$Nres[l]
            sdcor = matrix(0,ncol=Nres,nrow=Nres)
            vcv   = sdcor

            if (gml$covariance[l,1] | gml$covariance[l,2]) {
                for (r in 1:Nres) { # diagonals
                    sdcor[r,r] = exp(gml$par[gml$vcvbindex[1,ind]:gml$vcvbindex[2,ind]])
                    vcv[r,r]   = sdcor[r,r]^2
                    ind = ind + 1
                }
                if (gml$covariance[l,2]) {
                    for (r in 1:Nres) { # now do off-diagonals
                      ind2 <- 1
                      while (ind2 < r) {
                        sdcor[ind2,r] <- sdcor[r,ind2] <- tanh(gml$par[gml$vcvbindex[1,ind]:gml$vcvbindex[2,ind]])
                        vcv[ind2,r]   <- vcv[r,ind2] <- sdcor[ind2,ind2] * sdcor[r,r] * sdcor[ind2,r]
                        ind  = ind + 1
                        ind2 = ind2 + 1
                      }
                    }
                }
            }
            else if (gml$covariance[l,3]) {
                sd1 = exp(gml$par[gml$vcvbindex[1,ind]:gml$vcvbindex[2,ind]])
                for (r in 1:Nres) {
                    vcv[r,r] = sd1 ^ 2
                }
                ind = ind + 1
                c1 = tanh(gml$par[gml$vcvbindex[1,ind]:gml$vcvbindex[2,ind]])^2
                for (r in 1:Nres) { # now do off-diagonals
                    ind2 <- 1
                    while (ind2 < r) {
                      sdcor[ind2,r] <- sdcor[r,ind2] <- c1
                      vcv[ind2,r]   <- vcv[r,ind2] <- vcv[r,r] * c1
                      ind2 = ind2 + 1
                    }
                }
                ind = ind + 1
            }
            else {
                sd1 = exp(gml$par[gml$vcvbindex[1,ind]:gml$vcvbindex[2,ind]])
                for (r in 1:Nres) {
                   vcv[r,r] = sd1 ^ 2
                }
            }
            sigma[[l]] <- vcv

            #update integration points
            if      (gml$intmethod[l]=="mc") nmat[[l]] <- t(MASS::mvrnorm(gml$ip[l],rep(0,gml$Nres[l]),vcv))
            else                             nmat[[l]] <- chol(vcv) %*% gml$g[[l]]
        }

        gml$sigma <- sigma
        gml$nmat  <- nmat
    }

    return(gml)
}
