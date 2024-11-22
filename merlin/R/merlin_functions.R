
# function for trimming white space
merlin_trim <- function(x)
{
   gsub("^\\s+|\\s+$","",x)
}

# GHNM - Function to calculate gaussian-hermite nodes matrix
ghq_expand_matrix <- function(g,w,nr){

    if (nr == 1) {
        gn <- matrix(sqrt(2)*g,nrow=1)
        wn <- matrix(w/sqrt(pi),nrow=1)
    }
    else {
        nn <- length(g)
        gn <- wn <- matrix(NA,nrow=nr,ncol=(nn^nr))
        for (i in 1:nr) {
            gn[i,] <- rep(sqrt(2)*g,each=(nn^(i-1)))
            wn[i,] <- rep((w)/sqrt(pi),each=(nn^(i-1)))
        }
    }
    retlist <- list("nodes"=gn,"wts"=wn)

}






# extract splines for rp model
# merlin_xz_rp_rcs <- function(gml,i,j) {
#   ret <- gml$splinet[[gml$modelind]][[i]][gml$datause[[gml$modtouse]],] %*% matrix(gml$par[gml$xbindex[[gml$modelind]][[i]]],ncol=1)
#   return(ret)
# }

# extract the time function elements
# merlin_xz_t <- function(gml,i,j,t=NULL) {
#   if (length(t) == 0) { # same as a regular variable
#     if (gml$coeff[[gml$modelind]][i] == 1 & j == 1) ret <- gml$timevarxb[[gml$modelind]][[i]][[j]][gml$datause[[gml$modtouse]]] * gml$par[gml$xbindex[[gml$modelind]][[i]]]
#     else ret <- gml$timevarxb[[gml$modelind]][[i]][[j]][gml$datause[[gml$modtouse]]]
#   } else { # a time function, need to use the nodes values, not the time variable
#     if (gml$coeff[[gml$modelind]][i] == 1 & j == 1) ret <- t * gml$par[gml$xbindex[[gml$modelind]][[i]]]
#     else ret <- t
#   }
#   return(ret)
# }

# calculate fractional polynomials
# merlin_xz_fp <- function(gml,i,j,t=NULL) {
#   if (length(t) == 0) { # same as a regular variable
#     if (gml$coeff[[gml$modelind]][i] == 1 & j == 1) ret <- gml$timevarxb[[gml$modelind]][[i]][[j]][gml$datause[[gml$modtouse]],] %*% gml$par[gml$xbindex[[gml$modelind]][[i]]]
#     else ret <- gml$timevarxb[[gml$modelind]][[i]][[j]][gml$datause[[gml$modtouse]],]
#   } else { # a time function, need to use the nodes values, not the time variable
#     if (gml$coeff[[gml$modelind]][i] == 1 & j == 1) ret <- t * gml$par[gml$xbindex[[gml$modelind]][[i]]]
#     else ret <- t
#   }
#   return(ret)
# }



# merlin_xz_EV <- function(gml,i,j,deriv=0,t=NULL) { # NEED TO DEAL WITH THE MISSING DATA ISSUE
#   k <- gml$modelind
#   # change the model index to the model I want to read in
#   gml$modelind <- k2 <- gml$evmodelind[[k]][[i]][[j]]
#   # get the linear predictor
#   if (gml$coeff[[k]][i] == 0) {
#     if (length(t) == 0) res <- merlin_util_xzb(gml=gml)
#     if (length(t) >  0) res <- merlin_util_xzb(gml=gml,t=t)
#   }
#   if (gml$coeff[[k]][i] == 1) {
#     if (length(t) == 0) res <- merlin_util_xzb(gml=gml) * gml$par[gml$xbindex[[k]][[i]]]
#     if (length(t) >  0) res <- merlin_util_xzb(gml=gml,t=t) * gml$par[gml$xbindex[[k]][[i]]]
#   }
#   # change the index back to the current model
#   gml$modelind <- k
#   return(res)
# }

# calculate derivative of time dependent function
# merlin_xz_dt <- function(gml,i,j,t=NULL) {
#
#   if (j == 1) p <- gml$par[gml$xbindex[[gml$modelind]][[i]]]
#   if (j > 1)  p <- 1
#
#   # what is the function of time (it will probably be in term)
#   funct <- 0 # assume there's no function (basically *1) unless you find otherwise
#   if (grepl("log(",gml$vari[[gml$modelind]][[i]][j],fixed=TRUE)) funct <- 1
#
#   if (grepl("fp(",gml$vari[[gml$modelind]][[i]][j],fixed=TRUE)) stop("Stop! This does not work yet!")
#
#   if (length(t) == 0) {
#     tvar <- gml$timevarxb[[gml$modelind]][[i]][[j]][gml$datause[[gml$modtouse]]]
#     h <- 1e-8 * (abs(tvar) + 1e-8)/2
#     if (funct == 0) lh <- p*(tvar + h)
#     if (funct == 0) rh <- p*(tvar - h)
#     if (funct == 1) lh <- p*log(tvar + h)
#     if (funct == 1) rh <- p*log(tvar - h)
#     return((lh-rh)/(2*h))
#   }
#   if (length(t) > 0) {
#     h <- 1e-8 * (abs(t) + 1e-8)/2
#     if (funct == 0) lh <- p*(t + h)
#     if (funct == 0) rh <- p*(t - h)
#     if (funct == 1) lh <- p*log(t + h)
#     if (funct == 1) rh <- p*log(t - h)
#     return((lh-rh)/(2*h))
#   }
# }

stop_quietly <- function() {
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
}

#displaying labels in results table
merlin_strdi <- function(x,width=2)
{
    if (width<2) stop("width>1")
    l = nchar(x)
    if (l<=width) return(x)
    else {
        s1l = width - 2
        stub1 = substr(x,1,s1l)
        stub2 = substr(x,l,l)
        return(paste0(stub1,"~",stub2))
    }
}

merlin_identity <- function(x)
{
  x
}

merlin_exp <- function(x)
{
  exp(x)
}

merlin_invlogit <- function(x)
{
  1/(1 + exp(-x))
}






