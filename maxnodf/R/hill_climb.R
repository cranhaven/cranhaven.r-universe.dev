hill_climb_step2 <- function(mtx, R){
    NodesA <- nrow(mtx)
    NodesB <- ncol(mtx)
    oPosList <- get_valid_ones_cpp(mtx)
    support_data <- init_nodf(mtx)

    # Unpack the support data:
    MT <- support_data[[1]]
    Fill <- support_data[[2]]
    DM <- support_data[[3]]
    ND <- support_data[[4]]
    S <- support_data[[5]]
    # Unpack even futher to get a set a variables that we can work with:
    mt_0 <- as.vector(MT[[1]])
    mt_t <- as.vector(MT[[2]])
    F0 <- Fill[[1]][,]
    Ft <- Fill[[2]][,]
    DM0 <- DM[[1]][,]
    DMt <- DM[[2]][,]
    ND0 <- ND[[1]]*1
    NDt <- ND[[2]]*1

    opt_mtx <- mtx[,]
    opt_nodf <- nodf_cpp(opt_mtx)

    tp <- utils::txtProgressBar(min = 1, max = nrow(oPosList), style = 3)
    for(idx in 1:nrow(oPosList)){
        utils::setTxtProgressBar(tp, idx)
        opos <- oPosList[idx,]
        for(xshift in -R:R){
            for(yshift in -R:R){
                newx <- opos[1] + xshift
                newy <- opos[2] + yshift
                if(newx>= 1 & newx <= NodesA & newy >= 1 & newy <= NodesB){
                    if(mtx[newx, newy] == 0){
                        zpos <- c(newx, newy)
                        nodf <- nodf_neighbor2(mtx,opos,zpos,mt_0,mt_t,F0,Ft,DM0,DMt,ND0,NDt,S)
                        if(nodf > opt_nodf){
                            opt_mtx <- mtx[,]
                            opt_nodf <- nodf
                        }
                        # Revert the change:
                        nodf_neighbor2(mtx,zpos,opos,mt_0,mt_t,F0,Ft,DM0,DMt,ND0,NDt,S)
                    }
                }
            }
        }
    }
    return(opt_mtx)
}

full_hill_climb <- function(mtx, R=1){
    old_nodf <- -100.0
    count <- 0
    while(old_nodf < nodf_cpp(mtx)){
        count <- count + 1
        old_nodf <- nodf_cpp(mtx)
        mtx <- hill_climb_step2(mtx, R)
    }
    return(mtx)
}

