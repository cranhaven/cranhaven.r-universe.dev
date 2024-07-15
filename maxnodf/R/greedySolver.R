greedy_solve2 <- function(NodesA, NodesB, Edges){
    mtx <- matrix(0.0, nrow=NodesA, ncol=NodesB)
    mtx[1,] = 1.0
    mtx[,1] = 1.0
    mtx[2,2] = 1.0
    support_data <- init_nodf(mtx)
    nodf <- nodf_cpp(mtx)
    Edges_Left <- Edges - NodesA - NodesB
    if(Edges_Left >= 1){
        tp <- utils::txtProgressBar(min = 0, max = Edges_Left, style = 3)
        # Unpack the support data:
        MT <- support_data[[1]]
        Fill <- support_data[[2]]
        DM <- support_data[[3]]
        ND <- support_data[[4]]
        S <- support_data[[5]]
        # Unpack even futher to get a set a variables that we can work with:
        mt_0 <- as.vector(MT[[1]])
        mt_t <- as.vector(MT[[2]])
        # print("B1")
        F0 <- Fill[[1]]
        Ft <- Fill[[2]]
        # print("B2")
        DM0 <- DM[[1]]
        DMt <- DM[[2]]
        # print("B3")
        ND0 <- ND[[1]]*1
        NDt <- ND[[2]]*1

        for(i in 1:Edges_Left){
            zPosList <- websearch_cpp(mtx)
            # zPosList <- websearch_NODF_fast(mtx)
            opt_nodf <- -100.0
            opt_pos <- c(-1,-1)
            for(idx in 1:nrow(zPosList)){
                zPos <- zPosList[idx, ]
                xpos <- zPos[[1]]
                ypos <- zPos[[2]]
                new_nodf <- nodf_one_link_added_cpp(mtx,xpos,ypos,mt_0,mt_t,F0,Ft,DM0,DMt,ND0,NDt,S);
                if(new_nodf > opt_nodf){
                    opt_nodf <- new_nodf
                    opt_pos <- zPos
                }
                else if(new_nodf == opt_nodf){
                    score_old <- (opt_pos[1] / NodesA) + (opt_pos[2] / NodesB)
                    score_new <- (zPos[1] / NodesA) + (zPos[2] / NodesB)
                    if(score_new < score_old){
                        opt_nodf <- new_nodf
                        opt_pos <- zPos
                    }
                }
                # Revert the change:
                nodf_2 <- nodf_one_link_removed_cpp(mtx, zPos[1], zPos[2], mt_0, mt_t, F0, Ft, DM0, DMt, ND0, NDt, S);
            }
            # actually perform the update
            xpos <- opt_pos[[1]]
            ypos <- opt_pos[[2]]
            new_nodf <- nodf_one_link_added_cpp(mtx,xpos,ypos,mt_0,mt_t,F0,Ft,DM0,DMt,ND0,NDt,S);
            utils::setTxtProgressBar(tp, i)
        }
    }
    return(mtx)
}

