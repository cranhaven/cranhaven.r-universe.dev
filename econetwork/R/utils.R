# This file is part of econetwork

# econetwork is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# econetwork is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with econetwork.  If not, see <http://www.gnu.org/licenses/>

sbmParams <- function(g, groups=NULL){
    ## groups[i] for V(g)[i]    
    if(is.null(groups)){# each node forms its own group if groups is NULL
        groups=V(g)$name
        names(groups)=V(g)$name
    }
    
    alpha.vec <- V(g)$ab #abundance of the groups of the groups
    names(alpha.vec) <- V(g)$name
    alpha.vec = t(as.matrix(sapply(split(alpha.vec,groups),sum)))
    
    if(is.null(E(g)$weight)){#get the (weighted adjacency matrix)
        adj.mat <- get.adjacency(g)
    }  else { # the weight should represent a probability of link and must be so btwn 0 and 1
        adj.mat <- get.adjacency(g,attr = "weight")/max(E(g)$weight)
    }
    adj.mat.w <- adj.mat*V(g)$ab%*%t(V(g)$ab) #weight by species abundances (i.e. matrix of L_{ql} in the paper)
    l.mat <- matrix(0, length(alpha.vec),length(alpha.vec))
    colnames(l.mat) <- rownames(l.mat) <- colnames(alpha.vec)
    Q <- length(colnames(alpha.vec))
    for (q in 1:Q){
        for (l in 1:Q){
            l.mat[q,l] <- sum(adj.mat.w[which(groups==colnames(alpha.vec)[q]),
                                        which(groups==colnames(alpha.vec)[l])])
        }
    }
    pi.mat<-l.mat/(t(alpha.vec)%*%alpha.vec) #link probability matrix
    
    return(list(alpha=alpha.vec,
                l=l.mat,
                pi=pi.mat,
                C=sum(adj.mat.w)))
}

metawebParams <- function(gList, groups, priorMetaweb=FALSE){
    ## get the L,Pi arrays and P mat for a list of graph
    ## groups must be an array with names associated to the nodes of the metaweb 
    groups.id <- unique(groups)
    if(length(names(gList))){
        g.id <- names(gList)
    } else{
        g.id <- 1:length(gList)
    }
    Q <- length(groups.id) #number of different groups in the metaweb
    P.mat <- matrix(0, nrow=Q, ncol=length(gList))
    rownames(P.mat) <- groups.id
    colnames(P.mat) <- g.id
    
    L.array <- array(0, dim=c(Q,Q,length(gList))) #stacked adjacency matrix at a group level
    dimnames(L.array)[[1]] <- if(Q>1)  groups.id else list(groups.id)
    dimnames(L.array)[[2]] <- if(Q>1)  groups.id else list(groups.id)
    dimnames(L.array)[[3]] <- g.id
    
    SBMparams <- lapply(gList, function(g){
        sbmParams(g, groups[V(g)$name])
    })
    alpha_list <- lapply(SBMparams, function(p) p$alpha)
    L_list <- lapply(SBMparams, function(p) p$l)
    Pi_list <- lapply(SBMparams, function(p) p$pi)
    
    if (!priorMetaweb){
        Pi.array.NA <- array(NA, dim = c(Q,Q,length(gList)))
        dimnames(Pi.array.NA)[[1]] <- groups.id
        dimnames(Pi.array.NA)[[2]] <- groups.id 
        dimnames(Pi.array.NA)[[3]] <- g.id
        
        for(i in 1:length(gList)){
            P.mat[colnames(alpha_list[[i]]),i] <- alpha_list[[i]][1,]
            L.array[rownames(L_list[[i]]),colnames(L_list[[i]]),i] <- as.matrix(L_list[[i]][rownames(L_list[[i]]),colnames(L_list[[i]])])
            Pi.array.NA[rownames(Pi_list[[i]]), colnames(Pi_list[[i]]), i] <- as.matrix(Pi_list[[i]][rownames(Pi_list[[i]]),colnames(Pi_list[[i]])])
        }
        return(list(P.mat=P.mat, L.array=L.array, Pi.array=Pi.array.NA))
    }
    else {
        g.metaweb <- getMetaweb(gList)
        Pi.metaweb <- sbmParams(g.metaweb,groups)$pi
        Pi.array.metaweb <- array(rep(Pi.metaweb, length(gList)), dim = c(Q,Q,length(gList)))
        dimnames(Pi.array.metaweb)[[1]] <- groups.id
        dimnames(Pi.array.metaweb)[[2]] <- groups.id 
        dimnames(Pi.array.metaweb)[[3]] <- g.id
        
        for(i in 1:length(gList)){
            P.mat[names(alpha_list[[i]]),i] <- alpha_list[[i]]
            L.array[rownames(L_list[[i]]),colnames(L_list[[i]]),i] <- L_list[[i]]
            Pi.array.metaweb[rownames(Pi_list[[i]]), colnames(Pi_list[[i]]), i] <- Pi_list[[i]]
        }
        return(list(P.mat=P.mat, L.array=L.array, Pi.array=Pi.array.metaweb))
    }
}
