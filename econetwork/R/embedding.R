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

embedding <- function(gList, method="metric2vec", groups=NULL){
    if(sum(unlist(lapply(lapply(gList,FUN = function(x) V(x)$name),is.null)))>0){#check if nodes have names
        stop('nodes must have names (use V(g)$name)')
    }
    if(!is.null(groups)){
        if(is.null(names(groups))){#check whether groups vector has nodes
            stop("groups must have names (names(groups) is NULL)")
        }
        if(prod(names(groups) %in% unique(unlist(lapply(gList,FUN = function(g) V(g)$name))))*prod(unique(unlist(lapply(gList,FUN = function(g) V(g)$name))) %in% names(groups))!=1){ #check if the names of groups match to the names of the metaweb
            stop("the names of groups vector do not match to the names of the metaweb")
        }
    }
    METHODS <- c("metric2vec","motif2vec","group2vec","shortestpath2vec")
    i.meth <- pmatch(method, METHODS)
    if (is.na(i.meth)) 
        stop("invalid embedding method", paste("", method))
    if (i.meth==-1) 
        stop("ambiguous embedding method", paste("", method))
    if (i.meth==1)
        return(metricsEmbedding(gList))
    if (i.meth==2)
        return(motifEmbedding(gList))
    if (i.meth==3)
        return(groupEmbedding(gList,groups))
    if (i.meth==4)
        return(shortestPathEmbedding(gList,groups))
    return(NULL)
}

shortestPathEmbedding <- function(gList,groups=NULL,directedPath=TRUE){
  G.floyd <- as.list(rep(NA, length(gList)))
  pathsList <- as.list(rep(NA, length(gList)))
  for (i in 1:length(gList)){    
    # Floyd transform
    if(directedPath){
      D <- distances(gList[[i]],mode="out")
    }else{
      D <- distances(gList[[i]],mode="all")
    }
    # mark static paths
    diag(D) <- NA    
    df <- expand.grid(from=1:length(V(gList[[i]])),to=1:length(V(gList[[i]])),weight=NA)
    df$weight <- as.vector(D)
    # remove static paths
    df <- df[!is.na(df$weight),]    
    if(!is.null(groups)){
      # all path in the form From-Length-To
      nodeGroup <- groups[names(V(gList[[i]]))]
      tmp<-data.frame(from=1:length(nodeGroup),fromGroup= nodeGroup)   
      df <- merge(df,tmp,by="from")
      colnames(tmp) = c('to','toGroup')
      df <- merge(df,tmp,by="to")
      paths <- paste(df$fromGroup,'-',df$weight,'-',df$toGroup,sep="")
    }else{
      paths <- df$weight
    }    
    pathsList[[i]] <- table(paths)
  }  
  pathsVocab <- unique(names(unlist(pathsList)))  
  embd <- matrix(0,length(pathsList),length(pathsVocab))
  colnames(embd) <- pathsVocab
  for(i in 1:length(gList)){
    n <- length(V(gList[[i]]))
    # Size-Normalize each counts vector by the number of distinct nodes pairs in its graph
    embd[i,names(pathsList[[i]])] <- pathsList[[i]]/(n*(n-1))
  }
  row.names(embd) <- names(gList)
  return(embd)
}

motifEmbedding <- function(gList){
    embd <- as.data.frame(matrix(NA,length(gList),13))
    for(i in 1:length(gList)){
        adjmat <- as.matrix(as_adjacency_matrix(gList[[i]]))
        counts <- motifs(graph_from_adjacency_matrix(adjmat),size=3)
        counts <- counts[!is.na(counts)]
        ##counts <- counts[c(9,5,10,6,2,1,3,11,12,4,7,8,13)]
        embd[i,] <- counts/sum(counts)
    }
    colnames(embd) <- paste("M",1:13,sep="")
        ## paste("M",c(9,5,10,6,2,1,3,11,12,4,7,8,13),sep="")
    row.names(embd) <- names(gList)
    return(embd)    
}

groupEmbedding <- function(gList,groups=NULL){
    embd <- as.data.frame(matrix(NA,length(gList),length(unique(groups))))
    colnames(embd) <- unique(groups)
    for(i in 1:length(gList)){
        embd[i,] <- table(c(unique(groups),groups[names(V(gList[[i]]))]))-1
        embd[i,] <- embd[i,] / sum(embd[i,])
    }
    row.names(embd) <- names(gList)
    return(embd)       
}

metricsEmbedding <- function(gList){
    omniLevels <- lapply(gList, omnivoryLevels)
    npreys <- lapply(gList, function(g){
        rsum <- rowSums(as.matrix(as_adj(g)))
        rsum[rsum>0] # excluding basal spacies
    })
    npreds <- lapply(gList, function(g){
        csum <- colSums(as.matrix(as_adj(g)))
        csum[csum>0] # excluding top species        
    })
    tropLev <- lapply(gList, trophicLevels)
    embd <- cbind(
        connectance=sapply(gList, function(g) ecount(g)/(vcount(g)^2)),
        modularity=sapply(gList, function(g) modularity(as.undirected(g), cluster_infomap(as.undirected(g))$membership)),
        rangeTL=sapply(tropLev, function(lev) max(lev)-min(lev)),
        meanTL=sapply(tropLev, mean),
        meanSWTL=sapply(gList, function(g){mean(shortWeightedTrophicLevels(g))}),
        meanOmni=sapply(omniLevels, mean),
        propOmni=sapply(omniLevels, function(lev) mean(lev>0)),
        propCanib=sapply(gList, function(g) mean(diag(as.matrix(as_adj(g))))),
        meanNbPrey=sapply(npreys,mean),
        sdNbPrey=sapply(npreys,sd),
        skewNbPrey=sapply(npreys,function(x) (sum((x-mean(x))^3)/length(x))/(sum((x-mean(x))^2)/length(x))^(3/2)),
        meanNbPred=sapply(npreds,mean),
        sdNbPred=sapply(npreds,sd),
        skewNbPred=sapply(npreys,function(x) (sum((x-mean(x))^3)/length(x))/(sum((x-mean(x))^2)/length(x))^(3/2)),
        propBasal=sapply(gList, basalProportion),
        propTop=sapply(gList, topProportion),
        propInter=sapply(gList, intermediaryProportion),
        sdVulnerability=sapply(gList, function(g) sd(degree(g, mode = c("in")))),
        sdGeneralism=sapply(gList, function(g) sd(degree(g, mode = c("out")))),
        transitivity=sapply(gList, transitivity),
        diameter=sapply(gList, diameter, directed = TRUE),
        meanSP=sapply(gList, mean_distance),
        assortativity=sapply(gList, assortativity.degree, directed=TRUE)
    )
    return(embd)
}

basalProportion <- function(g){
  adjmat <- as.matrix(as_adj(g))
  basals <- which(rowSums(adjmat)==0)
  return(length(basals)/nrow(adjmat))
}
intermediaryProportion <- function(g){
  adjmat <- as.matrix(as_adj(g))
  inter <- which(rowSums(adjmat)>0 & colSums(adjmat)>0)
  return(length(inter)/nrow(adjmat))
}
topProportion <- function(g){
  adjmat <- as.matrix(as_adj(g))
  top <- which(colSums(adjmat)==0)
  return(length(top)/nrow(adjmat))
}

shortWeightedTrophicLevels <- function(g){
                                        # The short weighted trophic level (SWTL) 
                                        # is the average (directed) distance to the basal species (who have no prey)  
                                        # /!\ By convention 
                                        # i predates j iif as_adjacency_matrix(g)[i,j]=1  
                                        # NB: The SWTL of a species is computed only over 
                                        # basal species that can be reached from it 
                                        # following edges directions.  
    adjmat <- as.matrix(as_adj(g))
    diag(adjmat) <- 0 # Remove Cannibalism
    SWtrophicLevels <- rep(NA, nrow(adjmat)) 
    basals <- which(rowSums(adjmat)==0)
    D <- distances(g,mode='out')  
    if(length(basals)>0){
        for(i in 1:nrow(adjmat)){
            DirectedDistToBasals <- D[i,basals]
            DirectedDistToBasals <- DirectedDistToBasals[is.finite(DirectedDistToBasals)]
            SWtrophicLevels[i] <- mean(DirectedDistToBasals)
        }
    } else{
        warning('Short Weighted Trophic Level can not be computed, there is no basal species')
    }
    return(SWtrophicLevels)
}

omnivoryLevels <- function(g){
  # Standard Deviation of the Trophic Level (Kefi) of the preys
  # By convention it is 0 for basal species
  # and species with only one prey.
  TL <- trophicLevels(g)
  adjmat <- as.matrix(as_adj(g))
  omlev <- NULL
  for(i in 1:dim(adjmat)[1]){
    # Omnivory level of each species
    if(sum(adjmat[i,]>0)>1){
      omlev[i] <- sd(TL[adjmat[i,]>0])
    }else{
      omlev[i] <- 0 # For basals and those with only one prey
    }
  }
  return(omlev)
}

trophicLevels <- function(g){
    adjmat <- as.matrix(as_adj(g))
    N <- nrow(adjmat)
    TLvec <- rep(1,N)
    for(rep in 1:10){
        TLtemp <- rep(0,N)
        for(i in 1:N){
            temp1 <- sum(adjmat[i,])  ## temp1 contains the number of prey of species i
            if(temp1>0){ ## If species i has at least one prey
                ## calculate the mean TL of the preys of species i
                TLtemp[i] <- sum( adjmat[i,] * TLvec ) / temp1
            }
        }
        TLvec <- 1 + TLtemp ## TL = 1 + av TL of the prey
    }
    TLvec <- TLvec - min(TLvec) +1
    return(TLvec)
}

