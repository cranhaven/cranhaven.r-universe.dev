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

divPartition <- function(gList, groups=NULL, eta=1, framework=c('RLC','Chao'), type=c('P','L','Pi'), abTable=NULL){
   if(sum(unlist(lapply(lapply(gList,FUN = function(x) V(x)$name),is.null)))>0){#check if nodes have names
    stop('nodes must have names (use V(g)$name)')
  }
   if(is.null(groups)){#if groups is NULL then each node froms its own group
    groups=unique(unlist(lapply(gList,FUN = function(g) V(g)$name)))
    names(groups)=groups
  }
  
  if(is.null(names(groups))){#check whether groups vector has nodes
    stop("groups must have names (names(groups) is NULL)")
  }
  if(prod(names(groups) %in% unique(unlist(lapply(gList,FUN = function(g) V(g)$name))))*prod(unique(unlist(lapply(gList,FUN = function(g) V(g)$name))) %in% names(groups))!=1){
    stop("the names of groups vector do not match to the names of the nodes of the metaweb")
  }
  ##consider the abTalbe
  if(!is.null(abTable)){
         #check whether the names of abTable correspond of the groups vector (and so to the nodes of the network)
         if(sum(!setequal(rownames(abTable),names(groups)))){
         stop('the rownames of abTable do not match to the names of the nodes of the metaweb')}
         #check whether the number of columns of the abundance table is the same as the number of local networks
         if(ncol(abTable) != length(gList)){'The number of columns of abTable is no the same as the length of gList'}
         #check whether the species that have non-null abundances are present in the local networks
         if(!prod(unlist(lapply(1:length(gList),function(i) setequal(V(gList[[i]])$name,rownames(abTable)[which(abTable[,i]>0)]))))){
           stop('the species that have non-nul abundances in abTable do not match with the nodes of the local networks (gList)')
         }
         #create a vertex attribute ab
         for(k in 1:length(gList)){
           gList[[k]] = set_vertex_attr(gList[[k]],name = "ab",value = abTable[V(gList[[k]])$name,k]/sum(abTable[V(gList[[k]])$name,k]))
         }
  } 
  if(is.null(abTable)){
    #uniform abundance on the nodes
    for(k in 1:length(gList)){
      gList[[k]] = set_vertex_attr(gList[[k]],name = "ab",value = 1/length(V(gList[[k]])))
    }
  }


  metaweb.array <- metawebParams(gList,groups)#get the metaweb array
   if(framework=='RLC'){
    if(type=='P'){
      P.mat=metaweb.array$P.mat
      P.mat=P.mat/sum(P.mat)
      meta.P=metacommunity(P.mat)#convert to class metacom for rdiversity
      
      mAlpha=as.numeric(norm_meta_alpha(meta.P,qs = eta)[,7]) #overall alpha diversity
      
      alphas=as.matrix(norm_sub_alpha(meta.P,qs = eta)[,c(6,7)])
      alphas.vec=as.numeric(alphas[,2])
      names(alphas.vec)=alphas[,1]
      
      beta=as.numeric(norm_meta_beta(meta.P,qs = eta)[7])
      gamma=as.numeric(meta_gamma(meta.P,qs=eta)[7])
      
      return(list(mAlpha=mAlpha,Alphas=alphas.vec,Beta=beta,Gamma=gamma))
      
    }
    if(type=='L'){
      n.groups=nrow(metaweb.array$P.mat)
      meta.links <- aperm(metaweb.array$L.array,c(2,1,3))  
      dim(meta.links) <- c(n.groups*n.groups,ncol(metaweb.array$P.mat)) 
      colnames(meta.links) <- colnames(metaweb.array$P.mat) 
      if(sum(rowSums(meta.links)>0)<nrow(meta.links)){
        meta.links=meta.links[-which(rowSums(meta.links)==0),]
      }

      meta.links=meta.links/(sum(meta.links))
      meta.L=metacommunity(meta.links)
      
      mAlpha=as.numeric(norm_meta_alpha(meta.L,qs = eta)[,7])
      
      alphas=as.matrix(norm_sub_alpha(meta.L,qs = eta)[,c(6,7)])
      alphas.vec=as.numeric(alphas[,2])
      names(alphas.vec)=alphas[,1]
      
      beta=as.numeric(norm_meta_beta(meta.L,qs = eta)[7])
      gamma=as.numeric(meta_gamma(meta.L,qs=eta)[7])
      
      return(list(mAlpha=mAlpha,Alphas=alphas.vec,Beta=beta,Gamma=gamma,meta=metaweb.array))
    }
    if(type=='Pi'){
      n.groups=nrow(metaweb.array$P.mat)
      meta.Pi <- aperm(metaweb.array$Pi.array,c(2,1,3))  
      dim(meta.Pi) <- c(n.groups*n.groups,ncol(metaweb.array$P.mat)) 
      colnames(meta.Pi) <- colnames(metaweb.array$P.mat) 
      if(length(c(which(is.na(rowSums(meta.Pi))),which(rowSums(meta.Pi)==0)))>0){
        meta.Pi=meta.Pi[-c(which(is.na(rowSums(meta.Pi))),which(rowSums(meta.Pi)==0)),]
      }
      meta.Pi=meta.Pi/sum(meta.Pi)
      meta_Pi=metacommunity(meta.Pi)
      
      mAlpha=as.numeric(norm_meta_alpha(meta_Pi,qs = eta)[,7])
      
      alphas=as.matrix(norm_sub_alpha(meta_Pi,qs = eta)[,c(6,7)])
      alphas.vec=as.numeric(alphas[,2])
      names(alphas.vec)=alphas[,1]
      beta=as.numeric(norm_meta_beta(meta_Pi,qs = eta)[7])
      gamma=as.numeric(meta_gamma(meta_Pi,qs=eta)[7])
      
      return(list(mAlpha=mAlpha,Alphas=alphas.vec,Beta=beta,Gamma=gamma))
    }
  }
  if(framework=='Chao'){
    if(type=='P'){
      abg=abgDecompQ(spxp = t(metaweb.array$P.mat),q = eta)
      return(list(mAlpha=abg$mAlpha,Alphas=abg$Alphas,Beta=abg$Beta,Gamma=abg$Gamma))
    }
    if(type=='L'){
      n.groups=nrow(metaweb.array$P.mat)
      meta.links <- aperm(metaweb.array$L.array,c(2,1,3))  
      dim(meta.links) <- c(n.groups*n.groups,ncol(metaweb.array$P.mat)) 
      colnames(meta.links) <- colnames(metaweb.array$P.mat)
     if(sum(rowSums(meta.links)>0)<nrow(meta.links)){
        meta.links=meta.links[-which(rowSums(meta.links)==0),]
      }
      abg=abgDecompQ(spxp = t(meta.links),q = eta)
      return(list(mAlpha=abg$mAlpha,Alphas=abg$Alphas,Beta=abg$Beta,Gamma=abg$Gamma))
      
    }
    if(type=='Pi'){
      n.groups=nrow(metaweb.array$P.mat)
      meta.Pi <- aperm(metaweb.array$Pi.array,c(2,1,3))  
      dim(meta.Pi) <- c(n.groups*n.groups,ncol(metaweb.array$P.mat)) 
      colnames(meta.Pi) <- colnames(metaweb.array$P.mat) 
      if(length(c(which(is.na(rowSums(meta.Pi))),which(rowSums(meta.Pi)==0)))>0){
        meta.Pi=meta.Pi[-c(which(is.na(rowSums(meta.Pi))),which(rowSums(meta.Pi)==0)),]
      }
      abg=abgDecompQ(spxp = t(meta.Pi),q = eta)
      return(list(mAlpha=abg$mAlpha,Alphas=abg$Alphas,Beta=abg$Beta,Gamma=abg$Gamma))
    }
  }
}
