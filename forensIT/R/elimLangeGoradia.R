#' @title Eliminate Mendelian errors using Lange-Goradia algorithm
#' @description Eliminate Mendelian errors using Lange-Goradia algorithm
#' @param ped pedigree
#' @param iMarker index of marker to be used
#' @param bitera iterate until no more errors are found
#' @param bverbose print progress
#' @return pedigree with Mendelian errors eliminated
#' @export
#' @importFrom gtools combinations
#' @export
elimLangeGoradia<-function(ped,iMarker=1,bitera=TRUE,bverbose=TRUE){
  
  if(!is.null(ped$MARKERS)){
    bpedtools     <- TRUE
    pedMarkerData <- ped$MARKERS
  }else{
    bpedtools     <- FALSE
    pedMarkerData <- ped$markerdata
  }
    
  # get pedigree allele values per marker
  markerAlleles<- lapply(pedMarkerData,function(r){
    #rname   <-attr(r,"name")
    rAlleles<- list(unique(attr(r,"alleles")),attr(r,"name"))
    #names(rAlleles)[1]<-rname
    return(rAlleles)
  })

  names(markerAlleles)<-unlist(lapply(markerAlleles,function(r){return(r[2])}))
  markerAlleles<-lapply(markerAlleles,function(r){r[[1]]})
  
  #solo considero los marcadores presentes en el pedigree mas uno que representa...algun otro
  pedAlleles<-unique(c(pedMarkerData[[iMarker]]))
  alleles <-pedAlleles[pedAlleles>0]
  if(length(pedAlleles)<length(attr(pedMarkerData[[iMarker]],"alleles"))){
    alleles <- c(markerAlleles[[iMarker]][sort(unique(c(pedMarkerData[[iMarker]])))],"666")
  }
  
  #posibles genotipos a partir de los alelos del pedigree
  #gtypes  <-apply(combinations(length(alleles),2,alleles,repeats.allowed=TRUE),1,paste,collapse="/")
  gtypes  <-apply(combinations(length(alleles),2,alleles,repeats.allowed=TRUE),1,function(x){ #nolint
    paste(sort(x),collapse="/")})
  
  #identifico nuclear families
  
  if(bpedtools){
    pped <- data.frame(ped$ID,ped$FIDX,ped$MIDX)
    nInd <- length(ped$ID)
    ids  <- ped$ID   
  }else{
    pped <- ped$pedigree  
    nInd <- ped$nInd
    ids  <- ped$orig.ids   
  }
  parents = unique(pped[, 2:3])
  parents = parents[-match(0, parents[, 1]), , drop = FALSE]
  subnucs = lapply(nrow(parents):1, function(i) { #nolint
    par = parents[i, ]
    c(fa = par[[1]], mo = par[[2]], 
      offs = as.vector(pped[,1])[which(pped[, 2] == par[[1]] & pped[, 3] == par[[2]], 
                                       useNames = FALSE)])
  }) 
  
  # mdat  = do.call(cbind, pedMarkerData) # matriz de alelos por individuo del pedigree
  # rownames(mdat)<-ped$orig.ids
    lgeno<- vector("list", nInd)       # lista con posibles genotipos  
    names(lgeno)<-ids 

  for(i in 1:nInd){
    #lgeno[[i]]<-index2Genotypes(mdat[i,c(-1,0)+2*iMarker],alleles)
    lgeno[[i]]<-index2Genotypes2.pedtools(ped,i,iMarker,alleles)
  }
  
  #reordeno familias nucleares en funcion de cuanta info tengo (~ 1/|posibles_genotipos|)
  # priorizo padres, luego hijos
  infoScore<-unlist(lapply(subnucs,function(x){
    infoParents<-(1/length(lgeno[[x["fa"]]])+1/length(lgeno[[x["mo"]]]))
    infoOffs<-sum(unlist(lapply(lgeno[x[-c(1:2)]],function(y){
      return(1/length(y))
    })))
    return(2*infoParents+infoOffs)          
  }))
  subnucs <- subnucs[order(infoScore,decreasing=TRUE)]
  
  
  
  itera<-TRUE
  nitera<-0
  bchanged<-FALSE
  
  while(itera){
    lin<-vector("list",nInd) 
    names(lin)<-1:nInd
    
    itera<-FALSE
    nitera<-nitera+1
    if(bverbose)cat(paste("itera:", nitera),"\n")
    bchanged<-FALSE
    bchanged1 <- bchanged2 <- bchanged3 <- FALSE #nolint
    for (isub in seq_along(subnucs)){
      lin2<-vector("list",nInd) 
      names(lin2)<-1:nInd
      
      ssub<-as.character(subnucs[[isub]])
      offs = ssub[-(1:2)]   #family offsprings
      
      #parental genotypes
      gfa<-lgeno[[ssub[1]]]
      #if(is.null(gfa)) gfa <- index2Genotypes(mdat[sub[1],],alleles)
      gmo<-lgeno[[ssub[2]]]
      #if(is.null(gmo)) gmo <- index2Genotypes(mdat[sub[2],],alleles)
      genoParental<-expand.grid(gfa,gmo,stringsAsFactors=FALSE)
      
      
      
      #for each fa/mo genotype pair        
      for(i in seq_along(genoParental[,1])){   #itero sobre pares de genotipos parentales
        laux<-vector("list",length(ssub)) 
        names(laux)<-ssub
        mate<-expand.grid(getAllelesFromGenotypes(genoParental[i,1]),
                          getAllelesFromGenotypes(genoParental[i,2]),stringsAsFactors=FALSE)     
        gmate<-apply(mate,1,function(xx){paste(sort(xx),collapse="/")})             
        accum<-0
        for (iof in seq_along(offs)) {                
          of <- as.character(offs[iof])
          gof<-lgeno[[of]]#laux[[iof]]
          #if(is.null(gof)) gof<-index2Genotypes(mdat[of,],alleles)
          if(is.null(gof)) gof<-index2Genotypes2(ped,offs[iof],iMarker,alleles)                                  
          
          bcomp<-gof%in%gmate
          if(any(bcomp)){
            laux[[of]]    <- unique(c(laux[[of]],gof[which(bcomp)]))     #este offspring ok => lo almaceno temporalmente
            laux[[ssub[2]]]<- unique(c(laux[[ssub[2]]],genoParental[i,2]))
            laux[[ssub[1]]]<- unique(c(laux[[ssub[1]]],genoParental[i,1]))
            accum <- accum+1
          }else{
            break
          }
        }
        
        
        if( accum==length(offs) ){  #todos los hijos compatibles?
          #save parental genotypes
          bchanged1<-ifelse(all(laux[[ssub[1]]]%in%lin2[[ssub[1]]]),FALSE,TRUE)
          if(bchanged1) lin2[[ssub[1]]] <- sort(union(lin2[[ssub[1]]],laux[[ssub[1]]]))
          
          bchanged2<-ifelse(all(laux[[ssub[2]]]%in%lin2[[ssub[2]]]),FALSE,TRUE)
          if(bchanged2) lin2[[ssub[2]]] <- sort(union(lin2[[ssub[2]]],laux[[ssub[2]]]))
          
          #save offspring genotypes
          bchanged3<-FALSE
          for(iof in seq_along(offs)){
            bchangedOff <-ifelse(all(laux[[as.character(offs[iof])]]%in%lin2[[as.character(offs[iof])]]),FALSE,TRUE)
            if(bchangedOff){
              bchanged3<-TRUE
              lin2[[as.character(offs[iof])]] <- sort(union(lin2[[as.character(offs[iof])]],
                                                            laux[[as.character(offs[iof])]]))       
            } 
          }
        }
        
      }     #genoParental 
      
      # hay algun caso de incompatibilidad?
      if(any(unlist(lapply(lin2[ssub],is.null)))){
        warning(paste("Incompatibility detected for subnuclear group:",isub,"\n",
                      paste(names(subnucs[isub]),subnucs[isub],collapse="  "),"\n"))
      }else{
        
        #guardo los genotipos compatibles
        for(i in seq_along(ssub)){  
          if(!setequal(lgeno[[ssub[i]]],lin2[[ssub[i]]])) bchanged <-TRUE
          lgeno[[ssub[i]]]<-lin2[[ssub[i]]]
        }
      } 
    }       #subnucs  
    
    if(!bitera){
      itera=FALSE
    }else{
      itera<-bchanged
    }
  }        #while itera
  
  return(lgeno)
}           
