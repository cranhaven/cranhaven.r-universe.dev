# Part of the SMITIDstruct R package.
# Copyright (C) 2018 Jean-François Rey <jean-francois.rey@inra.fr>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation, Inc.,i
# 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#


#' @title loadViralPop
#' @description Load all ViralPop observated in the file.obs
#' @param directory path where is data
#' @param listFiles a dataframe with host ID, time observation and file name (filename.fasta)
#' @param listCol a list of listFiles colomns names ("id", "timeObs", "filename")
#' @param file.extension genotype file extension
#' @return a vector of VirlaPop object
#' @include Class-ViralPop.R
#' @examples
#' \donttest{
#' path = system.file("extdata", "data-simul/", package="SMITIDstruct")
#' files <- list.files(path, pattern = ".*.fasta" ,full.names=FALSE)
#' lfileinfo <- sapply(files,function(x){return(substr(x,1,nchar(x)-6))})
#' splitFiles <- strsplit(lfileinfo, "_");
#' listF <- cbind(data.frame(matrix(unlist(splitFiles),nrow=length(splitFiles), byrow=TRUE),
#'                stringsAsFactors = FALSE), names(splitFiles))
#' colnames(listF) <- c("id", "time", "filename")
#' lvpop <- loadViralPop(path,listF)
#' }
#' @export
loadViralPop <- function(directory, listFiles, listCol = list("id"="id", "timeObs"= "time", "filename" = "filename"), file.extension = "fasta") {
  if(missing(directory)) {
    stop("Directory is missing")
  }
  
  #obs.file <- paste(directory, "/", file.obs,sep="")
  
  #if( !file.exists(obs.file) ) {
  #  stop("can not find ", obs.file)
  #}
    
  #listObs <- utils::read.table(obs.file)
  listObs <- listFiles
  listObs[,listCol$filename] <- paste(directory,"/",listObs[,listCol$filename],sep="")
  
  #viralPopList <- vector()
  
  viralPopList <- as.list(apply(listObs,1, function(x){ loadViralObs(x[listCol$id], x[listCol$timeObs], x[listCol$filename])} ))
  #setNames(viralPopList, as.character(seq(1:length(viralPopList))))
  
  class(viralPopList) <- "ViralPopSet"
  
  return(unname(viralPopList))
  
  # 
  # files <- list.files(directory, file.extension, full.names = TRUE)
  # 
  # if( identical(files,character(0)) ) {
  #   stop(paste("no files",file.extension,"in",directory))
  # }
  # else {
  #   dnaset <- DNAStringSetList()
  #   for(file in files) {
  #     dnaset[[length(dnaset)+1]] <- readDNAStringSet(file)
  #   }
  # }
  # vp <- new("ViralPop", size = length(dnaset), genotypes = dnaset)
  # 
  # return(vp)
}

#' @title loadViralObs
#' @description load a ViralPop object 
#' @param id host pathogen ID
#' @param time time of the observation (numeric or Date)
#' @param file a fasta file
#' @return a new ViralPop object
#' 
#' @export
loadViralObs <- function(id, time, file) {
  if( is.numeric(as.numeric(time))) {time <- as.numeric(time)}
  else{
    if(is.StringDate(time)) time <- getTimestamp(time)
    else warning("loadViralObs can't determine time format")
  }

  obss <- readDNAStringSet(file)
  variants.nb <- length(obss)
  variants.genotypes <- unique(obss)
  variants.names <- sapply(unique(obss), FUN = function(p){ names(obss[p == obss])})
  variants.prop <- as.vector(sapply(unique(obss), FUN = function(p){ sum(p == obss) / length(obss)}))
  
  orderByProp <- order(variants.prop, decreasing=TRUE)
  variants.genotypesOrder <- variants.genotypes[orderByProp]
  variants.namesOrder <- as.list(variants.names[orderByProp])
  variants.propOrder <- variants.prop[orderByProp]
  rm(obss)
  
  return(new("ViralPop", ID = as.character(id), time = time, size = variants.nb, variants.namesOrder, genotypes = variants.genotypesOrder, proportions = variants.propOrder))
  
}

#' @title concatViralPop
#' @description concat several Viral population in one ViralPop object
#' @param lvpop a ViralPop Set
#' @param lid vector of viralpop id to concat
#' @return a ViralPop object with ID concatenation from all IDs and time at 0.
#' @export
concatViralPop <- function(lvpop, lid) {
    
    set=DNAStringSet(c())
    sizeT = 0
    for(v in lid) {
        set <- union(lvpop[[as.numeric(v)]]@genotypes,set)
        sizeT = sizeT + lvpop[[as.numeric(v)]]@size
    }
    
    res.names <- list()
    for( g in 1:length(set)) {
        for(v in lid) {
            index = which(lvpop[[as.numeric(v)]]@genotypes == set[[g]])
            if(length(index)) {
                if( is.null(res.names[g][[1]]) ){ res.names[g] <- list(lvpop[[as.numeric(v)]]@names[[index]]) }
                else{ res.names[g] <- list(c(unlist(res.names[[g]]), lvpop[[as.numeric(v)]]@names[[index]])) }
            }
        }
    }
    
    res.prop <- as.vector(sapply(1:length(set), FUN=function(x){length(res.names[[x]])/sizeT}))
    orderbyProp <- order(res.prop, decreasing = TRUE)
    all.names <- res.names[orderbyProp]
    all.prop <- res.prop[orderbyProp]
    all.set <- set[orderbyProp]
    names(all.set)<-lapply(all.names,'[[',1)
    
    # concat time
    #paste(sapply(lid, FUN=function(x){ return( lvpop[[x]]@time)}),collapse="_")
    
    return(new("ViralPop", ID = paste(lid,collapse="_"), time = 0, size = sizeT, all.names, genotypes = all.set, proportions = all.prop))
}

#' createAViralPop
#' @description Create a new ViralPop object
#' @param host_id host ID which viral pop is observed
#' @param obs_time time of the observation (numeric or date)
#' @param seq a data.frame of sequences ID, sequences and counts
#' @param id_seq column name containing the sequences ID
#' @param seq_value column name containing the sequences
#' @param prop column name containing the count of each sequences
#' @param compact boolean, default FALSE, if TRUE will try group identicals sequences (not implemented yet)
#' @export
createAViralPop <- function(host_id, obs_time, seq, id_seq="seq_ID", seq_value="seq", prop = "prop", compact=FALSE) {
    
    host_id <- as.character(host_id)
    if( !is.na(as.numeric(obs_time)) && is.numeric(as.numeric(obs_time))) {obs_time <- as.numeric(obs_time)}
    else { obs_time <- getTimestamp(obs_time) }
    proportions <- seq[[prop]]
    nb_variants <- sum(proportions)
    proportions <- seq[[prop]] / nb_variants
    names <- lapply(seq[[id_seq]], function(ids){ ids })
    names(names) <- seq[[id_seq]]
    genotypes <- DNAStringSet(seq[[seq_value]])
    names(genotypes) <- seq[[id_seq]]
    
    return(new("ViralPop", ID = host_id, time = obs_time, size = nb_variants, names, genotypes = genotypes, proportions = proportions))
}

#' loadViralPopSet
#' @description load a list of viral populations
#' @param lvpop a viralPopSet (default new one)
#' @param list a list (see details)
#' @details The list have to be on this format:  
#' list$HOST_ID$TIME$list$seq_id
#'                       $seq
#'                       $prop
#' A list indexed by host ID, follow by a list indexed by time (of observation).
#' The last list contains an array of seq_ID (sequence ID), an array of seq (sequence as characters), and an array of the count of seq.
#' example : 
#' $'HOST_42'$'2014-01-01T00:00:00'$seq_ID ["SEQ_1","SEQ_2"]
#' $'HOST_42'$'2014-01-01T00:00:00'$seq ["ACGT","TGCA"]
#' $'HOST_42'$'2014-01-01T00:00:00'$seq_ID ["46","6"]
#' @export
loadViralPopSet <- function(lvpop=list(), list) {
    
    viralPopList <- list()
    viralPop <- as.list(lapply(names(list),
                               function(host){ 
                                    viralPopList <<- append(viralPopList, as.list(lapply(names(list[[host]]), function(time){
                                        createAViralPop(host, time, data.frame(list[[host]][[time]], stringsAsFactors = FALSE ))
                                    })))
    }))
    
    class(viralPopList) <- "ViralPopSet"
    
    return(unname(viralPopList))
    
}


#' @title diversity.pDistance
#' @description diversity calculation using Mean Pairwise Distance
#' @param vpop a ViralPop object
#' @return result
#' 
#' @export
diversity.pDistance <- function(vpop) {
    matDist <- as.matrix(stringDist(vpop@genotypes, method="levenshtein"))
    
    som <- 0
    
    maxSeq <- length(vpop@genotypes)
    
    #for(i in 1:maxSeq) {
    #    for(j in i:maxSeq) {
            #if( i == j ) {
            #    som <- som + ((length(vpop@names[[i]]) * (length(vpop@names[[i]])-1)) /2) * matDist[i,j] #/length(vpop@genotypes[[i]])
            #} else {
    #            som <- som + (length(vpop@names[[i]]) * length(vpop@names[[j]])) * matDist[i,j]#/length(vpop@genotypes[[i]])
            #}
    #    }
    #}
    
    # for all i
    som <- sum(sapply(1:maxSeq, FUN = function(i) {
                                    # for all j from i to max
                                    sum(sapply(i:maxSeq, FUN = function(ind2, ind1){
                                                            length(vpop@names[[ind1]]) * length(vpop@names[[ind2]]) * matDist[ind1,ind2] }, ind1=i ))
                                }))
    
    
    p_distance <- 1/((vpop@size*(vpop@size-1))/2) * (2*som)/length(vpop@genotypes[[1]])
    
    return(p_distance)
}

#' diversity.sfs
#' @description Allele frequency spectrum or Site frequency spectra : the distribution of alternative allele frequencies across all sites of genetic sequences
#' @param vpop a viralPop class
#' @return the site frequency spectra 
#' @export
diversity.sfs <- function(vpop) {
    
    
    ### Test avec Z
    # library(adegenet)
    # library(ape)
    #load("~/Desktop/Jeff.RData")
    #Mat = Z[[3]][[1]]
    #Mat[Mat==1]="A"
    #Mat[Mat==2]="G"
    #Mat[Mat==3]="T"
    #Mat[Mat==4]="C"
    #Matbis=as.DNAbin(Mat)
    #Mat2=DNAbin2genind(Matbis,polyThres=1/1000)
    #Mat3=as.genlight(Mat2@tab)
    #Graph_Mat=glSum(Mat3,alleleAsUnit=TRUE, useC=FALSE)
    #Graph_Mat
    #ac_org <- alleleCount(Matbis, seq.char=c('a','c','g','t'))
    #sum(ac_org == Mat2@tab) == length(ac_org)
    #Graph_Mat_org <- glsum(ac_org)
    #sum(Graph_Mat == Graph_Mat_org) == length(Graph_Mat)
    
    alCount <- alleleCount(as.matrix(vpop@genotypes[])[1:length(vpop@genotypes),])
    
    # sum allele count of all sequence
    glsum <- function(x) {
        res <- integer(ncol(x))
        for(e in 1:nrow(x)){
            temp <- as.integer(x[e,])
            temp[is.na(temp)] <- 0L
            res <- res + temp
        }
    
        names(res) <- colnames(x)
        return(res)
    }
    
    globalAlleleCount <- glsum(alCount)
    
    
    return(globalAlleleCount)
}

#' alleleCount
#' @description count allele at each position
#' @param mat a genomique seq list as matrix by row
#' @param seq.char allele alphabet
#' @return a matrix, each row as a unique seq and col as allele count by position
alleleCount <- function(mat, seq.char =  c("A", "T", "G", "C")) {
    polyThres = 1/1000
    
    if (is.list(mat)) {
        mat <- as.matrix(mat)
    }
    if (is.null(colnames(mat))) {
        colnames(mat) <- 1:ncol(mat)
    }
    
    processLocus <- function(locus, posi) {
        vec <- as.character(locus)
        vec[!vec %in% seq.char] <- NA
        N <- sum(!is.na(vec))
        if (N == 0 || sum(table(vec)/N >= polyThres) < 2) 
            return(NULL)
        alleles <- unique(stats::na.omit(vec))
        out <- sapply(alleles, function(e) 1 * (vec == e))
        colnames(out) <- paste(posi, alleles, sep = ".")
        return(out)
    }
    
    temp <- lapply(1:ncol(mat), function(i) processLocus(mat[, i], 
                                                       i))
    col.names <- unlist(sapply(temp, colnames))
    temp <- as.matrix(data.frame(temp[!sapply(temp, is.null)]))
    if (is.null(temp) || ncol(temp) == 0) {
        cat("\nNo polymorphism detected - returning NULL.\n")
        return(NULL)
    }
    if (class(col.names) == "matrix") 
        col.names <- as.vector(col.names)
    
    colnames(temp) <- col.names
    rownames(temp) <- rownames(mat)
    return(temp)
}
