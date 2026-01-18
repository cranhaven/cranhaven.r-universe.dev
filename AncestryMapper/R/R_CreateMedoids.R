#' Creates Ancestry Mapper Population Reference.
#'
#' Generates arithmetic population reference from PLINK tPED files.
#'
#' @param pathTotpeds Character vector giving path to folder containing tPED file(s) to be used.
#' 
#' @param AMmcapply Logical value (TRUE or FALSE), specifying if the multicore funcion mcapply, should be used. Inappropriate for most HPC cluster systems.
#' Default = FALSE
#' 
#' @param nrcores Numeric value detailing how many cores should be used if AMmcapply==TRUE. If left unspecificed the number of cores will be detected and mc.cores will be set to that number -1.
#' 
#' @param wd Character vector giving the desired working directory to house the outputs of calculateAMids. If left unspecified will use current working directory.
#'
#' @param pathAll00 Character vector giving the path to a file containing the full data table of each dbSNP and both alleles. A toy version covering the SNPs used in the toy data is included. A full version can be found at: http://bit.ly/1OUstDP
#'
#' @param chipMan Character vector giving name of company from which the SNP panel is derived. Eg 'Illumina', 'Affymetrix'. If no value is given will default to 'ChipMan'. If it is whole genome sequencing, please put 'WG'.
#' The value will appear in the name of the arithmetic reference file. e.g. 'medoidArithmetic_Yoruba_HGDP_1000_Illumina.rda'.
#'
#' @param OutForm Character vector giving option for output format for arithmetic medoids. Can be one of three options.
#' 'ods' will generate a raw text file with the default extension of '.ods'. This is the default option as is the most flexible format.
#' 'rds' will save the arithmetic medoid as a .rds file which can be loaded into R faster and is also roughly a third the size of the raw text version.
#' 'rda' will save the arithmetic medoid as a .rda file which can be loaded into R faster and is also roughly a third the size of the raw text version.
#' 
#'
#' @examples
#' \dontrun{
#' chipManExample <- 'Illumina'
#' tpeds <- system.file('extdata', package='AncestryMapper')
#' 
#' createMedoid(pathTotpeds = tpeds, chipMan = chipManExample)
#' 
#' }
#' @import parallel
#' @rdname createMedoid
#' @export
#' 
createMedoid <- function(pathTotpeds, AMmcapply=F, nrcores, wd, pathAll00, chipMan='ChipMan', OutForm='rda'){

    #if(AMmcapply){
    #    library(parallel)
    #}
    
    
    if(missing(pathAll00)) stop("Error: No Path Given for pathAll00")
    
    if(missing(pathTotpeds)) stop("Error: No Path Given for tPED Files")
    
    #if(missing(plink)) stop("Error: No path given for Plink binary, if it is in the Root Directory, should still be either plink='plink' or plink='plink1.9', depending on version")

        
    if(!missing(wd)){
        initialwd <- getwd()
        setwd(wd)
    }
    if(missing(wd)){
        wd <- getwd()
        setwd(wd)
    }
    
    ## nice, easy function
    FUN.MinMax12 <- function(freq){
        MinMaxGeno <- c(freq[1],freq[2])
        ## get what is 1; 2 outside for what is not 1
        min1 <- min(MinMaxGeno, na.rm=F)
        ## max1 <- max(MinMaxGeno, na.rm=F)
        ## substitute 1 the minimum
        freq <- gsub(min1,'1',freq)
        ## freq <- gsub(max1,'2',freq)
        return(freq)
    }## FUN.MinMax12
    
    ## Calculate arithmetic medoids
    FUN.Arithmetic <- function(MinMax12){
        class(MinMax12) <- 'numeric'
        Arith <- (MinMax12[1]*MinMax12[3])+(MinMax12[2]*(1-MinMax12[3]))
        Arith <- Arith*2
        return(Arith)
    }## FUN.Arithmetic
    
    FUN.sumMinMaxName <- function(minMax,indGeno){
        MinMaxGeno <- as.character(minMax[indGeno[1],])
        ## get what is 1; 2 outside for what is not 1
        min1 <- min(MinMaxGeno, na.rm=F)
        ## substitute 1 the minimum
        submin <- gsub(min1,'1',indGeno[-1])
        indGeno <- c(indGeno[1],submin)
        return(indGeno)
    }
    
    
    
    FUN.addSnps <- function(indGeno){
        indGeno <- as.numeric(indGeno)
        indGeno <- matrix(unlist(indGeno), ncol=2, byrow = T)
        indGeno <- indGeno[, 1] + indGeno[, 2]
        return(indGeno)
    }
    
    
    FUN.getSnpsFromBeds <- function(bimFileName){
        snps <- read.table(bimFileName)
        snps <- as.character(snps$V1)
        return(snps)
    }
    
    
    ## get only the snps that will be used, via ped files
    bimFile <- list.files(path=pathTotpeds,pattern="*\\.snplist$",full.names=T)
    ## bimFile <- grep("HGDP|HapMap",bimFile,v=T)
    
    snpsList <- lapply(bimFile,FUN.getSnpsFromBeds)
    snpsList <- unique(do.call('c',snpsList))
    
        
    
    gc()
    if(identical(grep('rda$',pathAll00), as.integer(1))){
      load(pathAll00)
    } else if(identical(grep('rds$',pathAll00), as.integer(1))){
      MinMaxFreq <- readRDS(pathAll00)
    } else{
      MinMaxFreq <- read.table(pathAll00,header=F)
    }
    
    
    MinMaxFreq <- MinMaxFreq[row.names(MinMaxFreq)%in%snpsList,]
    
    
    ## ped.f=list.files(path=pathTotpeds,pattern="*\\.bed$")[2]
    FUN.bedMedoids <- function(ped.f){
        
        #zx2 <- paste0(pathTotpeds,'/',gsub(".tped","",ped.f))
        zx2 <- gsub(".tped","",ped.f)
        #outname <- gsub(paste0(pathTotpeds,'/'),"",ped.f)
        outname <- strsplit(zx2,'/')[[1]][length(strsplit(zx2,'/')[[1]])]
        #outname <- gsub(".tped","",outname)
        famfile <- paste0(zx2,".tfam")
        famfile <- read.table(famfile)

        if(length(famfile$V2)<10) print(paste0('Insufficient samples, minimum at this release is 10. Population dataset submitted: ',ped.f,' contains ',length(famfile$V2)))
        
        if(length(famfile$V2)>=10){
            
            
            print(ped.f)
            #tempOut <- paste0(ped.f,'_out10') ## temporary file names; delete later
            #zx2 <- paste0(pathTotpeds,'/',gsub(".bed","",ped.f))
            
                    
            ## snps and individuals; add to the final AM  id table
            #snps <- read.table(paste0(zx2,'.bim'))
            
            #inds <- read.table(paste0(zx2,'.fam'))
            inds <- famfile[,2]
            
            ## qc: ped is 1234, not ACGT, only run on first line, 'nline=1'
            ##conQc <- read.table(paste0(tempOut,'.tped'), nrow=1)
            ##all12 <- unique(unlist(conQc)[-c(1:4)])
            ##if(any(all12%in%c('1','2','3','4'))) stop("tm: recode to ACGT, it's currently 1234")
            
            ## import tped file; tried scan; read.table the fastest
            ##snps1234 <- read.table(paste0(tempOut,'.tped'))
            snps1234 <- read.table(ped.f)
            snps <- snps1234[,2]
            
            row.names(snps1234) <- snps1234$V2
            ## remove pedigree                                                                                            
            snps1234 <- snps1234[,!colnames(snps1234)%in%c('V1','V2','V3','V4')]
            ## matrix; much faster
            snps1234 <- as.matrix(snps1234)
            
            snps1234 <- snps1234[intersect(row.names(snps1234),row.names(MinMaxFreq)),]
            
            SortedGeno <- t(apply(snps1234,1,function(vec) paste0(sort(unique(vec)),collapse='')))
            
            #print(sum(SortedGeno=='CG'))
            #print(colnames(SortedGeno)[SortedGeno=='CG'])
            #print(SortedGeno=='CG')
            #colnames(SortedGeno)[SortedGeno == "CG"]
            
            CGs <- SortedGeno=='CG'
            indel <- nchar(SortedGeno)>2
            
            #if(sum(SortedGeno=='CG')!=0){
            #  SortedGeno2 <- t(matrix(SortedGeno[SortedGeno != "CG"]))
            #  colnames(SortedGeno2) <- colnames(SortedGeno)[SortedGeno != "CG"]
            #  SortedGeno <- SortedGeno2
            #  rm(SortedGeno2)
            #}
            #if(sum(nchar(SortedGeno)>2)){
            #  SortedGeno2 <- t(matrix(SortedGeno[nchar(SortedGeno)==2]))
            #  colnames(SortedGeno2) <- colnames(SortedGeno)[nchar(SortedGeno)==2]
            #  SortedGeno <- SortedGeno2
            #  rm(SortedGeno2)
            #}
            #if(sum(SortedGeno=='CG')!=0) stop("CGs Present in .frq file, strand reading issue, remove.")  

            
            monoCGs <- SortedGeno=='C' | SortedGeno=='G'
            
            
                                        # # If any mono Gs or Cs in .frq file, 
            if(sum(monoCGs)!=0) {
                snps1234 <- data.frame(UNIQID=row.names(snps1234),snps1234)
                snps1234 <- as.matrix(snps1234)
                if(sum(monoCGs)==1){
                    ## get what is 1; 2 outside for what is not 1
                    min1 <- min(MinMaxFreq[monoCGs,], na.rm=F)
                    max2 <- max(MinMaxFreq[monoCGs,], na.rm=F)
                    ## substitute 1 the minimum
                    submin <- gsub(min1,'1',snps1234[monoCGs,])
                    submax <- gsub(max2,'2',snps1234[monoCGs,])
                    #indGeno <- c(indGeno[1],submin)
                    #snp3 <- t(data.frame(snps1234[monoCGs,]))
                    #row.names(snp3) <- row.names(snps1234)[monoCGs]
                    #minMax.f <- t(data.frame(MinMaxFreq[dimnames(snp3)[[1]],]))
                    #row.names(minMax.f) <- row.names(snps1234)[monoCGs]
                }
                if(sum(monoCGs)>1){
                    minMax.f <- MinMaxFreq[dimnames(snps1234[monoCGs, ])[[1]],]
                    #debug(FUN.sumMinMaxName)
                    #undebug(FUN.sumMinMaxName)
                    snps1234[monoCGs,] <- t(apply(snps1234[monoCGs,],1, function(r.f) FUN.sumMinMaxName(minMax=minMax.f,indGeno=r.f)))
                }
                ## Replacing monoCG max with 2
                ###snps1234 <- snps1234[,!colnames(snps1234)%in%"UNIQID"]
                snps1234[monoCGs,] <- gsub("^C$|^G$","2",snps1234[monoCGs,])
                            
                #minMax.f <- MinMaxFreq[dimnames(snps1234[monoCGs,])[[1]],]
                #snps1234 <- data.frame(UNIQID=row.names(snps1234),snps1234)
                #snps1234 <- as.matrix(snps1234)
                
                #debug(FUN.sumMinMaxName)
                #undebug(FUN.sumMinMaxName)
                #snps1234[monoCGs,] <- t(apply(snps1234[monoCGs,],1, function(r.f) FUN.sumMinMaxName(minMax=minMax.f,indGeno=r.f)))
                ## Replacing monoCG max with 2
                #snps1234 <- snps1234[,!colnames(snps1234)%in%"UNIQID"]
                #snps1234[monoCGs,] <- gsub("^C$|^G$","2",snps1234[monoCGs,])
                
            } ## end of snps exist that are not A nor T 
            

            
            if(sum(nchar(SortedGeno)>2)!=0){
              indelrs <- colnames(SortedGeno)[indel]
              if(sum(nchar(SortedGeno)>2)==1){
                ## get what is 1; 2 outside for what is not 1
                min1 <- min(MinMaxFreq[indelrs,], na.rm=F)
                max2 <- max(MinMaxFreq[indelrs,], na.rm=F)
                ## substitute 1 the minimum
                submin <- gsub(min1,'1',snps1234[indelrs,])
                submax <- gsub(max2,'2',snps1234[indelrs,])
                #indGeno <- c(indGeno[1],submin)
                #snp3 <- t(data.frame(snps1234[monoCGs,]))
                #row.names(snp3) <- row.names(snps1234)[monoCGs]
                #minMax.f <- t(data.frame(MinMaxFreq[dimnames(snp3)[[1]],]))
                #row.names(minMax.f) <- row.names(snps1234)[monoCGs]
              }
              if(sum(nchar(SortedGeno)>2)>1){
                minMax.f <- MinMaxFreq[dimnames(snps1234[indelrs, ])[[1]],]
                #debug(FUN.sumMinMaxName)
                #undebug(FUN.sumMinMaxName)
                snps1234[indelrs,] <- t(apply(snps1234[indelrs,],1, function(r.f) FUN.sumMinMaxName(minMax=minMax.f,indGeno=r.f)))
                snps1234[nchar(snps1234)>1] <-'2'
              }
            }
            snps1234 <- snps1234[,!colnames(snps1234)%in%"UNIQID"]
                
            snps1234 <- gsub("A","1",snps1234)
            snps1234 <- gsub("T","2",snps1234)
            
            subx <- SortedGeno=='AC'
            snps1234[subx,] <- gsub("C","2",snps1234[subx,])
            subx <- SortedGeno=='CT'
            snps1234[subx,] <- gsub("C","1",snps1234[subx,])
            subx <- SortedGeno=='AG'
            snps1234[subx,] <- gsub("G","2",snps1234[subx,])
            subx <- SortedGeno=='GT'
            snps1234[subx,] <- gsub("G","1",snps1234[subx,])
            
            if(sum(SortedGeno=='CG')!=0){
              CGrs <- colnames(SortedGeno)[!CGs]
              snps1234 <- snps1234[CGrs,]
              SortedGeno <- SortedGeno[,CGrs]
            }
            
            
            SortedGeno <- paste0(names(SortedGeno),'_',SortedGeno)
            debug(FUN.addSnps)
            undebug(FUN.addSnps)
                                        # Sum of strands   
            SumPed <- apply(snps1234,1,FUN.addSnps)
            dimnames(SumPed)[[1]] <- inds
            ## distances of peds to lapplys
            eucdist <- as.matrix(dist(SumPed,method="euclidean",diag=T,upper=T))
            ## write.table(eucdist,file="eucdisttab",quote=F)
            #library(svd)
            
            h <- svd(eucdist,1,0)
            h0 <- data.frame(h$u)
            h0$V2 <- inds
            #print(names(eucdist))
            #print(h0)
            h001 <- h0[order(h0$h.u),]
            
            #print(h001)
            
            svdsd <- 2*sd(h001[,1])
            svdm <- mean(h001[,1])
            upperlm <- svdm + svdsd
            lowerlm <- svdm - svdsd
            h002 <- h001[h001[,1] > lowerlm & h001[,1] < upperlm,]
            PCAseq <- round(seq(from=1,to=length(h002[,1]),length.out=10),0)
            PCAseqInds <- as.character(h002[PCAseq,2])
            SumPed <- SumPed[row.names(SumPed)%in%PCAseqInds,]
                                        
            SumPedScores <- lapply(seq_len(ncol(SumPed)), function(i) as.numeric(SumPed[,i]))
            MedScores <- lapply(SumPedScores,function(i) round(mean(unlist(i)),1))
            MedScores <- as.character(unlist(MedScores))
            
            arithmeticRefsMedoids <- list(c('Pop',SortedGeno),c(outname,MedScores))
            #ArithMedoid <- t(matrix(MedScores))
            ##arithmeticRefsMedoids <- t(matrix(MedScores))
            #colnames(ArithMedoid) <- colnames(SumPed)
            ##colnames(arithmeticRefsMedoids) <- colnames(SumPed)
            
            print(outname)
            if(OutForm=='ods'){
                #write.table(arithmeticRefsMedoids,file=paste0('medoidArithmetic_',outname,'_',length(colnames(SumPed)),'_ChipManufacturer.ods'),row.names=F,col.names=T,quote=F,sep=' ')
                write.table(arithmeticRefsMedoids,file=paste0('medoidArithmetic_',outname,'_',length(colnames(SumPed)),'_',chipMan,'.ods'),row.names=F,col.names=T,quote=F,sep=' ')
                print(paste0('Wrote Arithmetic Medoid to:',getwd(),'/','medoidArithmetic_',outname,'_',length(colnames(SumPed)),'_',chipMan,'.ods'))
            }
            if(OutForm=='rda'){
                #save(arithmeticRefsMedoids,file=paste0('medoidArithmetic_',outname,'_',length(colnames(SumPed)),'_ChipManufacturer.rda'))
                save(arithmeticRefsMedoids,file=paste0('medoidArithmetic_',outname,'_',length(colnames(SumPed)),'_',chipMan,'.rda'))
                print(paste0('Saved Arithmetic Medoid to:',getwd(),'/','medoidArithmetic_',outname,'_',length(colnames(SumPed)),'_',chipMan,'.rda'))
            }
            if(OutForm=='rds'){
                #saveRDS(arithmeticRefsMedoids,file=paste0('medoidArithmetic_',outname,'_',length(colnames(SumPed)),'_ChipManufacturer.rds'))
                saveRDS(arithmeticRefsMedoids,file=paste0('medoidArithmetic_',outname,'_',length(colnames(SumPed)),'_',chipMan,'.rds'))
                print(paste0('Saved Arithmetic Medoid to:',getwd(),'/','medoidArithmetic_',outname,'_',length(colnames(SumPed)),'_',chipMan,'.rds'))
            }
        }
    }
    bedFile <- list.files(path=pathTotpeds,pattern="*\\.tped$",full.names=T)
    if(!AMmcapply) lapply(bedFile, FUN.bedMedoids)
    if(AMmcapply){
        if(missing(nrcores)) nrcores <- detectCores()-2
        mclapply(bedFile, FUN.bedMedoids,mc.cores=nrcores)
    }
}
