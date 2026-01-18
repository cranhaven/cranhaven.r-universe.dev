#' Calculate genetic distances.
#'
#' Calculates genetic distance between samples and population references.
#'
#' @param pathTotpeds Character vector giving path to folder containing the plink tPED file(s) to be used.
#' 
#' @param pathToAriMedoids Character vector giving path to folder containing the arithmetic references to be used.
#' 
#' @param AMmcapply Logical value (TRUE or FALSE), specifying if the multicore funcion mcapply, should be used.
#' Inappropriate for most HPC cluster systems. Default = FALSE 
#' 
#' @param nrcores Numeric value detailing how many cores should be used if AMmcapply==TRUE.
#' If left unspecificed the number of cores will be detected and nrcores will be set to that number -2.
#' 
#' @param seqchip Character vector specifying if only references from one main SNP chip panel are to be used. All references are marked with what chip panel they use at the end of their file names, eg 'Yoruba.HGDP.20000.Illumina.ods'
#' May be important if your data has few SNPs in common with one panel. All toy references prepared use 'Illumina' panels. Whole Genome sequence data is specified with 'WG'.
#' Supports custom designations, but will trigger a warning when used.
#' 
#' @param noseqdat Logical value (TRUE or FALSE), specifying if sequence data is to be excluded, will use only references that do not have names ending in '.WG.ods/rds/rda'. Default = FALSE
#' 
#' @param wd Character vector giving the desired working directory to house the outputs of calculateAMidsArith. If left unspecified will use current working directory.
#' 
#' @param NameOut Character vector giving the desired prefix name for the AMid file. Default is NULL.
#' 
#' @param pathAll00 Character vector giving the path to a file containing the full data table of each dbSNP and both alleles. An example version covering the SNPs used in the example data is included. A full version can be found at: http://bit.ly/1OUstDP
#' 
#'
#' 
#' @examples
#' \dontrun{
#' Refs <- system.file('data', package = 'AncestryMapper')
#' tpeds <- system.file('extdata', package = 'AncestryMapper')
#' Corpheno <- system.file('extdata', 'CorPheno', package = 'AncestryMapper')
#' All00Frq <- system.file ('data', 'MinMaxFreq.rda', package = 'AncestryMapper')
#' 
#' genetic.distance <- calculateAMidsArith(pathTotpeds = tpeds,
#'                                    NameOut = 'Example',
#'                                    pathToAriMedoids = Refs,
#'                                    pathAll00 = All00Frq)
#' 
#' plotAMids(AMids = genetic.distance, phenoFile = Corpheno, columnPlot = "I")
#' }
#' @import svd
#' @import parallel
#' @rdname calculateAMidsArith
#' @export
#' 
calculateAMidsArith <- function(pathTotpeds,pathToAriMedoids,AMmcapply=F,nrcores,seqchip='',noseqdat=F,wd,NameOut=NULL,pathAll00){
    
   
    #if(AMmcapply){
    #    library(parallel)
    #}

        
    if(missing(pathTotpeds)) stop("Error: No Path Given for Plink tPED Files")
    
    if(missing(pathToAriMedoids)) stop("Error: No Path Given for medoid references")
    
    if(!missing(wd)){
        initialwd <- getwd()
        setwd(wd)
    }
    if(missing(wd)) wd <- getwd()


    #if(species

    
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

    FUN.SharedSNPs <- function(y){
        for(z in y){
            #bim <- read.table(paste0(pathoToBeds,"/",z))
            bim <- read.table(z)
            bimSnp <- as.character(bim[,1])
            if(z==y[1]) SNPsComm <- bimSnp
            if(z!=y[1]) SNPsComm <- intersect(SNPsComm,bimSnp)
        }
        return(SNPsComm)
    }
    
    TM.distEuc <- function(X,y){
        X <- as.matrix(X)
        y <- as.vector(y)
        dX <- dim(X)
        p <- dX[1]
        n <- dX[2]
        return(sqrt(as.vector(rowMeans(X^2) + mean(y^2) - 2 * X %*% y/n)))
    } ## TM.distEuc

    tm.ped <- function(pedFile.f, ncol.f='no', sep.f=' ', nrLines, head.f=F, nFields, verbose.f=F){
        pedOut <- list()
        con <- file(pedFile.f, 'r')
        nrLinesFile <- as.numeric(unlist(strsplit(system(paste0('wc -l ',pedFile.f), intern=T),' '))[1])
        print(nrLinesFile)
        while(length(pedOut)<nrLinesFile){
            ped.i <- scan(con, nlines=1, what='character', quiet=T, sep=sep.f, flush=F)
            ## print(length(ped.i))
            ## print(head(ped.i,10))
            if(!missing(nFields)) ped.i <- ped.i[1:nFields]
            ## print(length(ped.i))
            if(verbose.f) print(length(pedOut))
            pedOut[[length(pedOut)+1]] <- ped.i
                                        #if(!missing(nrLines)) if(count>nrLines) break
        } ## while, get all lines of file
        close(con)
        if(identical(head.f,T)) {names(pedOut) <- pedOut[1]; pedOut <- pedOut[-1]}
        return(pedOut)
    }## close pedReadingFunction
    
    FUN.distEuc <- function(X,y,z){
        #inter <- intersect(colnames(X),names(y))
        inter <- intersect(colnames(X),refsnpinter)
        inter <- intersect(inter,z)
        #print(paste0('Len Inter1 ',length(inter)))
        if(nrow(X)==1) X <- t(data.frame(X[,inter]))
        if(nrow(X)!=1) X <- X[,inter]
        #print(paste0('Number Common SNPs between ',X,' and ',y,': ',length(X)))
        y <- as.vector(y[inter])
        #print(class(y))
        #print(class(X))
        EuDist <- TM.distEuc(X,y)
        attr(EuDist,'commonSnps') <- length(inter)
        return(EuDist)
    } ## FUN.distEuc
    



    ## nameArithmetic.f <- arithmeticRefsMedoidsFile[1]
    ## for cases where we are reading from a text "ods" file
    FUN.listArithmeticMedoidsOds <- function(nameArithmetic.f){
        filName <- strsplit(nameArithmetic.f,'_')
        if(identical(grep('rda$',nameArithmetic.f), as.integer(1))) load(nameArithmetic.f)
        if(identical(grep('.ods$',nameArithmetic.f), as.integer(1))) tm.ped(nameArithmetic.f,head.f=F)
        #arithmeticRefsMedoids <- tm.ped(nameArithmetic.f,head.f=F)
        #arithmeticRefsMedoids <- load(nameArithmetic.f)
        if(identical(grep('rds$',nameArithmetic.f), as.integer(1))) arithmeticRefsMedoids <- readRDS(nameArithmetic.f)
        snpNames <- arithmeticRefsMedoids[[1]]
        
        ## For dealing with nucleotides being kept as part of snpNames
        snpsMinusNucs <-lapply(snpNames[-1], function(z) strsplit(z, "_")[[1]][1])
        snpNames[2:length(snpNames)] <- snpsMinusNucs
        
        arithmetValuesSnps <- as.vector(arithmeticRefsMedoids[[2]])
        names(arithmetValuesSnps) <- snpNames
        ## get population name, remove it from the vector
        #pop <- as.character(arithmetValuesSnps[2])
        pop <- tail(filName[[1]],4)[1]
        
        arithmetValuesSnps <- arithmetValuesSnps[-1]
        class(arithmetValuesSnps) <- 'numeric'
        ##attr(arithmetValuesSnps, 'popName') <- paste0(pop,".",filName[[1]][3])
        attr(arithmetValuesSnps, 'popName') <- paste0(pop,".",tail(filName[[1]],3)[1])
        ## we are so cute, what's SimCity?
        if(length(grep('Irish',attr(arithmetValuesSnps,'popName'),value=T)>0)) print("Calculating Distance To Tipperary...")
        if(length(grep('Irish',attr(arithmetValuesSnps,'popName'),value=T)>0)) print("Not Far Enough.")
        if(length(grep('Finnish',attr(arithmetValuesSnps,'popName'),value=T)>0)) print("Finnished Finish")
        print(paste(attr(arithmetValuesSnps,'popName'),"Finished"))
        return(arithmetValuesSnps)
    }   ## FUN.listArithmeticMedoidsOds
    ## function to read "ods" arithmetic functions
    
    
                                        # MODULE Arithmetic medoids
    
                                        # list of all arithmetic medoids
    
    ## Pull list of medoids, select specific chipsets if applicable
    arithmeticRefsMedoidsFile <- list.files(path=pathToAriMedoids,pattern='medoidArithmetic_*',full.names=T)
    if(seqchip=='Illumina'){
        arithmeticRefsMedoidsFile <- grep("WG|Illumina",arithmeticRefsMedoidsFile,value=T)
    }
    if(seqchip=='Affymetrix'){
        arithmeticRefsMedoidsFile <- grep("WG|Affymetrix",arithmeticRefsMedoidsFile,value=T)
    }
    if(seqchip!='Illumina'&&seqchip!='Affymetrix'&&seqchip!=''&&seqchip!='WG'){
        arithmeticRefsMedoidsFile <- grep(paste0("WG|",seqchip),arithmeticRefsMedoidsFile,value=T)
        print(paste0("***Warning: ",seqchip," is a custom value not a native value for seqchip, only NULL, WG, Illumina or Affymetrix are. The function will still find medoids marked at the end of their name with .",seqchip,".ods but they will have to have been produced by the user, if this is what you have done, ignore this warning. Otherwise check there isn't a typo in the value given.*** Please contact the maintainer of the package if you have any questions." ))
    }
    if(noseqdat){
        arithmeticRefsMedoidsFile <- grep("WG",arithmeticRefsMedoidsFile,value=T,invert=T)
    }
        
    medoidsList <- lapply(arithmeticRefsMedoidsFile,FUN.listArithmeticMedoidsOds)
    names(medoidsList) <- unlist(lapply(medoidsList, function(obj) attr(obj,'popName')))
    medsnp <- list()
    for(z in seq(length(medoidsList))) medsnp[[z]] <- names(medoidsList[[z]])
    refsnpinter <- Reduce(intersect, medsnp)


    for(z in medoidsList){
      medsnp <- names(z)
      if(exists('intermed')) intermed <- intersect(intermed,medsnp)
      if(!exists('intermed')) intermed <- medsnp
      
    }
    #print(length(intermed))
    MedConform <- function(medentry){
      medentry <- medentry[intermed]
      return(medentry)
    }
    medoidsList <- lapply(medoidsList,MedConform)


                                        # MODULE MinMax
                                        # from Snp00 get all hets; give 1 or 2 depending on alphabetical order of the genotype
    
    
    FUN.getSnpsFromBeds <- function(bimFileName){
        z2 <- bimFileName
        snps <- read.table(paste0(z2))
        snps <- as.character(snps$V2)
        return(snps)
    }
    
    FUN.getUniqSnps <- function(bimFileName){
        z2 <- bimFileName
        snps <- read.table(paste0(z2))
        snps <- as.character(snps[,1])
        return(snps)
    }
    

    ## get only the snps that will be used, via ped files
    #bimFile <- list.files(path=pathTotpeds,pattern="*\\.bim$",full.names=T)
    #bimFile <- grep("HGDP|HapMap|TU_|Hellenthal|Ipatimup|IrinaMP|UCD|slavic_temp|BalkansTemp|Tibet|Genome",bimFile,v=T)
    
    
                                        #HMSNP <- read.table(HapMapSNPs)
                                        #HMSNP <- as.character(HMSNP$V1)
    
    
    #lapply(bimFile,FUN.Frq)
    #frqFile <- list.files(path=wd,pattern="*\\.frq$")
    
    #snpsList <- lapply(bimFile,FUN.getSnpsFromBeds)
    SNPls <- list.files(path=pathTotpeds,pattern="*\\.snplist$",full.names=T)
    snpsList <- lapply(SNPls,FUN.getUniqSnps)
    snpsList <- unique(do.call('c',snpsList))
    #unlink("snpls.txt")
    #write.table(snpsList,file="snpls.txt",row.names=FALSE,col.names=FALSE,quote=FALSE)
    
    ## from snp00 file, that has all the snps in the genome, create file for our snps
    #MinMaxFreq <- read.table(system.file('extdata',"FreqtempAllSnps00-IllAffy.frq",package="AncestryMapper"),h=T)
    #row.names(MinMaxFreq) <- MinMaxFreq$SNP
    #MinMaxFreq$SNP <- NULL
    #MinMaxFreq <- t(apply(MinMaxFreq,1,function(vec) sort(vec)))
    if(identical(grep('rda$',pathAll00), as.integer(1))){
          load(pathAll00)
      } else if(identical(grep('rds$',pathAll00), as.integer(1))){
              MinMaxFreq <- readRDS(pathAll00)
          } else{
              MinMaxFreq <- read.table(pathAll00,header=F)
             }
       
    
    MinMaxFreq <- MinMaxFreq[row.names(MinMaxFreq)%in%snpsList,]
    
    

    
                                        # MODULE beds: from ACGT to 12
                                        # substituting A by 1; T by 2; using min/Max otherwise
    
    ## ped.f=list.files(path=pathTotpeds,pattern="*\\.bed$")[2]
    FUN.bedMedoids <- function(ped.f){
        
        print(ped.f)
        #tempOut <- paste0(ped.f,'_out10') ## temporary file names; delete later
        zx2 <- gsub("\\.tped","",ped.f)
        outname <- strsplit(zx2,'/')[[1]][length(strsplit(zx2,'/')[[1]])]
        
        #command10 <- paste0(plink,' --bfile ',zx2 , ' --recode transpose --out ',tempOut)
        ## transpose bed file; easier to get into plink
        #system(command10, wait=T)
        
        ## snps and individuals; add to the final AM  id table
        snps <- unlist(SNPsComm)
        
        inds <- read.table(paste0(zx2,'.tfam'))
        inds <- inds[,2]
        
        
        ## qc: ped is 1234, not ACGT, only run on first line, 'nline=1'
        #conQc <- read.table(paste0(tempOut,'.tped'), nrows=1)
        conQc <- read.table(ped.f, nrows=1)
        all12 <- unique(unlist(conQc)[-c(1:6)])
        #if(any(all12%in%c('1','2','3','4'))) stop("tm: recode to ACGT, it's currently 1234")
        
        ## import tped file; tried scan; read.table the fastest
        #snps1234 <- read.table(paste0(tempOut,'.tped'))
        snps1234 <- read.table(ped.f)
        #unlink(grep(tempOut,dir(),value=T))
        row.names(snps1234) <- snps1234$V2
        ## remove pedigree                                                                                            
        snps1234 <- snps1234[,!colnames(snps1234)%in%c('V1','V2','V3','V4')]
        ## matrix; much faster
        snps1234 <- as.matrix(snps1234)
    
        snps1234 <- snps1234[intersect(row.names(snps1234),row.names(MinMaxFreq)),]
        
        SortedGeno <- t(apply(snps1234,1,function(vec) paste0(sort(unique(vec)),collapse='')))
        
        
        #if(sum(SortedGeno=='CG')!=0) stop("CGs Present in .frq file, strand reading issue, remove.")
        if(sum(SortedGeno=='CG')!=0) {
            print(paste0("Found ",sum(SortedGeno=='CG')," CGs Present, strand reading issue, removing."))
            snps1234 <- snps1234[colnames(SortedGeno)[!(colnames(SortedGeno)%in%colnames(SortedGeno)[SortedGeno[1,]=='CG'])],]
            SortedGeno <- t(SortedGeno[,!(colnames(SortedGeno)%in%colnames(SortedGeno)[SortedGeno[1,]=='CG'])])
            
        }
        if(sum(SortedGeno=='AT')!=0) {
            print(paste0("Found ",sum(SortedGeno=='AT')," ATs Present, strand reading issue, removing."))
            snps1234 <- snps1234[colnames(SortedGeno)[!(colnames(SortedGeno)%in%colnames(SortedGeno)[SortedGeno[1,]=='AT'])],]
            SortedGeno <- t(SortedGeno[,!(colnames(SortedGeno)%in%colnames(SortedGeno)[SortedGeno[1,]=='AT'])])
            
        }
        if(length(grep('0',SortedGeno[1,],value=T))!=0) {
            print(paste0("Found ",length(grep('0',SortedGeno[1,],value=T))," SNPs with missingness Present, removing."))
            snps1234 <- snps1234[names(grep('0',SortedGeno[1,],value=T,invert=T)),]
            SortedGeno <- t(SortedGeno[,names(grep('0',SortedGeno[1,],value=T,invert=T))])
            
        }
        monoCGs <- SortedGeno=='C' | SortedGeno=='G'
        
        #Deals with indels
        indel <- nchar(SortedGeno)>2
        
        
        ## If any mono Gs or Cs in .frq file, 
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
                debug(FUN.sumMinMaxName)
                undebug(FUN.sumMinMaxName)
                snps1234[monoCGs,] <- t(apply(snps1234[monoCGs,],1, function(r.f) FUN.sumMinMaxName(minMax=minMax.f,indGeno=r.f)))
            }
            ## Replacing monoCG max with 2
            #snps1234 <- snps1234[,!colnames(snps1234)%in%"UNIQID"]
            snps1234[monoCGs,] <- gsub("^C$|^G$","2",snps1234[monoCGs,])
            
        } ## end of snps exist that are not A nor T 
        
        
        #Indels
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
        
        
        debug(FUN.addSnps)
        undebug(FUN.addSnps)
                                        # Sum of strands   
        SumPed <- apply(snps1234,1,FUN.addSnps)
        if(length(inds)==1) {
            SumPed <- t(data.frame(SumPed))
        }
        dimnames(SumPed)[[1]] <- inds
        ## distances of peds to lapplys
        print(paste0('***** Number of SNPs Used: ',length(intersect(snps,refsnpinter))))
        arithmInds <- lapply(medoidsList,function(medoid) FUN.distEuc(X=SumPed,y=medoid,z=snps))
        #if(length(inds)==1) {
        #    arithmInds <- t(data.frame(arithmInds))
        #}
        
        AMids <- do.call('rbind',arithmInds)
        ## report; overlaps snps for each medoid, each ped
        cat(ped.f, ';', paste0(row.names(AMids),': ',lapply(arithmInds, function(obj) attr(obj,'commonSnps'))),'\n',file=overlapNumbersFile, append=T)
        AMids <- t(AMids)
        dimnames(AMids)[[1]] <- inds
        
        ## these are the C_ raw distances
        dimnames(AMids)[[2]] <- paste0('C_',dimnames(AMids)[[2]])
        ## indexes; least similar pop is 0; max is 100
        fun.rescale <- function(vec) 100-((vec-min(vec))/(max(vec)-min(vec))*100)
        Indexes <- data.frame(t(round(apply(AMids, 1, fun.rescale),1)))
        names(Indexes) <- gsub("C_", "I_", names(Indexes))
        ## merge Indexes, Coordinates
        AMids <- data.frame(Indexes, AMids)
        AMids$UNIQID <- row.names(AMids)
        return(AMids)

    }#FUN.bedMoids; big function: treats ped, gets AMids for each arithmeticMedoid
    
    #debug(FUN.bedMedoids)
    #undebug(FUN.bedMedoids)
    
    ## log file for number of Ts As Cgs
    logFile <- 'report_ArithmeticMedoids.txt'
    unlink(logFile)
    cat('report for arithmeticMedoids\n',file=logFile)
    ## report file for number of snps for ped/medoids
    overlapNumbersFile <- 'report_overlapSnpsPedMedoids.txt'
    unlink(overlapNumbersFile)
    cat('report for arithmeticMedoids\n',file=overlapNumbersFile)
    
    ## Pull list of Plink BED files for use
    tpedFile <- list.files(path=pathTotpeds,pattern="*\\.tped$",full.names=T)
    print('tPED files used:')
    print(tpedFile)
    
    ## Get list of common SNPs for all submitted Plink BED files.
    #SNPls <- list.files(path=pathTotpeds,pattern="*\\.snplist$",full.names=T)
    SNPsComm <- lapply(SNPls,FUN.SharedSNPs)
    print(paste0(length(SNPsComm[[1]])," common SNPs found between all submitted tPED files."))
    print(paste0(length(MinMaxFreq[,1]),' common SNPs found between All00 reference and all submitted tPED files'))
    
    ## Apply main function, with or without multicore
    if(AMmcapply==F) amidsList <- lapply(tpedFile, FUN.bedMedoids)
    if(AMmcapply==T){
        if(missing(nrcores)) nrcores <- detectCores()-2
        amidsList <- mclapply(tpedFile, FUN.bedMedoids,mc.cores=nrcores)
    }
    
    amids <- do.call('rbind',amidsList)
    #h(amids,2)
    
    ## Writes out AMid file for future reference and other analyses.
    #nameOutAMid <- paste0('AMid',NameOut,'_ref',(ncol(amids)-1)/2,'_pops',(length(tpedFile)),'_inds',nrow(amids),'.amid')
    
    nameOutAMid <- paste0('AMid',NameOut,'_ref',(ncol(amids)-1)/2,'_inds',nrow(amids),'_SNPs',length(MinMaxFreq[,1]),'.amid')
    #if(NameOut==NULL) nameOutAMid <- paste0('AMid',outname,'_ref',(ncol(amids)-1)/2,'_inds',nrow(amids),'_SNPs',length(MinMaxFreq[,1]),'.amid')
    write.table(amids, row.names=F, col.names=T, quote=F, file=nameOutAMid,sep=' ')
    print(paste0('Wrote out AMid file to, ',getwd(),'/',nameOutAMid))

    ## Set wd back to original
    if(exists('initialwd')) setwd(initialwd)
    rm(intermed)
    
    return(amids)
    
}
