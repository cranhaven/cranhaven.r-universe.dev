#' Add individuals to the CorPheno file.
#'
#' Add individuals to the CorPheno file in order to order and plot them with specific colours. Ids that aren't in the CorPheno will be plotted under 'Unspecified' and given a grey colour by plotAMids()
#'
#' @param phenoId Path to file containing list of invididuals to be added to CorPheno with the three columns 'UNIQID','Fam','Pheno_Pop'.
#' A full example called 'Example.phenoId' is present in the 'extdata' folder of the AncestryMapper package.
#' @param phenoValues Path to file containing information on each population to be added, such as continental origin and colours as well as other information.
#' A full example called 'Example.phenoValues' is present in the 'extdata' folder of the AncestryMapper package.
#' 
#' @param ignoreDupes Logical value (TRUE or FALSE), specifying if the presence of individual IDs already in CorPheno should be ignored. Useful if the user knows this is the case and just wants the individuals not already included in the directed phenoFile.
#' Default = FALSE 
#' 
#' @param phenoFile Main file with phenotype information for each individual. A sample file called CorPheno is included with the package in the ext folder. It contains values for the samples from the HGDP. This function augments this file with any novel individuals.
#' If no value is given the sample file in the ext folder is used by default.
#' 
#' @param writeCor Logical value (TRUE or FALSE), specifying if the new CorPheno should overwrite the file the given in 'phenoFile'. A backup of the previous CorPheno file with the same path as given in 'phenoFile' with '_Original' appended to the name will also be produced. You could alternatively write out your new file to your preferred location with write.table, making sure to keep the columns tab spaced. Default = TRUE
#'
#' @examples
#' \dontrun{
#' phenoIdPth <- system.file ("extdata", "Example.phenoId", package = "AncestryMapper")
#' PhenoValPth <- system.file("extdata", "Example.phenoValues", package = "AncestryMapper")
#' Corpheno <- system.file("extdata", "CorPheno", package = "AncestryMapper")
#'
#' refAdd(phenoId = phenoIdPth, phenoValues = PhenoValPth, phenoFile = Corpheno)
#' 
#' }
#' @rdname refAdd
#' @export
#' 
refAdd <- function(phenoId, phenoValues, ignoreDupes=F, phenoFile, writeCor = T){
    
    if(missing(phenoId)) stop('phenoId is missing, needs path to phenoId file, see ?refAdd')
    if(missing(phenoValues)) stop('phenoValues is missing, needs path to phenoValues file, see ?refAdd')

    
    SamplePheno <- read.table(phenoId,header=T)
    PopPheno <- read.table(phenoValues,header=T)
    
    #if(missing(CorPheno)) CorPheno <- read.csv(system.file('extdata',"CorPheno",package="AncestryMapper"),h=T,sep='\t')
    if(missing(phenoFile)) phenoFile <- system.file('extdata',"CorPheno",package="AncestryMapper")
    CorPheno <- read.csv(phenoFile,header=T,sep='\t')
    DupeID <- SamplePheno$UNIQID[SamplePheno$UNIQID%in%CorPheno$UNIQID]
    NovelID <- SamplePheno$UNIQID[!(SamplePheno$UNIQID%in%CorPheno$UNIQID)]
    if(ignoreDupes==F){
        if(length(DupeID)>0) stop(paste0("Found ",length(DupeID)," of ",length(SamplePheno$UNIQID)," IDs already in CorPheno, if you are aware of this wish to procede please run again with argument 'ignoreDupes' set to TRUE, this will add only those IDs not already in the CorPheno"))
    }
    
    #SamplePhenoNov <- SamplePheno[!(SamplePheno$UNIQID%in%CorPheno$UNIQID),]
    SampleIDNovel <- SamplePheno[!(SamplePheno$UNIQID%in%CorPheno$UNIQID),c('Pheno_Pop','UNIQID','Fam')]
    if(nrow(SampleIDNovel)==0) stop("No novel IDs found in phenoId, all already present in CorPheno")
    
    #SamplePhenoNov$UNIQID <- NULL
    #SamplePhenoNov$Fam <- NULL
    #SamplePhenoNov <- unique(SamplePhenoNov)
    
    
    #for(z in unique(SamplePheno[SamplePheno$PhenoPop[!(SamplePheno$UNIQID%in%Corpheno$UNIQID)]){
    #for(z in seq(1,length(SamplePhenoNov$Pheno_Pop))){
    #    NovelPopEntry <- CorPheno[1,]
    #    NovelPopEntry$UNIQID <- NULL
    #    NovelPopEntry$Fam <- NULL
    #    NovelPopEntry$Pheno_Pop <- SamplePhenoNov$PhenoPop[z]
    #    NovelPopEntry$Pheno_Data <- SamplePhenoNov$Pheno_Data[z]
    #    NovelPopEntry$Pheno_Region <- SamplePhenoNov$Pheno_Region[z]
    #    NovelPopEntry$Pheno_Continental <- SamplePhenoNov$Pheno_Continental[z]
    #    NovelPopEntry$Colors_Pop <- SamplePhenoNov$Colors_Pop[z]
    #    NovelPopEntry$Colors_Region <- SamplePhenoNov$Colors_Region[z]
    #    NovelPopEntry$Colors_Continental <- SamplePhenoNov$Colors_Continental[z]
    #    NovelPopEntry$Colors_Data <- SamplePhenoNov$Colors_Data[z]
    #    NovelPopEntry$Order <- SamplePhenoNov$Order[z]
    #    if(z!=1) NovelPopAll <- rbind(NovelPopAll,NovelPopEntry)
    #    if(z==1) NovelPopAll <- NovelPopEntry
    #}
    
    #NovelPopAllIds <- merge(NovelPopAll,SampleIDNovel,by='Pheno_Pop')
    NovelPopAllIds <- merge(SampleIDNovel,PopPheno,by='Pheno_Pop')
    NovelPopAllIds <- NovelPopAllIds[,colnames(CorPheno)]
    NovelPopAllIds$Pheno_Pop <- paste0(NovelPopAllIds$Pheno_Pop,'.',NovelPopAllIds$Pheno_Data)
    if(writeCor) CorPhenoOrig <- CorPheno
    CorPheno <- rbind(CorPheno,NovelPopAllIds)
    if(writeCor){
        #write.table(CorPhenoOrig,file=paste0(system.file('extdata',"CorPheno",package="AncestryMapper"),'_Backup'),row.names=F,col.names=T,quote=F,sep='\t')
        write.table(CorPhenoOrig,file=paste0(phenoFile,"_Original"),row.names=F,col.names=T,quote=F,sep='\t')
        #write.table(CorPheno,file=system.file('extdata',"CorPheno",package="AncestryMapper"),row.names=F,col.names=T,quote=F,sep='\t')
        write.table(CorPheno,file=phenoFile,row.names=F,col.names=T,quote=F,sep='\t')
    }
    return(CorPheno)
}


#if(Pathopop%in%corPheno.df$Pheno_Pop==FALSE){
#  print(paste0(Pathopop," is not in reference file, please use refAdd('Poptoadd','PopColor','Continent') to add to reference file or check to make sure ",Pathopop," does not contain typos.")
#  print("All Current Population Phenotypes")
#  print(unique(corPheno.df$Pheno_Pop))
#  PathopopEntry <- corPheno.df[1,]
#  PathopopEntry$UNIQID <- PathoFileNam
#  PathopopEntry$Pheno_Pop <- gsub('-','_',Pathopop)
#  corPheno.df <- rbind(corPheno.df,PathopopEntry)
#}
