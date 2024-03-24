variants_compile <-
function(omim = NULL,clinvar,uniprot,localPDB.path = paste(getwd(),"localPDB",sep="/")){
   
   ## function, extract the gene and p.change
   p.change <- function(x){
      # x = "NM_001173990.2(TMEM216):c.218G>T (p.Arg73Leu)"
        x.split = unlist(strsplit(x,":|\\(|\\)"))
        if( length(x.split) == 5){
           x.trim = paste(x.split[1], x.split[5],sep=":")
           }else{
              x.trim = x
        } 
        return(x.trim)
   }
   
    clinvarDB <- read.delim(gzfile(paste(localPDB.path,"variant_summary.txt.gz",sep="/"))) 
    clinvar.add <- uniprot.add <- c()
    
## compile omim variants into clinvar 
    if( !is.null(omim)){
       var.omim <- setdiff(omim[,"clinvarAccessions"],clinvar$RCVaccession) 
       omim.add <- c()   
        
       omim.rcvacces <- unlist(lapply(omim[,"clinvarAccessions"],function(x) unique(unlist(strsplit(x,";")))))
       clinvar.rcvacces <- unlist(lapply(clinvar[,"RCVaccession"],function(x) unique(unlist(strsplit(as.character(x),";")))))
       var.omim <- setdiff(omim.rcvacces,clinvar.rcvacces)
       var.omim.inclinvar <- var.omim[var.omim != "" & !is.na(var.omim)]
       for(i in var.omim.inclinvar){
          clinvarDB.i <- clinvarDB[grep(i,clinvarDB$RCVaccession),]
          clinvar.add <- rbind(clinvar.add,clinvarDB.i)
       }
       clinvar.add <- unique(clinvar.add)
       omim.add <- omim[omim[,"clinvarAccessions"] == "" | is.na(omim[,"clinvarAccessions"]),]
       if(is.matrix(omim.add)) {
         omim.add <- omim.add[omim.add[,"status"] == "live",]
         }else{
            omim.add <- omim.add[omim.add["status"] == "live"]
       }     
    }
      
## compile uniprot variants into clinvar
    var.uniprot <- paste(uniprot[,1],unlist(lapply(uniprot[,4],str_trim)),sep=":")
    var.clinvar <- unlist(lapply(as.character(clinvar[,"Name"]),  p.change))
    var.uniprot.1 <- setdiff(var.uniprot,var.clinvar)
    uniprot.add <- uniprot[is.element(var.uniprot,var.uniprot.1),]

## add the additional variants into clinvar, to get the final variant set.  
    var2pheno <- clinvar
    var2pheno$Mutation.add <- "" 

## add the variants from OMIM        
    if( !is.null(omim)){
      if(is.matrix(omim.add)) {
           nrow.omim.add <- nrow(omim.add)
           }else{
              if(length(omim.add) == 0){
                 nrow.omim.add <- 0
                 }else{
                   nrow.omim.add <- 1
              }     
      }     
    }
    
## add the variants from UniProt        
    if(is.data.frame(uniprot.add)) {
         nrow.uniprot.add <- nrow(uniprot.add)
         }else{
            if(is.null(uniprot.add)){
               nrow.uniprot.add <- 0
               }else{
                 nrow.uniprot.add <- 1
            }
         }         
     
    
    if( !is.null(omim)){
        var.add <- matrix(,nrow= sum(nrow.omim.add, nrow(clinvar.add), nrow.uniprot.add),ncol=ncol(var2pheno))
        }else{
            if(sum(nrow(clinvar.add), nrow.uniprot.add) == 0){
                   var.add = NULL
               }else{
                 var.add <- matrix(,nrow= sum(nrow(clinvar.add), nrow.uniprot.add),ncol=ncol(var2pheno))        
            }
    }
        
    colnames(var.add) <- colnames(var2pheno)
    if(!is.null(clinvar.add)) { 
        if( nrow(clinvar.add) > 0)
        var.add[1:nrow(clinvar.add),1:ncol(clinvar.add)] <- as.matrix(clinvar.add)
    }
        
    if( !is.null(omim)){
         if(nrow.omim.add > 1 ) {
             var.add[sum(nrow(clinvar.add),1):sum(nrow(clinvar.add),nrow.omim.add),c("GeneSymbol","Chromosome","Cytogenetic","PhenotypeList","OtherIDs","Mutation.add","RS...dbSNP.")] <- 
                      as.matrix(omim.add[,c("Approved.Symbol","Chromosome","cytoLocation","Phenotype","variants.ID","mutations","dbSNPs")])
             }else if(nrow.omim.add == 1){
                  var.add[sum(nrow(clinvar.add),1):sum(nrow(clinvar.add),nrow.omim.add),c("GeneSymbol","Chromosome","Cytogenetic","PhenotypeList","OtherIDs","Mutation.add","RS...dbSNP.")] <- 
                        as.matrix(omim.add[c("Approved.Symbol","Chromosome","cytoLocation","Phenotype","variants.ID","mutations","dbSNPs")])        
         }    
    }
                     
    if(nrow.uniprot.add > 1) { 
       if( !is.null(omim)){
           var.add[sum(nrow(clinvar.add),nrow.omim.add,1):nrow(var.add),c("GeneSymbol","PhenotypeList","Name","RS...dbSNP.","ClinicalSignificance")] <- 
                 as.matrix(uniprot.add[,c("GeneSymbol","DiseaseName","AA.change","dbSNP","type")])
           }else{
                 var.add[sum(nrow(clinvar.add),1):nrow(var.add),c("GeneSymbol","PhenotypeList","Name","RS...dbSNP.","ClinicalSignificance")] <- 
                     as.matrix(uniprot.add[,c("GeneSymbol","DiseaseName","AA.change","dbSNP","type")])           
           }      
         }else if(nrow.uniprot.add == 1) { 
           if( !is.null(omim)){
                 var.add[sum(nrow(clinvar.add),nrow.omim.add,1):nrow(var.add),c("GeneSymbol","PhenotypeList","Name","RS...dbSNP.","ClinicalSignificance")] <- 
                    as.matrix(uniprot.add[c("GeneSymbol","DiseaseName","AA.change","dbSNP","type")])
                 }else{
                    var.add[sum(nrow(clinvar.add),1):nrow(var.add),c("GeneSymbol","PhenotypeList","Name","RS...dbSNP.","ClinicalSignificance")] <- 
                        as.matrix(uniprot.add[c("GeneSymbol","DiseaseName","AA.change","dbSNP","type")])                 
                 }    
    }
    var2pheno <- rbind(var2pheno,var.add)
    if(as.character(var2pheno)[1]=="") var2pheno == NULL
    return(var2pheno)
}
