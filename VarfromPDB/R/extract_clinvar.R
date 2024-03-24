extract_clinvar <-
function(keyword, localPDB.path=paste(getwd(), "localPDB",sep="/"), type="both",
        HPO.disease = NULL, genelist = NULL, OMIM = NULL){
    if( !is.null(OMIM)) {    
        morbidmap=paste(localPDB.path,"morbidmap.txt",sep="/")
        morbidmap <- read.delim(morbidmap,comment.char = "#")           
        colnames(morbidmap) <- c("disease","gene","gene.mim.no","location")
    }
            
    if(file.exists(localPDB.path)){
         if(file.exists(paste(localPDB.path,"variant_summary.txt.gz",sep="/"))){
             clinvar <- paste(localPDB.path,"variant_summary.txt.gz",sep="/")
             }else{
                 clinvar <- NULL
         }        

         if(file.exists(paste(localPDB.path,"gene_condition_source_id",sep="/"))){
             gene2dis <- paste(localPDB.path,"gene_condition_source_id",sep="/")
             }else{
                 gene2dis <- NULL
         }        


        }else{
             clinvar <- NULL
             gene2dis <- NULL
    }     

##check clinvar database, download the files if missing
    if(is.null(clinvar)){
       clinvar <- "ftp://ftp.ncbi.nlm.nih.gov/pub/clinvar/tab_delimited/variant_summary.txt.gz"
       download.path <- paste(getwd(),"localPDB",sep="/")
       clinvar.local <- paste(download.path,"variant_summary.txt.gz",sep="/")
       if(!file.exists(download.path))
          dir.create(download.path )
       options(timeout = 300)
       if( !file.exists(clinvar.local))
           curl_download(clinvar, clinvar.local)
       clinvar <- paste(download.path,"variant_summary.txt.gz",sep="/")
    }

    if(is.null(gene2dis)){
       gene2dis <- "ftp://ftp.ncbi.nlm.nih.gov/pub/clinvar/gene_condition_source_id"
       download.path <- paste(getwd(),"localPDB",sep="/")
       gene2dis.local <- paste(download.path,"gene_condition_source_id",sep="/")
       if(!file.exists(download.path))
           dir.create(download.path )
       options(timeout = 300)
       if(!file.exists(gene2dis.local) )
           curl_download(gene2dis,gene2dis.local)
       gene2dis <- paste(download.path,"gene_condition_source_id",sep="/")
    }

## input the summary file
    if(substr(clinvar,nchar(clinvar)-1,nchar(clinvar)) == "gz"){
        clinvar <- read.delim(gzfile(clinvar))
        }else{
            clinvar <- read.delim(clinvar)
    }       

    gene2dis <- read.delim(gene2dis)
       

## HPO, the phynotypes maybe have diffrent names in HPO
       if(is.null(HPO.disease)){
          HPO.disease.check <- pheno_extract_HPO(keyword= keyword)
          HPO.disease <- as.character(unique(HPO.disease.check[grep("OMIM",HPO.disease.check[,1]),1]))
       }

##----------------------------------------------
##begin to search 
       if(!is.null(keyword)){      
          gene2dis.d <- gene2dis[grep_split(keyword,gene2dis[,"DiseaseName"]),]
          pheno.yes <- as.character(gene2dis.d[,"DiseaseName"])
          }else if((is.null(keyword)& !is.null(HPO.disease)) | (is.null(keyword) & !is.null(genelist))){
            gene2dis.d <- c()
            pheno.yes <- c()  
            }else{
              gene2dis.d <- gene2dis
              pheno.yes <- c()
       }           
     
       if(!is.null(HPO.disease)){
          HPO.disease.no <- unlist(lapply(HPO.disease,function(x) unlist(strsplit(x,"OMIM:"))[2]))
          gene2dis.d2 <- gene2dis[is.element(gene2dis[,"DiseaseMIM"],HPO.disease.no),]
          pheno.yes2 <- as.character(gene2dis.d2[,"DiseaseName"])
          gene2dis.d <- rbind(gene2dis.d,gene2dis.d2)
          pheno.yes <- union(pheno.yes,pheno.yes2)
       }
   
   #for a given genelist
       if(!is.null(genelist)){
          gene2dis.d3 <- gene2dis[is.element(gene2dis[,"AssociatedGenes"],genelist),]
          gene2dis.d <- rbind(gene2dis.d,gene2dis.d3)
       }

      gene2dis.extr <- unique(gene2dis.d)   
      if(nrow(gene2dis.extr) > 0){
         gene2dis.extr$pheno.check <- "no"
         gene2dis.extr[is.element(gene2dis.extr$DiseaseName,pheno.yes),"pheno.check"] <-  "yes"
              
         genes <- unique(as.character(gene2dis.extr[,2]))    
        
   ##extract the variants in the genes
        clinvar.extr <- clinvar[is.element(clinvar[,"GeneSymbol"],genes),]
        
   ## extract the variants from summary file directly
         clinvar.d <- clinvar[grep_split(keyword,clinvar[,"PhenotypeList"]),]
        
   ## merge the variants from ClinVar and other databases
        clinvar.extr <- unique(rbind(clinvar.extr,clinvar.d))
        }else{
           clinvar.extr = NULL
      }
   
     extract <- list(gene2dis.extr,clinvar.extr)
     names(extract) <- c("gene2dis","variants")
     return(extract)
}
