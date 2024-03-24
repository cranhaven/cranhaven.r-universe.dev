## Function: Localize the public databases including HPO, MedGen, GeneReview, HGNC, Orphanet, ClinVar and Uniprot.

localPDB <-
function(localPDB.path = paste(getwd(),"localPDB",sep="/"),PDB="all", omim.url = NULL, 
          download.method = "curl_fetch_disk"){
    download.path <- localPDB.path
    if(!file.exists(download.path))
          dir.create(download.path )
    options(timeout = 200)
    if(PDB == "all"){
      HPO <- "yes"; Orphanet <- "yes"; HGNC <- "yes"; ClinVar <- "yes"; Uniprot <- "yes"
      }else if(toupper(PDB) == "HPO"){
          HPO <- "yes"; Orphanet <- "no"; HGNC <- "no"; ClinVar <- "no"; Uniprot <- "no"
      }else if(toupper(PDB) == "ORPHANET"){
          HPO <- "no"; Orphanet <- "yes"; HGNC <- "no"; ClinVar <- "no"; Uniprot <- "no"
      }else if(toupper(PDB) == "HGNC"){
          HPO <- "no"; Orphanet <- "no"; HGNC <- "yes"; ClinVar <- "no"; Uniprot <- "no"
      }else if(toupper(PDB) == "CLINVAR"){
          HPO <- "no"; Orphanet <- "no"; HGNC <- "no"; ClinVar <- "yes"; Uniprot <- "no"
      }else if(toupper(PDB) == "UNIPROT"){
          HPO <- "no"; Orphanet <- "no"; HGNC <- "no"; ClinVar <- "no"; Uniprot <- "yes"
      }      
      
      
## function: file to disk
file2disk <- function(url,destfile){
       if( !file.exists(destfile)|(file.exists(destfile) & file.size(destfile) == 0)){
          if(download.method == "curl_fetch_disk"){
              curl_fetch_disk(url,destfile)
              }else if(download.method == "curl_download"){
                  curl_download(url,destfile)
                  }else {
                     download.file(refFlat,refFlat.local,method = "auto", mode = "a")
          }                   
       }
}

## download the file 'refFlat.txt.gz' from ucsc database    
       refFlat <- "http://hgdownload.soe.ucsc.edu/goldenPath/hg19/database/refFlat.txt.gz"
       refFlat.local <- paste(download.path,"refFlat.txt.gz",sep="/")
        for(i in 1:5){
           file2disk(refFlat,refFlat.local)
           if(file.exists(refFlat.local) & file.size(refFlat.local) > 0) break
        }
       if(file.size(refFlat.local) == 0)
           print("Warning: The network is not satisfying! Please try to run localPDB() again.")
       
## download the file 'morbidmap.txt' from OMIM database    
   if(is.null(omim.url)){ 
   #   print("Warning: please make sure you have localized the OMIM file morbidmap!if NOT, you should apply for an OMIM account and get the URL from http://omim.org/downloads.However, you can go on the process without the OMIM account, the final compiled genes maybe imcomplete!")
      }else if(!is.null(omim.url)){
        morbidmap <- omim.url
        morbidmap.local <- paste(download.path,"morbidmap.txt",sep="/")
        for(i in 1:5){
           file2disk(morbidmap,morbidmap.local)
           if(file.exists(morbidmap.local) & file.size(morbidmap.local) > 0) break
        }
       if(file.size(morbidmap.local) == 0)
           print("Warning: The network is not satisfying! Please try to run localPDB() again.")       
   }
   
## download the necessary files from HPO database    
   if(HPO == "yes" | toupper(PDB) == "HPO"){
       HPO <- "http://compbio.charite.de/hudson/job/hpo.annotations/lastStableBuild/artifact/misc/phenotype_annotation.tab"
       diseases_to_genes <- "http://compbio.charite.de/jenkins/job/hpo.annotations.monthly/lastStableBuild/artifact/annotation/diseases_to_genes.txt"
       HPO.local <- paste(download.path,"phenotype_annotation.tab",sep="/")
       diseases_to_genes.local <- paste(download.path,"diseases_to_genes.txt",sep="/")
   #     file2disk(HPO,HPO.local)
   #     file2disk(diseases_to_genes,diseases_to_genes.local)
        for(i in 1:5){
           file2disk(HPO,HPO.local)
           if(file.exists(HPO.local) & file.size(HPO.local) > 0) break
        }
       if(file.size(HPO.local) == 0)
           print("Warning: The network is not satisfying! Please try to run localPDB() again.")
        
        for(i in 1:5){
           file2disk(diseases_to_genes,diseases_to_genes.local)
           if(file.exists(diseases_to_genes.local) & file.size(diseases_to_genes.local) > 0) break
        }
       if(file.size(diseases_to_genes.local) == 0)
           print("Warning: The network is not satisfying! Please try to run localPDB() again.")
   }
      
## download the necessary files from orphanet database    
   if(Orphanet == "yes" | toupper(PDB) == "ORPHANET"){
       orphanet <- "http://www.orphadata.org/data/xml/en_product6.xml"
       orphanet.local <- paste(download.path,"en_product6.xml",sep="/")
        for(i in 1:5){
           file2disk(orphanet,orphanet.local)
           if(file.exists(orphanet.local) & file.size(orphanet.local) > 0) break
        }
       if(file.size(orphanet.local) == 0)
           print("Warning: The network is not satisfying! Please try to run localPDB() again.")
   }
      
## download the necessary files from HGNC database    
   if(HGNC == "yes" | toupper(PDB) == "HGNC"){
       hgnc <- "ftp://ftp.ebi.ac.uk/pub/databases/genenames/hgnc_complete_set.txt.gz"
#       hgnc <- "ftp://ftp.ebi.ac.uk/pub/databases/genenames/new/tsv/hgnc_complete_set.txt"
       hgnc.local <- paste(download.path,"hgnc_complete_set.txt.gz",sep="/")
        for(i in 1:5){
           file2disk(hgnc,hgnc.local)
           if(file.exists(hgnc.local) & file.size(hgnc.local) > 0) break
        }
       if(file.size(hgnc.local) == 0)
           print("Warning: The network is not satisfying! Please try to run localPDB() again.")
   }
    
## download the necessary files from clinvar database    
   if(ClinVar == "yes" | toupper(PDB) == "CLINVAR"){
       clinvar <- "ftp://ftp.ncbi.nlm.nih.gov/pub/clinvar/tab_delimited/variant_summary.txt.gz"
       clinvar.local <- paste(download.path,"variant_summary.txt.gz",sep="/")
        for(i in 1:5){
           file2disk(clinvar,clinvar.local)
           if(file.exists(clinvar.local) & file.size(clinvar.local) > 0) break
        }
       if(file.size(clinvar.local) == 0)
           print("Warning: The network is not satisfying! Please try to run localPDB() again.")
       gene2dis <- "ftp://ftp.ncbi.nlm.nih.gov/pub/clinvar/gene_condition_source_id"
       gene2dis.local <- paste(download.path,"gene_condition_source_id",sep="/")
        for(i in 1:5){
           file2disk(gene2dis,gene2dis.local)
           if(file.exists(gene2dis.local) & file.size(gene2dis.local) > 0) break
        }
       if(file.size(gene2dis.local) == 0)
           print("Warning: The network is not satisfying! Please try to run localPDB() again.")
   }
   
## download the necessary files from Uniprot database    
   if(Uniprot == "yes" | toupper(PDB) == "UNIPROT"){
       uniprot <- "http://www.uniprot.org/docs/humsavar.txt"
       uniprot.local <- paste(download.path,"humsavar.txt",sep="/")
   #    file2disk(uniprot,uniprot.local)
        for(i in 1:5){
           file2disk(uniprot,uniprot.local)
           if(file.exists(uniprot.local) & file.size(uniprot.local) > 0) break
        }
       if(file.size(uniprot.local) == 0)
           print("Warning: The network is not satisfying! Please try to run localPDB() again.")
   }
   
   print(paste("Congratulations! Public databases have been localized in  ",localPDB.path,".",sep=""))
}
