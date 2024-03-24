genes_add_pubmed <-
function(keyword,genepdb,pubmed,localPDB.path = paste(getwd(),"localPDB",sep="/")){
## filter the pubmed
   pubmed <- pubmed[!is.na(pubmed[,1])&(pubmed[,2]!=""),]
   Phenotype.trim = paste(pubmed[,"Phenotype"]," [PMID:",pubmed[,"PMID"],"]",sep= "")
   pubmed <- cbind(pubmed,Phenotype.trim)
   pubmed_genes <- unlist(lapply(as.character(pubmed[,"Approved.Symbol"]),function(x) unlist(strsplit(x,", "))))
   pdb_genes <- as.character(genepdb[!is.na(genepdb[,"chr"]),"Gene.symbol"])

# trim the gene names: 'ORF' -> 'orf'
    gene.orf <- function(x){
       # x = "C8ORF37"
        if(length(grep("ORF",x))>0){
          x <- paste(unlist(strsplit(x,"ORF")),collapse="orf")
          }
        return(x)    
    }
    
    hgnc <- read.delim(gzfile(paste(localPDB.path,"hgnc_complete_set.txt.gz",sep="/")))
    refFlat <- read.delim(gzfile(paste(localPDB.path,"refFlat.txt.gz",sep="/")),header= FALSE)    
    genes <- unique(c(as.character(pdb_genes), as.character(pubmed_genes)))
    genes <- genes[genes != ""]
    rm.nos <- grep("missing",genes)
    if(length(rm.nos) > 0)  genes <- genes[-rm.nos]
    genes.trim <- unique(unlist(lapply(genes,gene.orf)))
    genes_add <- setdiff(genes.trim,pdb_genes)
    refFlat.extract <- refFlat[is.element(refFlat[,1],genes.trim),]

  if(length(genes_add) > 0 ){  
    gene.position <- matrix(,nrow=length(genes_add),ncol=8)
    colnames(gene.position) <- c("Gene.symbol","chr","strand","start","end","Entrez.Gene.ID","Approved.Name","Synonyms")
    rownames(gene.position) <- gene.position[,1] <- genes_add
    for(g in genes_add){
       refFlat.extract.g <- refFlat.extract[refFlat.extract[,1] == g,]
       if(nrow(refFlat.extract.g) > 0 ){
           gene.position[g,"chr"] <- paste(unique(refFlat.extract.g[,3]),collapse=",")
           gene.position[g,"strand"] <- paste(unique(refFlat.extract.g[,4]),collapse=",")
           gene.position[g,"start"] <- min(refFlat.extract.g[,5])
           gene.position[g,"end"] <- max(refFlat.extract.g[,6])                 
       }
    }
    hgnc.extract <- hgnc[is.element(hgnc$symbol,genes_add),]
    rownames(hgnc.extract) <- hgnc.extract$symbol
    gene.position[rownames(hgnc.extract),c("Synonyms")] <- as.character(hgnc.extract[rownames(hgnc.extract),c("alias_symbol")])
    gene.position[rownames(hgnc.extract),c("Approved.Name")] <- as.character(hgnc.extract[rownames(hgnc.extract),c("name")])
    gene.position[rownames(hgnc.extract),c("Entrez.Gene.ID")] <- as.character(hgnc.extract[rownames(hgnc.extract),c("entrez_id")])
   
    geneAll <- matrix(,nrow=nrow(genepdb)+nrow(gene.position),ncol=ncol(genepdb))
    colnames(geneAll) <- colnames(genepdb)
    geneAll[1:nrow(genepdb),] <- as.matrix(genepdb[,])
    geneAll[(1+nrow(genepdb)):nrow(geneAll),1:ncol(gene.position)] <- as.matrix(gene.position) 
    }else {
       geneAll <- genepdb
  }   
    
    check.gene <- function(gene,x){
        x.split <- unlist(strsplit(as.character(x),", "))
        check.i <- intersect(gene,x.split)
        check.result <- ifelse(length(check.i)>0,TRUE,FALSE)
        return(check.result)
    }
    
    geneAll <- cbind(geneAll,"")
    colnames(geneAll)[ncol(geneAll)] <- "PubMed"
    for(i in 1:nrow(geneAll)){
       gene.i <- geneAll[i,1]  
       pheno.i <- as.character(unique(pubmed[unlist(lapply(pubmed[,"Approved.Symbol"],function(x) check.gene(gene.i,x))), "Phenotype.trim"]))
       if(length(pheno.i) > 0){
           geneAll[i,"PubMed"] <- paste(pheno.i,collapse=";")
           }else{
              geneAll[i,"PubMed"] <- ""
       }       
    }  
    
    geneAll = geneAll[!is.na(geneAll[,2]),]  
    
    ##  score for the relations
    score_relations <- function(x){
         # x <- geneAll[1,]
         score <- 0
         if(length(grep_split(keyword,x[9])) >0 )
            score <- score + 0.1
         if(length(grep_split(keyword,x[10])) >0 )
            score <- score + 0.2
         if(length(grep_split(keyword,x[11])) >0 )
            score <- score + 0.2
         if(length(grep_split(keyword,x[12])) >0 )
            score <- score + 0.2
         if(length(grep_split(keyword,x[13])) >0 )
            score <- score + 0.2
        ## check the evidences from literatures, the more evidence, the higher score.
         n_pubmed <- length(grep_split(keyword,unlist(strsplit(x[14],";")))) 
         score_pub <- ifelse(n_pubmed*0.03 < 0.1, n_pubmed*0.03,0.1)
            score <- score + score_pub
        
        return(score)
    }
    Score <- unlist(apply(geneAll,1,score_relations))
    geneAll <- cbind(geneAll, Score)
    geneAll <- geneAll[order(as.numeric(geneAll[,"Score"]),decreasing = TRUE),]
    return(geneAll)
}
