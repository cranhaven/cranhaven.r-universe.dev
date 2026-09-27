## parseGenotypes -- Steven J. Mack April 10, 2020
## v1.00
## Accepts and converts 2-column/locus BIGDAWG/PyPop-formatted genotype data to the GL String format expected by LDWrap

#' Reformat columnnar genotype data to GL String format
#' 
#' This function accepts genotype data organized in locus-column pairs, and returns GL String-formatted data structured for LDWrap(). Of the resulting multilocus haplotype pair, the first haplotype is constructed from the first column for each locus, and the second haplotype is constructed from the second column.
#' @param dataset A tab-delimited text file (with a .txt or .tsv filename suffix) with a header row or a data frame. Each row corresponds to a subject, with two columns per locus. Allele names can include a locus name (e.g., locus*allele) or can can exclude the locus, but all allele names in the dataset must either include or exclude the locus. Missing (untyped) allele data can be identified with an empty cell or a set of four asterisks in files, and with NA values in data frames. Column names for each locus pair must be adjacent, but can be either identical (e.g., "locus" and "locus"), or suffixed (e.g., "locus_1" and "locus_2", where "locus_1" always precedes "locus_2"). A optional column of sample identifiers can be included, but must be named "SampleID".  A column named "Disease" can be included, but will be ignored. No other non-locus columns are permitted.
#' @note This function is for internal POULD use only.
#' @return A data frame of two columns. The "Relation" column includes sample identifiers if provided, or numbers from 1 to the number of subjects. The "GL String" column contains the GL String formatted genotypes.
#' @keywords LDformat reformat GL String
#' @export
#' @examples #

parseGenotypes <- function(dataset) {
  
  if(missing(dataset)) {return(cat("Please provide a value for the dataset parameter.\n"))}
 
  if(!is.data.frame(dataset)) { dataset <- read.table(dataset,header=T,sep="\t",colClasses = "character",stringsAsFactors = FALSE,as.is = TRUE,check.names = FALSE,na.strings = "****")}
  
  colnames(dataset) <- toupper(colnames(dataset))
  
  if("SAMPLEID" %in% colnames(dataset)) {
    ids <- dataset$SAMPLEID
    dataset <- dataset[,!colnames(dataset) %in% "SAMPLEID"] } else {
      ids <- 1:nrow(dataset) }
  
  if("DISEASE" %in% colnames(dataset)) {
      dataset <- dataset[,!colnames(dataset) %in% "DISEASE"] }
  
  if(ncol(dataset) %% 2 !=0 ) {return(cat("Odd number of locus columns (",ncol(dataset),"). Please review your dataset.\n",sep=""))}
  
  colnames(dataset) <- sub("\\_\\d","",colnames(dataset))

  if(ncol(dataset) == 2) {return(cat("This dataset contains data for a single locus (",colnames(dataset)[1],"). LD analysis requires two loci.\n",sep=""))}
  
  if(!any(grepl("*",dataset,fixed=TRUE))) {dataset[] <- Map(paste,names(dataset),dataset,sep="*")}
  
  # V0.3 remove NAs that become locus*NA
  blanks <- paste(colnames(dataset),NA,sep="*")
  for(i in 1:ncol(dataset)) { 
                  if(nrow(dataset[dataset[,i] == blanks[i],][i]) != 0 ) {
                                dataset[dataset[,i] == blanks[i],][i] <- NA }
              }
  
  hap <-vector("list",2)   # paste together haplotypes & clean up stragglers
  for(x in FALSE:TRUE) { hap[[((1*x)+1)]] <- apply(dataset[,rep(c(TRUE,FALSE),(ncol(dataset)/2))==x],1,paste,collapse="~") 
                         hap[[((1*x)+1)]] <- gsub("[N][A]","",hap[[((1*x)+1)]]) # eliminate all 'NA' from missing data cells
                         hap[[((1*x)+1)]] <- gsub("~+","~",hap[[((1*x)+1)]])  # eliminate tilde-runs for empty cells
                         hap[[((1*x)+1)]][substr(hap[[((1*x)+1)]],1,1)=="~"] <- substr(hap[[((1*x)+1)]][substr(hap[[((1*x)+1)]],1,1)=="~"],2,nchar(hap[[((1*x)+1)]][substr(hap[[((1*x)+1)]],1,1)=="~"])) ## trim leading tilde
                         hap[[((1*x)+1)]][substr(hap[[((1*x)+1)]],(nchar(hap[[((1*x)+1)]])),nchar(hap[[((1*x)+1)]]))=="~"] <- substr(hap[[((1*x)+1)]][substr(hap[[((1*x)+1)]],(nchar(hap[[((1*x)+1)]])),nchar(hap[[((1*x)+1)]]))=="~"],1,nchar(hap[[((1*x)+1)]])-1) ## trim trailing tilde
                         } 
  fdataset <- cbind(as.data.frame(ids,stringsAsFactors = FALSE),as.data.frame(paste(hap[[2]],hap[[1]],sep="+"),stringsAsFactors = FALSE))
  colnames(fdataset) <- c("Relation","Gl.String")
  
  fdataset
  }
  