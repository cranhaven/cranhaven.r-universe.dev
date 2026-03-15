### Caluclating LD for 17th WS Family Data
### April 6, 2020 v1.0 -- Steven J. Mack
##-----------------------------------------------------------------------------------------------------------------------------------------##
## Wrapper for parsing 17th WS Family Data files with phased Haplotype data
## Parameters:
## famData: A CSV formatted file that contains the two columns: "Gl String" and "Relation"
##    Other columns can be included (in any order), but will not impact the analysis
##    The Relation column can contain any data; anything other than "Relation=child" will be included in the analysis
##    The Gl String column should consist of two tilde (~) delimited haplotypes conneced by a plus (+) sign
##    The name of this file will also be used to name the LD result files.
## threshold: an integer that specifies the minimnum number of subjects allowed for the analysis of a locus-pair
## phased: a boolean that determines if the LD calculations should be performed for phased data (TRUE) or unphased data (FALSE) (using EM estimation)

#' Parser for CSV-formatted GL String Haplotype Data 
#' 
#' A wrapper for parsing phased haplotype data recorded in GL String format. Extracts all pairs of loci from GL String formatted haplotypes or column formatted genotypes, passes paired-genotype data to the cALD function, and generates files consumed by the LD.sign.test() and LD.heat.map() functions.
#' 
#' This function coerces cALD() to generate a haplotype vector file for each locus pair analyzed, and generates a single LD results file containing LD values for all locus pairs, along with the number of haplotypes tested, one locus pair per row. The LD results file will contain six columns ("Loc1~Loc2","D'","Wn","WLoc1/Loc2","WLoc2/Loc1","N_Haplotypes"), and will be named "<filename prefix>_<Phased/Unphased>_LD_results.csv".
#' @param famData A data frame or CSV formatted file (with a .csv filename suffix) that contains the two columns named "Gl String" and "Relation". Other columns can be included (in any order), but will not impact the analysis. The Relation column can contain any data; however anything other than "Relation=child" will be included in the LD analyses. The Gl String column should consist of two tilde (~) delimited haplotypes conneced by a plus (+) sign (GL String format). Allele names should be recorded using the LOCUS*VARIANT structure used for HLA and KIR alleles. A locus prefix (e.g., 'HLA-') is not required, but if a locus prefix is included, all allele names must include the same locus prefix. Alternatively, LDWrap() will consume genotype data in a data frame or headered tab-delimited text file (TXT or TSV), with two columns per locus. See the parseGenotype() documentation for additional requirements. The name of the file provided will serve as the basis for the name of the LD result files.
#' @param threshold An integer that specifies the minimnum number of subjects allowed for the analysis of a locus-pair. The default value is 10. If the number of subjects with haplotypes for a locus pair is less than the threshold, the *_LD_results.csv file will contain 'Not Calculated' 'Subject Threshold=##' 'Complete subjects=#' '.' in columns 2-5 for that locus pair, where ## is the set threshold and # is the number of subjects. Column 6 will be empty.
#' @param phased A boolean that determines if the LD calculations should be performed for phased data (TRUE) or unphased data (FALSE). If phased=FALSE, the EM algorithm is used to estimate haplotypes for the data in the Gl String column of family haplotype datasets.
#' @param frameName A descriptor for the data frame of family data provided. The default value is "hla-family-data". This value is not used if a CSV file is provided. 
#' @param trunc An integer that specifies the number of fields to which colon-delimited allele names in famdData should be truncated. The default value of 0 indicates no truncation. A value higher than the number of fields in the supplied allele data will result in no truncation. When a positive value of trunc is provided, the names of the output files will include the specified truncation level.  
#' @param writeTo The directory into which the LDWrap() output files should be written. The default is the directory specified by tempdir(). 
#' @keywords ldwrap ldwrapper wrapper
#' @note When at least one locus in a locus pair is monomorphic, no LD calculations will be performed, and column 5 of the results for that locus pair will identify the monomorphic loci.
#' @note This function does not validate HLA allele names. Unusual allele names (e.g., `HLA-A*NULL`, `HLA-DRB1*NoMatch`, `HLA-DPB1*NT`) and truncated versions of allele names (e.g., `HLA-A*01`, `HLA-A*01:01`, `HLA-A*01:01:01`, etc.) will be analyzed as distinct alleles. Including unusual allele names or different truncated versions of the same allele name in a dataset will likely skew the analytic results. In the latter case, the trunc parameter can be used to specify analysis at a specific number of fields.
#' @note Column-formatted genotype data are generally unphased; unless genotype data have been structured so that all alleles in the first column for each locus are in one haplotype, and all of the alleles in the second column in each locus are in the other haplotype, phased should be set to FALSE for column-formatted genotype datasets.
#' @export
#' @examples 
#' # Analyze the first 10 rows of the drb1.dqb1.demo genotype dataset.
#' LDWrap(drb1.dqb1.demo[1:10,],frameName="DRDQDemo")
#' # Analyze the includeed example genotype data with all alleles truncated to one field.
#' LDWrap(drb1.dqb1.demo[1:10,],frameName="DRDQDemoTrunc",trunc=1)
#' @references Osoegawa et al. Hum Immunol. 2019;80(9):633 (https://doi.org/10.1016/j.humimm.2019.01.010)
#' @references Osoegawa et al. Hum Immunol. 2019;80(9):644 (https://doi.org/10.1016/j.humimm.2019.05.018)

LDWrap <- function(famData,threshold=10,phased=TRUE,frameName="hla-family-data",trunc=0,writeTo=tempdir()){
  #library(haplo.stats)
  #library(gap)
  if(missing(famData)) {return(warning("Please provide a value for the famData parameter."))}
  
  dataFile <- TRUE

  if(is.data.frame(famData)) {
      dataFile <- FALSE
      famTab <- famData
      famData <- paste(frameName,"csv",sep=".")
  } else {
          suffix <- tolower(substr(famData,nchar(famData)-2,nchar(famData)))
          if(suffix == "csv") {famTab <- read.table(famData,header=TRUE,sep=",",colClasses = "character",stringsAsFactors = FALSE)}
          if(suffix %in% c("txt","tsv")) {famTab <- read.table(famData,header=TRUE,sep="\t",colClasses = "character", stringsAsFactors = FALSE,na.strings = "****",as.is = TRUE,check.names = FALSE)
                              famData <- gsub(".txt",".csv",famData,fixed=TRUE)       }
          if(!exists("famTab")) {return(warning("The file name",famData,"does not have a .csv or .txt suffix.\nPlease append .csv for comma-separated-values files, and .txt for tab-delimited-text files.\n"))}
          }
  
  doCalc <- FALSE ## v0.5.0 parsing famTab to determine what to do with it
  headers <- colnames(famTab)
                  ## Check to see if the family data table has the right columns
    if ("Relation" %in% headers && "Gl.String" %in% headers) { 
        doCalc <- TRUE
        } else { 
                 #if("disease" %in% headers) { ## removing the BIGDAWG disease column
                 # famTab <- famTab[,!headers %in% "disease"] }
                  famTab <- parseGenotypes(famTab) ## cross your fingers here
                  if(!is.null(famTab)) { doCalc <- TRUE }
                  }
  
  if(doCalc) {
  ## Extract the phased parental haplotypes fom the family data table
  parentalHaps <- famTab[famTab$Relation!="child",]
  haps <- read.table(text=parentalHaps$Gl.String,sep="+",colClasses = "character")
  
  ##v0.3 incorporating truncation, and notation in output files
  if(trunc > 0) {haps <- trimAlleles(haps,trunc)
      famData <- gsub(".csv",paste("_",trunc,"-field.csv",sep=""),famData,fixed=TRUE)
    }

  ## v0.2 incorporating extractLoci()
  locDetails <- extractLoci(haps) # v0.2
  locNames <- locDetails$loci # v0.2
  locPrefix <- locDetails$prefix # v0.2
  
  nSamp <- nrow(haps)

  nLoc <- length(locNames)/2 # v0.2
  if(threshold<1){threshold <- 1} # in case someone sets threshold to 0 or less.
  
  masterTab <- data.frame(matrix(NA, nrow = nSamp, ncol = length(locNames))) # v0.2
  reportTab <- data.frame(matrix(NA,nrow=(nLoc*(nLoc-1))/2,ncol=6))
  tabRow = 1
  
  colnames(masterTab) <- locNames

  # Split the individual haps into their respective loci
  for(i in 1:nSamp) {
    
    hap1 <- read.table(text=haps$V1[i],sep="~",colClasses="character")
    hap2 <- read.table(text=haps$V2[i],sep="~",colClasses="character")
    
    nLoc1 <- ncol(hap1)
    nLoc2 <- ncol(hap2)
    
    # Assign each element of the GL string haplotype to the correct location in the masterTable
    for(j in 1:nLoc1) {
      
      currLoc <- substr(hap1[j],regexpr("-",text = hap1[j],fixed=TRUE)[1]+1,regexpr("*",text = hap1[j],fixed=TRUE)[1]-1)
      ## remove the locux prefixes
      masterTab[i,names(masterTab)==currLoc] <- gsub(paste(locPrefix,currLoc,"*",sep=""),"",hap1[j],fixed=TRUE) # v0.2
    }
    # Have to do this separately, because of structural variation in the DRB3/4/5 loci may mean uneven #s of haplotypes in each string
    for(j in 1:nLoc2) {
      currLoc <- substr(hap2[j],regexpr("-",text = hap2[j],fixed=TRUE)[1]+1,regexpr("*",text = hap2[j],fixed=TRUE)[1]-1)

      masterTab[i,names(masterTab)==paste(currLoc,"1",sep="_")] <- gsub(paste(locPrefix,currLoc,"*",sep=""),"",hap2[j],fixed=TRUE) # v0.2
    }
  }

  for(i in 1:(nLoc-1)){
    
    for(j in (i+1):nLoc){
      
      ##Locus i | Locus i_1 | Locus j | Locus j_1
      currPair <- as.data.frame(cbind(masterTab[,(2*i)-1],masterTab[,(2*i)],masterTab[,(2*j)-1],masterTab[,(2*j)]))
      colnames(currPair) <- c(colnames(masterTab[(2*i)-1]),colnames(masterTab[(2*i)-1]),colnames(masterTab[(2*j)-1]),colnames(masterTab[(2*j)-1]))
      rowChoice <- as.data.frame(rowSums(!is.na(currPair)))
      
      reportTab[tabRow,1] <- paste(colnames(masterTab[(2*i)-1]),colnames(masterTab[(2*j)-1]),sep="~")
      
      if(nrow(currPair[rowChoice==4,]) < threshold) {
        reportTab[tabRow,2:6] <- c("Not Calculated",paste("Subject Threshold",threshold,sep="="),paste("Complete subjects",nrow(currPair[rowChoice==4,]),sep="="),".","")
       } else {
        if(nrow(unique(rbind(currPair[1],currPair[2]))) == 1 || nrow(unique(rbind(currPair[3],currPair[4]))) == 1) {
          reportTab[tabRow,2:6] <- c("Not Calculated",paste("Subject Threshold",threshold,sep="="),paste("Complete subjects",nrow(currPair[rowChoice==4,]),sep="="),paste(if(nrow(unique(rbind(currPair[1],currPair[2]))) ==1){paste(colnames(currPair[1]),"is monomorphic.",sep=" ")}else{""},if(nrow(unique(rbind(currPair[3],currPair[4]))) == 1){paste(colnames(currPair[3]),"is monomorphic.",sep=" ")}else{""},sep=" "),"")
        } else {
          reportTab[tabRow,2:6] <- cALD(dataSet = currPair[rowChoice==4,], inPhase = phased,verbose = FALSE,saveVector = TRUE,vectorPrefix = sub(".csv","",x = basename(famData),fixed=TRUE),vecDir=writeTo) 
        }
      }
      
      tabRow <- tabRow + 1
      
    }
    
  }
  colnames(reportTab) <- c("Loc1~Loc2","D'","Wn","WLoc1/Loc2","WLoc2/Loc1","N_Haplotypes")
  phaseStat <- "Phased"
  if(phased == FALSE) {phaseStat <- "Unphased"}
  
  write.table(reportTab,paste(writeTo,sub(".csv",paste("_",phaseStat,"_LD_results",".csv",sep=""),x = basename(famData),fixed=TRUE),sep=.Platform$file.sep),append = FALSE,sep = ",",row.names = FALSE,quote=FALSE,col.names = TRUE)

  message("LD Analysis Complete")
  } else {if(!is.null(famTab)) { warning(paste("LD Analysis Halted: Your ",if(dataFile){"file"}else{"data frame"}," does not contain the proper columns. ",if(!"Relation" %in% headers && !"Gl.String" %in% headers) {"The 'Relation' and 'Gl String' columns are missing."} else {if(!"Gl.String" %in% headers) {"The 'Gl String' column is missing."} else {"The 'Relation' column is missing."} },sep=""))} }
  }

##--------------------------------------------------------------------------------------------------------------------------------------------------##
## Linkage Disequilibrium (LD) and Conditional asymmetric LD (cALD) analyses
## April 7, 2020 v1.0 -- Steven J. Mack

#' Calculation of the \eqn{D'}, \eqn{Wn}, and conditional Asymmetric LD Measures
#' 
#' Calculates \eqn{D'}, \eqn{Wn} (Cramer's V) and Thomson and Single's conditional asymmetric LD (\eqn{ALD}) measures for pairs of loci.
#' 
#' LD results can be directed to the console or to a data file or data frame object. This function can generate a haplotype vector file for each locus pair analyzed, and will return the LD results eiher in the console, or as a data frame object.
#' The implementation of ALD applied here is calculated using individual \eqn{Dij} LD values and allele frequencies.  
#'
#' @param dataSet A data frame or tab delimited file consisting of four columns of genotype data named, e.g. locus1_1 locus1_2 locus2_1 locus2_2, with 1 row per sample. The columns must be organized in this exact order, but the column names should not have _1 or _2 appended; use the same locus name for each column of a given locus. For phased data, locus1_1 is in phase with locus2_1, and locus1_2 is in phase with locus_2_2. Because this funciton operates on locus pairs, any rows with missing data should be excluded from the input genotype data.
#' @param inPhase A boolean identifying the genotyping data as phased or unphased (TRUE = phased; FALSE = unphased); default is unphased.
#' @param verbose A boolean identifying if results should be printed to the console (verbose = TRUE), or returned in a vector of (\eqn{D'}, \eqn{Wn}, \eqn{WLocus2/Locus1}, \eqn{WLocus1/Locus2}, number of haplotypes) (verbose = FALSE)
#' @param saveVector A boolean identifying if the vector of all haplotypes should be exported as a text file (saveVector = TRUE), or not (saveVector = FALSE). 
#' @param vectorName A name for the exported haplotype vector file; this name is not used if saveVector = FALSE. If a name is unspecified, then a filename including the locus-pair and a timestamp is generated.
#' @param vectorPrefix An optional prefix for the haplotpe vector to be used if saveVector = TRUE. This prefix will be appended, along with the phase status, before the locus name and timestamp. LDWrap() uses this parameter to identify the dataset and haplotype information passed to cALD().
#' @param vecDir The directory into which the haplotype vector should be written if saveVector = TRUE. The default is the directory specified by tempdir().
#' @keywords cALD ALD asymmetric conditional
#' @return A vector of {D'}, {Wn}, {WLocus2/Locus1}, {WLocus1/Locus2} values, and the number of haplotypes evaluated
#' @references Thomson G. & Single R.M. GENETICS 2014;198(1):321-31. https://doi.org/10.1534/genetics.114.165266
#' @importFrom utils write.table read.table
#' @importFrom gap LDkl
#' @importFrom haplo.stats haplo.em
#' @import gap
#' @import haplo.stats
#' @export
#' @examples 
#' # Analyze the first 10 rows of the included drb1.dqb1.demo genotype dataset
#' # and report LD results to the console.
#' cALD(drb1.dqb1.demo[1:10,])
#' # Alternatively, return a vector of LD results.
#' LDvec <- cALD(drb1.dqb1.demo[1:10,],verbose=FALSE)

cALD <- function(dataSet,inPhase=FALSE,verbose=TRUE,saveVector=FALSE,vectorName="",vectorPrefix="",vecDir=tempdir()){
#require(haplo.stats)
#require(gap)

## Specify phased or unphased genotype data in the inPhase argument (TRUE = phased; FALSE = unphased); default is unphased.
## Specify if you want results printed to the console (verbose = TRUE), or returned in a vector of (D', Wn, WB/A, WA/B) (verbose = FALSE)
## Specify if you want to report the vector of all haplotypes as a text file (saveVector = TRUE), or not (saveVector = FALSE). 
## Specify a name for the file exported for the haplotype vector as 'vectorName'; this is not used if saveVector = FALSE. If a name is unspecified, then a filename inclduding the locus-pair and a timestamp is generated.
## Specify an optional prefix for the haplotpe vector to be used if saveVector = TRUE. This prefix will be appended, along with the phase status, before the locus name and timestamp.  
  
## The dataset should consist of 4 columns of genotype data; locus1_1 locus1_2 locus2_1 locus2_2, with 1 row per sample. The data file should be tab-delimited.
## The columns must be organized in this exact order, but the column names should not have _1 or _2 appended; use the same locus name for each column of a given locus. 
## For phased data, locus1_1 is in phase with locus2_1, and locus1_2 is in phase with locus_2_2. 
## NOTE: Since this analysis is for 2-locus haplotypes, samples with missing data should be excluded from the input genotype data.

finRes <- rep("",5) ## vector for the final results

if (!is.data.frame(dataSet)) {
  gData <- read.table(paste(getwd(),dataSet,sep="/"), header=TRUE,sep="\t",colClasses = "character")
} else { gData <- dataSet
         gData[] <- lapply(gData, as.character) } # Convert factors in passed dataset to characters

gDataCols <- unique(gsub(".1","",colnames(gData),fixed=TRUE))

## generate 2-locus Haplotype Frequencies (phased or unphased)
if (inPhase == FALSE){
  hapFreqs <- haplo.em(gData,gDataCols,miss.val=0)
  hapTab <- cbind(hapFreqs$haplotype,hapFreqs$hap.prob)
} else if (inPhase == TRUE) {
  phaseHapCount <- as.data.frame(table(c(paste(gData[,1],gData[,3],sep="~"),paste(gData[,2],gData[,4],sep="~"))))
  hapTab <- data.frame(do.call(rbind,(strsplit(as.vector(phaseHapCount[,1]),'~'))),phaseHapCount[,2]/sum(phaseHapCount[,2]))
  }

colnames(hapTab) <- c(gDataCols,"Frequency")
nHap <- nrow(hapTab)

finRes[5] <- nHap

if (verbose == TRUE) {cat(paste("Calculating D', Wn and conditional ALD for ",nHap,if(inPhase==TRUE){" phased"}else{" unphased"}," genotypes at the ", gDataCols[1], " and ", gDataCols[2], " loci.\n",sep=""))}

## calculate Allele Freqs & Homozygosity
nLoc <- ncol(gData)/2
nAlleles <- array(1:nLoc, dim=c(1,nLoc)) ## [1,1] = #locus A alleles, [1,2] = #locus B alleles
locusF <- array(1:nLoc, dim=c(1,nLoc)) ## sum of squared frequencies [1,1] = locus A, [1,2] = locus B
resultHeader <- c("Locus#","Locus","Allele","Count","Frequency","Frequency^2")
Result = NULL
for (x in 1:nLoc){
  alleles <- c(gData[,x+(x-1)],gData[,x+(x-1)+1])
  alleleCount <- as.data.frame(table(alleles))
  alleleFreq <- cbind(alleleCount,alleleCount[,2]/sum(alleleCount[,2]),(alleleCount[,2]/sum(alleleCount[,2]))*(alleleCount[,2]/sum(alleleCount[,2])))
  freqTab <-cbind(x,gDataCols[x],alleleFreq) 
  nAlleles[,x] = nrow(freqTab)
  locusF[,x] <- sum(freqTab[,6])
  Result <- rbind(Result,freqTab)
}  
names(Result) <- resultHeader

## calculate D' and Wn values for this locus-pair
hapVector <- matrix(0,nAlleles[1],nAlleles[2]) 
for (h in 1:nHap){
  hapVector[match(hapTab[h,1],Result[1:nAlleles[1],3]),match(hapTab[h,2],Result[nAlleles[1]+1:nAlleles[2],3])] = hapTab[h,3]
}
resDprime <- LDkl(nAlleles[1],nAlleles[2],c(t(hapVector)),nrow(gData)*2)
resWn <- sqrt(resDprime$x2/((nrow(gData)*2)*min(nAlleles[1]-1,nAlleles[2]-1)))
    
if (verbose == TRUE) {
cat(paste("D' for ",gDataCols[1],"~",gDataCols[2]," haplotypes: ", resDprime$Dp, " (", round(resDprime$Dp,4),") ",'\n',sep=""))
cat(paste("Wn for ",gDataCols[1],"~",gDataCols[2]," haplotypes: ", resWn, " (", round(resWn,4),") ",'\n',sep=""))
}
finRes[1] <- resDprime$Dp
finRes[2] <- resWn

## Calculate Haplotype Specific Homozygosity (F), Overall Weighted hapSF values and Multi-allelic ALD squared values
maALDsq <- array(0, dim=c(1,nLoc)) ## [1,1] = WB/A^2 [1,2] = WA/B^2
weightedHapSF <- array(0, dim=c(1,nLoc)) ## [1,1] = FB/A [1,2] = FA/B
hapSFmatrix <- array(0, dim=c(max(nAlleles[,1],nAlleles[,2]),max(nAlleles[,1],nAlleles[,2]),nLoc)) ## [max(LocA/B),max(LocA/B),2]

for (x in 1:nLoc){ ## x = 1 = A for FB/Ai, then X = 2 = B for FA/Bj
             
      for (z in 1:nHap){                    
        hapSFmatrix[match(hapTab[z,numFlip(x)],Result[(((numFlip(x)-1)*nAlleles[1])+1):(((numFlip(x)-1)*nAlleles[2])+nAlleles[1]),3]),match(hapTab[z,x],Result[(((x-1)*nAlleles[1])+1):(((x-1)*nAlleles[2])+nAlleles[1]),3]),x] = (hapTab[z,3]/(Result[((x-1)*nAlleles[1])+match(hapTab[z,x],Result[(((x-1)*nAlleles[1])+1):(((x-1)*nAlleles[2])+nAlleles[1]),3]),5]))^2 
      }
   
    ## Overall weighted hapSF values 
     for (y in 1:nAlleles[,x]){ ## y alleles[,1] = Ai for FB/Ai; y alleles[,2] = Bj for FA/Bj 
       for (w in 1:nAlleles[,numFlip(x)]) { ## w alleles[,x=1] = Sum j for FB/Ai; w alleles[,x=2] = Sum i for FA/Bj  
         weightedHapSF[,x] = weightedHapSF[,x] + hapSFmatrix[w,y,x]*Result[y+((x-1)*nAlleles[,1]),5] 
       }
     }
    
  ## Multi-allelic ALD squared      
  maALDsq[,x] = (weightedHapSF[,x]-locusF[,numFlip(x)]) / (1-locusF[,numFlip(x)])

  ## Print ALD results
  if (verbose == TRUE) {cat(paste("Variation of ",gDataCols[numFlip(x)]," conditioned on ",gDataCols[x]," (W",gDataCols[numFlip(x)],'/',gDataCols[x],") = ",sqrt(maALDsq[,x])," (",round(sqrt(maALDsq[,x]),4),")",'\n',sep=""))}

  finRes[x+2] <- sqrt(maALDsq[,x])
}

  if (saveVector == TRUE) {writeVector(nAlleles[1],Result,hapVector,nrow(gData)*2,vectorName,inPhase,vectorPrefix,vecDir)}
  if (verbose == FALSE) {return(finRes)}
}
            
# Switch back and forth between A and B for a given x
numFlip <- function(x){
  if(x==1){return(2)}
  else if(x==2){return(1)}
}

#' Exporting Haplotype Vectors
#' 
#' This function writes the haplotype vector and any accessory information to a text file.
#' @param nLoc1 Number of alleles at locus 1
#' @param Res Data frame consisting of Locus Number, Locus Name, Allele Name, Allele Count, Allele Frequency and Frequency^2
#' @param hapVec Haplotype vector 
#' @param numSamp The dataset's 2N value
#' @param writeName The specified name for the vector
#' @param genPhase Boolean describing phased (TRUE) or unphased (FALSE) analysis
#' @param Prefix The specified prefix for the written vector file
#' @param vDir The directory into which the vector file should be written.
#' @note This function is for internal POULD use only.
#' @keywords writeVector
#' @examples #

# write haplotype vector and accessory information to a text file 
writeVector <- function(nLoc1,Res,hapVec,numSamp,writeName,genPhase,Prefix,vDir){
  
  # nAlleles[1] = nLoc1
  # Result = Res
  # hapVector = hapVec
  # nrow(gData)*2= numSamp 
  # vectorName = writeName
  # inPhase = genPhase
  # vectorPrefix = Prefix
  # vecDir = vDir
  
  phaseType <- "unphased_"
  if(genPhase) {phaseType <- "phased_"}
  if(Prefix != "") {Prefix <- paste(basename(Prefix),"_",phaseType, sep="")}
  if (writeName == "") {writeName = paste(Prefix,paste(paste(Res[1,2],Res[nLoc1+1,2],sep="~"),"haplotype_Vector",Sys.Date(),gsub(":","-",format(Sys.time(), "%X")),sep="_"),sep="")} else {writeName <- basename(writeName)}
  
  hapNames <- matrix(0,nLoc1,nrow(Res)-nLoc1)

  for (n in 1:nLoc1){
    for (m in 1:(nrow(Res)-nLoc1)) {
      hapNames[n,m]= paste(Res[n,3],Res[nLoc1+m,3],sep="~")
    }
  }
  vecTab <- cbind(writeName,as.logical(genPhase),c(hapNames),c(hapVec),c(hapVec*numSamp))
  colnames(vecTab) <- c("Dataset","Phase",paste(Res[1,2],Res[nLoc1+1,2],sep="~"),"Frequency","Count")
  write.table(vecTab,file=paste(vDir,paste(writeName,"txt",sep="."),sep=.Platform$file.sep),row.names=FALSE,sep="\t",quote=FALSE)
}

