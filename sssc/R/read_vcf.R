#' Check input filename
#' @param fn Exact full file name of input file, including directory
#' @param extension Expected input file extension: vcf & txt
#'
#' @return Valid directory
locateFile <- function(fn, extension) {
  if (!file.exists(fn)) {
    stop('Input file does NOT exist!')
  }
  if (file.access(names = fn, mode = 4) != 0) {
    stop('Input file does not have read permission!')
  }
  ext <- strsplit(x = fn, split = '\\.')[[1]]
  if ((ext[length(ext)]!=extension)&(ext[length(ext)-1]!=extension)) {
    stop('Invalid input file extension!')
  }
  return(fn)
}

#' Read in input vcf data in GATK format for Contamination detection
#' @param dr A valid input object
#' @param dbOnly Use dbSNP as filter, default is FALSE, passed from read_vcf
#' @param depCut Use a threshold for min depth , default is False
#' @param thred Threshold for min depth, default is 20
#' @param content Column names in VCF files
#' @param extnum The column number or numbers to be extracted from vcf, default is 10; 0 for not extracting any columns
#' @param keepall Keep unextracted column in output, default is TRUE, passed from read_vcf
#'
#' @return Dataframe from VCF file
readGATK <- function(dr, dbOnly, depCut, thred, content, extnum, keepall) {
  vcf <- read.table(file = dr, header = FALSE, sep = "\t", quote="", as.is = TRUE, skip = 0)
  if (dbOnly) {
    tmp_f <- vcf[vcf$V3 !=".",] #Filter unannotated variants
    message(paste0(100*nrow(tmp_f)/nrow(vcf), "% of variants are annotated."))
    if (nrow(tmp_f) == 0) {
      message("No variants are annotated, so dbOnly function is off!")
      tmp_f <- vcf
    }
    vcf <- tmp_f
  }
  names(vcf) <- content
  infoname <- lapply(X = vcf$FORMAT[1], FUN = function(x) { y <- strsplit(x = x, split = ":")[[1]] } )
  if (extnum != 0) {
    tmp <- lapply(X = vcf[,extnum], FUN = function(x) { y <- strsplit(x = x, split = ":")[[1]] } )

    for (i in 1:length(infoname[[1]])) {
      name <- infoname[[1]][i]
      vcf[, name] <- unlist(lapply(X = tmp, FUN = function(x) {x[i]}))
    }
    vcf$DP <- as.numeric(vcf$DP)
    if (any(grep(pattern=",", x=vcf$ALT))) print("This sample contains more than one alternative allele.")
    vcf$AC <- as.numeric(lapply(X = vcf$AD, FUN = function(x) { y <- strsplit(x = x, split = ",")[[1]][2]} )) #Allele depth
    vcf$RC <- as.numeric(lapply(X = vcf$AD, FUN = function(x) { y <- strsplit(x = x, split = ",")[[1]][1]} )) #Reference depth
    vcf$AF <- as.numeric(round(vcf$AC/as.numeric(vcf$DP), 4))
    vcf$ZG <- "Complex" #Zygosity
    vcf$ZG[ grep(pattern = "^1[/|]1", x = vcf[,extnum])] <- "hom"
    vcf$ZG[ grep(pattern = "^0[/|]1", x = vcf[,extnum])] <- "het"
    if (depCut) {
      vcf <- vcf[ vcf$DP > thred, ]
    }
  }
  if (!keepall) {
    remove_list <- c(10:length(content))[!(c(10:length(content)) %in% extnum)]
    vcf <- vcf[, -remove_list]
  }
  return(vcf)
}

#' Read in input vcf data in VarPROWL format

#' @param dr A valid input object
#' @param dbOnly Use dbSNP as filter, default is FALSE, passed from read_vcf
#' @param depCut Use a threshold for min depth , default is False
#' @param thred Threshold for min depth, default is 20
#' @param content Column names in VCF files
#' @param extnum The column number or numbers to be extracted from vcf, default is 10; 0 for not extracting any columns
#' @param keepall Keep unextracted column in output, default is TRUE, passed from read_vcf
#'
#' @return vcf Dataframe from VCF file
readVarPROWL <- function(dr, dbOnly, depCut, thred, content, extnum, keepall) {
  vcf <- read.table(file = dr, header = FALSE, sep = "\t", quote="", as.is = TRUE, skip = 0)
  if (dbOnly) {
    tmp_f <- vcf[ vcf$V3 !=".", ] #Filter unannotated variants
    message(paste0(100*nrow(tmp_f)/nrow(vcf), "% of variants are annotated."))
    if (nrow(tmp_f) == 0) {
      message("No variants are annotated, so dbOnly function is off!")
      tmp_f <- vcf
    }
    vcf <- tmp_f
  }
  vcf$V1 <- gsub("chr", "", vcf$V1) #To get a numeric chromosome column
  vcf$V1 <- gsub("Chr", "", vcf$V1) #The same reason
  names(vcf) <- content

  infoname <- lapply(X = vcf$FORMAT[1], FUN = function(x) { y <- strsplit(x = x, split = ":")[[1]] } )

  if (extnum != 0) {
    tmp <- lapply(X = vcf[,extnum], FUN = function(x) { y <- strsplit(x = x, split = ":")[[1]] } )

    for (i in 1:length(infoname[[1]])) {
      name <- infoname[[1]][i]
      vcf[, name] <- unlist(lapply(X = tmp, FUN = function(x) {x[i]}))
    }
    vcf$GT <- gsub("|", "/", vcf$GT, fixed=TRUE) #Genotype at that locus
    vcf$DP <- as.numeric(vcf$DP)
    if (any(grep(pattern=",", x=vcf$ALT))) print("This sample contains more than one alternative allele.")
    vcf$AF <- as.numeric(unlist(lapply(X = vcf$AF, FUN = function(x) { y <- strsplit(x = x, split = ",")[[1]][1] } )))
    vcf$AC <- as.numeric(unlist(lapply(X = vcf$AD, FUN = function(x) { y <- strsplit(x = x, split = ",")[[1]][1] } ))) #Alternate depth
    vcf$RC <- vcf$DP - vcf$AC #Reference depth
    vcf$ZG <- "Complex" #Zygosity
    vcf$ZG[ grep(pattern = "^1[/|]1", x = vcf[,extnum])] <- "hom"
    vcf$ZG[ grep(pattern = "^0[/|]1", x = vcf[,extnum])] <- "het"
    if (depCut) {
      vcf <- vcf[ vcf$DP > thred, ]
    }
  }
  if (!keepall) {
    remove_list <- c(10:length(content))[!(c(10:length(content)) %in% extnum)]
    vcf <- vcf[, -remove_list]
  }
  return(vcf)
}

#' Read in input vcf data in VarDict format for Contamination detection
#' @param dr A valid input object
#' @param dbOnly Use dbSNP as filter, default is FALSE, passed from read_vcf
#' @param depCut Use a threshold for min depth , default is False
#' @param thred Threshold for min depth, default is 20
#' @param content Column names in VCF files
#' @param extnum The column number to be extracted from vcf, default is 10; 0 for not extracting any column
#' @param keepall Keep unextracted column in output, default is TRUE, passed from read_vcf
#'
#' @return Dataframe from VCF file
readVarDict <- function(dr, dbOnly, depCut, thred, content, extnum, keepall) {
  vcf <- read.table(file = dr, header = FALSE, sep = "\t", quote="", as.is = TRUE, skip = 0)
  if (dbOnly) {
    tmp_f <- vcf[vcf$V3 !=".",] #Filter unannotated variants
    message(paste0(100*nrow(tmp_f)/nrow(vcf), "% of variants are annotated."))
    if (nrow(tmp_f) == 0) {
      message("No variants are annotated, so dbOnly function is off!")
      tmp_f <- vcf
    }
    vcf <- tmp_f
  }
  names(vcf) <- content

  infoname <- lapply(X = vcf$FORMAT[1], FUN = function(x) { y <- strsplit(x = x, split = ":")[[1]] } )

  if (extnum != 0) {
    tmp <- lapply(X = vcf[,extnum], FUN = function(x) { y <- strsplit(x = x, split = ":")[[1]] } )
    for (i in 1:length(infoname[[1]])) {
      name <- infoname[[1]][i]
      vcf[, name] <- unlist(lapply(X = tmp, FUN = function(x) {x[i]}))
    }
    vcf$DP <- as.numeric(vcf$DP)
    if (any(grep(pattern=",", x=vcf$ALT))) print("This sample contains more than one alternative allele.")
    vcf$AC <- as.numeric(vcf$VD)
    vcf$AF <- as.numeric(vcf$AF)
    vcf$RC <- as.numeric(unlist(lapply(X = vcf$AD, FUN = function(x) {y <- strsplit(x = x, split = ",")[[1]][1] })))
    vcf$ZG <- "Complex" #Zygosity
    vcf$ZG[ grep(pattern = "^1[/|]1", x = vcf[,extnum])] <- "hom"
    vcf$ZG[ grep(pattern = "^0[/|]1", x = vcf[,extnum])] <- "het"

    if (depCut) {
      vcf <- vcf[ vcf$DP > thred, ]
    }
  }
  if (!keepall) {
    remove_list <- c(10:length(content))[!(c(10:length(content)) %in% extnum)]
    vcf <- vcf[, -remove_list]
  }
  return(vcf)
}

#' Read in input vcf data in strelka2 format for Contamination detection
#' @param dr A valid input object
#' @param dbOnly Use dbSNP as filter, default is FALSE, passed from read_vcf
#' @param depCut Use a threshold for min depth , default is False
#' @param thred Threshold for min depth, default is 20
#' @param content Column names in VCF files
#' @param extnum The column number or numbers to be extracted from vcf, default is 10; 0 for not extracting any columns
#' @param keepall Keep unextracted column in output, default is TRUE, passed from read_vcf
#'
#' @return Dataframe from VCF file
readStrelka <- function(dr, dbOnly, depCut, thred, content, extnum, keepall) {
  vcf <- read.table(file = dr, header = FALSE, sep = "\t", quote="", as.is = TRUE, skip = 0)
  if (dbOnly) {
    tmp_f <- vcf[vcf$V3 !=".",] #Filter unannotated variants
    message(paste0(100*nrow(tmp_f)/nrow(vcf), "% of variants are annotated."))
    if (nrow(tmp_f) == 0) {
      message("No variants are annotated, so dbOnly function is off!")
      tmp_f <- vcf
    }
    vcf <- tmp_f
  }
  names(vcf) <- content
  infoname <- lapply(X = vcf$FORMAT[1], FUN = function(x) { y <- strsplit(x = x, split = ":")[[1]] } )

  if (extnum != 0) {
    tmp <- lapply(X = vcf[,extnum], FUN = function(x) { y <- strsplit(x = x, split = ":")[[1]] } )
    for (i in 1:length(infoname[[1]])) {
      name <- infoname[[1]][i]
      vcf[, name] <- unlist(lapply(X = tmp, FUN = function(x) {x[i]}))
    }
    vcf$DP <- as.numeric(vcf$DP)
    if (any(grep(pattern=",", x=vcf$ALT))) print("This sample contains more than one alternative allele.")
    vcf$RC <- as.numeric(unlist(lapply(X = vcf$AD, FUN = function(x) {y <- strsplit(x = x, split = ",")[[1]][1] })))
    vcf$AC <- as.numeric(unlist(lapply(X = vcf$AD, FUN = function(x) {y <- strsplit(x = x, split = ",")[[1]][2] })))
    vcf$AF <- (vcf$AC)*(1/vcf$DP)
    vcf$ZG <- "Complex" #Zygosity
    vcf$ZG[ grep(pattern = "^1[/|]1", x = vcf[,extnum])] <- "hom"
    vcf$ZG[ grep(pattern = "^0[/|]1", x = vcf[,extnum])] <- "het"

    if (depCut) {
      vcf <- vcf[ vcf$DP > thred, ]
    }
  }
  if (!keepall) {
    remove_list <- c(10:length(content))[!(c(10:length(content)) %in% extnum)]
    vcf <- vcf[, -remove_list]
  }
  return(vcf)
}

#' @title VCF Data Input
#' @description Reads a file in vcf or vcf.gz file and creates a list containing Content, Meta, VCF and file_sample_name
#'
#' @param fn Input vcf file name
#' @param vcffor Input vcf data format: 1) GATK; 2) VarPROWL; 3) VarDict; 4) strelka2
#' @param dbOnly Use dbSNP as filter, default is FALSE
#' @param depCut Use a threshold for min depth , default is False
#' @param thred Threshold for min depth, default is 20
#' @param metaline Number of head lines to read in (better to be large enough), the lines will be checked if they contain meta information, default is 200
#' @param extnum The column number to be extracted from vcf, default is 10; 0 for not extracting any column; extnum should be between 10 and total column number
#' @param keepall Keep unextracted column in output, default is TRUE
#'
#' @return A list containing (1) Content: a vector showing what is contained; (2) Meta: a data frame containing meta-information of the file;
#' (3) VCF: a data frame, the main part of VCF file; (4) file_sample_name: the file name and sample name,
#' in case when multiple samples exist in one file, file and sample names might be different
#'
#' @export
#' @importFrom utils read.table
#'
#' @examples
#' file.name <- system.file("extdata", "example.vcf.gz", package = "sssc")
#' example <- read_vcf(fn=file.name, vcffor="VarPROWL")
read_vcf <- function(fn, vcffor, dbOnly = FALSE, depCut = FALSE,
                     thred = 20, metaline = 200, extnum = 10, keepall = T) {
  vcfobj <- locateFile(fn = fn, extension = 'vcf')
  meta_lines <- readLines(con = vcfobj, n = metaline)
  content <- meta_lines[startsWith(meta_lines, "#CH")]
  if (!is.numeric(extnum)) {
    stop('extnum must be numeric!')
  } else if ((extnum < 10) & (extnum != 0)) {
    stop('extnum must >= 10 or 0!')
  }
  if (length(content)!=0) {
    contents <- strsplit(content, '\t')[[1]]
    contents[1] <- gsub("#", "", contents[1])
    if (extnum > length(contents)) {
      stop('extnum cannot beyond column number!')
    }
  } else {
    contents <- NULL
  }
  meta <- as.data.frame(meta_lines[startsWith(meta_lines, "##")])
  colnames(meta) <- "Meta"
  if (vcffor == 'VarDict'){
    df <- readVarDict(dr = vcfobj, dbOnly, depCut, thred, content = contents, extnum, keepall)
  } else if (vcffor == 'GATK') {
    df <- readGATK(dr = vcfobj, dbOnly, depCut, thred, content = contents, extnum, keepall)
  } else if (vcffor == 'VarPROWL') {
    df <- readVarPROWL(dr = vcfobj, dbOnly, depCut, thred, content = contents, extnum, keepall)
  } else if (vcffor == 'Strelka') {
    df <- readStrelka(dr = vcfobj, dbOnly, depCut, thred, content = contents, extnum, keepall)
  } else {
    stop('Invalid VCF type!')
  }
  res_list <- list("Content" = contents, "Meta" = meta, "VCF" = df,
                   "file_sample_name" = c(basename(vcfobj), contents[extnum]))
  return (res_list)
}
