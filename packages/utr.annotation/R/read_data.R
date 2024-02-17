# Author: Yating Liu
# Description: Define functions to read variant files
# Date: 02/01/2019

#' Read variant file in CSV format
#' @param variantFile A varian file in CSV or VCF format
#' @param format File format, csv or vcf. The default file format is csv
#' @return A data.table containing all variants if variant file is valid
#' @keywords internal
#' @description \code{readVariantData} Read variant file in CSV / VCF format, check if it contains required columns for UTR annotation, split rows if there're collapsed columns (items are separated by ,), and remove rows containing NAs in Chr, Pos, Ref or Alt
#' @importFrom data.table fread
readVariantData <- function(variantFile, format = "csv") {
  if (format == "csv") {
    variantsTable <- fread(variantFile, header = TRUE, sep = ",", data.table = TRUE)
  } else if (format == "vcf") {
    variantsTable <- readVCFData(variantFile)
  } else {
    stop('If the variants file is in CSV format, specify format = "csv".\nIf the variants file is in VCF format, specify format = "vcf"')
  }
  variantsTable <- checkInputValid(variantsTable)
  return(variantsTable)
}

#' Check if the variable table is a valid for UTR annotation
#' @description Check whether has 4 required columns, remove rows have NAs in the required columns, split rows if there're collapsed columns (items are separated by ,)
#' @keywords internal
#' @param variantsTable A data.table containing all variants
#' @param sep Delimiter used in variant file for concatenating multiple items in one column
#' @return A data.table containing all valid variants
#' @importFrom stats complete.cases
checkInputValid <- function(variantsTable, sep = ",") {
  # Check if the table has the required columns: Chr, Pos, Ref, Alt
  required <- c("Chr", "Pos", "Ref", "Alt")
  for (item in required) {
    if (!(item %in% colnames(variantsTable))) {
      stop(paste0("Missing required column: ", item, " doesn't exist in the input!"))
    }
  }
  # Check if there's only one item in each column in the table, if not, output column names with multiple items
  multiItemsCol <- c()
  for (feature in required) {
    items_list <- lapply(lapply(variantsTable[, get(feature)], str_split, sep), `[[`, 1)
    items_len <- lapply(items_list, length)
    multiRows <- which(items_len>1)
    if (length(multiRows) > 0) {
      multiItemsCol <- append(multiItemsCol, feature)
    }
  }
  if (length(multiItemsCol) > 0) {
    warning("Multiple items in one or more columns: ", paste0(multiItemsCol, collapse = ","), ".\nThe collapsed column(s) will be separated into multiple rows.")
    variantsTable <- splitRowsIfMultiFeature(variantsTable, multiItemsCol, sep)
  }
  # Remove rows if anyNA in Chr, Pos, Ref, and Alt
  variantsTable <- variantsTable[complete.cases(variantsTable[,c("Chr", "Pos", "Ref", "Alt")]),]
  return (variantsTable)
}

#' Get feature information from INFO column of VCF file
#' @description Get feature information from INFO column of VCF file
#' @keywords internal
#' @param infoList INFO column of VCF variant file
#' @return A data table of useful information extracted from INFO
#' @importFrom stringr str_c str_split
getFeatureFromInfo <- function(infoList) {
  geneId <- transcriptId <- consequence <- NULL
  dat <- data.table(info = infoList)
  for (row in 1:length(infoList)) {
    keys <- str_split(infoList[row], ";")[[1]]
    for (key in keys) {
      if (str_detect(key, "CSQ=")) {
        ## could have multiple genes
        csq_infos <- str_split(key, ",")[[1]]
        gene_ids <- c()
        transcript_ids <- c()
        features <- c()
        conseqs <- c()
        for (csq_info in csq_infos) {
          csq_info <- str_split(csq_info, "\\|")[[1]]
          gene_ids <- append(gene_ids, csq_info[2])
          transcript_ids <- append(transcript_ids, csq_info[3])
          features <- append(features, csq_info[4])
          conseqs <- append(conseqs, csq_info[5])
        }
        dat[row, geneId := str_c(gene_ids, collapse = ",")]
        dat[row, transcriptId := str_c(transcript_ids, collapse = ",")]
        dat[row, consequence := str_c(conseqs, collapse = ",")]
      }
    }
  }
  return (dat)
}


#' Split rows if the feature column(s) contains multiple items, separated by sep
#' @keywords internal
#' @param variantsTable variants table (data.table)
#' @param columnNames a string or a vector of strings that specifies columns need to be checked and splited
#' @param sep separator that is used to seperate items in the column(s)
#' @return A new data.table that contains one item in the specified column(s) in each row
#' @details
#' splitRowsIfMultiFeature(variants, "Alt"): check column "Alt" to see if any rows have multiple Alt sequences like ATG,TC; if so, split these rows so that one row only has one Alt sequences.
#' @importFrom data.table as.data.table
#' @importFrom tidyr separate_rows
splitRowsIfMultiFeature <- function(variantsTable, columnNames, sep = ",") {
  finalTable <- tidyr::separate_rows(variantsTable, columnNames, sep = sep)
  return (as.data.table(finalTable))
}

#' Read variants from a VCF formatted file
#' @keywords internal
#' @param variantFile A variant file in VCF format
#' @return A variant table
#' @importFrom data.table data.table
#' @importFrom vcfR read.vcfR
readVCFData <- function(variantFile) {
  Gene <- Transcript <- Consequence <- NULL
  variants_vcf <- read.vcfR(variantFile)
  table <- data.table(Chr = variants_vcf@fix[, "CHROM"],
                         Pos = variants_vcf@fix[, "POS"],
                         Ref = variants_vcf@fix[, "REF"],
                         Alt = variants_vcf@fix[, "ALT"])

  ## INFO can contains multiple genes sepearated by ","
  info <- variants_vcf@fix[, "INFO"]
  dt <- getFeatureFromInfo(info)

  table[, Gene := dt$geneId]
  table[, Transcript := dt$transcriptId]
  table[, Consequence := dt$consequence]
  return (table)
}
