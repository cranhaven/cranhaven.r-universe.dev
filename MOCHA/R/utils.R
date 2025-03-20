# Function to get sample-level metadata,
# from an ArchR project's colData
sampleDataFromCellColData <- function(cellColData, sampleLabel) {
  if (!(sampleLabel %in% colnames(cellColData))) {
    stop(paste(
      "`sampleLabel` must present in your ArchR Project's cellColData",
      "Check `names(getCellColData(ArchRProj)` for possible sample columns."
    ))
  }

  # Drop columns where all values are NA
  cellColDataNoNA <- BiocGenerics::Filter(function(x) {
    !all(is.na(x))
  }, cellColData)

  # Convert to data.table
  cellColDT <- data.table::as.data.table(cellColDataNoNA)

  BoolDT <- cellColDT[, lapply(.SD, function(x) {
    length(unique(x)) == 1
  }), by = c(sampleLabel)]
  trueCols <- apply(BoolDT, 2, all)
  trueCols[[sampleLabel]] <- TRUE
  cellColDF <- as.data.frame(cellColDT)

  sampleData <- dplyr::distinct(cellColDF[, names(which(trueCols)), drop = F])

  # Set sampleIDs as rownames
  rownames(sampleData) <- sampleData[[sampleLabel]]
  return(sampleData)
}


# Function to split the output of getPopFrags into a list
# of lists of GRanges, one named for each celltype.
# Resulting in a list containing each celltype, and each
# celltype has a list of GRanges name for each sample.
splitFragsByCellPop <- function(frags) {

  # Rename frags by cell population
  renamedFrags <- lapply(
    1:length(frags),
    function(y) {
      # Split out celltype and sample from the name
      x <- frags[y]
      celltype_sample <- names(x)
      splits <- unlist(stringr::str_split(celltype_sample, "#"))
      celltype <- splits[1]
      sample <- unlist(stringr::str_split(splits[2], "__"))[1]
      # Rename the fragments with just the sample
      names(x) <- sample
      # Return as a list named for celltype
      output <- list(x)
      names(output) <- celltype
      output
    }
  )

  # Group frags by cell population
  renamedFrags <- unlist(renamedFrags, recursive = FALSE)
  splitFrags <- split(renamedFrags, f = names(renamedFrags))
  return(splitFrags)
}


# Tests if a string is a in the correct format to convert to GRanges
validRegionString <- function(regionString) {
  if (!is.character(regionString)) {
    return(FALSE)
  }

  pattern <- "([0-9]{1,2}|chr[0-9]{1,2}|chr[X-Y]{1,1}):[0-9]*-[0-9]*"
  matchedPattern <- stringr::str_extract(regionString, pattern)

  if (any(is.na(matchedPattern))) {
    return(FALSE)
  } else if (any(!matchedPattern == regionString)) {
    return(FALSE)
  }

  splits <- stringr::str_split(regionString, "[:-]")[[1]]
  start <- splits[2]
  end <- splits[3]
  if (any(start > end)) {
    return(FALSE)
  }

  # All conditions satisfied
  return(TRUE)
}

#' @title \code{StringsToGRanges}
#'
#' @description \code{StringsToGRanges} Turns a list of strings in the format chr1:100-200
#'   into a GRanges object
#'
#' @param regionString A string or list of strings each in the format chr1:100-200
#' @return a GRanges object with ranges representing the input string(s)
#'
#' @export
StringsToGRanges <- function(regionString) {
  # if (length(regionString)>1){
  #   boolList <- lapply(regionString, function(x){validRegionString(x)})
  #   if (!all(boolList)){
  #     stop("Some region strings are invalid. Given regions must all be strings matching format 'seqname:start-end', where start<end e.g. chr1:123000-123500")
  #   }
  # } else if(!validRegionString(regionString)) {
  #   stop("Region must be a string matching format 'seqname:start-end', where start<end e.g. chr1:123000-123500")
  # }
  . <- NULL
  regionString <- gsub(",", "", regionString)
  chrom <- gsub(":.*", "", regionString)
  startSite <- gsub(".*:", "", regionString) %>%
    gsub("-.*|-.*", "", .) %>%
    as.numeric()
  endSite <- gsub(".*-|.*-", "", regionString) %>% as.numeric()

  if (any(startSite >= endSite)) {
    stop("Error in region string: Make sure the start of the genomic range occurs before the end")
  }
  regionGRanges <- GenomicRanges::GRanges(seqnames = chrom, ranges = IRanges::IRanges(start = startSite, end = endSite), strand = "*")
  return(regionGRanges)
}

#' @title \code{GRangesToString} Converts a GRanges object to a string in the format 'chr1:100-200'
#'
#' @description \code{GRangesToString} Turns a GRanges Object into
#'  a list of strings in the format chr1:100-200
#'
#' @param GR_obj the GRanges object to convert to a string
#' @return A string or list of strings in the format 'chr1:100-200' representing
#'  ranges in the input GRanges
#'
#' @export
GRangesToString <- function(GR_obj) {
  paste(GenomicRanges::seqnames(GR_obj), ":", GenomicRanges::start(GR_obj), "-", GenomicRanges::end(GR_obj), sep = "")
}

#' @title \code{differentialsToGRanges} Converts a data.frame matrix to a GRanges,
#'   preserving additional columns as GRanges metadata
#'
#' @param differentials a matrix/data.frame with a column tileColumn containing
#'   region strings in the format "chr:start-end"
#' @param tileColumn name of column containing region strings. Default is "Tile".
#'
#' @return a GRanges containing all original information
#' @export
differentialsToGRanges <- function(differentials, tileColumn = "Tile") {
  regions <- MOCHA::StringsToGRanges(differentials[[tileColumn]])
  GenomicRanges::mcols(regions) <- differentials
  regions
}

# Helpers for getAnnotationDb
.TXDB_PREFIX <- "TxDb."
.has_TxDb_prefix <- function(x) {
  substr(x, 1L, nchar(.TXDB_PREFIX)) == .TXDB_PREFIX
}

.ORGDB_PREFIX <- "Org."
.has_OrgDb_prefix <- function(x) {
  substr(x, 1L, nchar(.ORGDB_PREFIX)) == .ORGDB_PREFIX
}

#' @title \code{getAnnotationDbFromInstalledPkgname} Loads and attaches an installed TxDb or
#'   OrgDb-class Annotation database package.
#'
#' @description See \link[BSgenome]{getBSgenome}
#'
#' @param dbName Exact name of installed annotation data package.
#' @param type Expected class of the annotation data package, must be
#'   either "OrgDb" or "TxDb".
#' @return the loaded Annotation database object.#' @noRd
getAnnotationDbFromInstalledPkgname <- function(dbName, type) {
  if (!methods::is(dbName, "character")) {
    stop(
      "dbName must be a character string. ",
      "Please provide TxDb or OrgDb as a string."
    )
  }

  if (!type %in% c("OrgDb", "TxDb")) {
    stop('Invalid type. Type must be either "OrgDb" or "TxDb".')
  }

  ok <- suppressWarnings(require(dbName,
    quietly = TRUE,
    character.only = TRUE
  ))

  if (!ok) {
    stop(
      "Package ", dbName, " is not available. Please install and provide ",
      "the name of a Bioconductor AnnotationData Package for your organism."
    )
  }

  pkgenvir <- as.environment(paste("package", dbName, sep = ":"))

  db <- try(get(dbName, envir = pkgenvir, inherits = FALSE), silent = TRUE)

  if (!methods::is(db, type)) {
    stop(dbName, " doesn't look like a valid ", type, " data package")
  }
  db
}

#' @title \code{getCellTypes} Extract cell type names from a Tile Results or Sample Tile object.
#'
#' @description \code{getCellTypes} Returns a vector of cell names from a Tile Results or Sample Tile object.
#'
#' @param object tileResults object from callOpenTiles or SummarizedExperiment from getSampleTileMatrix
#' @return a vector of cell type names.
#'
#' @export
getCellTypes <- function(object) {
  if (class(object)[1] == "MultiAssayExperiment") {
    return(names(object))
  } else if (class(object)[1] == "RangedSummarizedExperiment") {
    return(names(SummarizedExperiment::assays(object)))
  } else {
    stop("Object not recognized. Please provide an object from callOpenTiles or getSampleTileMatrix.")
  }
}


#' @title \code{getCellTypeTiles} Extract the GRanges for a particular cell type
#'
#' @description \code{getCellTypeTiles} Returns a GRanges object of all tiles called for a certain cell type
#'
#' @param object A SampleTileObject.
#' @param cellType A string describing one cell type.
#' @return a vector of cell type names.
#'
#' @export
getCellTypeTiles <- function(object, cellType) {
  if (class(object)[1] == "MultiAssayExperiment") {
    stop("This is a MultiAssayExperiment, and thus like a tileResults object. Please provide a SampleTileMatrix object.")
  } else if (class(object)[1] == "RangedSummarizedExperiment") {
    all_ranges <- SummarizedExperiment::rowRanges(object)

    if (sum(cellType == names(SummarizedExperiment::assays(object))) != 1) {
      stop("Please provide a single cell type, which must be present in the SampleTileObject. cellType is either not found, or is a list of multiple cell types.")
    } else {
      subRange <- all_ranges[unlist(GenomicRanges::mcols(all_ranges)[, cellType])]
    }

    return(subRange)
  } else {
    stop("Object not recognized. Please provide a SampleTileMatrix object (SummarizedExperiment).")
  }
}


#' @title \code{getSampleCellTypeMetadata} Extract Sample-celltype specific metadata
#' @description \code{getSampleCellTypeMetadata} Extract Sample-celltype specific metadata like fragment number, cell counts, and
#'
#' @param object tileResults object from callOpenTiles or SummarizedExperiment from getSampleTileMatrix
#' @return a SummarizedExperiment where each assay is a different type of metadata.
#'
#' @export
getSampleCellTypeMetadata <- function(object) {
  # Check if the object has Sample-CellType-level metadata stored in the metadata slot.
  if (all(c("FragmentCounts", "CellCounts") %in% names(object@metadata))) {

    sampleData <- SummarizedExperiment::colData(object)

    fragCounts <- object@metadata$FragmentCounts
    fragMat <- as.matrix(t(fragCounts[match(rownames(sampleData), rownames(fragCounts)), ]))
    rownames(fragMat) <- colnames(fragCounts)

    cellCounts <- object@metadata$CellCounts
    cellMat <- as.matrix(t(cellCounts[match(rownames(sampleData), rownames(cellCounts)), ]))
    rownames(cellMat) <- colnames(cellMat)

    if (any(!dim(cellMat) %in% dim(cellCounts))) {
      stop("Error in processing Sample-Cell type metadata. Some Samples may be missing. Please correct Sample-Cell type metadata manually or regenerate the object.")
    }

    metaList <- list(fragMat, cellMat)
    names(metaList) <- c("FragmentCounts", "CellCounts")

    SE <- SummarizedExperiment::SummarizedExperiment(metaList, colData = sampleData)
    return(SE)
  } else {
    stop("Object does not appear to have Sample-Celltype metadata.")
  }
}


#' @title \code{plotIntensityDistribution}
#' @description \code{plotIntensityDistribution}  Plots the distribution of sample-tile intensities for a give cell type
#'
#' @param TSAM_object  SummarizedExperiment from getSampleTileMatrix
#' @param cellPopulation Cell type names (assay name) within the TSAM_object
#' @param density Boolean to determine whether to plot density or histogram. Default is TRUE (plots density).
#' @param returnDF If TRUE, return the data frame without plotting. Default is FALSE.
#' @return data.frame or ggplot histogram.
#'
#' @export
plotIntensityDistribution <- function(TSAM_object, cellPopulation, returnDF = FALSE, density = TRUE) {
  Values <- NULL
  if (!requireNamespace("Biobase", quietly = TRUE)) {
    stop("Package 'Biobase' is not installed. 'Biobase' is required for `plotIntensityDistribution`.")
  }
  mat <- unlist(Biobase::rowMedians(log2(getCellPopMatrix(TSAM_object, cellPopulation) + 1)))
  plotMat <- data.frame(Values = mat)

  if (returnDF) {
    return(plotMat)
  }
  if (density) {
    p1 <- ggplot2::ggplot(plotMat, ggplot2::aes(x = Values)) +
      ggplot2::geom_density() +
      ggplot2::theme_bw()
  } else {
    p1 <- ggplot2::ggplot(plotMat, ggplot2::aes(x = Values)) +
      ggplot2::geom_histogram() +
      ggplot2::theme_bw()
  }

  return(p1)
}
