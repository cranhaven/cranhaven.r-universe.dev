#' @title \code{exportSmoothedInsertions}
#'
#' @description \code{exportSmoothedInsertions} Takes a SampleTileMatrix with
#'   linked insertion files and applies a smoothing filter (a rolling sum then
#'   rolling median) to the insertions, finally exporting the smoothed insertion
#'   files to bigwig format.
#'
#' @param SampleTileObj A MultiAssayExperiment or RangedSummarizedExperiment
#'   from MOCHA
#' @param cellPopulation A string denoting the cell population of interest
#' @param outDir Directory to write output bigwig files. Default is NULL, where
#'   the directory in `SampleTileObj@metadata$Directory` will be used.
#' @param sumWidth Window size for rolling sum in basepairs. Default is 10.
#' @param medianWidth Window size for rolling median in basepairs. Must be odd.
#'   Default is 11.
#' @param force Set TRUE to overwrite existing files. Default is FALSE.
#' @param slow Set TRUE to bypass optimisations and compute smoothing filter
#'   directly on the whole genome. May run slower and consume more RAM. Default
#'   is FALSE.
#' @param verbose Set TRUE to display additional messages. Default is FALSE.
#'
#' @return outPaths List of paths of exported insertion files
#'
#' @examples
#' \dontrun{
#' # Depends on and manipulates files on filesystem
#' outPath <- MOCHA::exportSmoothedInsertions(
#'   SampleTileObj,
#'   cellPopulation = "CD4 Naive", sumWidth = 10, medianWidth = 11, verbose = FALSE
#' )
#' }
#'
#' @export
#' 
exportSmoothedInsertions <- function(SampleTileObj,
                                     cellPopulation,
                                     outDir = NULL,
                                     sumWidth=10, 
                                     medianWidth=11,
                                     force=FALSE,
                                     slow=FALSE,
                                     verbose=FALSE
                                    ){
    if (!requireNamespace("zoo", quietly = TRUE)) {
      stop(
        "Package 'zoo' is required for exportSmoothedInsertions. ",
        "Please install 'zoo' to proceed."
      )
    }
    score <- start <- seqnames <- NULL
    if (!any(names(SummarizedExperiment::assays(SampleTileObj)) %in% cellPopulation)) {
        stop("cellPopulation was not found within SampleTileObj. Check available cell populations with `colData(SampleTileObj)`.")
    }

    sourcedir <- SampleTileObj@metadata$Directory
    
    if (is.null(outDir)){
        outdir <- SampleTileObj@metadata$Directory
    } else if (!dir.exists(outDir)){
        dir.create(outDir)
    }
    
    covFile <- file.path(sourcedir, paste0(cellPopulation, "_CoverageFiles.RDS"))
    if (!file.exists(covFile)){
        stop("Coverage file ", covFile, " for could not be found. ",
            "Please ensure that the directory given by `SampleTileObj@metadata$Directory` exists and contains coverage files.", 
            " To preserved linked files when sharing MOCHA objects, ", 
            " use `packMOCHA()` and `unpackMOCHA()`.")
    }
    
    insertionsList <- readRDS(covFile)
    if (!"Insertions" %in% names(insertionsList)) {
        stop("No insertions found in the file `", covFile, "`.",
             " Please run peak calling with the latest MOCHA version,", 
             " 1.0.1 or greater.")
    }
    
    insertionsGRangesList <- insertionsList$Insertions
    outPaths <- list()
    for (sampleName in names(insertionsGRangesList)) {
        sample_start <- Sys.time()
        
        type <- paste0("sum", sumWidth,"_median", medianWidth)
        outfile <- file.path(outdir, paste0(
            cellPopulation, "__", sampleName, "__", type, ".bw"
        ))
        
        if (file.exists(outfile) && !force) {
            if (verbose) { message("Output file already exists: ", outfile) }
            next # Skip this one!
        }
        if (verbose) { message("Smoothing insertions for ", outfile, " ... ") }

        # Load in the raw insertions
        InsertionsGRanges <- insertionsGRangesList[[sampleName]]

        filtIns <- plyranges::filter(InsertionsGRanges, score!=0)
        
        # Operate one chromosome at a time to avoid R object size limits
        allChrGR <- GenomicRanges::GRanges()
        i = 0
        pb <- utils::txtProgressBar(min = i, max = length(levels(GenomicRanges::seqnames(filtIns))), style = 3)
        for (chr in levels(GenomicRanges::seqnames(filtIns))) { # RAM usage maxes at 24GB
            i = i+1
            utils::setTxtProgressBar(pb, value = i, title = NULL, label = NULL)


            chrIns <- plyranges::filter(filtIns, seqnames==chr)

            gpos <- GenomicRanges::GPos(chrIns)
            x <- rep(GenomicRanges::score(chrIns), GenomicRanges::width(chrIns))
            
            if (!slow) { # slow=FALSE by default to use these optimizations below

              ######## Expand insertions to selectively keep zeroes ########
              gpos$score <- x
  
              joinedGR <- GenomicRanges::GRanges(gpos)
  
              # Create GRanges of windows where insertions are <10bp apart
              windowsGR <- joinedGR %>% plyranges::stretch(extend = 2*max(sumWidth, medianWidth)) %>%
                              plyranges::reduce_ranges()
  
              # Create GRanges of zeroes back zeroes
              zeroesGR <- plyranges::compute_coverage(joinedGR) %>% plyranges::filter(score==0)
  
              # Filter zeroesGR to just zeroes in the windows
              zeroesGR <- plyranges::join_overlap_intersect(zeroesGR, windowsGR)
  
              # Merge the insertion GRanges with the filtered zeroes, and sort by start pos
              joinedGR <- plyranges::bind_ranges(joinedGR, zeroesGR) %>% plyranges::arrange(start)
  
              # Transform back to GPos for smoothing filter
              gpos <- GenomicRanges::GPos(joinedGR)
              x <- rep(GenomicRanges::score(joinedGR), GenomicRanges::width(joinedGR))
              gpos$score <- x
  
              rm(joinedGR)
              
            }

            ######## Transform with smoothing filter ######## 

            newx <- zoo::rollsum(x, k=sumWidth, align = 'center', fill = 0)
            newx <- zoo::rollmedian(newx, k=medianWidth, align = 'center', fill = 0)
            gpos$score <- newx

            # Append to allChrGR
            allChrGR <- c(allChrGR, GenomicRanges::GRanges(gpos))

            rm(gpos)
            end <- Sys.time()

        }

        if (verbose) { message("Writing to bigwig ", outfile, " ... ") }
        
        result <- tryCatch({
            plyranges::write_bigwig(allChrGR, outfile)
        }, warning = function(w) {
            message(w)
        }, error = function(e) {
            message("Error occurred writing the bigwig for sample `", sampleName, "` :")
            message(e)
            file.remove(outfile)
            return(allChrGR)
        }, finally = {
            end <- Sys.time()
            outPaths <- append(outPaths, outfile)
        })

        end <- Sys.time()
        if (verbose) { 
            message(round(as.numeric(difftime(time1 = end, time2 = sample_start, units = "secs")), 3), " Seconds")
        }
    }
  return(outPaths)
}