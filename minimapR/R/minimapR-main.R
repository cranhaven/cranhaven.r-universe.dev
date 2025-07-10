## minimapR-main.R
## LICENSE: MIT License

## This is a the main wrapper function for the command line tool 
## minimap2.
#' @title minimap2
#'
#' @description
#' This function is a wrapper for the command line tool minimap2. 
#' minimap2 is a long read sequencing alignment tool that is
#' used to align long reads to a reference genome.
#'
#' @param reference Reference genome to align the query sequences
#' @param query_sequences Query sequences to align to the reference genome
#' @param output_file_prefix Output file to save the alignment results
#' @param a Logical value to use the preset string with the -a flag
#' @param preset_string Preset string to use with the -x flag
#' @param threads Number of threads to use
#' @param return Logical value to return the alignment results
#' @param verbose Logical value to print progress of the installation
#' @param ... Additional arguments to pass to minimap2
#'
#' @return This function returns the line needed to add minimap2 to PATH
#' 
#' @examples
#' \dontrun{
#' reference <- system.file("extdata/S288C_ref_genome.fasta.gz", package = "minimapR")
#' query_sequences <- system.file("extdata/yeast_sample_hifi.fastq.gz", package = "minimapR")
#' # Warning: not setting output_file_prefix will generate the output in the current working directory
#' bam_out <- minimap2(reference, 
#'  query_sequences, 
#'  threads = 4,
#'  preset_string = "map-hifi", 
#'  return = TRUE, 
#'  verbose = TRUE)
#' }
#' 
#' @examples
#' \dontrun{
#' reference <- system.file("extdata/GRCh38_chr1_130k.fa.gz", package = "minimapR")
#' query_sequences <- system.file("extdata/ont_hs_sample.fastq.gz", package = "minimapR")
#' # Warning: not setting output_file_prefix will generate the output in the current working directory
#' bam_out <- minimap2(reference, 
#'  query_sequences, 
#'  threads = 4,
#'  preset_string = "map-hifi",
#'  return = TRUE, 
#'  verbose = TRUE)
#' }
#' 
#' @export
#' @import Rsamtools
#' @import pafr
minimap2 <- function(reference, 
    query_sequences, 
    output_file_prefix = "minimap2_out", 
    a = TRUE,
    preset_string = "map-hifi",
    threads = 1, 
    return = FALSE,
    verbose = TRUE, 
    ...) {

    # Get the path to the minimap2 executable
    mini_path <- Sys.which("minimap2")
    st_path <- Sys.which("samtools")

    # Check if the output file prefix is a directory
    if (!dir.exists(dirname(output_file_prefix))) {
        message("Output directory does not exist. Creating directory...")
        dir.create(dirname(output_file_prefix))
    }

    # Output files
    if (verbose) {
        message("Generating output file: ", output_file_prefix)
    }
    file.create(paste0(output_file_prefix, ".sam"))
    output_sam <- paste0(output_file_prefix, ".sam")
    output_bam <- paste0(output_file_prefix, ".bam")
    output_paf <- paste0(output_file_prefix, ".paf")

    # Check if minimap2 is installed
    if (file.exists(mini_path) && file.exists(st_path)) {
        # Run minimap2
        if (verbose) {
            message("Running minimap2...")
        }
        
        # Run minimap2
        if (a) {
            if (verbose) {
                message("Running minimap with the following command:\n")
                message(paste0(mini_path, " -ax ", preset_string, 
                    " -t ", threads, " ", reference, 
                    " ", query_sequences, " -o ", output_sam, 
                    " ", ...))
            }
            system(paste0(mini_path, " -ax ", preset_string, 
                " -t ", threads, " ", reference, 
                " ", query_sequences, " -o ", output_sam, 
                " ", ...), intern = verbose, 
                ignore.stdout = !verbose, 
                ignore.stderr = !verbose)
            system(paste0(st_path, " view -bS ", output_sam, " -o ", output_bam), 
                intern = verbose, 
                ignore.stdout = !verbose, 
                ignore.stderr = !verbose)
            system(paste0(st_path, " sort -o ", output_bam, " ", output_bam), 
                intern = verbose, 
                ignore.stdout = !verbose, 
                ignore.stderr = !verbose)

        if (return == TRUE) {
            bam_f <- Rsamtools::BamFile(output_bam)
            ret <- as.data.frame(Rsamtools::scanBam(bam_f))
            }

        } else {
            if (verbose) {
                message("Running minimap with the following command:\n")
                message(paste0(mini_path, " -x ", preset_string, 
                    " -t ", threads, " ", reference, 
                    " ", query_sequences, " -o ", output_sam, 
                    " ", ...))
            }
            system(paste0(mini_path, " -x ", preset_string, 
                " -t ", threads, " ", reference, 
                " ", query_sequences, " -o ", output_paf, 
                " ", ...), 
                intern = verbose,
                ignore.stdout = !verbose,
                ignore.stderr = !verbose)

            if (return == TRUE) {
                ret <- pafr::read_paf(output_paf)
            }
        }

        # Return the output
        if (return == TRUE) {
            return(ret)
        }

    } else {
        if (verbose) {
        message("minimap2 or samtools is not installed.",
                "\nPlease install minimap2 and samtools. ", 
                "\n\t On linux run: minimap2_installation.",
                "\n\t On Windows follow the output from running minimap2_installation." )
        }
    }
}