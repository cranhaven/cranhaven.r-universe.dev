#' @title create_mappability_profile
#'
#' @export
#' @importFrom Rsamtools BamFile
#' @importFrom Rsamtools scanBam
#' @importFrom R.utils seqToIntervals
#' @importFrom stringr str_split
#' @importFrom stringr fixed
#'
#' @examples
#' \dontrun{
#' ## Please change the path to your folder where you store the mapped viruses
#' mappability_profile <- create_mappability_profile("path/to/folder",
#'   category = "Unmappable regions")
#' }
#'
#'
#' @description Creates a data frame of start and end positions of the regions
#'     that are considered unmappable. Unmappable areas indicate that they can
#'     be mapped to another virus segment or a host genome. Note that we only
#'     have Arabidopsis Thaliana as a host.
#'
#' @param path_to_bam_files Path to the folder that stores the indexed and
#'     sorted BAM file(s) (ending in `*.sorted.bam` and `*.bai`).
#' @param category Type of unreliable region you are creating. You can use
#'     categories in the provided `unreliable_regions` data frame or customize
#'     in your own way.
#' @param window The sliding window size of your choice. Default is 75.
#' @param virus_info A DNAStringSet of virus segments. The format should be similar to `virus_segments`.
#'
#' @return A data frame of start and end positions of the regions that are
#'     considered unmappable.


create_mappability_profile <-
  function(path_to_bam_files, category, window = 75, virus_info) {
    bam_files <-
      list.files(
        path = path_to_bam_files,
        pattern = "sorted\\.bam$",
        full.names = TRUE,
        include.dirs = TRUE
      )
    MP_scan <- lapply(bam_files, function(x) {
      bamFile <- BamFile(x)
      aln <- scanBam(bamFile)
      aln <- as.data.frame(aln)
      aln_df <-
        aln[which(aln$rname != sub(pattern = "(.*)\\.sorted.*$",
                                   replacement = "\\1",
                                   basename(x))),]
      if (nrow(aln_df) == 0) {
        aln_df[nrow(aln_df) + 1,] <- NA
      }
      aln_df$seg_id <-
        sub(pattern = "(.*)\\.sorted.*$",
            replacement = "\\1",
            basename(x))
      return(aln_df)
    })

    unreliable_regions = data.frame(matrix(NA, ncol = 4, nrow = 0))

    for (ii in c(1:length(MP_scan))) {
      segment_id <- MP_scan[[ii]]$seg_id[1]
      seq <- virus_info[[segment_id]]

      regions <- rep(0, nchar(seq))

      location <- unlist(lapply(MP_scan[[ii]]$qname, function(x) {
        return(as.numeric(str_split(x, fixed("_"))[[1]][2]))
      }))

      if (!is.na(MP_scan[[ii]]$qname[1])) {
        for (jj in 1:length(location)) {
          idx <- location[jj]
          if (any(regions[idx:(idx + window - 1)] == 0)) {
            regions[idx:(idx + window - 1)] <- 1
          }
        }
      }
      regions <- which(regions == 1)

      region_index <- seqToIntervals(regions)

      for (nn in 1:nrow(region_index)) {
        if (nrow(region_index) != 0) {
          unreliable_regions <- rbind(unreliable_regions,
                                      c(region_index[nn, 1],
                                        region_index[nn, 2],
                                        segment_id,
                                        category))
        }
      }
    }

    colnames(unreliable_regions) = c("Start", "End", "Virus segment", "Categories")

    count_df = data.frame(matrix(0, ncol = 3, nrow = length(virus_info)))
    colnames(count_df) = c("virus_seg",
                           "numbers_being_mapped_to",
                           "percentage_unmappable")
    count_df[, 1] = names(virus_info)
    count_df[,1] = as.factor(count_df[,1])

    for (ii in 1:length(MP_scan)) {
      ref_names = MP_scan[[ii]]$rname
      for (jj in unique(ref_names)) {
        count_df[which(count_df$virus_seg == jj), 2] = count_df[which(count_df$virus_seg == jj), 2] + 1
      }
    }

    unmappable_virus = unreliable_regions[which(unreliable_regions$Categories == "Unmappable region (virus)"),]
    for (nn in 1:nrow(unmappable_virus)) {
      segment_id = unmappable_virus[nn, 3]
      positions_mapped = sum(unmappable_virus[which(unmappable_virus$`Virus segment` == segment_id),]$End - unmappable_virus[which(unmappable_virus$`Virus segment` == segment_id),]$Start + 1)
      percent = positions_mapped/length(virus_info[[segment_id]])
      count_df[which(count_df$virus_seg == segment_id), 3] = percent
    }

    new_unreliable = data.frame(matrix(data = NA, nrow = 0, ncol = 6))
    colnames(new_unreliable) = colnames(unreliable_regions)

    for (seg_id in unique(unreliable_regions$`Virus segment`)) {
      unreliable_seg = unreliable_regions[which(
        unreliable_regions$`Virus segment` == seg_id &
          count_df[which(count_df$virus_seg == seg_id), 3] <= 0.5 &
          count_df[which(count_df$virus_seg == seg_id), 2] <= 10
      ), ]
      new_unreliable = rbind(new_unreliable, unreliable_seg)
    }

    unreliable_regions
  }
