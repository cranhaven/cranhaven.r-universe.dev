#' Convert run-length encodings (RLEs) to a data frame.
#'
#' Converts a list of run-length encodings (RLEs) into a data frame.
#'
#' The returned dataframe contains 16 features for training a machine learning model.
#' after mappability profiling and nucleotide filtering.
#'
#' @examples
#' \dontrun{
#' df <- convert_rle_to_df(example_cov)
#' }
#'
#' @description Converts a list of run-length encodings (RLEs) into a data frame
#'     with 16 features after mappability profiling and nucleotide filtering.
#'
#' @param covs A list of Coverage profile(s) in RLE format. Can be one or more
#'     samples.
#' @param unreliable_region_version The version number (character string) of unreliable regions of the virus segments.
#'     Default is `1_4_0`. It includes the mappability profile from
#'     a host genome (we only have Arabidopsis thaliana right now) and virus
#'     references, and the regions that have CG% and A% over 60% and 45%
#'     respectively.
#' @param unreliable_region_enabled Default is `TRUE`. If `TRUE`, the input will be
#'     checked against `unreliable_region_df`. If `FALSE`, this step will be
#'     skipped.
#' @param additional_nucleotide_info Additional nucleotide information for virus
#'     segments that are not included in `nucleotide_info`. The information
#'     provided must be a data frame that follows the format of
#'     `nucleotide_info`. Default is an empty data frame.
#' @returns A data frame object that contains the mapping result for each virus
#'     segment that the plant sample reads are aligned to and a RLE list of
#'     coverage information.
#' @export
convert_rle_to_df <- function(covs,
                              unreliable_region_version = "1_4_0",
                              unreliable_region_enabled = TRUE,
                              additional_nucleotide_info = data.frame()) {
  if (unreliable_region_enabled == T) {
    if (unreliable_region_version == "1_4_0") {
      unreliable_region_df = unreliable_regions[which(unreliable_regions$`1_4_0` == TRUE), ]
    } else if (unreliable_region_version == "1_5_0") {
      unreliable_region_df = unreliable_regions[which(unreliable_regions$`1_5_0` == TRUE), ]
    }
    for (sample in names(covs)) {
      for (seg in names(covs[[sample]])) {
        unreliable_regions_seg <-
          unreliable_region_df[which(unreliable_region_df$`Virus segment` == seg),]

        if (nrow(unreliable_regions_seg) != 0) {
          for (ii in 1:nrow(unreliable_regions_seg)) {
            start <- unreliable_regions_seg$Start[ii]
            end <- unreliable_regions_seg$End[ii]

            covs[[sample]][[seg]][c(start:end)] <- 0
          }
        }
      }
    }
  }

  column_names <- c(
    "seg_id",
    "iso_id",
    "virus_name",
    "sample_id",
    "A_percent",
    "C_percent",
    "T_percent",
    "GC_percent",
    "avg_cov",
    "max_cov",
    "seg_len",
    "cov_2_percent",
    "cov_3_percent",
    "cov_4_percent",
    "cov_5_percent",
    "cov_6_percent",
    "cov_7_percent",
    "cov_8_percent",
    "cov_9_percent",
    "cov_10_percent"
  )

  model_data <-
    data.frame(matrix(ncol = length(column_names), nrow = 0))

  colnames(model_data) <- column_names

  max_cov <- vector()
  mean_cov <- vector()

  nucleotide_info <-
    rbind(additional_nucleotide_info, nucleotide_info)

  for (sample in names(covs)) {
    for (seg in names(covs[[sample]])) {
      isolate_id <- nucleotide_info[nucleotide_info$seg_id == seg, 2]
      v_name <- nucleotide_info[nucleotide_info$seg_id == seg, 1]
      sample_id <- sample
      seg_id <- seg
      seg_length <-
        nucleotide_info[nucleotide_info$seg_id == seg, 8]
      a_content <-
        nucleotide_info[nucleotide_info$seg_id == seg, 4]
      c_content <-
        nucleotide_info[nucleotide_info$seg_id == seg, 5]
      t_content <-
        nucleotide_info[nucleotide_info$seg_id == seg, 6]
      gc_content <-
        nucleotide_info[nucleotide_info$seg_id == seg, 7]
      max_val <- max(covs[[sample]][[seg]])
      mean_val <- mean(covs[[sample]][[seg]])
      max_cov <- append(max_cov, max_val)
      mean_cov <- append(mean_cov, mean_val)

      idx2 <- covs[[sample]][[seg]]@values > 2
      idx3 <- covs[[sample]][[seg]]@values > 3
      idx4 <- covs[[sample]][[seg]]@values > 4
      idx5 <- covs[[sample]][[seg]]@values > 5
      idx6 <- covs[[sample]][[seg]]@values > 6
      idx7 <- covs[[sample]][[seg]]@values > 7
      idx8 <- covs[[sample]][[seg]]@values > 8
      idx9 <- covs[[sample]][[seg]]@values > 9
      idx10 <- covs[[sample]][[seg]]@values > 10

      percent_2 <-
        sum(covs[[sample]][[seg]]@lengths[idx2]) / sum(covs[[sample]][[seg]]@lengths)
      percent_3 <-
        sum(covs[[sample]][[seg]]@lengths[idx3]) / sum(covs[[sample]][[seg]]@lengths)
      percent_4 <-
        sum(covs[[sample]][[seg]]@lengths[idx4]) / sum(covs[[sample]][[seg]]@lengths)
      percent_5 <-
        sum(covs[[sample]][[seg]]@lengths[idx5]) / sum(covs[[sample]][[seg]]@lengths)
      percent_6 <-
        sum(covs[[sample]][[seg]]@lengths[idx6]) / sum(covs[[sample]][[seg]]@lengths)
      percent_7 <-
        sum(covs[[sample]][[seg]]@lengths[idx7]) / sum(covs[[sample]][[seg]]@lengths)
      percent_8 <-
        sum(covs[[sample]][[seg]]@lengths[idx8]) / sum(covs[[sample]][[seg]]@lengths)
      percent_9 <-
        sum(covs[[sample]][[seg]]@lengths[idx9]) / sum(covs[[sample]][[seg]]@lengths)
      percent_10 <-
        sum(covs[[sample]][[seg]]@lengths[idx10]) / sum(covs[[sample]][[seg]]@lengths)

      new_row <- c(
        seg_id,
        isolate_id,
        v_name,
        sample_id,
        a_content,
        c_content,
        t_content,
        gc_content,
        mean_val,
        max_val,
        seg_length,
        percent_2,
        percent_3,
        percent_4,
        percent_5,
        percent_6,
        percent_7,
        percent_8,
        percent_9,
        percent_10
      )

      model_data <- rbind(model_data, new_row)
    }
  }

  for (i in colnames(model_data[,-c(1:4)])) {
    model_data[[i]] <- as.numeric(model_data[[i]])
  }

  model_data
}
