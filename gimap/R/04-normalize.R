#' Normalize Log fold changes
#'
#' @description This calculates the log fold change for a gimap dataset based on
#' the annotation and metadata provided.
#' gimap takes in a counts matrix that represents the number of cells that have
#' each type of pgRNA this data needs some normalization before CRISPR scores
#' and Genetic Interaction scores can be calculated.
#'
#' There are three steps of normalization.
#' 1. `Calculate log2CPM` - First we account for different read depths across
#' samples and transforms data to log2 counts per million reads.
#' `log2((counts / total counts for sample)) * 1 million) + 1)`
#' 2. `Calculate log2 fold change` - This is done by subtracting the log2CPM for
#' the pre-treatment from each sample.  control is what is highlighted.
#' The pretreatment is the day 0 of CRISPR treatment, before CRISPR pgRNAs
#' have taken effect.
#' `log2FC = log2CPM for each sample - pretreament log2CPM`
#'
#' 3. `Normalize by negative and positive controls` - Calculate a negative
#' control median for each sample and a positive control median for each sample
#' and divide each log2FC by this value.
#' log2FC adjusted = log2FC /
#' (median negative control for a sample - median positive control for a sample)
#'
#' @param .data Data can be piped in with a tidyverse pipe from function to
#' function. But the data must still be a gimap_dataset
#' @param gimap_dataset A special dataset structure that is setup using the
#' `setup_data()` function.
#' @param normalize_by_unexpressed TRUE/FALSE crispr data should be normalized
#' so that the median of unexpressed controls is 0. For this to happen set this
#' to TRUE but you need to have added TPM data in the gimap_annotate step using
#' cell_line_annotation or custom_tpm.
#' @param timepoints Specifies the column name of the metadata set up in
#' `$metadata$sample_metadata`
#' that has a factor that represents the timepoints.
#' Timepoints will be made into three categories:
#' plasmid for the earliest time point, early for all middle timepoints and
#' late for the latest timepoints.
#' The late timepoints will be the focus for the calculations. The column used
#' for timepoints must be numeric or at least ordinal.
#' @param treatments Specifies the column name of the metadata set up in
#' `$metadata$sample_metadata`
#' that has a factor that represents column that specifies the treatment applied
#' to each. The replicates will be kept collapsed to an average.
#' @param control_name A name that specifies the data either in the treatments
#' column that should be used as the control. This could be the Day 0 of
#' treatment or an untreated sample.
#' For timepoints testing it will be assumed that the mininmum timepoint
#' is the control.
#' @param adj_method Must be one of three methods as stated by a character string
#' "negative_control_adj" or "no_adjustment". Default is "negative_control_adj"
#' "negative_control_adj" where CRISPR scores will be used for the GI scores
#' "no_adjustment" is where LFC adjusted will be used for the GI scores
#' @param num_ids_wo_annot default is 20; the number of pgRNA IDs to display to
#' console if they don't have corresponding annotation data;
#' ff there are more IDs without annotation data than this number, the output
#' will be sent to a file rather than the console.
#' @param rm_ids_wo_annot default is TRUE; whether or not to filter out pgRNA
#' IDs from the input dataset that don't have corresponding annotation data
#' available
#' @param missing_ids_file If there are missing IDs and a file is saved, where
#' do you want this file to be saved? Provide a file path.
#' @param overwrite Should existing normalized_log_fc data in the gimap_dataset
#' be overwritten?
#' @return A gimap_dataset with normalized log FC as a data frame that can be
#' retrieve by using gimap_dataset$normalized_log_fc. This will contain
#' log2FC adjusted stored in a column named `log_adj` and the CRISPR scores
#' stored in a column named `crispr_score`.
#' genes in the set.
#' @import dplyr
#' @export
#' @examples \donttest{
#'
#' gimap_dataset_org <- get_example_data("gimap") %>%
#'   gimap_filter() %>%
#'   gimap_annotate(cell_line = "HELA") %>%
#'   gimap_normalize(
#'     timepoints = "day",
#'     missing_ids_file = tempfile()
#'   )
#' }
gimap_normalize <- function(.data = NULL,
                            gimap_dataset,
                            normalize_by_unexpressed = TRUE,
                            timepoints = NULL,
                            treatments = NULL,
                            control_name = NULL,
                            adj_method = "negative_control_adj",
                            num_ids_wo_annot = 20,
                            rm_ids_wo_annot = TRUE,
                            missing_ids_file = "missing_ids_file.csv",
                            overwrite = TRUE) {
  # Code adapted from
  # https://github.com/FredHutch/GI_mapping/blob/main/workflow/
  # scripts/03-filter_and_calculate_LFC.Rmd

  if (!is.null(.data)) gimap_dataset <- .data

  if (!("gimap_dataset" %in% class(gimap_dataset))) {
    stop(
      "This function only works with gimap_dataset objects which",
      "can be made with the setup_data() function."
    )
  }
  # Based on log fold change calculations and other handling will go
  # based on the code in:
  # https://github.com/FredHutch/GI_mapping/blob/main/workflow/scripts/
  # 03-filter_and_calculate_LFC.Rmd

  if (!adj_method %in% c("negative_control_adj", "no_negative_control", "no_adjustment")) {
    if (!is.null(gimap_dataset$normalized_log_fc) & !overwrite) {
      stop(
        "Normalization has already been preformed on this dataset.",
        "set overwrite = TRUE if you'd like the existing data to be overwritten."
      )
    }
  }

  if (overwrite) {
    gimap_dataset$normalized_log_fc <- NULL
  }

  if (gimap_dataset$filtered_data$filter_step_run) {
    dataset <- gimap_dataset$filtered_data$transformed_log2_cpm
    pg_ids <- gimap_dataset$filtered_data$metadata_pg_ids$id
  } else {
    dataset <- gimap_dataset$transformed_data$log2_cpm
    pg_ids <- gimap_dataset$metadata$pg_ids
  }

  if (is.null(timepoints) & is.null(treatments)) {
    stop("Either timepoints or treatments must be specified so comparisons can
         be made and stats calculated.")
  }

  ### IF WE HAVE TREATMENTS
  if (!is.null(treatments)) {
    if (!(treatments %in% colnames(gimap_dataset$metadata$sample_metadata))) {
      stop(
        "The column name specified for 'treatments' does not exist in",
        "gimap_dataset$metadata$sample_metadata"
      )
    }

    # Just extract what we are working with
    treatment_vector <- gimap_dataset$metadata$sample_metadata[[treatments]]

    # Check the control is here
    if (!(control_name %in% treatment_vector)) {
      stop(
        "The specified control with the name: '", control_name,
        "' does not exist in specified treatments column: '", treatments,
        "'"
      )
    }

    # What are the comparisons we are doing here?
    treatment_group_names <- setdiff(unique(treatment_vector), control_name)

    # Rename and recode the timepoints variable
    gimap_dataset$metadata$sample_metadata <-
      gimap_dataset$metadata$sample_metadata %>%
      dplyr::mutate(
        comparison = dplyr::case_when(
          treatment_vector == control_name ~ "control",
          TRUE ~ treatment_vector
        ),
        comparison = factor(comparison,
          levels = c("control", treatment_group_names)
        )
      )
  }

  ### IF WE HAVE TIMEPOINTS
  if (!is.null(timepoints)) {
    if (!(timepoints %in% colnames(gimap_dataset$metadata$sample_metadata))) {
      stop(
        "The column name specified for 'timepoints' does not exist in",
        "gimap_dataset$metadata$sample_metadata"
      )
    }

    # Rename and recode the timepoints variable
    gimap_dataset$metadata$sample_metadata <-
      gimap_dataset$metadata$sample_metadata %>%
      # Note that timepoints are extablished as three categories:
      # control, early, or late.
      dplyr::mutate(
        timepoints = !!sym(timepoints),
        comparison = dplyr::case_when(
          timepoints == min(timepoints) ~ "control",
          timepoints == max(timepoints) ~ "late",
          TRUE ~ "early"
        )
      ) %>%
      dplyr::mutate(comparison = factor(comparison,
        levels = c("control", "early", "late")
      ))

    # What are the comparisons we are doing here?
    treatment_group_names <- c("early", "late")
  }

  ### Stop if no annotations
  if (is.null(gimap_dataset$annotation)) {
    stop(
      "No annotations are stored in this gimap_dataset, annotations are needed",
      "so we know what genes should be used as controls.",
      "Please run gimap_annotate() function on your gimap_dataset and then",
      " retry this function."
    )
  }

  message("Normalizing Log Fold Change")

  # Doing some reshaping to get one handy data frame
  lfc_df <- dataset %>%
    as.data.frame() %>%
    dplyr::mutate(pg_ids = pg_ids) %>%
    tidyr::pivot_longer(-pg_ids, values_to = "log2_cpm") %>%
    # Adding on metadata
    dplyr::left_join(
      gimap_dataset$metadata$sample_metadata %>%
        dplyr::select(col_names, comparison),
      by = c("name" = "col_names")
    ) %>%
    tidyr::pivot_wider(
      values_from = "log2_cpm",
      names_from = c(name, comparison)
    )

  ##### Checking for missing ids
  missing_ids <- data.frame(
    missing_ids = setdiff(lfc_df$pg_ids, gimap_dataset$annotation$pgRNA_id)
  )

  if ((nrow(missing_ids) > 0) & (nrow(missing_ids) < num_ids_wo_annot)) {
    message(
      "The following ", nrow(missing_ids),
      " IDs were not found in the annotation data: \n",
      paste0(missing_ids, collapse = ", ")
    )

    if (rm_ids_wo_annot) {
      lfc_df <- lfc_df %>%
        filter(!pg_ids %in% missing_ids$missing_ids)
      message(
        "The input data for the IDs which were not found in the annotation",
        "data has been filtered out and will not be included in the analysis",
        "output."
      )
    } else {
      message(
        "The input data for the IDs which were not found in the annotation",
        " data will be kept throughout the analysis, but any data from the",
        " annotation won't be available for them."
      )
    }
  } else {
    missing_ids_file <- file.path(missing_ids_file)
    readr::write_csv(missing_ids, missing_ids_file)
  }

  ############### Subtract the control column (so either day 0 or pretreatment)

  # This is collapsing multiple controls into on should that occur
  ctrl_mean <- lfc_df %>%
    dplyr::select(dplyr::ends_with("_control")) %>%
    apply(., 1., mean, na.rm = TRUE)

  comparison_df <- lfc_df %>%
    dplyr::mutate_at(
      dplyr::vars(!c(pg_ids, dplyr::ends_with("_control"))), ~ .x - ctrl_mean
    ) %>%
    dplyr::select(!dplyr::matches(pg_ids) & !dplyr::ends_with("_control")) %>%
    dplyr::left_join(gimap_dataset$annotation,
      by = c("pg_ids" = "pgRNA_id")
    ) %>%
    tidyr::pivot_longer(dplyr::ends_with(treatment_group_names),
      names_to = "rep",
      values_to = "lfc"
    )

  ########################### Perform adjustments #############################
  if (normalize_by_unexpressed) {
    stopifnot(
      "For normalize_by_unexpressed to be TRUE you need to have added TPM
      data in the annotation step using cell_line_annotation or custom_tpm" =
        "unexpressed_ctrl_flag" %in% colnames(comparison_df)
    )

    comparison_df <- comparison_df %>%
      dplyr::mutate(
        lfc = lfc - median(comparison_df$lfc[comparison_df$unexpressed_ctrl_flag], na.rm = TRUE)
      )
  }

  if (adj_method == "negative_control_adj") {
    medians_df <- comparison_df %>%
      dplyr::group_by(norm_ctrl_flag, rep) %>%
      dplyr::summarize(median = median(lfc, na.rm = TRUE)) %>%
      tidyr::pivot_wider(
        values_from = median,
        names_from = norm_ctrl_flag
      ) %>%
      dplyr::select(rep, negative_control, positive_control)

    # logFC adjusted = (log2FC - log2FC_negctls) / |log2FC_posctls|
    lfc_adj <- comparison_df %>%
      dplyr::left_join(medians_df, by = "rep") %>%
      dplyr::mutate(
        crispr_score = (lfc - negative_control) /
          (negative_control - positive_control)
      )
    # These should equal 0 and -1
    lfc_adj %>%
      dplyr::group_by(rep, norm_ctrl_flag) %>%
      dplyr::summarize(median_crispr = median(crispr_score)) %>%
      dplyr::filter(norm_ctrl_flag %in% c("negative_control", "positive_control"))
  } else {
    lfc_adj <- comparison_df
  }

  # Save this at the construct level
  gimap_dataset$normalized_log_fc <- lfc_adj

  return(gimap_dataset)
}
