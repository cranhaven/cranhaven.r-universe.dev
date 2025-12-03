#' Calculate Genetic Interaction scores
#' @description Create results table that has CRISPR scores, Wilcoxon rank-sum
#' test and t tests.
#' The output of the `gimap` package is genetic interaction scores which _is the
#' distance between the observed CRISPR score and the expected CRISPR score._
#' The expected CRISPR scores are what we expect for the CRISPR values should
#' two genes be unrelated to each other. The further away an observed CRISPR
#' scoreis from its expected the more we suspect genetic interaction.
#' This can be true in a positive way (a CRISPR knockout pair caused more cell
#' proliferation than expected) or in a negative way (a CRISPR knockout pair
#' caused more cell lethality than expected).
#'
#' The genetic interaction scores are based on a linear model calculated for
#' each sample where `observed_crispr_single` is the outcome variable and
#' `expected_crispr_single` is the predictor variable.
#' For each sample: lm(observed_crispr_single ~ expected_crispr_single)
#'
#' Using `y = mx+b`, we can fill in the following values:
#' * `y` = observed CRISPR score
#' * `x` = expected CRISPR score
#' * `m` = slope from linear model for this sample
#' * `b` = intercept from linear model for this sample
#'
#' The intercept and slope from this linear model are used to adjust the CRISPR
#' scores for each sample:
#' single target gi score =
#'   observed single crispr - (intercept + slope * expected single crispr)
#' double_target_gi_score =
#'   double crispr score - (intercept + slope * expected double crispr)
#' These single and double target genetic interaction scores are calculated at
#' the construct level and are then summarized using a t-test to see if the the
#' distribution of the set of double targeting constructs is significantly
#' different than the overall distribution single targeting constructs.
#' After multiple testing correction, FDR values are reported.
#' Low FDR value for a double construct means high suspicion of paralogs.
#' @param .data Data can be piped in with tidyverse pipes from function to
#' function. But the data must still be a gimap_dataset
#' @param gimap_dataset A special dataset structure that is setup using the
#' `setup_data()` function.
#' @param use_lfc Should Log fold change be used to calculate GI scores instead
#' of CRISPR scores? If you do not have negative controls or CRISPR scores you
#' will need to set this to TRUE.
#' @return A gimap dataset with statistics and genetic interaction scores
#' calculated. Overall results in the returned object can be obtained using
#' gimap_dataset$overall_results Whereas target level genetic interaction
#' scores can be retrieved using `gimap_dataset$gi_scores`.
#' @import dplyr
#' @importFrom stats lm
#' @export
#' @examples \donttest{
#'
#' gimap_dataset <- get_example_data("gimap",
#'   data_dir = tempdir()
#' ) %>%
#'   gimap_filter() %>%
#'   gimap_annotate(
#'     cell_line = "HELA",
#'     annot_dir = tempdir()
#'   ) %>%
#'   gimap_normalize(
#'     timepoints = "day",
#'     missing_ids_file = tempfile()
#'   ) %>%
#'   calc_gi()
#'
#' saveRDS(gimap_dataset, file.path(tempdir(), "gimap_dataset_final.RDS"))
#' }
calc_gi <- function(.data = NULL,
                    gimap_dataset,
                    use_lfc = FALSE) {
  # Summary the calculation
  # single_target_crispr_1 = geneA_nt1, geneA_nt2...
  # single_target_crispr_2 = nt1_geneB, nt2_geneB...
  # double_crispr_score = geneA_geneBpg1, geneA_geneBpg2...

  # mean_double_control_crispr = mean for the same control sequence

  # expected_crispr_double=single_target_crispr_1 + single_target_crispr_2
  # expected_crispr_single_1=single_target_crispr_1 + mean_double_control_crispr
  # expected_crispr_single_2=single_target_crispr_2 + mean_double_control_crispr

  # linear model is with all samples:
  # lm(mean_observed_single_crispr ~ mean_expected_single_crispr)

  # single_target_gi_score = mean_observed_single_crispr -
  #      (intercept + slope * mean_expected_single_crispr)
  # double_target_gi_score = mean_double_crispr_score -
  #      (intercept + slope * mean_expected_double_crispr)

  # Code adapted from
  # https://github.com/FredHutch/GI_mapping/blob/main/workflow/scripts/
  # 04-calculate_GI_scores.Rmd

  if (!is.null(.data)) gimap_dataset <- .data

  op <- options("R_MAX_VSIZE" = 32000000000)
  on.exit(options(op))

  if (!("gimap_dataset" %in% class(gimap_dataset))) {
    stop(
      "This function only works",
      " with gimap_dataset objects which can be made with the",
      " setup_data() function."
    )
  }

  if (is.null(gimap_dataset$normalized_log_fc)) {
    stop(
      "This function only works",
      "with already normalized gimap_dataset objects",
      "which can be done with the gimap_normalize() function."
    )
  }
  lfc_adj <- gimap_dataset$normalized_log_fc

  if (!"crispr_score" %in% colnames(lfc_adj)) {
    if (!use_lfc) {
      stop("No CRISPR scores found, you can set use_lfc = TRUE to calculate
         genetic interaction scores using log fold change")
    }
  }
  if (use_lfc) {
    lfc_adj$crispr_score <- lfc_adj$lfc
    message("Calculating Genetic Interaction scores using LFC")
  }

  #### STEP 1 CALCULATE EXPECTED VALUES AND COLLAPSE TO MEANS
  # Get mean control target CRISPR scores -- they will be used for expected
  # calculations
  control_target_df <- lfc_adj %>%
    dplyr::filter(target_type == "ctrl_ctrl") %>%
    tidyr::pivot_longer(
      cols = c(gRNA1_seq, gRNA2_seq),
      names_to = "position",
      values_to = "control_gRNA_seq"
    ) %>%
    # If there's the same control sequence, and rep
    dplyr::group_by(pgRNA_target, control_gRNA_seq, norm_ctrl_flag) %>%
    # Then take the mean for when controls have the same sequence
    dplyr::summarize(
      mean_double_control_crispr =
        mean(crispr_score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::select(
      pgRNA_target,
      control_gRNA_seq,
      mean_double_control_crispr,
      norm_ctrl_flag,
    )
  # This means we have a mean double control crispr for each control sequence

  # Calculate expected and mean CRISPR scores for single targets
  single_crispr_df <- lfc_adj %>%
    dplyr::filter(target_type %in% c("ctrl_gene", "gene_ctrl")) %>%
    # We will be joining things based on the gRNA sequences so
    # we do some recoding here
    mutate(
      targeting_gRNA_seq = case_when(
        target_type == "gene_ctrl" ~ gRNA1_seq,
        target_type == "ctrl_gene" ~ gRNA2_seq
      ),
      control_gRNA_seq = case_when(
        target_type == "gene_ctrl" ~ gRNA2_seq,
        target_type == "ctrl_gene" ~ gRNA1_seq
      ),
      gene_symbol = dplyr::case_when(
        target_type == "gene_ctrl" ~ gene1_symbol,
        target_type == "ctrl_gene" ~ gene2_symbol
      ),
    ) %>%
    dplyr::left_join(control_target_df,
      by = c("control_gRNA_seq" = "control_gRNA_seq"),
      suffix = c("", "_control")
    ) %>%
    group_by(pgRNA_target, targeting_gRNA_seq) %>%
    # Taking the mean of the single target crisprs that have the same
    # targeting sequence
    summarize(
      mean_single_crispr = mean(crispr_score, na.rm = TRUE),
      # Note this is now the mean of all the control sequences for a particular target
      mean_double_control_crispr = mean(mean_double_control_crispr),
      .groups = "drop"
    ) %>%
    dplyr::select(
      pgRNA_target,
      targeting_gRNA_seq,
      mean_single_crispr,
      mean_double_control_crispr
    ) %>%
    ## calculate expected single-targeting GI score by summing the
    ## single-targeting and the mean_double_control_crispr
    dplyr::mutate(
      expected_single_crispr = mean_single_crispr + mean_double_control_crispr,
    )
  # TODO: Ask alice do we want to combine target sequences as well?

  # This will be added on to the double crispr df
  expected_single_crispr_df <- single_crispr_df %>%
    dplyr::select(pgRNA_target, targeting_gRNA_seq, mean_single_crispr) %>%
    dplyr::distinct()

  # Now put it all together into one df
  double_crispr_df <- lfc_adj %>%
    dplyr::filter(target_type == "gene_gene") %>%
    dplyr::select(
      pg_ids,
      rep,
      crispr_score,
      gRNA1_seq,
      gRNA2_seq,
      pgRNA_target
    ) %>%
    dplyr::left_join(expected_single_crispr_df,
      by = c("gRNA1_seq" = "targeting_gRNA_seq"),
      suffix = c("", "_1")
    ) %>%
    dplyr::left_join(expected_single_crispr_df,
      by = c("gRNA2_seq" = "targeting_gRNA_seq"),
      suffix = c("", "_2")
    ) %>%
    dplyr::select(
      pg_ids,
      double_crispr = crispr_score,
      gRNA1_seq,
      gRNA2_seq,
      pgRNA_target,
      mean_single_crispr_1 = mean_single_crispr,
      mean_single_crispr_2
    ) %>%
    dplyr::mutate(
      expected_double_crispr = mean_single_crispr_1 + mean_single_crispr_2
    )
  # TODO: Ask Alice: Should there be by reps at this point?

  #### STEP 2 LINEAR MODEL AND GI SCORE CALC

  message("Calculating Genetic Interaction scores")

  # Calculate the linear models from this
  # Previously we had a dplyr::group_by(rep) %>% here because we made linear models
  # by each rep but we found doing a single linear model made more sense for
  # expectation calculations
  gimap_dataset$linear_model <- lm(mean_single_crispr ~ expected_single_crispr,
    data = single_crispr_df
  )

  # Pull out the intercept and slope
  calc_slope <- gimap_dataset$linear_model$coefficients[["expected_single_crispr"]]
  calc_intercept <- gimap_dataset$linear_model$coefficients[["(Intercept)"]]

  # Do the linear model adjustments
  gi_calc_single <- single_crispr_df %>%
    # Previously GI scores were calculated on a per construct basis but then we
    # realized a mean_expected should be used because it is hypothetically closer
    # to the expectation for the target overall.
    dplyr::group_by(pgRNA_target) %>%
    dplyr::summarize(
      mean_expected_cs = mean(expected_single_crispr, na.rm = TRUE),
      mean_observed_cs = mean(mean_single_crispr, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      gi_score = mean_observed_cs -
        (calc_intercept + calc_slope * mean_expected_cs)
    )

  # Do the linear model adjustments but don't collapse double
  gi_calc_double <- double_crispr_df %>%
    dplyr::group_by(pg_ids, pgRNA_target) %>%
    dplyr::summarize(
      mean_expected_cs = mean(expected_double_crispr, na.rm = TRUE),
      mean_observed_cs = mean(double_crispr, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Using the single target's linear model here
    dplyr::mutate(
      gi_score = mean_observed_cs -
        (calc_intercept + calc_slope * mean_expected_cs)
    )
  # TODO: Check with Alice.
  # We need at least more than one data point per target but currently have
  # All 16 constructs going in where each is a mean of the reps


  #### STEP 3: RUN THE TESTS
  target_results_df <- gimap_stats(
    gi_calc_single = gi_calc_single,
    gi_calc_double = gi_calc_double
  )

  #### STEP 4: FORMATTING DATA FOR EASY STORING
  gi_calc_double_clean <- gi_calc_double %>%
    # Collapse to just stats and don't care about pg_ids anymore
    dplyr::select(
      pgRNA_target,
      mean_expected_cs,
      mean_observed_cs,
      gi_score
    ) %>%
    dplyr::group_by(
      pgRNA_target
    ) %>%
    # Collapse to one mean per target now that we've tested
    dplyr::summarize(
      mean_observed_cs = mean(mean_observed_cs, na.rm = TRUE),
      mean_expected_cs = mean(mean_expected_cs, na.rm = TRUE),
      gi_score = mean(gi_score, na.rm = TRUE),
      target_type = "gene_gene"
    )

  gi_calc_single_clean <- gi_calc_single %>%
    dplyr::mutate(target_type = dplyr::case_when(
      grepl("^ctrl_*", pgRNA_target) ~ "ctrl_gene",
      grepl("*_ctrl$", pgRNA_target) ~ "gene_ctrl"
    )) %>%
    dplyr::select(
      target_type,
      pgRNA_target,
      mean_expected_cs,
      mean_observed_cs,
      gi_score
    ) %>%
    dplyr::distinct()

  all_gi_scores <- dplyr::bind_rows(gi_calc_double_clean, gi_calc_single_clean)

  # Add on test results
  all_gi_scores <- all_gi_scores %>%
    dplyr::left_join(target_results_df,
      by = c("pgRNA_target")
    )

  # Store the useful bits
  gimap_dataset$gi_scores <- all_gi_scores

  if (!use_lfc) {
    # Save at the target level but call them crispr scores
    gimap_dataset$crispr_score$single_crispr_score <- single_crispr_df
    gimap_dataset$crispr_score$double_crispr_score <- double_crispr_df
    gimap_dataset$crispr_score$neg_control_crispr <- control_target_df
  } else {
    # Save at the target level but they aren't crispr scores
    colnames(single_crispr_df) <- gsub("crispr", "lfc", colnames(single_crispr_df))
    colnames(double_crispr_df) <- gsub("crispr", "lfc", colnames(double_crispr_df))
    colnames(control_target_df) <- gsub("crispr", "lfc", colnames(control_target_df))

    gimap_dataset$lfc$single_lfc <- single_crispr_df
    gimap_dataset$lfc$double_lfc <- double_crispr_df
    gimap_dataset$lfc$neg_control_crispr <- control_target_df

    # The final results also aren't crispr scores
    gimap_dataset$gi_scores <- gimap_dataset$gi_scores %>%
      dplyr::rename(
        mean_expected_lfc = mean_expected_cs,
        mean_observed_lfc = mean_observed_cs
      )
  }

  return(gimap_dataset)
}


#' Do tests --an internal function used by calc_gi() function
#' @description Create results table that has t test p values
#' @param gi_calc_single a data.frame with adjusted single gi scores
#' @param gi_calc_double a data.frame with adjusted double gi scores
#' @param replicate a name of a replicate to filter out from gi_calc_adj Optional
#' @importFrom stats p.adjust t.test wilcox.test
gimap_stats <- function(gi_calc_double, gi_calc_single, replicate = NULL) {
  ## get a vector of GI scores for all single-targeting ("control") pgRNAs
  ## for each rep
  ## get double-targeting pgRNAs for this rep, do a t-test to compare the
  ## double-targeting GI scores for each paralog pair to the control vector
  ## adjust for multiple testing using the Benjamini-Hochberg method

  gi_scores <- gi_calc_double %>%
    group_by(pgRNA_target) %>%
    mutate(p_val = t.test(
      x = gi_calc_single$gi_score,
      y = gi_score, # all construct guides here
      paired = FALSE
    )$p.value)


  ## adjust for multiple testing using the Benjamini-Hochberg method
  d_p_val <- gi_scores %>%
    dplyr::select(pgRNA_target, p_val) %>%
    arrange(p_val) %>%
    distinct(p_val, .keep_all = TRUE)

  fdr_vals <- p.adjust(d_p_val$p_val, method = "BH")

  d_fdr <- tibble("fdr" = fdr_vals) %>%
    bind_cols(d_p_val) %>%
    dplyr::select(pgRNA_target, p_val, fdr)

  return(d_fdr)
}
