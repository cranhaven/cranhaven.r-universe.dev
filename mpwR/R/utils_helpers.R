
#calculate CV
calculate_CV <- function(input_df,
                         cv_col = c("Retention.time", "Peptide_LFQ", "ProteinGroup_LFQ"),
                         analysis_name = "analysis_name") {

  if (cv_col == "Retention.time") {
    level <- "Precursor.IDs"
  } else if (cv_col == "Peptide_LFQ") {
    level <- "Stripped.Sequence"
  } else if (cv_col == "ProteinGroup_LFQ") {
    level <- "ProteinGroup.IDs"
  }

  #remove empty entries in RT_col
  input_prepared <- input_df %>%
    dplyr::filter(!is.na(!!as.symbol(paste0(cv_col, "_mpwR")))) %>%
    dplyr::filter(!!as.symbol(paste0(cv_col, "_mpwR")) != 0)

  #get precursor with full profiles - present in each run
  full_profile <- get_full_profile(input_df = input_prepared, level = level)

  #get entries with full profile and calculate CV for RT
  output_df <- input_prepared %>%
    dplyr::semi_join(full_profile, by = paste0(level, "_mpwR")) %>%
    dplyr::group_by(.data$Run_mpwR) %>%
    dplyr::distinct(!!as.symbol(paste0(level, "_mpwR")), .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!as.symbol(paste0(level, "_mpwR"))) %>%
    dplyr::mutate(
      !!as.symbol(paste0("CV_", cv_col, "_mpwR")) := stats::sd(!!as.symbol(paste0(cv_col, "_mpwR"))) / mean(!!as.symbol(paste0(cv_col, "_mpwR"))) * 100) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(!!as.symbol(paste0(level, "_mpwR")), .keep_all = TRUE)

  #Add analysis name to CV_RT df
  output_df <- bind_cols(
    "Analysis_mpwR" = rep(analysis_name, each = nrow(output_df)),
    output_df
  )

  return(output_df)
}

#add analysis column to Reports
add_analysis_col <- function(input_df,
                             analysis_name) {
  Report <- cbind(
    "Analysis" = rep(analysis_name, each = nrow(input_df)),
    input_df
  )

  return(Report)
}


#Count for ID Report
generate_level_count <- function(input_df,
                                 level = c("ProteinGroup.IDs", "Protein.IDs", "Peptide.IDs", "Precursor.IDs")) {

  output_df <- input_df %>%
    dplyr::group_by(.data$Run_mpwR, !!as.symbol(paste0(level, "_mpwR"))) %>%
    dplyr::distinct(!!as.symbol(paste0(level, "_mpwR")), .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::count(.data$Run_mpwR) %>%
    dplyr::rename(!!as.symbol(level) := n)

  return(output_df)
}

#reorder ID_Report
reorder_ID_Report <- function(input_df) {
  input_df[, c("Run", "ProteinGroup.IDs", "Protein.IDs", "Peptide.IDs", "Precursor.IDs")]
}

#Tidy peptides.txt, proteinGroupt.txt from MaxQuant #generate_ID_Report
tidy_MQ_pep_pg <- function(input_df) {

  #handle global vars
  . <- NULL

  cols <- input_df %>%
    dplyr::select(contains("Intensity ")) %>%
    dplyr::select(!contains("LFQ")) %>%
    colnames()

  input_df %>%
    tidyr::pivot_longer(., names_to = "Run_mpwR", values_to = "Intensity_value", cols = all_of(cols)) %>% #using intensity cols as run
    dplyr::filter(!is.na(.data$Intensity_value)) %>%
    dplyr::filter(.data$Intensity_value != "") %>%
    dplyr::filter(.data$Intensity_value != 0)

}

#Count for DC Report
generate_DC_count <- function(input_df,
                              level = c("ProteinGroup.IDs", "Protein.IDs", "Peptide.IDs", "Precursor.IDs")) {

  input_df %>%
    dplyr::group_by(.data$Run_mpwR) %>%
    dplyr::distinct(!!as.symbol(paste0(level, "_mpwR")), .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::count(!!as.symbol(paste0(level, "_mpwR"))) %>%
    dplyr::rename(Nr_Appearances = n) %>%
    dplyr::count(.data$Nr_Appearances, .drop = FALSE) %>%
    dplyr::rename(!!as.symbol(level) := n)

}

#Make frame for DC report
make_frame_DC <- function(input_df) {
  tibble::tibble(Nr_Appearances = seq_len(length(unique(input_df$Run_mpwR)))) %>%
    dplyr::mutate(Nr.Missing.Values = length(unique(input_df$Run_mpwR)) - .data$Nr_Appearances)
}

#Join DC levels
join_DC_levels <- function(input_df,
                           prec_df,
                           pep_df,
                           prot_df,
                           pg_df) {
  input_df %>%
    dplyr::left_join(prec_df, by = "Nr_Appearances") %>%
    dplyr::left_join(pep_df, by = "Nr_Appearances") %>%
    dplyr::left_join(prot_df, by = "Nr_Appearances") %>%
    dplyr::left_join(pg_df, by = "Nr_Appearances") %>%
    tidyr::replace_na(list(
      Precursor.IDs = 0,
      Peptide.IDs = 0,
      Protein.IDs = 0,
      ProteinGroup.IDs = 0
    )) %>%
    select(-.data$Nr_Appearances)
}

#Generate DC Report with percentage entries
generate_DC_Report_percentage <- function(input_df) {
  input_df %>%
    dplyr::mutate(
      ProteinGroup.IDs = round((.data$ProteinGroup.IDs / sum(.data$ProteinGroup.IDs)) * 100, digits = 2),
      Protein.IDs = round((.data$Protein.IDs / sum(.data$Protein.IDs)) * 100, digits = 2),
      Peptide.IDs = round((.data$Peptide.IDs / sum(.data$Peptide.IDs)) * 100, digits = 2),
      Precursor.IDs = round((.data$Precursor.IDs / sum(.data$Precursor.IDs)) * 100, digits = 2)) %>%
    tidyr::replace_na(list(
      Precursor.IDs = 0,
      Peptide.IDs = 0,
      Protein.IDs = 0,
      ProteinGroup.IDs = 0
    ))
}

#Generate DC Report with profiles
generate_DC_Report_profile <- function(input_df) {
  input_df <- input_df %>%
    dplyr::mutate(
      Nr.Appearances = (max(.data$Nr.Missing.Values) + 1) - .data$Nr.Missing.Values,
      Nr.Appearances_percentage = round((.data$Nr.Appearances / max(.data$Nr.Appearances)) * 100, digits = 2)
    )

  input_profile <- data.frame(Profile = character(), stringsAsFactors = FALSE)

  for (i in seq_len(nrow(input_df))) {
    if (input_df[i, "Nr.Appearances"] == 1) {
      input_profile[i, "Profile"] <- as.character(c("unique"))
    } else if (input_df[i, "Nr.Appearances"] > 1 & input_df[i, "Nr.Appearances_percentage"] < 50) {
      input_profile[i, "Profile"] <- as.character(c("sparse"))
    } else if (input_df[i, "Nr.Appearances_percentage"] >= 50 & input_df[i, "Nr.Appearances_percentage"] < 100) {
      input_profile[i, "Profile"] <- as.character(c("shared with at least 50%"))
    } else if (input_df[i, "Nr.Appearances_percentage"] == 100) {
      input_profile[i, "Profile"] <- as.character(c("complete"))
    }
  }

  output_df <- cbind(input_df[, c("Nr.Missing.Values", "Precursor.IDs", "Peptide.IDs", "Protein.IDs", "ProteinGroup.IDs")], input_profile)
  return(output_df)
}

#Get full profile entries
get_full_profile <- function(input_df,
                             level = c("ProteinGroup.IDs", "Protein.IDs", "Peptide.IDs", "Precursor.IDs")) {
  input_df %>%
    dplyr::group_by(.data$Run_mpwR) %>%
    dplyr::distinct(!!as.symbol(paste0(level, "_mpwR")), .keep_all = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::count(!!as.symbol(paste0(level, "_mpwR"))) %>%
    dplyr::rename(Nr_Appearances = n) %>%
    dplyr::filter(.data$Nr_Appearances == length(unique(input_df$Run_mpwR)))
}

#Tidy peptides.txt and proteinGroup.txt of MaxQuant for CV_LFQ calculation
tidy_MQ_LFQ <- function(input_df,
                        cv_col = c("Peptide_LFQ", "ProteinGroup_LFQ")) {

  #handle global vars
  . <- NULL

  cols <- input_df %>%
    dplyr::select(starts_with("LFQ intensity ")) %>%
    colnames()

  output_df <- input_df %>%
    tidyr::pivot_longer(., names_to = "Run_mpwR", values_to = paste0(cv_col, "_mpwR"), cols = all_of(cols)) %>%
    dplyr::filter(!is.na(!!as.symbol(paste0(cv_col, "_mpwR")))) %>%
    dplyr::filter(!!as.symbol(paste0(cv_col, "_mpwR")) != "") %>%
    dplyr::filter(!!as.symbol(paste0(cv_col, "_mpwR")) != 0) # also in calculate_CV_LFQ

}

#Percentage for MC_Report
generate_MC_Report_percentage <- function(input_df) {

  input_df$mc_count <- as.numeric(input_df$mc_count)

  input_df %>%
    dplyr::mutate(
      mc_count = round((.data$mc_count / sum(.data$mc_count)) * 100, digits = 2)
    ) %>%
    tidyr::replace_na(list(
      mc_count = 0
    ))
}

#DC - wrap_DC_barplot - get only names for plot != NULL e.g. for Spectronaut - Protein.IDs
#ID - wrap_ID_barplot - get only names for plot != NULL e.g. for Spectronaut - Protein.IDs
get_index <- function(input_df,
                      level) {

  if (nrow(dplyr::filter(input_df, !is.na(!!as.symbol(level)))) == 0) {  #NA entries - Spectronaut Protein.IDs
    FALSE
  } else {
    TRUE
  }
}

#DC for plotting stacked barplots
prepare_stacked_barplot <- function(input_df,
                                    level) {

  input_df %>%
    dplyr::select("Analysis", contains("IDs"), "Profile") %>%
    tidyr::pivot_longer(cols = contains("IDs"), names_to = "level", values_to = "value") %>%
    dplyr::group_by(.data$Analysis, level, .data$Profile) %>%
    dplyr::summarize(
      sum_level = sum(.data$value)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = "level", values_from = "sum_level") %>%
    dplyr::select("Analysis", all_of(level), "Profile")
}

#get median ID from ID Report
get_median <- function(input_df,
                       level = c("Precursor.IDs", "Peptide.IDs", "Protein.IDs", "ProteinGroup.IDs")) {
  input_df %>%
    dplyr::select(!!as.symbol(level)) %>%
    dplyr::summarize(
      median_level = round(stats::median(!!as.symbol(level)), digits = 0)
    ) %>%
    dplyr::rename(!!paste0("Median ", level, " [abs.]") := "median_level")
}

#get full profile from DC Report
get_DC_summary <- function(input_df,
                           metric = c("absolute", "percentage")) {

  output_df <- input_df %>%
    dplyr::filter(.data$Nr.Missing.Values == 0) %>%
    dplyr::rename("Full profile - Precursor.IDs" = "Precursor.IDs",
           "Full profile - Peptide.IDs" = "Peptide.IDs",
           "Full profile - Protein.IDs" = "Protein.IDs",
           "Full profile - ProteinGroup.IDs" = "ProteinGroup.IDs")

  cols <- output_df %>%
    colnames()

  if (metric == "absolute") {
    new_names <- gsub("IDs", replacement = "IDs [abs.]", cols)
  } else if (metric == "percentage") {
    new_names <- gsub("IDs", replacement = "IDs [%]", cols)
  }

  names(output_df) <- new_names
  output_df %>%
    dplyr::select(contains("IDs"))
}

# get absolute number for specific CV threshold
get_CV_IDs <- function(input_df,
                       th_hold,
                       cv_col = c("CV_Retention.time_mpwR", "CV_Peptide_LFQ_mpwR", "CV_ProteinGroup_LFQ_mpwR")) {

  if (cv_col == "CV_Retention.time_mpwR") {
    value <- "Retention time"
    level <- "Precursor"
  } else if (cv_col == "CV_Peptide_LFQ_mpwR") {
    value <- "LFQ"
    level <- "Peptide"
  } else if (cv_col == "CV_ProteinGroup_LFQ_mpwR") {
    value <- "LFQ"
    level <- "Proteingroup"
  }

  if (cv_col == "CV_Peptide_LFQ_mpwR" & sum(stringr::str_detect(string = colnames(input_df), pattern = "CV_Peptide_LFQ_mpwR")) < 1) {

    df <- tibble::tibble(count_CV = NA) %>%
      dplyr::rename(!!paste0(level, ".IDs [abs.] with a CV ", value, " < ", th_hold, " [%]") := .data$count_CV)

    return(df)
  } else if (cv_col == "CV_ProteinGroup_LFQ_mpwR" & sum(stringr::str_detect(string = colnames(input_df), pattern = "CV_ProteinGroup_LFQ_mpwR")) < 1) {

    df <- tibble::tibble(count_CV = NA) %>%
      dplyr::rename(!!paste0(level, ".IDs [abs.] with a CV ", value, " < ", th_hold, " [%]") := .data$count_CV)

    return(df)
  } else {

    #note: input has duplicates removed for respective level
    input_df %>%
      dplyr::filter(!!as.symbol(cv_col) < th_hold) %>%
      dplyr::select(!!as.symbol(cv_col)) %>%
      dplyr::summarize(
        count_CV = n()
      ) %>%
      dplyr::rename(!!paste0(level, ".IDs [abs.] with a CV ", value, " < ", th_hold, " [%]") := .data$count_CV)
  }
}

#Percentage of 0 missed cleavages
get_mc_zero <- function(input_df,
                        metric = c("absolute", "percentage")) {

  if (metric == "absolute") {
    value <- "[abs.]"
  } else if (metric == "percentage") {
    value <- "[%]"
  }

  input_df %>%
    dplyr::filter(.data$Missed.Cleavage == 0) %>%
    dplyr::select("mc_count") %>%
    dplyr::mutate(
      mc_count = as.numeric(.data$mc_count)
    ) %>%
    dplyr::rename(!!paste0("Peptide IDs with zero missed cleavages ", value) := "mc_count")

}

#combine summary functions
get_summary <- function(ID_Report,
                        DC_Report_abs,
                        DC_Report_perc,
                        CV_RT,
                        CV_LFQ_PG,
                        CV_LFQ_Pep,
                        MC_Report_abs,
                        MC_Report_perc,
                        CV_RT_th_hold = 5,
                        CV_LFQ_Pep_th_hold = 20,
                        CV_LFQ_PG_th_hold = 20) {

  #Analysis
  Analysis <- ID_Report %>%
    dplyr::select("Analysis") %>%
    dplyr::distinct()

  cbind(
    Analysis,
    get_median(input_df = ID_Report, level = "ProteinGroup.IDs"),
    get_median(input_df = ID_Report, level = "Protein.IDs"),
    get_median(input_df = ID_Report, level = "Peptide.IDs"),
    get_median(input_df = ID_Report, level = "Precursor.IDs"),
    get_DC_summary(DC_Report_abs, metric = "absolute"),
    get_DC_summary(DC_Report_perc, metric = "percentage"),
    get_CV_IDs(input_df = CV_RT, th_hold = CV_RT_th_hold, cv_col = "CV_Retention.time_mpwR"),
    get_CV_IDs(input_df = CV_LFQ_PG, th_hold = CV_LFQ_PG_th_hold, cv_col = "CV_ProteinGroup_LFQ_mpwR"), #not available for PD
    get_CV_IDs(input_df = CV_LFQ_Pep, th_hold = CV_LFQ_Pep_th_hold, cv_col = "CV_Peptide_LFQ_mpwR"), #not availalbe for PD, DIA-NN
    get_mc_zero(input_df = MC_Report_abs, metric = "absolute"),
    get_mc_zero(input_df = MC_Report_perc, metric = "percentage")
  )
}

#summary data to percentage per metric
get_summary_percentage <- function(input_df) { #NAs changed to 0

  #handle global vars
  . <- NULL

  cols <- input_df %>%
    dplyr::select(-"Analysis") %>%
    colnames()

  input_df[, -1] %>% #mutate_if(is.character...) not possible with Analysis
    dplyr::mutate_if(is.character, as.double) %>% #MC_Report entries as character - No R/K cleavage site
    cbind("Analysis" = input_df[, 1], .) %>%
    tidyr::pivot_longer(., cols = all_of(cols), names_to = "metric", values_to = "results") %>%
    tidyr::replace_na(list(results = 0)) %>%
    dplyr::group_by(.data$metric) %>%
    dplyr::mutate(
      results = round(.data$results / max(.data$results) * 100, digits = 0)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(., id_cols = "Analysis", names_from = "metric", values_from = "results")
}

#radarchart - get values for plotting
#columns + first column
categories <- function(input_df) {

  output_vec <- input_df %>%
    dplyr::select(-"Analysis") %>%
    colnames()

  output_vec <- c(output_vec, output_vec[1])

  return(output_vec)
}

#radarchart
#column entries + entry of first column
values <- function(input_df,
                   row) {
  #handle global vars
  . <- NULL

  val <- input_df %>%
    dplyr::select(-"Analysis")

  val <- val[row, ] %>%
    unlist(., use.names = FALSE)

  add_on <- val[1] #for plotly - to connect dots around cirlce
  val <- append(val, add_on)

  return(val)
}

#Upset
#get unique (not in the sense of proteotypic) entries within one Run, e.g. all present peptides within one run (duplicates removed)
get_unique <- function(input_df,
                       run,
                       level) {
  input_df %>%
    dplyr::filter(.data$Run_mpwR == run) %>%
    dplyr::select(all_of(level)) %>%
    dplyr::distinct()
}

#always make conversion with flowTraceR -- to remove_traceR_unknownMods also when traceR is FALSE
prepare_Upset <- function(input_df,
                          level = c("Precursor.IDs", "Peptide.IDs", "Protein.IDs", "ProteinGroup.IDs"),
                          percentage_runs = 100,
                          flowTraceR = FALSE,
                          remove_traceR_unknownMods = FALSE, #relevant for precursor and modified peptides
                          software = c("MaxQuant", "DIA-NN", "Spectronaut", "PD", "Generic")) { #for flowTraceR

  #handle global vars
  . <- NULL

  if (flowTraceR == FALSE) {

    if (level == "Protein.IDs" & software == "Spectronaut") {
      stop("Protein.IDs not available for Spectronaut")
    }

    level <- paste0(level, "_mpwR")

  } else if (flowTraceR == TRUE) {

    if (remove_traceR_unknownMods == TRUE) {
      if (level == "Precursor.IDs") {
        input_df <- flowTraceR::convert_precursor(input_df = input_df, software = software) %>%
          dplyr::filter(.data$traceR_precursor_unknownMods != TRUE)

      } else if (level == "Peptide.IDs") {
        input_df <- flowTraceR::convert_modified_peptides(input_df = input_df, software = software) %>%
          dplyr::filter(.data$traceR_mod.peptides_unknownMods != TRUE)


      } else if (level == "Protein.IDs") {
        message("No flowTraceR conversion for Protein.IDs available - so no flowTraceR filtering applied.")

      } else if (level == "ProteinGroup.IDs") {

          input_df <- flowTraceR::convert_proteingroups(input_df = input_df, software = software)
          message("For ProteinGroup.IDs - No flowTraceR filtering applied")
      }
    } else if (remove_traceR_unknownMods == FALSE) {

      if (level == "Precursor.IDs") {
        input_df <- flowTraceR::convert_precursor(input_df = input_df, software = software)

      } else if (level == "Peptide.IDs") {
        input_df <- flowTraceR::convert_modified_peptides(input_df = input_df, software = software)

      } else if (level == "Protein.IDs") {
        #not available in flowTraceR #so no conversion

      } else if (level == "ProteinGroup.IDs") {

          input_df <- flowTraceR::convert_proteingroups(input_df = input_df, software = software)
        }
    }

    if (level == "Precursor.IDs") {

      level <- "traceR_precursor"

    } else if (level == "Peptide.IDs") {

      level <- "traceR_mod.peptides"

    } else if (level == "Protein.IDs") {
      stop("flowTraceR not available for Protein.IDs level - put flowTraceR = FALSE")

    } else if (level == "ProteinGroup.IDs") {

      level <- "traceR_proteinGroups"
    }

  }

  Run_mpwR <- input_df %>%
    dplyr::select(Run_mpwR) %>%
    dplyr::distinct() %>%
    dplyr::arrange(Run_mpwR) %>%
    unlist()

  #========
  #put unique values of each run in lists
  list_df <- comprehenr::to_list(for (i in Run_mpwR) get_unique(input_df = input_df, run = i, level = level))

  #========
  #use the list and group by input level and count the number of appearance of each feature
  Appearances <- dplyr::bind_rows(list_df) %>%
    dplyr::group_by(.data[[level]]) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      n_perc = (n / max(n)) * 100 #n is count column
    ) %>%
    dplyr::filter(.data$n_perc >= percentage_runs) %>%
    dplyr::select(all_of(level)) %>%
    unlist(., use.names = FALSE) #require character vector for UpsetR without names for individual entries

  return(Appearances)
}

#sample for duplicates in create_example
sample_replicates <- function(input_vec) {
  set.seed(1234)
  a <- sample(input_vec, 5)
  b <- sample(input_vec, 5)

  c(a, b)
}

#Define used global varaibles
utils::globalVariables(c(":="))
