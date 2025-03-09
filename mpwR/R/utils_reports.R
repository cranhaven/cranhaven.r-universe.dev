
#DC Report
generate_DC_Report <- function(input_df,
                               input_MQ_proteingroup,
                               input_PD_protein,
                               input_PD_proteingroup,
                               analysis_name = "analysis_name",
                               software = c("MaxQuant", "DIA-NN", "Spectronaut", "PD", "Generic"),
                               metric = c("absolute", "percentage")) {
  #handle global vars
  . <- NULL

  if (software == "MaxQuant") {
    MQ_tidy_pg <- tidy_MQ_pep_pg(input_df = input_MQ_proteingroup)

    DC_Report <- make_frame_DC(input_df = input_df)

    Prec.DC <- generate_DC_count(input_df = input_df, level = "Precursor.IDs")
    Pep.DC <- generate_DC_count(input_df = input_df, level = "Peptide.IDs")
    Prot.DC <- generate_DC_count(input_df = input_df, level = "Protein.IDs")
    PG.DC <- generate_DC_count(input_df = MQ_tidy_pg, level = "ProteinGroup.IDs")

    #*****DC_Report********
    DC_Report <- join_DC_levels(input_df = DC_Report, prec_df = Prec.DC, pep_df = Pep.DC, prot_df = Prot.DC, pg_df = PG.DC) %>%
      generate_DC_Report_profile(input_df = .) %>%
      add_analysis_col(input_df = ., analysis_name = analysis_name)

    if (metric == "percentage") {
      DC_Report <- generate_DC_Report_percentage(input_df = DC_Report)
      return(DC_Report)
    } else if (metric == "absolute") {
      return(DC_Report)
    }

  } else if (software == "DIA-NN" | software == "Generic") {

    DC_Report <- make_frame_DC(input_df = input_df)

    Prec.DC <- generate_DC_count(input_df = input_df, level = "Precursor.IDs")
    Pep.DC <- generate_DC_count(input_df = input_df, level = "Peptide.IDs")
    Prot.DC <- generate_DC_count(input_df = input_df, level = "Protein.IDs")
    PG.DC <- generate_DC_count(input_df = input_df, level = "ProteinGroup.IDs")

    #*****DC_Report********
    DC_Report <- join_DC_levels(input_df = DC_Report, prec_df = Prec.DC, pep_df = Pep.DC, prot_df = Prot.DC, pg_df = PG.DC) %>%
      generate_DC_Report_profile(input_df = .) %>%
      add_analysis_col(input_df = ., analysis_name = analysis_name)

    if (metric == "percentage") {
      DC_Report <- generate_DC_Report_percentage(input_df = DC_Report)
      return(DC_Report)
    } else if (metric == "absolute") {
      return(DC_Report)
    }

  } else if (software == "Spectronaut") { #no Protein.IDs

    DC_Report <- make_frame_DC(input_df = input_df)

    Prec.DC <- generate_DC_count(input_df = input_df, level = "Precursor.IDs")
    Pep.DC <- generate_DC_count(input_df = input_df, level = "Peptide.IDs")
    Prot.DC <- cbind(
      Pep.DC[, "Nr_Appearances"],
      data.frame("Protein.IDs" = c(rep(NA, each = nrow(Pep.DC))))
    )
    PG.DC <- generate_DC_count(input_df = input_df, level = "ProteinGroup.IDs")

    #*****DC_Report********
    DC_Report <- join_DC_levels(input_df = DC_Report, prec_df = Prec.DC, pep_df = Pep.DC, prot_df = Prot.DC, pg_df = PG.DC) %>%
      generate_DC_Report_profile(input_df = .) %>%
      add_analysis_col(input_df = ., analysis_name = analysis_name)

    DC_Report$Protein.IDs <- NA

    if (metric == "percentage") {
      DC_Report <- generate_DC_Report_percentage(input_df = DC_Report)
      DC_Report$Protein.IDs <- NA
      return(DC_Report)
    } else if (metric == "absolute") {
      return(DC_Report)
    }

  } else if (software == "PD") {

    DC_Report <- make_frame_DC(input_df = input_df)

    Prec.DC <- generate_DC_count(input_df = input_df, level = "Precursor.IDs")
    Pep.DC <- generate_DC_count(input_df = input_df, level = "Peptide.IDs")
    Prot.DC <- generate_DC_count(input_df = input_PD_protein, level = "Protein.IDs")
    PG.DC <- generate_DC_count(input_df = input_PD_proteingroup, level = "ProteinGroup.IDs")

    #*****DC_Report********
    DC_Report <- join_DC_levels(input_df = DC_Report, prec_df = Prec.DC, pep_df = Pep.DC, prot_df = Prot.DC, pg_df = PG.DC) %>%
      generate_DC_Report_profile(input_df = .) %>%
      add_analysis_col(input_df = ., analysis_name = analysis_name)

    if (metric == "percentage") {
      DC_Report <- generate_DC_Report_percentage(input_df = DC_Report)
      return(DC_Report)
    } else if (metric == "absolute") {
      return(DC_Report)
    }
  }
}

#ID Report
generate_ID_Report <- function(input_df,
                               input_MQ_proteingroup,
                               input_PD_protein,
                               input_PD_proteingroup,
                               analysis_name = "analysis_name",
                               software = c("MaxQuant", "DIA-NN", "Spectronaut", "PD", "Generic")) {

  #handle global vars
  . <- NULL

  if (software == "MaxQuant") {

    MQ_tidy_pg <- tidy_MQ_pep_pg(input_df = input_MQ_proteingroup)

    MQ_pg <- generate_level_count(MQ_tidy_pg, "ProteinGroup.IDs") %>% arrange(.data$Run_mpwR)

    ID_Report <- dplyr::left_join(
      generate_level_count(input_df, "Protein.IDs"),
      generate_level_count(input_df, "Peptide.IDs"),
      by = c("Run_mpwR")) %>%
      dplyr::left_join(., generate_level_count(input_df, "Precursor.IDs"),
                       by = c("Run_mpwR")) %>%
      dplyr::arrange(.data$Run_mpwR) %>%
      dplyr::rename("Run" = "Run_mpwR")

    ID_Report <- cbind(MQ_pg[, -1], ID_Report) %>% reorder_ID_Report(.)


    #add_analysis col
    ID_Report <- add_analysis_col(input_df = ID_Report, analysis_name = analysis_name)
    return(ID_Report)

  } else if (software == "DIA-NN" | software == "Generic") {

    ID_Report <- dplyr::left_join(
      generate_level_count(input_df, "ProteinGroup.IDs"),
      generate_level_count(input_df, "Protein.IDs"),
      by = c("Run_mpwR")) %>%
      dplyr::left_join(., generate_level_count(input_df, "Peptide.IDs"),
                       by = c("Run_mpwR")) %>%
      dplyr::left_join(., generate_level_count(input_df, "Precursor.IDs"),
                       by = c("Run_mpwR")) %>%
      dplyr::rename("Run" = "Run_mpwR")

    #add_analysis col
    ID_Report <- add_analysis_col(input_df = ID_Report, analysis_name = analysis_name)
    return(ID_Report)

  } else if (software == "Spectronaut") {

    ID_Report <- dplyr::left_join(
      generate_level_count(input_df, "ProteinGroup.IDs"),
      generate_level_count(input_df, "Peptide.IDs"),
      by = c("Run_mpwR")) %>%
      dplyr::left_join(., generate_level_count(input_df, "Precursor.IDs"),
                       by = c("Run_mpwR")) %>%
      dplyr::rename("Run" = "Run_mpwR")


    #Spectronaut has no Protein.IDs
    Prot.IDs <- data.frame("Protein.IDs" = c(rep(NA, each = nrow(ID_Report))))

    ID_Report <- cbind(ID_Report, Prot.IDs) %>% reorder_ID_Report(.)

    #add_analysis col
    ID_Report <- add_analysis_col(input_df = ID_Report, analysis_name = analysis_name)
    return(ID_Report)

  } else if (software == "PD") {

    #Run information only in psm-file
    ID_Report <- dplyr::left_join(
      generate_level_count(input_df, "Peptide.IDs"),
      generate_level_count(input_df, "Precursor.IDs"),
      by = c("Run_mpwR")) %>%
      dplyr::rename("Run" = "Run_mpwR")

    PD_pg <- generate_level_count(input_df = input_PD_proteingroup, level = "ProteinGroup.IDs") %>% arrange(.data$Run_mpwR)
    PD_prot <- generate_level_count(input_df = input_PD_protein, level = "Protein.IDs") %>% arrange(.data$Run_mpwR)

    ID_Report <- cbind(ID_Report, PD_pg[, -1], PD_prot[, -1]) %>% reorder_ID_Report(.)

    #add_analysis col
    ID_Report <- add_analysis_col(input_df = ID_Report, analysis_name = analysis_name)
    return(ID_Report)

  }

  return(ID_Report)
}

generate_MC_Report <- function(input_df,
                               analysis_name = "analysis_name",
                               software = c("MaxQuant", "DIA-NN", "Spectronaut", "PD", "Generic"),
                               metric = c("absolute", "percentage")) {

  #handle global vars
  . <- NULL

  if (software == "MaxQuant" | software == "PD" | software == "Spectronaut" | software == "Generic") {
    value <- "present"
  } else if (software == "DIA-NN") {
    value <- "generate"
  }

  if (value == "present") {

    MC_Report <- input_df %>%
      dplyr::distinct(.data$Stripped.Sequence_mpwR, .keep_all = TRUE) %>%
      dplyr::count(.data$Missed.Cleavage_mpwR) %>%
      dplyr::rename(Missed.Cleavage = "Missed.Cleavage_mpwR", mc_count = n) %>%
      add_analysis_col(input_df = ., analysis_name = analysis_name)

    if (metric == "percentage") {
      MC_Report <- generate_MC_Report_percentage(input_df = MC_Report)
      MC_Report$Missed.Cleavage <- as.character(MC_Report$Missed.Cleavage) #Sometimes: "No R/K cleavage site"
      return(MC_Report)
    } else if (metric == "absolute") {
      MC_Report$Missed.Cleavage <- as.character(MC_Report$Missed.Cleavage) #Sometimes: "No R/K cleavage site"
      return(MC_Report)
    }

  } else if (value == "generate") {

    #count missed cleavages - count R and K in Stripped Sequence -1 (one exists at the end)
    mc_df <- input_df %>%
      distinct(.data$Stripped.Sequence_mpwR, .keep_all = TRUE)

    #non-tryptic peptides present? (like in DIA-NN)
    tryptic_R <- stringr::str_detect(string = mc_df$Stripped.Sequence_mpwR, pattern =  "R$")
    tryptic_K <- stringr::str_detect(string = mc_df$Stripped.Sequence_mpwR, pattern =  "K$")

    tryptic <- tryptic_R + tryptic_K
    non_tryptic <- tryptic == 0
    non_tryptic <- mc_df[non_tryptic, ] %>% nrow()

    #count K+R for tryptic peptides
    tryptic <- tryptic == 1
    mc_df <- mc_df[tryptic, ]
    K <- stringr::str_count(mc_df$Stripped.Sequence_mpwR, pattern = "K")
    R <- stringr::str_count(mc_df$Stripped.Sequence_mpwR, pattern = "R")
    MC <- K + R - 1
    Missed.Cleavage_df <-  tibble("Missed.Cleavage" = MC)

    MC_Report <- Missed.Cleavage_df %>%
      dplyr::count(.data$Missed.Cleavage) %>%
      dplyr::rename(mc_count = n)

    if (non_tryptic > 0) {

      MC_Report <- rbind(
        MC_Report,
        tibble::tibble("Missed.Cleavage" = "No R/K cleavage site", "mc_count" = paste(non_tryptic)))

    }

    MC_Report <- add_analysis_col(input_df = MC_Report, analysis_name = analysis_name)

    if (metric == "percentage") {
      MC_Report <- generate_MC_Report_percentage(input_df = MC_Report)

      MC_Report$mc_count <- as.numeric(MC_Report$mc_count)
      MC_Report$Missed.Cleavage <- as.character(MC_Report$Missed.Cleavage) #Sometimes: "No R/K cleavage site"
      return(MC_Report)

    } else if (metric == "absolute") {

      MC_Report$mc_count <- as.numeric(MC_Report$mc_count)
      MC_Report$Missed.Cleavage <- as.character(MC_Report$Missed.Cleavage) #Sometimes: "No R/K cleavage site"
      return(MC_Report)
    }
  }
}

#Summary Report
generate_summary_Report <- function(input_df, #Precursor level
                                    analysis_name = "analysis_name",
                                    software = c("MaxQuant", "DIA-NN", "Spectronaut", "PD"),
                                    input_MQ_peptide, #MC #LFQ
                                    input_MQ_proteingroup, #ID #DC #LFQ
                                    input_PD_peptide, #MC
                                    input_PD_protein, #ID #DC
                                    input_PD_proteingroup, #ID #DC
                                    CV_RT_th_hold = 5,
                                    CV_LFQ_Pep_th_hold = 20,
                                    CV_LFQ_PG_th_hold = 20) {

  #handle global vars
  . <- NULL

  if (software == "MaxQuant") { #tidy peptides.txt for MC_Report

    ID_Report <- generate_ID_Report(input_df = input_df, input_MQ_proteingroup = input_MQ_proteingroup, analysis_name = analysis_name, software = software)
    DC_Report_abs <- generate_DC_Report(input_df = input_df, input_MQ_proteingroup = input_MQ_proteingroup, analysis_name = analysis_name, software = software, metric = "absolute")
    DC_Report_perc <- generate_DC_Report(input_df = input_df, input_MQ_proteingroup = input_MQ_proteingroup, analysis_name = analysis_name, software = software, metric = "percentage")
    CV_RT <- calculate_CV(input_df = input_df, cv_col = "Retention.time", analysis_name = analysis_name)
    CV_LFQ_Pep <- tidy_MQ_LFQ(input_df = input_MQ_peptide, cv_col = "Peptide_LFQ") %>% calculate_CV(input_df = ., cv_col = "Peptide_LFQ", analysis_name = analysis_name)
    CV_LFQ_PG <- tidy_MQ_LFQ(input_df = input_MQ_proteingroup, cv_col = "ProteinGroup_LFQ") %>% calculate_CV(input_df = ., cv_col = "ProteinGroup_LFQ", analysis_name = analysis_name)
    MC_Report_abs <- tidy_MQ_pep_pg(input_df = input_MQ_peptide) %>% generate_MC_Report(input_df = ., software = software, analysis_name = analysis_name, metric = "absolute")
    MC_Report_perc <- tidy_MQ_pep_pg(input_df = input_MQ_peptide) %>% generate_MC_Report(input_df = ., software = software, analysis_name = analysis_name, metric = "percentage")

  } else if (software == "DIA-NN") { #no Pep_LFQ data

    ID_Report <- generate_ID_Report(input_df = input_df, analysis_name = analysis_name, software = software)
    DC_Report_abs <- generate_DC_Report(input_df = input_df, analysis_name = analysis_name, software = software, metric = "absolute")
    DC_Report_perc <- generate_DC_Report(input_df = input_df, analysis_name = analysis_name, software = software, metric = "percentage")
    CV_RT <- calculate_CV(input_df = input_df, cv_col = "Retention.time", analysis_name = analysis_name)
    CV_LFQ_PG <- calculate_CV(input_df = input_df, cv_col = "ProteinGroup_LFQ", analysis_name = analysis_name)
    CV_LFQ_Pep <- input_df #if no Peptide_LFQ_mpwR detect in get_CV_IDs --> dataframe with NA generated
    MC_Report_abs <- generate_MC_Report(input_df = input_df, software = software, analysis_name = analysis_name, metric = "absolute")
    MC_Report_perc <- generate_MC_Report(input_df = input_df, software = software, analysis_name = analysis_name, metric = "percentage")

  } else if (software == "Spectronaut") {

    ID_Report <- generate_ID_Report(input_df = input_df, analysis_name = analysis_name, software = software)
    DC_Report_abs <- generate_DC_Report(input_df = input_df, analysis_name = analysis_name, software = software, metric = "absolute")
    DC_Report_perc <- generate_DC_Report(input_df = input_df, analysis_name = analysis_name, software = software, metric = "percentage")
    CV_RT <- calculate_CV(input_df = input_df, cv_col = "Retention.time", analysis_name = analysis_name)
    CV_LFQ_PG <- calculate_CV(input_df = input_df, cv_col = "ProteinGroup_LFQ", analysis_name = analysis_name)
    CV_LFQ_Pep <- calculate_CV(input_df = input_df, cv_col = "Peptide_LFQ", analysis_name = analysis_name)
    MC_Report_abs <- generate_MC_Report(input_df = input_df, software = software, analysis_name = analysis_name, metric = "absolute")
    MC_Report_perc <- generate_MC_Report(input_df = input_df, software = software, analysis_name = analysis_name, metric = "percentage")

  } else if (software == "PD") { #no Pep_LFQ #no PG_LFQ

    ID_Report <- generate_ID_Report(input_df = input_df, input_PD_protein = input_PD_protein, input_PD_proteingroup = input_PD_proteingroup, analysis_name = analysis_name, software = "PD")
    DC_Report_abs <- generate_DC_Report(input_df = input_df, input_PD_protein = input_PD_protein, input_PD_proteingroup = input_PD_proteingroup, analysis_name = analysis_name, software = "PD", metric = "absolute")
    DC_Report_perc <- generate_DC_Report(input_df = input_df, input_PD_protein = input_PD_protein, input_PD_proteingroup = input_PD_proteingroup, analysis_name = analysis_name, software = "PD", metric = "percentage")
    CV_RT <- calculate_CV(input_df = input_df, cv_col = "Retention.time", analysis_name = analysis_name)
    CV_LFQ_PG <- input_df  #if no ProteinGroup_LFQ_mpwR detect in get_CV_IDs --> dataframe with NA generated
    CV_LFQ_Pep <- input_df  #if no Peptide_LFQ_mpwR detect in get_CV_IDs --> dataframe with NA generated
    MC_Report_abs <- generate_MC_Report(input_df = input_PD_peptide, software = software, analysis_name = analysis_name, metric = "absolute")
    MC_Report_perc <- generate_MC_Report(input_df = input_PD_peptide, software = software, analysis_name = analysis_name, metric = "percentage")

  } else if (software == "Generic") {

    ID_Report <- generate_ID_Report(input_df = input_df, analysis_name = analysis_name, software = software)
    DC_Report_abs <- generate_DC_Report(input_df = input_df, analysis_name = analysis_name, software = software, metric = "absolute")
    DC_Report_perc <- generate_DC_Report(input_df = input_df, analysis_name = analysis_name, software = software, metric = "percentage")
    CV_RT <- calculate_CV(input_df = input_df, cv_col = "Retention.time", analysis_name = analysis_name)
    CV_LFQ_PG <- calculate_CV(input_df = input_df, cv_col = "ProteinGroup_LFQ", analysis_name = analysis_name)
    CV_LFQ_Pep <- calculate_CV(input_df = input_df, cv_col = "Peptide_LFQ", analysis_name = analysis_name)
    MC_Report_abs <- generate_MC_Report(input_df = input_df, software = software, analysis_name = analysis_name, metric = "absolute")
    MC_Report_perc <- generate_MC_Report(input_df = input_df, software = software, analysis_name = analysis_name, metric = "percentage")

  }

  #generate summary report
  get_summary(
    ID_Report = ID_Report,
    DC_Report_abs = DC_Report_abs,
    DC_Report_perc = DC_Report_perc,
    CV_RT = CV_RT,
    CV_LFQ_PG = CV_LFQ_PG,
    CV_LFQ_Pep = CV_LFQ_Pep,
    MC_Report_abs = MC_Report_abs,
    MC_Report_perc = MC_Report_perc,
    CV_RT_th_hold = CV_RT_th_hold,
    CV_LFQ_Pep_th_hold = CV_LFQ_Pep_th_hold,
    CV_LFQ_PG_th_hold = CV_LFQ_PG_th_hold)

}
