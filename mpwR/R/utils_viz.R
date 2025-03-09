
#CV density
viz_CV_density <-  function(input_df,
                             xaxes_limit = 50,
                             cv_col = c("CV_Retention.time_mpwR", "CV_Peptide_LFQ_mpwR", "CV_ProteinGroup_LFQ_mpwR")) {


  if (cv_col %in% colnames(input_df) == FALSE) {
    message(paste0("For plotting density: ", cv_col, " is not detected in submitted data."))
    return(NULL)

  } else {

    if (cv_col == "CV_Retention.time_mpwR") {
      label <- "Retention time"
      level <- "precursor"
    } else if (cv_col == "CV_Peptide_LFQ_mpwR") {
      label <- "LFQ"
      level <- "peptide"
    } else if (cv_col == "CV_ProteinGroup_LFQ_mpwR") {
      label <- "LFQ"
      level <- "proteingroup"
    }

    input_df %>%
      ggplot2::ggplot(ggplot2::aes(x = !!as.symbol(cv_col), colour = .data$Analysis_mpwR)) +
      ggplot2::stat_ecdf(geom = "step", lwd = 0.8) +
      ggplot2::ylab("Cumulative frequency \n") +
      ggplot2::xlab(paste0("\n", label, " CV [%] at ", level, " level")) +
      ggplot2::labs(colour = "Analysis") +
      ggplot2::coord_cartesian(xlim = c(0, xaxes_limit)) +
      ggplot2::theme_bw()
  }
}

#DC barplot
viz_DC_barplot <- function(input_df,
                            level = c("Precursor.IDs", "Peptide.IDs", "Protein.IDs", "ProteinGroup.IDs"),
                            label = c("absolute", "percentage")) {

  input_df <- as.data.frame(input_df)

  if (nrow(filter(input_df, !is.na(!!as.symbol(level)))) == 0) {  #NA entries - Spectronaut Protein.IDs
    message(paste0("For plotting barplot: Only NA detected for ", level, "! Not included in vizualisation."))
    return(NULL)
  } else {

    input_df <- input_df %>%
      dplyr::select("Nr.Missing.Values", any_of(level))

    if (label[1] == "absolute") {
      name_yaxes <- "Nr. of affected profiles [abs.]\n"
    } else if (label[1] == "percentage") {
      name_yaxes <- "Nr. of affected profiles [%]\n"
    }

    name_xaxes <- "\nNr. of missing values"
    ylimit <- max(input_df[, 2]) + (max(input_df[, 2]) * 0.1)

    ggplot2::ggplot(data = input_df, ggplot2::aes(x = .data$Nr.Missing.Values, y = input_df[, 2])) +
      ggplot2::geom_bar(stat = "identity", position = "dodge", width = 0.5, colour = "black") +
      ggplot2::labs(y = name_yaxes,
           x = name_xaxes) +
      ggplot2::theme(axis.line = ggplot2::element_line(color = "black", size = 0.5, linetype = "solid")) +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, ylimit)) +
      ggplot2::scale_x_continuous(breaks = seq(from = 0, to = nrow(input_df), by = 1)) +
      ggplot2::geom_text(ggplot2::aes(y = input_df[, 2], label = round(input_df[, 2], digits = 0)), vjust = -0.5, color = "black", size = 3) +
      ggplot2::theme_bw()

  }
}

#DC stacked barplot
viz_DC_stacked_barplot <- function(input_df,
                                    level = c("Precursor.IDs", "Peptide.IDs", "Protein.IDs", "ProteinGroup.IDs"),
                                    label = c("absolute", "percentage")) {

  #handle global vars
  . <- NULL

  input_df <- as.data.frame(input_df)

  if (nrow(filter(input_df, !is.na(!!as.symbol(level)))) == 0) {  #NA entries - Spectronaut Protein.IDs
    message(paste0("For plotting stacked barplot: Only NA detected for ", level, "! Not included in vizualisation."))
    return(NULL)
  } else {

    #summarize per Profile
    input_df <- prepare_stacked_barplot(input_df =  input_df, level = level[1])

    #color scheme
    unique_Profile <- length(unique(input_df$Profile))

    if (unique_Profile < 3) {

      color_palette_dc <- c("#CA0020", "#404040") #red to grey

    } else if (unique_Profile == 3) {

      color_palette_dc <- c("#CA0020", "#F7E2A6", "#404040") #red to grey

    } else if (unique_Profile == 4) {
      color_palette_dc <- c("#CA0020", "#F7E2A6", "#BABABA", "#404040") #red to grey
    }

    #xlimit
    xlimit <- input_df %>%
      dplyr::group_by(.data$Analysis) %>%
      dplyr::summarize(Sum_Level = sum(.data[[!!level[1]]])) %>%
      dplyr::ungroup()

    #x axes name - appendix
    if (label[1] == "absolute") {
      name_xaxes <- paste0(level[1], " [abs.]")
      xlimit <- max(xlimit[, 2]) + (max(xlimit[, 2]) * 0.1)
    } else if (label[1] == "percentage") {
      name_xaxes <- paste0(level[1], " [%]")
      xlimit <- 101
    }

    #add text for labelling
    input_label <- data.frame(label = character(), stringsAsFactors = FALSE)

    for (i in seq_len(nrow(input_df))) {
      if (input_df[i, "Profile"] == "complete") {
        input_label[i, "label"] <- as.character(round(input_df[i, 2], digits = 0))
      } else {
        input_label[i, "label"] <- ""
      }
    }

    input_df <- cbind(input_df, input_label) %>%
      arrange(.data$Analysis)

    #factorize
    input_df$Analysis <- as.factor(input_df$Analysis) %>%
      forcats::fct_rev(.)

    input_df$Profile <- factor(input_df$Profile, levels = c("unique", "sparse", "shared with at least 50%", "complete"))

    #plotting
    input_df %>%
      ggplot2::ggplot(ggplot2::aes(y = .data$Analysis, x = !!as.symbol(colnames(input_df[2])), fill = .data$Profile)) +
      ggplot2::geom_col(position = "stack", width = 0.5) +
      ggplot2::scale_fill_manual(values = color_palette_dc) +
      ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, xlimit), position = "top") +
      ggplot2::labs(x = paste(name_xaxes, "\n"),
           y = "Analysis \n") +
      ggplot2::theme(legend.position  = "right") +
      ggplot2::geom_text(ggplot2::aes(label = input_df[, "label"]), color = "white", size = 3, hjust = 1.2) +
      ggplot2::theme_bw()
  }
}

#ID barplot
viz_ID_barplot <- function(input_df,
                            level = c("Precursor.IDs", "Peptide.IDs", "Protein.IDs", "ProteinGroup.IDs")) {

  input_df <- as.data.frame(input_df)

  if (nrow(filter(input_df, !is.na(!!as.symbol(level)))) == 0) {  #NA entries - Spectronaut Protein.IDs
    message(paste0("For plotting barplot: Only NA detected for ", level, "! Not included in vizualisation."))
    return(NULL)
  } else {

    input_df <- input_df %>%
      dplyr::select("Run", all_of(level))

    name_xaxes <- paste(level, " [abs.]\n")
    xlimit <- max(input_df[, 2]) + (max(input_df[, 2]) * 0.1)

    input_df %>%
      dplyr::mutate(Run = factor(.data$Run, levels = .data$Run)) %>%
      dplyr::mutate(Run = forcats::fct_rev(.data$Run)) %>%
      ggplot2::ggplot(ggplot2::aes(x = !!as.symbol(colnames(input_df[2])), y = .data$Run)) +
      ggplot2::geom_col(width = 0.5) +
      ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, xlimit), position = "top") +
      ggplot2::labs(x = name_xaxes,
           y = "Run \n") +
      ggplot2::geom_text(ggplot2::aes(label = input_df[, 2]), color = "white", size = 3, hjust = 1.5) +
      ggplot2::theme_bw()
  }
}

#ID boxplot
viz_ID_boxplot <- function(input_df,
                            level = c("Precursor.IDs", "Peptide.IDs", "Protein.IDs", "ProteinGroup.IDs")) {

  #handle global vars
  . <- NULL

  input_df <- as.data.frame(input_df)

  if (nrow(filter(input_df, !is.na(!!as.symbol(level)))) == 0) {  #NA entries - Spectronaut Protein.IDs
    message(paste0("For plotting boxplot: Only NA detected for ", level, "! Not included in vizualisation."))
    return(NULL)
  } else {

    input_df <- input_df %>%
      dplyr::select("Analysis", all_of(level))

    name_yaxes <- paste(level, " [abs.]\n")
    xlimit <- max(input_df[, 2]) + (max(input_df[, 2]) * 0.1)

    analysis_levels <- input_df %>%
      dplyr::select("Analysis") %>%
      dplyr::arrange(.data$Analysis) %>%
      dplyr::distinct() %>%
      unlist(., use.names = FALSE)

    input_df %>%
      dplyr::mutate(Analysis = factor(.data$Analysis, levels = analysis_levels)) %>%
      ggplot2::ggplot(ggplot2::aes(x = forcats::fct_rev(.data$Analysis), y = input_df[, 2])) +
      ggplot2::geom_boxplot(fill = "grey", show.legend = FALSE, width = 0.6, lwd = 1.05) +
      ggplot2::geom_jitter() +
      ggplot2::labs(y = name_yaxes,
           x = "Analysis \n") +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, xlimit), position = "right") +
      ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
      ggplot2::theme(axis.line = ggplot2::element_line(color = "black", size = 0.5, linetype = "solid")) +
      ggplot2::theme_bw()
  }
}

#MC barplot
viz_MC_barplot <- function(input_df,
                           label = c("absolute", "percentage")) {

  input_df <- as.data.frame(input_df)
  input_df$mc_count <- as.numeric(input_df$mc_count)

  input_df <- input_df %>%
    dplyr::select("Missed.Cleavage", "mc_count")

  if (label == "absolute") {
    name_yaxes <- "Nr. of Peptide.IDs [abs.]\n"
  } else if (label == "percentage") {
    name_yaxes <- "Nr. of Peptide.IDs [%]\n"
  }

  #Alter possible Missed.Cleavage - "No R/K cleavage site" for plot
  input_df[stringr::str_detect(string = input_df$Missed.Cleavage, pattern = "No R/K cleavage site"), "Missed.Cleavage"] <- "No R/K \n cleavage site"

  name_xaxes <- "\nNr. of Missed Cleavages"
  ylimit <- max(input_df[, 2]) + (max(input_df[, 2]) * 0.1)

  ggplot2::ggplot(data = input_df, ggplot2::aes(x = .data$Missed.Cleavage, y = input_df[, 2])) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", width = 0.5, colour = "black") +
    ggplot2::labs(y = name_yaxes,
         x = name_xaxes) +
    ggplot2::theme(axis.line = ggplot2::element_line(color = "black", size = 0.5, linetype = "solid")) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, ylimit)) +
    ggplot2::geom_text(ggplot2::aes(y = input_df[, 2], label = round(input_df[, 2], digits = 0)), vjust = -0.5, color = "black", size = 3) +
    ggplot2::theme_bw()
}

#MC stacked barplot
viz_MC_stacked_barplot <- function(input_df,
                                   label = c("absolute", "percentage")) {

  input_df <- as.data.frame(input_df)
  input_df$mc_count <- as.numeric(input_df$mc_count)

  input_df <- input_df[, c("Analysis", "mc_count", "Missed.Cleavage")]

  #summarize per profile
  unique_profile <- length(unique(input_df$Missed.Cleavage))

  #color scheme
  if (unique_profile < 3) {
    color_palette_mc <- c("#A6611A", "#018571")
  } else if (unique_profile == 3) {
    color_palette_mc <- c("#A6611A", "#BAB8B8", "#018571")
  } else if (unique_profile == 4) {
    color_palette_mc <- c("#A6611A", "#BAB8B8", "#80CDC1", "#018571")
  } else if (unique_profile == 5) {
    color_palette_mc <- c("#A6611A", "#DFC27D", "grey", "#80CDC1", "#018571")
  }

  #xlimit
  xlimit <- input_df %>%
    dplyr::group_by(.data$Analysis) %>%
    dplyr::summarize(
      Sum_Level = sum(.data$mc_count)
    )

  name_xaxes <- "Peptide IDs"

  if (label == "absolute") {
    name_xaxes <- paste0(name_xaxes, " [abs.]")
    xlimit <- max(xlimit[, 2]) + (max(xlimit[, 2]) * 0.1)
  } else if (label == "percentage") {
    name_xaxes <- paste0(name_xaxes, " [%]")
    xlimit <- 101
  }


  input_label <- data.frame(label = character(), stringsAsFactors = FALSE)

  for (i in seq_len(nrow(input_df))) {
    if (input_df[i, "Missed.Cleavage"] == "0") {
      input_label[i, "label"] <- as.character(round(input_df[i, 2], digits = 0))
    } else {
      input_label[i, "label"] <- ""
    }
  }

  input_df <- cbind(input_df, input_label) %>%
    dplyr::arrange(.data$Analysis)

  #factorize
  input_df$Analysis <- as.factor(input_df$Analysis)
  input_df$Missed.Cleavage <- factor(input_df$Missed.Cleavage, levels = sort(unique(input_df$Missed.Cleavage), decreasing = TRUE))

  input_df$Analysis <-  forcats::fct_rev(input_df$Analysis)

  input_df <- input_df %>%
    dplyr::rename("Peptide_count" = "mc_count")

  #plot
  input_df %>%
    ggplot2::ggplot(ggplot2::aes(y = .data$Analysis, x = .data$Peptide_count, fill = .data$Missed.Cleavage)) +
    ggplot2::geom_col(position = "stack", width = 0.5) +
    ggplot2::scale_fill_manual(values = color_palette_mc) +
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, xlimit), position = "top") +
    ggplot2::labs(x = paste(name_xaxes, "\n"),
         y = "Analysis \n") +
    ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = "right") +
    ggplot2::theme(legend.background = ggplot2::element_rect(size = 0.5, linetype = "solid", colour = "black")) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(input_df[, "label"])), color = "white", size = 3, hjust = 1.2) +
    ggplot2::theme_bw()

}
