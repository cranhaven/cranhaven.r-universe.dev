#' Scale feature coverage values to estimate their absolute abundance
#'
#' Calculates global scaling factors for features (contigs or bins),based on linear regression of sequin coverage. Options include log-transformations of coverage, as well as filtering features based on limit of detection. This function must be called first, before the feature abundance table, feature detection table, and plots are retrieved.
#'
#'@param f_tibble Can be either of
#' (1) a tibble with first column "Feature" that contains bin IDs, and the rest of the columns represent samples with bins' coverage values.
#' (2) a tibble as outputted by the program "checkm coverage" from the tool CheckM. Please check CheckM documentation - https://github.com/Ecogenomics/CheckM on the usage for "checkm coverage" program
#'@param sequin_meta tibble containing sequin names ("Feature column") and concentrations in attamoles/uL ("Concentration") column.
#'@param seq_dilution tibble with first column "Sample" with **same sample names as in f_tibble**, and a second column "Dilution" showing ratio of sequins added to final sample volume (e.g. a value of 0.01 for a dilution of 1 volume sequin to 99 volumes sample)
#'@param coe_of_variation Acceptable coefficient of variation for coverage and detection (eg. 20 - for 20 % threshold of coefficient of variation). Coverages above the threshold value will be flagged in the plots.
#'@param log_trans Boolean (TRUE or FALSE), should coverages and sequin concentrations be log-scaled?
#'@param cook_filtering Boolean (TRUE or FALSE), should data points be filtered based on Cook's distance metric. Cooks distance can be useful in detecting influential outliers in an ordinary least square’s regression model, which can negatively influence the model. A threshold of Cooks distance of 4/n (where n is the sample size) is chosen, and any data point with Cooks distance > 4/n is filtered out. It is typical to choose 4/n as the threshold in detecting the outliers in the data. Default = TRUE
#'@param lod_limit (Decimal range 0-1) Threshold for the percentage of minimum detected sequins per concentration group. Default = 0
#'@param save_plots Boolean (TRUE or FALSE), should sequin scaling be saved? Default = TRUE
#'@param plot_dir Directory where plots are to be saved. Will create a directory "sequin_scaling_plots_lm" if it does not exist.
#'@import magrittr MASS
#'@importFrom rlang .data
#'@importFrom dplyr mutate
#'@return a list of tibbles containing
#'  - mag_tab: a tibble with first column "Feature" that contains bin (or contig IDs), and the rest of the columns represent samples with features' scaled abundances (attamoles/uL)
#'  - mag_det: a tibble with first column "Feature" that contains bin (or contig IDs),
#'  - plots: linear regression plots for scaling MAG coverage values to absolute abundance
#'  - scale_fac: a master tibble with all of the intermediate values in above calculations
#'@export
#'
#'@examples
#'data(f_tibble, sequins, seq_dil)
#'
#'\donttest{
#'
#'### scaling sequins from coverage values
#'scaled_features_lm = scale_features_lm(f_tibble,sequin_meta, seq_dil)
#'}
#'

scale_features_lm <- function(f_tibble, sequin_meta, seq_dilution,
                                  log_trans = TRUE, coe_of_variation=250,
                                  lod_limit = 0, save_plots = TRUE, plot_dir=tempdir(),
                                  cook_filtering = TRUE){
  Sample <- cov_tab <- seq_cov <- Dilution <- seq_group  <- influential_data <- seq_cov_filt_temp <- NULL
  seq_det <- grouped_seq_cov <- seq_cov_filt <- lod <- fit <- mag_cov <- slope <- mag_ab <- intercept <- cooksd <- log_scale <- NULL
  seq_cov_filt_temp_grouped <- seq_cov_filt_round2 <- zero_row_check <- fit_filtered_lm <- slope_filtered <- intercept_filtered <- cooksd_filtered <- NULL
  mag_ab_filtered <- mag_det_filtered <- cooksd_plot <- cooksd_plot_filtered <- plots_filtered_lm <- NULL
  # Retrieve sample names from feature tibble
  # Retrieve sample names from feature tibble
  scale_fac <- dplyr::tibble(Sample = names(f_tibble) %>%
                               stringr::str_subset(pattern = "Feature", negate = TRUE))

  # Merge dilution factors for samples, add log-scaling option
  scale_fac <- scale_fac %>%
    dplyr::inner_join(seq_dilution, by = "Sample") %>%
    dplyr::mutate(log_trans = log_trans)

  # Make coverage table for features
  scale_fac <- scale_fac %>%
    dplyr::mutate(
      cov_tab = purrr::map(Sample, ~ dplyr::select(f_tibble, Feature, all_of(.))), # Make list of coverage tables for samples
      cov_tab = purrr::map(cov_tab, ~stats::setNames(., c("Feature", "Coverage")))  #get rid of sample name in header
    ) %>%

    # merge sequins with coverage tables, remove them from mag/feature table
    dplyr::mutate(
      seq_cov = purrr::map(cov_tab, ~ dplyr::inner_join(., sequins, by = "Feature")),
      mag_cov = purrr::map(cov_tab, ~ dplyr::anti_join(., sequins, by = "Feature"))
    )  %>%

    # scale sequin concentrations based on dilution factors
    dplyr::mutate(
      seq_cov = purrr::map2(seq_cov, Dilution, ~ dplyr::mutate(.x, Concentration = Concentration / .y))
    ) %>%

    # Determine groups of spike-in concentrations
    dplyr::mutate(
      seq_group = purrr::map(seq_cov, ~.x %>% dplyr::group_by(Concentration) %>%
                               dplyr::tally(name="standards"))
    ) %>%

    # determine limit of detection of spike-ins, based on presence of 5 sequins per conc.
    dplyr::mutate(
      #determine lowest concentration where at least 1 sequins is detected
      #lod = purrr::map_dbl(seq_cov, ~ dplyr::filter(., Coverage > 0) %>%
      #                       dplyr::group_by(., Concentration) %>%
      #                       dplyr::tally(name="detected") %>%
      #                       dplyr::summarise(Min = min(Concentration)) %>%
      #                       dplyr::pull(Min)),

      #create tibble comparing number of observed and theoretical spike ins
      seq_det = purrr::map2(seq_cov, seq_group, ~ dplyr::filter(., Coverage > 0) %>%
                              dplyr::group_by(., Concentration) %>%
                              dplyr::tally(name="detected") %>%
                              dplyr::inner_join(.y, by = "Concentration")),

      # determine difference between standards and observed spike ins
      seq_det = purrr::map(seq_det, ~ dplyr::mutate(., diff = standards - detected)),
      seq_det = purrr::map(seq_det, ~ dplyr::mutate(., ratio = detected*100/standards)),
      lod = purrr::map_dbl(seq_det, ~ dplyr::filter(., ratio > lod_limit) %>%
                             dplyr::filter(Concentration == min(Concentration)) %>%
                             dplyr::pull(Concentration)),
      seq_warning = purrr::map_int(seq_det, ~ dplyr::summarise(., Sum = sum(diff)) %>%
                                     dplyr::pull(Sum)) #positive values give warning later
    ) %>%

    # Calculate mean, standard deviation, and coeefficient of variation for groups of sequins
    # Create a logical vector determining if the sequin is within the threshold
    dplyr::mutate(
      grouped_seq_cov = purrr::map(cov_tab, ~ dplyr::inner_join(., sequins, by = "Feature") %>%
                                     dplyr::select(Feature, Coverage, Concentration)),
      grouped_seq_cov = purrr::map2(grouped_seq_cov, Dilution, ~ dplyr::mutate(.x, Concentration = Concentration/.y) %>%
                                      dplyr::group_by(Concentration) %>%
                                      dplyr::summarise(mean_cov = mean(Coverage),
                                                       sd_cov = stats::sd(Coverage)) %>%
      																dplyr::mutate(dplyr::across(dplyr::all_of(c("mean_cov", "sd_cov")), function(col) dplyr::na_if(col, 0))) %>%
                                      dplyr::mutate(coe_var = sd_cov*100/mean_cov) %>%
                                      dplyr::mutate(threshold_detection = coe_var <= coe_of_variation))) %>%

    #Create a list of samples in which sequins were not detected
    dplyr::mutate(under_detected = purrr::map(grouped_seq_cov, ~.x %>%
                                                dplyr::filter(is.na(mean_cov)) %>%
                                                dplyr::select(Concentration))
    ) %>%

    # perform linear regression on coverage vs conc., extract lm params, make plots
    dplyr::mutate(
      seq_cov_filt = purrr::map2(seq_cov,grouped_seq_cov, ~ dplyr::inner_join(.x, .y , by = "Concentration") %>%
                                   dplyr::filter(., Coverage > 0)), #remove zero coverage values before lm
      seq_cov_filt = purrr::map2(seq_cov_filt, lod, ~.x %>%
                                   dplyr::filter(Concentration >= .y) %>% #Remove concentrations below limit of detection
                                   dplyr::filter(., coe_var <= coe_of_variation) %>% #Remove sequin concentration groups which have high coefficient of variation
                                   dplyr::mutate(
                                     lod = .y))) %>%

    dplyr::mutate(
      seq_cov_filt = purrr::map(seq_cov_filt, ~ .x %>%
                           dplyr::mutate(
                             log_10_coverage = log10(Coverage),
                             lod_10_concentration = log10(Concentration))),
      fit = ifelse(log_trans == "TRUE" , # check log_trans input
                   purrr::map(seq_cov_filt, ~ stats::lm(lod_10_concentration ~ log_10_coverage , data = .)), #log lm if true
                   purrr::map(seq_cov_filt, ~ stats::lm(Concentration ~ Coverage , data = .)) # lm if false
      ),
      slope = purrr::map_dbl(fit, ~ summary(.)$coef[2]), # get slope
      intercept = purrr::map_dbl(fit, ~summary(.)$coef[1]) # get intercept
    ) %>%

    #plot linear regressions
    dplyr::mutate(
      plots = ifelse(log_trans == "TRUE" , # check log_trans input
                     purrr::map(seq_cov_filt, # log-scaled plot if true
                                ~ ggplot2::ggplot(data=. , ggplot2::aes(x=log10(Coverage), y= log10(Concentration))) +
                                  ggplot2::geom_point(ggplot2::aes(shape = threshold_detection)) +
                                  ggplot2::geom_smooth(method = "lm") +
                                  ggpubr::stat_regline_equation(label.x= -0.1, label.y = 3) +
                                  ggpubr::stat_cor(ggplot2::aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.x = -0.1, label.y = 3.5) +
                                  ggplot2::xlab("Coverage (log[read depth])") +
                                  ggplot2::ylab("DNA Concentration (log[attamoles/uL])") +
                                  ggplot2::scale_shape(name = "Coefficient of variation", labels = c(paste("below the threshold (",coe_of_variation,")"), paste("above the threshold(",coe_of_variation,")"))) +
                                  ggplot2::theme_bw()
                     ),
                     purrr::map(seq_cov_filt, # non-scaled plot if true
                                ~ ggplot2::ggplot(data=. , ggplot2::aes(x=Coverage, y= Concentration)) +
                                  ggplot2::geom_point(ggplot2::aes(shape = threshold_detection)) +
                                  ggplot2::geom_smooth(method = "lm") +
                                  ggpubr::stat_regline_equation(label.x= 0) +
                                  ggpubr::stat_cor(ggplot2::aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.x = 0) +
                                  ggplot2::xlab("Coverage (read depth)") +
                                  ggplot2::ylab("DNA Concentration (attamoles/uL)") +
                                  ggplot2::scale_shape(name = "Coefficient of variation", labels = c(paste("below the threshold (",coe_of_variation,")"), paste("above the threshold(",coe_of_variation,")"))) +
                                  ggplot2::theme_bw()
                     )
      )
    ) %>%

    #flag MAGs below LOD, and scale MAGs by slope and intercept
    dplyr::mutate(
      # Scale MAGs based on linear regression
      mag_ab = ifelse(log_trans == "TRUE" , # check log_trans input
                      purrr::map2(mag_cov, slope, ~ dplyr::mutate(.x, Concentration = log10(Coverage) * .y)), # y = mx (in log scale) if true
                      purrr::map2(mag_cov, slope, ~ dplyr::mutate(.x, Concentration = Coverage * .y)) # y = mx if false
      ),
      mag_ab = purrr::map2(mag_ab, intercept, ~ dplyr::mutate(.x, Concentration = Concentration + .y)),
      mag_ab = ifelse(log_trans == "TRUE" , # check log_trans input
                      purrr::map(mag_ab, ~ dplyr::mutate(.x, Concentration = 10^Concentration)), #convert back from log10 if true
                      mag_ab), # no change if false
      mag_det = mag_ab,
      mag_ab = purrr::map(mag_ab, ~ dplyr::select(., Feature, Concentration)), # drop Coverage column
      mag_ab = purrr::map2(mag_ab, Sample, ~ stats::setNames(.x, c("Feature", .y))), #put sample name in MAG table

      # Remove MAGs below LOD
      #mag_det = mag_ab,
      #mag_det = map2(mag_det, Sample, ~ setNames(.x, .y, 'Concentration')), #get sample name out of header for filter
      mag_det = purrr::map2(mag_det, lod, ~ dplyr::filter(.x, Concentration > .y)), #Retain MAGs with concentration above lod
      mag_det = purrr::map(mag_det, ~ dplyr::select(.x, Feature, Concentration)),
      mag_det = purrr::map2(mag_det, Sample, ~ stats::setNames(.x, c("Feature", .y))) #change header back to sample
    )
  if(cook_filtering == "TRUE") {
    scale_fac = scale_fac %>%
      dplyr::mutate(cooksd = purrr::map(fit, ~ stats::cooks.distance(.)), #calculate Cooks distance
                    influential_data = purrr::map(cooksd, ~as.numeric(names(.)[(. > (4/length(.)))])), #Identify row IDs which have data points higher than Cooks threshold
                    cooksd_plot = purrr::map(cooksd, ~ ggplot2::ggplot(tibble::as_tibble(.), ggplot2::aes(y = value, x = seq(1, length(.)))) +
                                               ggplot2::geom_point() + ggplot2::geom_hline(yintercept = 4/length(.)) +
                                               ggplot2::ggtitle("Cooks distance - \n horizontal line is 4/n (n is the # of data)") +
                                               ggplot2::xlab("#") + ggplot2::ylab("cooks distance")), #Plot Cooks distance
                    seq_cov_filt_temp = purrr::map2(seq_cov_filt, influential_data, ~dplyr::slice(.x,-.y)) #Retain only those points passing Cooks distance threshold
      )  %>%
      mutate(
        seq_cov_filt_temp_grouped = purrr::map(seq_cov_filt_temp, ~ dplyr::group_by(., Concentration) %>%
                                                 dplyr::summarise(mean_cov = mean(Coverage),
                                                                  sd_cov = stats::sd(Coverage)) %>%
        																			 	dplyr::mutate(dplyr::across(dplyr::all_of(c("mean_cov", "sd_cov")), function(col) dplyr::na_if(col, 0))) %>%
                                                 dplyr::mutate(coe_var = sd_cov*100/mean_cov) %>%
                                                 dplyr::mutate(threshold_detection = coe_var <= coe_of_variation) %>%
                                                 tidyr::drop_na()), #Calculate mean, standard deviation, and coefficient of variation of samples grouped by Concentration
        seq_cov_filt_round2 = purrr::map2(seq_cov_filt_temp, seq_cov_filt_temp_grouped, ~ dplyr::select(.x, -mean_cov, -sd_cov, -coe_var, -threshold_detection) %>%
                                            dplyr::inner_join(., .y, by = "Concentration")),
        seq_cov_filt_round2 = purrr::map(seq_cov_filt_round2, ~ dplyr::filter(., Concentration > 0)), #Retain data which have Concentration more than zero
        seq_cov_filt_round2 = purrr::map(seq_cov_filt_round2, ~ dplyr::filter(., Coverage > 0)), #Retain data which have Coverage more than zero
        seq_cov_filt_round2 = purrr::map(seq_cov_filt_round2, ~ tidyr::drop_na(.)), #Drop any NAs in the data
        outliers = purrr::map2(seq_cov_filt_temp, seq_cov_filt_temp_grouped, ~ dplyr::select(.x, -mean_cov, -sd_cov, -coe_var, -threshold_detection) %>%
                                 dplyr::anti_join(., .y, by = "Concentration")), #List outliers in the data
        zero_row_check = purrr::map(seq_cov_filt_round2, ~nrow(.)) # For linear regression, check if any samples have zero data points or only one data point. This precludes linear regression analysis
      ) %>%
      dplyr::filter(zero_row_check > 0) %>% #Filter samples which have one or zero data points in the seq_cov_filt_round2 tibble
      dplyr::mutate(
        seq_cov_filt_round2 = purrr::map(seq_cov_filt_round2, ~ .x %>%
                             dplyr::mutate(
                               log_10_coverage = log10(Coverage),
                               lod_10_concentration = log10(Concentration))),
        fit_filtered_lm = ifelse(log_trans == "TRUE" , # check log_trans input
                                 purrr::map(seq_cov_filt_round2, ~ stats::lm(lod_10_concentration ~ log_10_coverage , data = .)), #log lm if true
                                 purrr::map(seq_cov_filt_round2, ~ stats::lm(Concentration ~ Coverage , data =.)) # lm if false
        ),
        slope_filtered = purrr::map_dbl(fit_filtered_lm, ~ summary(.)$coef[2]), # get slope
        intercept_filtered = purrr::map_dbl(fit_filtered_lm, ~summary(.)$coef[1])
      ) %>%
      dplyr::filter(slope_filtered > 0) %>%
      dplyr::mutate(
        cooksd_filtered = purrr::map(fit_filtered_lm, ~ stats::cooks.distance(.)), #Recalculate Cooks distance to validate if the pipeline to filter out outliers worked
        cooksd_plot_filtered = purrr::map(cooksd_filtered, ~ ggplot2::ggplot(tibble::as_tibble(.), ggplot2::aes(y = value, x = seq(1, length(.)))) +
                                            ggplot2::geom_point() + ggplot2::geom_hline(yintercept = 4/length(.)) +
                                            ggplot2::ggtitle("Cooks distance - \n horizontal line is 4/n (n is the # of data)") + ggplot2::xlab("#") + ggplot2::ylab("cooks distance"))
      ) %>%
      dplyr::mutate(
        seq_cov_filt_round2 = purrr::map2(seq_cov_filt_round2, slope_filtered,
                                          ~dplyr::mutate(.x, slope = .y)),
        seq_cov_filt_round2 = purrr::map2(seq_cov_filt_round2, intercept_filtered,
                                          ~dplyr::mutate(.x, intercept = .y))
      ) %>%
      #Plot lm based graphs to show r squared on the graph
      dplyr::mutate(
        plots_filtered_lm = ifelse(log_trans == "TRUE" , # check log_trans input
                                   purrr::map(seq_cov_filt_round2, # log-scaled plot if true
                                              ~ ggplot2::ggplot(data=. , ggplot2::aes(x=log10(Coverage), y= log10(Concentration))) +
                                                ggplot2::geom_point(ggplot2::aes(shape = threshold_detection)) +
                                                ggplot2::geom_smooth(method = "lm") +
                                                ggpubr::stat_regline_equation(label.x= -0.1, label.y = 3) +
                                                ggpubr::stat_cor(ggplot2::aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.x = -0.1, label.y = 3.5) +
                                                ggplot2::xlab("Coverage (log[read depth])") +
                                                ggplot2::ylab("DNA Concentration (log[attamoles/uL])") +
                                                ggplot2::scale_shape(name = "Coefficient of variation", labels = c(paste("below the threshold (",coe_of_variation,")"), paste("above the threshold(",coe_of_variation,")"))) +
                                                ggplot2::theme_bw()
                                   ),
                                   purrr::map(seq_cov_filt_round2, # non-scaled plot if true
                                              ~ ggplot2::ggplot(data=. , ggplot2::aes(x=Coverage, y= Concentration)) +
                                                ggplot2::geom_point(ggplot2::aes(shape = threshold_detection)) +
                                                ggplot2::geom_smooth(method = "lm") +
                                                ggpubr::stat_regline_equation(label.x= 0, label.y.npc = 0.9, ggplot2::aes(label = paste(..adj.rr.label.., ..eq.label.., sep = "~`,`~"))) +
                                                ggpubr::stat_cor(ggplot2::aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.x = 0,label.y.npc = 0.8) +
                                                ggplot2::xlab("Coverage (read depth)") +
                                                ggplot2::ylab("DNA Concentration (attamoles/uL)") +
                                                ggplot2::scale_shape(name = "Coefficient of variation", labels = c(paste("below the threshold (",coe_of_variation,")"), paste("above the threshold(",coe_of_variation,")"))) +
                                                ggplot2::theme_bw()
                                   )
        )) %>%
      dplyr::mutate(
        # Scale MAGs based on robust linear regression
        mag_ab_filtered = ifelse(log_trans == "TRUE" , # check log_trans input
                                 purrr::map2(mag_cov, slope_filtered, ~ dplyr::mutate(.x, Concentration = log10(Coverage) * .y)), # y = mx (in log scale) if true
                                 purrr::map2(mag_cov, slope_filtered, ~ dplyr::mutate(.x, Concentration = Coverage * .y)) # y = mx if false
        ),
        mag_ab_filtered = purrr::map2(mag_ab_filtered, intercept_filtered, ~ dplyr::mutate(.x, Concentration = Concentration + .y)),
        mag_ab_filtered = ifelse(log_trans == "TRUE" , # check log_trans input
                                 purrr::map(mag_ab_filtered, ~ dplyr::mutate(.x, Concentration = 10^Concentration)), #convert back from log10 if true
                                 mag_ab_filtered), # no change if false
        mag_det_filtered = mag_ab_filtered,
        mag_ab_filtered = purrr::map(mag_ab_filtered, ~ dplyr::select(., Feature, Concentration)), # drop Coverage column
        mag_ab_filtered = purrr::map2(mag_ab_filtered, Sample, ~ stats::setNames(.x, c("Feature", .y))), #put sample name in MAG table

        # Remove MAGs below LOD
        #mag_det = mag_ab,
        #mag_det = map2(mag_det, Sample, ~ setNames(.x, .y, 'Concentration')), #get sample name out of header for filter
        mag_det_filtered = purrr::map2(mag_det_filtered, lod, ~ dplyr::filter(.x, Concentration > .y)), #Retain MAGs with concentration above lod
        mag_det_filtered = purrr::map(mag_det_filtered, ~ dplyr::select(.x, Feature, Concentration)),
        mag_det_filtered = purrr::map2(mag_det_filtered, Sample, ~ stats::setNames(.x, c("Feature", .y))))  #change header back to sample
  }


  # compile feature abundance across samples
  mag_tab = if (cook_filtering == "TRUE") {
    scale_fac$mag_ab_filtered %>%
      purrr::reduce(dplyr::full_join, by="Feature") %>%
      tibble::column_to_rownames(var = "Feature")
  } else {
    scale_fac$mag_ab %>%
      purrr::reduce(dplyr::full_join, by="Feature") %>%
      tibble::column_to_rownames(var = "Feature")
  }
  #Create regression plots directory
  dir.create(plot_dir)
  # extract plots
  plots <- scale_fac %>% dplyr::select(Sample, dplyr::contains("plot"))
  #Save scaling plots in .pdf format in the regression plots directory
  if(save_plots == "TRUE"){
  plots <- plots %>%
   dplyr::mutate(save_plots = purrr::map2(plots, Sample,  ~ ggplot2::ggsave(filename = paste("lm_scaled_",.y, ".pdf", sep=""), plot = .x, path = plot_dir, width=7,height=7, units = "in")))
  }
  if(save_plots == "TRUE"){
  if(cook_filtering == "TRUE") {
    dir.create(paste(plot_dir,"/cook_filtering", sep = ""))
    plots <- plots %>%
      dplyr::mutate(
        save_cooksd_plot = purrr::map2(cooksd_plot, Sample, ~ ggplot2::ggsave(filename = paste("cooksd_",.y, ".pdf", sep=""), plot = .x, path = paste(plot_dir,"/cook_filtering", sep = ""), width=7,height=7, units = "in")),
        save_cooksd_filtered_plot = purrr::map2(cooksd_plot_filtered, Sample, ~ ggplot2::ggsave(filename = paste("cooksd_filtered_",.y, ".pdf", sep=""), plot = .x, path = paste(plot_dir,"/cook_filtering", sep = ""),width=7,height=7, units = "in")),
        save_plots_filtered = purrr::map2(plots_filtered_lm, Sample,  ~ ggplot2::ggsave(filename = paste("cooks_distance_filtered_lm_scaled",.y, ".pdf", sep=""), plot = .x, path = paste(plot_dir,"/cook_filtering", sep = ""),width=7,height=7, units = "in"))
      )
  }
  }
  # extract feature detection
  mag_det = if (cook_filtering == "TRUE") {
    scale_fac$mag_det_filtered %>%
      purrr::reduce(dplyr::full_join, by="Feature") %>%
      tibble::column_to_rownames(var = "Feature")
  } else {
    scale_fac$mag_det %>%
      purrr::reduce(dplyr::full_join, by="Feature") %>%
      tibble::column_to_rownames(var = "Feature")
  }

  mag_det = mag_det %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), .fns = ~ tidyr::replace_na(.,0)))


  # make list of results
  results <- list("mag_tab" = mag_tab,
                  "mag_det" = mag_det,
                  "plots" = plots,
                  "scale_fac" = scale_fac)

 file.remove(plot_dir) #If plots are not saved, this directory will be empty and here it will be removed, if the dir is not empty and a name is not provided the script raises a warning
  return(results)
 }

