# E. chisholm
# March 23, 2020

# creating sample data for use in examples and tests for vprr
# THIS SHOULD ONLY BE RUN BY DEVELOPERS WHEN UPDATING DATA FOR TESTING

# set up data directory

# save_dir <- 'data/processed/'


## load raw files


## PROCESS
# process raw data saving a new data object after the use of each function

##### PROCESSING  --------------------------------------------------------------------------------------------------------------------
#library(vprr)
#library(dplyr)

# source('R/EC_functions.R')

#### FILE PATHS & SETTINGS --------------------------------------------------------------------------------------------------------------------
# loads processing environment specific to user

cruise <- 'COR2019002'
station_of_interest <- 'test'
day_of_interest <- '222'
hour_of_interest <- c('03', '04')
dayhour <- paste0('d', day_of_interest, '.h', hour_of_interest)
year <- '2019'
binSize <- 5
category_of_interest <- c('Calanus', 'krill')

#VPR OPTICAL SETTING (S0, S1, S2 OR S3)
opticalSetting <- "S2"
imageVolume <- 83663 #mm^3

castdir <- 'inst/extdata/COR2019002/rois/vpr5/d222/'
auto_id_folder <- 'inst/extdata/COR2019002/autoid/'
auto_id_path <- list.files(paste0(auto_id_folder, "/"), full.names = T)


  # TODO: include station names file
  # get day and hour info from station names list
  # dayhour <- vpr_dayhour(station_of_interest, file = station_names_file)


  ##### PULL CTD CASTS ----------------------------------------------------------------------------------------------------------------------------
  # get file path for ctd data

  # list ctd files for desired day.hours
  # ctd_files <- vpr_ctd_files(castdir, cruise, dayhour)
  ctd_files <- list.files('.dat', path = castdir, full.names = TRUE)
#ctd_files <- list()
#ctd_files[[1]] <- system.file('extdata/COR2019002/rois/vpr5/d222', 'h03ctd.dat', package = 'vprr', mustWork = TRUE)
#ctd_files[[2]] <- system.file('extdata/COR2019002/rois/vpr5/d222', 'h04ctd.dat', package = 'vprr', mustWork = TRUE)

##### READ CTD DATA ----------------------------------------------------------------------------------------------------------------------------

  ctd_dat_combine <- vpr_ctd_read(ctd_files, station_of_interest)

 # subset data for size concerns
ctd_dat_combine <- ctd_dat_combine[1:1000,]
# save(ctd_dat_combine, file = paste0(save_dir, 'vpr_ctd_read.RData'))
usethis::use_data(ctd_dat_combine, overwrite = TRUE)
  ##### FIND VPR DATA FILES ----------------------------------------------------------------------------------------------------------------------

  # Path to aid for each taxa
  aid_path <- paste0(auto_id_path, '/aid/')
  # Path to mea for each taxa
  aidmea_path <- paste0(auto_id_path, '/aidmea/')

  # AUTO ID FILES
  aid_file_list <- list()
  aidmea_file_list <- list()
  for (i in seq_len(length(dayhour))) {
    aid_file_list[[i]] <-
      list.files(aid_path, pattern = dayhour[[i]], full.names = TRUE)
    # SIZE DATA FILES
    aidmea_file_list[[i]] <-
      list.files(aidmea_path, pattern = dayhour[[i]], full.names = TRUE)
  }

  aid_file_list_all <- unlist(aid_file_list)
  aidmea_file_list_all <- unlist(aidmea_file_list)


  # save(aid_file_list_all, file = paste0(save_dir,'aid_files.RData'))
  # save(aidmea_file_list_all, file = paste0(save_dir, 'aidmea_files.RData'))

  # usethis::use_data( aid_file_list_all, overwrite = TRUE)
  # usethis::use_data(aidmea_file_list_all, overwrite = TRUE)
  ##### READ ROI AND MEASUREMENT DATA ------------------------------------------------------------------------------------------------------------


  # ROIs
  roi_dat_combine <-
    vpr_autoid_read(
      file_list_aid = aid_file_list_all,
      file_list_aidmeas = aidmea_file_list_all,
      export = 'aid',
      station_of_interest = station_of_interest,
      opticalSetting = opticalSetting,
      warn = FALSE
    )

  # subset for size concerns
  roi_dat_combine <- roi_dat_combine[1:1000,]
  # save(roi_dat_combine, file = paste0(save_dir, 'vpr_autoid_read_aid.RData'))
  usethis::use_data(roi_dat_combine, overwrite = TRUE)

  # MEASUREMENTS
  roimeas_dat_combine <-
    vpr_autoid_read(
      file_list_aid = aid_file_list_all,
      file_list_aidmeas = aidmea_file_list_all,
      export = 'aidmeas',
      station_of_interest = station_of_interest,
      opticalSetting = opticalSetting,
      warn = FALSE
    )

  # subset for size concerns
  roimeas_dat_combine <- roimeas_dat_combine[1:1000,]

 # save(roimeas_dat_combine, file = paste0(save_dir, 'vpr_autoid_read_aidmeas.RData'))
 usethis::use_data(roimeas_dat_combine, overwrite = TRUE)


  ##### MERGE CTD AND ROI DATA ---------------------------------------------------------------------------------------------------------------------
  ctd_roi_merge <- vpr_ctdroi_merge(ctd_dat_combine, roi_dat_combine)

 # save(ctd_roi_merge, file = paste0(save_dir, 'vpr_ctdroi_merge.RData'))
 usethis::use_data(ctd_roi_merge, overwrite = TRUE)

  ##### CALCULATED VARS ----------------------------------------------------------------------------------------------------------------------------

  # add avg hr and sigma T data and depth
  data <- ctd_roi_merge %>%
    dplyr::mutate(., avg_hr = time_ms / 3.6e+06)

  data <- vpr_ctd_ymd(data, year)


  ##### BIN DATA AND DERIVE CONCENTRATION ----------------------------------------------------------------------------------------------------------

  ctd_roi_oce <- vpr_oce_create(data)

  # save(ctd_roi_oce, file = paste0(save_dir, 'vpr_oce_create.RData'))
  usethis::use_data(ctd_roi_oce, overwrite = TRUE)


  # bin and calculate concentration for all taxa (combined)
  # vpr_depth_bin <- bin_cast(ctd_roi_oce = ctd_roi_oce, binSize =  binSize, imageVolume = imageVolume)

  # save(vpr_depth_bin, file = paste0(save_dir, 'bin_vpr_data.RData'))
  # usethis::use_data(vpr_depth_bin, overwrite = TRUE)

  # get list of valid taxa
   taxas_list <- unique(roimeas_dat_combine$taxa)

  # bin and calculate concentrations for each category
  # taxa_conc_n <- vpr_roi_concentration(data, taxas_list, station_of_interest, binSize, imageVolume)

  # save(taxa_conc_n, file = paste0(save_dir, 'vpr_roi_concentration.RData'))
  # usethis::use_data(taxa_conc_n, overwrite = TRUE)


  # bin size data

  # size_df_f <- vpr_ctdroisize_merge(data, data_mea = roimeas_dat_combine, taxa_of_interest = category_of_interest)

  # save(size_df_f, file = paste0(save_dir, 'vpr_ctdroisize_merge.RData'))
  # usethis::use_data(size_df_f, overwrite = TRUE)


  ##### SAVE DATA ---------------------------------------------------------------------------------------------------------------------------------
  # Save oce object
  # oce_dat <- vpr_save(taxa_conc_n)

  # save(oce_dat, file = paste0(save_dir, 'vpr_save.RData'))
  # usethis::use_data(oce_dat, overwrite = TRUE)


  # Save RData files
  # save(file = paste0(savedir, '/ctdData_', station_of_interest,'.RData'), ctd_dat_combine) #CTD data
  # save(file = paste0(savedir, '/stationData_', station_of_interest,'.RData'), data) # VPR and CTD data
  # save(file = paste0(savedir, '/meas_dat_', station_of_interest,'.RData'), roimeas_dat_combine) #measurement data
  # save(file = paste0(savedir, '/bin_dat_', station_of_interest,'.RData'), vpr_depth_bin) # binned data with cumulative concentrations
  # save(file = paste0(savedir, '/bin_size_dat_', station_of_interest,'.RData'), size_df_b) # binned data inclouded measurements
