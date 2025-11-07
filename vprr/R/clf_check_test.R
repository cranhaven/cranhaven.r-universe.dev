## classifier check
# E. Chisholm

# edited by K. Sorochan

## January 6th, 2020
## version 3


vpr_manual_classification <-
  function(day,
           hour,
           basepath,
           taxa_of_interest,
           gr = TRUE,
           scale = 'x300',
           opticalSetting = 'S2',
           img_bright = TRUE) {
    #' Function to check results of classification manually
    #'
    #'
    #' Displays each image in day hour specified,
    #' prompts user to confirm or deny classification.
    #' If classification is denied, asks for a reclassification
    #'  value based on available taxa
    #'
    #' @param day day of interest in autoid
    #' @param hour hour of interest in autoid
    #' @param basepath file path to auto id folder eg 'E:/autoID_EC_07032019/'
    #' @param taxa_of_interest list of taxa folders you wish you sort through
    #' @param gr logical indicating whether pop up graphic menus are used (user preference - defaults to TRUE)
    #' @param scale argument passed to \code{\link{image_scale}}, default = 'x300'
    #' @param opticalSetting specifies optical setting of VPR, defining image frame
    #'   size, current options are 'S0', 'S1', 'S2' (default), 'S3', see further
    #'   info in details
    #'@param img_bright logical value indicating whether or not to include a blown
    #'  out high brightness version of image (can be helpful for viewing dark field
    #'  fine appendages)
    #'
    #' @details Optical Setting frame sizes: S0 = 7x7 mm, S1 = 14x14mm, S2 =
    #'   24x24mm, S3 = 48x48 mm. These settings define the conversion factor from
    #'   pixels to millimetres and calculate image size for classification
    #'   reference
    #'
    #'
    #' @section Development:
    #'   \itemize{
    #'       \item Add "undo" functionality to go back on a typing mistake
    #'       \item  Fix scaling/ size issue so images are consistently sized
    #'       \item show ROI number for image somewhere for reference when in doubt of classification
    #'       }
    #'
    #'@export


    day_hour <- paste0('d', day, '.h', hour)
    dirpath <- file.path("manual_reclassification_record",day_hour)
    dir.create(path = dirpath, showWarnings = FALSE, recursive = TRUE)
    existingFiles <- list.files(dirpath, full.names = TRUE)
    ans <-
      menu(
        c('Yes', 'No'),
        graphics = FALSE,
        title = paste(
          'WARNING!!! ALL EXISITING FILES IN', day_hour,
          'ARE ABOUT TO BE DELETED. DO YOU WISH TO PROCEED?'
        )
      )
    if (ans == 1) {
      file.remove(existingFiles)
    } else{
      warning(immediate. = TRUE,
              paste('CAUTION, FILES FOR', day_hour, 'ARE BEING APPENDED!!'))
    }

    taxaFolders_og <- list.files(basepath, full.names = TRUE)
    taxaNames <- list.files(basepath)
    allTaxa <- list.files(basepath)

    taxaFolders <- taxaFolders_og[taxaNames %in% taxa_of_interest]
    taxaNames <- taxaNames[taxaNames %in% taxa_of_interest]
    if (length(taxaFolders) == 0) {
      stop('No taxa folders match taxa of interest!
                                     Caution of capitalization!')
    }

    t_f <- dir.exists(taxaFolders)

    # Make an empty list for reclassficiations with named elements for each taxa
    reclassified <- vector("list", length(allTaxa))
    names(reclassified) <- allTaxa

    for (i in seq_len(length(taxaFolders))) {
      misclassified <- vector()

      print(paste('TAXA START : ', taxaFolders[i]))
      y <- readline(paste('CONFIRM NEW TAXA : ', taxaFolders[i]))
      # clear existing files
      path <- taxaFolders[i]

      if (t_f[i] == FALSE) {
        print(paste('TAXA : ', taxaFolders[i], 'DOES NOT EXIST!'))
        SKIP = TRUE
      } else{
        dayHrFolders <- list.files(path, full.names = TRUE)

        dayHrFolder <-
          grep(dayHrFolders, pattern = day_hour, value = TRUE)

        if (length(dayHrFolder) == 0) {
          print(paste('TAXA : ', taxaFolders[i], 'DOES NOT EXIST IN ', day_hour, '!'))
          SKIP = TRUE
        } else{
          SKIP = FALSE


          # grab aid file info
          aidFolder <-
            grep(dayHrFolders, pattern = 'aid$', value = TRUE)
          aidFile <-
            list.files(aidFolder, pattern = day_hour, full.names = TRUE)
          aid_dat <- read.table(aidFile, stringsAsFactors = FALSE) # TODO read in pred_results instead of aid
          aid_dat <- unique(aid_dat$V1) # KS added unique to duplicate bug fix
          rois <- list.files(dayHrFolder, full.names = TRUE)


          # find correct conversion factor based on VPR optical setting
          if (opticalSetting == 'S0') {
            # px to mm conversion factor
            frame_mm <- 7
            mm_px <-
              frame_mm / 1024 # 1024 is resolution of VPR images (p.4 DAVPR manual)
            pxtomm <- 1 / mm_px
          }
          if (opticalSetting == 'S1') {
            # px to mm conversion factor
            frame_mm <- 14
            mm_px <-
              frame_mm / 1024 # 1024 is resolution of VPR images (p.4 DAVPR manual)
            pxtomm <- 1 / mm_px
          }
          if (opticalSetting == 'S2') {
            # px to mm conversion factor
            frame_mm <- 24
            mm_px <-
              frame_mm / 1024 # 1024 is resolution of VPR images (p.4 DAVPR manual)
            pxtomm <- 1 / mm_px
          }
          if (opticalSetting == 'S3') {
            # px to mm conversion factor
            frame_mm <- 42 # correct conversion factor (7/11/2022)
            mm_px <-
              frame_mm / 1024 # 1024 is resolution of VPR images (p.4 DAVPR manual)
            pxtomm <- 1 / mm_px
          }

          for (ii in seq_len(length(rois))) {
            print(paste(ii, '/', length(rois)))
            img <- magick::image_read(rois[ii], strip = FALSE) %>%
              magick::image_scale(scale) %>%
              magick::image_annotate(taxaNames[i], color = 'red', size = 12)
            # read in original image without scaling
            img_o <- magick::image_read(rois[ii])
            imgdat <- magick::image_info(img_o)

            # annotate original image size
            img <-
              magick::image_annotate(
                img,
                text = paste(
                  round(imgdat$width / pxtomm, digits = 2),
                  'x',
                  round(imgdat$height / pxtomm, digits = 2),
                  'mm'
                ),
                location = '+0+10',
                color = 'red'
              )
            if (img_bright == TRUE) {
              img_n <- magick::image_modulate(img, brightness = 500)

              img_f <- magick::image_append(c(img, img_n))

              print(img_f)

            } else{
              print(img)
            }


            #pop up menu
            ans <-
              menu(
                choices = c('Yes', 'No'),
                graphics = gr,
                title = paste(
                  "Is the classification, ",
                  taxaNames[i],
                  ", accurate? (y/n)"
                )
              )

            if (ans == 1) {


            } else{
              # original method
              # sink(file = paste0(day_hour,'/misclassified_', taxaNames[i], '.txt'), append = TRUE)
              # cat(aid_dat[[ii]], '\n')
              # sink()

              misclassified <- c(misclassified, aid_dat[[ii]])


              # update to create generic taxa options
              # EC 2019 October 30
              ans <-
                menu(c(allTaxa),
                     graphics = gr,
                     title = "Appropriate Taxa Classification?")


              reclassified[[ans]] <-
                c(reclassified[[ans]], aid_dat[[ii]])

              # original method
              # sink(file = paste0(day_hour,'/reclassify_', allTaxa[[ans]], '.txt'), append = TRUE)
              # cat(aid_dat[ii], '\n')
              # sink()

            }


          }

          # Write information to file
          # sink(
          #   file = paste0(day_hour, '/misclassified_', taxaNames[i], '.txt'),
          #   append = T
          # )
          withr::with_output_sink(paste0(dirpath, '/misclassified_', taxaNames[i], '.txt'),
                                  append = TRUE,
                                  code = {
                                    cat(misclassified, sep = '\n')
                                  })
          #sink()

        }# skip = TRUE loop (taxa)
      }# skip = TRUE loop (dayhr)

      if (SKIP == TRUE) {
        # creates blank misclassified file if taxa of interest is not present in specified hour (so images reclassified as this taxa will be moved)
        # sink(
        #   file = paste0(day_hour, '/misclassified_', taxaNames[i], '.txt'),
        #   append = TRUE
        # )
        # sink()
        withr::with_output_sink(paste0(dirpath, '/misclassified_', taxaNames[i], '.txt'),
                                append = TRUE,
                                code = {
                                  cat('\n')
                                })
      }
    }

    # Write reclassified files for each taxa
    for (i in seq_len(length(reclassified))) {
      taxa_id <- names(reclassified[i])
      recl_tmp <- reclassified[[i]]

      # Make a reclassify file only for taxa that need to be reclassified
      if (length(recl_tmp != 0)) {
        # sink(
        #   file = paste0(day_hour, '/reclassify_', taxa_id, '.txt'),
        #   append = TRUE
        # )
        withr::with_output_sink(paste0(dirpath, '/reclassify_', taxa_id, '.txt'), append = TRUE, code = {
          cat(recl_tmp, sep = '\n')
        })
        # sink()

      }

    }

  }


vpr_autoid_create <- function(reclassify, misclassified, basepath, day, hour, mea = TRUE) {
  #' Modifies aid and aid mea files based on manual reclassification
  #' @author E. Chisholm
  #'
  #'@param reclassify list of reclassify files (output from vpr_manual_classification())
  #'@param misclassified list misclassify files (output from vpr_manual_classification())
  #'@param basepath base path to auto ID folder eg 'E:/autoID_EC_07032019/'
  #'@param day day identifier for relevant aid & aidmeas files
  #'@param hour  hour identifier for relevant aid & aidmeas files
  #'@param mea logical indicating whether or not there are accompanying measurement files to be created
  #'
  #'
  #' ### examples
  #'basepath <- 'E:/autoID_EC_07032019/'
  #'day <- '289'
  #'hr <- '08'
  #'day_hour_files <-  paste0('d', day, '.h', hr)
  #'misclassified <- list.files(day_hour_files, pattern = 'misclassified_', full.names = TRUE)
  #'reclassify <- list.files(day_hour_files, pattern = 'reclassify_', full.names = TRUE)
  #'vpr_autoid_create(reclassify, misclassified, basepath)
  #'
  #'@export

  # avoid CRAN notes
  . <- day <- hour <- NA
  taxaNames <- list.files(basepath)

  # find aid txt files
  taxaFolders <- list.files(basepath, full.names = TRUE)
  # remove misclassified ROIS
  for (i in seq_len(length(misclassified))) {
    # TODO: generalize solution, remove hardcoding
    # TODO make sure this works with new directory structure
    taxa <- vpr_category(misclassified[i])

   # if (taxa == 'ctenophores'){ browser()}
    #  <- substr(misclassified[i], 24, nchar(misclassified[i]) - 4)
    taxaFolder <- grep(taxaFolders, pattern = taxa, value = TRUE)
    if (!taxa %in% taxaNames) {
      stop(
        paste(
          taxa,
          'is not a valid taxa name. Please run vpr_category_create() to create proper file structure within basepath'
        )
      )
    }

    mis_roi <- readLines(misclassified[i])
    if (length(mis_roi) != 0) {
      day_hour <-
        unique(substr(sub(
          mis_roi, pattern = '.*d', replacement = 'd'
        ), 1, 8))
      day_hour <-
        gsub(pattern = "\\\\",
             replacement = '.',
             x = day_hour)

      # mis_roi_gen <- substr(mis_roi, nchar(mis_roi) - 18, nchar(mis_roi))
      mis_roi_gen <- unlist(vpr_roi(mis_roi))

      mis_roi_df <-
        data.frame(mis_roi_gen, day_hour, taxa, stringsAsFactors = FALSE)

      aidFolder <-
        list.files(taxaFolder, pattern = '^aid$', full.names = TRUE)

      mis_roi_df <- mis_roi_df %>%
        dplyr::group_by(., day_hour)

      if (length(unique(mis_roi_df$day_hour)) > 1) {
        stop('MULTIPLE HOURS IN ONE FILE, PLEASE CORRECT.')
      }
    } else{
      # if there is no misclassified information

      print(paste('Blank misclassified file found for', taxa, '!'))
      # browser()
      day_n <- vpr_day(misclassified[i])
      hr_n <- vpr_hour(misclassified[i])
      day_hour <- paste0('d', day, '.h', hour)
      # day_hour <- unique(substr(misclassified[i], 1, 8))
      aidFolder <-
        list.files(taxaFolder, pattern = '^aid$', full.names = TRUE)

    }
    # open correct day hour aid file
    aids <- list.files(aidFolder, full.names = TRUE)
    aid_list_old_fn <- grep(aids, pattern = day_hour , value = TRUE)
    # changed day hour pattern to accomodate lack of mis_roi_df in dummy = TRUE scenario


    # possibility that original data file does not exist
    if (length(aid_list_old_fn) == 0) {

      # make blank dummy data file to insert reclassified info into
      # needs file path (aidFolder)
      aid_list_old_fn <-
        paste0(aidFolder, '/dummy_svmaid.', day_hour)
      #sink(aid_list_old_fn)
      withr::with_output_sink(aid_list_old_fn, code = {
        cat('\n')
      })
      #sink()
      print(paste('DUMMY FILE CREATED FOR', taxa, ' : ', aid_list_old_fn))

      DUMMY = TRUE

      # blank file to be appended
      aid_new <- NULL
    } else{
      aid_list_old <- readLines(aid_list_old_fn)
      # BUG FIX 01/16/2020
      # issue where duplicated ROIs in original aid files were not getting removed with misclassified/ reclassified data
#browser()
      aid_list_old <- unique(aid_list_old)

      aid_old_gen <- unlist(vpr_roi(aid_list_old))


      # KS big fix: issue #24
      if(length(mis_roi) == 0) {

        aid_new <- aid_list_old

      } else {

      sub_mis_roi <- mis_roi_df %>%
        dplyr::filter(., day_hour == unique(mis_roi_df$day_hour))
      #%>%
        # dplyr::filter(.,!duplicated(mis_roi_gen)) #remove duplicates #BUG FIX 01/16/20

      mm <-   sub_mis_roi$mis_roi_gen %in% aid_old_gen #switched order to prevent error (EC: 01/16/2020)

      ind <- grep(mm , pattern = 'TRUE')

      # new list with misclassified rois removed
      aid_new <- aid_list_old[-ind]


      cat(
        paste(
          '>>>>',
          length(ind),
          'ROIs removed from',
          taxa ,
          'in',
          unique(day_hour),
          '\n>>>> File:',
          aid_list_old_fn,
          '\n'
        )
      )
      }
      DUMMY <- FALSE
    }

    # FIX MEAS FILE TO MATCH
    if(mea == TRUE){
    aidMeaFolder <-
      list.files(taxaFolder, pattern = '^aidmea$', full.names = TRUE)
    aidMeaFile <-
      list.files(aidMeaFolder,
                 pattern = paste0('*', day_hour),
                 full.names = TRUE)

    # if there is no original data file
    if (length(aidMeaFile) == 0) {
      # make dummy data file
      # needs file path
      aidMeaFile <-
        paste0(aidMeaFolder, "/dummy_svmaid.mea.", day_hour)
      # sink(aidMeaFile)
      withr::with_output_sink(aidMeaFile, code = {
        cat('\n')
      })
      # sink()

      print(paste('DUMMY FILE CREATED FOR MEAS OF', taxa, ' : ', aidMeaFile))

      aidMea_new <- NULL
      DUMMY = TRUE
    } else{
      aidMea_old <- read.table(aidMeaFile)

      aidMea_old <- unique(aidMea_old) # KS fix for bug duplicates

      # KS bug fix: Issue #24
      if(length(mis_roi) == 0) {

        aidMea_new <- aidMea_old

      } else { aidMea_new <- aidMea_old[-ind,]
      cat(
        paste(
          '>>>>',
          length(ind),
          'Measurements removed from',
          taxa ,
          'in',
          unique(day_hour),
          '\n>>>> File:',
          aidMeaFile,
          '\n'
        )
      )
      }


      DUMMY = FALSE
    }
    }
    # add reclassified rois
    # to specific taxa
    recl <- grep(reclassify, pattern = taxa)
    if (length(recl) == 0) {
      print(paste('No', taxa, 'to be reclassified'))
      # final files only have rois removed
      if(mea == TRUE){aidMea_final <- aidMea_new}
      aid_final <- aid_new
      if (DUMMY == TRUE) {
        warning(print(
          'No original data and no reclassified data, consider removing taxa.'
        ))
      }
    } else{
      # loop should end right before files are saved

      reclassify_taxa <-
        grep(reclassify, pattern = taxa, value = TRUE)


      # pull one reclassify file at a time
      recl_roi <- readLines(reclassify_taxa)
      # get day.hour info
      day_hour_re <- paste(day, hour, sep = ".") # arguments now given to function no need to find them in file names
      # day_hour_re <-
      #   substr(sub(recl_roi, pattern = '.*d', replacement = 'd'), 1, 8)
      # day_hour_re <-
      #   gsub(pattern = "\\\\",
      #        replacement = '.',
      #        x = day_hour_re)

      # get generic roi string
      recl_roi_gen <- unlist(vpr_roi(recl_roi))

      # which taxa to add recl rois to

      # check only one hour present in file
      if (length(unique(day_hour_re)) > 1) {
        stop(
          paste(
            reclassify_taxa,
            'has more than one unique hour value!
                                                     Please double check file.'
          )
        )
      }


      recl_roi_df <-
        data.frame(recl_roi_gen, day_hour_re, recl_roi,  stringsAsFactors = FALSE)


      recl_roi_df <- recl_roi_df %>%
        dplyr::filter(.,!duplicated(recl_roi_gen))
      # filter to remove duplicates causing errors in size and aid files being different lengths
      # script was not catching duplicates because of different vpr tow numbers


      # add reclassified rois
      aid_final <- c(aid_new, recl_roi_df$recl_roi)

      cat(paste(
        '>>>>',
        length(recl_roi_df$recl_roi),
        'ROIs added to',
        taxa ,
        'in',
        unique(day_hour),
        '\n'
      ))
      # ADD RECLASSIFIED ROI MEAS TO MEA FILE

      # find original meas file
      # need original taxa info

      # find original taxa data
      # read in all roi folders
      # weird folder character cut off problems



#deprecating getRoiMeasurements
      # bp <- substr(basepath, 1, nchar(basepath) - 1)
      # auto_id_folder <- bp
      # nchar_folder <- nchar(auto_id_folder)
      # taxafolder <- list.files(auto_id_folder, full.names = T)
      # auto_measure_px <-
        # getRoiMeasurements(taxafolder, nchar_folder, unit = 'px')
# DONE : Edit so that only required size data is loaded, without using getRoiMeasurements [ EC 28-01-2020 ]

      #get all taxa aid and aidmea files for day/hour of interest
      aid_fn_list <- list()
    for(l in seq_len(length(taxaFolders))){

      all_aids <- list.files(file.path(taxaFolders[[l]], 'aid'), full.names = TRUE)
      aid_fn_list[[l]] <- grep(all_aids, pattern = day_hour, value = TRUE)
    }
if(mea == TRUE){
      aidm_fn_list <- list()
      for(l in seq_len(length(taxaFolders))){

        all_aidms <- list.files(file.path(taxaFolders[[l]], 'aidmea'), full.names = TRUE)
        aidm_fn_list[[l]] <- grep(all_aidms, pattern = day_hour, value = TRUE)
      }


      # MEASUREMENTS
# browser()

      roimeas_dat_combine <-
        vpr_autoid_read(
          file_list_aid = unlist(aid_fn_list),
          file_list_aidmeas = unlist(aidm_fn_list),
          export = 'aidmeas',
          station_of_interest = NA,
          warn = FALSE
        )



      # find roi ids in meas

      recl_roi_num <- recl_roi_df$recl_roi_gen

      # day_hour_var <- day_hour #deprecated with getroiMeas

      # subset auto measure to correct day hour
      #deprecated with getRoiMeas
      # auto_measure_px <- auto_measure_px %>%
       # dplyr::filter(., day_hour == day_hour_var)

      # find index of recl rois in auto measure

      recl_roi_meas <-
        roimeas_dat_combine[roimeas_dat_combine$roi %in% recl_roi_num ,]


      # getRoiMeas method
       # recl_roi_meas <-
        # auto_measure_px[auto_measure_px$roi_ID %in% recl_roi_num ,]

      # check for duplicate ROI IDs

      if (length(recl_roi_meas$roi_ID) > length(recl_roi)) {

        print(paste(
          'Warning, duplicate ROI detected! Removing automatically'
        ))
        print(recl_roi_meas[duplicated(recl_roi_meas$roi_ID), ])

        recl_roi_meas <- recl_roi_meas %>%
          dplyr::filter(.,!duplicated(recl_roi_meas$roi_ID))
      }

      # append old aidmea file with mis rois removed

      #find measurement columns

      col_names <- c('Perimeter','Area','width1','width2','width3','short_axis_length','long_axis_length')

      recl_roi_meas <- recl_roi_meas %>%
        dplyr::select(., col_names)

      #combine new reclassifed meas data with original meas data
      aidMea_list <- list()
      for (iii in 1:7) {
        aidMea_list[[iii]] <-
          c(aidMea_new[, iii], unname(recl_roi_meas[, iii]))
      }
      # get into data frame format
      aidMea_final <-
        data.frame(matrix(unlist(aidMea_list), ncol = length(aidMea_list)))

      cat(paste(
        '>>>>',
        length(recl_roi_meas$Perimeter),
        'Measurements added to',
        taxa ,
        'in',
        unique(day_hour),
        '\n'
      ))

      if (length(recl_roi_meas$Perimeter) != length(recl_roi_df$recl_roi_gen)) {
        warning("Measurements and ROI numbers in reclassification do not match!!!")
      }
    }# end reclassified loop
    }
    # save files
    dirpath <- file.path('new_autoid', taxa[[1]])
    dir.create(dirpath, showWarnings = FALSE, recursive = TRUE)


if(mea == TRUE){
    aidMea_final_nm <- paste0('new_aid.mea.', unique(day_hour))
    aidMea_final_fn <- file.path(dirpath, 'aidmea', aidMea_final_nm)
    dir.create(file.path(taxa, 'aidmea'),
               showWarnings = FALSE,
               recursive = TRUE)
    write.table(
      file = aidMea_final_fn,
      aidMea_final,
      sep = "    ",
      quote = FALSE,
      col.names = FALSE,
      row.names = FALSE
    )
}
    # note output could be better formatted to match line width in original files

    aid_final_nm <- paste0('new_aid.', unique(day_hour))
    aid_final_fn <- file.path(dirpath, 'aid', aid_final_nm)
    dir.create(file.path(dirpath, 'aid'),
               showWarnings = FALSE,
               recursive = TRUE)
    write.table(
      file = aid_final_fn,
      aid_final,
      quote = FALSE,
      col.names = FALSE,
      row.names = FALSE
    )
    cat(paste(
      '>>>> New aid and aid.mea files created for',
      taxa,
      'in',
      unique(day_hour),
      '\n'
    ))


    # remove dummy files if they exist

    if (DUMMY == TRUE) {
      atf <- grep(aid_list_old_fn, pattern = 'dummy')
      amtf <- grep(aidMeaFile, pattern = 'dummy')

      if (length(atf) != 0 & length(amtf) != 0) {
        print(paste('Deleting dummy files!'))
        print(paste(aidMeaFile, ' & ', aid_list_old_fn))

        unlink(aid_list_old_fn)
        unlink(aidMeaFile)

      }

    }

  }


}



# function to create new taxa within data structure post VP output

#' Create a new taxa to be considered for classification after processing with VP
#'
#' creates empty directory structure to allow consideration of new taxa during vpr_manual_classification()
#'
#' @param taxa new taxa name to be added (can be a list of multiple taxa names)
#' @param basepath basepath used for vpr_manual_classification
#'
#' @return empty directory structure using new taxa name inside basepath
#' @export
#'
#'
#'
#'
vpr_category_create <- function(taxa, basepath) {
  for (i in seq_len(length(taxa))) {
    # create new taxa folder
    newtaxapath <- file.path(basepath, taxa[[i]])
    dir.create(newtaxapath)

    # create blank aid and aidmeas folders

    dir.create(paste0(newtaxapath, '/aid'), showWarnings = FALSE)
    dir.create(paste0(newtaxapath, '/aidmea'), showWarnings = FALSE)

  }
}
