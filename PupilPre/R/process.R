#' Check the classes of specific columns and re-assigns as necessary.
#'
#' \code{ppl_prep_data} checks for necessary columns and converts the class
#' if needed.
#'
#' @export
#' @import dplyr
#' @import rlang
#' @import VWPre
#'
#' @param data A data frame object created from an Eyelink Sample Report.
#' @param Subject An obligatory string containing the column name corresponding to the subject identifier.
#' @param Item An optional string containing the column name corresponding to the item identifier; by default, NA.
#' @param EventColumns A vector specifying the columns which will be used for creating
#' the Event variable; by default, Subject and TRIAL_INDEX.
#' @return An object of type data table as described in \link[tibble]{tibble}.
#' @examples
#' # Load example data
#' data("Pupilex1")
#'
#' dat <- ppl_prep_data(Pupilex1, Subject = "RECORDING_SESSION_LABEL",
#'                      Item = "item",
#'                      EventColumns = c("Subject","TRIAL_INDEX"))
#'
#' # Please see the vignettes for detailed example usage.
#' # vignette("PupilPre_Basic_Preprocessing", package="PupilPre")
#'
ppl_prep_data <- function(data, Subject = NULL, Item = NA,
                      EventColumns=c("Subject","TRIAL_INDEX")){

    reqcols <- data.frame(Column=c("RECORDING_SESSION_LABEL",
                                   "LEFT_PUPIL_SIZE",
                                   "LEFT_GAZE_X",
                                   "LEFT_GAZE_Y",
                                   "LEFT_IN_BLINK",
                                   "LEFT_IN_SACCADE",
                                   "LEFT_ACCELERATION_X",
                                   "LEFT_ACCELERATION_Y",
                                   "LEFT_VELOCITY_X",
                                   "LEFT_VELOCITY_Y",
                                   "RIGHT_PUPIL_SIZE",
                                   "RIGHT_GAZE_X",
                                   "RIGHT_GAZE_Y",
                                   "RIGHT_IN_BLINK",
                                   "RIGHT_IN_SACCADE",
                                   "RIGHT_ACCELERATION_X",
                                   "RIGHT_ACCELERATION_Y",
                                   "RIGHT_VELOCITY_X",
                                   "RIGHT_VELOCITY_Y",
                                   "TIMESTAMP",
                                   "TRIAL_INDEX"),
                          Present=NA,
                          Mode=c("factor",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric",
                                 "numeric"),
                          stringsAsFactors = FALSE)
    optcols <- data.frame(Column=c("SAMPLE_MESSAGE",
                                   "EYE_TRACKED"),
                          Present=NA,
                          Mode=c("factor",
                                 "factor"),
                          stringsAsFactors = FALSE)

  {## SHARED CODE BEGINS HERE ##

    data <- as_tibble(data)

    message("Checking required columns...")

    for (x in 1:nrow(reqcols)) {
      if (!(reqcols[x,1] %in% names(data))) {
        reqcols[x,2] <- 0
      }
      else {
        reqcols[x,2] <- 1
      }
    }

    missingcols <- filter(reqcols, Present==0)

    if (nrow(missingcols) > 0) {
      stop(paste("\n The following column is required to process the data: ", unique(as.factor(missingcols$Column))))
    } else {
      message("    All required columns are present in the data.")
    }


    message("Checking optional columns...")

    for (x in 1:nrow(optcols)) {
      if (!(optcols[x,1] %in% names(data))) {
        optcols[x,2] <- 0
      }
      else {
        optcols[x,2] <- 1
      }
    }

    missingoptcols <- filter(optcols, Present==0)

    if (nrow(missingoptcols) > 0) {
      message(paste("    The following optional is not present in the data: ", unique(as.factor(missingoptcols$Column)), "\n"))
    } else {
      message("    All optional columns are present in the data.")
    }

    # Conversion helper function
    .conversionhelper <- function(data, columnname, datamode){
      if(datamode=="numeric"){
        if (is.numeric(data[, columnname]) == FALSE){
          if(is.factor(data[, columnname]) == TRUE){
            data[, columnname] <- lapply(data[, columnname], as.character)
          }
          conv <- lapply(data[, columnname], as.numeric)
          message(paste0("    ", columnname, " converted to numeric."))
        } else {
          conv <- data[, columnname]
          message(paste0("    ", columnname, " already numeric."))
        }
      }
      if(datamode=="factor"){
        if (is.factor(data[, columnname]) == FALSE){
          conv <- lapply(data[, columnname], factor)
          message(paste0("    ", columnname, " converted to factor."))
        } else {
          conv <- data[, columnname]
          message(paste0("    ", columnname, " already factor."))
        }
      }
      return(conv)
    }

    message("Working on required columns...")

    # Work on Subject
    if(is.null(Subject)){
      stop("Please supply the name of the subject column!")
    } else {
      subject <- Subject
      subject <- enquo(subject)
    }

    data <- rename(data, Subject = !!subject)
    message(paste("   ", quo_name(subject), "renamed to Subject. "))
    reqcols[1,"Column"] <- "Subject"

    # Work on Item
    item <- Item
    if (!is.na(item)) {
      if (!(item %in% names(data))) {
        stop(paste(item, "is not a column name in the data."))
      }
      item <- enquo(item)
      data <- rename(data, Item = !!item)
      message(paste("   ", quo_name(item), "renamed to Item."))
      reqcols <- rbind(reqcols, c("Item", "factor", 1))
    } else {
      reqcols <- rbind(reqcols, c("Item", "factor", 0))
      message("    No Item column specified.")
    }

    # Check and convert
    rc <- filter(reqcols, Present==1)
    for(i in 1:nrow(rc)){
      data[, rc[i,1]] <- .conversionhelper(data, rc[i,1], rc[i,3])
    }


    # Event column
    if (!(EventColumns[1] %in% names(data))) {
      stop(paste(EventColumns[1], "is not a column name in the data."))
    }
    if (!(EventColumns[2] %in% names(data))) {
      stop(paste(EventColumns[2], "is not a column name in the data."))
    }
    #data$Event <- interaction(data[,EventColumns], drop=TRUE)
    data$Event <- as.factor(as.character(paste(data[[EventColumns[1]]], data[[EventColumns[2]]], sep = ".")))
    message(paste("    Event variable created from", EventColumns[1], "and", EventColumns[2]), "")


    message("Working on optional columns...")

    if (nrow(missingoptcols) == nrow(optcols)) {
      message("    No optional columns present in the data.")
    }

    # Check and convert
    oc <- filter(optcols, Present==1)
    for(i in 1:nrow(oc)){
      data[,oc[i,1]] <- .conversionhelper(data, oc[i,1], oc[i,3])
    }

    } ## SHARED CODE ENDS HERE ##

    return(droplevels(ungroup(data)))
}




#' Checks for and removes unnecessary DV output columns.
#'
#' \code{ppl_rm_extra_DVcols} checks for unnecessary DataViewer output columns and
#' removes them, unless specified.
#'
#' @export
#' @import dplyr
#' @import rlang
#' @import VWPre
#'
#' @param data A data frame object created from an Eyelink Sample Report.
#' @param Keep An optional string or character vector containing the column names
#' of SR Research sample report columns the user would like to keep in the data set.
#' @return An object of type data table as described in \link[tibble]{tibble}.
#' @examples
#' # Load example data
#' data("Pupilex1")
#'
#' dat <- ppl_rm_extra_DVcols(Pupilex1, Keep = NULL)
#'
#' # Please see the vignettes for detailed example usage.
#' # vignette("PupilPre_Basic_Preprocessing", package="PupilPre")
#'
ppl_rm_extra_DVcols <- function(data, Keep=NULL){

    SR_not_needed <- c(
      "AVERAGE_ACCELERATION_X",
      "AVERAGE_ACCELLERATION_X", # THIS IS A DV MISSPELLING
      "AVERAGE_ACCELERATION_Y",
      "AVERAGE_ACCELLERATION_Y", # THIS IS A DV MISSPELLING
      "AVERAGE_GAZE_X",
      "AVERAGE_GAZE_Y",
      "AVERAGE_IN_BLINK",
      "AVERAGE_IN_SACCADE",
      "AVERAGE_INTEREST_AREAS",
      "AVERAGE_INTEREST_AREA_DATA",
      "AVERAGE_INTEREST_AREA_DISTANCE",
      "AVERAGE_INTEREST_AREA_ID",
      "AVERAGE_INTEREST_AREA_LABEL",
      "AVERAGE_INTEREST_AREA_PIXEL_AREA",
      "AVERAGE_Pupil_SIZE",
      "AVERAGE_VELOCITY_X",
      "AVERAGE_VELOCITY_Y",
      "HTARGET_DISTANCE",
      "HTARGET_FLAGS",
      "HTARGET_X",
      "HTARGET_Y",
      "IP_DURATION",
      "IP_END_EVENT_MATCHED",
      "IP_END_TIME",
      "IP_INDEX",
      "IP_LABEL",
      "IP_START_EVENT_MATCHED",
      "IP_START_TIME",
      "LEFT_INTEREST_AREA_ID",
      "LEFT_INTEREST_AREA_LABEL",
      "LEFT_FIX_INDEX",
      "LEFT_INTEREST_AREAS",
      "LEFT_INTEREST_AREA_DATA",
      "LEFT_INTEREST_AREA_DISTANCE",
      "LEFT_INTEREST_AREA_PIXEL_AREA",
      "LEFT_SACCADE_INDEX",
      "RESOLUTION_X",
      "RESOLUTION_Y",
      "LEFT_INTEREST_AREA_ID",
      "LEFT_INTEREST_AREA_LABEL",
      "RIGHT_FIX_INDEX",
      "RIGHT_INTEREST_AREAS",
      "RIGHT_INTEREST_AREA_DATA",
      "RIGHT_INTEREST_AREA_DISTANCE",
      "RIGHT_INTEREST_AREA_PIXEL_AREA",
      "RIGHT_INTEREST_AREA_LABEL",
      "RIGHT_SACCADE_INDEX",
      "SAMPLE_BUTTON",
      "SAMPLE_INPUT",
      "TARGET_ACCELERATION_X",
      "TARGET_ACCELLERATION_X", # THIS IS A DV MISSPELLING
      "TARGET_ACCELERATION_Y",
      "TARGET_ACCELLERATION_Y", # THIS IS A DV MISSPELLING
      "TARGET_Velocity_X",
      "TARGET_Velocity_Y",
      "TARGET_VISIBLE",
      "TARGET_X",
      "TARGET_Y",
      "TRIAL_LABEL",
      "TRIAL_START_TIME",
      "VIDEO_FRAME_INDEX",
      "VIDEO_NAME"
    )

    {## SHARED CODE BEGINS HERE ##

      data <- ungroup(data)

      delcols <- data.frame(Column=SR_not_needed, Present=NA, Keep=NA)
      delcols$Column <- as.factor(delcols$Column)

      message("Checking the data for extra (deletable) DV columns.")

      for (x in 1:nrow(delcols)) {
        if (!(delcols[x,1] %in% names(data))) {
          delcols[x,2] <- 0
          delcols[x,3] <- 0
        }
        else {
          delcols[x,2] <- 1
          if (delcols[x,1] %in% Keep) {
            delcols[x,3] <- 1
          } else {
            delcols[x,3] <- 0
          }
        }
      }

      deletecols <- filter(delcols, Present==1) %>% droplevels()

      if (nrow(deletecols) == 0) {
        stop("No deletable columns present in the data.")
      }

      if (!(is.null(Keep))) {
        message("Checking for columns to keep...")
      }
      message("     Columns kept by request: ", paste(levels(droplevels(deletecols[deletecols$Keep==1,]$Column)), collapse = ", "))
      deletecols <- filter(deletecols, Keep==0) %>% droplevels()

      del <- unique(levels(deletecols$Column))

      message("Removing deletable columns...")
      message("     Columns removed: ", paste(levels(deletecols$Column), collapse = ", "))
      data <- select(data, -one_of(del))

      return(droplevels(ungroup(data)))

    } ## SHARED CODE ENDS HERE ##
}


#' Select the eye used during recording
#'
#' \code{ppl_select_recorded_eye} examines each event and determines which
#' eye contains interest area information, based on the \code{Recording}
#' parameter (which can be determined using \code{\link{ppl_check_eye_recording}}).
#' This function then selects the data from the recorded eye and copies it to
#' new columns (Pupil, Gaze_X, Gaze_Y, Velocity_X, Velocity_Y, Acceleration_X,
#' Acceleration_Y, In_Blink, In_Saccade). The function prints a summary of the output.
#'
#' @export
#' @import dplyr
#' @import rlang
#' @import VWPre
#'
#' @param data A data table object output by \code{\link{create_time_series}}.
#' @param Recording A string indicating which eyes were used for recording gaze data
#' ("R" when only right eye recording is present, "L" when only left eye recording
#' is present, "LorR" when either the left or the right eye was recorded, "LandR"
#' when both the left and the right eyes were recorded).
#' @param WhenLandR A string indicating which eye ("Right" or "Left) to use
#' if gaze data is available for both eyes (i.e., Recording = "LandR").
#' @return A data table with 11 additional columns added to \code{data}.
#' @examples
#' # Load example data
#' data("Pupilex2")
#'
#' dat <- ppl_select_recorded_eye(data = Pupilex2, Recording = "R",
#'                                WhenLandR = "Right")
#'
#' # Please see the vignettes for detailed example usage.
#' # vignette("PupilPre_Basic_Preprocessing", package="PupilPre")
#'
ppl_select_recorded_eye <- function(data, Recording = NULL, WhenLandR = NA) {

  message(paste("Selecting gaze data needed for pupil processing..."))

  # Establish which columns needed
    lcol <- "LEFT_PUPIL_SIZE"
    rcol <- "RIGHT_PUPIL_SIZE"

    {## SHARED CODE BEGINS HERE ##

      if("EYE_TRACKED" %in% names(data)) {
        message("Selecting gaze data using Data Viewer column EYE_TRACKED")
      } else {
        if(is.null(Recording)){
          stop("Please supply the recording eye(s)!")
        }
        message(paste("Selecting gaze data using Data Viewer columns", lcol, "and", rcol, "and the Recording argument:", Recording))
      }

      if("EYE_TRACKED" %in% names(data)) {

        # Previous versions of DataViewer output "Both", Newer versions (>=4.1.1) output "Binocular"
        # The following code allows for both types of input
        if(("Both" %in% unique(data$EYE_TRACKED) | "Binocular" %in% unique(data$EYE_TRACKED)) & is.na(WhenLandR)){
          stop("Please specify which eye to use when Recording is set to 'LandR'!")
        }
        tmp <- data %>%
          group_by(Event) %>%
          mutate(., EyeRecorded = as.character(EYE_TRACKED)) %>%
          do(
            mutate(., EyeSelected = ifelse(EyeRecorded %in% c("Both", "Binocular") & WhenLandR == "Right", "Right",
                                           ifelse(EyeRecorded %in% c("Both", "Binocular") & WhenLandR == "Left", "Left",
                                                  ifelse(EyeRecorded == "Right", EyeRecorded,
                                                         ifelse(EyeRecorded == "Left", EyeRecorded, NA)))))
          )

      } else {

        # Prep columns for dplyr
        lcol <- enquo(lcol)
        rcol <- enquo(rcol)

        if (Recording == "LandR") {

          if(is.na(WhenLandR)){
            stop("Please specify which eye to use when Recording is set to 'LandR'!")
          }

          tmp <- data %>%
            group_by(Event) %>%
            mutate(., EyeRecorded = ifelse(sum(UQ(sym(eval_tidy(lcol))), na.rm = TRUE) > 0 &&
                                             sum(UQ(sym(eval_tidy(rcol))), na.rm = TRUE) > 0, "Both",
                                           ifelse(sum(UQ(sym(eval_tidy(lcol))), na.rm = TRUE) > 0 &&
                                                    sum(UQ(sym(eval_tidy(rcol))), na.rm = TRUE) == 0, "Left",
                                                  ifelse(sum(UQ(sym(eval_tidy(lcol))), na.rm = TRUE) == 0 &&
                                                           sum(UQ(sym(eval_tidy(rcol))), na.rm = TRUE) > 0, "Right", "NoData")))) %>%
            do(
              mutate(., EyeSelected = ifelse(EyeRecorded == "Both" & WhenLandR == "Right", "Right",
                                             ifelse(EyeRecorded == "Both" & WhenLandR == "Left", "Left",
                                                    ifelse(EyeRecorded == "Right", EyeRecorded,
                                                           ifelse(EyeRecorded == "Left", EyeRecorded,
                                                                  ifelse(EyeRecorded == "NoData", "Neither"))))))
            )


        }

        if (Recording == "LorR") {

          tmp <- data %>%
            group_by(Event) %>%
            mutate(., EyeRecorded = ifelse(sum(UQ(sym(eval_tidy(lcol))), na.rm = TRUE) > 0 &
                                             sum(UQ(sym(eval_tidy(rcol))), na.rm = TRUE) == 0, "Left",
                                           ifelse(sum(UQ(sym(eval_tidy(lcol))), na.rm = TRUE) == 0 &
                                                    sum(UQ(sym(eval_tidy(rcol))), na.rm = TRUE) > 0,
                                                  "Right", "NoData"))) %>%
            do(
              mutate(., EyeSelected = ifelse(EyeRecorded == "Right", EyeRecorded,
                                             ifelse(EyeRecorded == "Left", EyeRecorded,
                                                    ifelse(EyeRecorded == "NoData", "Neither"))))
            )

        }

        if (Recording == "R" | Recording == "L") {
          if (Recording == "R") {
            col <- rcol
            val <- "Right"
            val <- enquo(val)
          } else {
            col <- lcol
            val <- "Left"
            val <- enquo(val)
          }

          tmp <- data %>%
            group_by(Event) %>%
            mutate(., EyeRecorded = ifelse(sum(UQ(sym(eval_tidy(col))), na.rm = TRUE) > 0, quo_name(val), "NoData")) %>%
            do(
              mutate(., EyeSelected = ifelse(EyeRecorded == quo_name(val), EyeRecorded,
                                             ifelse(EyeRecorded == "NoData", "Neither")))
            ) %>% ungroup()
        }


      }

    } ## SHARED CODE ENDS HERE ##

  # Transfer columns
    tmp <- tmp %>% group_by(Event) %>%
        mutate(., Pupil = ifelse(EyeSelected == "Right", RIGHT_PUPIL_SIZE,
                                 ifelse(EyeSelected == "Left", LEFT_PUPIL_SIZE, NA)),
               Gaze_X = ifelse(EyeSelected == "Right", RIGHT_GAZE_X,
                               ifelse(EyeSelected == "Left", LEFT_GAZE_X, NA)),
               Gaze_Y = ifelse(EyeSelected == "Right", RIGHT_GAZE_Y,
                               ifelse(EyeSelected == "Left", LEFT_GAZE_Y, NA)),
               Velocity_X = ifelse(EyeSelected == "Right", RIGHT_VELOCITY_X,
                                   ifelse(EyeSelected == "Left", LEFT_VELOCITY_X, NA)),
               Velocity_Y = ifelse(EyeSelected == "Right", RIGHT_VELOCITY_Y,
                                   ifelse(EyeSelected == "Left", LEFT_VELOCITY_Y, NA)),
               Acceleration_X = ifelse(EyeSelected == "Right", RIGHT_ACCELERATION_X,
                                ifelse(EyeSelected == "Left", LEFT_ACCELERATION_X, NA)),
               Acceleration_Y = ifelse(EyeSelected == "Right", RIGHT_ACCELERATION_Y,
                                ifelse(EyeSelected == "Left", LEFT_ACCELERATION_Y, NA)),
               In_Blink = ifelse(EyeSelected == "Right", RIGHT_IN_BLINK,
                                 ifelse(EyeSelected == "Left", LEFT_IN_BLINK, NA)),
               In_Saccade = ifelse(EyeSelected == "Right", RIGHT_IN_SACCADE,
                                   ifelse(EyeSelected == "Left", LEFT_IN_SACCADE, NA))
        )
    tmp$Pupil <- as.numeric(as.character(tmp$Pupil))
    tmp$Gaze_X <- as.numeric(as.character(tmp$Gaze_X))
    tmp$Gaze_Y <- as.numeric(as.character(tmp$Gaze_Y))
    tmp$Velocity_X <- as.numeric(as.character(tmp$Velocity_X))
    tmp$Velocity_Y <- as.numeric(as.character(tmp$Velocity_Y))
    tmp$Acceleration_X <- as.numeric(as.character(tmp$Acceleration_X))
    tmp$Acceleration_Y <- as.numeric(as.character(tmp$Acceleration_Y))
    tmp$In_Blink <- as.numeric(as.character(tmp$In_Blink))
    tmp$In_Saccade <- as.numeric(as.character(tmp$In_Saccade))
    tmp <- tmp %>%
      mutate(Pupil_Data = ifelse(sum(Pupil, na.rm = TRUE) > 0, "Contains_Pupil_Data", "No_Pupil_Data"))
    tmp$Pupil_Data <- as.factor(as.character(tmp$Pupil_Data))

  tmp$EyeRecorded <- as.factor(as.character(tmp$EyeRecorded))
  tmp$EyeSelected <- as.factor(as.character(tmp$EyeSelected))

  message(paste("Gaze data summary for", length(unique(levels(tmp$Event))), "events:"))

  if (!(is.na(WhenLandR))) {
      n <- tmp %>% group_by(Event) %>% summarise(T1 = min(Time), Eye = EyeSelected[1]) %>% filter(Eye=="Both") %>% droplevels()
    message(paste(nrow(n), "event(s) contained gaze data for both eyes, for which the", WhenLandR, "eye has been selected." ))
  }

  if (("EYE_TRACKED" %in% names(tmp)) | Recording == "LandR" | Recording == "LorR" | Recording == "R" ) {
    n <- tmp %>% group_by(Event) %>% summarise(T1 = min(Time), Eye = EyeSelected[1]) %>% filter(Eye=="Right") %>% droplevels()
    message(paste("The final data frame contains", nrow(n), "event(s) using gaze data from the right eye."))
  }

  if (("EYE_TRACKED" %in% names(tmp)) | Recording == "LandR" | Recording == "LorR" | Recording == "L" ) {
    n <- tmp %>% group_by(Event) %>% summarise(T1 = min(Time), Eye = EyeSelected[1]) %>% filter(Eye=="Left") %>% droplevels()
    message(paste("The final data frame contains", nrow(n), "event(s) using gaze data from the left eye."))
  }

  n <- tmp %>% group_by(Event) %>% summarise(T1 = min(Time), Data = Pupil_Data[1]) %>% filter(Data=="No_Pupil_Looks") %>% droplevels()
  message(paste("The final data frame contains", nrow(n), "event(s) with no samples containing pupil size data during the given time series."))

  return(droplevels(ungroup(tmp)))
}


#' Check for samples off-screen and marks as NA.
#'
#' \code{recode_off_screen} checks samples falling outside the bounds of
#' the screen and marks them with NA
#'
#' @export
#' @import dplyr
#' @import rlang
#' @import ggplot2
#'
#' @param data A data frame object created from an Eyelink Sample Report.
#' @param ScreenSize A numeric vector specifying (in pixels) the dimensions
#' of the x and y of the screen used during the experiment.
#' @param PlotData A logical indicating whether or not to output a
#' visualization of the result.
#' @return An object of type data table as described in \link[tibble]{tibble}.
#' @examples
#' # Load example data
#' data("Pupilex3")
#'
#' dat <- recode_off_screen(data = Pupilex3, ScreenSize = c(1920, 1080),
#'                          PlotData = FALSE)
#'
#' # Please see the vignettes for detailed example usage.
#' # vignette("PupilPre_Basic_Preprocessing", package="PupilPre")
#'
recode_off_screen <- function(data = data, ScreenSize = NULL, PlotData = FALSE){

  if(!("Gaze_X" %in% names(data))) {
    stop("Please select recording eye using `ppl_select_recorded_eye` before marking off-screen data.")
  }

  if(is.null(ScreenSize)) {
    stop("Please input screen size in pixels in the format: c(x, y).")
  }

  # Overwrite Pupil_Previous and create columns
  data <- data %>% arrange(Event, Time) %>%
    mutate(Pupil_Previous = Pupil,
           Gaze_X_Previous = Gaze_X,
           Gaze_Y_Previous = Gaze_Y)

  # Mark data points
  message(paste0("Marking data points outside of ", ScreenSize[1], "x", ScreenSize[2], "."))
  tmp <- data %>% group_by(Event) %>%
    do(
    mutate(., Screen = case_when(
      is.na(Gaze_X) | is.na(Gaze_Y) ~ "Unknown",
      (Gaze_X < 0) | (Gaze_X > ScreenSize[1]) ~ "OffScreen",
      (Gaze_Y < 0) | (Gaze_Y > ScreenSize[2]) ~ "OffScreen",
      TRUE ~ "OnScreen"
    ))
    )

  message(paste("\nRecoding pupil size data."))
  tmp <- tmp %>%
    do(
    mutate(., Pupil = ifelse(Screen == "OffScreen", NA, Pupil_Previous),
           Gaze_X = ifelse(Screen == "OffScreen", NA, Gaze_X_Previous),
           Gaze_Y = ifelse(Screen == "OffScreen", NA, Gaze_Y_Previous))
    )

  message(paste0("\n",round((nrow(tmp[tmp$Screen=="OffScreen",])/nrow(tmp))*100, 2), "% of data marked as off-screen"))

  if(PlotData==TRUE) {
    message(paste("Plotting recoded data."))
    plt <- ggplot(tmp, aes(x = Gaze_X, y = Gaze_Y)) +
      stat_bin2d(aes(fill = stat(density)), binwidth = c(1,1), na.rm = TRUE) +
      scale_y_reverse() +
      .pac_theme() +
      labs(x = "Gaze X", y = "Gaze Y", fill = "Density", title = paste0("Set for screen size: ", ScreenSize[1], "x", ScreenSize[2])) +
      coord_cartesian(xlim = c(0, ScreenSize[1], ylim = c(0, ScreenSize[2])))
    print(plt)
  }

  return(droplevels(ungroup(tmp)))
}


#' Automatically clean Eyelink marked blinks.
#'
#' \code{clean_blink} performs two stage automated clean-up of blinks
#' in the pupil and gaze coordinate data.
#'
#' @export
#' @import dplyr
#' @import rlang
#' @importFrom stats quantile
#'
#' @param data A data frame object created from \code{ppl_select_recorded_eye}.
#' @param BlinkPadding A numeric vector of length two containing values (in
#' msec) to pad the marked blink creating a window within which to operate
#' the cleanup.
#' @param Delta A numeric value specifying the maximal difference between
#' subsequent pupil values in order to mark greater differences for removal. If
#' NA, the delta will be estimated from the data using the 95th percentile
#' value.
#' @param MaxValueRun A numeric value specifying the maximal run of existing
#' values flanked by NAs that could be targeted for removal.
#' @param NAsAroundRun A numeric vector of length two containing values (in
#' number of subsequent NA) to be used to identify straggler runs of data
#' that could be removed.
#' @param LogFile A character string indicating the file name (with extension)
#' of the log file to be created/written. The file keeps track of which events
#' have been cleaned. We suggest "BlinkCleanupLog.rds".
#' @return An object of type data table as described in \link[tibble]{tibble}.
#' @examples
#' # Load example data
#' data("Pupilex3")
#'
#' # Writing log file to temporary folder for the example
#' dat <- clean_blink(Pupilex3, BlinkPadding = c(100, 100), Delta = 5,
#'                    MaxValueRun = 5, NAsAroundRun = c(2,2),
#'                    LogFile = paste0(tempdir(),"/BlinkCleanupLog.rds"))
#'
#' # Please see the vignettes for detailed example usage.
#' # vignette("PupilPre_Cleanup", package="PupilPre")
#'
clean_blink <- function(data=data, BlinkPadding = c(100, 100), Delta = NA,
                        MaxValueRun = 5, NAsAroundRun = c(2,2),
                        LogFile = NULL){

  # Overwrite Pupil_Previous and create new columns
  data <- data %>% arrange(Event, Time) %>% mutate(Pupil_Previous = Pupil,
                                                   Gaze_X_Previous = Gaze_X,
                                                   Gaze_Y_Previous = Gaze_Y,
                                                   Delta = 0,
                                                   CleanedPupil = 0,
                                                   InBlinkValue = FALSE)

  # Check for file and path
  if(is.null(LogFile)){
    stop("Please provide the name of the log file to be written including file extension.
    This MUST be a .rds file. A path may also be provided if you wish to have the file placed somewhere other than your working directory.
    We suggest \"BlinkCleanupLog.rds\".")
  }
  if(tolower(tools::file_ext(LogFile)) != "rds"){
    stop("The file extension you have provided is not valid.
    You MUST specify a .rds file. We suggest \"BlinkCleanupLog.rds\".")
  }
  if(!dir.exists(dirname(LogFile))){
    stop("The file path you have provided does not exist.
    Please specify a valid path or remove path to save the log file into your working directory.")
  }

  # Estimate delta from the data
  if(is.na(Delta)){
    deltmp <- data %>% select(Pupil) %>% mutate(Delta = abs(Pupil-lag(Pupil)))
    Delta <- stats::quantile(deltmp$Delta, probs = seq(0, 1, 0.05), na.rm = TRUE)[20]
    message(paste0("Delta set to ", Delta, " based on 95th percentile."))
  }

  # Blink fix function to run on each event
  .fixblink <- function(data=data, BlinkPadding = BlinkPadding, Delta = Delta,
                        MaxValueRun = MaxValueRun, NAsAroundRun = NAsAroundRun){

    if(sum(data$In_Blink) > 0) {

      blnk_artif <- rle(data$In_Blink) %>%
        unclass() %>%
        as.data.frame() %>%
        mutate(end = cumsum(lengths),
               start = c(1, lag(end)[-1] + 1)) %>%
        select(lengths, values, start, end) %>%
        filter(values == 1)

      # Recode samples as Time
      for(i in 1:nrow(blnk_artif)){
        blnk_artif[i, "start"] <- data$Time[blnk_artif[i, "start"]]
        blnk_artif[i, "end"] <- data$Time[blnk_artif[i, "end"]]
      }

      # Make buffer to time points, check boundaries, and buffer the blink
      BlinkPadding1 <- BlinkPadding[1]
      BlinkPadding2 <- BlinkPadding[2]

      for(i in 1:nrow(blnk_artif)){

        blnk_artif[i, "start"] <- blnk_artif[i, "start"]-BlinkPadding1
        blnk_artif[i, "end"] <- blnk_artif[i, "end"]+BlinkPadding2

        if(blnk_artif[i, "start"] < min(data$Time)) {
          blnk_artif[i, "start"] <- min(data$Time)
        }

        if(blnk_artif[i, "end"] > max(data$Time)) {
          blnk_artif[i, "end"] <- max(data$Time)
        }

        data$In_Blink[which(data$Time >= blnk_artif[i, "start"] & data$Time <= blnk_artif[i, "end"])] <- 1

      }

      # Calc diffs
      data <- data %>% mutate(Delta = Pupil-lag(Pupil))

      # Remove large diffs in blink
      d <- Delta
      data <- data %>% mutate(CleanedPupil = ifelse(In_Blink == TRUE & abs(Delta) >= d, NA, Pupil))

      # Mark values in the blink
      data <- data %>% mutate(InBlinkValue = ifelse(In_Blink == TRUE & is.na(CleanedPupil), FALSE, TRUE))

      # Identify stragglers in the blink windows
      MaxValueRun <- MaxValueRun
      NAsBefore <- NAsAroundRun[1]
      NAsAfter <- NAsAroundRun[2]

      for(i in 1:nrow(blnk_artif)){

        tmp <- data %>% filter(Time >=  blnk_artif[i, "start"], Time <=  blnk_artif[i, "end"])
        blnk_stragler <- rle(tmp$InBlinkValue) %>%
          unclass() %>%
          as.data.frame() %>%
          mutate(end = cumsum(lengths),
                 start = c(1, lag(end)[-1] + 1)) %>%
          select(lengths, values, start, end)

        for(j in 1:(nrow(blnk_stragler))){
          if(! j %in% c(1, nrow(blnk_stragler))) {
            if(blnk_stragler$values[j] == TRUE && blnk_stragler$lengths[j] <= MaxValueRun){  # Run of datapoints in a row
              # Check surroundings
              lengthbefore <- blnk_stragler$length[j-1]
              lengthafter <- blnk_stragler$length[j+1]
              if(lengthbefore >=NAsBefore & lengthafter >=NAsAfter){  # NAs on either side
                data$CleanedPupil[which(data$Time >= blnk_artif[i, "start"] & data$Time <= blnk_artif[i, "end"])][blnk_stragler[j,]$start:blnk_stragler[j,]$end] <- NA
              }
            }
          }
        }

      }

    } else{
      data$CleanedPupil <- data$Pupil
    }

    return(data)
  }


  # Run the blink fix function on each event
  message("Running clean-up based on Eyelink marked blinks.")
  data <- data %>% group_by(Event) %>% do(
    .fixblink(data=., BlinkPadding = BlinkPadding, Delta = Delta, MaxValueRun = MaxValueRun, NAsAroundRun = NAsAroundRun)
  )


  # Mark auto-cleaned datapoints
  # Remake pupil column with cleaned data
  # Clean X and Y based on pupil
  # Remove unnecessary columns
  data <- data %>% group_by(Event) %>% mutate(BlinkCleaned = !(.compareNA(Pupil_Previous,
                                                                          CleanedPupil))) %>%
    mutate(Pupil = CleanedPupil) %>% mutate(Gaze_X = ifelse(is.na(Pupil),
                                                            NA, Gaze_X), Gaze_Y = ifelse(is.na(Pupil), NA, Gaze_Y)) %>%
    select(-InBlinkValue, -Delta, -CleanedPupil)


  # Create log
  message(paste0("\nWriting cleanup info to ", LogFile, "."))
  blinks <- vector("list", length = length(unique(data$Event)))
  names(blinks) <- unique(data$Event)
  cleanedevents <- unique(droplevels(data[data$BlinkCleaned == TRUE, ]$Event))
  for(i in unique(data$Event)){
    if(i %in% cleanedevents) {
      blinks[i] <- as.logical(TRUE)
    } else {
      blinks[i] <- as.logical(NA)
    }
  }
  saveRDS(blinks, file = LogFile, compress = TRUE)

  return(droplevels(ungroup(data)))

}


#' Automatically clean artifacts.
#'
#' \code{clean_artifact} performs two stage and distributional automated
#' clean-up of artifacts in the pupil and gaze coordinate data.
#'
#' @export
#' @import dplyr
#' @import rlang
#' @import tidyr
#' @importFrom robustbase covMcd
#' @importFrom stats quantile
#'
#' @param data A data frame object created from \code{ppl_select_recorded_eye}.
#' @param MADWindow A numeric value specifying the window size (in msec) to use
#' for the MAD calculation.
#' @param MADConstant A numeric value specifying the constant (a multiplier for
#' the third quartile) when determining MAD outlier status.
#' @param MADPadding A numeric vector of length two containing values (in
#' msec) to pad the identified artifact creating a window within which to
#' operate the cleanup.
#' @param MahaConstant A numeric value specifying the constant (a multiplier
#' for the third quartile) when determining Mahalanobis outlier status.
#' @param Method A character string ("Basic" or "Robust") indicating
#' which method to use for the distance calculation. Basic is a standard
#' Mahalanobis distance calculation based on covariance. Robust is also a
#' Mahalanobis distance, however, it is based on Minimum Covariance
#' Determinant (Rousseeuw and van Driessen, 1999) with reweighted covariance
#' (Pison et al., 2002). For more details, see \code{\link[robustbase]{covMcd}}.
#' @param XandY A logical value specifying whether to also use horizontal
#' velocity and acceleration in outlier detection.
#' @param Second A logical value specifying whether secondary cleaning should
#' be applied.
#' @param MaxValueRun A numeric value specifying the maximal run of existing
#' values flanked by NAs that could be targeted for removal.
#' @param NAsAroundRun A numeric vector of length two containing values (in
#' number of subsequent NA) to be used to identify straggler runs of data
#' that could be removed.
#' @param LogFile A character string indicating the file name (with extension)
#' of the log file to be created/written. The file keeps track of which events
#' have been cleaned. We suggest "ArtifactCleanupLog.rds".
#' @return An object of type data table as described in \link[tibble]{tibble}.
#' @references Rousseeuw, P. J. and van Driessen, K. (1999) A fast algorithm
#' for the minimum covariance determinant estimator. Technometrics 41, 212–223.
#' @references Pison, G., Van Aelst, S., and Willems, G. (2002) Small Sample
#' Corrections for LTS and MCD, Metrika 55, 111–123.
#' @examples
#' # Load example data
#' data("Pupilex4")
#'
#' # Writing log file to temporary folder for the example
#' dat <- clean_artifact(Pupilex4, MADWindow = 100, MADConstant = 2,
#'                       MADPadding = c(200, 200), MahaConstant = 2,
#'                       Method = "Robust", XandY = TRUE, Second = TRUE,
#'                       MaxValueRun = 5, NAsAroundRun = c(2,2),
#'                       LogFile = paste0(tempdir(),"/ArtifactCleanupLog.rds"))
#'
#' # Please see the vignettes for detailed example usage.
#' # vignette("PupilPre_Cleanup", package="PupilPre")
#'
clean_artifact <- function(data=data, MADWindow = 100, MADConstant = 2,
                           MADPadding = c(200, 200), MahaConstant = 2,
                           Method = "Robust", XandY = TRUE, Second = TRUE,
                           MaxValueRun = 5, NAsAroundRun = c(2,2),
                           LogFile = NULL){

  if(Method =="Robust"){
    # Check if robustbase is available
    if("robustbase" %in% (.packages())){
    } else{
      if(nchar(system.file(package = "robustbase")) == 0){
        stop("Please install the package 'robustbase' for distributional cleanup")
      }
    }
  }

  # Check for file and path
  if(is.null(LogFile)){
    stop("Please provide the name of the log file to be written including file extension.
    This MUST be a .rds file. A path may also be provided if you wish to have the file placed somewhere other than your working directory.
    We suggest \"ArtifactCleanupLog.rds\".")
  }
  if(tolower(tools::file_ext(LogFile)) != "rds"){
    stop("The file extension you have provided is not valid.
    You MUST specify a .rds file. We suggest \"ArtifactCleanupLog.rds\".")
  }
  if(!dir.exists(dirname(LogFile))){
    stop("The file path you have provided does not exist.
    Please specify a valid path or remove path to save the log file into your working directory.")
  }

  if(XandY==TRUE){
    MahaCols <- c("Time", "Pupil", "Velocity_Y", "Acceleration_Y", "Velocity_X", "Acceleration_X")
  } else {
    MahaCols <- c("Time", "Pupil", "Velocity_Y", "Acceleration_Y")
  }

  # Overwrite Pupil_Previous and create columns
  data <- data %>% arrange(Event, Time) %>% mutate(Pupil_Previous = Pupil,
                                                   Gaze_X_Previous = Gaze_X,
                                                   Gaze_Y_Previous = Gaze_Y,
                                                   CleanedPupil = 0,
                                                   InArtifValue = FALSE,
                                                   MahaOutlier = 0)

  # Artifact fix function to run on each event
  .fixartif <- function(data=data, MADWindow = MADWindow, MADConstant = MADConstant,
                        MADPadding = MADPadding, MahaConstant = MahaConstant, MahaCols = MahaCols,
                        Second = Second, MaxValueRun = MaxValueRun, NAsAroundRun = NAsAroundRun,
                        Method = Method){

    # Create windows for MAD calc
    start <- seq(min(data$Time), max(data$Time), by=MADWindow)
    end <- c(start[2:(length(start))], max(data$Time))
    wins <- as.data.frame(cbind(start, end))
    wins$MAD <- 0

    # Calculate MAD
    for(i in 1:nrow(wins)){
      wins[i, "MAD"] <- stats::mad(data[data$Time >= wins[i, "start"] & data$Time <= wins[i, "end"],]$Pupil, na.rm = TRUE)
    }

    # Set cleaned pupil
    data$CleanedPupil <- data$Pupil

    # Find windows with extreme MAD
    #    artwindows <- wins[!is.na(wins$MAD) & wins$MAD >= (MADConstant*stats::sd(wins$MAD, na.rm = TRUE)),]
    artwindows <- wins[!is.na(wins$MAD) & wins$MAD >= (MADConstant*as.numeric(stats::quantile(wins$MAD, na.rm = TRUE)[4])),]

    # Place buffer around extreme windows
    Buf1 <- MADPadding[1]
    Buf2 <- MADPadding[2]

    # Pad time in extreme MAD windows
    # Only do this if there are any windows
    if(nrow(artwindows)==0){
    } else {
      for(g in 1:nrow(artwindows)) {
        artwindows[g, "start"] <- artwindows[g, "start"]-Buf1
        artwindows[g, "end"] <- artwindows[g, "end"]+Buf2
      }
    }

    # Find outliers withing the padded windows
    # Only do this if there are any windows
    if(nrow(artwindows)==0){
    } else{
      for(i in 1:nrow(artwindows)){

        tmpall <- data[which(data$Time >= artwindows[i, "start"] & data$Time <= artwindows[i, "end"]), MahaCols]

        # Determine if there are any 0 columns and remove them
        tmpna <- tidyr::drop_na(tmpall)
        tmpmaha <- tmpna %>% filter_at(vars(contains("_")), all_vars(. != 0))
        Time <- tmpmaha$Time
        tmpmaha <- tmpmaha %>% select(-Time)
        if(nrow(tmpmaha)<=5) {
          cat(paste("\nSkipping event", unique(data$Event), "for window:", min(tmpall$Time), "to", max(tmpall$Time)))
          tmpall$outlier_maha <- 0
        }
        if ( nrow(tmpmaha) > 5) {

          if(Method == "Robust"){
            Stmpmaha <- robustbase::covMcd(tmpmaha)
            mahaoutput <- stats::mahalanobis(tmpmaha, Stmpmaha$center, Stmpmaha$cov)
          } else if (Method == "Basic"){
            Stmpmaha <- stats::cov(tmpmaha, use = "na.or.complete")
            mahaoutput <- stats::mahalanobis(tmpmaha, colMeans(tmpmaha, na.rm = TRUE), Stmpmaha)
          }

          mahadata <- cbind(Time, mahaoutput)
          tmpall <- left_join(tmpall, as.data.frame(mahadata), by = "Time")
          tmpall$maha_dist <- round(tmpall$mahaoutput, 4)
          tmpall$outlier_maha <- 0
          tmpall$outlier_maha[tmpall$maha_dist > MahaConstant * as.numeric(stats::quantile(tmpall$maha_dist, na.rm = TRUE)[4])] <- 1
          tmpall <- as.data.frame(tmpall)
        }

        data$MahaOutlier[which(data$Time >= artwindows[i, "start"] & data$Time <= artwindows[i, "end"])] <- tmpall$outlier_maha
        # In case there are no outliers
        if(sum(data$MahaOutlier)>0){
          data[data$MahaOutlier==1,]$CleanedPupil <- NA
        }

      }
    }

    if(Second==TRUE){

      # Mark values in the blink
      data <- data %>% mutate(InArtifValue = ifelse(is.na(CleanedPupil), FALSE, TRUE))

      # Identify stragglers in the blink windows
      MaxValueRun <- MaxValueRun
      NAsBefore <- NAsAroundRun[1]
      NAsAfter <- NAsAroundRun[2]

      if(nrow(artwindows)==0){
      } else{
        for(i in 1:nrow(artwindows)){

          tmp <- data %>% filter(Time >=  artwindows[i, "start"], Time <=  artwindows[i, "end"])
          artif_stragler <- rle(tmp$InArtifValue) %>%
            unclass() %>%
            as.data.frame() %>%
            mutate(end = cumsum(lengths),
                   start = c(1, lag(end)[-1] + 1)) %>%
            select(lengths, values, start, end)

          for(j in 1:(nrow(artif_stragler))){
            if(! j %in% c(1, nrow(artif_stragler))) {
              if(artif_stragler$values[j] == TRUE && artif_stragler$lengths[j] <= MaxValueRun){  # Run of datapoints in a row
                # Check surroundings
                lengthbefore <- artif_stragler$length[j-1]
                lengthafter <- artif_stragler$length[j+1]
                if(lengthbefore >=NAsBefore & lengthafter >=NAsAfter){  # NAs on either side
                  data$CleanedPupil[which(data$Time >= artwindows[i, "start"] & data$Time <= artwindows[i, "end"])][artif_stragler[j,]$start:artif_stragler[j,]$end] <- NA
                }
              }
            }
          }

        }
      }
    }

    return(data)
  }

  # Run artifact clean-up on events if requested
  message(paste0("\nRunning cleanup based on MAD (median absolute deviation) and ", tolower(Method) ," Mahalanobis distance."))
  data <- data %>% group_by(Event) %>% do(
    .fixartif(data=., MADWindow = MADWindow, MADConstant = MADConstant, MADPadding = MADPadding, MahaConstant = MahaConstant, MahaCols = MahaCols, Second = Second, MaxValueRun = MaxValueRun, NAsAroundRun = NAsAroundRun, Method = Method)
  )


  # Mark auto-cleaned datapoints
  # Remake pupil column with cleaned data
  # Clean X and Y based on pupil
  # Remove unnecessary columns
  data <- data %>% group_by(Event) %>% mutate(ArtifactCleaned = !(.compareNA(Pupil_Previous,
                                                                             CleanedPupil))) %>%
    mutate(Pupil = CleanedPupil) %>%
    mutate(Gaze_X = ifelse(is.na(Pupil),
                           NA, Gaze_X), Gaze_Y = ifelse(is.na(Pupil), NA, Gaze_Y)) %>%
    select(-MahaOutlier, -InArtifValue, -CleanedPupil)

  # Create log
  message(paste0("\nWriting cleanup info to ", LogFile, "."))
  arts <- vector("list", length = length(unique(data$Event)))
  names(arts) <- unique(data$Event)
  cleanedevents <- unique(droplevels(data[data$ArtifactCleaned == TRUE, ]$Event))
  for(i in unique(data$Event)){
    if(i %in% cleanedevents) {
      arts[i] <- as.logical(TRUE)
    } else {
      arts[i] <- as.logical(NA)
    }
  }
  saveRDS(arts, file = LogFile, compress = TRUE)

  return(droplevels(ungroup(data)))

}




#' Applies user-selected changes to auto cleanup
#'
#' \code{apply_cleanup_change} applies to each event the user-selected
#' changes to the automatic cleanup based on information stored in the RDS file
#' using \code{verify_cleanup_app} which was created using either
#' \code{blink_cleanup} or \code{artifact_cleanup}.
#'
#' @export
#' @import dplyr
#' @import rlang
#'
#' @param data A data frame object created from \code{downsample}.
#' @param LogFile A character string indicating the name (and location) of the
#' log file.
#' @return An object of type data table as described in \link[tibble]{tibble}.
#' @examples
#' if (interactive()) {
#'
#' # Load example data
#' data("Pupilex3")
#'
#' # Ensure the log file exists by running cleanup
#' # Writing log file to temporary folder for the example
#' dat <- clean_blink(Pupilex3, BlinkPadding = c(100, 100), Delta = 5,
#'                    MaxValueRun = 5, NAsAroundRun = c(2,2),
#'                    LogFile = paste0(tempdir(),"/BlinkCleanupLog.rds"))
#'
#' # Read log file from temporary folder
#' verify_cleanup_app(dat, LogFile = paste0(tempdir(),"/BlinkCleanupLog.rds"))
#'
#' # Make verification via the app interface
#'
#' # Read log file from the temporary folder
#' dat <- apply_user_cleanup(dat,
#'                           LogFile = paste0(tempdir(),"/BlinkCleanupLog.rds"))
#' }
#'
#' # Please see the vignettes for detailed example usage.
#' # vignette("PupilPre_Cleanup", package="PupilPre")
#'
#'
apply_cleanup_change <- function(data = data, LogFile = NULL){

  # Check for file and path
  if(is.null(LogFile)){
    stop("Please provide the name of the log file including file extension.
    Suggested names are 'BlinkCleanupLog.rds' and 'ArtifactCleanupLog.rds'.
    Please also provide the path if you have saved the file outside of your working directory.")
  }
  if(tolower(tools::file_ext(LogFile)) != "rds"){
    stop("The file extension you have provided is not valid.
    You MUST specify a .rds file.")
  }
  if(!dir.exists(dirname(LogFile))){
    stop("The file path you have provided does not exist.
    Please specify a valid path or remove path to read the file from your working directory.")
  }

  if(file.exists(LogFile)) {
    message(paste("Loading file", LogFile))
    blinks <- readRDS(file = LogFile)
  } else {
    stop(paste0("Cannot find ", LogFile))
  }

  message(paste("Changing cleanup based on ", LogFile))

  data$PupilClean <- NA

  revert <- unique(names(blinks[blinks==FALSE]))
  # message(revert)

  tmp <- data %>% group_by(Event) %>% do(
    mutate(., PupilRevert = ifelse(.$Event %in% revert, .$Pupil_Previous, .$Pupil),
           XRevert = ifelse(.$Event %in% revert, .$Gaze_X_Previous, .$Gaze_X),
           YRevert = ifelse(.$Event %in% revert, .$Gaze_Y_Previous, .$Gaze_Y))
  )

  # Remake pupil column with reverted data
  tmp <- tmp %>% group_by(Event) %>%
    mutate(Pupil = PupilRevert,
           Gaze_X = XRevert,
           Gaze_Y = YRevert) %>%
    select(-PupilRevert, -XRevert, -YRevert)

  return(droplevels(ungroup(tmp)))

}



#' Applies manual cleanup to the data
#'
#' \code{apply_user_cleanup} applies to each event the manual cleanup based on
#' data points stored in the RDS file created using \code{user_cleanup_app}. The
#' Identified datapoints will be changed to NA.
#'
#' @export
#' @import dplyr
#' @import rlang
#'
#' @param data A data frame object created from \code{downsample}.
#' @param LogFile A character string indicating the name (and location) of the
#' log file.
#' @return An object of type data table as described in \link[tibble]{tibble}.
#' @examples
#' if (interactive()) {
#'
#' # Load example data
#' data("Pupilex4")
#'
#' # Ensure log file exists by using the cleanup app
#' # Writing log file to temporary folder for the example
#' user_cleanup_app(Pupilex4, LogFile = paste0(tempdir(),"/UserCleanupLog.rds"))
#'
#' # Make cleanup via the app interface
#'
#' # Read log file from the temporary folder
#' dat <- apply_user_cleanup(Pupilex4,
#'                           LogFile = paste0(tempdir(),"/UserCleanupLog.rds"))
#' }
#'
#' # Please see the vignettes for detailed example usage.
#' # vignette("PupilPre_Cleanup", package="PupilPre")
#'
apply_user_cleanup <- function(data = data, LogFile = NULL){

  # Check for file and path
  if(is.null(LogFile)){
    stop("Please provide the name of the log file including file extension.
    Suggested name is 'UserCleanupLog.rds'.
    Please also provide the path if you have saved the file outside of your working directory.")
  }
  if(tolower(tools::file_ext(LogFile)) != "rds"){
    stop("The file extension you have provided is not valid.
    You MUST specify a .rds file.")
  }
  if(!dir.exists(dirname(LogFile))){
    stop("The file path you have provided does not exist.
    Please specify a valid path or remove path to read the file from your working directory.")
  }

  if(file.exists(LogFile)) {
    message(paste("Loading file", LogFile))
    blinks <- readRDS(file = LogFile)
  } else {
    stop(paste0("Cannot find ", LogFile))
  }

  message(paste("Applying cleanup based on ", LogFile))

  data$PupilClean <- NA

  tmp <- data %>% group_by(Event) %>% do(
    mutate(., PupilClean = ifelse(.$Time %in% blinks[[.$Event[1]]], NA, .$Pupil))
  )

  # Remake previous column
  tmp <- tmp %>% group_by(Event) %>% mutate(Pupil_Previous = Pupil,
                                            Gaze_X_Previous = Gaze_X,
                                            Gaze_Y_Previous = Gaze_Y)
  # Remake pupil column with cleaned data
  tmp <- tmp %>% group_by(Event) %>% mutate(Pupil = PupilClean) %>% select(-PupilClean)

  # Clean X and Y based on pupil
  tmp <- tmp %>% group_by(Event) %>% mutate(Gaze_X = ifelse(is.na(Pupil), NA, Gaze_X),
                                            Gaze_Y = ifelse(is.na(Pupil), NA, Gaze_Y))

  return(droplevels(ungroup(tmp)))


}



#' Removes events with excessive missing data
#'
#' \code{rm_sparse_events} removes events with less data than the
#' specified amount.
#'
#' @export
#' @import dplyr
#' @import rlang
#'
#' @param data A data table object output after having run
#' \code{\link{ppl_select_recorded_eye}}.
#' @param BaselineWindow A numeric vector of length 1 or 2 specifying the time
#' points of the baseline window to be examined. Providing two values indicates
#' the start time and the end time of the baseline, respectively. Providing a
#' single value (i.e., time point) assumes that every preceding time point is
#' part of the baseline (N.B. trials may vary in the size of the baseline
#' window and will result in an error).
#' @param CriticalWindow A numeric vector of length 1 or 2 specifying the time
#' points of the critical (i.e., post-stimulus) window to be examined. Providing
#' two values indicates the start time and the end time of the window,
#' respectively. Providing a single value (i.e., time point) assumes that
#' every subsequent time point is part of the window.
#' @param BaselineRequired A number indicating the percentage of data
#' required in the baseline to be included (i.e., drop events with less than
#' this amount of data).
#' @param CriticalRequired A number indicating the percentage of data
#' required in the critical window to be included (i.e., drop events with less
#' than this amount of data).
#' @return An object of type data table as described in \link[tibble]{tibble}.
#' @examples
#' # Load example data
#' data("Pupilex3")
#'
#' dat <- rm_sparse_events(data = Pupilex3, BaselineWindow = c(-500, 0),
#'                         CriticalWindow = c(200, 2000),
#'                         BaselineRequired = 50,
#'                         CriticalRequired = 50)
#'
#' # Please see the vignettes for detailed example usage.
#' # vignette("PupilPre_Basic_Preprocessing", package="PupilPre")
#'
rm_sparse_events <- function(data = data, BaselineWindow = NULL, CriticalWindow = NULL,
                        BaselineRequired = NULL, CriticalRequired = NULL) {

  # Call internally check_baseline
  basecheck <- suppressMessages(
    check_baseline(data = data, BaselineWindow = BaselineWindow, ReturnData = TRUE)
    )
  basecheck <- basecheck %>% select(Event, BaselinePercentNA = PercentNA)

  # Call internally NA_summary for event on specified window (filtered)
  if(length(CriticalWindow)==1) {
    tmp <- data %>% filter(Time >= CriticalWindow)
  } else {
    tmp <- data %>% filter(Time >= CriticalWindow[1], Time <= CriticalWindow[2])
  }
  critcheck <- suppressMessages(
    NA_summary(data = tmp, Summary = "Event", PupilColumn = "Pupil", ReturnData = TRUE)
  )
  critcheck <- critcheck %>% select(Event, CrititalPercentNA = PercentNA)

  # Merge in data and do conditional that both are met
  checkinfo <- merge(basecheck, critcheck, by = "Event")

  pre <- length(unique(checkinfo$Event))

  checkinfo <- checkinfo %>% group_by(Event) %>%
    mutate(BaselineQuality = 100 - BaselinePercentNA,
           CriticalQuality = 100 - CrititalPercentNA) %>%
    filter(BaselineQuality >= BaselineRequired, CriticalQuality >= CriticalRequired) %>%
    droplevels()

  post <- length(unique(checkinfo$Event))

  message(paste0("Removing ", pre-post, " events."))

  # Drop events
  data <- data %>% filter(Event %in% unique(levels(checkinfo$Event)))

  # return
  return(droplevels(ungroup(data)))

}


#' Interpolation for missing data.
#'
#' \code{interpolate_NAs} performs interpolation of missing data for
#' the pupil and gaze coordinates (if desired).
#'
#' @export
#' @import dplyr
#' @import rlang
#' @importFrom zoo na.approx
#' @importFrom zoo na.spline
#'
#' @param data A data frame object created from \code{auto_cleanup}.
#' @param Method A character string indicating type of interpolation ("linear"
#' or "spline") as implemented in \link[zoo]{na.approx}.
#' @param XandY A logical specifying if interpolation should also be done on
#' gaze coordinates
#' @param MinData A number indicating the minimum number of data points
#' required in order for interpolation to be applied to the event.
#' @return An object of type data table as described in \link[tibble]{tibble}.
#' @examples
#' # Load example data
#' data("Pupilex4")
#'
#' dat <- interpolate_NAs(Pupilex4, Method = "linear",
#'                        XandY = TRUE, MinData = 2)
#'
#' # Please see the vignettes for detailed example usage.
#' # vignette("PupilPre_Interpolation_and_Filtering", package="PupilPre")
#'
interpolate_NAs <- function(data = data, Method = "linear", XandY = TRUE, MinData = 2) {

  # Check if zoo is available
  if("zoo" %in% (.packages())){
  } else{
    if(nchar(system.file(package = "zoo")) == 0){
      stop("Please install the package 'zoo' for interpolation.")
    }
  }

  # Overwrite Pupil_Previous
  data <- data %>% mutate(Pupil_Previous = Pupil)

  # Check for at least two data points per event

  message(paste("Checking for minimum required data:", MinData, "data points per event"))

  data <- data %>% group_by(Event) %>%
    mutate(InterpPupilSkip = ifelse(sum(!is.na(Pupil)) < MinData, TRUE, FALSE))

  if(Method=="linear"){

    message(paste("Applying linear interpolation to pupil size data"))
    tmp <- data %>%
      group_by(Event) %>%
      mutate(Pupil = ifelse(InterpPupilSkip==TRUE, Pupil, zoo::na.approx(Pupil_Previous, na.rm = FALSE, rule = 2)))

    if(XandY == TRUE) {
      message(paste("Applying linear interpolation to gaze coordinates"))
      tmp <- tmp %>% ungroup() %>%
        mutate(Gaze_X_Previous = Gaze_X, Gaze_Y_Previous = Gaze_Y) %>%
        group_by(Event) %>%
        mutate(InterpXYSkip = ifelse(sum(!is.na(Gaze_X)) < MinData || sum(!is.na(Gaze_Y)) < MinData, TRUE, FALSE)) %>%
        mutate(Gaze_X = ifelse(InterpXYSkip==TRUE, Gaze_X, zoo::na.approx(Gaze_X_Previous, na.rm = FALSE, rule = 2)),
               Gaze_Y = ifelse(InterpXYSkip==TRUE, Gaze_Y, zoo::na.approx(Gaze_Y_Previous, na.rm = FALSE, rule = 2)))
    }
  } else if(Method=="spline") {

    message(paste("Applying cubic spline interpolation to pupil size data"))

    tmp <- data %>%
      group_by(Event) %>%
      mutate(Pupil = ifelse(InterpPupilSkip==TRUE, Pupil, zoo::na.spline(Pupil_Previous, na.rm = FALSE, method = "natural")))

    if(XandY == TRUE) {
      message(paste("Applying cubic spline interpolation to gaze coordinates"))
      tmp <- tmp %>% ungroup() %>%
        mutate(Gaze_X_Previous = Gaze_X, Gaze_Y_Previous = Gaze_Y) %>%
        group_by(Event) %>%
        mutate(InterpXYSkip = ifelse(sum(!is.na(Gaze_X)) < MinData || sum(!is.na(Gaze_Y)) < MinData, TRUE, FALSE)) %>%
        mutate(Gaze_X = ifelse(InterpXYSkip==TRUE, Gaze_X, zoo::na.spline(Gaze_X_Previous, na.rm = FALSE, method = "natural")),
               Gaze_Y = ifelse(InterpXYSkip==TRUE, Gaze_Y, zoo::na.spline(Gaze_Y_Previous, na.rm = FALSE, method = "natural")))
    }
  }

  # Print info
  tmp2 <- tmp %>% group_by(Event) %>% slice(1) %>% as.data.frame()
  message(paste0("Pupil summary:\n  ", nrow(tmp2[tmp2$InterpPupilSkip==FALSE,]), " events interpolated\n  ",
                 nrow(tmp2[tmp2$InterpPupilSkip==TRUE,]), " events skipped due to insufficient data"))
  if(XandY == TRUE) {
    message(paste0("Gaze coordinate summary:\n  ", nrow(tmp2[tmp2$InterpXYSkip==FALSE,]), " events interpolated\n  ",
                   nrow(tmp2[tmp2$InterpXYSkip==TRUE,]), " events skipped due to insufficient data"))
  }
  return(droplevels(ungroup(tmp)))
}


#' Applies a Butterworth filter to each event.
#'
#' \code{apply_butter} applies a Butterworth filter to the pupil
#' size data.
#'
#' @export
#' @importFrom signal butter
#' @importFrom signal filtfilt
#' @import dplyr
#'
#' @param data A data table object.
#' @param n A number specifying the filter order (as described in
#' \link[signal]{butter}).
#' @param W The critical frequencies of the filter (as described in
#' \link[signal]{butter}). W must be a scalar for low-pass or high-pass filters.
#' W must be a two-element vector c(low, high) specifying the lower and upper
#' bands for stop-band or band-pass filters. For digital filters, W must be
#' between 0 and 1 where 1 is the Nyquist frequency.
#' @param type The filter type (as described in \link[signal]{butter}), one of
#' "low" for a low-pass filter, "high" for a high-pass filter, "stop" for a
#' stop-band (band-reject) filter, or "pass" for a pass-band filter.
#' @param plane A character string (as described in \link[signal]{butter}), "z"
#' for a digital filter or "s" for an analog filter.
#' @examples
#' # Load example data
#' data("Pupilex5")
#'
#' dat <- apply_butter(Pupilex5, n = 1, W = 0.1,
#'                     type = "low", plane = "z")
#'
#' # Please see the vignettes for detailed example usage.
#' vignette("PupilPre_Interpolation_and_Filtering", package="PupilPre")
#'
apply_butter <- function(data = data, n = NULL, W = NULL, type = NULL, plane = "z"){

  # Check if signal is available
  if("signal" %in% (.packages())){
  } else{
    if(nchar(system.file(package = "signal")) == 0){
      stop("Please install the package 'signal' for Butterworth filtering.")
    }
  }

  if(is.null(n)){
    stop("Please specify n")
  }
  if(is.null(W)){
    stop("Please specify W")
  }
  if(is.null(type)){
    stop("Please specify type")
  }
  if((type %in% c("low", "high") && length(W)==1) ||
    (type %in% c("stop", "pass") && length(W)==2)) {
    b1 <- signal::butter(n = n, W = W, type = type, plane = plane)
  } else {
    stop("For types 'low' and 'high', provide only a single value between 0 and 1.\nFor types 'stop' and 'pass', provide numeric vector with two values between 0 and 1.")
  }

  # Overwrite Pupil_Previous and create columns
  data <- data %>% arrange(Event, Time) %>%
    mutate(Pupil_Previous = Pupil,
           Gaze_X_Previous = Gaze_X,
           Gaze_Y_Previous = Gaze_Y)

  # Check for trials with NAs in Pupil
  message("Applying Butterworth filter to eligible events")
  data <- data %>% group_by(Event) %>%
    mutate(FilterSkip = ifelse(sum(is.na(Pupil)) > 0, TRUE, FALSE)) %>%
    do(
      mutate(., Pupil = ifelse(FilterSkip==TRUE, Pupil, signal::filtfilt(filt = b1, x = Pupil)))
    )

  # Print info
  tmp <- data %>% group_by(Event) %>% slice(1) %>% as.data.frame()
  message(paste0("\n",nrow(tmp[tmp$FilterSkip==FALSE,]), " events filtered\n",
                 nrow(tmp[tmp$FilterSkip==TRUE,]), " events skipped due to the presence of NAs"))

  # Return data
  return(droplevels(ungroup(data)))

}


#' Trim the beginning and end of filtered events.
#'
#' \code{trim_filtered} removes events skipped by the filter as well as a
#' specified number of milliseconds from the beginning and the end of each
#' filtered event (as to remove artifacts created by the filter).
#'
#' @export
#' @import dplyr
#' @import rlang
#'
#' @param data A data frame object created from \code{auto_cleanup}.
#' @param RmSkipped A logical value indicating whether or not to remove events
#' that were skipped during the filtering process (due to NAs).
#' @param RmEdges A numeric vector of length 2 indicating the number of
#' milliseconds to remove from the beginning and end of each event.
#' @return An object of type data table as described in \link[tibble]{tibble}.
#' @examples
#' # Load example data
#' data("Pupilex6")
#'
#' dat <- trim_filtered(data = Pupilex6, RmSkipped = TRUE,
#'                      RmEdges = c(75, 75))
#'
#' # Please see the vignettes for detailed example usage.
#' vignette("PupilPre_Interpolation_and_Filtering", package="PupilPre")
#'
trim_filtered <- function(data = data, RmSkipped = NULL, RmEdges = NULL){

  # Check RmSkipped
  if(is.null(RmSkipped)){
    stop("Please set RmSkipped to TRUE or FALSE.")
  }

  # Evaluate RmSkipped
  if(RmSkipped == TRUE){
      message("Removing skipped events.")
    tmp <- data %>% filter(FilterSkip == FALSE)
  } else {
    tmp <- data
  }

  if(is.null(RmEdges)){
    message("RmEdges not specified. Timeseries will not be trimmed.")
  } else {

    # Mark data points
    message("Marking data points for trimming.")
    tmp <- tmp %>% group_by(Event) %>%
      do(
        mutate(., Trimmed = case_when(
          Time >= min(Time) & Time <= (min(Time) + RmEdges[1]) ~ as.logical(TRUE),
          Time >= (max(Time) - RmEdges[2]) & Time <= max(Time) ~ as.logical(TRUE),
          TRUE ~ as.logical(FALSE)
        ))
      ) %>% ungroup()

    message(paste("\nRemoving edges from the data."))
    tmp <- tmp %>%
      filter(Trimmed == FALSE) %>%
      select(-Trimmed)

  }

  return(droplevels(ungroup(tmp)))

}


#' Baseline correct the data
#'
#' \code{baseline} calculates the average pupil value for the window of Time
#' provided in BaselineWindow. The baseline value is then used in one of three
#' different calculation types that must be specified (Subtraction, Division,
#' or Normalization). Baselining is carried out separately for each event.
#'
#' @export
#' @import dplyr
#'
#' @param data A data frame object created from \code{downsample}.
#' @param BaselineWindow A numeric vector of length 1 or 2 specifying the time
#' points of the baseline window to be examined. Providing two values indicates
#' the start time and the end time of the baseline, respectively. Providing a single value
#' (i.e., time point) assumes that every preceding time point is part of the
#' baseline (N.B. trials may vary in the size of the baseline window and will
#' result in an error).
#' @param  BaselineType A character string specifying Subtraction, Division, or
#' Normalization. Subtraction subtracts the average baseline value; Division
#' divides by the average baseline value; and Normalization subtracts and
#' divides by the average baseline value.
#' @param DiffBaseOverride A logical value indicating whether or not to
#' override the error produced when baseline windows differ in size.
#' @return An object of type data table as described in \link[tibble]{tibble}.
#' @examples
#' # Load example data
#' data("Pupilex4")
#'
#' dat <- baseline(Pupilex4, BaselineWindow = c(-500, 0),
#'                 BaselineType = "Subtraction")
#'
#' # Please see the vignettes for detailed example usage.
#' vignette("PupilPre_Basic_Preprocessing", package="PupilPre")
#'
baseline = function(data = data, BaselineWindow = 0, BaselineType = NULL, DiffBaseOverride = FALSE){

  # Check for Baseline type
  if(is.null(BaselineType)){
    stop("Please provide the requested baseline calculation (Subtraction, Division, or Normalization).")
  }

  # Overwrite Pupil_Previous
  data <- data %>% mutate(Pupil_Previous = Pupil)

  # Calculate baseline value
  avg_baseline <- data %>% group_by(Event)

    if(length(BaselineWindow)==1) {
      avg_baseline <- avg_baseline %>% filter(Time <= BaselineWindow)
    } else if(length(BaselineWindow)==2){
      avg_baseline <- avg_baseline %>% filter(Time >= min(BaselineWindow), Time <= max(BaselineWindow))
    }

  # Check that all have the same amount of time
  tmp <- avg_baseline %>% group_by(Event) %>% summarise(N = n())
  if(length(unique(tmp$N)) > 1 & DiffBaseOverride == FALSE){
    stop("Some events have different baseline windows")
  } else if (length(unique(tmp$N)) > 1 & DiffBaseOverride == TRUE){
    message("Some events have different baseline windows. This has been overridden.")
  } else if (length(unique(tmp$N)) == 1) {
    message("All events have the same baseline window available.")
  }

  avg_baseline <- avg_baseline %>%
    summarise(Baseline = mean(Pupil_Previous, na.rm = TRUE))

  tmp <- inner_join(data, avg_baseline, by = c("Event"))

  # Baseline calculation
  if(BaselineType=="Subtraction"){
    .base_fnc <- function(Pupil, Baseline) {return(Pupil-Baseline)}
  } else if(BaselineType=="Division") {
    .base_fnc <- function(Pupil, Baseline) {return(Pupil/Baseline)}
  } else if(BaselineType=="Normalization") {
    .base_fnc <- function(Pupil, Baseline) {return(100*((Pupil-Baseline)/Baseline))}
  }

  tmp <- tmp %>% mutate(Pupil = .base_fnc(Pupil = Pupil_Previous, Baseline = Baseline))
  return(droplevels(ungroup(tmp)))
}


#' Downsample the data
#'
#' \code{downsample} reduces the sampling rate using median values for the
#' Pupil and gaze coordinates.
#'
#' @export
#' @import dplyr
#' @import rlang
#'
#' @param data A data frame object created from \code{blink_cleanup}.
#' @param SamplingRate A postive integer specifying the current sampling rate.
#' @param NewRate A postive integer specifying the desired downsampled rate.
#' @return An object of type data table as described in \link[tibble]{tibble}.
#' @examples
#' # Load example data
#' data("Pupilex4")
#'
#' dat <- downsample(Pupilex4, SamplingRate = 250, NewRate = 25)
#'
#' # Please see the vignettes for detailed example usage.
#' vignette("PupilPre_Basic_Preprocessing", package="PupilPre")
#'
downsample <- function(data, SamplingRate = NULL, NewRate = NULL) {

  # Overwrite Pupil_Previous
  data <- data %>% arrange(Event, Time) %>%
    mutate(Pupil_Previous = Pupil, Gaze_X_Previous = Gaze_X, Gaze_Y_Previous = Gaze_Y)

  if(is.null(SamplingRate)){
    stop("Please supply the sampling rate!")
  }
  if(is.null(NewRate)){
    stop("Please supply the new rate!")
  }

  sampleinterval <- data$Time[2]-data$Time[1]

  if(SamplingRate != 1000/sampleinterval){
    stop("Sampling rate provided does not match the sampling rate of the data.\n Please check the sampling rate using the function check_samplingrate()!")
  }

  binsize <- 1000/NewRate

  if (binsize %% sampleinterval != 0) {
    stop("Sample frequency of data is not a multiple of the target frequency. Please use the function ds_options to determine an appropriate new rate.")
  } else {
    message(paste("Binning information: \n", "Original rate of", SamplingRate,
                  "Hz with one sample every", sampleinterval, "ms. \n", "Downsampled rate of",
                  round(1000/binsize, 2), "Hz using", binsize, "ms bins. \n Binning windows contain", binsize/sampleinterval, "samples."))
  }

  data$DS <- data$Time %/% binsize
  data$DS <- data$DS * binsize

  tmp <- data %>% group_by(Event, DS) %>%
    mutate(Pupil = stats::median(Pupil_Previous, na.rm = TRUE),
           Gaze_X = stats::median(Gaze_X_Previous, na.rm = TRUE), Gaze_Y = stats::median(Gaze_Y_Previous, na.rm = TRUE)) %>%
    filter(Time %in% DS) %>% ungroup() %>% select(-DS, -Pupil_Previous, -Gaze_X_Previous, -Gaze_Y_Previous)

  return(droplevels(ungroup(tmp)))
}


