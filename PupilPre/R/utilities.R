#' Check which eyes were recorded during the experiment
#'
#' \code{ppl_check_eye_recording} quickly checks which eyes contain gaze data
#' either using the EYE_TRACKED column (if available) or the Right and
#' Left interest area columns. It prints a summary and
#' suggests which setting to use for the \code{Recording} parameter in the
#' function \code{\link{ppl_select_recorded_eye}}.
#'
#' @export
#' @import dplyr
#' @import rlang
#' @import VWPre
#'
#' @param data A data table object output by \code{\link{create_time_series}}.
#' @return Text feedback and instruction.
#' @examples
#' # Load example data
#' data("Pupilex2")
#'
#' ppl_check_eye_recording(data = Pupilex2)
#'
#' # Please see the vignettes for detailed example usage.
#' # vignette("PupilPre_Basic_Preprocessing", package="PupilPre")
#'
ppl_check_eye_recording <- function(data) {

  # Establish which columns needed, if EYE_TRACKED not present
  lcol <- "LEFT_PUPIL_SIZE"
  rcol <- "RIGHT_PUPIL_SIZE"

  {## SHARED CODE BEGINS HERE ##

    if("EYE_TRACKED" %in% names(data)) {

      message("Checking data using Data Viewer column EYE_TRACKED")
      tmp <- as.data.frame(table(data$EYE_TRACKED))

    } else if(!("EYE_TRACKED" %in% names(data))) {

      # Prep columns for dplyr
      lcol <- enquo(lcol)
      rcol <- enquo(rcol)

      message(paste0("Checking gaze data using Data Viewer columns ", eval_tidy(lcol), " and ", eval_tidy(rcol), "."))
      tmp <- data %>% group_by(Event) %>%
        summarize(left = sum(UQ(sym(eval_tidy(lcol))), na.rm = TRUE), right = sum(UQ(sym(eval_tidy(rcol))), na.rm = TRUE)) %>%
        mutate(Var1 = ifelse(left > 0 & right > 0, "Both",
                             ifelse(left > 0 & right == 0, "Left",
                                    ifelse(left == 0 & right > 0, "Right", NA))))
    }

    if ("Both" %in% unique(tmp$Var1)) {
      b <- 1
    } else {
      b <- 0
    }
    if ("Left" %in% unique(tmp$Var1)) {
      l <- 1
    } else {
      l <- 0
    }
    if ("Right" %in% unique(tmp$Var1)) {
      r <- 1
    } else {
      r <- 0
    }

    if (b == 0 && l == 0 && r == 0) {
      message("No gaze data detected.")
    }
    if (b > 0) {
      if (l == 0 & r == 0) {
        message("The dataset contains recordings for both eyes (ALL participants had both eyes tracked). \n Set the Recording parameter in select_recorded_eye() to 'LandR' and the WhenLandR parameter to either 'Left' or 'Right'.")
      } else {
        message("The dataset contains recordings for both eyes (SOME participants had both eyes tracked). \n Set the Recording parameter in select_recorded_eye() to 'LorR'.")
      }
    } else {
      if (l > 0 && r == 0) {
        message("The dataset contains recordings for ONLY the left eye. \n Set the Recording parameter in select_recorded_eye() to 'L'.")
      }
      if (l == 0 && r > 0) {
        message("The dataset contains recordings for ONLY the right eye. \n Set the Recording parameter in select_recorded_eye() to 'R'.")
      }
      if (l > 0 && r > 0) {
        message("The dataset contains recordings for both eyes (Participants had either the left eye OR the right eye tracked). \n Set the Recording parameter in select_recorded_eye() to 'LorR'.")
      }
    }

  } ## SHARED CODE ENDS HERE ##

}




#' Check blinks
#'
#' \code{blink_summary} summarizes Eyelink marked blinks by Event, Subject, or Item.
#'
#' @export
#' @import dplyr
#' @import rlang
#'
#' @param data A data table object output by \code{\link{create_time_series}}.
#' @param Summary A character string indicating the type of summary.
#' @param ReturnData A logical indicating whether to return a data table
#' containing Start Time information for each event.
#' @return Summary information
#' @examples
#' # Load example data
#' data("Pupilex3")
#'
#' blink_summary(Pupilex3, Summary = "Event")
#'
#' # Please see the vignettes for detailed example usage.
#' # vignette("PupilPre_Basic_Preprocessing", package="PupilPre")
#'
blink_summary <- function(data, Summary = "Event", ReturnData = FALSE) {

  if(!(Summary %in% colnames(data))){
    stop(paste(Summary, " column not present in data!"))
  }

  message(paste0("Calculating blink summary by ", Summary, "."))

  ## calculate summary statistics
  tmp <- data %>% group_by(Event) %>%
    mutate(ContainsBlink = if_else(sum(In_Blink) > 0, "yes", "no")) %>%
  ungroup()

  ## generate tmp data frame
  if("Item" %in% colnames(tmp)) {
    tmp <- tmp %>% group_by(Event) %>%
      summarise(Subject = Subject[1],
                Trial = TRIAL_INDEX[1],
                Item = Item[1],
                ContainsBlink = ContainsBlink[1]) %>%
      ungroup()
  } else {
    tmp <- tmp %>% group_by(Event) %>%
      summarise(Subject = Subject[1],
                Trial = TRIAL_INDEX[1],
                ContainsBlink = ContainsBlink[1]) %>%
      ungroup()
  }

  # summary for subject
  if(Summary != "Event") {
    smry <- sym(eval_tidy(Summary))
    tmp <- tmp %>%
    group_by(!!smry) %>% mutate(Blink = ifelse(ContainsBlink == "yes", 1, 0)) %>%
      summarise(N_Trials = n(), N_Blinks = sum(Blink)) %>%
      mutate(PercentTrialsWithBlinks = (N_Blinks / N_Trials) * 100)
    message(paste0("There are ", nrow(tmp[tmp$N_Blinks > 0,]), " ", Summary, "s containing marked blinks."))
  } else {
    message(paste0("There are ", nrow(tmp[tmp$ContainsBlink=="yes",]), " events containing a marked blink."))
  }

  if(ReturnData == TRUE) {
    return(droplevels(ungroup(tmp)))
  } else {
    message("Set ReturnData to TRUE to output full information.")
  }
}


#' Check missing data
#'
#' \code{NA_summary} summarizes missing data by Event, Subject, or Item.
#'
#' @export
#' @import dplyr
#' @import rlang
#'
#' @param data A data table object output by \code{\link{create_time_series}}.
#' @param Summary A character string indicating the type of summary.
#' @param PupilColumn A character string indicating which column to use for the summary.
#' @param ReturnData A logical indicating whether to return a data table
#' containing the summary information.
#' @return Summary information.
#' @examples
#' # Load example data
#' data("Pupilex3")
#'
#' NA_summary(Pupilex3, Summary = "Event", PupilColumn = "Pupil")
#'
#' # Please see the vignettes for detailed example usage.
#' # vignette("PupilPre_Basic_Preprocessing", package="PupilPre")
#'
NA_summary <- function(data, Summary = "Event", PupilColumn = NULL, ReturnData = FALSE) {

  if(is.null(PupilColumn)){
    stop("Please supply the column containing pupil data!")
  } else {
    if(!(PupilColumn %in% colnames(data))){
      stop(paste(PupilColumn, "column not present in data!"))
    }
  }

  if(!(Summary %in% colnames(data))){
    stop(paste(Summary, " column not present in data!"))
  }

  message(paste0("Calculating missing data summary by ", Summary, "."))

  PupilColumn <- enquo(PupilColumn)
  smry <- sym(eval_tidy(Summary))

  tmp <- data %>% group_by(!! smry) %>%
    mutate(is_NA = if_else(is.na(UQ(sym(eval_tidy(PupilColumn)))), 1, 0)) %>%
    summarise(N_Total = n(), N_NA = sum(is_NA), PercentNA = 100*mean(is_NA)) %>%
    ungroup()
  message(paste0("There are ", nrow(tmp[tmp$PercentNA > 0,]), " ", Summary, "s with missing data."))

  if(ReturnData == TRUE) {
    return(droplevels(ungroup(tmp)))
  } else {
    message("Set ReturnData to TRUE to output full information.")
  }
}



#' Check baseline window for missing data
#'
#' \code{check_baseline} examines the data in a specified baseline window.
#'
#' @export
#' @import dplyr
#' @import rlang
#'
#' @param data A data table object output by \code{\link{create_time_series}}.
#' @param BaselineWindow A numeric vector of length 1 or 2 specifying the time
#' points of the baseline window to be examined. Providing two values indicates
#' the start time and the end time of the baseline, respectively. Providing a single value
#' (i.e., time point) assumes that every preceding time point is part of the
#' baseline (N.B. trials may vary in the size of the baseline window and will
#' result in an error).
#' @param ReturnData A logical indicating whether to return a data table
#' containing information for each event.
#' @return Summary information
#' @examples
#' # Load example data
#' data("Pupilex3")
#'
#' check_baseline(Pupilex3, BaselineWindow = c(-500, 0))
#'
#' # Please see the vignettes for detailed example usage.
#' # vignette("PupilPre_Basic_Preprocessing", package="PupilPre")
#'
check_baseline <- function(data = data, BaselineWindow = NULL, ReturnData = FALSE){

  if(is.null(BaselineWindow)){
    stop("Please supply the baseline window on which to perform the check!")
  }

  # Calc proportion of baseline that is missing
  tmp <- data %>% group_by(Event)

  if(length(BaselineWindow)==1) {
    tmp <- tmp %>% filter(Time <= BaselineWindow)
  } else {
    tmp <- tmp %>% filter(Time >= BaselineWindow[1], Time <= BaselineWindow[2])
  }

  tmp <- tmp %>% mutate(isNA = ifelse(is.na(Pupil), 1, 0)) %>%
    summarise(BaselineMin = min(Time), BaselineMax = max(Time),
              BaselinePeriod = max(Time)-min(Time),
              NBaseline = n(), Blink = ifelse(sum(In_Blink) > 0, "Blink", "NoBlink"), NNA = sum(isNA)) %>%
    mutate(PercentNA = (NNA/NBaseline)*100) %>%
    arrange(-PercentNA)

  message(paste0("There are ", nrow(tmp[tmp$PercentNA>0,]), " events with NAs in the specified baseline."))
  message(paste0("There are ", nrow(tmp[tmp$Blink=="Blink",]), " events with a marked blink in the specified baseline."))

  # Baseline messages
  if(length(unique(tmp$BaselinePeriod)) > 1){
    message(paste0("Some events have different baseline windows prior to ", BaselineWindow, " ms."))
  }

  if(length(BaselineWindow)==2) {
    msgtmp <- tmp %>% filter(BaselinePeriod != BaselineWindow[2]-BaselineWindow[1])
    if(nrow(msgtmp) > 0){
      message(paste0("Some events have shorter baselines than specified (", BaselineWindow[1], " to ", BaselineWindow[2], " ms)."))
    }
  }

  # Return?
  if(ReturnData==TRUE) {
    return(droplevels(ungroup(tmp)))
  } else {
    message("Set ReturnData to TRUE to output full, event-specific information.")
	}

}




#' A utility function to compare pupil size data before and after applying
#' the cleanup
#'
#' \code{compare_summary} is a utility function to compare pupil size data
#' before and after applying the cleanup and summarizes a comparison between
#' Pupil and Pupil_Previous by Event
#'
#' @export
#' @import dplyr
#' @import rlang
#'
#' @param data A data table object output by \code{\link{create_time_series}}.
#' @param ReturnData A logical indicating whether to return a data table
#' containing the summary information.
#' @return Summary information.
#' @examples
#' # Load example data
#' data("Pupilex4")
#'
#' compare_summary(Pupilex4)
#'
#' # Please see the vignettes for detailed example usage.
#' # vignette("PupilPre_Basic_Preprocessing", package="PupilPre")
#'
compare_summary <- function(data, ReturnData = FALSE) {

  if("Baseline" %in% colnames(data)){
    stop("The data appear to be baselined. Comparison is no longer meaningful.")
  }

  if(!("Pupil_Previous" %in% colnames(data))){
    stop("The data appear to be downsampled. Comparison is no longer meaningful.")
  }

  tmp <- data %>% group_by(Event) %>%
    mutate(is_same = .compareNA(Pupil,Pupil_Previous)) %>%
    summarise(PercentDifferent = 100*(1-mean(is_same))) %>%
    ungroup()
  message(paste0("There are ", nrow(tmp[tmp$PercentDifferent > 0,]), " events with differences between Pupil and Pupil_Previous."))

  if(ReturnData == TRUE) {
    return(droplevels(ungroup(tmp)))
  } else {
    message("Set ReturnData to TRUE to output full information.")  }
}



# Compare two columns that might contain NAs
# Returns FALSE when not the same
.compareNA <- function(v1,v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}
