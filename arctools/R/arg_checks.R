
require(lubridate)

#' @noRd
arg_check_acc_m2m <- function(acc){

  ## Check if acc length is a multiple of 1440 (number of minutes in a day)
  acc_n <- length(acc)
  if ((acc_n / 1440) != round(acc_n / 1440)){
    stop("acc vector length is not a multiple of 1440 (number of minutes in a day).")
  }
}


#' @noRd
arg_check_acc_ts <- function(acc_ts){

  if (!lubridate::is.POSIXct(acc_ts)){
    stop("acc_ts should be a POSIXct vector.")
  }
}



#' @noRd
arg_check_acc_and_acc_ts <- function(acc, acc_ts){

  if (length(acc) != length(acc_ts)){
    stop("acc and acc_ts vectors provided are of different vector length while they should be of the same vector length.")
  }
}


#' @noRd
arg_check_valid_day_flag <- function(valid_day_flag){

  ## Check if wear_flag length is a multiple of 1440 (number of minutes in a day)
  valid_day_flag_n <- length(valid_day_flag)
  if ((valid_day_flag_n / 1440) != round(valid_day_flag_n / 1440)){
    stop("valid_day_flag vector length is not a multiple of 1440 (number of minutes in a day).")
  }
}


#' @noRd
arg_check_wear_flag <- function(wear_flag){

  ## Check if wear_flag length is a multiple of 1440 (number of minutes in a day)
  wear_flag_n <- length(wear_flag)
  if ((wear_flag_n / 1440) != round(wear_flag_n / 1440)){
    stop("wear_flag vector length is not a multiple of 1440 (number of minutes in a day).")
  }

  # Check if all wear_flag elements are of type 0,1, NA
  if (any(!(wear_flag %in% c(0,1, NA)))){
    stop("wear_flag vector consists of values other than those in c(0,1,NA). It should not.")
  }
}


#' @noRd
arg_check_minutes_subset <- function(minutes_subset_arg){

  if (!is.null(minutes_subset_arg)){
    if (!all(is.numeric(minutes_subset_arg))){
      stop("Not all values provided in subset/exclude minutes range vector are numeric (while they should be).")
    }
    if (!all(round(minutes_subset_arg) == minutes_subset_arg)){
      stop("Not all values provided in subset/exclude minutes range vector are integer-values (while they should be).")
    }
    if (!(all(1 <= minutes_subset_arg) & all(minutes_subset_arg <= 1440))){
      stop("Not all values provided in subset/exclude minutes range vector are between 1 and 1440 (while they should be).")
    }
  }
}


#' @noRd
arg_check_bed_time <- function(in_bed_time, out_bed_time){

  ## Check that either both are null or both are POSIXct
  if (!is.null(in_bed_time)){
    if (is.null(out_bed_time)){
      stop("out_bed_time is NULL while in_bed_time is not. Should be either both are NULL, or both are POSIXct vectors.")
    }
  }
  if (!is.null(out_bed_time)){
    if (is.null(in_bed_time)){
      stop("in_bed_time is NULL while out_bed_time is not. Should be either both are NULL, or both are POSIXct vectors.")
    }
  }
  if ((!is.null(in_bed_time)) & (!is.null(out_bed_time))){
    if (!lubridate::is.POSIXct(in_bed_time)){
      stop("in_bed_time should be a POSIXct vector.")
    }
    if (!lubridate::is.POSIXct(out_bed_time)){
      stop("out_bed_time should be a POSIXct vector.")
    }
    if (length(in_bed_time) != length(out_bed_time)){
      stop("length(in_bed_time) != length(out_bed_time).")
    }
    if (!all(in_bed_time < out_bed_time)){
      stop("!all(in_bed_time < out_bed_time)")
    }
  }
}



#' @noRd
arg_check_subset_weekdays <- function(subset_weekdays){

  ## Check that subset_weekdays is a vector of integer values from 1 to 7
  if (!is.null(subset_weekdays)){
    if (!all(subset_weekdays %in% 1:7)){
      stop("(If not NULL) subset_weekdays should be a vector of integer values from 1 to 7")
    }
  }
}

