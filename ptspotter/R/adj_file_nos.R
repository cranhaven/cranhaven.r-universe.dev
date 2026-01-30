#' Adjust file numbers.
#'
#' This function is used to increment / decrease sequential scripts within the
#' specified directory, allowing efficient adjustment of script sequence for
#' additional or removed files.
#'
#' @param target Required. The number in the sequential scripts to begin the
#' adjustment. Use single digits only. The adjustment will affect script with
#' that leading digit and greater.
#' @param directory The directory holding the sequential
#' scripts.
#' @param action Defaults to "up". Whether to adjust file numbers up or down.
#'
#' @param step Defaults to 1. The step by which to increment or decrement the
#' file numbering.
#'
#' @return Renumbers filenames in the specified directory, according to the
#' specified action. Only affects the target file and above.
#'
#'@importFrom stringr str_count str_extract str_remove
#'
#' @examples
#' \dontshow{.old_wd <- setwd(tempdir())}
#'
#' seq_file_ops(n = 10, target_dir = "munge")
#'
#' # Increase files numbered 6 and above by 1
#' adj_file_nos(target = 6, directory = "munge")
#'
#' # Increase above target files by a further 2
#' adj_file_nos(target = 6, directory = "munge", step = 2)
#'
#' # Use step = "down" to restore original sequence
#' adj_file_nos(target = 6, directory = "munge", action = "down", step = 3)
#'
#' # writing books or websites:
#' seq_file_ops(n = 5, target_dir = "images", filetype = "png")
#' # adjust by decimals
#' adj_file_nos(target = 1, directory = "images", step = 0.1)
#'
#' # tidying up environment
#' unlink(c("munge", "images"), recursive = TRUE)
#'
#' \dontshow{setwd(.old_wd)}
#'
#' @export
adj_file_nos <- function(target, directory = NULL, action = "up", step = 1) {
  # passing vectors to target currently dangerous. Future compatibility.
  if(length(target) > 1) {
    stop("Please use single digits for `target` only.")
  }

  # list all files in specified directory
  files_found <- list.files(directory)

  # filter out anything that doesn't contain digits at start of string
  num_filenms <- files_found[grepl("^[0-9].", files_found)]

  # if action == up, reverse filenames vec to ensure chain rewrite doesn't occur
  if(action == "up"){
    num_filenms <- rev(num_filenms)
  }

  # extract numbering
  nums_only <- as.numeric(str_extract(num_filenms, "^[0-9]."))

  # remove all numbers from listed filenames vector
  alpha_only <- str_remove(num_filenms, "^[0-9].")

  # reassign the numbers ready for increasing / decreasing
  nums_new <- nums_only

  # if action == up (the default), increment numbers from target and larger up
  # by step
  if (action == "up") {
    # any file numbers greater than the specified target, increase by step
    nums_new[nums_new >= target] <- nums_new[nums_new >= target] + step

    # if action == down, decrease numbers from target and larger down by step
  } else if (action == "down") {
    # any file numbers greater than specified target, decrease by step
    nums_new[nums_new >= target] <- nums_new[nums_new >= target] - step
  }

  # wherever the digits are single, add a 0 in front
  nums_new[str_count(nums_new) == 1] <- paste0(
    "0", nums_new[str_count(nums_new) == 1]
  )

  # paste together new digits and filenames
  adj_filenames <- paste(directory, paste0(nums_new, alpha_only),
                         sep = "/")

  # paste directory name to complete write path
  old_nums <- paste(directory, num_filenms, sep = "/")

  # write out only adjusted filenames
  file.rename(from = old_nums, to = adj_filenames)

  # If action is up, need to reverse old_nums & adj_filenames for print
  if(action == "up"){
    old_nums <- rev(old_nums)
    adj_filenames <- rev(adj_filenames)
  }

  # message confirmation msg to console
  message(paste(
    length(old_nums), "Filenames adjusted.\n",
    "From:", paste(basename(old_nums), collapse = ", "), "\n",
    "To:",
    paste(basename(adj_filenames), collapse = ", ")
  ))
}
