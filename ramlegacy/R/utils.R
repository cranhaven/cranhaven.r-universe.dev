# private function to check version argument
check_version_arg <- function(version) {
  if (length(version) != 1) {
    stop("Please pass in only one version number.")
  }
  version <- fmt_version(version)

  if (version < "4.4") {
    old_vers <- c("1.0", "2.0", "2.5", "3.0", "4.3")
    if (!version %in% old_vers) {
      stop(paste(
        "Invalid version number."
      ), call. = FALSE)
    }
  } else {
    new_vers <- c("4.40", "4.41", "4.44")
    if (!version %in% new_vers) {
      stop(paste(
        "Invalid version number."
      ), call. = FALSE)
    }
  }
  invisible(TRUE)
}

# private function to format version argument
fmt_version <- function(version) {
  if (typeof(version) == "character") {
    version <- as.numeric(version)
  }
  if (version < 4.4) {
    version <- sprintf("%.1f", version)
  } else {
    version <- sprintf("%.2f", version)
  }
  return(version)
}


# private function to check string
is_string <- function(x) {
  length(x) == 1 && is.character(x)
}

# private function to check validity of path
check_path <- function(path) {
  if (!is_string(path)) {
    stop("`path` must be a string", call. = FALSE)
  }
  TRUE
}

#' @title Output OS-independent path to the rappdirs directory on user's computer where
#' the RAM Legacy database is downloaded by default
#' @name ram_dir
#' @family ramlegacy functions
#' @description Provides the download location for \code{\link{download_ramlegacy}}
#' in an OS independent manner. This is also the location from where
#' \code{\link{load_ramlegacy}} loads the database from.
#' @param vers character, version number of the database. As of writing this
#' package, the available versions are "1.0", "2.0", "2.5", "3.0", "4.3","4.40",
#' "4.41", and "4.44". If version is not specified the \code{ram_dir()}
#' returns the path to the rappdirs top-level directory which stores
#' all the version subdirectories.
#' @export
#' @examples
#' # return the path to the rappdirs directory where
#' # all version subdirectories are stored
#' ram_dir()
#' 
#' # Returns the path of the subdirectory where v4.3
#' # of the database is downloaded to and read from.
#' ram_dir(vers = "4.3")
ram_dir <- function(vers = NULL) {
  if (!is.null(vers)) {
    vers <- fmt_version(vers)
    check_version_arg(vers)
  }
  temp_path <- Sys.getenv("RAM_HOME")
  if (nchar(temp_path) != 0) {
    return(temp_path)
  } else {
    return(rappdirs::user_data_dir("ramlegacy", version = vers))
  }
}

## Ask for yes or no
#' @noRd
ask_yn <- function(...) {
  choices <- c("Yes", "No")
  cat(crayon::green(paste0(..., "\n", collapse = "")))
  cli::cat_rule(col = "green")
  utils::menu(choices) == which(choices == "Yes")
}


## Ask for multiple choices
#' @noRd
ask_multiple <- function(msg, choices) {
  cat(crayon::green(paste0(msg, "\n", collapse = "")))
  cli::cat_rule(col = "green")
  utils::select.list(choices)
}


# Catch 'network timeout error' or 'could not resolve host error' generated
# when dealing with proxy-related or connection related
# issues and fail with an informative error message
#' @noRd
net_check <- function(url, show_error = FALSE) {
  response <- tryCatch(httr::GET(url),
    error = function(e) {
      if (show_error) {
        stop(paste(
          "Could not connect to the internet.",
          "Please check your connection settings and try again."
        ),
        call. = FALSE
        )
      }
    }
  )

  if (typeof(response) == "list") invisible(TRUE) else invisible(FALSE)
}


# regex function to find the latest version from the ram website
#' @noRd
find_latest <- function(ram_url) {

  # set version to 4.44 if retrieving fails for some reason
  vers <- "4.44"

  # make sure there are no connection issues
  if (net_check(ram_url, show_error = FALSE)) {
    req <- httr::GET(ram_url)
    # try to get latest version
    if (req$status_code == 200) {
      # get the content
      contnt <- httr::content(req, "text")
      # get the text within the title tag
      m <- gregexpr("<title>.*</title>", contnt)
      title_text <- unlist(regmatches(contnt, m))
      m_vers <- gregexpr("\\d\\.\\d{1,3}", text = title_text)
      vers <- unlist(regmatches(title_text, m_vers))
    }
  }
  return(vers)
}


# Returns the versions present locally by checking local rappdirs directory
#' @noRd
find_local <- function(path) {

  # get the number of subdirectories in rappdirs directory
  num_dirs <- length(dir(path, pattern = "^[0-9]\\.[0-9]{1,2}$"))

  if (num_dirs == 0) {
    return(NULL)
  }

  # a vector containing all the subdirectories inside rappdirs directory
  # they are all potential local versions
  potential_vers_vec <- dir(path, pattern = "^[0-9]\\.[0-9]{1,}$")

  # check if these directories (potential local versions) contain rds file
  # and create rds_exists_vec, a boolean vector indicating whether the potential
  # local version actually contains a rds file of the database
  rds_exists_vec <- unlist(lapply(potential_vers_vec, function(vers) {
    vers <- fmt_version(vers)
    if (vers < "4.4") {
      rds_path <- file.path(path, file.path(vers, paste0("v", vers, ".rds")))
    } else {
      path1 <- file.path(path, file.path(vers))
      path2 <- file.path(path1, paste0("RLSADB v", vers))
      path3 <- file.path(path2, "DB Files With Assessment Data")
      rds_path <- file.path(path3, paste0("v", vers, ".rds"))
    }
    file.exists(rds_path)
  }))

  # these are the versions that actually exist since they contain rds files
  vers_that_exist <- potential_vers_vec[rds_exists_vec]
  return(vers_that_exist)
}

#' @export
print.ramlist <- function(x, ...) {
  cat(" Number of tables: ", length(x), "\n")
  cat("======================================", "\n")
  for (nme in names(x)) {
    if (nme != "most.used.time.series") {
      tbl_i_dim <- dim(x[[nme]])
      num_rows <- tbl_i_dim[1]
      num_cols <- tbl_i_dim[2]
      cat(paste0(
        "'", nme, "':  ", num_rows,
        " obs. of ", num_cols, " variables"
      ), "\n")
    } else {
      lst_dfs <- x[[nme]]
      cat("\n")
      cat("----------------------------------------------------------", "\n")
      cat('"most.used.time.series": list of following tables:', "\n")
      cat("----------------------------------------------------------", "\n")
      cat("\n")
      for (j in seq_len(length(lst_dfs))) {
        tbl_j_dim <- dim(lst_dfs[[j]])
        tbl_j_name <- names(lst_dfs)[j]
        num_rows <- tbl_j_dim[1]
        num_cols <- tbl_j_dim[2]
        cat(paste0(
          "'", tbl_j_name, "':  ", num_rows,
          " obs. of ", num_cols, " variables"
        ), "\n")
      }
    }
  }
}
