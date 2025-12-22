#' @importFrom stats coef confint filter lag lm na.exclude qchisq reshape runif qf
#' @importFrom utils read.csv read.table write.csv capture.output head tail askYesNo packageVersion
#' @importFrom magrittr %>%
#' @rawNamespace import(future, except = run)
#' @importFrom processx run
#' @import stringr
#' @importFrom jsonlite fromJSON
#' @import ps
NULL
utils::globalVariables(c("period")) # just to avoid the note 'no visible binding for global variable 'period'' on devtools::check()
`%>%` <- magrittr::`%>%`

#' delete_files
#'
#'removes unneeded NONMEM files, including "exe", "f90", "grd", "cpu", "shm", "lnk"
#'
#' @param folder A folder from which to delete non-essential NONMEM files
#' @details Files to be deleted are:
#' FDATA",
#' FCON
#' FSUBS
#' FSUBS.o
#' FSIZES
#' FREPORT,
#' FSTREAM,
#' GFCOMPILE.BAT,
#' INTER,
#' nmprd4p.mod
#' and file with the following extensions:
#' exe, f90, grd, shk, cpu, shm, lnk, phi
#' @noRd
delete_files <- function(folder) {
  # need .ext for use_identifiable, keep .lst, .xml, .mod, FMSG, .shk, .shm
  # but can't delete all, need to keep $TABLE
  try({
    delfiles <-
      c(
        "FDATA",
        "FCON",
        "FSUBS",
        "FSUBS.o",
        "FSIZES",
        "FREPORT",
        "FSTREAM",
        "GFCOMPILE.BAT",
        "INTER",
        "nmprd4p.mod"
      )

    ExtensionsToDelete <- c("exe", "f90", "grd", "cpu", "shm", "lnk")
    ExtensionsToDelete <- paste0("(*.", ExtensionsToDelete, ")")
    ExtensionsToDeleteCollapsed <- paste(ExtensionsToDelete, collapse = "|")
    delfiles <-
      c(
        delfiles,
        dir(path = folder, pattern = ExtensionsToDeleteCollapsed)
      )

    file.remove(file.path(folder, delfiles))
    if (dir.exists(file.path(folder, "temp_dir"))) {
      unlink(file.path(folder, "temp_dir"),
        recursive = TRUE,
        force = TRUE
      )
    }
  })
}
#' Check Requirements for Model-Based BE Assessment
#'
#' This function verifies the necessary requirements for conducting a model-based bioequivalence (BE) assessment.
#'
#' @param run_dir Character string specifying the directory containing the source model files.
#' @param samp_size Integer specifying the number of samples.
#' @param model_list List of character strings representing the model control files to be utilized for model averaging.
#' @param ngroups Integer representing the number of groups for BE testing in the dataset.
#' @param reference_groups Character vector indicating the groups classified as "reference".
#' @param test_groups Character vector indicating the groups classified as "test".
#' @param nmfe_path Character string indicating the path to the nmfe batch file (e.g., nmfe?.bat).
#' @param use_check_identifiable Logical. Determines whether to verify the identifiability by checking if `SADDLE_RESET` is present in $EST.
#' @param simulation_data_path Character string specifying the path to the data file used for simulation.
#' @param user_R_code Logical. Should user-defined R code be executed post the NONMEM run for bootstrap?
#' @param R_code_path Optional. Character string indicating the path to the user-defined R code to be executed post the NONMEM bootstrap. This returns a penalty value to be appended to the BIC.
#'
#' @details The function checks for the following:
#' \enumerate{
#'   \item Presence of the `.mod` file containing `;;;;.*Start EST`.
#'   \item Verification that the sum of reference and test groups equals `ngroups`.
#'   \item Absence of duplicate values in both reference and test groups.
#'   \item Presence of the specified data file.
#'   \item Validity of the provided `nmfe_path`.
#'   \item (If requested) Verification for `SADDLE_RESET` in the context of identifiability.
#'   \item Absence of repeat IDs in the dataset.
#'   \item Availability of simulation data.
#'}
#'
#' @return \code{TRUE} if all requirements pass and \code{FALSE} if any failures.
#'
#' @noRd
check_requirements <- function(run_dir,
                               samp_size,
                               model_list,
                               ngroups,
                               reference_groups,
                               test_groups,
                               nmfe_path,
                               use_check_identifiable,
                               simulation_data_path,
                               user_R_code,
                               R_code_path = NULL) {
    if(dir.exists(run_dir)){
      message("A dialog asking permission to remove ", run_dir, " will appear, it may not be on top")
      OK <- utils::askYesNo(paste("MBBE will delete the folder", run_dir, ", is this OK? (Yes|No)\n"))
      if(!is.na(OK)){
        if(OK){
          message("Continuing")
          unlink(run_dir, recursive = TRUE, force = TRUE)
          dir.create(run_dir)
        }else{
          stop("Removing the run directory is required, consider changing the run directory to one that can be removed, Exiting\n")
        }
      }else{
        stop("Removing the run directory is required, consider changing the run directory to one that can be removed, Exiting\n")
      }
    } else {
      dir.create(run_dir)
    }

  msg <- remove_old_files(run_dir, samp_size)

 if(!is.logical(user_R_code)){
   msg <-
     paste(msg,
           paste0("user_R_code ", user_R_code, " must be logical"),
           sep = "\n")
 }
 if(user_R_code){
   if(!file.exists(R_code_path)){
    msg <-
     paste(msg,
           paste("Error, cannot find ", R_code_path),
           sep = "\n")
   }
 }

  if (!file.exists(nmfe_path)) {
    msg <-
      paste(msg,
        paste0("Cannot find nmfe?? at ", nmfe_path),
        sep = "\n"
      )
  }

  # check number in Reference and Test groups
  if (sum(length(reference_groups), length(test_groups)) != ngroups) {
    msg <-
      paste(
        msg,
        paste(
          "Number of Reference groups",
          length(reference_groups),
          "+ Test groups",
          length(test_groups),
          "doesn't equal the number of groups",
          ngroups
        ),
        sep = "\n"
      )
  }

  # no duplicated in Reference and Test groups
  if (anyDuplicated(c(reference_groups, test_groups)) > 0) {
    msg <-
      paste(msg,
        "There are duplicated group numbers between Reference and Test group",
        sep = "\n"
      )
  }

  if (anyDuplicated(reference_groups) > 0) {
    msg <-
      paste(msg,
        "There are duplicated group numbers in the Reference group",
        sep = "\n"
      )
  }

  if (anyDuplicated(test_groups) > 0) {
    ReturnedValue$rval <- FALSE
    ReturnedValue$msg <-
      paste(ReturnedValue$msg,
        "There are duplicated group numbers in the Test group",
        sep = "\n"
      )
  }
  if (is.null(model_list)) {
    msg <-
      paste(msg,
        "Model list is NULL, error in json file (if called from mbbe_run_json), or model_source (if from mbbe_run)",
        sep = "\n"
      )
  } else {
    for (this_model in 1:length(model_list)) {
      if (!file.exists(model_list[this_model])) {
        msg <-
          paste(msg,
            paste0("Cannot find NONMEM model file ", model_list[this_model]),
            sep = "\n"
          )
        next
      } else {
        control <-
          readLines(model_list[this_model], encoding = "UTF-8", warn = FALSE)

        data_line <- get_block("$DATA", control)
        if (nchar(data_line) == 0) {
          msg <- paste(msg,
            paste0("In the model file ", model_list[this_model], " DATA block not found."),
            sep = "\n"
          )
          next
        }

        data_line <-
          gsub("(\\s*\\$DATA\\s*)|(\\s*$)", "", data_line)
        any.quotes <- grep("^\"", data_line)
        if (length(any.quotes) > 0) {
          data_file <-
            sapply(
              regmatches(
                data_line,
                gregexpr('(\").*?(\")', data_line, perl = TRUE)
              ),
              function(y) {
                gsub("^\"|\"$", "", y)
              }
            )[1]
        } else {
          # find first white space
          data_file <- unlist(strsplit(data_line, " "))[1]
        }

        if (!file.exists(data_file)) {
          msg <-
            paste(msg,
              paste("Cannot find data file", data_file,"from $DATA record in model", this_model),
              sep = "\n"
            )
          next
        }

        # repeat IDs??
        Data <-
          utils::read.csv(data_file, stringsAsFactors = FALSE)
        # no ID found
        if (!"ID" %in% colnames(Data)) {
          msg <-
            append(msg, list(
              rval = FALSE,
              msg = paste("ID column not found in data set ", data_file)
            ))
        }

        IDtimesChanged <-
          which(c(FALSE, tail(Data$ID, -1) != head(Data$ID, -1)))

        if (length(IDtimesChanged) + 1 != length(unique(Data$ID))) {
          msg <-
            paste(
              msg,
              paste0(
                "There appears to be repeat IDs in ",
                data_file,
                "\n, for bootstrap sampling IDs must not repeat. "
              ),
              sep = "\n"
            )
        }

        # $EST must be on one line, need to fix this contains ;;;; Start EST?
        contains_start <-
          grepl(";;;;.*Start\\s+EST", control, ignore.case = TRUE)

        if (!any(contains_start)) {
          msg <- paste(
            msg,
            paste0(
              "The control file ",
              model_list[this_model],
              " does not contain \";;;; Start EST\", required before the $EST and the $THETA records and after $OMEGA and $SIGMA"
            ),
            sep = "\n"
          )
        }
        if (use_check_identifiable) {
          EST.line <- get_block("$EST", control)
          if (!grepl("SADDLE_RESET\\s*=\\s*1", EST.line, fixed = FALSE)) {
            msg <-
              paste(
                msg,
                paste0(
                  "Identifiability check requested, but SADDLE_RESET not set to 1 in ",
                  model_list[this_model]
                ),
                sep = "\n"
              )
          }
        }
        if (!file.exists(simulation_data_path)) {
            msg <- paste(msg,
              paste0(
                "Cannot find simulation data set",
                simulation_data_path
              ),
              sep = "\n"
            )
          }
      }
    }
    ReturnedValue <- list()
    if (nchar(msg) > 0) {
      ReturnedValue$rval <- FALSE
      ReturnedValue$msg <- paste(msg, "Exiting", "\n")
    } else {
      ReturnedValue$rval <- TRUE
      ReturnedValue$msg <- "Passes requirements check"
    }
  }
  return(ReturnedValue)
}

#' split_path
#'
#' split a path into parents
#'
#' @param  path, string, path name to be split
#' @param mustWork logical, optional, default = FALSE
#' @returns list of path parents
#' @noRd
split_path <- function(path, mustWork = FALSE) {
  output <- c(strsplit(dirname(normalizePath(path, mustWork = FALSE)), "/|\\\\")[[1]], basename(path))
  return(output)
}

#' copy_model_files
#'
#' copy NONMEM control files from a source to a run directory
#' no change in control files
#'
#' @param model_source list of NONMEM model files
#' @param run_dir Folder where models are to be run
#' @return nmodels how many models are there
#'
#'
#'@noRd
copy_model_files <- function(model_source, run_dir) {
  if (!file.exists(run_dir)) {
    # split run_dir, check each parent
    normpath <- normalizePath(run_dir)
    parents <- split_path(run_dir)
    nparents <- length(parents)
    cur_path <- parents[1] # should be the drive
    for (this.parent in 2:nparents) {
      cur_path <- file.path(cur_path, parents[this.parent])
      if (!file.exists(cur_path)) {
        dir.create(cur_path)
      }
    }
  }
  nmodels <- length(model_source)
  tryCatch(
    {
      for (this_model in 1:nmodels) {
        bs_dir <- file.path(run_dir, paste0("model", this_model)) # bootstrap directory
        if (dir.exists(bs_dir)) {
          unlink(bs_dir, recursive = TRUE, force = TRUE)
        }
        dir.create(bs_dir)
        modfile <- model_source[this_model]
        if (file.exists(modfile)) {
          file.copy(modfile, file.path(bs_dir, paste0("bs", this_model, ".mod")))
        } else {
          message("Cannot file model file ", modfile, ", exiting")
          return(-999)
        }
      }
      return(nmodels)
    },
    error = function(conc) {
      message("Error in copy_model_files, model # ", this_model, ", exiting")
      return(-999)
    }
  )
}


#' sample_data
#'
#' create bootstrap samples of NONMEM data set, placed in run_dir, file names = data_sampN.csv
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @param run_dir Folder where models are to be run
#' @param nmodels how many models are there
#' @param samp_size how many bootstrap samples
#' @examples
#' \dontrun{
#' sample_data("c:/modelaveraging", 100, 4)
#' }
#' @noRd
sample_data <- function(run_dir, nmodels, samp_size) {
  con <- file(file.path(run_dir, "model1", "bs1.mod"), "r")
  suppressWarnings(control <- readLines(con, encoding = "UTF-8"))
  close(con)
  data_line <- stringr::str_trim(control[grep("\\$DATA", control)])
  data_line <- stringr::str_trim(stringr::str_replace(data_line, "\\$DATA", ""), side = "both")
  # if file name is quoted, just put out part in in quotes, otherwise get first white space
  any.quotes <- grep("^\"", data_line)
  if (length(any.quotes) > 0) {
    # find 2nd
    pos <- gregexpr(pattern = "\"", data_line)
    data_file <- substr(data_line, 1, pos[[1]][2])
    rest.of.data_line <- substr(data_line, pos[[1]][2], nchar(data_line))
  } else {
    # find first white space
    pos <- gregexpr(pattern = "\\s", data_line)
    data_file <- stringr::str_trim(substr(data_line, 1, pos[[1]][1]), side = "both")
    rest.of.data_line <- substr(data_line, pos[[1]][1], nchar(data_line))
  }
  # get datafile possibly different data files in different models? not supported at this time
  org.data <- utils::read.csv(data_file, header = TRUE, stringsAsFactors = FALSE)
  # get IDs
  cols <- colnames(org.data)
  num.data.items <- length(cols)
  # cannot have repeat IDs!!!!
  idID <- match(cols, "ID")
  IDcol <- which(idID %in% 1)
  IDs <- org.data[IDcol] %>%
    dplyr::distinct(ID)
  nsubs <- dim(IDs)[1]
  # create BS data sets
  for (this_samp in 1:samp_size) {
    who <- sample(IDs$ID, nsubs, replace = TRUE)
    this_data <- data.frame(matrix(-99, nrow = 0, ncol = num.data.items))
    colnames(this_data) <- cols
    for (this_rep in 1:nsubs) {
      # need to be sure not to have adjacent with same ID, just just number IDs sequentially
      next_sub <- org.data %>%
        dplyr::filter(ID == who[this_rep]) %>%
        dplyr::mutate(ID = this_rep)
      this_data <- rbind(this_data, next_sub)
    }
    # write data, will be in parent directory of run directory
    datafile.name <- file.path(run_dir, paste0("data_samp", this_samp, ".csv"))
    write.csv(this_data, datafile.name, quote = FALSE, row.names = FALSE)
    # replace $DATA for each control, each model

    for (this_model in 1:nmodels) {
      control_path <- file.path(run_dir, paste0("model", this_model), paste0("bs", this_model, ".mod"))
      con <- file(control_path, "r")
      suppressWarnings(control <- readLines(con, encoding = "UTF-8"))
      close(con)
      data_line <- grep("\\$DATA", control)
      # get rest of data line !!! note, will need to read entire $DATA block, maybe on more than one line!!!!
      newdata_line <- paste0("$DATA ", file.path("..", "..", paste0("data_samp", this_samp, ".csv ", rest.of.data_line)))
      control[data_line] <- newdata_line
      # remove covariance
      cov_line <- grep("\\$COV", control)
      if (length(cov_line) > 0) {
        control[cov_line] <- ";; no covariance step"
      }
      # and any tables
      control <- stringr::str_replace(control, "$TABLE", ";$TABLE")
      dir.create(file.path(run_dir, paste0("model", this_model), this_samp))

      con <- file(file.path(run_dir, paste0("model", this_model), this_samp, paste0("bsSamp", this_model, "_", this_samp, ".mod")), "w")
      writeLines(control, con)
      close(con)
    }
  }
}



#' run_one_model
#'
#' Run a single NONMEM model. This is called for either bootstrap (BS==True) or simulation,
#' The only difference in the call is the name of the resulting control, output, xml etc files
#' executable is always nonmem.exe (Windows)
#'
#' @param run_dir location of source control files, naming is bsSampModelNum_SampleNum.mod
#' @param nmfe_path path to nmfe??.bat
#' @param this_model which of the models used for model averaging is this, integer if bootstrap, NULL if monte carlo
#' @param this_samp  which sample (from bootstrap or Monte Carlo) is this, integer
#' @param BS Logical, TRUE if bootstrap, FALSE if Monte Carlo
#'
#' @examples
#' \dontrun{
#'  run_one_model("c:/MBBE/rundir", "c:/nm74g64/util/nmfe74.bat", 1, 1, TRUE)
#' }
#' @noRd
run_one_model <- function(run_dir, nmfe_path, this_model, this_samp, BS) {
  try({
    if (BS) {
      nmrundir <- file.path(run_dir, paste0("model", this_model), this_samp)
      control_file <- paste0("bsSamp", this_model, "_", this_samp, ".mod")
      output_file <- paste0("bsSamp", this_model, "_", this_samp, ".lst")
      exefile <- paste0("bsSamp", this_model, "_", this_samp, ".exe")
    } else {
      nmrundir <- file.path(run_dir, paste0("MBBEsim", this_samp))
      control_file <- paste0("MBBEsim", this_samp, ".mod")
      output_file <- paste0("MBBEsim", this_samp, ".lst")
      exefile <- paste0("MBBEsim", this_samp, ".exe")
    }

      nmoutput <- processx::run(nmfe_path, args = c(control_file, output_file), wd = nmrundir)
      delete_files(nmrundir)
  })
}
#' run_any_models
#'
#' run the bootstrap or Monte Carlo models/samples
#' This function can use, if available, parallel execution with multicore or multisession
#' The plan is set in run_mbbe
#'
#' @param nmfe_path path to nmfe??.bat
#' @param run_dir Folder where models are to be run
#' @param nmodels how many models are there
#' @param samp_size how many samples are there
#' @param num_parallel number of models to run in parallel
#' @param BS logical TRUE = is bootstrap, FALSE is Monte Carlo Simulation
#' @examples
#' \dontrun{
#' run.bootstrap("c:/nmfe744/util/nmfe74.bat", "c:/modelaveraging", 8)
#' }
#' @noRd
run_any_models <- function(nmfe_path, run_dir, nmodels, samp_size, num_parallel, BS) {

  if (BS) {
    rval <- tryCatch(
      {
        total_runs <- samp_size * nmodels
        message(format(Sys.time(), digits = 0), " Starting bootstrap sample models, total number = ", total_runs, " with ", num_parallel," in parallel" )
        message("Progress bar is NONMEM bootstrap models started")
        output <- furrr::future_map(1:total_runs, function(this_num) {
          tryCatch(
            {
              this_samp <- (this_num - 1) %% samp_size + 1
              this_model <- (this_num - this_samp) / samp_size + 1
              run_one_model(run_dir, nmfe_path, this_model, this_samp, BS)

            },
            error = function(cond) {
              message("Failed to run boostrap in run_any_models, ", cond)
              return(NULL)
            }
          )
        }, .progress = TRUE, .options = furrr::furrr_options(seed = NULL))
      },
      error = function(cond) {
        message("Failed running bootstrap in run_any_models for bootstrap, ", cond)
        return(FALSE)
      }
    )
  } else {
    rval <- tryCatch(
      {
        total_runs <- samp_size
        message(format(Sys.time(), digits = 0), " Starting simulation models, total number = ", total_runs)
        message("Progress bar is NONMEM simulation runs started")
        output <- furrr::future_map(1:total_runs, function(this_num) {
          tryCatch(
            {
              run_one_model(run_dir, nmfe_path, NULL, this_num, FALSE)
            },
            error = function(cond) {
              message("Failed to run simulation in run_any_models for Monte Carlo Simulation, ", cond)
              return(NULL)
            }
          )
        }, .progress = TRUE, .options = furrr::furrr_options(seed = NULL))
      },
      error = function(cond) {
        message("Failed running simulation in run_any_models ", cond)
        return(NULL)
      }
    )
  }
  return(TRUE)
}

#' get_parameters
#'
#' read the xml file for each bootstrap/model, calculate BIC
#'  return list of BICs (with the BEST column) and parameters for all models/samples
#'
#' @param run_dir Folder where models are to be run
#' @param nmodels how many models are there
#' @param samp_size how many samples are there
#' @param delta_parms criteria for identifiability test
#' @param crash_value value for failed calculation
#' @param use_check_identifiable - logical, is check_identifiable to be used
#' @param user_R_code - logical, whether user defined R code is to be run after the bootstrap for model averaging
#' @param R_code_path if user_R_code is TRUE, the path to the R code to be run, default is NULL
#' @examples
#' \dontrun{
#' get_parameters("c:/modelaveraging", 4, 100, 0.1, 999999, TRUE)
#' }
#' @noRd
get_parameters <- function(run_dir, nmodels,
                           samp_size, delta_parms,
                           crash_value, use_check_identifiable,
                           user_R_code, R_code_path = NULL) {

  passes_identifiable <- c(rep(0, nmodels))
  names(passes_identifiable) <- paste0("Model", seq(1:nmodels))
  BICS <- data.frame(matrix(crash_value, nrow = samp_size, ncol = nmodels + 3))
  colnames(BICS) <- c(paste0("Model", seq(1:nmodels)), "Best", "Max_Delta_parm", "Max_Delta")
  Total_penalty <- data.frame(matrix(0, ncol = nmodels, nrow = samp_size))
  colnames(Total_penalty) <- c(paste0("Model", seq(1:nmodels)))
  BICS$Best <- NA
  # BIC=k*ln(n) -2LL where k is is the number of estimated parameters and n is the number of data
  nparms <- data.frame(ntheta = rep(NA, nmodels))

  # only need parameters for best model, but need to read xml file to get the number of observations and parameters, so may as well get
  # THETA etc now for all
  nfailed_ident <- 0 # number of samples in this model that fail the identifiability test
  # do by sample first only save parameters for the best model
  selected_parameters <- vector("list", samp_size)
  identifiable_ok <- c(passes = FALSE, max_delta = -999, max_delta_parm = -999)
  all_identifiables <- rep(TRUE, nmodels)
  this_model <- this_samp <-  1
  tryCatch(
    {
      if(user_R_code){
        source(R_code_path)
      }
      for (this_samp in 1:samp_size) {
        num_successful <- 1 # only save parameters if model finished, doesn't need to converge, just finish, this is the number of successful samples for this model
        parameters_this_sample <- vector("list", nmodels)

        all_identifiables <- rep(TRUE, nmodels)
        for (this_model in 1:nmodels) {
          theta <- NA
          xml_file <- file.path(run_dir, paste0("model", this_model), this_samp, paste0("bsSamp", this_model, "_", this_samp, ".xml"))
          identifiable_ok <- c(passes = FALSE, max_delta = -999, max_delta_parm = -999)
          rval <- tryCatch(
            {
              count <- 0
              boolFalse <- FALSE
              # wait for files to close?? otherwise get Error in file(file, "rt") : cannot open the connection
              while (boolFalse == FALSE & count < 10) {
                data <- NULL
                count <- count + 1
                tryCatch({
                  data <- xml2::read_xml(xml_file, encoding = "ASCII")
                  boolFalse <- TRUE
                }, error = function(e) {
                  Sys.sleep(0.1)
                }, finally = {}) # fails will throw error below in outer tryCatch
              }
              if (count >= 10) {
                message("Failed to read xml file for sample ", this_samp, ", model = ", this_model, ", xml file = ", xml_file)
                BICS[this_samp, this_model] <- crash_value
                identifiable_ok <- c(passes = FALSE, max_delta = -999, max_delta_parm = -999)
                parameters_this_sample[[this_model]] <- NA
                tryCatch(
                  {
                    lstFile <- file.path(run_dir, paste0("model", this_model), this_samp, paste0("bsSamp", this_model, "_", this_samp, ".lst"))
                    cat(BICS[this_samp, this_model], file = lstFile, append = TRUE)
                  },
                  error = function(err) {
                    con <- file(lstFile, "a")
                    message("Cannot write to output file for sample, ", this_samp, ", model, ", this_model)
                  }
                )
              } else {
                problem_node <- xml2::xml_find_all(data, "//nm:problem_information")
                contents <- xml2::xml_contents(problem_node)
                text <- xml2::xml_text(contents)
                text <- as.list(unlist(strsplit(text, "\n")))
                nobsline <- grep("TOT. NO. OF OBS RECS:", text)
                nobs <- as.integer(stringr::str_replace(text[nobsline], "TOT. NO. OF OBS RECS:", ""))
                info_node <- xml2::xml_find_all(data, "//nm:problem_options")
                info_contents <- xml2::xml_attrs(info_node)
                ntheta <- as.numeric(info_contents[[1]]["nthetat"])
                if (this_samp == 1) {
                  # have to do this here, don't know how many thetas until here
                  nparms$ntheta[this_model] <- ntheta
                }
                parameters_this_model <- data.frame(matrix(NA, ncol = ntheta))
                colnames(parameters_this_model) <- paste0("THETA", seq(1:ntheta))
                parameters_this_sample[[this_model]] <- parameters_this_model
                # and OFV
                estim.node <- xml2::xml_find_all(data, "//nm:estimation")
                estim_contents <- xml2::xml_attrs(estim.node)
                # nm:final_objective_function
                OFV.node <- xml2::xml_find_all(data, "//nm:final_objective_function")
                OFV_contents <- xml2::xml_contents(OFV.node)
                OFV <- as.numeric(xml2::xml_text(OFV_contents))
                if (length(OFV) > 0) {
                  BICS[this_samp, this_model] <- ntheta * log(nobs) + OFV
                } else {
                  BICS[this_samp, this_model] <- crash_value
                }
                theta_node <- xml2::xml_find_all(data, "//nm:theta")
                theta_children <- xml2::xml_children(theta_node)
                theta_contents <- xml2::xml_contents(theta_node)
                theta <- as.numeric(xml2::xml_text(theta_contents))
                # length of theta will be 1 if a crash
                if (length(theta) > 1) {
                  parameters_this_model <- theta
                  num_successful <- num_successful + 1
                  parameters_this_sample[[this_model]] <- parameters_this_model
                } else {
                  parameters_this_sample[[this_model]] <- NA
                }

                # if using saddle_reset, test for identifiability
                if (!use_check_identifiable) {
                  identifiable_ok["passes"] <- TRUE
                  passes_identifiable[this_model] <- passes_identifiable[this_model] + 1
                } else {
                  # run_dir,this_model,this_sample,delta_parms
                  identifiable_ok <- check_identifiable(run_dir, this_model, this_samp, delta_parms, ntheta)
                  all_identifiables[this_model] <- identifiable_ok["passes"]
                  if (!identifiable_ok$passes) {
                    # first just get the number of parameters and observations for this model
                    BICS[this_samp, this_model] <- crash_value
                  } else {
                    passes_identifiable[this_model] <- passes_identifiable[this_model] + 1
                  }
                }
              }
            },
            error = function(e) {
              message("error in get_parameters, read data, inner loop, error  = ", e, ", sample ", this_samp, ", model = ", this_model)
              identifiable_ok <- c(passes = FALSE, max_delta = -999, max_delta_parm = -999)
              all_identifiables[this_model] <- identifiable_ok["passes"]
              BICS[this_samp, this_model] <- crash_value
              parameters_this_sample[[this_model]] <- NA
            }
          )
          tryCatch(
            {
              lstFile <- file.path(
                run_dir, paste0("model", this_model),
                this_samp, paste0("bsSamp", this_model, "_", this_samp, ".lst")
              )
              cat(
                paste0(
                  "BIC = ", round(BICS[this_samp, this_model], 3),", ",
                  ", identifiable = ", identifiable_ok["passes"],", ",
                  ", max_delta = ",identifiable_ok['max_delta'],", "
                ),
                file = lstFile, append = TRUE
              )
              cat(paste0(", max_delta_parm = ", identifiable_ok['max_delta_parm']),
                file = lstFile, append = TRUE
              )
            },
            error = function(err) {
              warning("Failed to append parameters to ", lstFile, " after failing in get_parameters, this is only a warning")
            }
          )
          if(user_R_code){
            penalty<- MBBE_RPenaltyCode(run_dir,
                                this_model,
                                this_samp)
            Total_penalty[this_samp, this_model] <- penalty + BICS[this_samp, this_model]
          }else{
            Total_penalty[this_samp, this_model] <- BICS[this_samp, this_model]
          }
         }

        # and select best model, NA if all crashed
        if (all(BICS[this_samp, 1:nmodels] == crash_value)) {
          BICS$Best[this_samp] <- NA
          warning("No models successful for model ", this_samp, " identifiability = ", toString(all_identifiables))
          selected_parameters[[this_samp]] <- NA
          BICS$Max_Delta[this_samp] <- 999999
          BICS$Max_Delta_parm[this_samp] <- NA
        } else {
          best <- which.min(Total_penalty[this_samp,])
          BICS$Best[this_samp] <- best
          selected_parameters[[this_samp]] <- unlist(parameters_this_sample[[best]])
          # check in nparms = length of selected_parameters

          if(nparms$ntheta[best]!= length(selected_parameters[[this_samp]])){
            stop("Length of parameters for sample ", this_samp, " doesn't equal the number in the xml file for model ", best, ", exiting")
          }
          BICS$Max_Delta[this_samp] <- identifiable_ok["max_delta"]
          BICS$Max_Delta_parm[this_samp] <- identifiable_ok["max_delta_parm"]
        }
      }
      },
    error = function(cond) {
      # everything crashes??
      message("Unknown error in get_parameters, read data,", " error  = ", cond)
      BICS[this_samp, this_model] <- crash_value
    }
  )
  # write out results,
  tryCatch(
    {
      # convert BICS to simple data frame for writing to file
      flatBICs <- apply(BICS, 2, as.character)
      write.csv(flatBICs, file.path(run_dir, "BICS.csv"), quote = FALSE)
      message("BICS = ")
      message(paste0(capture.output(BICS), collapse = "\n"))
      write.csv(Total_penalty, file.path(run_dir, "Total_penalties.csv"), quote = FALSE)
      message("Total Penalties for bootstrap models = ")
      message(paste0(capture.output(Total_penalty), collapse = "\n"))
    },
    error = function(err) {
      message("Failed to write out BICS to ", file.path(run_dir, "BICS.csv"), " in get_parameters")
    }
  )
  # write out parameters, can't do write.csv, as rows have different number of parameters

  tryCatch(
    {
      conn <- file(file.path(run_dir, "Parameters.csv"), open = "w")
      writeLines(text = c("Sample","Best Model for Sample","Number of Parmeters","Selected Parameters"), con = conn, sep = "\n")
      newlist <- lapply(seq_len(length(selected_parameters)), function(i) {
        temp <- lapply(seq_len(length(selected_parameters[[i]])), function(j) {
          temp <- c(selected_parameters[[i]][[j]])
        })

        writeLines(text = paste(i,BICS$Best[i], length(selected_parameters[[i]]),
                                paste(temp, collapse = ","), sep = ","), con = conn, sep = "\n")
      })
      close(conn)
    },
    error = function(err) {
      message("Failed to write to ", file.path(run_dir, "Parameters.csv"), " in get_parameters")
    }
  )
  rval <- list(BICS = BICS, parameters = selected_parameters,
               passes_identifiable = passes_identifiable,
               Total_penalties = Total_penalty)
  return(rval)
}

#' get_base_model
#'
#' for each model get the control file used for the bootstrap
#' return a list of the models
#'
#' @param run_dir directory where models are run
#' @param nmodels how many models are there
#' @examples
#' \dontrun{
#' get_base_model("c:/mbbe/rundir", 4)
#' }
#' @return list of the original models, prior to editing for BS data sets
#' @noRd
get_base_model <- function(run_dir, nmodels) {
  base_models <- vector(mode = "list", length = 0) # all but $THETA, $SIM, $TABLE

  for (this_model in 1:nmodels) {
    tryCatch(
      {
        con <- file(file.path(run_dir, paste0("model", this_model), paste0("bs", this_model, ".mod")), "r")
        suppressWarnings(control <- readLines(con, encoding = "UTF-8"))
        control <- control[1:grep(";;;;.*Start EST", control)]
        close(con)
        base_models <- append(base_models, list(control))
      },
      error = function(e) {
        stop("Cannot find ", file.path(paste0(run_dir, "model", this_model), paste0("bs", this_model, ".mod")), "exiting")
      }
    )
  }
  return(base_models)
}

#' write_sim_controls
#'
#' Edits the best based model for each sample, replaces the original parameters with the bootstrap parameters
#' and adds the $SIM and $TABLE
#'
#' @param run_dir Folder where models are to be run
#' @param parms list that include BICs and parameters for each sample/model
#' @param parms list that include BICs and parameters for each sample/model
#' @param base_models list of the text in each model used for the bootstrap
#' @param samp_size how many samples are there
#' @param simulation_data_path, path to the file to be used for simulation
#' @examples
#' \dontrun{
#' write_sim_controls("c:/modelaveraging", parms, base_models, 100)
#' }
#' @noRd
write_sim_controls <- function(run_dir, parms,
                               base_models, samp_size,
                               simulation_data_path) {
  final_models <- list()
  if (!file.exists(simulation_data_path)) {
    stop(paste("Cannot find ", simulation_data_path, " exiting"))
  } else {
    nmodels <- length(base_models)
    current_runable_samp <- 0 # some models may not be runable (all crashed), if so, start over with model, and different seed in NONMEM
    for (this_samp in 1:samp_size) {
      current_runable_samp <- current_runable_samp + 1
      count <- 0
      while (is.na(parms$BICS$Best[current_runable_samp]) & count < samp_size * 4) { # only go through samples 4 times? don't keep running same samples over?
        current_runable_samp <- current_runable_samp + 1
        count <- count + 1
        if (current_runable_samp > samp_size) {current_runable_samp <- 1}
        # use BS parameters, when you run out (as some BS samples fail), just start over, so need different random seed in $SIM model
      }
      if (count >= (samp_size * 4)) {
        stop("Not enough samples with successful outcome for simulation")
      }
      # current_runable_samp is sample #, from 1 to samp_size
      # which.model is the best model in that sample, from 1 to nmodels
      # note this determines the base model, but parameters come from
      # parms#parameters[[this_samp]]
      which.model <- parms$BICS$Best[current_runable_samp]
      # is first index, e.g., parameters[[2]]$THETA1[1] is THETA[1] for model 2, first sample
      # model.indices[which.model] <- model.indices[which.model] + 1
      full_control <- base_models[which.model][[1]]
      # need to get sequence in here, calculated in $PK
      seed <- round(runif(1, 0, 10000), 0)
      full_control <- c(full_control, paste("$SIM ONLYSIM (", seed, ")"), "$THETA")
      ntheta <- length(parms$parameters[[this_samp]])

        # get $DATA
      start_line <- grep("^\\$DATA", full_control)
      next_line <- grep("^\\$", full_control[start_line + 1:length(full_control)])[1]
      if (is.empty(next_line)) {
          # $DATA is last
        last_line <- length(full_control[[1]])
      } else {
        last_line <- start_line + next_line
      }
        # replace with simulation data set
      full_control <- c(full_control[1:(start_line - 1)], paste("$DATA", simulation_data_path, "IGNORE=@"), full_control[(last_line):length(full_control)])

      for (this_parm in parms$parameters[[this_samp]]) {
        full_control <- c(full_control, paste0(this_parm, "  ;; THETA(", this_parm, ")"))
      }
      full_control <- c(full_control, "$TABLE ID TIME GROUP PERIOD SEQ DV EVID NOPRINT NOAPPEND FILE=OUT.DAT ONEHEADER")
      full_control <- c(full_control, paste0(";;Sample #", this_samp, "; Selected sample = ",current_runable_samp, "\n"))
      full_control <- c(full_control, paste0(";;Source = model #", which.model, ", numparms = ", ntheta, "\n"))

      sim_dir <- file.path(run_dir, paste0("MBBEsim", this_samp))
      if (dir.exists(sim_dir)) {
        unlink(sim_dir, recursive = TRUE, force = TRUE)
      }
      if (dir.exists(sim_dir)) {
        unlink(sim_dir, recursive = TRUE, force = TRUE)
      }
      dir.create(sim_dir)
      control_file <- file.path(sim_dir, paste0("MBBEsim", this_samp, ".mod"))
      count <- 0
      while (!file.exists(control_file) & count < 10) {
        con <- file(control_file, "w")
        Sys.sleep(0.1)
        writeLines(unlist(full_control), con)
        count <- count + 1
        close(con)
      }
      if (!file.exists((control_file))) {
        message("Cannot write to ", control)
      }
      final_models <- append(final_models, list(full_control))
    }
    return(final_models)
  }
}
#'

#' Calculate Non-Compartmental Analysis (NCA) Parameters
#'
#' This function performs Non-Compartmental Analysis (NCA) to derive key pharmacokinetic parameters such as Cmax, AUCinf, and AUClast for specified time intervals.
#'
#' @param run_dir Character string specifying the path to the run directory.
#' @param ngroups Integer specifying the total number of groups (e.g., 4 for an ABBA design).
#' @param reference_groups Numeric vector indicating the group IDs that are designated as reference.
#' @param test_groups Numeric vector indicating the group IDs that are designated as test.
#' @param NCA_end_time Numeric value specifying the end time for calculations of AUClast and AUCinf.
#' @param samp_size Integer indicating the sample size or the total number of samples for the analysis.
#'
#' @details
#' The `calc_NCA` function internally calls `getNCA` for each sample in the sequence from 1 to `samp_size`. Note that the function is currently executed in a serial manner and is not parallelized.
#'
#' @examples
#' \dontrun{
#' run_dir <- "c:/Workspace/mbbe"
#' ngroups <- 4
#' reference_groups <- c(1,2)
#' test_groups <- c(3,4)
#' NCA_end_time <- 7
#' samp_size <- 6
#' calc_NCA(run_dir, ngroups, reference_groups, test_groups, NCA_end_time, samp_size)
#' }
#'
#' @return The function returns a list containing the derived NCA parameters for each sample.
#'
#' @export
calc_NCA <-
  function(run_dir,
           ngroups,
           reference_groups,
           test_groups,
           NCA_end_time,
           samp_size) {
    # still need to parallelize this
    tryCatch(
      {
        for (this_samp in 1:samp_size) {
          getNCA(
            run_dir,
            this_samp,
            ngroups,
            reference_groups,
            test_groups,
            NCA_end_time
          )
        }
      },
      error = function(cond) {
        message("Failed in calc_NCA, sample # ", this_samp, ", error ", cond)
        return(NULL)
      }
    )
    return()
  }

#' Check Model Identifiability Based on Parameter Differences
#'
#' Evaluates model identifiability based on the definitions by Aoki (https://www.page-meeting.org/default.asp?abstract=5951).
#' The check involves evaluating the differences between pre-reset and post-reset parameters during the NONMEM minimization process.
#' If the fractional difference between any of these parameters exceeds a given threshold (`delta_parms`), the model is considered non-identifiable.
#'
#' @param run_dir Character string specifying the home directory.
#' @param this_model Integer representing the specific model being evaluated.
#' @param this_sample Integer representing the specific sample being evaluated.
#' @param delta_parms Numeric threshold for the absolute difference in parameters, above which the model fails identifiability criteria.
#' @param nparms Integer specifying the number of parameters in the .ext file. This can be fewer than the total number of parameters.
#'
#' @details
#' The algorithm involves:
#' 1. Reading the .lst file to identify instances of saddle resets (which apparently aren't recorded elsewhere).
#' 2. Extracting parameters before the reset from the .ext file (designated as "Pre.parms").
#' 3. Comparing these with the final parameters (designated as "Post.parms") to determine if any differ by the `delta_parms` threshold.
#'
#' @return
#' A list containing:
#' \itemize{
#'   \item \strong{passes}: Logical indicating if the model passes the identifiability check.
#'   \item \strong{max_delta}: Numeric value representing the maximum difference observed between pre and post reset parameters.
#'   \item \strong{max_delta_parm}: Integer indicating the parameter in the .ext file that exhibits the largest difference.
#' }
#'
#' @examples
#' \dontrun{
#' check_identifiable("c:/runmodels", 1, 1, 0.1, 5)
#' }
#' @noRd
check_identifiable <- function(run_dir,
                               this_model,
                               this_sample,
                               delta_parms,
                               nparms) {
  lstfile <- file.path(run_dir, paste0("model", this_model), this_sample, paste0("bsSamp", this_model, "_", this_sample, ".lst"))
  extfile <- file.path(run_dir, paste0("model", this_model), this_sample, paste0("bsSamp", this_model, "_", this_sample, ".ext"))
  max_deltap <- -999
  max_delta_parmp <- -999
  if (!file.exists(lstfile) | !file.exists(extfile)) {
    message("Cannot find ", listfile, " or ", extfile, " for determining identifiability")
    return(c(passes = FALSE, max_delta = -999, max_delta_parm = -999))
  } else {
    tryCatch(
      {
        con <- file(lstfile, "r")
        suppressWarnings(output <- readLines(con, encoding = "UTF-8"))
        close(con)
        reset.line <- grep("^0SADDLE POINT RESET", output)

        # get previous '0ITERATION NO.: '
        first_output <- output[1:reset.line]
        first_output <- first_output[grep("^0ITERATION NO.:", first_output)]
        first_output <- first_output[length(first_output)]
        reset_iteration <- as.integer(substr(first_output, 16, 24))
        last_output <- output[reset.line:length(output)]
        last_output <- last_output[grep("^0ITERATION NO.:", last_output)]
        last_output <- last_output[1]
        last.iteration <- as.integer(substr(last_output, 16, 24))
        # read parameters from .ext
        ext <- read.table(extfile, header = TRUE, skip = 1)
        Pre.parms <- ext %>%
          dplyr::filter(ITERATION == reset_iteration)
        Post.parms <- ext %>%
          dplyr::filter(ITERATION == last.iteration)
        # nparms <- length(Pre.parms)  # includes the first column - 'ITERATION'
        passes_identifiability <- TRUE

        for (this_parm in 2:nparms) {
          if (Post.parms[this_parm] != 0) {
            difference <- abs((Pre.parms[this_parm] - Post.parms[this_parm]) / Post.parms[this_parm])
            if (difference > delta_parms) {
              passes_identifiability <- FALSE
            }
            if (difference > max_deltap) {
              max_deltap <- difference
              max_delta_parmp <- this_parm - 1
            }
          }
        }
        rval <- c(passes = passes_identifiability, max_delta = max_deltap, max_delta_parm = max_delta_parmp)
        names(rval) <- c("passes", "max_delta", "max_delta_parm")
        return(rval)
      },
      error = function(cond) {
        rval <- c(passes = passes_identifiability, max_delta = max_deltap, max_delta_parm = max_delta_parmp)
        names(rval) <- c("passes", "max_delta", "max_delta_parm")
        return(rval)
      }
    )
  }
}

#' Calculate NCA Parameters from NONMEM Output
#'
#' This function extracts and calculates Non-Compartmental Analysis (NCA) parameters (Cmax, AUCinf, AUClast) by group
#' from a NONMEM simulation output file. The output file must adhere to certain specifications,
#' including naming conventions and table header options.
#'
#' @param run_dir Character string specifying the home directory where the NONMEM output files are located.
#' @param this_sample Integer representing the specific sample being evaluated.
#' @param NumGroups Integer specifying the total number of groups (e.g., 4 for an ABBA design).
#' @param reference_groups Numeric vector indicating the group IDs that are designated as reference.
#' @param test_groups Numeric vector indicating the group IDs that are designated as test.
#' @param NCA_end_time Numeric value indicating the end time for the AUClast and AUCinf calculations.
#'        Calculation starts at TIME=0. A data point at TIME=0 is mandatory in the simulated study.
#'
#' @details
#' The NONMEM output file should be present in the folder named `MBBEsimN`, where `N` is the simulation number.
#' The `$TABLE` in the NONMEM file must use the `ONEHEADER` option and include specific columns such as `ID`, `GROUP`, `SEQ`,
#' and `PERIOD`. For MBBE, the simulation control file, usually created by the `write_sim` function, will have the
#' table specifications like: "$TABLE ID TIME GROUP PERIOD SEQ DV EVID NOPRINT NOAPPEND FILE=OUT.DAT ONEHEADER".
#' This function reads the `$TABLE` output from the simulation (filename `OUT.DAT`) and performs NCA.
#'
#' @return
#' Logical indicating the success or failure of the NCA calculation.
#'
#' @examples
#' \dontrun{
#' getNCA("c:/runmodels", 1, 4, c(1, 2), c(3, 4), 72)
#' }
#'
#' @noRd
getNCA <- function(run_dir,
                   this_sample,
                   NumGroups,
                   reference_groups,
                   test_groups,
                   NCA_end_time) {
  output_file <- file.path(run_dir, paste0("MBBEsim", this_sample), paste0("NCAresults", this_sample, ".csv"))
  tryCatch(
    {
      NMoutFile <- file.path(run_dir, paste0("MBBEsim", this_sample), "OUT.DAT")
      if (file.exists(NMoutFile)) {
        group_NCA_results <- data.frame(
          ID = as.integer(), treatment = as.integer(), period = as.integer(), sequence = as.integer(),
          Cmax = as.numeric(), AUCinf = as.numeric(), AUClast = as.numeric()
        )
        All_NCA_results <- data.frame(
          ID = as.integer(), treatment = as.integer(), period = as.integer(), sequence = as.integer(),
          Cmax = as.numeric(), AUCinf = as.numeric(), AUClast = as.numeric()
        )
        data <- read.table(NMoutFile, skip = 1, header = TRUE)
        # remove negative concentrations
        data <- data %>%
          dplyr::filter(EVID == 0) %>%
          dplyr::filter(DV > 0)

        if (file.exists(output_file)) file.remove(output_file)
        for (this_group in 1:NumGroups) {
          group_data <- data %>%
            dplyr::filter(GROUP == this_group)
          # keep period for each subject, for this group
          period_seq <- group_data %>%
            dplyr::group_by(ID) %>%
            dplyr::distinct(ID, .keep_all = TRUE) %>%
            dplyr::select(ID, PERIOD, SEQ) %>%
            dplyr::arrange(ID)

          # insert conc=0 at time = 0, but remove if duplicated??
          zero_time <- group_data %>%
            dplyr::distinct(ID, .keep_all = TRUE)
          zero_time$TIME <- 0
          zero_time$DV <- 0
          group_data <- rbind(group_data, zero_time) %>%
            dplyr::arrange(ID, TIME)
          conc_obj <- PKNCA::PKNCAconc(
            group_data,
            DV ~ TIME | ID
          )
          data_obj <- PKNCA::PKNCAdata(
            data.conc = conc_obj,
            intervals = data.frame(
              start = 0,
              end = NCA_end_time,
              aucinf.obs = TRUE,
              auclast = TRUE,
              cmax = TRUE
            )
          )
          tryCatch({
          suppressMessages(
            results_obj <- PKNCA::pk.nca(data_obj, verbose = FALSE)$result
          )
            },  error = function(cond) {
            message("Error in NCA calculation, ", this_sample, "\n")
            message(cond)
            return(FALSE)
          }
            )
          AUCinf <- results_obj %>%
            dplyr::filter(PPTESTCD == "aucinf.obs") %>%
            dplyr::select(ID, PPORRES)
          AUClast <- results_obj %>%
            dplyr::filter(PPTESTCD == "auclast") %>%
            dplyr::select(ID, PPORRES)
          CMAX <- results_obj %>%
            dplyr::filter(PPTESTCD == "cmax") %>%
            dplyr::select(ID, PPORRES)
          if (this_group %in% reference_groups) {
            treatment <- "Reference"
          } else {
            treatment <- "Test"
          }
          group_NCA_results <- data.frame(
            ID = AUCinf$ID,
            treatment = treatment,
            period = period_seq$PERIOD,
            sequence = period_seq$SEQ,
            Cmax = CMAX$PPORRES,
            AUCinf = AUCinf$PPORRES,
            AUClast = AUClast$PPORRES
          )
          All_NCA_results <- rbind(
            All_NCA_results,
            group_NCA_results
          )
          }
        count <- 0
        while (file.exists(output_file) & count < 50) {
          file.remove(output_file)
          count <- count + 1
          Sys.sleep(0.1)
        }
        count <- 0
        while (!file.exists(output_file) & count < 50) {
          write.csv(All_NCA_results,
            file = output_file,
            quote = FALSE,
            row.names = FALSE
          )
          count <- count + 1
          Sys.sleep(0.1)
        }
        if (!file.exists(output_file)) {
          message("Cannot write to ", output_file)
          return(FALSE)
        }
        return(TRUE)
      }else{
        message("Cannot find file ",NMoutFile )
        return(FALSE)
      }
    },
    error = function(cond) {
      message("Error in NCA calculation, ", this_sample, "\n")
      message(cond)
      return(FALSE)
    })
}




#' Calculate Bioequivalence Power
#'
#' Computes the power for bioequivalence (BE) testing based on EMA standards statistics
#' applied to each Monte Carlo simulation. The power is determined by the proportion of
#' simulations that meet the BE criteria.
#'
#' @param run_dir Character string specifying the run directory where simulation outputs are located.
#' @param samp_size Integer indicating the number of samples to be used in the analysis.
#' @param alpha Numeric value representing the alpha error rate. It must lie between 0 and 1.
#' @param model_averaging_by Character string indicating the method for model averaging, either "subject" or "study".
#' @param NTID Logical indicating if the drug being tested is a narrow therapeutic index drug.
#'
#' @details
#' When the simulation is conducted by study (i.e., a unique model for each study),
#' this results in model averaging at the study level. If `model_averaging_by` is set to "subject",
#' data from different studies are merged. For each study dataset, subjects are randomly selected
#' (without replacement) from across all studies.
#'
#' The function iterates over each sample, reading the corresponding NCAresults (designated by the
#' sample number). Subsequently, it determines if each sample meets or fails the BE testing criteria.
#'
#' @return
#' A list containing the results for:
#' - `Cmax_result`: Power for the Cmax parameter.
#' - `AUCinf_result`: Power for the AUCinf parameter.
#' - `AUClast_result`: Power for the AUClast parameter.
#' All power values range between 0 and 1.
#'
#' @examples
#' calc_power(
#'  run_dir = system.file(package = "mbbe", "examples", "calc_power"),
#'  samp_size = 5,
#'  alpha = 0.05,
#'  model_averaging_by = "study",
#'  NTID = FALSE
#' )
#'
#' @export

calc_power <- function(run_dir,
                       samp_size,
                       alpha,
                       model_averaging_by,
                       NTID) {

  BEsuccess <- data.frame(
    Cmax.success = as.logical(),
    AUCinf.success = as.logical(),
    AUClast.success = as.logical()
  )
  message(format(Sys.time(), digits = 0), " Starting statistics for NCA parameters, simulations 1-", samp_size)

  all_results <- data.frame(
    SampleNum = as.integer(),
    Cmax_MetricColumn = as.numeric(),
    Cmax_Ratio = as.numeric(),
    Cmax_lower_CL = as.numeric(),
    Cmax_upper_CL = as.numeric(),
    Cmax_swR = as.numeric(),
    Cmax_pe = as.numeric(),
    Cmax_critbound = as.numeric(),
    Cmax_VarianceCriterion = as.numeric(),
    Cmax_Assessment = as.numeric(),
    Cmax_BE = as.logical(),
    AUCinf_MetricColumn = as.numeric(),
    AUCinf_Ratio = as.numeric(),
    AUCinf_lower_CL = as.numeric(),
    AUCinf_upper_CL = as.numeric(),
    AUCinf_swR = as.numeric(),
    AUCinf_pe = as.numeric(),
    AUCinf_critbound = as.numeric(),
    AUCinf_VarianceCriterion = as.numeric(),
    AUCinf_Assessment = as.numeric(),
    AUCinf_BE = as.logical(),
    AUClast_MetricColumn = as.numeric(),
    AUClast_Ratio = as.numeric(),
    AUClast_lower_CL = as.numeric(),
    AUClast_upper_CL = as.numeric(),
    AUClast_swR = as.numeric(),
    AUClast_pe = as.numeric(),
    AUClast_critbound = as.numeric(),
    AUClast_VarianceCriterion = as.numeric(),
    AUClast_Assessment = as.numeric(),
    AUClast_BE = as.logical()
  )

  # read in all NCAresults file
  # theses will one one model per study
  # once all are collected reassmble into on model per subject in each study
  all_nca_data <- data.frame(
    sample = as.integer(), ID = as.integer(), treatment = as.integer(), period = as.integer(), sequence = as.integer(), Cmax = as.numeric(),
    AUCinf = as.numeric(), AUClast = as.numeric()
  )

  nsubs <- NA

  for (this_samp in 1:samp_size) {
    this_ncafile <- file.path(run_dir, paste0("MBBEsim", this_samp), paste0("NCAresults", this_samp, ".csv"))
    # wait for files to close?? otherwise get Error in file(file, "rt") : cannot open the connection

    boolFalse <- FALSE
    count <- 0
    while (boolFalse == FALSE & count < 20) {
      count <- count + 1
      tryCatch({
        if (file.exists(this_ncafile)) {}
        this_data <- utils::read.csv(this_ncafile, header = TRUE)
        boolFalse <- TRUE
        if (is.na(nsubs)) {
          IDs <- this_data %>% dplyr::distinct(ID)
          nsubs <- dim(IDs)[1]
        }
      }, error = function(e) {
        Sys.sleep(0.1)
      }, finally = {}) # fails will throw error below
    }
    if(exists("this_data")){
      this_data$sample <- this_samp
      this_data <- subset(this_data, select = c(sample, ID:AUClast))
      all_nca_data <- rbind(all_nca_data, this_data)
      if(exists("this_data")) { rm(this_data)}
    }else {
      message("Cannot find ",this_ncafile, ", NCA output file")
    }

  }

  if (model_averaging_by == "subject") {
    # original, doesn't really need to be study numbers
    sequences <- matrix(NA, nrow = nsubs, ncol = samp_size)
    colnames(sequences) <- paste0("Study", 1:samp_size)
    for (this_sub in 1:nsubs) {
      sequences[this_sub, ] <- sample(1:samp_size, samp_size, replace = FALSE)
    }
    # reassemble all_nca_data
    #
    new_all_nca_data <- all_nca_data[0, ]
    # and fill in with new sequence
    for (this_samp in 1:samp_size) {
      for (this_sub in 1:nsubs) {
        new_sample <- all_nca_data %>% dplyr::filter(ID == this_sub, sample == sequences[this_sub, this_samp])
        new_sample$sample <- this_samp
        new_all_nca_data <- rbind(new_all_nca_data, new_sample)
      }
    }
    all_nca_data <- new_all_nca_data
    outfile <- file.path(run_dir, "resampledNCA.csv")
    message("Resampled NCA data for subject level model averaging written to ", outfile)
    write.csv(all_nca_data, outfile)
  }

# convert to factors
  all_nca_data <- all_nca_data %>%
    dplyr::mutate(sample = as.factor(sample),
           ID = as.factor(ID),
           treatment = as.factor(treatment),
           period = as.factor(period),
           sequence = as.factor(sequence) )
  message(format(Sys.time(), digits = 0), " Done reading NCA results, doing stats")
  if (!is.null(all_nca_data)) {
   sample_nums <- all_nca_data %>% dplyr::distinct(sample)
    ## and do stats, loop over do stats for each
    for(this_study in sample_nums$sample){
      Cmax_result <- data.frame(
        MetricColumn = "Cmax", Ratio = -999, lower.CL = -999, upper.CL = -999,
        swR = -999, pe = -999, critbound = -999, VarianceCriterion = -999,
        Assessment = -999, BE = -999
      )
      tryCatch(
        {
          Cmax_result <- get_BEQDF(all_nca_data %>%
                                     dplyr::filter(!is.na(Cmax)) %>%
                                     dplyr::filter(sample == this_study) %>%
                                     dplyr::filter(sample == this_study),
            MetricColumn = "Cmax",
            SubjectColumn = "ID", TreatmentColumn = "treatment", SequenceColumn = "sequence",
            PeriodColumn = "period", RefValue = "Reference", alpha = alpha, PartialReplicate = FALSE, NTID
          )
          Cmax_result$SampleNum <- this_study
          # reorder, but sampleNum first
          Cmax_result <- subset(Cmax_result, select = c(SampleNum, MetricColumn:BE))
        },
        error = function(e) {
          Cmax_result <- data.frame(
            SampleNum = this_samp, MetricColumn = "Cmax", Ratio = -999, lower.CL = -999, upper.CL = -999,
            swR = -999, pe = -999, critbound = -999, VarianceCriterion = -999, Assessment = -999, BE = -999
          )
        }
      )
      if(Cmax_result$Ratio == -999){ # if crash, need to add study #
        Cmax_result$SampleNum <- this_study
        Cmax_result$VarianceCriterion  <- NA
        Cmax_result$BE  <- FALSE
        # reorder
        Cmax_result <- Cmax_result %>% dplyr::select(SampleNum, MetricColumn, Ratio, lower.CL, upper.CL, swR, pe,
                               critbound, VarianceCriterion, Assessment, BE)
      }
      colnames(Cmax_result) <- c(
        "SampleNum", "Cmax_MetricColumn", "Cmax_Ratio", "Cmax_lower_CL", "Cmax_upper_CL", "Cmax_swR", "Cmax_pe",
        "Cmax_critbound", "Cmax_VarianceCriterion", "Cmax_Assessment", "Cmax_BE"
      )


      AUClast_result <- data.frame(
        MetricColumn = "AUClast", Ratio = -999, lower.CL = -999, upper.CL = -999,
        swR = -999, pe = -999, critbound = -999, VarianceCriterion = -999,
        Assessment = -999, BE = -999
      )
      tryCatch(
        {
          AUClast_result <- get_BEQDF(all_nca_data %>% dplyr::filter(!is.na(AUClast)) %>% dplyr::filter(sample == this_study),
            MetricColumn = "AUClast",
            SubjectColumn = "ID", TreatmentColumn = "treatment", SequenceColumn = "sequence",
            PeriodColumn = "period", RefValue = "Reference", alpha = alpha, PartialReplicate = FALSE, NTID
          )
        },
        error = function(e) {
          AUClast_result <- data.frame(
            MetricColumn = "AUClast", Ratio = -999, lower.CL = -999, upper.CL = -999,
            swR = -999, pe = -999, critbound = -999, VarianceCriterion = -999, Assessment = -999, BE = -999
          )
        }
      )
      if(AUClast_result$Ratio == -999){ # if crash, need to add study #
        AUClast_result$BE  <- FALSE
        AUClast_result$VarianceCriterion  <- NA
        # fix sequence
        AUClast_result <- AUClast_result %>% dplyr::select(MetricColumn, Ratio, lower.CL, upper.CL, swR, pe,
                                      critbound, VarianceCriterion, Assessment, BE)
      }
      colnames(AUClast_result) <- c(
        "AUClast_MetricColumn", "AUClast_Ratio", "AUClast_lower_CL", "AUClast_upper_CL", "AUClast_swR", "AUClast_pe",
        "AUClast_critbound", "AUClast_VarianceCriterion", "AUClast_Assessment", "AUClast_BE"
      )


      AUCinf_result <- data.frame(
        MetricColumn = "AUCinf", Ratio = -999, lower.CL = -999, upper.CL = -999,
        swR = -999, pe = -999, critbound = -999, VarianceCriterion = -999,
        Assessment = -999, BE = -999
      )
      tryCatch(
        {
          AUCinf_result <- get_BEQDF(all_nca_data %>% dplyr::filter(!is.na(AUCinf)) %>% dplyr::filter(sample == this_study),
            MetricColumn = "AUCinf",
            SubjectColumn = "ID", TreatmentColumn = "treatment", SequenceColumn = "sequence",
            PeriodColumn = "period", RefValue = "Reference", alpha = alpha, PartialReplicate = FALSE, NTID
          )
        },
        error = function(e) {
          AUCinf_result <- data.frame(
            MetricColumn = "AUCinf", Ratio = -999, lower.CL = -999, upper.CL = -999,
            swR = -999, pe = -999, critbound = -999, VarianceCriterion = -999,
            Assessment = -999, BE = -999
          )
        }
      )
      if(AUCinf_result$Ratio == -999){
        AUCinf_result$BE  <- FALSE
        AUCinf_result$VarianceCriterion  <- NA
        AUCinf_result <- AUCinf_result %>% dplyr::select(MetricColumn, Ratio, lower.CL, upper.CL, swR, pe,
                                                           critbound, VarianceCriterion, Assessment, BE)

      }
      colnames(AUCinf_result) <- c(
        "AUCinf_MetricColumn", "AUCinf_Ratio", "AUCinf_lower_CL", "AUCinf_upper_CL", "AUCinf_swR", "AUCinf_pe",
        "AUCinf_critbound", "AUCinf_VarianceCriterion", "AUCinf_Assessment", "AUCinf_BE"
      )
      all_results <- rbind(all_results, c(Cmax_result, AUCinf_result, AUClast_result))
      }
    } else {
    # no out.dat
  }
  return(all_results)
}
#' make_NCA_plots
#'
#'generated histograms of AUClast, AUCinf and Cmax for the BE samples
#'
#' @param BICS - data objects for Bayesian information Criteria
#' @param run_dir Folder that NONMEM models were run in
#' @param samp_size Integer, number of samples
#' @param nmodels Integer, number of models
#' @param reference_groups, which GROUP (required item in NONMEM $TABLE) are reference formulation
#' @param test_groups, which GROUP (required item in NONMEM $TABLE) are test formulation
#' @param savePlots, logical whether to write the plot files to disc, default is FALSE
#' @noRd
make_NCA_plots <- function(BICS, run_dir, samp_size, nmodels, reference_groups, test_groups, savePlots = FALSE) {
  rval <- tryCatch(
    {
      BICS <- BICS %>%
        dplyr::mutate(Samp_num = dplyr::row_number())
      this_NCAs <- NULL
      all_NCAs <- data.frame(
        ID = as.integer(), treatment = as.character(), period = as.integer(), sequence = as.integer(),
        Cmax = as.numeric(), AUCinf = as.numeric(), AUClast = as.numeric(), model = as.integer()
      )
      this_model <- 1
      for (this_model in 1:nmodels) {
        all_NCAs_this_model <- data.frame(
          ID = as.integer(), treatment = as.character(), period = as.integer(), sequence = as.integer(),
          Cmax = as.numeric(), AUCinf = as.numeric(), AUClast = as.numeric(), model = as.integer()
        )
        # gather all Cmax etc from models labeled by model superimpose distributions
        which_models <- BICS$Best
        # compile list of NCA with best
        this_samp <- which_models[1]
        for (this_samp in which_models) {
          filename <- file.path(run_dir, paste0("MBBEsim", this_samp), paste0("NCAresults", this_samp, ".csv"))
          if (file.exists(filename)) {
            this_NCAs <- utils::read.csv(filename)
            this_NCAs$model <- this_model
            all_NCAs_this_model <- rbind(all_NCAs_this_model, this_NCAs)
          }
        }
        if (dim(all_NCAs_this_model)[1] > 0) {
          all_NCAs <- rbind(all_NCAs, all_NCAs_this_model)
          Cmax_plot <- ggplot2::ggplot(all_NCAs_this_model, ggplot2::aes(x = Cmax), ) +
            ggplot2::geom_histogram(ggplot2::aes(color = treatment, fill = treatment), position = "identity", alpha = 0.4, bins = 30) +
            ggplot2::ggtitle(paste("Cmax, model =", this_model))
          if(savePlots){
            ggplot2::ggsave(file.path(run_dir, paste0("Model_", this_model, "_Cmax_histogram_by_treatment.jpeg")), Cmax_plot, device = "jpeg", width = 9, height = 6)
          }
            AUCinf_plot <- ggplot2::ggplot(all_NCAs_this_model, ggplot2::aes(x = AUCinf), ) +
            ggplot2::geom_histogram(ggplot2::aes(color = treatment, fill = treatment), position = "identity", alpha = 0.4, bins = 30) +
            ggplot2::ggtitle(paste("AUCinf, model =", this_model))
          if(savePlots){
            ggplot2::ggsave(file.path(run_dir, paste0("Model_", this_model, "_AUCinf_histogram_by_treatment.jpeg")), AUCinf_plot, device = "jpeg", width = 9, height = 6)
          }
            AUClast_plot <- ggplot2::ggplot(all_NCAs_this_model, ggplot2::aes(x = AUClast), ) +
            ggplot2::geom_histogram(ggplot2::aes(color = treatment, fill = treatment), position = "identity", alpha = 0.4, bins = 30) +
            ggplot2::ggtitle(paste("AUClast, model =", this_model))
          if(savePlots){
            ggplot2::ggsave(file.path(run_dir, paste0("Model_", this_model, "_AUClast_histogram_by_treatment.jpeg")), AUClast_plot, device = "jpeg", width = 9, height = 6)
          }
        }
      }
      Cmax_plot <- ggplot2::ggplot(all_NCAs, ggplot2::aes(x = Cmax), ) +
        ggplot2::geom_histogram(ggplot2::aes(color = treatment, fill = treatment), position = "identity", alpha = 0.4, bins = 30) +
        ggplot2::ggtitle("Cmax, all Models")
      if(savePlots){
          ggplot2::ggsave(file.path(run_dir, "All_models_Cmax_histogram_by_treatment.jpeg"), Cmax_plot, device = "jpeg", width = 9, height = 6)
      }
         AUCinf_plot <- ggplot2::ggplot(all_NCAs, ggplot2::aes(x = AUCinf), ) +
        ggplot2::geom_histogram(ggplot2::aes(color = treatment, fill = treatment), position = "identity", alpha = 0.4, bins = 30) +
        ggplot2::ggtitle("AUCinf, all Models")
      if(savePlots){
        ggplot2::ggsave(file.path(run_dir, "All_models_AUCinf_histogram_by_treatment.jpeg"), AUCinf_plot, device = "jpeg", width = 9, height = 6)
      }
        AUClast_plot <- ggplot2::ggplot(all_NCAs, ggplot2::aes(x = AUClast), ) +
        ggplot2::geom_histogram(ggplot2::aes(color = treatment, fill = treatment), position = "identity", alpha = 0.4, bins = 30) +
        ggplot2::ggtitle("AUClast, all Models")
        if(savePlots){
           ggplot2::ggsave(file.path(run_dir, "All_models_AUClast_histogram_by_treatment.jpeg"), AUClast_plot, device = "jpeg", width = 9, height = 6)
        }
          },
    error = function(cond) {
      message("Failed in make_NCA_plots, error message = ", cond)
    }
  )
  return()
}

#' run_mbbe_json
#'
#' Runs MBBE from a json file of options e.g., calls `run_mbbe`
#'
#' @param Args.json, path to JSON file with arguments
#' @export
#' @examples
#' \dontrun{
#' run_mbbe_json("Args.json")
#' }
#' @inherit run_mbbe return
run_mbbe_json <- function(Args.json) {
  if (!file.exists(Args.json)) {
    message(Args.json, " file not found, exiting")
  } else {
    user_R_code  <- FALSE # default value
    save_plots <- FALSE
    bypass_check <-  FALSE
    R_code_path <- NULL
    Args <- jsonlite::fromJSON(Args.json)
    all_args <- list(
      Args$crash_value, Args$ngroups, Args$reference_groups, Args$test_groups, Args$num_parallel, Args$samp_size, Args$run_dir, Args$model_source,
      Args$nmfe_path, Args$delta_parms, Args$use_check_identifiable, Args$NCA_end_time, Args$rndseed,
      Args$plan, Args$alpha_error, Args$NTID, Args$model_averaging_by
    )
    if (any(sapply(all_args, is.null))) {
      message(
        "required values in json file are: \n",
        "run_dir \n",
        "model_source \n",
        "num_parallel \n",
        "crash_value \n",
        "nmfe_path \n",
        "delta_parms \n",
        "use_check_identifiable \n",
        "NCA_end_time \n",
        "rndseed \n",
        "ngroups \n",
        "samp_size \n",
        "reference_groups \n",
        "test_groups \n",
        "plan \n",
        "alpha_error \n",
        "NTID \n",
        "model_averaging_by \n",
        "simulation_data_path, path to data used for simulation\n",
        "user_R_code (optional, TRUE|FALSE, default = FALSE) and \n",
        "R_code_path (path to R code, if used, default = NULL) \nexiting"
      )
    } else {

      return <- run_mbbe(
        Args$crash_value,
        Args$ngroups,
        Args$reference_groups,
        Args$test_groups,
        Args$num_parallel,
        Args$samp_size,
        Args$run_dir,
        Args$model_source,
        Args$nmfe_path,
        Args$delta_parms,
        Args$use_check_identifiable,
        Args$NCA_end_time,
        Args$rndseed,
        Args$simulation_data_path,
        Args$plan,
        Args$alpha_error,
        Args$NTID,
        Args$model_averaging_by,
        user_R_code = ifelse(is.null(Args$user_R_code),
                             user_R_code,
                             Args$user_R_code),
        R_code_path = Args$R_code_path,
        save_plots = ifelse(is.null(Args$save_plots),
                            save_plots,
                             Args$save_plots),
        bypass_check = ifelse(is.null(Args$bypass_check),
                              bypass_check,
                              Args$bypass_check)
      )
      file_out <- data.frame(return$Cmax_power, return$AUClast_power, return$AUCinf_power)
      colnames(file_out) <- c("Cmax power", "AUClast power", "AUCinf power")
      write.csv(file_out, file.path(Args$run_dir, paste0("MBBEpower", stringr::str_replace_all(Sys.time(), ":", "-"), ".csv")), row.names = FALSE, quote = FALSE)
      message(paste0(capture.output(return), collapse = "\n"))

    }
  }
}

#' Execute MBBE Analysis
#'
#' This function runs the MBBE analysis. It's typically called by `run_mbbe_json`
#' which provides the necessary options via a JSON file.
#'
#' @param crash_value Numeric. Value to be returned for BIC in models that crash during either bootstrap or simulation.
#' @param ngroups Integer. Number of groups in the simulated data (e.g., an ABBA design has 4 groups).
#' @param reference_groups Numeric vector. Indices of the groups representing the reference formulation (e.g., c(2,3) for an ABBA design).
#' @param test_groups Numeric vector. Indices of the groups representing the test formulation (e.g., c(1,4) for an ABBA design).
#' @param num_parallel Integer. Number of NONMEM processes (both bootstrap and simulation) to run concurrently.
#' @param samp_size Integer. Size of the bootstrap and simulation samples.
#' @param run_dir Character string. Directory for NONMEM execution.
#' @param model_source Character string. Paths to the NONMEM control files for model averaging.
#' @param nmfe_path Character string. Path to the nmfe executable.
#' @param delta_parms Numeric. Parameter difference threshold defining identifiability.
#' @param use_check_identifiable Logical. Should identifiability be checked based on the criterion defined by [Aoki](https://www.page-meeting.org/default.asp?abstract=5951)?
#' @param NCA_end_time Numeric. The NCA calculation will start at 0 and end at this value.
#' @param rndseed Integer. Random seed for reproducibility.
#' @param simulation_data_path Character string. Path to the simulation dataset.
#' @param plan Character string (default: "multisession"). Parallel execution plan. Can be "multisession", "sequential", or "multicore".
#' @param alpha_error Numeric (default: 0.05). Alpha error rate for statistical tests.
#' @param NTID Logical (default: FALSE). Is the drug a narrow therapeutic index drug?
#' @param model_averaging_by Character string (default: "study"). Method of model averaging, either "study" or "subject".
#' @param user_R_code Logical (default: FALSE). Should custom R code be used for model penalty?
#' @param R_code_path Character string. If `user_R_code` is TRUE, this parameter defines the path to the custom R script.
#' @param save_plots Logical (default: \code{FALSE}). Set to \code{TRUE} to save plot output.
#' @param ... Additional args
#'
#' @details
#' This function is primarily intended to be called by `run_mbbe_json`, which provides input parameters through a JSON configuration.
#'
#' @return A list containing:
#' - `Cmax_power`: Power for Cmax
#' - `AUClast_power`: Power for AUClast
#' - `AUCinf_power`: Power for AUCinf
#' - `run_dir`: Directory where the function was executed
#' - `Num_identifiable`: Number of identifiable parameters
#' - `BICS`: Bayesian Information Criterion Scores
#'
#' @export
run_mbbe <- function(crash_value,
                     ngroups,
                     reference_groups,
                     test_groups,
                     num_parallel,
                     samp_size,
                     run_dir,
                     model_source,
                     nmfe_path,
                     delta_parms,
                     use_check_identifiable,
                     NCA_end_time,
                     rndseed,
                     simulation_data_path,
                     plan = c("multisession", "sequential", "multicore"),
                     alpha_error = 0.05,
                     NTID = FALSE,
                     model_averaging_by = "study",
                     user_R_code = FALSE,
                     R_code_path = "",
                     save_plots = FALSE,
                     ...) {

  tictoc::tic()

  plan <- match.arg(plan)

  oldOptions <- options()
  oldPlan <- future::plan()
  on.exit(options(oldOptions), add = TRUE)
  on.exit(future::plan(oldPlan), add = TRUE)

  if (plan == "multisession") {
    future::plan(future::multisession, workers = num_parallel)
  } else if (plan == "multicore") {
    future::plan(future::multicore, workers = num_parallel)
  } else if (plan == "sequential") {
    future::plan(future::sequential)
  }


  message(format(Sys.time(), digits = 0), " Start time\nModel file(s) =")
  for(this_model in model_source){message(this_model)}
  message("reference groups = ", toString(reference_groups),
            "\ntest groups = ", toString(test_groups))
  message(paste("Run Directory =", run_dir))
  message(paste("Number of groups =", ngroups))
  if (!model_averaging_by %in% c("study", "subject")) {
    stop("Error, model_averaging is ",model_averaging_by, " model_averaging_by must be one of study or subject, exiting")
  } else {
    message("Model averaging will be by ", model_averaging_by)
  }
  message("Bootstrap/Monte Carlo sample size = ", samp_size, "\nnmfe??.bat path = ", nmfe_path, "\nUse_check_identifiability = ", use_check_identifiable)
  message("Narrow Therapeutic Index  = ", NTID)
  message("Alpha error rate for bioequilvalence testing = ", alpha_error)
  message("Number parallel runs for bootstrap, simulations and NCA = ", num_parallel)
  if (use_check_identifiable) {
    message("Delta parameter for use_check_identifiable = ", delta_parms)
  }

  if(file.exists(simulation_data_path)){
    message("Simulation data path = ", simulation_data_path)
  }else{
    stop("Cannot find", simulation_data_path )
  }
  if(user_R_code){
    message("post run R code for model averaging selection = ", user_R_code)
    if(file.exists(R_code_path)){
     message("R_code_path = ", R_code_path)
    } else{
      stop("Cannot find ", R_code_path )
    }
    }else{
      message("Not using post run R code for model averaging selection")
       }

  message("BICs values will be written to ", file.path(run_dir,"BIC.csv"))
  message("Total Model averaging penalties will be written to ", file.path(run_dir,"Total_penalties.csv"))
  set.seed(rndseed)

  additional_args <- list(...)
  if (!is.null(additional_args$bypass_check) && additional_args$bypass_check) {
    msg <- list(rval=TRUE)
  } else {
  msg <- check_requirements(
    run_dir, samp_size, model_source, ngroups,
    reference_groups, test_groups, nmfe_path,
    use_check_identifiable,
    simulation_data_path,
    user_R_code,
    R_code_path
  )
  }
  if (msg$rval) {
    message(
      "Passed requirements check\nCopying source control files from ", toString(model_source), " to ", file.path(run_dir, "modelN"),
      "\n where N is the model number"
    )
    nmodels <- copy_model_files(model_source, run_dir)

    if (nmodels > 0) {
      message(format(Sys.time(), digits = 0), " Sampling data 1-", samp_size, " writing data to ", file.path(run_dir, "data_sampM.csv"), " where M is the bootstrap sample number")
      sample_data(run_dir, nmodels, samp_size)
      message(format(Sys.time(), digits = 0), " Starting bootstrap runs 1-", samp_size, " in ", file.path(run_dir, "modelN", "M"), " where N is the model number and M is the sample number")
      if (!run_any_models(nmfe_path, run_dir, nmodels, samp_size, num_parallel, TRUE)) {
        message("Failed bootstrap")
      } else {
        # need to wait until all are done, this returns when all are started.

        message("\n", format(Sys.time(), digits = 0), " Getting bootstrap model parameters, samples 1-", samp_size)
        if(user_R_code){
          message("and executing user provided R code from ", R_code_path)
        }
        parms <- get_parameters(
          run_dir, nmodels,
          samp_size, delta_parms,
          crash_value, use_check_identifiable,
          user_R_code, R_code_path
        )

        }


        base_models <- get_base_model(run_dir, nmodels) # get all nmodels base model
        message(format(Sys.time(), digits = 0), " Constructing simulation  models in ", file.path(run_dir, "MBBEsimM"), " where M is the simulation number")
        final_models <- write_sim_controls(run_dir, parms, base_models, samp_size, simulation_data_path) # don't really do anything with final models, already written to disc
        message(format(Sys.time(), digits = 0), " Running simulation models 1-", samp_size, " in ", file.path(run_dir, "MBBEsimM"), " where M is the simulation number")
        if (run_any_models(nmfe_path, run_dir, NULL, samp_size, num_parallel, FALSE)) {
          message("\n", format(Sys.time(), digits = 0), " Calculating NCA parameters for simulations 1-", samp_size, ", writing to ", file.path(run_dir, "MBBEsimM", "NCAresultsM.csv"), ",  where M is the simulation number")
          if (model_averaging_by == "subject") {
            message("\n Note the NCA parameters are by study, prior random selection by subject for model averaging")
          }

          # note would like to do NCA parallel, so don't end
          calc_NCA(
            run_dir,
            ngroups,
            reference_groups,
            test_groups,
            NCA_end_time,
            samp_size
          )
          message(format(Sys.time(), digits = 0), " Done calculating NCA parameters, making plots")

          make_NCA_plots(
            parms$BICS,
            run_dir,
            samp_size,
            nmodels,
            reference_groups,
            test_groups,
            savePlots = save_plots
          )

          message(format(Sys.time(), digits = 0), " Plots are saved in ", run_dir)
          message(format(Sys.time(), digits = 0), " Calculating power")
          # read NCA output and do stats
          all_results <- calc_power(run_dir, samp_size,
                                    alpha = alpha_error,
                                    model_averaging_by, NTID = NTID
          )
        } else {
          all_results <- NULL
        }
        if (!is.null(all_results)) {
          output_file <- file.path(run_dir, "All_results.csv")
          count <- 0
          while (file.exists(output_file) & count < 20) {
            count <- count + 1
            file.remove(output_file)
            Sys.sleep(0.25)
          }
          count <- 0
          while (!file.exists(output_file) & count < 20) {
            write.csv(all_results, file = output_file, quote = FALSE)
            count <- count + 1
            Sys.sleep(0.25)
          }
          if (!file.exists(output_file)) {
            message("Unable to write to ", output_file)
          }
          Cmax_power <- all_results %>%
            dplyr::filter(Cmax_BE != -999) %>%
            dplyr::summarise(Power = mean(Cmax_BE))
          AUClast_power <- all_results %>%
            dplyr::filter(AUClast_BE != -999) %>%
            dplyr::summarise(Power = mean(AUClast_BE))
          AUCinf_power <- all_results %>%
            dplyr::filter(AUCinf_BE != -999) %>%
            dplyr::summarise(Power = mean(AUCinf_BE))
          power <- c(Cmax_power, AUCinf_power, AUClast_power)
          #write.csv(power, file.path(run_dir, "Power.csv"))
        } else {
          message("Failed in calc_power")
          message(msg$msg, " exiting")
          Cmax_power <- NULL
          AUClast_power <- NULL
          AUCinf_power <- NULL
        }
      }
  } else {
    message(msg$msg, " exiting")
    Cmax_power <- NULL
    AUClast_power <- NULL
    AUCinf_power <- NULL
  }
  NCA_stats <- all_results %>% dplyr::select(Cmax_BE, Cmax_Ratio ,
                                             Cmax_lower_CL, Cmax_upper_CL,
                                             AUClast_BE, AUClast_Ratio,
                                             AUClast_lower_CL,AUClast_upper_CL,
                                             AUCinf_BE, AUCinf_Ratio,
                                             AUCinf_lower_CL, AUCinf_upper_CL)
  tictoc::toc()
  message("Done at ", Sys.time())
  return(
    list(
      "Cmax_power" = Cmax_power,
      "AUClast_power" = AUClast_power,
      "AUCinf_power" = AUCinf_power,
      "run_dir" = run_dir,
      "Num_identifiable" = parms$passes_identifiable,
      "NCA_stats" = NCA_stats
    )
  )
}


