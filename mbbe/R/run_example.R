
#'  run_example for Model-Based BE Assessment
#'
#' This function calls the example models (model1-5.mod), performs the bootstrap, model averaging and the Monte Carlo simulation.
#'
#' @param run_dir Character string specifying the directory containing the parent folder where the models are to be run.
#' @param nmfe_path Character string indicating the path to the nmfe batch file (e.g., nmfe?.bat).
#' @param Include_R_Code Logical, whether the include the code in R_Penalty_Code in model averaging algorithm, Default is FALSE
#' @param plan for future execution, one of "sequential", "multisession","multicore", Default is multisession
#' @importFrom jsonlite toJSON
#' @details The function executes the mbbe::run_mbbe_json() function. A user supplied installation of NONMEM is required
#'  run_dir is the parent folder where the models are to be run, nmfe_path is the path the nmfe??.bat where ?? is the version of NONMEM available
#'  plan is "sequential", "multisession","multicore", defining the plan for parallel execution (sequential is non-parallel execution)
#'  The function uses the included file mbbeargs.json as the options file for the run, and runs 5 supplied models for model averaging.
#'  Monte Carlo Simulation is then done, with the number of samples set in the mbbearg.json file, to 10 (probaby more would be appropriate for
#'  and actual power analysis)
#'  The model selection for the model averaging also includes a penalty calculate by the script RPenaltyCode.r for missing Cmax, AUCinf and AUClast
#'  Run time on 32 cores is ~3 minutes without the R code execution an 10 minutes with
#'  and the output should include:
#' @examples
#' \dontrun{
#' run_dir <- tempdir()
#' mbbe::run_example(run_dir = run_dir,
#'   nmfe_path = "c:/nm74g64/util/nmfe74.bat",
#'   plan = "multisession")
#' }
#' @inherit run_mbbe return
#' @export
#'
run_example <- function(run_dir, nmfe_path, Include_R_Code = FALSE, plan = "multisession") {
  if(!file.exists(nmfe_path)){
    stop("Path to nmfe??.bat (", nmfe_path,") not found, exiting")

  }
  message("if ", run_dir," exists, a dialog will appear asking permission to remove it, it may not be on top")
  if(dir.exists(run_dir)){
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
    dir.create(run_dir, recursive = TRUE)
  }

  n_cores <- future::availableCores() - 1
  if(n_cores == 0){n_cores <- 1}
  if(Include_R_Code) {
    base_ET <- 500
    message("R code in ", file.path(run_dir,"RPenaltyCode.R")," will be used to calculate additional penalty for Model averaging")
  }else{
    base_ET <- 200
    message("No additional penalty for model averaging will be used")
  }
  message("MBBE will be run ", n_cores," x parallel")
  message("Estimated run time for example = ", round(base_ET*0.5/n_cores, 0)," minutes")

  mbbe_example_dir <- system.file(package = "mbbe", "examples")
  mbbe_example_files <- list.files(mbbe_example_dir, full.names = TRUE)
  # do not copy folder e.g., nca_results
  mbbe_example_files <- mbbe_example_files[!file.info(mbbe_example_files)$isdir]
  # copy .mod files and data
  lapply(mbbe_example_files, function(x) {
    if (!file.copy(x, file.path(run_dir, basename(x)))) {
      stop("Could not copy ", x, " to ", file.path(run_dir, basename(x)))
    }
  })


  model_source_files <- list.files(run_dir, full.names = TRUE, pattern = "\\.mod$")
  # Do we use all files or a subset? e.g., let's use the first two
  #model_source_files <- model_source_files[1:2]
  data_file <- file.path(run_dir, "data.csv")
  sim_data_file <- file.path(run_dir, "data_sim.csv")


  for(model_path in model_source_files){
    model_file <- suppressWarnings(readLines(model_path))
    model_file[4] <- sub("\\$DATA[[:space:]]*(.*?)[[:space:]]*IGNORE=@",
                         paste0("$DATA ", gsub("\\\\", "\\\\\\\\", data_file), " IGNORE=@"),
                         model_file[4])
    con <- file(model_path, "w")
    writeLines(model_file, con)
    close(con)
  }
# write json
  if(Include_R_Code){
    user_R_code = TRUE
    R_code_path = file.path(run_dir,"RPenaltyCode.R")
  }else{
    user_R_code = FALSE
    R_code_path = ""
  }
  args <- c(
    crash_value = 999999,
    run_dir = file.path(run_dir, "example"),
    ngroups = 4,
    reference_groups = list(c(1, 2)),
    test_groups = list(c(3, 4)),
    num_parallel = unname(n_cores),
    samp_size = 5,
    model_source = list(model_source_files),
    nmfe_path = nmfe_path,
    delta_parms = 0.1,
    use_check_identifiable = FALSE,
    NCA_end_time = 72,
    rndseed = 1,
    simulation_data_path = sim_data_file,
    plan = plan,
    alpha_error = 0.05,
    NTID = FALSE,
    model_averaging_by = "study",
    user_R_code = user_R_code,
    R_code_path = R_code_path,
    save_plots = TRUE,
    bypass_check = TRUE
    )
  exportJson <- jsonlite::toJSON(args)
  write(exportJson, file.path(run_dir,"example.json"))
  message("Example mbbe arguments file is at ",file.path(run_dir,"example.json") )
  run_mbbe_json(file.path(run_dir,"example.json"))

}
