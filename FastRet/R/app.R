#' @title Start the FastRet GUI
#' @description Starts the FastRet GUI
#' @param port The port the application should listen on
#' @param host The address the application should listen on
#' @param reload Whether to reload the application when the source code changes
#' @param nw The number of worker processes started. The first worker always listens for user input from the GUI. The other workers are used for handling long running tasks like model fitting or clustering. If `nw` is 1, the same process is used for both tasks, which means that the GUI will become unresponsive during long running tasks.
#' @param nsw The number of subworkers each worker is allowed to start. The higher this number, the faster individual tasks like model fitting can be processed. A value of 1 means that all subprocesses will run sequentially.
#' @return A shiny app. This function returns a shiny app that can be run to interact with the model.
#' @details If you set `nw = 3` and `nsw = 4`, you should have at least 16 cores, one core for the shiny main process. Three cores for the three worker processes. And 12 cores (3 * 4) for the subworkers. For the default case, `nworkers = 1` and `nsw = 2`, you should have at least 4 cores.
#' @keywords public
#' @examples
#' if (interactive()) start_gui()
#' @export
start_gui <- function(port = 8080,
                      host = "0.0.0.0",
                      reload = FALSE,
                      nw = 2,
                      nsw = 1) {
    catf("Checking CDK version")
    check_cdk_version()
    oldplan <- future::plan("multisession", workers = nw)
    on.exit(future::plan(oldplan), add = TRUE)
    catf("Starting FastRet GUI")
    app <- fastret_app(port, host, reload, nsw)
    runApp(app)
}

mocklist <- c("inpFRM", "inpDf", "adjDf", "btnTrain", "cluster_calc", "tiPredSmiles", "getCDs", "preprocess_data", "train_frm", "selective_measuring")
strategies <- c("sequential", "multicore", "multisession")
startModes <- c("Train new Model", "Predict Retention Times", "Selective Measuring", "Adjust existing Model")

#' @title Start the FastRet GUI in development mode
#' @description Starts the FastRet GUI in development mode
#' @param strategy The strategy to use for parallel processing. Can be one of "sequential", "multicore", "multisession"
#' @param mocks A character vector of mocks to be used. The following mocks are available:
#' * Shiny mocks
#'   * `inpFRM`: inits `RV$ubInpFRM`
#'   * `inpDf`: inits `RV$inpDf`
#'   * `adjDf`: inits `RV$adjDf`
#'   * `btnTrain`: triggers `SE$ABH$btnTrain`
#'   * `cluster_calc`: TODO
#'   * `tiPredSmiles`: TODO
#' * Functions mocks
#'   * `getCDs`: mocks [getCDs()]
#'   * `preprocess_data`: mocks [preprocess_data()]
#'   * `train_frm`: mocks [train_frm()]
#'   * `selective_measuring`: mocks [selective_measuring()]
#' @param startMode The start mode to use. Can be one of "Train new Model", "Predict Retention Times", "Selective Measuring", "Adjust existing Model"
#' @return NULL. Called for side effects.
#' @details By using no subworkers and multicore or sequential, we can ensure that all processes are forked from the current R session and therefore use the functions loaded via devtools. If we use multisession and or subworkers, these processes will use the installed version of FastRet instead. ==> If we work on the UI part, we can use multisession and/or subworkers, because the UI part is handled by the main process, BUT, If we develop train/predict/plot functions, we must use multicore or sequential and NO subworkers! In particular, to use `browser()` in these functions, we must use sequential.
#' @keywords internal
#' @noRd
start_gui_in_devmode <- function(strategy = "sequential",
                                 mocks = mocklist,
                                 startMode = "Train new Model") {

    catf("Checking args")
    startMode <- match.arg(startMode, startModes)
    strategy <- match.arg(strategy, strategies)
    if (!all(mocks %in% mocklist)) stop("mocks must be a subset of: ", paste(mocklist, collapse = ", "))

    catf("Patching shiny and pkgload")
    patch_file <- pkg_file("misc/scripts/patch-shiny.R")
    if (file.exists(patch_file)) { # I.e. the package is loaded from source via `devtools::load_all()`.
        patch_env <- new.env()
        source(patch_file, local = patch_env, echo = FALSE)
        patch_env$patch_shiny()
        patch_env$patch_pkgload()
    } else { # I.e. the package was installed and loaded e.g. via `library(FastRet)`. This code part shouldn't be reached, because the function is not exported, but we print a message just in case.
        catf("No patch file found. Autoreload will not work.")
    }


    catf("Reloading FastRet")
    devtools::load_all() # needs to be called once with updated function

    catf("Setting development options")
    opts <- options(shiny.autoreload = TRUE, FastRet.mocks = mocks, FastRet.UI.startMode = startMode, warn = 1)
    on.exit(expr = {catf("Resetting development options"); options(opts)}, add = TRUE)

    catf("Initializing cluster")
    oldplan <- future::plan(strategy)
    on.exit(expr = {catf("Deleting cluster"); future::plan(oldplan)}, add = TRUE, after = FALSE)

    catf("Starting FastRet GUI in development mode")
    pkg_root <- dirname(system.file("DESCRIPTION", package = "FastRet"))
    shiny::with_devmode(TRUE, shiny::runApp(pkg_root), verbose = TRUE)
}

#' @title The FastRet GUI
#' @description This function creates the FastRet GUI
#' @param port The port the application should listen on
#' @param host The address the application should listen on
#' @param reload Whether to reload the application when the source code changes
#' @param nsw The number of subworkers each worker is allowed to start. The higher this number, the faster individual tasks like model fitting can be processed.
#' @return A shiny app. This function returns a shiny app that can be run to interact with the model.
#' @examples
#' x <- fastret_app()
#' if (interactive()) shiny::runApp(x)
#' @keywords public
#' @return An object of class `shiny.appobj`.
#' @export
fastret_app <- function(port = 8080,
                        host = "0.0.0.0",
                        reload = FALSE,
                        nsw = 0) {
    shinyApp(
        ui = function(req) fastret_ui(req),
        server = function(input, output, session) fastret_server(input, output, session, nsw),
        options = list(port = port, host = host, quiet = TRUE, launch.browser = FALSE, reload = reload),
        onStart = function() catf("Listening on http://localhost:%s", port)
    )
}

check_cdk_version <- function() {
    if (rcdk::cdk.version() != "2.9") {
        msg <- paste(
            sep = "\n",
            "FastRet requires CDK Version 2.9, but the installed version is %s.",
            "For details about the installation process see https://github.com/CDK-R/rcdklibs."
        )
        msgf <- sprintf(msg, rcdk::cdk.version())
        stop(msgf, call. = FALSE)
    }
}
