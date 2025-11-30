# TMB Model Compilation and Loading System for gkwreg Package
# 
# Simple and robust TMB model compilation with caching.
# Critical: Never unloads TMB model DLLs to avoid finalization errors.

# Internal package environment for DLL state management
.gkwreg_env <- new.env(parent = emptyenv())


#' Check and Compile TMB Model Code
#'
#' @description
#' Ensures a TMB model is compiled and loaded. Uses persistent cache to avoid
#' recompilation across sessions.
#'
#' @param dll_name Character, base name of the C++ file (e.g., "kwreg", "ekwreg")
#' @param pkg_name Character, package name (default: "gkwreg")
#' @param force_recompile Logical, force recompilation (default: FALSE)
#' @param verbose Logical, print diagnostic messages (default: FALSE)
#'
#' @return Invisibly returns a list with DLL information
#'
#' @importFrom TMB compile dynlib
#' @keywords internal
.check_and_compile_TMB_code <- function(dll_name,
                                        pkg_name = "gkwreg",
                                        force_recompile = FALSE,
                                        verbose = FALSE) {
  
  log_msg <- function(...) if (verbose) message("[TMB] ", ...)
  
  # Check session cache first
  cache_key <- paste0("TMB_", dll_name, "_loaded")
  
  if (!force_recompile && isTRUE(get0(cache_key, envir = .gkwreg_env, ifnotfound = FALSE))) {
    if (.is_dll_functional(dll_name, verbose)) {
      log_msg("Using already loaded model: ", dll_name)
      return(invisible(list(name = dll_name, cached = TRUE)))
    }
  }
  
  log_msg("Preparing TMB model: ", dll_name)
  
  # Get source file path
  cpp_file <- system.file("tmb", paste0(dll_name, ".cpp"), package = pkg_name)
  if (cpp_file == "" || !file.exists(cpp_file)) {
    stop("TMB source file not found: inst/tmb/", dll_name, ".cpp")
  }
  
  # Setup cache directory
  cache_dir <- .get_cache_dir(pkg_name)
  dll_ext <- .Platform$dynlib.ext
  cached_dll <- file.path(cache_dir, paste0(dll_name, dll_ext))
  
  # Determine if compilation needed
  need_compile <- force_recompile || 
    !file.exists(cached_dll) ||
    file.info(cpp_file)$mtime > file.info(cached_dll)$mtime
  
  if (!need_compile) {
    # Try loading cached DLL
    if (.load_dll(cached_dll, dll_name, verbose)) {
      assign(cache_key, TRUE, envir = .gkwreg_env)
      log_msg("Loaded cached DLL: ", dll_name)
      return(invisible(list(name = dll_name, path = cached_dll, compiled = FALSE)))
    } else {
      log_msg("Cached DLL failed to load, recompiling...")
      need_compile <- TRUE
    }
  }
  
  # Compile if necessary
  if (need_compile) {
    compiled_dll <- .compile_model(cpp_file, dll_name, verbose)
    
    # Load the compiled DLL
    if (!.load_dll(compiled_dll, dll_name, verbose)) {
      stop("Failed to load freshly compiled DLL: ", dll_name)
    }
    
    # Copy to cache
    if (!file.copy(compiled_dll, cached_dll, overwrite = TRUE)) {
      warning("Failed to cache DLL (will need recompilation next session)")
    }
    
    # Clean up temp directory
    unlink(dirname(compiled_dll), recursive = TRUE, force = TRUE)
    
    log_msg("Compiled and loaded: ", dll_name)
  }
  
  # Mark as loaded in session cache
  assign(cache_key, TRUE, envir = .gkwreg_env)
  
  invisible(list(
    name = dll_name,
    path = cached_dll,
    compiled = need_compile,
    timestamp = Sys.time()
  ))
}


#' Load TMB DLL
#'
#' @description
#' Loads a TMB DLL and registers it with TMB's symbol table.
#' CRITICAL: Never calls dyn.unload() to avoid finalization errors.
#'
#' @param dll_path Full path to DLL file
#' @param dll_name Base name of DLL
#' @param verbose Logical
#'
#' @return Logical, TRUE if successful
#'
#' @keywords internal
.load_dll <- function(dll_path, dll_name, verbose = FALSE) {
  
  log_msg <- function(...) if (verbose) message("[TMB] ", ...)
  
  if (!file.exists(dll_path)) {
    log_msg("DLL not found: ", dll_path)
    return(FALSE)
  }
  
  # Check if already loaded and functional
  if (.is_dll_functional(dll_name, verbose)) {
    log_msg("DLL already loaded: ", dll_name)
    return(TRUE)
  }
  
  # Load binary
  load_success <- tryCatch({
    dyn.load(dll_path)
    log_msg("Loaded DLL: ", basename(dll_path))
    TRUE
  }, error = function(e) {
    log_msg("dyn.load failed: ", conditionMessage(e))
    FALSE
  })
  
  if (!load_success) return(FALSE)
  
  # Register with TMB
  register_success <- tryCatch({
    tmb_path <- TMB::dynlib(dll_name)
    !is.null(tmb_path)
  }, error = function(e) {
    log_msg("TMB registration failed: ", conditionMessage(e))
    FALSE
  })
  
  if (!register_success) {
    log_msg("Warning: DLL loaded but TMB registration uncertain")
  }
  
  TRUE
}


#' Check if DLL is Loaded and Functional
#'
#' @param dll_name Base name of DLL
#' @param verbose Logical
#'
#' @return Logical
#'
#' @keywords internal
.is_dll_functional <- function(dll_name, verbose = FALSE) {
  
  # Check if in R's DLL table
  loaded_dlls <- getLoadedDLLs()
  dll_loaded <- any(sapply(loaded_dlls, function(dll) {
    grepl(dll_name, dll[["name"]], fixed = TRUE)
  }))
  
  if (!dll_loaded) return(FALSE)
  
  # Check TMB can find it
  tmb_ok <- tryCatch({
    test_dll <- TMB::dynlib(dll_name)
    !is.null(test_dll) && file.exists(test_dll)
  }, error = function(e) FALSE)
  
  tmb_ok
}


#' Compile TMB Model
#'
#' @description
#' Compiles a TMB C++ template in a temporary directory.
#'
#' @param cpp_file Path to source file
#' @param dll_name Base name for output
#' @param verbose Logical
#'
#' @return Path to compiled DLL
#'
#' @keywords internal
.compile_model <- function(cpp_file, dll_name, verbose = FALSE) {
  
  log_msg <- function(...) if (verbose) message("[TMB] ", ...)
  
  # Create temporary compilation directory
  temp_dir <- file.path(
    tempdir(),
    paste0(dll_name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  )
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Copy source to temp directory
  temp_cpp <- file.path(temp_dir, basename(cpp_file))
  if (!file.copy(cpp_file, temp_cpp, overwrite = TRUE)) {
    stop("Failed to copy source file to temp directory")
  }
  
  # Compile in temp directory
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(temp_dir)
  
  log_msg("Compiling ", dll_name, "...")
  
  compile_ok <- tryCatch({
    TMB::compile(
      file = basename(temp_cpp),
      safebounds = FALSE,
      safeunload = FALSE,
      verbose = verbose
    )
    TRUE
  }, error = function(e) {
    stop("TMB compilation failed: ", conditionMessage(e))
  })
  
  # Locate compiled DLL
  dll_ext <- .Platform$dynlib.ext
  expected_dll <- TMB::dynlib(dll_name)
  
  if (file.exists(expected_dll)) {
    dll_path <- expected_dll
  } else {
    # Search for any DLL in temp directory
    dll_files <- list.files(temp_dir, pattern = paste0("\\", dll_ext, "$"), full.names = TRUE)
    
    if (length(dll_files) == 0) {
      stop("Compilation succeeded but no DLL file found")
    }
    
    # Prefer DLL matching dll_name
    matches <- dll_files[grepl(dll_name, basename(dll_files), fixed = TRUE)]
    dll_path <- if (length(matches) > 0) matches[1] else dll_files[1]
  }
  
  if (!file.exists(dll_path)) {
    stop("Compiled DLL not found: ", dll_path)
  }
  
  log_msg("Compilation successful: ", basename(dll_path))
  normalizePath(dll_path, mustWork = TRUE)
}


#' Get Cache Directory for Compiled Models
#'
#' @description
#' Creates version-specific cache directory for TMB DLLs.
#'
#' @param pkg_name Package name
#'
#' @return Path to cache directory
#'
#' @keywords internal
.get_cache_dir <- function(pkg_name) {
  
  # Determine cache root
  if (requireNamespace("rappdirs", quietly = TRUE)) {
    cache_root <- rappdirs::user_cache_dir(pkg_name)
  } else {
    if (.Platform$OS.type == "windows") {
      base <- Sys.getenv("LOCALAPPDATA", Sys.getenv("TEMP"))
    } else {
      base <- Sys.getenv("XDG_CACHE_HOME", "~/.cache")
    }
    cache_root <- file.path(base, pkg_name, "tmb_cache")
  }
  
  # Add R version and package version subdirectories
  r_version <- paste(R.version$major, strsplit(R.version$minor, "\\.")[[1]][1], sep = ".")
  pkg_version <- tryCatch(
    as.character(packageVersion(pkg_name)),
    error = function(e) "dev"
  )
  
  cache_dir <- file.path(cache_root, paste0("R", r_version), pkg_version)
  
  # Create if needed
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  normalizePath(cache_dir, mustWork = FALSE)
}


#' Clean TMB Cache
#'
#' @description
#' Removes all cached compiled TMB models. Useful for troubleshooting or
#' after package updates.
#'
#' @param pkg_name Package name (default: "gkwreg")
#' @param verbose Logical
#'
#' @return Invisibly returns number of files deleted
#'
#' @export
clean_tmb_cache <- function(pkg_name = "gkwreg", verbose = TRUE) {
  
  cache_dir <- .get_cache_dir(pkg_name)
  
  if (!dir.exists(cache_dir)) {
    if (verbose) message("No cache directory found")
    return(invisible(0))
  }
  
  dll_ext <- .Platform$dynlib.ext
  dll_files <- list.files(cache_dir, pattern = paste0("\\", dll_ext, "$"), full.names = TRUE)
  
  if (length(dll_files) == 0) {
    if (verbose) message("Cache directory is empty")
    return(invisible(0))
  }
  
  n_deleted <- 0
  for (dll in dll_files) {
    if (file.remove(dll)) {
      n_deleted <- n_deleted + 1
      if (verbose) message("Deleted: ", basename(dll))
    }
  }
  
  if (verbose) message("Cleaned ", n_deleted, " cached DLL(s)")
  invisible(n_deleted)
}


#' List Available TMB Models
#'
#' @description
#' Returns vector of TMB model names available in the package.
#'
#' @param pkg_name Package name (default: "gkwreg")
#'
#' @return Character vector of model names (without .cpp extension)
#'
#' @export
list_tmb_models <- function(pkg_name = "gkwreg") {
  
  tmb_dir <- system.file("tmb", package = pkg_name)
  
  if (tmb_dir == "" || !dir.exists(tmb_dir)) {
    return(character(0))
  }
  
  cpp_files <- list.files(tmb_dir, pattern = "\\.cpp$")
  tools::file_path_sans_ext(cpp_files)
}


#' Precompile All TMB Models
#'
#' @description
#' Compiles and caches all TMB models found in inst/tmb/.
#' Useful to run after package installation to avoid compilation delays
#' during first use.
#'
#' @param pkg_name Package name (default: "gkwreg")
#' @param verbose Logical
#'
#' @return Invisibly returns list of compilation results
#'
#' @export
precompile_tmb_models <- function(pkg_name = "gkwreg", verbose = TRUE) {
  
  models <- list_tmb_models(pkg_name)
  
  if (length(models) == 0) {
    if (verbose) message("No TMB models found")
    return(invisible(list()))
  }
  
  if (verbose) {
    message("Precompiling ", length(models), " TMB model(s)...")
  }
  
  results <- list()
  
  for (model in models) {
    if (verbose) message("  - ", model)
    
    results[[model]] <- tryCatch({
      .check_and_compile_TMB_code(
        dll_name = model,
        pkg_name = pkg_name,
        force_recompile = FALSE,
        verbose = verbose
      )
      list(success = TRUE)
    }, error = function(e) {
      if (verbose) message("    Failed: ", conditionMessage(e))
      list(success = FALSE, error = conditionMessage(e))
    })
  }
  
  if (verbose) {
    n_success <- sum(sapply(results, function(x) isTRUE(x$success)))
    message("Successfully compiled ", n_success, "/", length(models), " model(s)")
  }
  
  invisible(results)
}
