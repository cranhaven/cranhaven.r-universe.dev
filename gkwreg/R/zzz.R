utils::globalVariables(c(
  "packageVersion", "dev.interactive", "calculateMean", "index", "theoretical", "observed", "lower",
  "upper", "log", "log_scale", "calculateResponseResiduals", "::", ":::", ".gkwreg_env", "get_tmb_info",
  "x", "Theoretical", "Empirical", "value", "loglik", "cook_dist", "fitted", "abs_resid", "leverage", "y_obs",
  "linpred", "resid", "model_label", "metric", "y", "Type", "Deviation", "object", "p_empirical", "p_theoretical",
  "statistic", "type", "Residual", "Family", "Value", "Criterion", "Parameter"
))

# Tem Env for TMB compile
.gkwreg_env <- new.env(parent = emptyenv())


#' Check and Compile TMB Model Code with Persistent Cache
#'
#' @description
#' This utility function verifies whether the TMB model shared object (\code{.so/.dll}) file
#' has already been compiled for a specified DLL name. If not, it compiles the
#' corresponding C++ file and caches it in a persistent directory across R sessions.
#'
#' @details
#' The function works through the following steps:
#' \enumerate{
#'   \item Creates a persistent cache directory for storing compiled TMB models.
#'   \item Checks if a compiled file for the specified DLL already exists in the
#'         cache directory and whether it's up-to-date compared to the source code.
#'   \item If a valid compiled file exists, it loads it directly.
#'   \item If not, the function locates the corresponding C++ file inside the
#'         package, compiles it, and stores the result in the cache directory.
#'   \item Provides diagnostic messages regarding compilation status and exported symbols.
#' }
#'
#' @param dll_name A character string specifying the base name of the C++ file
#'   and the resulting DLL. The function assumes the code file is \code{dll_name.cpp}
#'   located in the \code{inst/tmb/} directory of the package.
#' @param pkg_name A character string specifying the package name. Defaults to "gkwreg".
#' @param force_recompile Logical; if \code{TRUE}, forces recompilation even if
#'   a valid compiled file exists (default is \code{FALSE}).
#' @param verbose Logical; if \code{TRUE}, prints detailed status messages
#'   (default is \code{TRUE}).
#'
#' @return
#' Returns (invisibly) a list with information about the compiled model, including path,
#' normalized path, name, and compilation status.
#' If any step fails, an error is thrown.
#'
#' @importFrom TMB compile dynlib
#' @importFrom tools file_path_sans_ext
#' @keywords internal
.check_and_compile_TMB_code <- function(dll_name,
                                        pkg_name = "gkwreg",
                                        force_recompile = FALSE,
                                        verbose = FALSE) {
  # Helper function for logging
  log_msg <- function(...) {
    if (verbose) cat(...)
  }

  if (verbose) log_msg("Checking TMB model status for ", dll_name, "...\n")

  # # Ensure .gkwreg_env exists
  # if (!exists(".gkwreg_env", envir = .gkwreg_env)) {
  #   assign(".gkwreg_env", new.env(parent = emptyenv()), envir = .gkwreg_env)
  # }

  # 1. Create a persistent cache directory for TMB compilation
  if (requireNamespace("rappdirs", quietly = TRUE)) {
    cache_dir <- file.path(rappdirs::user_cache_dir(pkg_name), "tmb_cache")
  } else {
    # Fallback to a directory in the user's home folder
    cache_dir <- file.path(path.expand("~"), ".R", paste0(pkg_name, "_tmb_cache"))
  }

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    if (verbose) log_msg("Created persistent cache directory: ", cache_dir, "\n")
  }

  # 2. Create a subdirectory for this specific R version and package version
  r_version <- paste0("R", getRversion()[1, 1:2])
  pkg_version <- tryCatch(
    as.character(packageVersion(pkg_name)),
    error = function(e) "dev" # Use "dev" for package development mode
  )

  version_dir <- file.path(cache_dir, r_version, pkg_version)
  if (!dir.exists(version_dir)) {
    dir.create(version_dir, recursive = TRUE, showWarnings = FALSE)
    if (verbose) log_msg("Created version-specific cache directory: ", version_dir, "\n")
  }

  # 3. Define the full path for the shared object based on platform
  dll_ext <- .Platform$dynlib.ext
  compiled_name <- paste0(dll_name, dll_ext)
  cached_dll_path <- file.path(version_dir, compiled_name)

  # Normalize the path to avoid issues with tilde (~) and short paths
  if (file.exists(dirname(cached_dll_path))) {
    cached_dll_path <- normalizePath(cached_dll_path, mustWork = FALSE)
  }

  # 4. Locate the original C++ source file - try multiple standard locations
  cpp_file <- system.file("tmb", paste0(dll_name, ".cpp"), package = pkg_name)

  # If not found in "tmb", try "src/tmb"
  if (cpp_file == "") {
    cpp_file <- system.file("src/tmb", paste0(dll_name, ".cpp"), package = pkg_name)
  }

  # If still not found, try "src"
  if (cpp_file == "") {
    cpp_file <- system.file("src", paste0(dll_name, ".cpp"), package = pkg_name)
  }

  # If still not found, check standard paths for package development
  if (cpp_file == "") {
    potential_paths <- c(
      file.path("inst/tmb", paste0(dll_name, ".cpp")),
      file.path("src/tmb", paste0(dll_name, ".cpp")),
      file.path("src", paste0(dll_name, ".cpp"))
    )

    for (path in potential_paths) {
      if (file.exists(path)) {
        cpp_file <- normalizePath(path, mustWork = TRUE)
        break
      }
    }
  }

  if (cpp_file == "" || !file.exists(cpp_file)) {
    stop(
      "Could not find the TMB source file ", dll_name, ".cpp in any standard location. ",
      "Searched in inst/tmb/, src/tmb/, and src/ directories."
    )
  }

  if (verbose) log_msg("Found TMB source file: ", cpp_file, "\n")

  # 5. Check if the cached shared object exists and is up-to-date
  need_compile <- force_recompile

  if (file.exists(cached_dll_path) && !force_recompile) {
    if (verbose) log_msg("Found cached TMB model: ", cached_dll_path, "\n")

    # Check if source code is newer than compiled version
    if (file.info(cpp_file)$mtime > file.info(cached_dll_path)$mtime) {
      if (verbose) log_msg("Source code is newer than compiled version. Recompiling...\n")
      need_compile <- TRUE
    } else {
      # Try to load the existing library
      load_result <- try(
        {
          # Use absolute normalized path to avoid issues with tilde (~)
          norm_path <- normalizePath(cached_dll_path, mustWork = TRUE)

          # Check if this DLL is already loaded
          dll_loaded <- FALSE
          loaded_dlls <- getLoadedDLLs()
          for (i in seq_along(loaded_dlls)) {
            if (loaded_dlls[[i]][["path"]] == norm_path) {
              dll_loaded <- TRUE
              break
            }
          }

          # Only load if not already loaded
          if (!dll_loaded) {
            dyn.load(norm_path)
          }

          # Store the path and DLL name in an environment variable for future reference
          assign(paste0("TMB_", dll_name, "_PATH"), norm_path, envir = .gkwreg_env)
          TRUE
        },
        silent = TRUE
      )

      if (!inherits(load_result, "try-error")) {
        if (verbose) log_msg("Successfully loaded cached TMB model.\n")

        # Create dll_info to return
        dll_info <- list(
          path = cached_dll_path,
          normalized_path = normalizePath(cached_dll_path, mustWork = FALSE),
          name = basename(tools::file_path_sans_ext(cached_dll_path)),
          compiled = FALSE
        )

        # Store in the environment
        assign(paste0("TMB_", dll_name, "_INFO"), dll_info, envir = .gkwreg_env)

        return(invisible(dll_info))
      } else {
        if (verbose) log_msg("Cached model exists but could not be loaded. Recompiling...\n")
        try(dyn.unload(cached_dll_path), silent = TRUE)
        need_compile <- TRUE
      }
    }
  } else if (!file.exists(cached_dll_path)) {
    if (verbose) log_msg("No cached model found. Compiling...\n")
    need_compile <- TRUE
  }

  # 6. Compile if needed - Windows-specific fixes here
  if (need_compile) {
    # IMPORTANT FIX 1: Create a simpler, shorter temporary path without special characters
    # This helps avoid Windows path issues with ~1 short names
    is_windows <- .Platform$OS.type == "windows"

    if (is_windows) {
      # Create a temporary directory with simple name, avoiding temp paths with spaces
      temp_base <- "C:/RTMP"
      if (!dir.exists(temp_base)) {
        dir.create(temp_base, recursive = TRUE, showWarnings = FALSE)
      }
      temp_compile_dir <- file.path(
        temp_base,
        paste0(
          dll_name, "_",
          format(Sys.time(), "%Y%m%d%H%M%S")
        )
      )
    } else {
      # For non-Windows, use standard temp directory
      temp_compile_dir <- file.path(
        tempdir(),
        paste0(
          dll_name, "_compile_",
          format(Sys.time(), "%Y%m%d%H%M%S")
        )
      )
    }

    dir.create(temp_compile_dir, recursive = TRUE, showWarnings = FALSE)

    # IMPORTANT FIX 2: Use the base filename only, not full path for temporary file
    cpp_basename <- basename(cpp_file)
    temp_cpp_file <- file.path(temp_compile_dir, cpp_basename)
    file.copy(cpp_file, temp_cpp_file, overwrite = TRUE)

    # Set current directory to temp compilation location
    old_dir <- getwd()
    setwd(temp_compile_dir)
    on.exit(setwd(old_dir), add = TRUE)

    if (verbose) log_msg("Compiling TMB model from ", temp_cpp_file, "...\n")

    # IMPORTANT FIX 3: For Windows, use filename only when in the current directory
    if (is_windows) {
      # Copy all TMB-related DLLs to ensure linking works
      tmb_dlls <- list.files(system.file("libs", package = "TMB"),
        pattern = "\\.dll$", full.names = TRUE
      )
      if (length(tmb_dlls) > 0) {
        for (dll in tmb_dlls) {
          file.copy(dll, temp_compile_dir, overwrite = TRUE)
        }
      }

      # Copy RcppEigen include files if needed
      if (requireNamespace("RcppEigen", quietly = TRUE)) {
        eigen_includes <- system.file("include", package = "RcppEigen")
        if (dir.exists(eigen_includes)) {
          eigen_target <- file.path(temp_compile_dir, "eigen_includes")
          dir.create(eigen_target, showWarnings = FALSE)
          eigen_files <- list.files(eigen_includes, recursive = TRUE, full.names = TRUE)
          for (f in eigen_files) {
            target_dir <- dirname(file.path(
              eigen_target,
              sub(eigen_includes, "", f, fixed = TRUE)
            ))
            if (!dir.exists(target_dir)) {
              dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
            }
            file.copy(f, target_dir, overwrite = TRUE)
          }
        }
      }

      # For Windows, use the base filename only for compilation
      compile_result <- try(
        {
          TMB::compile(cpp_basename, # Use only the filename
            safebounds = FALSE,
            safeunload = FALSE,
            verbose = verbose
          )
        },
        silent = !verbose
      )
    } else {
      # For non-Windows, use the full path as before
      compile_result <- try(
        {
          TMB::compile(temp_cpp_file,
            safebounds = FALSE,
            safeunload = FALSE,
            verbose = verbose
          )
        },
        silent = !verbose
      )
    }

    if (inherits(compile_result, "try-error")) {
      # IMPORTANT FIX 4: Enhanced error handling for Windows path issues
      error_msg <- attr(compile_result, "condition")$message
      if (is_windows && grepl("No such file or directory", error_msg)) {
        # Try POSIX path handling as a fallback on Windows
        try_posix <- try(
          {
            cpp_code <- readLines(temp_cpp_file, warn = FALSE)
            writeLines(cpp_code, "tmb_file.cpp") # Simple filename
            TMB::compile("tmb_file.cpp",
              safebounds = FALSE,
              safeunload = FALSE,
              verbose = verbose
            )
          },
          silent = !verbose
        )

        if (!inherits(try_posix, "try-error")) {
          compile_result <- try_posix
        } else {
          stop(
            "TMB model compilation failed with Windows path handling issues. ",
            "Error: ", error_msg
          )
        }
      } else {
        stop("TMB model compilation failed. Error: ", error_msg)
      }
    }

    # Check if compilation was successful
    temp_dll_name <- TMB::dynlib(dll_name)
    temp_dll_path <- file.path(temp_compile_dir, temp_dll_name)

    # IMPORTANT FIX 5: If first attempt failed, look for alternative filenames
    if (!file.exists(temp_dll_path) && is_windows) {
      # Check for "tmb_file" if we used the fallback
      alt_dll_path <- file.path(temp_compile_dir, TMB::dynlib("tmb_file"))
      if (file.exists(alt_dll_path)) {
        temp_dll_path <- alt_dll_path
      } else {
        # Look for any DLL that might have been created
        dll_files <- list.files(temp_compile_dir, pattern = "\\.dll$")
        if (length(dll_files) > 0) {
          temp_dll_path <- file.path(temp_compile_dir, dll_files[1])
        }
      }
    }

    if (!file.exists(temp_dll_path)) {
      stop("Compiled file was not generated. Check permissions and file paths.")
    }

    # Copy the compiled file to the cache directory
    file.copy(temp_dll_path, cached_dll_path, overwrite = TRUE)
    if (verbose) log_msg("Copied compiled model to cache: ", cached_dll_path, "\n")

    # Clean up temporary files - important for Windows to release file handles
    if (is_windows) {
      # Unload any loaded DLLs first to avoid "file in use" errors
      for (dll_file in list.files(temp_compile_dir, pattern = "\\.dll$", full.names = TRUE)) {
        try(dyn.unload(dll_file), silent = TRUE)
      }

      # On Windows, use shell.exec to delete the directory in a separate process
      # This avoids file locking issues
      try(unlink(temp_compile_dir, recursive = TRUE, force = TRUE), silent = TRUE)
    } else {
      unlink(temp_compile_dir, recursive = TRUE)
    }

    # Load the newly compiled model
    load_result <- try(
      {
        norm_path <- normalizePath(cached_dll_path, mustWork = TRUE)

        # Unload first if already loaded
        try(dyn.unload(norm_path), silent = TRUE)

        # Now load
        dyn.load(norm_path)

        # Store the path in environment variable
        assign(paste0("TMB_", dll_name, "_PATH"), norm_path, envir = .gkwreg_env)
        TRUE
      },
      silent = !verbose
    )

    if (inherits(load_result, "try-error")) {
      error_msg <- attr(load_result, "condition")$message
      stop("Failed to load compiled model. Error: ", error_msg)
    }

    if (verbose) log_msg("Successfully compiled and loaded TMB model.\n")
  }

  # Return the DLL path and other relevant information
  dll_info <- list(
    path = cached_dll_path,
    normalized_path = normalizePath(cached_dll_path, mustWork = FALSE),
    name = basename(tools::file_path_sans_ext(cached_dll_path)),
    compiled = need_compile
  )

  # Associate this information with the R environment for future reference
  assign(paste0("TMB_", dll_name, "_INFO"), dll_info, envir = .gkwreg_env)

  invisible(dll_info)
}



.onLoad <- function(libname, pkgname) {
  # Pre-compile all TMB models
  all_models <- c("gkwbetareg", "bkwreg", "ekwreg", "gkwmletmb", "gkwreg", "kkwreg", "kwreg", "mcreg")
  for (model in all_models) {
    tryCatch(
      {
        .check_and_compile_TMB_code(model, verbose = FALSE)
      },
      error = function(e) {
        warning("Failed to pre-compile TMB model '", model, "': ", e$message)
      }
    )
  }
}





#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


## usethis namespace: start
#' @importFrom Rcpp sourceCpp evalCpp
#' @import RcppArmadillo
#' @import graphics
## usethis namespace: end
NULL

#' @useDynLib gkwreg, .registration = TRUE
NULL
