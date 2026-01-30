search_files_sim2 <- function(words, files = list.files("R", pattern = "\\.R$", full.names = TRUE)) {

  # Ensure words is a character vector
  words <- as.character(words)

  for (f in files) {
    lines <- readLines(f, warn = FALSE)

    # Check if all words appear in the same line
    hits_idx <- which(sapply(lines, function(line) all(sapply(words, function(w) grepl(w, line)))))

    if (length(hits_idx) > 0) {
      message("\n--- Matches in file:", basename(f), "---")
      for (i in hits_idx) {
        message("Line", i, ":", lines[i])
      }
    }
  }
}


#' @keywords internal
#' @noRd
search_files_sim <- function(word, files = list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  for (w in word) {
    message("\n--- Searching for:", w, "---")
    results <- lapply(files, function(f) {
      lines <- readLines(f, warn = FALSE)
      hits <- grep(w, lines, value = TRUE)
      if (length(hits) > 0) {
        paste(basename(f), ":", hits)
      }
    })
    results <- unlist(results)
    if (length(results) == 0) {
      message("No matches found.")
    } else {
      message(paste(results, collapse = "\n"))
    }
  }
}



#' Search for text in code or Rmd files
#'
#' Internal helper function to search for occurrences of a word or phrase
#' in R scripts and R Markdown files within a project.
#'
#' @param word Character vector. Words or patterns to search for.
#' @param files Character vector of file paths to search. If `NULL`, the
#'   function searches in default directories (`R`, `inst/rmd`, `vignettes`)
#'   for files matching the specified `patterns`.
#' @param dirs Character vector of directories to search when `files = NULL`.
#' @param patterns Character vector of file patterns to match (e.g., `\\.R$`, `\\.Rmd$`).
#'
#' @return Prints matches to the console. Each match shows the file name and the line containing the match.
#' @keywords internal
#' @noRd
search_files <- function(word, files = NULL, dirs = c("R", "inst/rmd", "vignettes"), patterns = c("\\.R$", "\\.Rmd$")) {

  # If files not provided, build list from dirs and patterns
  if (is.null(files)) {
    files <- unlist(lapply(dirs, function(d) {
      list.files(d, pattern = paste(patterns, collapse = "|"), recursive = TRUE, full.names = TRUE)
    }))
  }

  if (length(files) == 0) {
    stop("No files found to search.")
  }

  # Ensure word is a character vector
  word <- as.character(word)

  for (w in word) {
    message("\n--- Searching for: ", w, " ---")


    results <- lapply(files, function(f) {
      lines <- readLines(f, warn = FALSE)
      hits <- grep(w, lines, value = TRUE)
      if (length(hits) > 0) {
        paste0(basename(f), ":", hits)
      } else {
        NULL
      }
    })

    results <- unlist(results)
    if (length(results) == 0) {
      message("No matches found.")
    } else {
      message(paste(results, collapse = "\n"))
    }
  }
}



#' Check Unqualified Function Calls in R and Rmd Files
#'
#' Internal helper function for package development.
#' Scans R and Rmd files in specified paths for functions from given packages
#' that are called without namespace (i.e., without `pkg::`).
#'
#' @param paths Character vector of paths to scan. Default is `c("R", "inst/rmd")`.
#' @param pkgs Character vector of package names to check for. Default is `c("htmltools", "knitr", "gt")`.
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{file}{File path}
#'     \item{line}{Line number in the file}
#'     \item{fun}{Function name found}
#'     \item{pkg}{Package(s) the function belongs to}
#'   }
#'
#' @keywords internal
#' @noRd
check_unqualified_calls <- function(paths = c("R", "inst/rmd"),
                                    pkgs = c("htmltools", "knitr", "gt")) {

  files <- unlist(lapply(paths, function(p) {
    pattern <- if (p == "inst/rmd") "\\.Rmd$" else "\\.R$"
    list.files(p, pattern = pattern, recursive = TRUE, full.names = TRUE)
  }))

  # build lookup: package -> exported functions
  exported_funs <- lapply(pkgs, function(pkg) {
    tryCatch(getNamespaceExports(pkg), error = function(e) character(0))
  })
  names(exported_funs) <- pkgs
  all_exported <- unlist(exported_funs)

  res <- lapply(files, function(f) {
    lines <- readLines(f, warn = FALSE)

    # remove trailing comments for scanning
    code_content <- gsub("#.*$", "", lines)

    # scan each line
    line_dfs <- lapply(seq_along(code_content), function(i) {
      line <- code_content[i]
      matches <- regmatches(
        line,
        gregexpr("\\b(?<![[:alnum:]_:\\$\\[\\.])([a-zA-Z0-9_]+)\\s*\\(", line, perl = TRUE)
      )
      fns <- unlist(lapply(matches, function(x) gsub("\\s*\\($", "", x)))
      fns <- intersect(fns, all_exported)  # keep only functions in packages
      if (length(fns) == 0) return(NULL)

      # determine package(s) for each function
      pkg_list <- sapply(fns, function(fn) {
        pkgs_found <- names(exported_funs)[sapply(exported_funs, function(e) fn %in% e)]
        paste(pkgs_found, collapse = ";")
      })

      data.frame(file = f, line = i, fun = fns, pkg = pkg_list, stringsAsFactors = FALSE)
    })

    # combine lines in this file
    do.call(rbind, line_dfs)
  })

  # combine all files
  do.call(rbind, res)
}



#' Interactive Add Package Namespace Workflow
#'
#' Internal helper to find unqualified function calls from specified packages
#' in R or Rmd files, optionally interactively add `pkg::`, and update originals.
#'
#' @param paths Character vector of paths to scan. Default: c("R", "inst/rmd").
#' @param pkgs Character vector of package names to check for. Default: c("htmltools", "knitr", "gt").
#' @keywords internal
#' @noRd
interactive_add_pkg <- function(paths = c("R", "inst/rmd"),
                                pkgs = c("htmltools", "knitr", "gt")) {

  # --- 1. Scan for unqualified function calls ---
  res_list <- lapply(paths, function(p) {
    pattern <- if (p == "inst/rmd") "\\.Rmd$" else "\\.R$"
    files <- list.files(p, pattern = pattern, recursive = TRUE, full.names = TRUE)

    # build lookup: package -> exported functions
    exported_funs <- lapply(pkgs, function(pkg) {
      tryCatch(getNamespaceExports(pkg), error = function(e) character(0))
    })
    names(exported_funs) <- pkgs
    all_exported <- unlist(exported_funs)

    # scan each file
    lapply(files, function(f) {
      lines <- readLines(f, warn = FALSE)
      code_content <- gsub("#.*$", "", lines)  # remove trailing comments

      line_dfs <- lapply(seq_along(code_content), function(i) {
        line <- code_content[i]
        matches <- regmatches(
          line,
          gregexpr("\\b(?<![[:alnum:]_:\\$\\[\\.])([a-zA-Z0-9_]+)\\s*\\(", line, perl = TRUE)
        )
        fns <- unlist(lapply(matches, function(x) gsub("\\s*\\($", "", x)))
        fns <- intersect(fns, all_exported)
        if(length(fns) == 0) return(NULL)

        # determine package(s) for each function
        pkg_list <- sapply(fns, function(fn) {
          pkgs_found <- names(exported_funs)[sapply(exported_funs, function(e) fn %in% e)]
          paste(pkgs_found, collapse = ";")
        })

        data.frame(file = f, line = i, fun = fns, pkg = pkg_list, stringsAsFactors = FALSE)
      })

      # combine lines in this file
      df <- do.call(rbind, line_dfs)
      if(is.null(df)) {
        df <- data.frame(file=character(0), line=integer(0), fun=character(0), pkg=character(0), stringsAsFactors = FALSE)
      }
      df
    })
  })

  # combine all files
  res <- do.call(rbind, unlist(res_list, recursive = FALSE))

  # ensure res is always a data.frame
  if(is.null(res) || nrow(res) == 0) {
    message("No unqualified functions found in the specified paths.")
    res <- data.frame(file=character(0),
                      line=integer(0),
                      fun=character(0),
                      pkg=character(0),
                      stringsAsFactors = FALSE)
    return(invisible(res))
  }

  # print(res)


  # --- 2. Option to continue or just search ---
  ans_continue <- readline("Continue to add pkg:: to functions? [Y/n]: ")
  if(tolower(ans_continue) == "n") {
    message("Search-only mode. Returning results without modifying files.")
    return(invisible(res))
  }

  # --- 3. Interactive add pkg:: workflow ---
  for(f in unique(res$file)) {
    lines <- readLines(f)
    f_copy <- sub("(\\.R|\\.Rmd)$", "_copy\\1", f)
    writeLines(lines, f_copy)
    new_lines <- lines
    file_res <- res[res$file == f, ]

    for(i in seq_len(nrow(file_res))) {
      ln <- file_res$line[i]
      fn <- file_res$fun[i]
      pkg <- strsplit(file_res$pkg[i], ";")[[1]][1]

      message("\nFile:", f_copy, "Line", ln, ":", lines[ln])
      ans <- readline(paste0("Add ", pkg, ":: to function '", fn, "'? [y/N]: "))
      if(tolower(ans) == "y") {
        pattern <- paste0("(?<![[:alnum:]_:\\$\\[\\.])", fn, "\\s*\\(")
        replacement <- paste0(pkg, "::", fn, "(")
        new_lines[ln] <- gsub(pattern, replacement, new_lines[ln], perl = TRUE)
      }
    }

    writeLines(new_lines, f_copy)

    # compare original vs copy
    diff_lines <- which(lines != new_lines)
    if(length(diff_lines) > 0) {
      message("\nDifferences between original and _copy:")
      for(ln in diff_lines) {
        message(sprintf("Line %d:\n  Original: %s\n  Copy:     %s\n",
                        ln, lines[ln], new_lines[ln]))

      }

      ans2 <- readline(paste0("\nUpdate original file '", f, "' with changes from _copy? [y/N]: "))
      if(tolower(ans2) == "y") {
        writeLines(new_lines, f)
        message("Original file updated:", f)
        unlink(f_copy)
      } else {
        message("Original file NOT changed:", f)
        unlink(f_copy)
      }
    } else {
      message("No differences detected for", f_copy)
      unlink(f_copy)
    }
  }

  invisible(res)
}


