# ==============================================================================
# Matching Memory - RAM-aware guard for dense cost-matrix construction
# ==============================================================================
# By default couplr materializes a dense n_left x n_right cost matrix before
# solving. This file estimates that matrix's memory footprint against free
# system RAM and resolves the requested `memory_mode` ("auto"/"dense"/"lazy")
# to a concrete decision, warning loudly instead of silently crashing/thrashing
# on oversized problems. A lazy path exists for method = "jv"/"auction" with a
# built-in distance metric (see R/matching_lazy.R); callers whose downstream
# solve path is not lazy-aware pass solver_supports_lazy = FALSE, so "auto"
# only ever switches to "lazy" where a lazy solve actually exists.

# Below this cell count, never even probe free RAM: ~1e7 cells is ~80MB dense,
# well within any machine's headroom, and probing on every ordinary-sized
# problem would add needless overhead (a full memory-mode audit costs nothing
# for the overwhelming majority of existing, small problems).
COUPLR_MEMORY_PROBE_THRESHOLD_CELLS <- 1e7

#' Page size behind a block of `vm_stat` output
#'
#' `vm_stat` counts in pages and states its own page size in the header line
#' ("page size of 16384 bytes"). Apple Silicon pages are 16K and Intel pages are
#' 4K, so the size is read rather than assumed; `sysctl hw.pagesize` is the
#' fallback when the header cannot be parsed.
#'
#' @param vm Character vector of `vm_stat` output lines.
#' @return Page size in bytes, or `NA_real_` if undetermined.
#' @keywords internal
vm_stat_page_size <- function(vm) {
  header <- grep("page size of", vm, value = TRUE)
  page_size <- if (length(header) > 0) {
    suppressWarnings(as.numeric(
      sub(".*page size of ([0-9]+) bytes.*", "\\1", header[1])))
  } else {
    NA_real_
  }
  if (length(page_size) == 0 || is.na(page_size)) {
    # /usr/sbin is not on a non-interactive PATH, so try the absolute path too.
    for (sysctl in c("sysctl", "/usr/sbin/sysctl")) {
      page_size <- tryCatch(
        suppressWarnings(as.numeric(system2(sysctl, c("-n", "hw.pagesize"),
                                            stdout = TRUE, stderr = FALSE)[1])),
        error = function(e) NA_real_
      )
      if (length(page_size) > 0 && !is.na(page_size)) break
    }
  }
  if (length(page_size) == 0 || is.na(page_size) || page_size <= 0) {
    return(NA_real_)
  }
  page_size
}

#' Available megabytes implied by a block of `vm_stat` output
#'
#' Inactive and speculative pages are reclaimed on demand, so they count as
#' available to an allocation in the same sense as Linux's `MemAvailable`; macOS
#' keeps almost nothing on the free list, so the free count alone understates
#' what a large matrix can actually obtain.
#'
#' Split from [get_free_ram_mb()] so the page-size and page-class handling can be
#' checked without a macOS host.
#'
#' @param vm Character vector of `vm_stat` output lines.
#' @param page_size Page size in bytes, as returned by [vm_stat_page_size()].
#' @return Numeric scalar (MB available), or `NA_real_` if unparseable.
#' @keywords internal
vm_stat_available_mb <- function(vm, page_size) {
  pages_in <- function(label) {
    hit <- grep(paste0("^", label, ":"), vm, value = TRUE)
    if (length(hit) == 0) return(NA_real_)
    v <- suppressWarnings(as.numeric(
      regmatches(hit[1], regexpr("[0-9]+", hit[1]))))
    if (length(v) == 0) NA_real_ else v[1]
  }
  free_pages <- pages_in("Pages free")
  if (is.na(free_pages)) return(NA_real_)
  reclaimable <- c(pages_in("Pages inactive"), pages_in("Pages speculative"))
  ((free_pages + sum(reclaimable, na.rm = TRUE)) * page_size) / 1024^2
}

#' Estimate available system RAM in megabytes
#'
#' Cross-platform, base-R-only (shells out; no new package dependency).
#' Never errors: returns `NA_real_` if detection fails or the platform is
#' unrecognized, so callers must treat `NA` as "unknown" and fall back to a
#' fixed threshold rather than skipping the guard entirely.
#'
#' "Available" means memory an allocation can obtain without swapping, which on
#' every platform is more than the untouched free list: Linux reports it
#' directly as `MemAvailable`, and on macOS it is the free, inactive and
#' speculative pages together, since the kernel keeps almost nothing on the
#' free list and reclaims the rest on demand.
#'
#' @return Numeric scalar (MB of available RAM), or `NA_real_` if undetermined.
#' @keywords internal
get_free_ram_mb <- function() {
  sysname <- Sys.info()[["sysname"]]
  tryCatch({
    if (identical(sysname, "Windows")) {
      out <- suppressWarnings(system2("powershell",
        c("-NoProfile", "-Command",
          "(Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory"),
        stdout = TRUE, stderr = FALSE))
      kb <- suppressWarnings(as.numeric(trimws(out[1])))
      if (is.na(kb)) {
        out <- suppressWarnings(system2("wmic",
          c("OS", "get", "FreePhysicalMemory", "/Value"),
          stdout = TRUE, stderr = FALSE))
        hit <- out[grepl("=", out)]
        kb <- suppressWarnings(as.numeric(sub("FreePhysicalMemory=", "", hit)))
      }
      if (length(kb) == 0 || is.na(kb[1])) return(NA_real_)
      kb[1] / 1024
    } else if (identical(sysname, "Linux")) {
      lines <- readLines("/proc/meminfo", warn = FALSE)
      hit <- grep("^MemAvailable:", lines, value = TRUE)
      if (length(hit) == 0) return(NA_real_)
      kb <- suppressWarnings(as.numeric(regmatches(hit, regexpr("[0-9]+", hit))))
      if (length(kb) == 0 || is.na(kb[1])) return(NA_real_)
      kb[1] / 1024
    } else if (identical(sysname, "Darwin")) {
      vm <- suppressWarnings(system2("vm_stat", stdout = TRUE, stderr = FALSE))
      if (length(vm) == 0) return(NA_real_)
      page_size <- vm_stat_page_size(vm)
      if (is.na(page_size)) return(NA_real_)
      vm_stat_available_mb(vm, page_size)
    } else {
      NA_real_
    }
  }, error = function(e) NA_real_, warning = function(w) NA_real_)
}

#' Estimate dense cost-matrix memory footprint in megabytes
#'
#' What the matrix itself costs a process, which is more than the 8 bytes a cell
#' occupies: `matrix(0, n, m)` at the R level is copied into a
#' `lap::CostMatrix` (8B data + 4B mask), and the two coexist while garbage
#' collection lags. Building the matrix and stopping there has peaked at between
#' 1.5 and 1.9 times the raw cell bytes from 5,000 to 20,000 units on the memory
#' benchmark, so the default multiplier is above what has been measured rather
#' than fitted to it. `n`/`m`
#' are coerced to `double` before multiplying so the estimate itself can't
#' overflow the way `lap::CostMatrix`'s old `int` flat-index arithmetic did.
#'
#' This is the matrix, not the solve. [estimate_dense_solve_mb()] is what the
#' memory guard reads; see there for why the two differ by more than a copy.
#'
#' @param n,m Problem dimensions.
#' @param overhead_factor Multiplier on the raw cell bytes.
#' @return Numeric scalar, the estimated footprint of the matrix in megabytes.
#' @examples
#' estimate_dense_matrix_mb(5000, 5000)
#' @export
estimate_dense_matrix_mb <- function(n, m, overhead_factor = 4) {
  (as.numeric(n) * as.numeric(m) * 8 * overhead_factor) / 1e6
}

#' Estimate the peak footprint of a dense solve in megabytes
#'
#' The matrix is the allocation a caller can reason about and it is not what
#' bounds a dense solve. Preparation copies, the solver's own workspace and the
#' assignment structures it carries all sit on top of the matrix, and the peak
#' is what decides whether the solve fits, so that is the quantity the guard
#' compares against available RAM.
#'
#' The multiplier is read off the measurement rather than from an enumeration of
#' the copies, which has proved to understate it. On the memory benchmark -- one
#' fresh R session per arm, peak resident set taken from outside and read
#' against an idle session that loaded the same packages -- a dense one-to-one
#' solve has peaked at between 7 and 11 times the raw matrix bytes from 5,000 to
#' 20,000 units, with the figure at a given size moving between runs. The
#' default of 12 sits above every measured peak, which is the direction a guard
#' should err in: it exists to refuse a solve that will not fit, not to predict
#' where the peak lands.
#'
#' @param n,m Problem dimensions.
#' @param solve_factor Multiplier on the raw cell bytes.
#' @return Numeric scalar, the estimated peak footprint of a dense solve in
#'   megabytes.
#' @examples
#' estimate_dense_solve_mb(5000, 5000)
#' @export
estimate_dense_solve_mb <- function(n, m, solve_factor = 12) {
  (as.numeric(n) * as.numeric(m) * 8 * solve_factor) / 1e6
}

#' Resolve a requested memory_mode to a concrete decision
#'
#' @param n,m Problem dimensions (left/right unit counts).
#' @param memory_mode One of "auto" (probe RAM and decide), "dense" (always,
#'   skip probing entirely), "lazy" (always, error if unsupported here), or
#'   "implicit" (always, error if unsupported here).
#' @param solver_supports_lazy Whether a lazy path actually exists for the
#'   caller's chosen solver/distance combination (`TRUE` only for `method =
#'   "jv"`/`"auction"` with a built-in distance metric, on a caller whose
#'   solve path consumes a `lazy_cost_spec`; see R/matching_lazy.R).
#' @param solver_supports_implicit Whether the caller's design is the one the
#'   edge-generation loop solves: a 1:1 matching over a built-in distance
#'   metric, whose network is one unit-capacity bipartite block.
#' @param ram_fraction Fraction of available RAM a dense solve's peak may
#'   reach before "auto" switches away from dense.
#' @param fallback_threshold_mb Fixed threshold used when available RAM can't be
#'   determined (mirrors the warn+fallback precedent in
#'   `R/morph_utils.R`'s `matrix_size > 1e8` cell guard).
#'
#' @return "dense", "lazy" or "implicit".
#' @keywords internal
resolve_memory_mode <- function(n, m,
                                memory_mode = c("auto", "dense", "lazy",
                                                "implicit"),
                                solver_supports_lazy = FALSE,
                                solver_supports_implicit = FALSE,
                                ram_fraction = 0.5,
                                fallback_threshold_mb = 4000) {
  memory_mode <- match.arg(memory_mode)

  if (identical(memory_mode, "dense")) {
    return("dense")
  }

  if (identical(memory_mode, "lazy")) {
    if (!solver_supports_lazy) {
      stop("memory_mode = \"lazy\" is not supported for this method/path yet.",
           call. = FALSE)
    }
    return("lazy")
  }

  if (identical(memory_mode, "implicit")) {
    if (!solver_supports_implicit) {
      stop("memory_mode = \"implicit\" is not supported for this method/path ",
           "yet: the edge-generation loop solves match_couples() one-to-one and ",
           "full_match(method = \"optimal\") over a built-in distance metric.",
           call. = FALSE)
    }
    return("implicit")
  }

  # memory_mode == "auto".
  #
  # "auto" never resolves to "implicit". The loop wins by a factor that moves
  # with problem size and covariate dimension at once, and it loses below
  # roughly n = 10,000, where a complete solve already costs hundredths of a
  # second. A rule that has to know a crossover surface in two variables,
  # measured on one machine, is a rule that sends someone's small problem the
  # slow way. It is opt-in until a rule with measurements behind it exists,
  # which is the convention gabow_tarjan is already opt-in under.
  n_cells <- as.numeric(n) * as.numeric(m)
  if (n_cells < COUPLR_MEMORY_PROBE_THRESHOLD_CELLS) {
    return("dense")
  }

  needed_mb <- estimate_dense_solve_mb(n, m)
  free_mb <- get_free_ram_mb()

  if (is.na(free_mb)) {
    if (needed_mb > fallback_threshold_mb) {
      if (solver_supports_lazy) {
        warning(sprintf(
          "Could not determine available system RAM; a dense solve of this problem peaks at ~%.0f MB. Switching to memory_mode = \"lazy\".",
          needed_mb), call. = FALSE)
        return("lazy")
      }
      warning(sprintf(
        "Could not determine available system RAM; a dense solve of this problem peaks at ~%.0f MB. Proceeding densely -- consider blocking (block_id), method = \"greedy\", or reducing the problem size.",
        needed_mb), call. = FALSE)
    }
    return("dense")
  }

  if (needed_mb > ram_fraction * free_mb) {
    if (solver_supports_lazy) {
      warning(sprintf(
        "A dense solve of this problem peaks at ~%.1f GB against ~%.1f GB available RAM. Switching to memory_mode = \"lazy\".",
        needed_mb / 1e3, free_mb / 1e3), call. = FALSE)
      return("lazy")
    }
    warning(sprintf(
      "A dense solve of this problem peaks at ~%.1f GB against ~%.1f GB available RAM, and this path does not support memory_mode = \"lazy\" yet. Proceeding densely -- consider blocking (block_id), method = \"greedy\", reducing the problem size, or running on a machine with more RAM.",
      needed_mb / 1e3, free_mb / 1e3), call. = FALSE)
  }

  "dense"
}
