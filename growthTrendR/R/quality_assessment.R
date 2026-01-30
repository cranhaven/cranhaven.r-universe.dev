#' pair-wise ccf
#' @description
#' pair-wise ccf
#' @param ts1 first series
#' @param ts2 second series
#' @param lag.max maximum lag for ccf


# #' @export ccf_pairs
#' @keywords internal
#' @noRd
ccf_pairs <- function(ts1, ts2, lag.max = 10) {
  ccf.chk <- ccf(ts1, ts2, lag.max = lag.max, na.action = na.pass,  plot = FALSE)
  max_ccf <- max(ccf.chk$acf)
  max_lag <- ccf.chk$lag[which.max(ccf.chk$acf)]
  return(list(max_lag = max_lag, max_ccf = max_ccf))
}


#' ccf with chronologies
#' @description
#' ccf with chronologies
#' @param icol column index
#' @param data data in wide format, first column is "Year", second column is chronologies
#' @param blcrit criteria for borderline
#' @param lag.max maximum lag for ccf
#' @param qa_code quality classification code (NULL, no code)


# #' @export ccf_avg
#' @keywords internal
#' @noRd
ccf_avg <- function(icol, data, blcrit = 0.1, lag.max = 10, qa_code="fail"){

  dt.pairs <- ccf(data[,2],data[,icol, with = FALSE],lag.max=lag.max,plot=FALSE, na.action = na.pass)
  dt.ccf <- data.table(SampleID.chr = names(data)[icol], lag = as.vector(dt.pairs$lag), acf.trt = as.vector(dt.pairs$acf))
  dt.ccf <- dt.ccf[!is.na(acf.trt)]
  setorder(dt.ccf, -acf.trt)
  dt.ccf[, ccf.ord:= 1:.N]
  setorder(dt.ccf, lag)

  if (!is.null(qa_code)){
    qa_code <- "Fail"
  # max acf at lag 0
  if (nrow(dt.ccf[lag == 0 & ccf.ord == 1]) == 1 ) qa_code="pass"
  if (nrow(dt.ccf[lag == 0 & ccf.ord == 2]) == 1 & abs(dt.ccf[ccf.ord == 1]$acf.trt - dt.ccf[ccf.ord == 2]$acf.trt) <blcrit) qa_code="borderline"
  if (nrow(dt.ccf[lag == 0 & ccf.ord == 1]) == 0 & dt.ccf[ccf.ord == 1]$acf.trt / dt.ccf[ccf.ord == 2]$acf.trt > 2) qa_code="highpeak"
  if (nrow(dt.ccf[lag == 1 & ccf.ord == 1]) + nrow(dt.ccf[lag == -1 & ccf.ord == 1]) == 1) qa_code="pm1"
  dt.ccf$qa_code <- qa_code
  }
  return(dt.ccf)
}


# Define the main function to process each pair

# Use map to calculate pairwise cross-correlation for each pair
# this is the most efficient way for this calculation so far i found, 20-06-24, 2 mins for 490 series
# system.time(ccf.pairs <- map(col_pairs, ~ ccf_pairs(dt.trt_wide[[.x[1]]], dt.trt_wide[[.x[2]]])))

#' @keywords internal
process_batch <- function(batch,dt.wide, qa.max_lag = 10) {

  check_optional_deps()
  rbindlist(
    purrr::map2(batch,
         purrr::map(batch, ~ ccf_pairs(dt.wide[[.x[1]]], dt.wide[[.x[2]]], qa.max_lag)),
         ~ {
           data.table(ts1 = .x[1], ts2 = .x[2], max_lag = .y$max_lag, max_ccf = .y$max_ccf)
         }
    )
  )
}

# Helper function column-wise correlation with mean
cor_avg <- function(data){
  # Helper function using cor.test
  cor_with_stats <- function(x, y) {

    valid <- complete.cases(x, y)  # Remove NA values
    x <- x[valid]
    y <- y[valid]

    if (length(x) > 2) {
      test <- cor.test(x, y)
      list(corr = test$estimate, n = length(x), p_value = test$p.value)
    } else {
      list(corr = NA, n = length(x), p_value = NA)  # Insufficient data
    }
  }
  label <- utils::tail(str_split(deparse(substitute(data)), "\\.")[[1]], 1)



  corr_stats <- lapply(3:ncol(data), function(i) {
    cor_with_stats(data$mean.rw, data[[i]])
  })

  # Create data.table with the results
  correlations_df <- data.table(
    SampleID.chr = names(data)[3:ncol(data)],
    corr_mean = round(sapply(corr_stats, function(x) x$corr),2),
    ncorr_mean = sapply(corr_stats, function(x) x$n),
    pcorr_mean = round(sapply(corr_stats, function(x) x$p_value),2)
  )
  names(correlations_df)[-1] <- paste0(names(correlations_df)[-1], "_", label)
  return(correlations_df)

}


#' Quality assessment: ring-width measurement at plot level
#' @description
#' Apply a k-nearest neighbors (k-NN) method based on geographic location for the same species.
#' It compares the median tree-ring measurements of a specific site to those of nearby sites
#'
#' @param target_site  data.table with columns uid_site, site_id, species, longitude and latitude
#' @param ref_sites  reference sites including species, uid_site, latitude, longitude, uid_radius, year, rw_mm in long format
#' @param scale.label_data_ref description of ref_sites
#' @param scale.max_dist_km maximum distance to search the neighbors in km
#' @param scale.N_nbs  number of nearest-neighbors (maximum)
#'
# #' @import geosphere
#'
#' @return A data table containing the median ring-width measurements of the involved sites, along with the distances from the specific site

#' @export CFS_scale
#'

#' @examples
#' # loading formatted
#' dt.samples_trt <- readRDS(system.file("extdata", "dt.samples_trt.rds", package = "growthTrendR"))
#' all.sites <- dt.samples_trt$tr_all_wide[,.N, by = c("species", "uid_site", "site_id")][, N:=NULL]
#' dupes <- all.sites[, .N, by = .(species, site_id)][N > 1]

#' # e.g. taking the target sites
#' target_site <- all.sites[c(1,2), -"uid_site"]

#' ref.sites <- merge(
#' dt.samples_trt$tr_all_wide[,c("species", "uid_site", "site_id",
#'  "latitude","longitude", "uid_radius")],
#' dt.samples_trt$tr_all_long$tr_7_ring_widths, by = c("uid_radius"))

#' dt.scale <- CFS_scale( target_site = target_site, ref_sites = ref.sites,
#' scale.label_data_ref = "demo-samples", scale.max_dist_km = 200, scale.N_nbs = 2)

#'

CFS_scale <- function(target_site, ref_sites,
                      scale.label_data_ref = "",
                      scale.max_dist_km = 20,
                      scale.N_nbs = 10) {
  check_optional_deps()

  # check key columns
  required_ref_cols <- c("species", "uid_site", "site_id", "latitude", "longitude", "uid_radius", "year", "rw_mm")
  if (length(setdiff(required_ref_cols, names(ref_sites))) > 0)
    stop(paste0("Missing mandatory columns in ref_sites.", setdiff(required_ref_cols, names(ref_sites))))

  # check duplicates
  dup_sites <- target_site[, .N, by = .(species, site_id)][N > 1]
  if (nrow(dup_sites) > 0) {
    stop("Duplicated rows found in target_site:\n",
         paste(capture.output(print(dup_sites)), collapse = "\n"))
  }


  # check unmatched sites
  unmatched <- target_site[!ref_sites, on = .(species, site_id)]
  if (nrow(unmatched) > 0) {
    stop(
      "The following (species, site_id) not found in ref_sites:\n",
      paste(capture.output(print(unmatched)), collapse = "\n")
    )
  }


  if (scale.max_dist_km < 0) stop(paste0( "please check invalid scale.max_dist_km: ", scale.max_dist_km))
  if (scale.N_nbs < 1) stop(paste0( "please check invalid scale.N_nbs: ", scale.N_nbs))
  # site list from ref_sites
  site.all <- ref_sites[, .N, by = .(species, uid_site, site_id, latitude, longitude)][, N := NULL]
  target_site.LL <- merge(target_site, site.all, by = c("species", "site_id"))


  dup_rows <- target_site.LL[, .N, by = .(species, site_id)][N > 1]

  if (nrow(dup_rows) > 0) {
    stop(
      "Duplicated rows found after merging with reference dataset.\n",
      "Check reference dataset: species-site_id associated with multiple uid_site:\n",
      paste(capture.output(print(dup_rows)), collapse = "\n")
    )
  }


  # loop per row
  results_list <- lapply(1:nrow(target_site.LL), function(i) {
    # this_target <- target_site[i]
    # target_site.LL <- merge(this_target, site.all, by = c("species", "site_id"))

    # subset reference sites of same species
    site.all.spc <- ref_sites[species == target_site.LL[i]$species,
                              .N, by = .(uid_site, site_id, species, latitude, longitude)]

    # compute distance matrix (target to all ref sites)
    dist.mat <- geosphere::distm(
      target_site.LL[i][, c("longitude", "latitude")],
      site.all.spc[, c("longitude", "latitude")],
      fun = geosphere::distGeo
    )

    site.all.spc$dist_to_chk_m <- as.vector(t(dist.mat))
    site.all.spc$dist_to_chk_km <- round(site.all.spc$dist_to_chk_m / 1000, 1)
    setorder(site.all.spc, dist_to_chk_m)
    site.all.spc$ord <- 0:(nrow(site.all.spc) - 1)


    site.all.spc <- site.all.spc[ord <= scale.N_nbs,]
    scale.N_nbs.real <- max(site.all.spc$ord)


    site.closest <- site.all.spc[dist_to_chk_km <= scale.max_dist_km, ]


    # if no neighbors were selected
    if (nrow(site.closest) == 1) {

      message(
        "No neighbours were found within ", scale.max_dist_km,
        " km. You may need to increase scale.max_dist_km."
      )


      rw.median <- data.table()
      dt.plots <- list()
      }else{

        rw.closest <- merge(ref_sites,
                        site.closest[, .(ord,uid_site)],
                        by = "uid_site")

    # compute median stats
    med.site <- rw.closest[
      , .(N = .N, rw.median = median(rw_mm), yr.mn = min(year), yr.max = max(year)),
      by = .(ord, species, uid_site, longitude, latitude, uid_radius)
    ][
      , .(Ncores = .N, rw.median = round(median(rw.median), 2),
          yr.mn = min(yr.mn), yr.max = max(yr.max)),
      by = .(ord, species, uid_site, longitude, latitude)
    ]

    med.site.yr <- rw.closest[
      , .(N = .N, rw.median = median(rw_mm)),
      by = .(ord, species, uid_site, uid_radius, year)
    ][
      , .(Ncores = .N, rw.median = median(rw.median)),
      by = .(ord, species, uid_site, year)
    ]

    setorder(med.site.yr, ord, uid_site, year)

    # ratio to target site (ord == 0)
    med.site$rw.ratio <- round(med.site[ord == 0]$rw.median / med.site$rw.median, 2)
    med.site$size_class <- cut(
      med.site$rw.ratio,
      breaks = c(-Inf, 0.1, 0.2, 0.5, 2, 5, 10, Inf),
      labels = c("< 0.1", "0.1 - 0.2", "0.2 - 0.5", "0.5 - 2", "2 - 5", "5 - 10", "> 10"),
      include.lowest = TRUE
    )

    if (any(is.na(med.site$size_class))) {
      warning("Some values in rw.ratio are outside the defined breaks.")
    }

    rw.median <- data.table(
      med.site[ord == 0, .(uid_site, species, rw.median)],
      med.site[ord > 0, .(scale.N_nbs = .N,
                          rw.min.nbs = min(rw.median),
                          rw.max.nbs = max(rw.median),
                          rw.median.nbs = median(rw.median))]
    )[, ratio_median := round(rw.median / rw.median.nbs, 2)]
    dt.plots <-list(med.site.yr = med.site.yr, med.site = med.site)
    }
    result <- list(
      dt.plots = dt.plots,
      ratio.median = rw.median,
      scale.parms = c(
        scale.label_data_site = target_site.LL[i]$site_id,
        scale.label_data_ref = scale.label_data_ref,
        scale.max_dist_km = scale.max_dist_km,
        scale.N_nbs = scale.N_nbs.real
      )
    )
    class(result) <- "cfs_scale"
    return(result)
  })

  names(results_list) <- paste(target_site.LL$species, target_site.LL$site_id, sep = "_")
  class(results_list) <- "cfs_scale_list"
  return(results_list)
}





# #############################################################

#' Auto-batching Cross-Correlation Function for Pairwise Analysis
#'
#' Internal helper function that performs pairwise cross-correlation analysis
#' with automatic batch sizing based on available system memory and CPU cores.
#'
#' @param dt.trt_wide A data.table in wide format with treated time series as columns
#' @param qa.max_lag Integer. Maximum lag for cross-correlation function (default: 10)
#' @param mem_target Numeric. Proportion of free memory to use (0-1, default: 0.6)
#'
#' @return A data.table containing pairwise CCF results with columns for time series
#'   pairs, lag values, and correlation coefficients
#'
#' @details
#' This function automatically:
#' \itemize{
#'   \item Detects available system memory (cross-platform)
#'   \item Estimates memory requirements per CCF operation
#'   \item Calculates optimal batch size with safety margins
#'   \item Configures parallel processing based on available cores
#'   \item Processes pairs in batches with garbage collection
#' }
#'
#' Memory detection works on:
#' \itemize{
#'   \item Linux: reads /proc/meminfo
#'   \item macOS: uses sysctl
#'   \item Windows: defaults to 16GB
#' }
#'
#' @keywords internal
#' @noRd
run_safe_ccf <- function(dt.trt_wide, qa.max_lag = 10, mem_target = 0.6) {
  # Generate all column pairs
  col_pairs <- combn(names(dt.trt_wide), 2, simplify = FALSE)
  n_pairs <- length(col_pairs)

  # message(sprintf("Computing CCF for %d pairs across %d columns", n_pairs, ncol(dt.trt_wide)))

  # Detect available cores
  available_cores <- parallel::detectCores(logical = FALSE)
  available_cores <- max(1, available_cores - 1)

  # Choose the best parallel plan
  if (.Platform$OS.type == "unix") {
    plan(multicore, workers = available_cores)
  } else {
    plan(multisession, workers = min(available_cores, 4))
  }

  # Estimate available memory (cross-platform)
  total_mem <- tryCatch({
    if (.Platform$OS.type == "unix") {
      if (file.exists("/proc/meminfo")) {
        as.numeric(system("awk '/MemTotal/ {print $2}' /proc/meminfo", intern = TRUE)) * 1024
      } else {
        # macOS fallback
        as.numeric(system("sysctl -n hw.memsize", intern = TRUE))
      }
    } else {
      # Windows - default to 16GB
      16e9
    }
  }, error = function(e) {
    warning("Could not detect system memory, using 16GB default")
    16e9
  })

  used_mem <- as.numeric(pryr::mem_used())
  free_mem <- max(0, total_mem - used_mem)
  usable_mem <- free_mem * mem_target

  # Estimate memory per pair (sample from first few pairs)
  test_sample <- min(5, length(col_pairs))
  mem_estimates <- numeric(test_sample)

  for (i in seq_len(test_sample)) {
    test_pair <- col_pairs[[i]]
    test_ccf <- tryCatch(
      ccf(dt.trt_wide[[test_pair[1]]], dt.trt_wide[[test_pair[2]]],
          lag.max = qa.max_lag, plot = FALSE),
      error = function(e) NULL
    )
    if (!is.null(test_ccf)) {
      mem_estimates[i] <- as.numeric(pryr::object_size(test_ccf))
    }
  }

  mem_per_pair <- mean(mem_estimates[mem_estimates > 0], na.rm = TRUE)
  if (is.na(mem_per_pair) || mem_per_pair == 0) {
    # warning("Could not estimate memory per pair, using default")
    mem_per_pair <- 1e5  # 100KB default
  }

  # Auto-adjust batch size with safety buffer
  est_batch_size <- floor(usable_mem / (mem_per_pair * available_cores * 1.5))
  batch_size <- max(50, min(est_batch_size, 10000))  # clamp between 50-10000

  # message(sprintf("Memory: %.1f GB total, %.1f GB used, %.1f GB usable (%.0f%% target)",
  #                 total_mem/1e9, used_mem/1e9, usable_mem/1e9, mem_target * 100))
  # message(sprintf("Using %d cores with batch size %d", available_cores, batch_size))

  # Split pairs into batches
  pair_batches <- split(col_pairs, ceiling(seq_along(col_pairs) / batch_size))
  # message(sprintf("Processing %d batches...", length(pair_batches)))

  # Run with progress
  message("Progress pair-wise ccf...")

  dt.ccf.pairs <- future_map(pair_batches, function(batch) {
    res <- process_batch(batch, dt.wide = dt.trt_wide, qa.max_lag = qa.max_lag)
    gc(verbose = FALSE)
    res
  }, .progress = TRUE) %>% rbindlist()

  # cat("\n")
  dt.ccf.pairs
}

#' Quality assessment: radius alignment
#'
#' Performs comprehensive quality assurance analysis for tree-ring crossdating
#' using pairwise cross-correlation functions and iterative chronologies
#' refinement with automatic memory-efficient batch processing.
#'
#' @param dt.input A data.table containing tree-ring measurements with required columns:
#'   \describe{
#'     \item{species}{Character. Species identifier (must be single species)}
#'     \item{SampleID}{Character/Integer. Unique sample identifier}
#'     \item{Year}{Integer. Year of measurement}
#'     \item{RawRing}{Numeric. Raw ring-width measurement}
#'     \item{RW_trt}{Numeric. Treated/detrended ring-width series}
#'   }
#' @param qa.label_data Character. Label identifier for the dataset (required)
#' @param qa.label_trt Character. Label identifier for the treatment method (required)
#' @param qa.max_lag Integer. Maximum lag for cross-correlation analysis (default: 10)
#' @param qa.max_iter Integer. Maximum iterations for chronologies refinement (default: 100)
#' @param qa.min_nseries Integer. Minimum number of series required (default: 100)
#' @param qa.blcrit Numeric. Borderline threshold criterion for quasi-pass classification (default: 0.1)
#' @param qa.mem_target Numeric. Proportion of free memory to use for batch processing
#'   (0-1, default: 0.6). Higher values use more memory but may be faster.
#'
#' @return An object of class \code{cfs_qa} containing:
#'   \describe{
#'     \item{dt.ccf}{data.table with CCF results and QA codes (\code{qa_code}) for each series}
#'     \item{dt.chron}{data.table with chronologies statistics}
#'     \item{dt.stats}{data.table with summary statistics per radius}
#'     \item{dt.plots}{List of data.tables formatted for plotting (raw and treated series, CCF plots)}
#'     \item{qa.parms}{List of parameters used in the analysis}
#'   }
#'
#' @details
#' The function performs a two-step quality assurance process:
#'
#' \strong{Step 1: Pairwise Cross-Correlation}
#' \itemize{
#'   \item Computes CCF for all pairs of treated series
#'   \item Uses auto-batching to manage memory efficiently
#'   \item Identifies initial qualified samples with max CCF at lag 0
#' }
#'
#' \strong{Step 2: Iterative chronologies Refinement}
#' \itemize{
#'   \item Computes chronologies from qualified samples
#'   \item Evaluates each sample by running CCF with the chronologies
#'   \item Iteratively refines the qualified samples until convergence
#' }
#'
#' \strong{QA Code Classification:}
#' \itemize{
#'   \item \code{pass}: Maximum correlation at lag 0
#'   \item \code{borderline}: Second highest correlation at lag 0 (within threshold)
#'   \item \code{pm1}: Maximum correlation at lag +/- 1 (slight misalignment)
#'   \item \code{highpeak}: Maximum at non-zero lag, >2x second highest
#'   \item \code{fail}: All other cases
#' }
#'
#' \strong{Auto-batching:}
#' The function automatically determines optimal batch size based on:
#' \itemize{
#'   \item Available system memory
#'   \item Number of CPU cores
#'   \item Estimated memory per CCF operation
#'   \item Safety margins to prevent out-of-memory errors
#' }
#'
#' @examples
#' \donttest{
#' # loading processed data
#' dt.samples_trt <- readRDS(system.file("extdata", "dt.samples_trt.rds", package = "growthTrendR"))

#' # data processing
#' dt.samples_long <- prepare_samples_clim(dt.samples_trt, calbai = FALSE)

#' # rename to the reserved column name
#' data.table::setnames(dt.samples_long,
#' c("sample_id", "year", "rw_mm"), c("SampleID", "Year" ,"RawRing"))

#' # assign treated series
#' # users can decide their own treated series
#' # for rhub::rhub_check() on macos VECTOR_ELT issues
#' data.table::setorder(dt.samples_long, SampleID, Year)
#' dt.samples_long$RW_trt <-
#'   ave(
#'   as.numeric(dt.samples_long$RawRing),
#'   dt.samples_long$SampleID,
#'   FUN = function(x)
#'   if (length(x) > 1L) c(NA_real_, diff(x)) else NA_real_
#'   )


#' # quality check on radius alignment based on the treated series
#' dt.qa <-CFS_qa(dt.input = dt.samples_long, qa.label_data = "demo-samples",
#' qa.label_trt = "difference", qa.min_nseries = 5)
#'}
#'
#' @seealso
#' \code{\link{ccf}} for cross-correlation function
#'
#'
#' @export

CFS_qa <- function(dt.input, qa.label_data = "", qa.label_trt = "",
                   qa.max_lag = 10, qa.max_iter = 100, qa.min_nseries = 100,
                   qa.blcrit = 0.1, qa.mem_target = 0.6) {

  check_optional_deps()

  # Input validation
  if (length(setdiff(c("species", "SampleID", "Year","RawRing", "RW_trt"), names(dt.input))) > 0) {
    stop("at least one of the mandatory columns (species, SampleID, Year, RawRing, RW_trt) doesn't exist, please check...")
  }
  if (qa.label_data == "") stop("please specify qa.label_data...")
  if ("RW_trt" %in% names(dt.input) & qa.label_trt == "") stop("please specify qa.label_trt...")
  if (length(unique(dt.input$species)) > 1) stop("only 1 species is allowed in the dataset...")
  if (length(unique(dt.input$SampleID)) < qa.min_nseries) {
    stop(paste0("please increase the sample size at minimum: ", qa.min_nseries))
  }
  if (nrow(dt.input[, .N, by = .(SampleID, Year)][N > 1]) > 0) {
    stop("SampleID-Year is not unique key, please check...")
  }
  # range <- detect_isolated_year_ranges(dt.input)
  # if (nrow(range[isolated == TRUE])) stop(paste(
  #   "please check the year range of samples: ",
  #   paste(range[isolated == TRUE]$SampleID, collapse = ", ")
  #   ))
  # Prepare data
  dt.rw_long <- dt.input[, c("SampleID", "Year","RawRing", "RW_trt")]
  setorder(dt.rw_long, SampleID, Year)

  dt.rw_long[, SampleID.chr := paste0("d_", SampleID)]
  setorder(dt.rw_long, SampleID.chr)
  sample.lst <- sort(unique(dt.rw_long$SampleID.chr))

  dt.rw_wide <- dcast(dt.rw_long[!is.na(RW_trt)], Year ~ SampleID.chr, value.var = "RawRing")
  dt.trt_wide <- dcast(dt.rw_long[!is.na(RW_trt)], Year ~ SampleID.chr, value.var = "RW_trt")

  setcolorder(dt.trt_wide, c("Year", sample.lst))

  dt.trt_wide.o <- copy(dt.trt_wide)
  dt.trt_wide$Year <- NULL

  # STEP 1: Pair-wise CCF with auto-batching
  message(strrep("=", 60))
  message("STEP 1: Computing pair-wise CCF with auto-batching")
  message(strrep("=", 60))

  dt.ccf.pairs <- run_safe_ccf(dt.trt_wide, qa.max_lag = qa.max_lag, mem_target = qa.mem_target)

  result_dt.sel <- dt.ccf.pairs[max_lag == 0 & !is.na(max_ccf)]

  ts.sel <- data.table(ts.sel = unlist(c(result_dt.sel$ts1, result_dt.sel$ts2)))
  ts.sel <- ts.sel[, .N, by = .(ts.sel)]
  id.candi <- unique(ts.sel$ts.sel)

  message(sprintf("Step 1 complete: %d out of %d initial candidate samples identified",
                  length(id.candi), ncol(dt.trt_wide)))

  # STEP 2: Iterative refinement
  message("\n", strrep("=", 60))
  message("STEP 2: Iterative refinement of chronologies")
  message(strrep("=", 60))

  s2.end <- FALSE
  i.iter <- 1

  while (!s2.end & i.iter <= qa.max_iter) {
    # Mean of treated chronology as chronologies
    dt.s2.avg <- dt.rw_long[SampleID.chr %in% id.candi][,
                                                        .(.N, mean.rw = mean(RawRing), mean.rw_trt = mean(RW_trt, na.rm = TRUE)),
                                                        by = .(Year)
    ]

    dt.s2.wide <- merge(dt.s2.avg[, c("Year", "mean.rw_trt")], dt.trt_wide.o, by = "Year")

    # CCF of all samples with the chronologies
    dt.s2.ccf <- rbindlist(lapply(3:ncol(dt.s2.wide), ccf_avg,
                                  data = dt.s2.wide, blcrit = qa.blcrit,
                                  lag.max = qa.max_lag, qa_code = "Fail"))

    # Valid samples for chronologies
    id.pass <- unique(dt.s2.ccf[qa_code == "pass"]$SampleID.chr)
    s2.end <- length(setdiff(union(id.candi, id.pass), intersect(id.candi, id.pass))) == 0

    # message(paste0(i.iter, " N.pass: ", length(id.candi)))

    if (s2.end != TRUE) {
      id.candi <- id.pass
      i.iter <- i.iter + 1
    }
  }

  setorder(dt.s2.ccf, SampleID.chr)
  dt.s2.avg[, c("success", "iteration") := .(s2.end, i.iter)]

  message(sprintf("Step 2 complete: Converged in %d iterations (%s)",
                  i.iter, ifelse(s2.end, "success", "max iterations reached")))

  # Prepare output data
  dt.raw.series <- merge(dt.s2.avg[, c("Year", "mean.rw")], dt.rw_wide, by = "Year")
  dt.trt.series <- merge(dt.s2.avg[, c("Year", "mean.rw_trt")], dt.trt_wide.o, by = "Year")

  setcolorder(dt.raw.series, c("Year", "mean.rw", sample.lst))
  setcolorder(dt.trt.series, c("Year", "mean.rw_trt", sample.lst))

  # Prepare CCF bar plots data
  dt.trt.ccf <- copy(dt.s2.ccf)
  dt.ccf.idlabel <- dt.trt.ccf[ccf.ord == 1, c("SampleID.chr", "qa_code", "lag")]
  dt.ccf.idlabel[, id.label := paste0(str_sub(SampleID.chr, 3, -1), "$", qa_code, "$", lag)]
  dt.trt.ccf <- merge(dt.trt.ccf, dt.ccf.idlabel[, c("SampleID.chr", "id.label")], by = "SampleID.chr")
  dt.trt.ccf <- dcast(dt.trt.ccf, lag ~ id.label, value.var = "acf.trt")

  idlabel.lst <- sort(unique(dt.ccf.idlabel$id.label))
  idlabel.lst2 <- str_split_fixed(idlabel.lst, "\\$", 3)[, 1]

  if (!all.equal(idlabel.lst2, str_split_fixed(sample.lst, "\\_", 2)[, 2])) {
    message("check the order of id.label in dt.ccf.idlabel")
  }

  setcolorder(dt.trt.ccf, c("lag", idlabel.lst))

  dt.raw.ccf <- rbindlist(lapply(3:ncol(dt.raw.series), ccf_avg, data = dt.raw.series))
  dt.raw.ccf <- dcast(dt.raw.ccf, lag ~ SampleID.chr, value.var = "acf.trt")
  setcolorder(dt.raw.ccf, c("lag", sample.lst))

  # Statistics per radius
  dt.s2.ccf[, SampleID := str_split_fixed(SampleID.chr, "\\_", 2)[, 2]]

  dt.radii <- dt.rw_long[, .(
    N = .N,
    rw.mean = mean(RawRing),
    rw.sd = sd(RawRing),
    rw.min = min(RawRing),
    rw.max = max(RawRing),
    ymin = min(Year),
    ymax = max(Year),
    ar1_rw = acf(RawRing, plot = FALSE)$acf[2]
  ), by = .(SampleID.chr)]

  acf.trt <- dt.rw_long[!is.na(RW_trt), .(
    ar1_trt = round(acf(RW_trt, plot = FALSE)$acf[2], 2)
  ), by = .(SampleID.chr)]

  # Correlations
  dt_wide.rw <- merge(dt.s2.avg[, c("Year", "mean.rw")], dt.rw_wide, by = "Year")
  dt_wide.trt <- merge(dt.s2.avg[, c("Year", "mean.rw_trt")], dt.trt_wide.o, by = "Year")

  dt.cor <- merge(cor_avg(dt_wide.rw), cor_avg(dt_wide.trt), by = "SampleID.chr")

  stats_radii <- merge(dt.radii, acf.trt, by = "SampleID.chr")
  stats_radii <- merge(stats_radii, dt.cor, by = "SampleID.chr")
  stats_radii <- merge(dt.s2.ccf[lag == 0, c("SampleID", "SampleID.chr", "qa_code")],
                       stats_radii, by = "SampleID.chr")

  dt.s2.ccf[, SampleID.chr := NULL]
  setcolorder(dt.s2.ccf, "SampleID")
  stats_radii[, SampleID.chr := NULL]

  # Reset to sequential
  plan(sequential)

  # Add species column
  dt.s2.ccf <- data.table(species = unique(dt.input$species), dt.s2.ccf)
  dt.s2.avg <- data.table(species = unique(dt.input$species), dt.s2.avg)
  stats_radii <- data.table(species = unique(dt.input$species), stats_radii)

  qa_code <- data.frame(
    qa_code = c("pass", "borderline", "pm1", "highpeak", "fail"),
    Description = c(
      "The maximum correlation occurs at lag 0",
      "The correlation at lag 0 ranks as the second highest, and its difference from the maximum remains within a predefined threshold, categorizing as a quasi-pass",
      "The maximum correlation occurs at lag 1 or -1, suggesting slight misalignment.",
      "The maximum correlation occurs at a non-zero lag and is more than twice the second-highest value, potentially signaling an issue",
      "All other measurements that do not fit into the aforementioned categories fall under this classification."
    )
  )

  result <- list(
    ts.sel.step1 = ts.sel,
    dt.ccf = dt.s2.ccf,
    dt.chron = dt.s2.avg,
    dt.stats = stats_radii,
    dt.plots = list(
      dt.trt.series = dt.trt.series,
      dt.raw.series = dt.raw.series,
      dt.trt.ccf = dt.trt.ccf,
      dt.raw.ccf = dt.raw.ccf
    ),
    qa.parms = list(
      qa.label_data = qa.label_data,
      qa.label_trt = qa.label_trt,
      qa.max_lag = qa.max_lag,
      qa.max_iter = qa.max_iter,
      qa.min_nseries = qa.min_nseries,
      qa.blcrit = qa.blcrit,
      qa.mem_target = qa.mem_target,
      qa.code_desc = qa_code
    )
  )

  class(result) <- "cfs_qa"

  message("\n", strrep("=", 60))
  message("CFS_qa analysis complete!")
  message(strrep("=", 60))

  return(result)
}

#' Detect Isolated Year Ranges for Samples
#'
#' This function summarizes year ranges for each sample (identified by uid_radius)
#' and flags samples as "isolated" if their year ranges fall outside the main cluster
#' defined by quantiles.
#'
#' @param df A data frame containing at least two columns:
#'   \code{uid_radius} (sample ID) and \code{year} (numeric year).
#' @param q_low Numeric, the lower quantile used to define the main cluster (default 0.25).
#' @param q_high Numeric, the upper quantile used to define the main cluster (default 0.75).
#'
#'
#' @keywords internal
#' @noRd
detect_isolated_year_ranges <- function(df, q_low = 0.25, q_high = 0.75) {
  range <- df %>%
    group_by(SampleID) %>%
    summarise(
      N = n(),
      year_min = min(Year, na.rm = TRUE),
      year_max = max(Year, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      main_min = quantile(year_min, q_low),
      main_max = quantile(year_max, q_high),
      isolated = year_max < main_min | year_min > main_max
    )
  range<- as.data.table(range)
  isolated_rows <- range[isolated == TRUE]

  if (nrow(isolated_rows) > 0) {

    message("\nMain-stream year range:")
    message(unique(range$main_min), " - ", unique(range$main_max))

    message("\n=== Isolated samples ===")
    # Convert data.table to text lines for safe printing
    message(paste(capture.output(isolated_rows[, .(SampleID, year_min, year_max)])),
            sep = "\n")
  }

  if (nrow(range[N < 30]) > 0) message(paste0("number of short series (30 or less) ", nrow(range[N < 30])))
  return(range)
}
