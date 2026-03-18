#' Confidence Intervals for Species Delimitations Methods
#'
#' @description
#' These functions compute confidence intervals for various species delimitation
#' methods, including GMYC, bGMYC, Local Minima, and mPTP.
#'
#' @details
#' Both `gmyc_ci` and `bgmyc_ci` can take a very long time to proccess, depending on how many
#' posterior trees are provided. As an alternative, these analyses can be sped up significantly
#' by running in parallel using [plan][future::plan].
#'
#' @author
#' Pedro S. Bittencourt, Rupert A. Collins.
#'
#' @return
#' A vector containing the number of species partitions in `tr`, `dna` or `infile` followed by
#' the number of partitions in `posterior`, `reps` or `bootstraps`.
#'
#' @inheritParams splits::gmyc
#' @param posterior Trees from posterior. An object of class [multiphylo][ape::multiphylo].
#' @name confidence_intervals
#'
#' @examples
#' \donttest{
#'
#' # gmyc confidence intervals
#'
#' # compute values using multisession mode
#' {
#'   future::plan("multisession")
#'
#'   gmyc_res <- gmyc_ci(ape::as.phylo(geophagus_beast), geophagus_posterior)
#'
#'   # reset future parameters
#'   future::plan("sequential")
#' }
#'
#' # plot distribution
#' plot(density(gmyc_res))
#'
#' # tabulate
#' tibble::tibble(
#'   method = "gmyc",
#'   point_estimate = gmyc_res[1],
#'   CI_95 = as.integer(quantile(gmyc_res[-1], probs = c(0.025, 0.975))) |>
#'     stringr::str_flatten(collapse = "-"),
#'   CI_mean = as.integer(mean(gmyc_res[-1])),
#'   CI_median = as.integer(stats::median(gmyc_res[-1]))
#' )
#' }
#'
#' @export
#' @rdname confidence_intervals
gmyc_ci <- function(tr, posterior, method = "single", interval = c(0, 5)) {
  # combine trees and remove names
  trees <- c(tr, posterior) |> unname()

  # get a quiet GMYC function
  gmyc_quietly <- purrr::quietly(splits::gmyc)

  # run gmyc over trees and combine results
  gmyc_res <- furrr::future_map(trees,
    ~ {
      gmyc_quietly(.x, method = method, interval = interval) |>
        purrr::pluck("result") |>
        delimtools::gmyc_tbl() |>
        dplyr::pull(2) |>
        vctrs::vec_unique_count()
    },
    .options = furrr::furrr_options(seed = TRUE)
  ) |>
    purrr::list_c()
}

#' @inheritParams bGMYC::bgmyc.singlephy
#' @param ppcutoff Posterior probability threshold for clustering samples into species partitions.
#' See [bgmyc.point][bGMYC::bgmyc.point] for details. Default to 0.05.

#' @export
#' @rdname confidence_intervals
bgmyc_ci <- function(tr, posterior, ppcutoff = 0.05, mcmc, burnin,
                     thinning, py1 = 0, py2 = 2, pc1 = 0, pc2 = 2, t1 = 2,
                     t2 = 51, scale = c(20, 10, 5), start = c(1, 0.5, 50)) {
  # combine trees and remove names
  trees <- c(tr, posterior) |> unname()

  # get a quiet bGMYC function
  bgmyc_quietly <- purrr::quietly(bGMYC::bgmyc.singlephy)

  # run bGMYC over trees
  bgmyc_res <- furrr::future_map(trees,
    ~ {
      bgmyc_quietly(.x,
        mcmc = mcmc,
        burnin = burnin,
        thinning = thinning,
        py1 = py1,
        py2 = py2,
        pc1 = pc1,
        pc2 = pc2,
        t1 = t1,
        t2 = t2,
        scale = scale,
        start = start
      ) |>
        purrr::pluck("result") |>
        delimtools::bgmyc_tbl(ppcutoff = ppcutoff) |>
        dplyr::pull(2) |>
        vctrs::vec_unique_count()
    },
    .options = furrr::furrr_options(seed = TRUE)
  ) |>
    purrr::list_c()
}

#' @inheritParams boot_dna
#' @inheritParams locmin_tbl
#' @param reps Number of bootstrap replicates. Default to 100.
#' @param ... Further arguments to be passed to [dist.dna][ape::dist.dna].

#' @export
#' @rdname confidence_intervals
locmin_ci <- function(dna, block = 1, reps = 100, threshold = 0.01, haps = NULL, ...) {
  mat <- ape::dist.dna(dna, ...)

  boot_dist <- purrr::map(seq(1, reps), ~ delimtools::boot_dna(dna, block)) |>
    purrr::map(ape::dist.dna, ...)

  dist_ls <- append(boot_dist, list(mat), after = 0)

  locmin_quietly <- purrr::quietly(spider::localMinima)

  if (is.null(threshold)) {
    threshold <- locmin_quietly(mat) |>
      purrr::pluck("result", "localMinima", 1)

    cli::cli_warn("{cli::col_yellow({cli::symbol$warning})} Argument {.arg threshold} not provided.
                    Using {.val {threshold}} as cutoff value for clustering.")
  }

  if (threshold == "multiple") {
    threshold_ls <- purrr::map(
      dist_ls,
      ~ {
        locmin_quietly(.x) |>
          purrr::pluck("result", "localMinima", 1)
      }
    ) |>
      purrr::list_c()

    locmin_res <- purrr::map2(
      dist_ls, threshold_ls,
      ~ {
        delimtools::locmin_tbl(.x, .y, haps) |>
          dplyr::pull(2) |>
          vctrs::vec_unique_count()
      }
    ) |>
      purrr::list_c()
  } else {
    locmin_res <- purrr::map(
      dist_ls,
      ~ {
        .x |>
          delimtools::locmin_tbl(threshold, haps) |>
          dplyr::pull(2) |>
          vctrs::vec_unique_count()
      }
    ) |>
      purrr::list_c()
  }
}

#' @inheritParams mptp_tbl
#' @param bootstraps Bootstrap trees. An object of class [multiphylo][ape::multiphylo].

#' @export
#' @rdname confidence_intervals
mptp_ci <- function(infile, bootstraps, exe = NULL, outfolder = NULL,
                    method = c("multi", "single"), minbrlen = 0.0001,
                    webserver = NULL) {
  tr_ml <- ape::read.tree(file = infile)

  if (!methods::is(bootstraps, "multiPhylo")) {
    cli::cli_warn("{cli::col_yellow({cli::symbol$warning})} Argument {.arg bootstraps}
                    must have class {.cls multiPhylo}. Try reading trees using {.fn ape::read.tree}")
  }

  trees <- c(tr_ml, bootstraps) |> unname()

  names(trees) <- c("atr", paste0("bootstrap_tree_", seq(1, length(bootstraps))))

  if (is.null(outfolder)) {
    outfolder <- tempdir()
  }

  purrr::walk2(trees, names(trees), ~ {
    ape::write.tree(.x, file = glue::glue("{outfolder}/{.y}.nwk"))
  })

  infiles <- list.files(outfolder, pattern = ".nwk", full.names = TRUE) |>
    gtools::mixedsort() |>
    as.list()

  if (is.null(minbrlen)) {
    minbrlen <- delimtools::min_brlen(tr_ml) |> purrr::pluck("dist", 1)
  }

  if (is.numeric(minbrlen)) {
    mptp_res <- purrr::map(
      infiles,
      ~ {
        delimtools::mptp_tbl(
          infile = .x,
          exe = exe,
          outfolder = outfolder,
          method = method,
          minbrlen = minbrlen,
          webserver = webserver
        ) |>
          dplyr::pull(2) |>
          vctrs::vec_unique_count()
      }
    ) |>
      purrr::list_c()
  } else if (minbrlen == "multiple") {
    minbr_ls <- purrr::map(
      trees,
      ~ {
        delimtools::min_brlen(.x, verbose = FALSE)
      } |>
        purrr::pluck("dist", 1)
    )

    mptp_res <- purrr::map2(
      infiles, minbr_ls,
      ~ {
        delimtools::mptp_tbl(
          infile = .x,
          exe = exe,
          outfolder = outfolder,
          method = method,
          minbrlen = .y,
          webserver = webserver
        ) |>
          dplyr::pull(2) |>
          vctrs::vec_unique_count()
      }
    ) |>
      purrr::list_c()
  }
}
