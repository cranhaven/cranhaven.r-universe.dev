#' Computes census of imaginary errors
#' @param x An object of class [barry_graph].
#' @param counter_type An integer indicating the type of census to compute (see details).
#' @details
#' We can also separate the counts as a function of whether the perceiver is looking
#' into all ties, only ties including them, or only ties not including them.
#' This is controlled by the \code{counter_type} argument:
#'
#' - 0: All ties
#' - 1: Only ties including the perceiver
#' - 2: Only ties not including the perceiver
#'
#' There are ten (10) values:
#' - (01) Accurate null
#' - (02) Partial false positive (null)
#' - (03) Complete false positive (null)
#' - (04) Partial false negative (assym)
#' - (05) Accurate assym
#' - (06) Mixed assym
#' - (07) Partial false positive (assym)
#' - (08) Complete false negative (full)
#' - (09) Partial false negative (full)
#' - (10) Accurate full
#' @return A data frame of class `"imaginary_census"` with columns `id`,
#'   `name`, and `value`.
#' @examples
#' data(krackhardt_advice)
#' data(krackhardt_advice_perceptions)
#'
#' n_people <- 21
#' advice_matrix <- matrix(0L, nrow = n_people, ncol = n_people)
#' advice_matrix[cbind(krackhardt_advice$from, krackhardt_advice$to)] <-
#'   krackhardt_advice$value
#'
#' krack_graph <- new_barry_graph(
#'   c(list(advice_matrix), krackhardt_advice_perceptions)
#' )
#' census <- count_imaginary_census(krack_graph)
#' head(census)
#' summary(census)
#' @export
count_imaginary_census <- function(x, counter_type = 0L) {
  
  stopifnot_barry_graph(x)
  res <- count_imaginary_census_cpp(x, counter_type)
  class(res) <- c("imaginary_census", class(res))
  res
  
}

#' Summarize an imaginary census
#'
#' Aggregates the per-perceiver motif counts returned by
#' [count_imaginary_census()] into totals per motif type.
#'
#' @param object A data frame returned by [count_imaginary_census()].
#' @param ... Currently ignored.
#'
#' @return A named numeric vector with total counts per motif type (sorted in
#'   decreasing order).
#'
#' @examples
#' data(krackhardt_advice)
#' data(krackhardt_advice_perceptions)
#'
#' n_people <- 21
#' advice_matrix <- matrix(0L, nrow = n_people, ncol = n_people)
#' advice_matrix[cbind(krackhardt_advice$from, krackhardt_advice$to)] <-
#'   krackhardt_advice$value
#'
#' krack_graph <- new_barry_graph(
#'   c(list(advice_matrix), krackhardt_advice_perceptions)
#' )
#' census <- count_imaginary_census(krack_graph)
#' summary(census)
#'
#' @export
summary.imaginary_census <- function(object, ...) {

  agg <- .aggregate_motifs(object)
  sort(agg, decreasing = TRUE)

}

#' Test imaginary census motifs against a null model
#'
#' Generates a null distribution of imaginary-census motif counts by
#' repeatedly sampling CSS networks with [sample_css_network()] and compares
#' the observed counts to this null distribution. Returns an S3 object of
#' class `"imaginarycss_test"` with `print`, `summary`, and `plot` methods.
#'
#' @param graph A `barry_graph` object.
#' @param n_sim Integer. Number of null-model simulations (default `100`).
#' @param alpha Numeric. Significance level for the two-sided test
#'   (default `0.05`).
#' @param counter_type Integer passed to [count_imaginary_census()]
#'   (default `0L`).
#'
#' @return An object of class `"imaginarycss_test"`, which is a list
#'   containing:
#' \describe{
#'   \item{results}{A data frame with columns `motif`, `observed`,
#'     `null_mean`, `null_sd`, `z_score`, `p_value`, and `significant`.}
#'   \item{observed}{Named numeric vector of observed motif totals.}
#'   \item{null_matrix}{Matrix of null motif totals (motifs x simulations).}
#'   \item{n_sim}{Number of simulations used.}
#'   \item{alpha}{Significance level used.}
#' }
#'
#' @examples
#' data(krackhardt_advice)
#' data(krackhardt_advice_perceptions)
#'
#' n_people <- 21
#' advice_matrix <- matrix(0L, nrow = n_people, ncol = n_people)
#' advice_matrix[cbind(krackhardt_advice$from, krackhardt_advice$to)] <-
#'   krackhardt_advice$value
#'
#' krack_graph <- new_barry_graph(
#'   c(list(advice_matrix), krackhardt_advice_perceptions)
#' )
#' res <- test_imaginary_census(krack_graph, n_sim = 50)
#' res
#' summary(res)
#' plot(res)
#'
#' @export
test_imaginary_census <- function(
    graph,
    n_sim        = 100L,
    alpha        = 0.05,
    counter_type = 0L
) {

  stopifnot_barry_graph(graph)

  # --- Observed motif totals ---
  census_obs  <- count_imaginary_census(graph, counter_type)
  observed    <- .aggregate_motifs(census_obs)

  # --- Null distribution ---
  null_matrix <- replicate(n_sim, {
    .aggregate_motifs(
      count_imaginary_census(
        new_barry_graph(sample_css_network(graph)),
        counter_type
      )
    )
  })

  # --- Significance tests ---
  motifs   <- names(observed)
  obs      <- as.numeric(observed)
  null_mat <- null_matrix[motifs, , drop = FALSE]
  null_mu  <- rowMeans(null_mat)
  null_sd  <- apply(null_mat, 1, sd)

  z_score <- ifelse(null_sd > 0, (obs - null_mu) / null_sd, 0)
  p_value <- 2 * pmin(
    rowMeans(null_mat <= obs),
    rowMeans(null_mat >= obs)
  )

  results <- data.frame(
    motif       = motifs,
    observed    = as.integer(obs),
    null_mean   = round(null_mu, 1),
    null_sd     = round(null_sd, 2),
    z_score     = round(z_score, 2),
    p_value     = round(p_value, 4),
    significant = abs(z_score) > stats::qnorm(1 - alpha / 2) &
                  p_value < alpha,
    row.names   = NULL,
    stringsAsFactors = FALSE
  )

  structure(
    list(
      results     = results,
      observed    = observed,
      null_matrix = null_matrix,
      n_sim       = n_sim,
      alpha       = alpha
    ),
    class = "imaginarycss_test"
  )
}

#' @rdname test_imaginary_census
#' @param x An object of class `"imaginarycss_test"`.
#' @param ... Currently ignored.
#' @return `print.imaginarycss_test()` returns the input object invisibly.
#'   Called for its side effect of printing a summary of significant motifs.
#' @export
print.imaginarycss_test <- function(x, ...) {

  cat("Imaginary-census significance test\n")
  cat("  Simulations:", x$n_sim, " | Alpha:", x$alpha, "\n\n")

  sig <- x$results[x$results$significant, , drop = FALSE]
  if (nrow(sig) > 0L) {
    cat("Significant motifs:\n")
    print(sig[order(-abs(sig$z_score)),
              c("motif", "observed", "z_score", "p_value")],
          row.names = FALSE)
  } else {
    cat("No significant deviations from the null model.\n")
  }

  invisible(x)
}

#' @rdname test_imaginary_census
#' @param object An object of class `"imaginarycss_test"`.
#' @return `summary.imaginarycss_test()` returns the input object invisibly.
#'   Called for its side effect of printing the full results table.
#' @export
summary.imaginarycss_test <- function(object, ...) {

  n_sig    <- sum(object$results$significant)
  n_motifs <- nrow(object$results)

  cat("Imaginary-census significance test\n")
  cat("  Simulations:", object$n_sim, " | Alpha:", object$alpha, "\n")
  cat("  Motifs tested:", n_motifs,
      "| Significant:", n_sig, "\n\n")

  print(
    object$results[
      order(-abs(object$results$z_score)),
      c("motif", "observed", "null_mean", "null_sd",
        "z_score", "p_value", "significant")
    ],
    row.names = FALSE
  )

  invisible(object)
}

#' @rdname test_imaginary_census
#' @param main Character. Plot title.
#' @return `plot.imaginarycss_test()` returns the input object invisibly.
#'   Called for its side effect of drawing a horizontal barplot of z-scores.
#' @export
#' @importFrom graphics barplot abline par
plot.imaginarycss_test <- function(x, main = "Motif Z-Scores vs Null", ...) {

  res  <- x$results[order(x$results$z_score), ]
  cols <- ifelse(res$significant, "steelblue", "grey80")
  z_crit <- stats::qnorm(1 - x$alpha / 2)

  old_par <- par(mar = c(4, 12, 3, 1))
  on.exit(par(old_par))

  barplot(
    res$z_score,
    horiz     = TRUE,
    las       = 1,
    names.arg = res$motif,
    col       = cols,
    border    = NA,
    xlab      = "Z-Score",
    main      = main,
    ...
  )
  abline(v = c(-z_crit, z_crit), lty = 2, col = "red")
  abline(v = 0, col = "grey50")

  invisible(x)
}

# -- internal helper -----------------------------------------------------------

#' Aggregate imaginary census counts by motif type
#' @noRd
.aggregate_motifs <- function(census) {
  motif <- gsub("\\([0-9]+\\)\\s*", "", census$name)
  res   <- tapply(census$value, motif, sum)
  # Convert from 1-d array to plain named vector
  setNames(as.numeric(res), names(res))
}
