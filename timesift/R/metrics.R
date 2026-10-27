#' The true skill statistic
#'
#' Sensitivity plus specificity minus one, at the threshold that maximises it. This is the metric
#' species distribution modelling reports. The shipped presence-absence response is scored by
#' [roc_auc()] instead, which chooses no threshold, and TSS is reported beside it.
#'
#' A cut may only fall between distinct predictions: units sharing a prediction are decided
#' together, so the same score comes back whatever order they arrived in. A cell holding only
#' presences or only absences has no skill to measure and returns `NA` rather than a number.
#'
#' The threshold is chosen on the same units the score is then read on, which is how the metric is
#' defined in the literature and how it is defined here, and it inflates the level where presences
#' are thin. [tss_inflation()] measures that inflation for a given design. How large it is depends
#' on how a model's predictions are distributed as well as on the design, so two models of equal
#' skill scored on the same cells can carry different inflations, and a paired difference in TSS is
#' not free of it. Given a `threshold` learned elsewhere, the score
#' is read at that cut instead, which is what [select_grain()] reports under `threshold =` with a
#' cut learned on the inner folds.
#'
#' @param y Observed presence-absence, `0`/`1` or logical.
#' @param p Predicted scores for the same units, in the same order. Higher means presence.
#' @param threshold `NULL` for the maximum over every cut, or one cut, presence being predicted at
#'   `p >= threshold`.
#'
#' @return One number, or `NA` where the cell defines none.
#'
#' @examples
#' tss(c(0, 0, 1, 1), c(0.1, 0.2, 0.8, 0.9))
#' tss(c(0, 0, 1, 1), c(0.9, 0.8, 0.2, 0.1))
#' tss(c(0, 0, 0, 0), c(0.1, 0.2, 0.8, 0.9))
#' tss(c(0, 0, 1, 1), c(0.1, 0.6, 0.8, 0.9), threshold = 0.5)
#'
#' @export
tss <- function(y, p, threshold = NULL) {
  if (!is.null(threshold)) {
    y <- .check_labels(y, p)
    if (is.null(y) || length(threshold) != 1L || !is.finite(threshold)) {
      return(NA_real_)
    }
    hit <- as.numeric(p) >= threshold
    return(mean(hit[y == 1L]) - mean(hit[y == 0L]))
  }
  s <- .sweep(y, p)
  if (is.null(s)) {
    return(NA_real_)
  }
  max(s$tp / s$n_pos - s$fp / s$n_neg)
}

#' The area under the ROC curve
#'
#' The whole curve rather than its best point. TSS is a maximum over cuts, so a small change in a
#' prediction often moves it not at all and then moves it a long way; the area responds to every
#' reordering, which is what makes it the steadier reading when many rescorings are compared, as in
#' [occlusion()]. Tied predictions take the average rank.
#'
#' @inheritParams tss
#'
#' @return One number, or `NA` where the cell defines none.
#'
#' @examples
#' roc_auc(c(0, 0, 1, 1), c(0.1, 0.2, 0.8, 0.9))
#'
#' @export
roc_auc <- function(y, p) {
  y <- .check_labels(y, p)
  if (is.null(y)) {
    return(NA_real_)
  }
  n_pos <- sum(y == 1L)
  n_neg <- length(y) - n_pos
  (sum(rank(p)[y == 1L]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

#' Average precision
#'
#' The area under the precision-recall curve, as the step sum over the distinct predictions: at
#' each, the precision of calling every unit at or above it a presence, weighted by the share of
#' presences that cut adds. Units sharing a prediction enter together, so the order they arrived in
#' does not move the score. Its floor is the prevalence rather than one half, which makes it the
#' reading of how well presences are ranked above absences where presences are rare, and it is
#' reported beside [roc_auc()] for that reason.
#'
#' @inheritParams tss
#'
#' @return One number, or `NA` where the cell defines none.
#'
#' @examples
#' average_precision(c(0, 0, 1, 1), c(0.1, 0.2, 0.8, 0.9))
#' average_precision(c(0, 1, 0, 1), c(0.1, 0.2, 0.8, 0.9))
#'
#' @export
average_precision <- function(y, p) {
  s <- .sweep(y, p)
  if (is.null(s)) {
    return(NA_real_)
  }
  precision <- s$tp / (s$tp + s$fp)
  gained <- diff(c(0, s$tp)) / s$n_pos
  sum(gained * precision)
}

#' Cohen's kappa, and where two models disagree
#'
#' A chance-corrected agreement rate on a two-by-two table. Read against the observed response it
#' is a skill score beside [tss()]; read between two models' decisions on the same units it says
#' where the two part company.
#'
#' Kappa is read at a threshold rather than maximised over one, so the rule that picks the
#' threshold is part of the statistic. `"youden"` is the operating point [tss()] is defined at and
#' inherits its selection bias; `"kappa"` maximises kappa itself and inherits the analogous bias;
#' `"prevalence"` cuts at the observed presence rate, which selects nothing from the labels and is
#' the rule to read an absolute level at.
#'
#' @inheritParams tss
#' @param rule Threshold rule: `"youden"`, `"kappa"` or `"prevalence"`.
#'
#' @return For `kappa_score()`, one number. For `decision_threshold()`, the cut itself, applied as
#'   `p >= threshold`. For `model_agreement()`, a one-row data frame carrying the agreement kappa
#'   between two models cut by the same rule, the share of units they decide differently, and how
#'   often each is the one that is right there.
#'
#' @examples
#' y <- c(0, 0, 0, 1, 1, 1)
#' kappa_score(y, c(0.1, 0.2, 0.6, 0.4, 0.8, 0.9))
#' model_agreement(y, c(0.1, 0.2, 0.6, 0.4, 0.8, 0.9), c(0.2, 0.1, 0.3, 0.7, 0.9, 0.8))
#'
#' @export
kappa_score <- function(y, p, rule = c("youden", "kappa", "prevalence")) {
  thr <- decision_threshold(y, p, rule)
  if (!is.finite(thr)) {
    return(NA_real_)
  }
  .kappa_table(.labels(y), as.integer(p >= thr))
}

#' @rdname kappa_score
#' @export
decision_threshold <- function(y, p, rule = c("youden", "kappa", "prevalence")) {
  rule <- match.arg(rule)
  s <- .sweep(y, p)
  if (is.null(s)) {
    return(NA_real_)
  }
  if (rule == "prevalence") {
    return(unname(stats::quantile(as.numeric(p), 1 - s$n_pos / (s$n_pos + s$n_neg), type = 7)))
  }
  if (rule == "youden") {
    return(s$thr[which.max(s$tp / s$n_pos - s$fp / s$n_neg)])
  }
  n <- s$n_pos + s$n_neg
  fn <- s$n_pos - s$tp
  tn <- s$n_neg - s$fp
  po <- (s$tp + tn) / n
  pe <- ((s$tp + s$fp) * s$n_pos + (fn + tn) * s$n_neg) / n^2
  k <- ifelse(pe >= 1, -Inf, (po - pe) / (1 - pe))
  s$thr[which.max(k)]
}

#' @rdname kappa_score
#' @param p_a,p_b Two models' predictions for the same units.
#' @export
model_agreement <- function(y, p_a, p_b, rule = c("youden", "kappa", "prevalence")) {
  rule <- match.arg(rule)
  y <- .labels(y)
  ta <- decision_threshold(y, p_a, rule)
  tb <- decision_threshold(y, p_b, rule)
  if (!is.finite(ta) || !is.finite(tb)) {
    return(data.frame(kappa = NA_real_, n = length(y), n_disagree = NA_integer_,
                      share_disagree = NA_real_, a_right = NA_integer_, b_right = NA_integer_))
  }
  da <- as.integer(p_a >= ta)
  db <- as.integer(p_b >= tb)
  diff <- da != db
  data.frame(kappa = .kappa_table(da, db), n = length(y), n_disagree = sum(diff),
             share_disagree = mean(diff),
             a_right = sum(diff & da == y), b_right = sum(diff & db == y))
}

.kappa_table <- function(a, b) {
  n <- length(a)
  both <- sum(a == 1L & b == 1L)
  neither <- sum(a == 0L & b == 0L)
  a_only <- sum(a == 1L & b == 0L)
  b_only <- sum(a == 0L & b == 1L)
  po <- (both + neither) / n
  pe <- ((both + a_only) * (both + b_only) + (b_only + neither) * (a_only + neither)) / n^2
  if (pe >= 1) NA_real_ else (po - pe) / (1 - pe)
}

# The response as 0/1 integers, refused where it is not one: checked on the values as given,
# because coercing first would read 0.6 as 0 and pass a response that was never binary.
.labels <- function(y) {
  if (!all(y %in% c(0, 1))) {
    stop("`y` must be presence-absence, 0/1 or logical.", call. = FALSE)
  }
  as.integer(y)
}

.check_labels <- function(y, p) {
  y <- .labels(y)
  if (length(y) != length(p)) {
    stop("`y` and `p` must be the same length, got ", length(y), " and ", length(p), ".",
         call. = FALSE)
  }
  if (anyNA(p) || any(!is.finite(as.numeric(p)))) {
    return(NULL)
  }
  n_pos <- sum(y == 1L)
  if (n_pos == 0L || n_pos == length(y)) NULL else y
}

# The one place a score becomes a set of cuts. Ties are decided together, so a cut can only fall
# between distinct predictions; both languages read every threshold metric off this.
.sweep <- function(y, p) {
  y <- .check_labels(y, p)
  if (is.null(y)) {
    return(NULL)
  }
  p <- as.numeric(p)
  o <- order(p, decreasing = TRUE, method = "radix")
  ys <- y[o]
  ps <- p[o]
  keep <- c(ps[-length(ps)] != ps[-1L], TRUE)
  list(thr = ps[keep], tp = cumsum(ys)[keep], fp = cumsum(1L - ys)[keep],
       n_pos = sum(y == 1L), n_neg = sum(y == 0L))
}
