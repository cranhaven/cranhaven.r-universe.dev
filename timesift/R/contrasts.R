#' Compare every grain against a learner's best one
#'
#' A mixed model on the per-cell scores of one learner, `score ~ grain + (1 | variable) +
#' (1 | fold)`, fitted by restricted maximum likelihood. Every grain is then compared against the
#' reference by Dunnett's many-to-one procedure, which corrects for the comparisons made without
#' correcting for pairs nobody asked about.
#'
#' The design is balanced across variables, folds and grains, so each grain is compared within a
#' variable and within a fold and the variation between variables cancels from the comparison. That
#' is what lets a difference of 0.015 hold up where absolute skill ranges across variables by ten
#' times as much.
#'
#' Taking the best-observed grain as the reference is a choice that favours the reference, so read
#' this beside the paired differences [paired_contrast()] gives, which single out no grain.
#'
#' @param ladder A [grain_ladder()] result.
#' @param learner Which learner's grid to fit. The only one in the ladder by default.
#' @param reference The grain every other is compared against. The learner's best by default.
#' @param adjust Multiplicity adjustment passed to `emmeans::contrast()`.
#'
#' @return A data frame of one row per grain: the estimated marginal mean difference from the
#'   reference, its interval, and the adjusted p-value. Needs `lme4`, `lmerTest` and `emmeans`.
#'
#' @examplesIf all(vapply(c("lme4", "lmerTest", "emmeans"), requireNamespace, logical(1), quietly = TRUE))
#' \donttest{
#' set.seed(1)
#' t <- seq(as.POSIXct("2021-09-01", tz = "UTC"), by = "hour", length.out = 24 * 200)
#' units <- sprintf("p%03d", 1:60)
#' warmth <- rnorm(60)
#' d <- data.frame(
#'   plot = rep(units, each = length(t)), t = rep(t, length(units)),
#'   temp = as.numeric(vapply(warmth, function(w) 1.5 * w + rnorm(length(t), sd = 20),
#'                            numeric(length(t)))))
#' y <- matrix(rbinom(60 * 6, 1, plogis(3 * as.numeric(outer(warmth, rep(c(1, -1), 3))))),
#'             ncol = 6, dimnames = list(units, paste0("sp", 1:6)))
#' x <- grain_matrix(d, plot, t, temp, grain = c("day", "week", "month"))
#' lad <- grain_ladder(x, y, elasticnet(), folds = fold_map(y, v = 5), verbose = FALSE)
#' grain_contrasts(lad)
#' }
#'
#' @export
grain_contrasts <- function(ladder, learner = NULL, reference = NULL, adjust = "mvt") {
  for (p in c("lme4", "lmerTest", "emmeans")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("`grain_contrasts()` needs ", p, ". Install it with install.packages(\"", p, "\").",
           call. = FALSE)
    }
  }
  if (!inherits(ladder, "timesift_ladder")) {
    stop("expected a grain_ladder() result, got ", class(ladder)[1L], ".", call. = FALSE)
  }
  arms <- unique(ladder$learner)
  if (is.null(learner)) {
    if (length(arms) != 1L) {
      stop("this ladder holds ", length(arms), " learners; name the one to fit: ",
           paste(arms, collapse = ", "), ".", call. = FALSE)
    }
    learner <- arms[1L]
  }
  d <- ladder[ladder$learner == learner & !is.na(ladder$score), , drop = FALSE]
  if (!nrow(d)) {
    stop("no scored cell for learner \"", learner, "\".", call. = FALSE)
  }
  grains <- unique(ladder$grain[ladder$learner == learner])
  if (length(grains) < 2L) {
    stop("a grain contrast needs at least two grains, this ladder has ", length(grains), ".",
         call. = FALSE)
  }
  if (is.null(reference)) {
    s <- summary(ladder)
    reference <- s$grain[s$learner == learner & s$best]
    if (length(reference) != 1L) {
      stop("no grain of learner \"", learner, "\" has a score to take as the reference. Name ",
           "`reference`.", call. = FALSE)
    }
  }
  if (length(reference) != 1L || !reference %in% grains) {
    stop("\"", reference, "\" is not a grain of this ladder.", call. = FALSE)
  }

  d$grain <- factor(d$grain, levels = c(reference, setdiff(grains, reference)))
  d$variable <- factor(d$variable)
  d$fold <- factor(d$fold)
  fit <- lmerTest::lmer(score ~ grain + (1 | variable) + (1 | fold), data = d, REML = TRUE)
  # emmeans computes Satterthwaite's degrees of freedom only up to 3000 observations by default and
  # falls back to asymptotic intervals above that, under other column names; a ladder of a hundred
  # variables, ten folds and seven grains is past it. The limit is the ladder's own size, so every
  # contrast is read the way the documentation says.
  means <- emmeans::emmeans(fit, "grain", lmer.df = "satterthwaite", lmerTest.limit = nrow(d))
  # trt.vs.ctrl gives every grain minus the reference, so a grain scoring below its learner's
  # best carries a negative difference, which is the direction the comparison is read in.
  comparison <- emmeans::contrast(means, method = "trt.vs.ctrl", ref = 1L, adjust = adjust)
  out <- as.data.frame(stats::confint(comparison))
  out$p_value <- as.data.frame(comparison)$p.value
  names(out)[names(out) == "estimate"] <- "diff"
  names(out)[names(out) == "lower.CL"] <- "lower"
  names(out)[names(out) == "upper.CL"] <- "upper"
  # One row per grain other than the reference, in the order of the factor's levels. The grain is
  # read from there rather than parsed back out of the contrast's label, which emmeans writes with
  # parentheses around a level holding a character it treats as an operator.
  out$grain <- levels(d$grain)[-1L]
  out$learner <- learner
  out$reference <- reference
  rownames(out) <- NULL
  out[c("learner", "grain", "reference", "diff", "lower", "upper", "p_value")]
}
