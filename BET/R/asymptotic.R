# Reusable bivariate calibration, derived from the validated paper runtime.
# Existing R/plot.R and native BEAST implementations are deliberately unchanged.

.beast_asym_integer <- function(x, name, minimum, maximum = .Machine$integer.max) {
    if (!is.numeric(x) || is.complex(x) || length(x) != 1L ||
        !is.finite(x) || x != floor(x) || x < minimum || x > maximum)
        stop(name, " must be an integer between ", minimum, " and ", maximum, call. = FALSE)
    as.integer(x)
}

.beast_asym_config <- function(n, dep, subsample.percent, B, lambda, H, G) {
    n <- .beast_asym_integer(n, "n", 2L)
    dep <- .beast_asym_integer(dep, "dep", 1L, 5L)
    B <- .beast_asym_integer(B, "B", 1L)
    H <- .beast_asym_integer(H, "H", 2L)
    G <- .beast_asym_integer(G, "G", 2L)
    if (!is.numeric(subsample.percent) || is.complex(subsample.percent) ||
        length(subsample.percent) != 1L || !is.finite(subsample.percent) ||
        subsample.percent <= 0 || subsample.percent > 1)
        stop("subsample.percent must be in (0, 1]", call. = FALSE)
    # Frozen BEAST passes n*fraction through the Rcpp size_t conversion.
    r <- as.integer(n * subsample.percent)
    if (r < 2L) stop("The effective subsample size r must be at least 2", call. = FALSE)
    if (as.integer(n * (r/n)) != r)
        stop("The effective fraction r/n is not stable under native integer truncation for this n and r", call. = FALSE)
    if (n %% 2^dep != 0L || r %% 2^dep != 0L)
        stop("n and effective r must be divisible by 2^dep for the validated rank-bin construction", call. = FALSE)
    if (is.null(lambda)) lambda <- sqrt(log(2^(2 * dep)) / (8 * n))
    if (!is.numeric(lambda) || is.complex(lambda) || length(lambda) != 1L ||
        !is.finite(lambda) || lambda < 0)
        stop("lambda must be a finite nonnegative number", call. = FALSE)
    list(n = n, dep = dep, subsample.percent = as.double(subsample.percent),
         r = r, B = B, lambda = as.double(lambda), q = as.integer((2^dep - 1)^2),
         H = H, G = G)
}

.beast_asym_seed <- function(seed, stream, n = 0L, dataset = 0L) {
    as.integer((as.double(seed) + 10000019 * stream + 10007 * n + dataset) %% 2147483646 + 1)
}

.beast_asym_seed_value <- function(seed) {
    .beast_asym_integer(seed, "seed", 0L, 2147483646)
}

.beast_asym_local_rng <- function(seed, fun) {
    old_kind <- RNGkind()
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
    on.exit({
        do.call(RNGkind, as.list(old_kind))
        if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
        else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
            rm(".Random.seed", envir = .GlobalEnv)
    }, add = TRUE)
    RNGkind("Mersenne-Twister", "Inversion", "Rejection")
    set.seed(seed)
    fun()
}

.beast_asym_null_pair <- function(h, cfg, basis, seed) {
    set.seed(.beast_asym_seed(seed, 10L, cfg$n, h))
    X <- matrix(stats::rnorm(cfg$n * 2L), nrow = cfg$n)
    full <- .beast_asym_interaction_means(X, cfg$dep, list(1L, 2L), basis)
    sub <- numeric(cfg$q)
    for (b in seq_len(cfg$B)) {
        take <- sample.int(cfg$n, cfg$n, replace = FALSE)[seq_len(cfg$r)]
        sub <- sub + .beast_asym_interaction_means(X[take, , drop = FALSE], cfg$dep,
                                                 list(1L, 2L), basis)
    }
    sqrt(cfg$n - 1) * c(full, sub / cfg$B)
}

.beast_asym_covariance <- function(pairs) {
    .beast_asym_clip_covariance(stats::cov(pairs))
}

.beast_asym_clip_covariance <- function(gamma) {
    eig <- eigen((gamma + t(gamma))/2, symmetric = TRUE)
    root <- sweep(eig$vectors, 2L, sqrt(pmax(eig$values, 0)), `*`)
    list(raw = gamma, Gamma = tcrossprod(root), root = root,
         minimum_raw_eigenvalue = min(eig$values),
         clipped_eigenvalues = sum(eig$values < 0))
}

.beast_asym_map <- function(joint, q, c_n) {
    z <- .beast_asym_soft_threshold(joint[, seq_len(q), drop = FALSE], c_n)
    w <- .beast_asym_soft_threshold(joint[, q + seq_len(q), drop = FALSE], c_n)
    denom <- sqrt(rowSums(z^2))
    values <- numeric(nrow(joint))
    nonzero <- denom > 0
    values[nonzero] <- rowSums(z[nonzero, , drop = FALSE] *
                             w[nonzero, , drop = FALSE]) / denom[nonzero]
    values
}

.beast_asym_gaussian <- function(root, cfg, seed) {
    set.seed(.beast_asym_seed(seed, 11L, cfg$n))
    draws <- numeric(cfg$G)
    c_n <- sqrt(cfg$n - 1) * cfg$lambda
    # Keep the validated full-profile RNG matrix/chunk layout fixed.
    for (start in seq.int(1L, cfg$G, by = 1000L)) {
        count <- min(1000L, cfg$G - start + 1L)
        joint <- matrix(stats::rnorm(count * 2L * cfg$q), nrow = count) %*% t(root)
        draws[start:(start + count - 1L)] <- .beast_asym_map(joint, cfg$q, c_n)
    }
    draws
}

.beast_asym_critical <- function(draws, alpha) {
    i <- ceiling((1 - alpha) * length(draws))
    unname(sort(draws, partial = i)[i])
}

BEAST.asymptotic.calibrate <- function(n, dep = 3, subsample.percent = 3/16,
                                     B = 128, lambda = NULL, H = 1000,
                                     G = 10000, seed = NULL, ncores = 1) {
    cfg <- .beast_asym_config(n, dep, subsample.percent, B, lambda, H, G)
    ncores <- .beast_asym_integer(ncores, "ncores", 1L)
    if (ncores != 1L)
        stop("This release supports serial calibration only: use ncores = 1", call. = FALSE)
    requested_seed <- seed
    if (is.null(seed)) seed <- sample.int(2147483646L, 1L)
    seed <- .beast_asym_seed_value(seed)
    basis <- .beast_asym_cross_interaction_basis(2L, cfg$dep, list(1L, 2L))
    ans <- .beast_asym_local_rng(seed, function() {
        pairs <- do.call(rbind, lapply(seq_len(cfg$H), .beast_asym_null_pair,
                                      cfg = cfg, basis = basis, seed = seed))
        cv <- .beast_asym_covariance(pairs)
        draws <- .beast_asym_gaussian(cv$root, cfg, seed)
        list(Gamma = cv$Gamma, Gamma.raw = cv$raw, G0 = draws,
             nonzero.fraction = mean(draws != 0),
             minimum.raw.eigenvalue = cv$minimum_raw_eigenvalue,
             clipped.eigenvalues = cv$clipped_eigenvalues)
    })
    ans <- c(cfg, ans, list(configuration = cfg, schema.version = 1L,
         package.version = as.character(utils::packageVersion("BET")),
         method.id = "bivariate-rank-joint-gaussian-v1",
         scope = "bivariate-independence-continuous-margins",
         interaction.collection = "bivariate-cross-ascending-masks",
         index = list(1L, 2L), basis = basis, seed = seed,
         rng = list(requested.seed = requested_seed, master.seed = seed,
                    kind = c("Mersenne-Twister", "Inversion", "Rejection"),
                    null.seeds = vapply(seq_len(cfg$H), function(h)
                        .beast_asym_seed(seed, 10L, cfg$n, h), integer(1L)),
                    gaussian.seed = .beast_asym_seed(seed, 11L, cfg$n),
                    gaussian.chunk = 1000L, ncores = 1L)))
    class(ans) <- "BEASTAsymptoticCalibration"
    ans
}

.beast_asym_check_calibration <- function(calibration) {
    bad <- function(why) stop("Invalid BEAST asymptotic calibration: ", why, call. = FALSE)
    if (!inherits(calibration, "BEASTAsymptoticCalibration") || !is.list(calibration) ||
        anyDuplicated(names(calibration))) bad("unsupported object or duplicate fields")
    if (!identical(calibration$schema.version, 1L)) bad("unsupported schema version")
    if (!identical(calibration$method.id, "bivariate-rank-joint-gaussian-v1") ||
        !identical(calibration$scope, "bivariate-independence-continuous-margins")) bad("unsupported method or scope")
    required <- c("n", "dep", "subsample.percent", "r", "B", "lambda", "q", "H", "G")
    if (!all(required %in% names(calibration))) bad("missing configuration fields")
    cfg <- tryCatch(.beast_asym_config(calibration$n, calibration$dep,
        calibration$subsample.percent, calibration$B, calibration$lambda,
        calibration$H, calibration$G), error = function(e) bad(conditionMessage(e)))
    if (!identical(cfg, calibration$configuration) ||
        !identical(cfg, unclass(calibration)[required])) bad("configuration fields were changed or are inconsistent")
    if (!identical(calibration$index, list(1L, 2L)) ||
        !identical(calibration$interaction.collection, "bivariate-cross-ascending-masks") ||
        !identical(calibration$basis, .beast_asym_cross_interaction_basis(2L, cfg$dep, list(1L, 2L))))
        bad("interaction collection does not match bivariate cross interactions")
    for (field in c("Gamma", "Gamma.raw")) {
        mat <- calibration[[field]]
        if (!is.matrix(mat) || !is.numeric(mat) || is.complex(mat) ||
            !identical(dim(mat), c(2L * cfg$q, 2L * cfg$q)) ||
            any(!is.finite(mat)) || max(abs(mat - t(mat))) > 1e-10)
            bad(paste(field, "has invalid dimensions, values or symmetry"))
    }
    if (!is.numeric(calibration$G0) || is.complex(calibration$G0) ||
        !is.null(dim(calibration$G0)) || length(calibration$G0) != cfg$G ||
        any(!is.finite(calibration$G0))) bad("G0 must contain G finite draws")
    if (!identical(calibration$nonzero.fraction, mean(calibration$G0 != 0)))
        bad("inconsistent nonzero fraction")
    if (!is.character(calibration$package.version) || length(calibration$package.version) != 1L ||
        is.na(calibration$package.version)) bad("missing package version")
    invisible(cfg)
}

BEAST.asymptotic <- function(X, calibration, alpha = 0.05, seed = NULL) {
    if (is.data.frame(X)) {
        if (!all(vapply(X, function(z) is.numeric(z) && !is.complex(z), logical(1L))))
            stop("X must be numeric", call. = FALSE)
        X <- as.matrix(X)
    }
    if (!is.matrix(X)) stop("X must be a numeric matrix with exactly two columns", call. = FALSE)
    if (ncol(X) != 2L)
        stop("Reusable asymptotic BEAST calibration is currently implemented only for bivariate independence.", call. = FALSE)
    if (!is.numeric(X) || is.complex(X)) stop("X must be numeric", call. = FALSE)
    if (any(!is.finite(X))) stop("X must contain finite values with no missing values", call. = FALSE)
    if (nrow(X) < 2L || any(vapply(seq_len(2L), function(j) length(unique(X[, j])) < 2L, logical(1L))))
        stop("X must have at least two rows and nonconstant margins", call. = FALSE)
    cfg <- .beast_asym_check_calibration(calibration)
    if (nrow(X) != cfg$n) stop("nrow(X) does not match calibration$n", call. = FALSE)
    if (!is.numeric(alpha) || is.complex(alpha) || length(alpha) != 1L ||
        !is.finite(alpha) || alpha <= 0 || alpha >= 1)
        stop("alpha must be in (0, 1)", call. = FALSE)
    if (!is.null(seed)) seed <- .beast_asym_seed_value(seed)
    tied <- any(vapply(seq_len(2L), function(j) anyDuplicated(X[, j]) > 0L, logical(1L)))
    if (tied) warning("Ties detected: reusable bivariate rank-null calibration assumes continuous margins. Ties require separate treatment; permutation calibration is the safer option. Asymptotic distribution-free validity is not claimed for tied data.", call. = FALSE)
    critical <- .beast_asym_critical(calibration$G0, alpha)
    if (critical <= 0)
        stop("Nonpositive asymptotic critical value: the paper procedure requires response permutation. Use permutation calibration separately; no automatic fallback is performed.", call. = FALSE)
    observed <- function() BEAST(X, dep = cfg$dep, subsample.percent = cfg$r/cfg$n,
        B = cfg$B, lambda = cfg$lambda, index = list(1L, 2L), method = "stat")
    fit <- if (is.null(seed)) observed() else .beast_asym_local_rng(seed, observed)
    stat <- unname(fit$BEAST.Statistic)
    if (length(stat) != 1L || !is.finite(stat))
        stop("The unchanged BEAST statistic is nonfinite for these data/tuning values; no numerical repair was applied", call. = FALSE)
    scaled <- sqrt(cfg$n - 1) * stat
    structure(list(BEAST.Statistic = stat, Scaled.Statistic = scaled,
        p.value = mean(calibration$G0 >= scaled), critical.value = critical,
        reject = unname(scaled > critical), alpha = alpha, Interaction = fit$Interaction,
        calibration = cfg, calibration.method = calibration$method.id,
        ties = tied, seed = seed), class = "BEASTAsymptoticResult")
}

print.BEASTAsymptoticCalibration <- function(x, ...) {
    .beast_asym_check_calibration(x)
    cat("Reusable bivariate BEAST calibration (continuous margins)\n",
        "n =", x$n, "; dep =", x$dep, "; r =", x$r, "; B =", x$B, "\n",
        "H =", x$H, "; G =", x$G, "; lambda =", format(x$lambda), "\n", sep = " ")
    invisible(x)
}

print.BEASTAsymptoticResult <- function(x, ...) {
    cat("Bivariate asymptotic BEAST\n",
        "scaled statistic =", format(x$Scaled.Statistic),
        "; p-value =", format(x$p.value), "; reject =", x$reject, "\n", sep = " ")
    invisible(x)
}
