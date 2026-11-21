library(CIMEHR)

# Deterministic Stage-3 designs.
ns <- asNamespace("CIMEHR")
expect_numerical_error <- function(expr) {
    ans <- tryCatch(force(expr), error = identity)
    stopifnot(inherits(ans, "error"),
        grepl("rank|numerical|compensation", conditionMessage(ans), ignore.case = TRUE))
}
relative_difference <- function(x, y) max(abs(x-y))/max(1, max(abs(y)))

for (variant in c("current", "higher_order_gaussian_moments")) {
    engine <- get(".cimehr_build_engine", ns)(variant)
    up <- engine$.cimehr_updated
    sp <- engine$.cimehr_spline
    n <- 48L
    x <- seq(-1.5, 1.5, length.out = n)
    u <- residuals(lm(sin(seq_len(n)) ~ x))
    h <- 1 + .3*x + 1e-4*u
    D0 <- cbind(x=x, compensation=h)
    subject <- rep(seq_len(n), each=4L)
    tt <- rep(c(.1, .3, .6, .9), n)
    y <- .2 + .3*x[subject] + .4*tt + .1*u[subject]
    affine <- function(D) up$.cimehr_affine_stage3(D, 0*D, y,
        subject, rep(1:4, n), tt, rep(1, n), rep(4L, n),
        first_risk=rep(1L, n), shared=TRUE, zero_rank_tolerance=1e-7)
    ans <- affine(D0)
    stopifnot(isTRUE(ans$numerical_solver$numerical_rescue),
        up$.lu_zero_rank(ans$bread, 1e-7) < ncol(D0),
        all(is.finite(ans$coefficients)), all(is.finite(ans$shared$inverse)),
        all(is.finite(ans$covariance)), ncol(ans$shared$D)==ncol(D0),
        identical(colnames(ans$shared$D), colnames(D0)))
    # Compensation aliased with the baseline and x.
    expect_numerical_error(affine(cbind(x=x, compensation=1+.3*x)))
    expect_numerical_error(affine(cbind(x=x, compensation=rep(1,n))))

    # Nonsymmetric estimating matrix H'WD.
    D <- cbind(x, sin(seq_len(n)), cos(seq_len(n)/3))
    H <- D - cbind(.2+cos(seq_len(n))/5, x/4, x^2/6)
    cw <- 1 + seq_len(n)/n
    response <- x/3 + cos(seq_len(n)/5)
    exact <- solve(crossprod(H, cw*D), crossprod(H, cw*response))
    wrong <- solve(crossprod(H, cw*H), crossprod(H, cw*response))
    solved <- up$.cimehr_stage3_qr_solve(D, response, H, cw)
    stopifnot(relative_difference(solved$coefficients, exact)<1e-11,
        relative_difference(solved$inverse, solve(crossprod(H,cw*D)))<1e-11,
        relative_difference(solved$coefficients, wrong)>1e-3)

    # Spline rescue with full and chunked evaluation.
    bs <- sp$.cimehr_spline_basis(tt, df=2L, boundary_knots=c(0,1),
        type="natural", intercept=TRUE)$spec
    spec <- list(kind="quotient", tolerance=1e-7,
        posterior=up$.lu_laplace(rep(4,n), rep(4,n), 0))
    spline <- function(multiplier, chunk_size) sp$.cimehr_spline_stage3(y,
        rep(1L,length(y)), tt, rep(TRUE,length(y)), subject,
        matrix(x,n,1,dimnames=list(NULL,"x")), matrix(0,n,0), character(),
        matrix(1,n,1,dimnames=list(NULL,"Intercept")), FALSE, multiplier,
        TRUE, bs, bs, 0, 1, chunk_size=chunk_size, zero_spec=spec)
    one <- spline(h, length(y))
    chunks <- spline(h, 13L)
    stopifnot(isTRUE(one$numerical_solver$numerical_rescue),
        isTRUE(chunks$numerical_solver$numerical_rescue),
        one$design_rank==length(one$coef), chunks$design_rank==length(chunks$coef),
        all(is.finite(one$inverse_bread)), all(is.finite(chunks$vcov_conditional)),
        relative_difference(chunks$coef,one$coef)<1e-8,
        relative_difference(chunks$inverse_bread,one$inverse_bread)<1e-8,
        relative_difference(chunks$vcov_conditional,one$vcov_conditional)<1e-7)
    original_D <- cbind(sp$.cimehr_spline_eval(tt,bs),x[subject],h[subject])
    reference <- qr.coef(qr(original_D, LAPACK=TRUE),y)
    stopifnot(relative_difference(one$coef,reference)<1e-8,
        identical(one$coefficient_names,chunks$coefficient_names))
    expect_numerical_error(spline(1+.3*x,13L))
}
