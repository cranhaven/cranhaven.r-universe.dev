library(CIMEHR)

ns <- asNamespace("CIMEHR")
diagnose <- get(".cimehr_numerical_diagnostics", ns)
attach_diagnostic <- get(".cimehr_attach_numerical_diagnostics", ns)
signal_diagnostic <- get(".cimehr_signal_identification_warning", ns)
quiet <- get(".cimehr_without_identification_signal", ns)
outcome_table <- get(".outcome_table", ns)
capture <- function(expr) {
    messages <- list()
    value <- withCallingHandlers(force(expr), warning=function(w) {
        messages[[length(messages)+1L]] <<- w
        invokeRestart("muffleWarning")
    })
    list(value=value, warnings=messages)
}

# Collinear fixed effects with zero compensation.
x <- seq(-1,1,length.out=80)
D <- cbind(X=x, X2=x+1e-5*sin(seq_along(x)))
near <- diagnose(crossprod(D))
stopifnot(near$near_singular_equation, !near$qr_rescue,
    identical(near$status,"near_singular"), length(near$warnings)==1L,
    grepl("fixed effects",near$warnings), grepl("retained",near$warnings))
well <- diagnose(diag(2))
stopifnot(!length(well$warnings), identical(well$status,"no_warning_detected"))
# QR rescue diagnostics.
rescued <- diagnose(diag(2),solver=list(numerical_rescue=TRUE))
stopifnot(rescued$qr_rescue, identical(rescued$status,"qr_rescue"),
    grepl("QR rescue",rescued$warnings))
spline <- diagnose(relative_minimum=1e-12,spline=TRUE)
stopifnot(spline$near_singular_equation, grepl("Gram matrix",spline$measure))

for (method in c("cimehr","cimehr_hl","cimehr_spline","cimehr_spline_hl")) {
    original <- list(method_key=method,method=method,
        coefficients=c(beta.X=.2,beta.X2=-.1),se=c(beta.X=.03,beta.X2=.04),
        vcov=diag(c(.03^2,.04^2)),vcov_type="reported covariance",
        diagnostics=list(ok=TRUE,converged=TRUE,identified=TRUE,regular=TRUE,
            conditional_inference_available=FALSE,conditional_inference_valid=FALSE))
    dimnames(original$vcov) <- list(names(original$coefficients),names(original$coefficients))
    class(original) <- c("cimehr_fit","list")
    before <- outcome_table(original)
    captured <- capture(attach_diagnostic(original,rescued))
    fit <- captured$value
    stopifnot(length(captured$warnings)==1L,
        inherits(captured$warnings[[1L]],"cimehr_numerical_warning"),
        identical(fit$coefficients,original$coefficients), identical(fit$se,original$se),
        identical(fit$vcov,original$vcov), identical(outcome_table(fit),before),
        identical(fit$diagnostics[names(original$diagnostics)],original$diagnostics))
    # Public and internal numerical warnings.
    stopifnot(length(capture(signal_diagnostic(fit))$warnings)==1L,
        length(capture(quiet(signal_diagnostic(fit)))$warnings)==0L,
        identical(summary(fit)$numerical_warning,rescued$warnings),
        any(grepl("Numerical warning:.*QR rescue",capture.output(print(fit)))))
    # Failed-fit status.
    failed <- original
    failed$diagnostics$covariance_failed <- TRUE
    failed_before <- outcome_table(failed)
    failed_after <- capture(attach_diagnostic(failed,near))$value
    stopifnot(identical(outcome_table(failed_after),failed_before))
}
