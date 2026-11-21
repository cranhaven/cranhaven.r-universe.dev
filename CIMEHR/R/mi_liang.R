.mi_liang_baseline_design <- function(time_values,time_col,estimate_intercept,baseline_formula,
    prediction_terms=NULL) {
  if(!is.logical(estimate_intercept)||length(estimate_intercept)!=1L||is.na(estimate_intercept))
    stop("estimate_intercept must be TRUE or FALSE.",call.=FALSE)
  if(!inherits(baseline_formula,"formula")||length(baseline_formula)!=2L)
    stop("baseline_formula must be a one-sided formula.",call.=FALSE)
  if(any(!all.vars(baseline_formula)%in%time_col))
    stop("baseline_formula may use only the specified time column: ",time_col,call.=FALSE)
  allowed<-c("+","-","*","/","^",":","(","I","c","poly","ns","bs",
    "log","log1p","exp","expm1","sqrt","sin","cos","tan","abs",
    "pmin","pmax")
  walk<-function(x,inside_transform=FALSE){
    if(!is.call(x))return(invisible(NULL))
    h<-x[[1L]]
    valid<-is.symbol(h)&&as.character(h)%in%allowed
    fn<-if(is.symbol(h))as.character(h) else ""
    if(is.call(h)&&identical(h[[1L]],as.name("::"))&&length(h)==3L){
      ns<-as.character(h[[2L]]);fn<-as.character(h[[3L]])
      valid<-(ns=="stats"&&fn=="poly")||(ns=="splines"&&fn%in%c("ns","bs"))
    }
    if(!valid)stop("baseline_formula supports deterministic time transformations only; unsupported function call.",call.=FALSE)
    if(fn%in%c("poly","ns","bs")&&inside_transform)
      stop("Data-adaptive poly/ns/bs bases must be direct formula terms so their prediction basis can be preserved.",call.=FALSE)
    transformed<-inside_transform||!fn%in%c("+","-","*","/","^",":","(")
    for(a in as.list(x)[-1L])walk(a,transformed)
    invisible(NULL)
  }
  walk(baseline_formula[[2L]])
  safe<-new.env(parent=baseenv())
  safe$poly<-stats::poly;safe$ns<-splines::ns;safe$bs<-splines::bs
  formula_use<-baseline_formula;environment(formula_use)<-safe
  tt<-stats::terms(formula_use)
  if(attr(tt,"intercept")!=1L)
    stop("baseline_formula must include an intercept.",call.=FALSE)
  text<-paste(deparse(formula_use,width.cutoff=500L),collapse=" ")
  if(!estimate_intercept){
    if(length(attr(tt,"term.labels"))||length(all.vars(formula_use)))
      stop("A nonconstant baseline_formula requires estimate_intercept=TRUE; it is not used in profile mode.",call.=FALSE)
    return(list(design=matrix(numeric(),length(time_values),0L),formula=text,terms=NULL,
      assumption="The outcome time baseline is unspecified and removed by the centered profile estimating equation"))
  }
  dd<-setNames(data.frame(as.numeric(time_values)),time_col)
  mf<-stats::model.frame(if(is.null(prediction_terms))formula_use else prediction_terms,
    data=dd,na.action=stats::na.pass)
  fitted_terms<-attr(mf,"terms")
  F<-stats::model.matrix(fitted_terms,data=mf)
  if(nrow(F)!=length(time_values)||any(!is.finite(F)))
    stop("baseline_formula must produce finite rows for every analysis outcome.",call.=FALSE)
  if(!"(Intercept)"%in%colnames(F))stop("baseline_formula must include an intercept.",call.=FALSE)
  nn<-colnames(F);nn[nn!="(Intercept)"]<-paste0("baseline:",nn[nn!="(Intercept)"])
  colnames(F)<-nn
  list(design=F,formula=text,terms=fitted_terms,assumption=if(ncol(F)==1L)
    "Liang-time assumes a constant outcome baseline alpha(t)" else
    paste0("Liang-time assumes alpha(t) lies in the supplied deterministic time basis: ",text))
}

.mi_liang_check <- function(data, vars) {
  d <- as.data.frame(data)
  bad <- setdiff(unique(vars), names(d))
  if (length(bad)) stop("Missing columns: ", paste(bad, collapse = ", "), call. = FALSE)
  if (anyNA(d[[vars[1L]]])) stop("Subject id cannot be missing.", call. = FALSE)
  d
}

.mi_liang_seed <- function(seed, expr) {
  if (missing(seed) || length(seed) != 1L || !is.finite(seed))
    stop("`seed` must be one finite integer.", call. = FALSE)
  had <- exists(".Random.seed", .GlobalEnv, inherits = FALSE)
  if (had) old <- get(".Random.seed", .GlobalEnv, inherits = FALSE)
  on.exit({
    if (had) assign(".Random.seed", old, .GlobalEnv)
    else if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(as.integer(seed)); force(expr)
}

.mi_liang_imputation_random <- function(d, id, vars) {
  vars <- unique(vars)
  if (!length(vars)) return(list(used = character(), dropped = character()))
  varies <- vapply(vars, function(v) {
    zz <- split(d[[v]], as.character(d[[id]]), drop = TRUE)
    any(vapply(zz, function(x) length(unique(x[is.finite(x)])) > 1L,
               logical(1)))
  }, logical(1))
  list(used = vars[varies], dropped = vars[!varies])
}

.mi_liang_impute <- function(d, id, time, outcome, obs_indicator, fixed_vars,
                        random_vars, m, maxit, seed) {
  if (!requireNamespace("mice", quietly = TRUE) || !requireNamespace("pan", quietly = TRUE))
    stop("MI-Liang requires CRAN packages 'mice' and 'pan'.", call. = FALSE)
  d$.mi_liang_row <- seq_len(nrow(d)); vis <- is.finite(d[[time]])
  a <- d[vis, , drop = FALSE]; z0 <- d[!vis, , drop = FALSE]
  if (!nrow(a)) stop("There are no visit rows.", call. = FALSE)
  marker_observed <- !is.na(a[[obs_indicator]]) & a[[obs_indicator]] == 1 &
    is.finite(a[[outcome]])
  a[[outcome]][!marker_observed] <- NA_real_
  random_info <- .mi_liang_imputation_random(a, id, random_vars)
  random_vars <- random_info$used
  pred <- unique(c(fixed_vars, random_vars, random_info$dropped, time))
  nonnum <- pred[!vapply(a[pred], is.numeric, logical(1))]
  if (length(nonnum)) stop("2l.pan predictors must be numeric: ", paste(nonnum, collapse = ", "))
  miss <- !is.finite(a[[outcome]])
  if (!any(miss)) {
    out <- rbind(a, z0); out <- out[order(out$.mi_liang_row), ]; out$.mi_liang_row <- NULL
    ans <- rep(list(out), as.integer(m))
    attr(ans, "imputation_random_used") <- random_info$used
    attr(ans, "imputation_random_dropped") <- random_info$dropped
    return(ans)
  }
  cl <- ".mi_liang_cluster"; while (cl %in% names(a)) cl <- paste0(cl, "_")
  a[[cl]] <- match(a[[id]], unique(a[[id]]))
  .mi_liang_seed(seed, {
    ini <- mice::mice(a, maxit = 0, printFlag = FALSE)
    pm <- ini$predictorMatrix; pm[,] <- 0
    meth <- ini$method; meth[] <- ""; meth[outcome] <- "2l.pan"
    pm[outcome, cl] <- -2L
    for (v in pred) if (v != outcome && v %in% colnames(pm))
      pm[outcome, v] <- if (v %in% random_vars) 2L else 1L
    wh <- matrix(FALSE, nrow(a), ncol(a), dimnames = list(NULL, names(a)))
    wh[miss, outcome] <- TRUE
    imp <- mice::mice(a, m = as.integer(m), maxit = as.integer(maxit),
                      predictorMatrix = pm, method = meth, where = wh,
                      printFlag = FALSE)
    ans <- lapply(seq_len(m), function(k) {
      x <- mice::complete(imp, k); x[[cl]] <- NULL
      x <- rbind(x, z0); x <- x[order(x$.mi_liang_row), ]; x$.mi_liang_row <- NULL
      rownames(x) <- NULL; x
    })
    attr(ans, "imputation_random_used") <- random_info$used
    attr(ans, "imputation_random_dropped") <- random_info$dropped
    ans
  })
}

.mi_liang_pinv <- function(A, tol = sqrt(.Machine$double.eps)) {
  A <- as.matrix(A); s <- svd(A)
  if (!length(s$d) || !is.finite(max(s$d)) || max(s$d) == 0)
    stop("Estimating-equation bread has rank zero.", call. = FALSE)
  keep <- s$d > tol * max(s$d)
  if (sum(keep) == min(dim(A)))
    return(s$v[, keep, drop = FALSE] %*% (t(s$u[, keep, drop = FALSE]) / s$d[keep]))
  fail <- function() stop("Estimating-equation bread is rank deficient; named coefficients are not identified.", call. = FALSE)
  if (nrow(A) != ncol(A)) fail()
  column_scale <- apply(abs(A), 2L, max)
  if (any(!is.finite(column_scale) | column_scale <= 0)) fail()
  scaled <- sweep(A, 2L, column_scale, "/")
  row_scale <- apply(abs(scaled), 1L, max)
  if (any(!is.finite(row_scale) | row_scale <= 0)) fail()
  scaled <- scaled / row_scale
  balanced <- svd(scaled)
  keep <- balanced$d > tol * max(balanced$d)
  if (sum(keep) < ncol(A)) fail()
  inverse <- balanced$v[, keep, drop = FALSE] %*%
    (t(balanced$u[, keep, drop = FALSE]) / balanced$d[keep])
  sweep(sweep(inverse, 1L, column_scale, "/"), 2L, row_scale, "/")
}

.mi_liang_constant <- function(d, id, vars) {
  if (!length(vars)) return(invisible(TRUE))
  groups <- d[[id]]; simple_id <- is.atomic(groups) && is.null(dim(groups)) &&
    (!is.object(groups) || is.factor(groups))
  if (simple_id) si <- as.integer(factor(groups))
  for (v in vars) {
    x <- d[[v]]
    if (simple_id && is.numeric(x) && !is.object(x) && is.null(dim(x))) {
      keep <- !is.na(x) & !is.na(groups); xx <- x[keep]; ss <- si[keep]
      varies <- any(xx != xx[match(ss, ss)])
    } else {
      nval <- tapply(x, groups, function(x) length(unique(x[!is.na(x)])))
      varies <- any(nval > 1L, na.rm = TRUE)
    }
    if (varies)
      stop("Liang requires subject-level time-invariant covariate `", v, "`.",
           call. = FALSE)
  }
  invisible(TRUE)
}

.mi_liang_gamma <- function(d, id, time, visit_covs) {
  .mi_liang_constant(d, id, visit_covs)
  ids <- unique(d[[id]]); si <- match(d[[id]], ids); first <- match(ids, d[[id]])
  V <- as.matrix(d[first, visit_covs, drop = FALSE]); storage.mode(V) <- "double"
  if (any(!is.finite(V))) stop("Baseline visit covariates must be finite.", call. = FALSE)
  m_i <- tabulate(si[is.finite(d[[time]])], nbins = length(ids)); M <- sum(m_i)
  if (!M) stop("No visit events.", call. = FALSE)
  event_sum <- colSums(V * m_i)
  cached_g <- cached_fg <- NULL
  fg <- function(g) {
    if (!is.null(cached_g) && identical(g, cached_g)) return(cached_fg)
    lp <- as.vector(V %*% g); a <- max(lp); ew <- exp(lp - a); sw <- sum(ew)
    value <- list(value = M * (a + log(sw)) - sum(event_sum * g),
                  gradient = M * colSums(V * ew) / sw - event_sum)
    cached_g <<- g; cached_fg <<- value; value
  }
  opt <- stats::optim(rep(0, ncol(V)), function(g) fg(g)$value,
                      function(g) fg(g)$gradient, method = "BFGS",
                      control = list(reltol = 1e-10, maxit = 1000))
  g <- opt$par; names(g) <- visit_covs
  lp <- as.vector(V %*% g); w <- exp(lp - max(lp)); bar <- colSums(V * w) / sum(w)
  Vc <- sweep(V, 2, bar, "-"); H <- M * crossprod(Vc * sqrt(w)) / sum(w)
  list(gamma = g, V = V, ids = ids, si = si, first = first, m = m_i,
       M = M, exp_gamma = exp(lp), vcov = .mi_liang_pinv(H),
       converged = opt$convergence == 0L, code = opt$convergence,
       score_max = max(abs(fg(g)$gradient)))
}

.mi_liang_profile_scores <- function(Dsub,center,D,H,coef,y,subject,counts) {

  residual<-y-drop(D%*%coef)
  score<-matrix(0,nrow(Dsub),ncol(Dsub))
  direct<-rowsum(H*residual,subject,reorder=FALSE)
  score[as.integer(rownames(direct)),]<-direct
  center_derivative<-sweep(Dsub,2,center,"-")*(counts/sum(counts))
  score-center_derivative*sum(residual)
}

.mi_liang_liang <- function(d, id, time, outcome, visit_covs, long_covs, random_covs,
                       sigma_floor = 1e-8, estimate_intercept=FALSE, baseline_formula=~1,
                       baseline_terms=NULL, gamma_cache=NULL, censor_col=NULL) {
  if (.liang_completed_varying_followup(d, id, censor_col))
    return(.liang_completed_followup_fit(d, id, time, outcome, visit_covs,
      long_covs, random_covs, sigma_floor, censor_col, estimate_intercept,
      baseline_formula, baseline_terms))
  .mi_liang_constant(d, id, unique(c(long_covs, random_covs)))
  gamma_input <- if (!is.null(gamma_cache)) list(id=id, time=time,
    visit_covs=visit_covs, data=d[, unique(c(id, time, visit_covs)), drop=FALSE]) else NULL
  if (!is.null(gamma_cache) && identical(gamma_input, gamma_cache$input)) {
    ga <- gamma_cache$fit
  } else {
    ga <- .mi_liang_gamma(d, id, time, visit_covs)
    if (!is.null(gamma_cache)) {
      gamma_cache$input <- gamma_input; gamma_cache$fit <- ga
    }
  }
  n <- length(ga$ids); first <- ga$first; si <- ga$si
  Lambda <- ga$M / sum(ga$exp_gamma); Lambda_i <- ga$exp_gamma * Lambda
  raw <- sum(ga$m^2 - ga$m - Lambda_i^2) / sum(Lambda_i^2)
  sigma2 <- max(raw, 0); use_B <- sigma2 > sigma_floor
  Bi <- if (use_B) (1 + ga$m * sigma2) / (1 + Lambda_i * sigma2) - 1 else rep(0, n)

  X <- as.matrix(d[, long_covs, drop = FALSE]); storage.mode(X) <- "double"
  Xb <- X[first, , drop = FALSE]
  Q <- as.matrix(d[, random_covs, drop = FALSE]); storage.mode(Q) <- "double"
  Q0 <- cbind(`(Intercept)` = 1, Q); Qb <- Q0[first, , drop = FALSE]
  if (use_B) {
    B <- Q0 * Bi[si]; Bb <- Qb * Bi
    D <- cbind(X, B); dn <- c(long_covs, paste0("theta:", colnames(Q0)))
  } else { D <- X; Bb <- NULL; dn <- long_covs }
  colnames(D) <- dn
  wsub <- ga$m
  if (sum(wsub) <= 0) stop("Zero Stage-3 weight.", call. = FALSE)
  center <- colSums(Xb * wsub) / sum(wsub)
  if (use_B) center <- c(center, colSums(Bb * wsub) / sum(wsub))
  H <- sweep(D, 2, center, "-")
  ok <- is.finite(d[[time]]) & is.finite(d[[outcome]]) &
        stats::complete.cases(D)
  if (sum(ok) <= ncol(D)) stop("Too few completed outcome rows for Liang fit.", call. = FALSE)
  baseline<-.mi_liang_baseline_design(d[[time]][ok],time,estimate_intercept,
                                   baseline_formula,prediction_terms=baseline_terms)
  F<-baseline$design;nb<-ncol(F);Dobs<-D[ok,,drop=FALSE]
  if(estimate_intercept) {

    W<-cbind(F,Dobs);an<-colnames(W)
    if(anyDuplicated(an))stop("Baseline and outcome coefficient names must be distinct.",call.=FALSE)
    A<-crossprod(W);Ai<-.mi_liang_pinv(A)
    ca<-drop(Ai%*%crossprod(W,d[[outcome]][ok]));names(ca)<-an
    rr<-d[[outcome]][ok]-drop(W%*%ca)
    sc<-matrix(0,length(ga$ids),ncol(W))
    tmp<-rowsum(W*rr,si[ok],reorder=FALSE)
    sc[as.integer(rownames(tmp)),]<-tmp
  } else {
    Hobs<-H[ok,,drop=FALSE];an<-colnames(D)
    A<-crossprod(Hobs,Dobs);Ai<-.mi_liang_pinv(A)
    ca<-drop(Ai%*%crossprod(Hobs,d[[outcome]][ok]));names(ca)<-an
    sc<-.mi_liang_profile_scores(D[first,,drop=FALSE],center,Dobs,Hobs,
                             ca,d[[outcome]][ok],si[ok],ga$m)
  }
  VA<-Ai%*%crossprod(sc)%*%t(Ai);dimnames(VA)<-list(an,an)
  primary<-seq_len(nb+length(long_covs));theta_index<-setdiff(seq_along(ca),primary)
  model_scope<-if(estimate_intercept)"Liang-time with an explicit deterministic outcome baseline" else
    "Liang profile estimating equation with unspecified outcome baseline"
  equation<-if(estimate_intercept)"W'W c=W'Y, W=[F(time),D]; 2023 Section 2.5.2 Eq. (5)" else
    "H'D b=H'Y, H=D-visit-count center; 2023 Section 2.5.1 Eq. (4)"
  list(coef=ca[primary],vcov=VA[primary,primary,drop=FALSE],
       coef_all=ca,vcov_all=VA,vcov_full=VA,
       theta=if(use_B)ca[theta_index] else
         setNames(rep(NA_real_,ncol(Q0)),colnames(Q0)),
       estimate_intercept=estimate_intercept,baseline_formula=baseline$formula,
       baseline_terms=baseline$terms,baseline_assumption=baseline$assumption,
       model_scope=model_scope,outcome_equation=equation,
       vcov_type="variance (plug-in): subject sandwich conditional on Liang Stage 1; empirical-center influence in profile mode",
       gamma=ga$gamma,vcov_gamma=ga$vcov,
       vcov_gamma_type="variance (plug-in): inverse-information model variance for the visiting nuisance; not a cluster-robust gamma variance",
       Lambda=Lambda,sigma2=sigma2,sigma2_raw=raw,use_B=use_B,
       converged=ga$converged,gamma_code=ga$code,gamma_score_max=ga$score_max,
       condition=kappa(A),n_outcome=sum(ok),n_visit=ga$M,
       baseline_prediction=if(estimate_intercept)
         .liang_trajectory_parametric_summary(baseline$terms,ca[seq_len(nb)]) else
         .liang_trajectory_increment_summary(d[[time]][ok],
           d[[outcome]][ok]-drop(Dobs%*%ca),log(sum(ga$m)/Lambda)))
}

.mi_liang_compact_analysis_fit <- function(z) {
  se_from <- function(V, nm) {
    out <- stats::setNames(rep(NA_real_, length(nm)), nm)
    if (!is.matrix(V) || is.null(rownames(V))) return(out)
    hit <- intersect(nm, rownames(V))
    out[hit] <- sqrt(pmax(diag(V)[match(hit, rownames(V))], 0))
    out
  }
  list(
    coefficients = z$coef,
    coef = z$coef,
    se = se_from(z$vcov, names(z$coef)),
    vcov = z$vcov,
    vcov_type = z$vcov_type,
    coefficient_all = z$coef_all,
    se_all = se_from(z$vcov_all, names(z$coef_all)),
    vcov_all = z$vcov_all,
    theta = z$theta,
    estimate_intercept=z$estimate_intercept,baseline_formula=z$baseline_formula,
    baseline_terms=z$baseline_terms,
    model_scope=z$model_scope,baseline_assumption=z$baseline_assumption,
    outcome_equation=z$outcome_equation,
    vcov_full = z$vcov_full,
    gamma = z$gamma,
    gamma_se = se_from(z$vcov_gamma, names(z$gamma)),
    vcov_gamma = z$vcov_gamma,
    vcov_gamma_type = z$vcov_gamma_type,
    Lambda = z$Lambda,
    sigma2 = z$sigma2,
    sigma2_raw = z$sigma2_raw,
    use_B = z$use_B,
    converged = z$converged,
    gamma_code = z$gamma_code,
    gamma_score_max = z$gamma_score_max,
    condition = z$condition,
    n_outcome = z$n_outcome,
    n_visit = z$n_visit
  )
}

.mi_liang_pool <- function(fits) {
  nm <- Reduce(intersect, lapply(fits, function(x) names(x$coef)))
  Q <- do.call(rbind, lapply(fits, function(x) x$coef[nm]))
  W <- Reduce(`+`, lapply(fits, function(x) x$vcov[nm, nm, drop = FALSE])) / length(fits)
  B <- if (length(fits) > 1L) stats::cov(Q) else matrix(0, length(nm), length(nm))
  if (length(nm) == 1L) B <- matrix(B, 1, 1); dimnames(B) <- list(nm, nm)
  mm <- length(fits); T <- W + (1 + 1 / mm) * B
  u <- pmax(diag(W), 0); b <- pmax(diag(B), 0)
  riv <- ifelse(u > 0, (1 + 1 / mm) * b / u, ifelse(b > 0, Inf, 0))
  df <- if (mm <= 1L) rep(Inf, length(nm)) else
    ifelse(b > 0, (mm - 1) * (1 + 1 / pmax(riv, .Machine$double.eps))^2, Inf)
  fmi <- ifelse(is.finite(riv), (riv + 2 / (df + 3)) / (riv + 1), 1)
  names(riv) <- names(df) <- names(fmi) <- nm
  list(coef = colMeans(Q), vcov = T, W = W, B = B, rubin_df = df,
       relative_increase_variance = riv, fraction_missing_information = fmi)
}

.fit_mi_liang <- function(
    data, id = "id", time = "time", outcome = "Y", obs_indicator = "R",
    visit_covs = c("Z", "X"), long_covs = c("Z", "X"), random_covs = "Z",
    m = 5L, maxit = 5L, seed = 1L, sigma_floor = 1e-8,
    censor_col = if ("C" %in% names(data)) "C" else NULL,
    keep_completed = FALSE, estimate_intercept = FALSE, baseline_formula = ~1) {
  if (length(m) != 1L || !is.finite(m) || m < 1 || m != as.integer(m) ||
      length(maxit) != 1L || !is.finite(maxit) || maxit < 1 || maxit != as.integer(maxit))
    stop("`m` and `maxit` must be positive integers.", call. = FALSE)
  d <- .mi_liang_check(data, c(id, time, outcome, obs_indicator, visit_covs, long_covs,
                           random_covs, if (!is.null(censor_col)) censor_col))
  baseline_request<-.mi_liang_baseline_design(d[[time]][is.finite(d[[time]])],
                                           time,estimate_intercept,baseline_formula)
  if (!is.null(censor_col)) {
    cc <- as.numeric(d[match(unique(d[[id]]), d[[id]]), censor_col])
    if (any(!is.finite(cc)))
      stop("Every subject needs a finite censoring time.", call. = FALSE)
  }
  varying_followup <- .liang_completed_varying_followup(d, id, censor_col)
  completed <- .mi_liang_impute(d, id, time, outcome, obs_indicator,
                           unique(c(visit_covs, long_covs)), random_covs,
                           m, maxit, seed)
  imp_random_used <- attr(completed, "imputation_random_used")
  imp_random_dropped <- attr(completed, "imputation_random_dropped")
  fits <- lapply(completed, .mi_liang_liang, id = id, time = time, outcome = outcome,
                 visit_covs = visit_covs, long_covs = long_covs,
                 random_covs = random_covs, sigma_floor = sigma_floor,
                 estimate_intercept=estimate_intercept,baseline_formula=baseline_formula,
                 baseline_terms=baseline_request$terms,
                 gamma_cache=new.env(parent=emptyenv()), censor_col=censor_col)
  pool <- .mi_liang_pool(fits)
  pool_all <- .mi_liang_pool(lapply(fits,function(z)list(coef=z$coef_all,vcov=z$vcov_all)))
  sig <- vapply(completed, function(z) {
    yy <- z[[outcome]]; sum((seq_along(yy) %% 1009L) * ifelse(is.finite(yy), yy, 0))
  }, 0.0)
  conv <- all(vapply(fits, `[[`, logical(1), "converged"))
  ans <- list(method_key = "mi_liang", method = if(estimate_intercept)"MI+Liang-time" else "MI+Liang",
              estimand = "Liang marginal longitudinal coefficient pooled over MAR multiple imputations",
              coefficients = pool$coef, vcov = pool$vcov,
              coefficients_all=pool_all$coef,vcov_all=pool_all$vcov,
              se = sqrt(pmax(diag(pool$vcov), 0)),
              vcov_type = "Rubin pooling with within-imputation variance (plug-in): Liang/ Liang-time subject sandwich conditional on Stage 1",
              nuisance = list(gamma = fits[[1]]$gamma, Lambda = fits[[1]]$Lambda,
                              sigma2 = fits[[1]]$sigma2,
                              baseline_terms=fits[[1]]$baseline_terms,
                              theta_by_imputation = lapply(fits, `[[`, "theta"),
                              theta=pool_all$coef[setdiff(names(pool_all$coef),names(pool$coef))],
                              within_vcov = pool$W, between_vcov = pool$B),
              converged = conv,
              diagnostics = list(ok = isTRUE(conv && all(is.finite(pool$coef)) &&
                                               all(is.finite(pool$vcov))),
                                 estimate_intercept=estimate_intercept,
                                 baseline_formula=fits[[1]]$baseline_formula,
                                 model_scope=fits[[1]]$model_scope,
                                 baseline_assumption=fits[[1]]$baseline_assumption,
                                 m_requested = as.integer(m), m_success = length(fits),
                                 rubin_df = pool$rubin_df,
                                 rubin_df_all=pool_all$rubin_df,
                                 relative_increase_variance = pool$relative_increase_variance,
                                 fraction_missing_information = pool$fraction_missing_information,
                                 seed = as.integer(seed), n_subjects = length(unique(d[[id]])),
                                 censor_source = if (is.null(censor_col))
                                   "common maximum observed time" else censor_col,
                                 n_visits = sum(is.finite(d[[time]])),
                                 n_observed = sum(is.finite(d[[time]]) & !is.na(d[[obs_indicator]]) &
                                                    d[[obs_indicator]] == 1 & is.finite(d[[outcome]])),
                                 n_imputed_per_dataset = vapply(completed, function(z)
                                   sum(is.finite(d[[time]]) &
                                         (is.na(d[[obs_indicator]]) | d[[obs_indicator]] != 1 |
                                            !is.finite(d[[outcome]])) &
                                         is.finite(z[[outcome]])), 0L),
                                 original_R_preserved = all(vapply(completed, function(z)
                                   identical(z[[obs_indicator]], d[[obs_indicator]]), logical(1))),
                                 imputation_random_used = imp_random_used,
                                 imputation_random_dropped_as_aliased = imp_random_dropped,
                                 downstream_process = "all actual visits N(t); R is not overwritten",
                                 outcome_equation = fits[[1]]$outcome_equation,
                                 imputation_signature = sig,
                                 condition_number = vapply(fits, `[[`, 0.0, "condition"),
                                 gamma_score_max = fits[[1]]$gamma_score_max),
              assumptions = c("Congenial 2l.pan MAR imputation; not valid for latent-variable MNAR without further assumptions",
                              "Common follow-up and time-invariant Liang covariates",
                              fits[[1]]$baseline_assumption,
                              "Within-imputation sandwich is conditional on frailty Stage 1; Rubin pooling cannot repair that conditional nuisance variance"),
              call = match.call())
  if (varying_followup) {
    ans$vcov_type <- "Rubin pooling with ordinary Liang-V all-stage patient stacked within-imputation covariance"
    ans$nuisance$followup <- lapply(fits, `[[`, "followup_nuisance")
    ans$diagnostics$subject_specific_followup <- TRUE
    ans$diagnostics$nuisance_estimation_propagated <- TRUE
    ans$assumptions[2L] <- "Subject-specific noninformative follow-up and time-invariant Liang covariates"
    ans$assumptions[4L] <- "Within-imputation stacked covariance propagates the visiting rate, profiled baseline, frailty moment and outcome centering; Rubin pooling adds between-imputation uncertainty"
  }
  storage_started <- proc.time()[["elapsed"]]
  ans$analysis_fits <- lapply(fits, .mi_liang_compact_analysis_fit)
  if (varying_followup) for (k in seq_along(fits))
    ans$analysis_fits[[k]]$followup_nuisance <- fits[[k]]$followup_nuisance
  ans$diagnostics$analysis_fit_storage_seconds <-
    proc.time()[["elapsed"]] - storage_started
  ans$diagnostics$analysis_fit_storage_bytes <-
    as.numeric(object.size(ans$analysis_fits))

  cnt <- tabulate(match(d[[id]][is.finite(d[[time]])], unique(d[[id]])),
                  nbins = length(unique(d[[id]])))
  ans$diagnostics$zero_visit_subjects <- sum(cnt == 0L)
  ans$prediction <- .liang_trajectory_spec(
    list(type="mi",components=lapply(fits, `[[`, "baseline_prediction")),
    d,id,time,long_covs,pool$coef, .liang_trajectory_window(d,time,censor_col))
  if (keep_completed) ans$completed_data <- completed
  class(ans) <- c("mi_liang")
  ans
}
