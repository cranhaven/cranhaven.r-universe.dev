.li_iirr_check <- function(data, vars) {
  d <- as.data.frame(data); bad <- setdiff(unique(vars), names(d))
  if (length(bad)) stop("Missing columns: ", paste(bad, collapse = ", "), call. = FALSE)
  if (anyNA(d[[vars[1L]]])) stop("Subject id cannot be missing.", call. = FALSE)
  d
}
.li_iirr_pinv <- function(A, tol = sqrt(.Machine$double.eps)) {
  A <- as.matrix(A); s <- svd(A)
  if (!length(s$d) || !is.finite(max(s$d)) || max(s$d) == 0)
    stop("Rank-zero estimating equation.", call. = FALSE)
  k <- s$d > tol * max(s$d)
  if (sum(k) < min(dim(A)))
    stop("Estimating equation is rank deficient; named coefficients are not identified.",
         call. = FALSE)
  s$v[, k, drop = FALSE] %*% (t(s$u[, k, drop = FALSE]) / s$d[k])
}
.li_iirr_gamma_subject_score <- function(g,V,m){
  lp<-as.vector(V%*%g);ew<-exp(lp-max(lp));prob<-ew/sum(ew)
  vbar<-colSums(V*prob)
  sweep(sweep(V,2L,vbar,"-"),1L,m-sum(m)*prob,"*")
}
.li_iirr_const <- function(d, id, vars) {
  subject <- d[[id]]
  plain_id <- is.atomic(subject) && !is.object(subject) && is.null(dim(subject)) && !anyNA(subject)
  grouping <- if (!plain_id) NULL else if (is.integer(subject) || is.character(subject))
    match(subject, subject) else as.integer(factor(subject))
  for (v in vars) {
    x <- d[[v]]
    if (plain_id && is.numeric(x) && !is.object(x) && is.null(dim(x))) {
      keep <- !is.na(x); values <- x[keep]; group <- grouping[keep]
      different <- any(values != values[match(group, group)])
    } else {
      z <- tapply(x, subject, function(y) length(unique(y[!is.na(y)])))
      different <- any(z > 1L, na.rm = TRUE)
    }
    if (different) stop("IIRR requires time-invariant visit covariate `", v, "`.", call. = FALSE)
  }
}

.li_iirr_li_loop <- function(d, id, time, outcome, obs_indicator, li_covs, covariance) {
  d$.li_iirr_row <- seq_len(nrow(d)); o <- order(d[[id]], d[[time]], na.last = TRUE)
  z <- d[o, , drop = FALSE]; rownames(z) <- NULL
  actual <- is.finite(z[[time]])
  observed <- actual & !is.na(z[[obs_indicator]]) & z[[obs_indicator]] == 1 &
    is.finite(z[[outcome]])
  z[[outcome]][actual & !observed] <- NA_real_
  ids <- unique(z[[id]]); si <- match(z[[id]], ids); p <- 2L + length(li_covs)
  nm <- c("(Intercept)", li_covs, paste0("lag_", outcome))
  actual_rows <- split(which(actual), factor(si[actual], levels = seq_along(ids)))
  cache_covariates <- !outcome %in% li_covs &&
    all(vapply(z[, li_covs, drop = FALSE], function(x)
      is.numeric(x) && !is.object(x) && is.null(dim(x)), logical(1)))
  if (cache_covariates) {
    li_values <- as.matrix(z[, li_covs, drop = FALSE])
    li_row <- function(r) as.numeric(li_values[r, , drop = TRUE])
  } else {
    li_row <- function(r) as.numeric(z[r, li_covs, drop = FALSE])
  }
  WL <- list(); rate <- gapv <- numeric(); pid <- integer(); k <- 0L
  for (ii in seq_along(ids)) {
    ix <- actual_rows[[ii]]; if (length(ix) < 2L) next
    for (j in 2:length(ix)) {
      a <- ix[j - 1L]; b <- ix[j]; gap <- z[[time]][b] - z[[time]][a]
      if (!observed[a] || !observed[b] || !is.finite(gap) || gap <= 0 ||
          !all(is.finite(li_row(a)))) next
      k <- k + 1L; WL[[k]] <- c(1, li_row(a),
                                 z[[outcome]][a])
      rate[k] <- (z[[outcome]][b] - z[[outcome]][a]) / gap
      gapv[k] <- gap; pid[k] <- ii
    }
  }
  if (k < max(10L, p + 2L)) stop("Too few observed adjacent increments for LI.", call. = FALSE)
  W <- do.call(rbind, WL); colnames(W) <- nm
  if (qr(W)$rank < ncol(W)) stop("Linear-increment design is rank deficient.", call. = FALSE)
  ww <- if (identical(covariance, "randomwalk")) gapv else rep(1, k)
  A <- crossprod(W, W * ww); Ai <- .li_iirr_pinv(A)
  eta <- as.vector(Ai %*% crossprod(W, ww * rate)); names(eta) <- nm
  er <- rate - as.vector(W %*% eta); us <- matrix(0, length(ids), p)
  tmp <- rowsum(W * (ww * er), pid, reorder = FALSE)
  us[as.integer(rownames(tmp)), ] <- tmp
  Ve <- Ai %*% crossprod(us) %*% t(Ai); dimnames(Ve) <- list(nm, nm)
  grad <- matrix(NA_real_, nrow(z), p, dimnames = list(NULL, nm))
  rec <- unr <- rep(FALSE, nrow(z)); ey <- eta[p]
  for (ii in seq_along(ids)) {
    ix <- actual_rows[[ii]]; anchored <- FALSE; prev <- NA_integer_
    yp <- NA_real_; gp <- rep(NA_real_, p)
    for (r in ix) {
      if (observed[r]) { yp <- z[[outcome]][r]; gp <- rep(0, p); grad[r, ] <- gp
        anchored <- TRUE; prev <- r; next }
      if (!anchored || is.na(prev)) { unr[r] <- TRUE; prev <- r; next }
      gap <- z[[time]][r] - z[[time]][prev]
      xv <- li_row(prev)
      if (!is.finite(gap) || gap <= 0 || !all(is.finite(xv))) {
        unr[r] <- TRUE; anchored <- FALSE; prev <- r; next }
      wv <- c(1, xv, yp); yn <- yp + gap * sum(wv * eta)
      gn <- (1 + gap * ey) * gp + gap * wv
      z[[outcome]][r] <- yn; grad[r, ] <- gn; rec[r] <- TRUE
      yp <- yn; gp <- gn; prev <- r
    }
  }
  back <- order(z$.li_iirr_row); out <- z[back, , drop = FALSE]; out$.li_iirr_row <- NULL
  rownames(out) <- NULL
  list(data = out, n_imputed = sum(rec), n_unreconstructed = sum(unr),
       reconstructed = rec[back], gradient = grad[back, , drop = FALSE],
       eta = eta, vcov_eta = Ve, eta_scores = us, eta_bread = A,
       n_observed_increments = k, increment_design_rank = qr(W)$rank,
       increment_design_columns = ncol(W), increment_condition = kappa(A),
       formula = paste0("d", outcome, "/dt ~ ",
                        paste(c(li_covs, paste0("lag_", outcome)), collapse = " + ")),
       weighting = if (identical(covariance, "randomwalk"))
         "WLS weight dt (Var increment proportional to dt)" else "equal-weight increment rates")
}

.li_iirr_li <- function(d, id, time, outcome, obs_indicator, li_covs, covariance,
                        full = TRUE) {
  numeric_columns <- unique(c(time, outcome, li_covs))
  ordinary_numeric <- all(vapply(d[, numeric_columns, drop = FALSE], function(x)
    is.numeric(x) && !is.object(x) && is.null(dim(x)), logical(1)))
  if (outcome %in% li_covs || !ordinary_numeric)
    return(.li_iirr_li_loop(d, id, time, outcome, obs_indicator, li_covs, covariance))

  s <- .li_sorted_increments(d, id, time, outcome, obs_indicator, li_covs, ".li_iirr_row")
  p <- 2L + length(li_covs)
  nm <- c("(Intercept)", li_covs, paste0("lag_", outcome))
  a <- s$a; b <- s$b; gapv <- s$gap; k <- length(a)
  if (k < max(10L, p + 2L)) stop("Too few observed adjacent increments for LI.", call. = FALSE)
  W <- cbind(1, s$x[a, , drop = FALSE], s$y[a]); colnames(W) <- nm
  if (qr(W)$rank < ncol(W)) stop("Linear-increment design is rank deficient.", call. = FALSE)
  rate <- (s$y[b] - s$y[a]) / gapv
  ww <- if (identical(covariance, "randomwalk")) gapv else rep(1, k)
  A <- crossprod(W, W * ww); Ai <- .li_iirr_pinv(A)
  eta <- as.vector(Ai %*% crossprod(W, ww * rate)); names(eta) <- nm
  fw <- .li_forward_recursion(s, d, time, eta)
  out <- s$data; out[[outcome]] <- fw$y
  li <- list(data = out, n_imputed = sum(fw$rec), n_unreconstructed = sum(s$unobserved & !fw$rec),
             reconstructed = fw$rec, eta = eta)
  if (!full) return(li)
  er <- rate - as.vector(W %*% eta); us <- matrix(0, length(s$ids), p)
  tmp <- rowsum(W * (ww * er), s$si[a], reorder = FALSE)
  us[as.integer(rownames(tmp)), ] <- tmp
  Ve <- Ai %*% crossprod(us) %*% t(Ai); dimnames(Ve) <- list(nm, nm)
  c(li, list(vcov_eta = Ve, eta_scores = us, eta_bread = A,
       n_observed_increments = k, increment_design_rank = qr(W)$rank,
       increment_design_columns = ncol(W), increment_condition = kappa(A),
       formula = paste0("d", outcome, "/dt ~ ",
                        paste(c(li_covs, paste0("lag_", outcome)), collapse = " + ")),
       weighting = if (identical(covariance, "randomwalk"))
         "WLS weight dt (Var increment proportional to dt)" else "equal-weight increment rates"))
}

# Forward and reverse increment fits use observed adjacent pairs.
.li_iirr_reconstruct <- function(d, id, time, outcome, obs_indicator,
                                 visit_covs, long_covs, random_covs, covariance,
                                 leading_imputation, full = TRUE) {
  li <- .li_iirr_li(d, id, time, outcome, obs_indicator, visit_covs, covariance, full)
  .li_reverse_reconstruct(li, d, id, time, outcome, obs_indicator,
    unique(c(visit_covs, long_covs, random_covs)), covariance,
    leading_imputation, "LI+IIRR", full)
}

.li_iirr_gamma <- function(d, id, time, visit_covs, check = TRUE) {
  if (check) .li_iirr_const(d, id, visit_covs)
  ids <- unique(d[[id]]); si <- match(d[[id]], ids); first <- match(ids, d[[id]])
  V <- as.matrix(d[first, visit_covs, drop = FALSE]); storage.mode(V) <- "double"
  if (any(!is.finite(V))) stop("Baseline visit covariates must be finite.")
  m <- tabulate(si[is.finite(d[[time]])], nbins = length(ids)); M <- sum(m)
  if (!M) stop("No visit events.")
  es <- colSums(V * m)
  cached_g <- cached_fg <- NULL
  fg <- function(g) {
    if (!is.null(cached_g) && identical(g, cached_g)) return(cached_fg)
    lp <- as.vector(V %*% g); a <- max(lp); w <- exp(lp-a); sw <- sum(w)
    value <- list(v = M*(a+log(sw))-sum(es*g), gr = M*colSums(V*w)/sw-es)
    cached_g <<- g; cached_fg <<- value
    value
  }
  o <- stats::optim(rep(0,ncol(V)), function(g) fg(g)$v, function(g) fg(g)$gr,
                    method="BFGS", control=list(reltol=1e-10,maxit=1000))
  g <- o$par; names(g) <- visit_covs
  list(gamma=g, ids=ids, si=si, first=first, V=V, m=m, M=M,
       converged=o$convergence==0L, code=o$convergence,
       score_max=max(abs(fg(g)$gr)))
}

# full = FALSE returns coefficients and the convergence flag.
# Visit covariates must be constant within each subject.
.li_iirr_iirr <- function(d, id, time, outcome, visit_covs, long_covs, full = TRUE) {
  .li_require_completed(d, id, time, outcome, "LI+IIRR")
  .li_require_design(d, id, visit_covs, "LI+IIRR")
  .li_require_design(d, id, long_covs, "LI+IIRR", rows = is.finite(d[[time]]))
  ga <- .li_iirr_gamma(d,id,time,visit_covs,check=full||outcome%in%visit_covs)
  ok <- is.finite(d[[time]])
  dd <- d[ok,,drop=FALSE]; sid <- ga$si[ok]
  X <- stats::model.matrix(stats::reformulate(c(long_covs,time)),dd); y <- dd[[outcome]]
  Vr <- as.matrix(dd[,visit_covs,drop=FALSE]); storage.mode(Vr)<-"double"
  lp <- as.vector(Vr%*%ga$gamma); if(any(abs(lp)>700)) stop("IIRR predictor overflow.")
  w <- exp(-lp); A <- crossprod(X,X*w); Ai <- .li_iirr_pinv(A)
  beta <- as.vector(Ai%*%crossprod(X,w*y)); names(beta)<-colnames(X)
  q<-length(ga$gamma);p<-length(beta);n<-length(ga$ids)
  psi <- function(par){
    g<-par[seq_len(q)];b<-par[q+seq_len(p)]

    Ug<-.li_iirr_gamma_subject_score(g,ga$V,ga$m)
    wr<-exp(-as.vector(Vr%*%g)); rr<-y-as.vector(X%*%b)
    tmp<-rowsum(X*(wr*rr),sid,reorder=FALSE); Ub<-matrix(0,n,p)
    Ub[as.integer(rownames(tmp)),]<-tmp; cbind(Ug,Ub)
  }
  lp0<-as.vector(ga$V%*%ga$gamma);ew<-exp(lp0-max(lp0));rr<-y-as.vector(X%*%beta)
  ld<-.Machine$sizeof.longdouble>0L
  total<-function(z,k){
    tz<-if(k<=q){
      g<-z[seq_len(q)];lp<-as.vector(ga$V%*%g)
      li_iirr_total_cpp(ga$V,ga$m,exp(lp-max(lp)),X,exp(-as.vector(Vr%*%g))*rr,sid,ld)
    } else li_iirr_total_cpp(ga$V,ga$m,ew,X,w*(y-as.vector(X%*%z[q+seq_len(p)])),sid,ld)
    if(is.null(tz))colSums(psi(z)) else tz
  }
  par<-c(ga$gamma,beta)
  J<-matrix(NA_real_,length(par),length(par))
  for(k in seq_along(par)){h<-1e-5*(abs(par[k])+1);a<-b<-par;a[k]<-a[k]+h;b[k]<-b[k]-h
    J[,k]<-(total(a,k)-total(b,k))/(2*h)}
  bread <- -J; BI <- .li_iirr_pinv(bread)
  if(!full)return(list(coef=beta,converged=ga$converged))
  P<-psi(par); VF<-BI%*%crossprod(P)%*%t(BI)
  nm<-c(paste0("gamma:",visit_covs),names(beta));dimnames(VF)<-list(nm,nm)
  VB<-VF[q+seq_len(p),q+seq_len(p),drop=FALSE]
  list(coef=beta,vcov=VB,vcov_full=VF,gamma=ga$gamma,converged=ga$converged,
       code=ga$code,score_max=ga$score_max,condition=kappa(bread),
       n_visit=ga$M,n_outcome=nrow(dd),zero_visit=sum(ga$m==0L),n_subject=n)
}

.li_iirr_seed <- function(seed, expr) {
  if (length(seed) != 1L || !is.finite(seed)) stop("Invalid bootstrap seed.", call. = FALSE)
  had <- exists(".Random.seed", .GlobalEnv, inherits = FALSE)
  if (had) old <- get(".Random.seed", .GlobalEnv, inherits = FALSE)
  on.exit(if (had) assign(".Random.seed", old, .GlobalEnv) else if
    (exists(".Random.seed", .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
  set.seed(as.integer(seed)); force(expr)
}

.fit_li_iirr <- function(
    data,id="id",time="time",outcome="Y",obs_indicator="R",
    visit_covs=c("Z","X"),long_covs=c("Z","X"),random_covs="Z",
    li_covariance=c("randomwalk","rate_independence"),
    censor_col=if("C"%in%names(data))"C"else NULL,
    inference=c("conditional","patient_bootstrap"),bootstrap_reps=200L,
    bootstrap_seed=1901L,keep_completed=FALSE,
    leading_imputation=c("reverse_li","error"),
    unobserved_subjects=c("exclude","error")) {
  li_covariance<-match.arg(li_covariance)
  inference<-match.arg(inference)
  leading_imputation<-match.arg(leading_imputation)
  unobserved_subjects<-match.arg(unobserved_subjects)
  d<-.li_iirr_check(data,c(id,time,outcome,obs_indicator,visit_covs,long_covs,random_covs,
                        if(!is.null(censor_col))censor_col))
  selection<-.li_select_subjects(d,id,time,outcome,obs_indicator,unobserved_subjects,"LI+IIRR")
  d<-selection$data
  .li_require_design(d,id,visit_covs,"LI+IIRR")
  .li_require_design(d,id,unique(c(long_covs,random_covs)),"LI+IIRR",
                     rows=is.finite(d[[time]]))
  if(!is.null(censor_col)){
    cc<-as.numeric(d[match(unique(d[[id]]),d[[id]]),censor_col])
    if(any(!is.finite(cc)))stop("Every subject needs a finite censoring time.",call.=FALSE)
    if(diff(range(cc))>1e-10) stop("LI-IIRR: common-risk IIRR does not support varying censoring.",call.=FALSE)
  }
  li<-.li_iirr_reconstruct(d,id,time,outcome,obs_indicator,visit_covs,long_covs,
                          random_covs,li_covariance,leading_imputation)
  f<-.li_iirr_iirr(li$data,id,time,outcome,visit_covs,long_covs)
  Vout<-f$vcov;boot_success<-0L;boot_failure<-character()
  if(inference=="patient_bootstrap"){
    if(length(bootstrap_reps)!=1L||!is.finite(bootstrap_reps)||bootstrap_reps<2L||
       bootstrap_reps!=as.integer(bootstrap_reps))stop("`bootstrap_reps` must be an integer >= 2.",call.=FALSE)
    resample<-.li_resampler(d,id)
    bb<-.li_iirr_seed(bootstrap_seed,lapply(seq_len(bootstrap_reps),function(b){
      tryCatch({db<-resample()
        lb<-.li_iirr_reconstruct(db,id,time,outcome,obs_indicator,visit_covs,long_covs,
                                random_covs,li_covariance,leading_imputation,full=FALSE)
        zb<-.li_iirr_iirr(lb$data,id,time,outcome,visit_covs,long_covs,full=FALSE)
        if(!isTRUE(zb$converged)||!all(is.finite(zb$coef)))
          stop("Nonconverged LI+IIRR bootstrap fit.",call.=FALSE)
        zb$coef},
        error=function(e)structure(list(message=conditionMessage(e)),class="li_iirr_failure"))
    }))
    nm<-names(f$coef)
    good<-vapply(bb,function(x)is.numeric(x)&&length(x)==length(nm)&&
      !anyDuplicated(names(x))&&setequal(names(x),nm)&&all(is.finite(x[nm])),logical(1));boot_success<-sum(good)
    boot_failure<-vapply(bb[!good],function(x)if(inherits(x,"li_iirr_failure"))x$message else
      "coefficient schema mismatch or non-finite coefficient","")
    if(boot_success<ceiling(.95*bootstrap_reps))
      stop("Fewer than 95% successful patient bootstrap fits: ",boot_success,"/",bootstrap_reps,call.=FALSE)
    Q<-do.call(rbind,lapply(bb[good],`[`,nm));Vout<-stats::cov(Q)
    dimnames(Vout)<-list(nm,nm)
  }
  ans<-list(method_key="li_iirr",method="LI+IIRR",
            estimand=paste0("Visit-process IIRR-weighted longitudinal coefficient after recursive LI reconstruction",
              if(selection$n_subjects_excluded>0L)
                " in the retained cohort after excluding subjects without any observed outcome" else ""),
            coefficients=f$coef,vcov=Vout,
            se=sqrt(pmax(diag(Vout),0)),
            vcov_type=if(inference=="patient_bootstrap")
              "patient-cluster bootstrap of the full LI+IIRR procedure" else
              "variance (plug-in): stacked visit-process IIRR sandwich conditional on LI reconstruction",
            nuisance=list(gamma=f$gamma,increment_coefficients=li$eta,
                          increment_vcov=li$vcov_eta,
                          backward_increment_coefficients=li$backward$coefficients,
                          backward_increment_vcov=li$backward$vcov,
                          conditional_vcov_type="variance (plug-in): stacked visit-process IIRR sandwich conditional on forward/reverse LI reconstruction",
                          conditional_vcov=f$vcov),converged=f$converged,
            diagnostics=list(ok=isTRUE(f$converged&&all(is.finite(f$coef))&&all(is.finite(f$vcov))),
                             unobserved_subjects=unobserved_subjects,
                             n_subjects_input=selection$n_subjects_input,
                             n_visits_input=selection$n_visits_input,
                             n_subjects_excluded_unobserved=selection$n_subjects_excluded,
                             n_visits_excluded_unobserved=selection$n_visits_excluded,
                             analysis_population=if(selection$n_subjects_excluded>0L)
                               "retained cohort after excluding subjects without any observed outcome" else
                               "input cohort; no subjects excluded",
                             bootstrap_population="retained eligible subjects; conditional on cohort eligibility",
                             n_subjects=f$n_subject,n_visits=f$n_visit,
                             zero_visit_subjects=f$zero_visit,
                             n_observed_before_imputation=sum(is.finite(d[[time]])&
                               !is.na(d[[obs_indicator]])&d[[obs_indicator]]==1&is.finite(d[[outcome]])),
                             n_imputed=li$n_imputed,n_analysis=f$n_outcome,
                             n_imputed_forward=li$n_imputed_forward,
                             n_imputed_backward=li$n_imputed_backward,
                             backward_model_used=!is.null(li$backward),
                             leading_imputation=leading_imputation,
                             backward_model=li$backward[c("formula","predictors","weighting",
                               "n_observed_increments","rank","columns","condition")],
                             n_unreconstructed=li$n_unreconstructed,
                             all_visit_outcome_coverage=f$n_outcome==f$n_visit,
                             unreconstructed_policy="error; preserve all actual visits of retained subjects",
                             n_observed_increments=li$n_observed_increments,
                             increment_design_rank=li$increment_design_rank,
                             increment_design_columns=li$increment_design_columns,
                             increment_condition_number=li$increment_condition,
                             increment_weighting=li$weighting,
                             original_R_preserved=identical(li$data[[obs_indicator]],d[[obs_indicator]]),
                             downstream_process="all actual visits N(t) of retained subjects; reconstructed outcomes are not new process events",
                             li_uncertainty_included=inference=="patient_bootstrap",
                             inference=inference,
                             bootstrap_requested=if(inference=="patient_bootstrap")as.integer(bootstrap_reps)else 0L,
                             bootstrap_success=boot_success,bootstrap_failures=boot_failure,
                             gamma_score_max=f$score_max,condition_number=f$condition),
            assumptions=c("Pooled linear increment-rate model and discrete-time independent censoring (DTIC)",
                          "Observed adjacent R=1 pairs train increments; forward recursion re-anchors at returned observations",
                          "Re-anchoring after nonmonotone missingness requires a compatible return-to-observation mechanism",
                          "Every actual visit has a finite observed or LI-reconstructed outcome; at least one observed anchor is needed per retained subject with visits",
                          if(selection$n_subjects_excluded>0L)
                            "Inference concerns the retained cohort; visit/recording-based exclusion can change the visit and outcome models and does not establish unbiasedness for the original cohort",
                          if(!is.null(li$backward))c(
                            "Leading outcomes use a separately fitted reverse-time increment-rate model, not inversion of forward coefficients",
                            "Reverse conditional means given later outcomes, covariates and the visit process follow the pooled linear model; observed adjacent pairs represent leading missing runs",
                            "Reverse reconstruction errors have zero conditional mean given the visit process, preserving the IIRR-weighted outcome score; forward DTIC alone does not imply this assumption"),
                          "Common follow-up and time-invariant visit covariates",
                          "IIRR visiting-at-random/separability assumptions",
                          if(inference=="conditional")"Covariance conditions on LI reconstruction" else
                            "Subjects are the independent resampling units"),call=match.call())
  if(keep_completed)ans$completed_data<-li$data
  class(ans)<-c("li_iirr");ans
}
