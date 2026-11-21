.liang_marker_stable_risk <- function(log_weight, values, times, entry, censor) {
  values <- as.matrix(values); n <- nrow(values)
  stopifnot(length(log_weight) == n, length(entry) == n, length(censor) == n)
  if (anyNA(log_weight) || any(log_weight == Inf) || any(!is.finite(values)))
    stop("Non-finite risk-set inputs.", call. = FALSE)
  ly_stable_risk_cpp(log_weight, values, times, entry, censor)
}

.liang_marker_stable_risk_reference <- .liang_marker_stable_risk
.liang_marker_stable_risk <- function(log_weight, values, times, entry, censor) {
  values <- as.matrix(values); n <- nrow(values)
  reference <- .liang_marker_stable_risk_reference
  if (length(times) < 2L || length(log_weight) != n ||
      length(entry) != n || length(censor) != n ||
      !isTRUE(all(entry == entry[1L])) ||
      any(!is.finite(times)) || anyNA(entry) || any(entry == Inf) || any(!is.finite(censor)))
    return(reference(log_weight, values, times, entry, censor))

  key <- findInterval(times, sort(censor), left.open = TRUE)
  key[times <= entry[1L]] <- -1L
  first <- which(!duplicated(key))
  if (length(first) == length(times))
    return(reference(log_weight, values, times, entry, censor))
  expanded <- match(key, key[first])
  z <- reference(log_weight, values, times[first], entry, censor)
  list(log_sum = z$log_sum[expanded], mean = z$mean[expanded, , drop = FALSE],
       covariance = z$covariance[, , expanded, drop = FALSE],
       n_risk = z$n_risk[expanded])
}

.liang_marker_stable_interval <- function(times, log_weight, values, entry, censor) {
  values <- as.matrix(values)
  if (anyNA(log_weight)||any(log_weight==Inf)||any(!is.finite(values)))
    stop("Non-finite interval-sum inputs.",call.=FALSE)
  ly_stable_interval_cpp(times, log_weight, values, entry, censor)
}

.liang_marker_stable_risk_loop <- .liang_marker_stable_risk
.liang_marker_stable_risk_geometry <- function(times, entry, censor) {
  key <- findInterval(times, sort(unique(c(entry, censor))), left.open = TRUE)
  if (isFALSE(is.unsorted(times))) {
    new <- diff(c(-1L, key)) != 0L; ut <- times[which(new)]; at <- cumsum(new)
  } else {
    uk <- sort(unique(key)); ut <- times[match(uk, key)]; at <- match(key, uk)
  }
  K <- length(ut); lo <- findInterval(entry, ut) + 1L; hi <- findInterval(censor, ut)
  ok <- which(lo <= hi)
  count <- function(j) cumsum(tabulate(lo[j], K)) - cumsum(c(0L, tabulate(hi[j], K)[-K]))
  list(ut = ut, K = K, at = at, lo = lo, hi = hi, ok = ok, count = count, n_risk = count(ok),
       unusable = any(!is.finite(times)) || anyNA(entry) || anyNA(censor) || any(entry > censor))
}

# at maps times to risk states; per-time fallback results omit at.
.liang_marker_stable_risk_states <- function(log_weight, values, times, entry, censor, geometry = NULL) {
  fallback <- function() .liang_marker_stable_risk_loop(log_weight, values, times, entry, censor)
  values <- as.matrix(values); n <- nrow(values); q <- ncol(values)
  if (!length(times) || !n || !q || length(log_weight) != n || length(entry) != n ||
      length(censor) != n || anyNA(log_weight) || any(log_weight == Inf) ||
      any(!is.finite(values)) || (if (is.null(geometry)) any(!is.finite(times)) ||
                                  anyNA(entry) || anyNA(censor) || any(entry > censor) else geometry$unusable))
    return(fallback())
  if (is.null(geometry)) geometry <- .liang_marker_stable_risk_geometry(times, entry, censor)
  ut <- geometry$ut; K <- geometry$K; at <- geometry$at
  lo <- geometry$lo; hi <- geometry$hi; ok <- geometry$ok; n_risk <- geometry$n_risk
  fin <- ok[is.finite(log_weight[ok])]
  if (!length(fin) || any((if (length(fin) < length(ok)) geometry$count(fin) else n_risk) == 0L))
    return(fallback())
  core <- lw_liang_risk_core(log_weight, values, fin, lo, hi, K)
  if (is.null(core)) return(fallback())
  logs <- core$log_sum; means <- core$mean; covs <- core$covariance; bad <- core$bad
  if (any(bad)) {
    stable <- .liang_marker_stable_risk_loop(log_weight, values, ut[bad], entry, censor)
    logs[bad] <- stable$log_sum; means[bad, ] <- stable$mean
    covs[, , bad] <- stable$covariance; n_risk[bad] <- stable$n_risk
  }
  list(log_sum = logs, mean = means, covariance = covs, n_risk = n_risk, at = at)
}

.liang_marker_stable_risk <- function(log_weight, values, times, entry, censor, geometry = NULL,
                                      covariance = TRUE) {
  z <- .liang_marker_stable_risk_states(log_weight, values, times, entry, censor, geometry)
  at <- z[["at"]]
  if (is.null(at)) return(z)
  list(log_sum = z$log_sum[at], mean = z$mean[at, , drop = FALSE],
       covariance = if (covariance) z$covariance[, , at, drop = FALSE], n_risk = z$n_risk[at])
}

.liang_marker_stable_interval_loop <- .liang_marker_stable_interval
.liang_marker_stable_interval <- function(times, log_weight, values, entry, censor) {
  fallback <- function() .liang_marker_stable_interval_loop(times, log_weight, values, entry, censor)
  values <- as.matrix(values); q <- ncol(values); K <- length(times); n <- length(entry)
  if (!K || !q || length(log_weight) != K || nrow(values) != K || length(censor) != n ||
      any(!is.finite(times)) || anyNA(log_weight) || any(log_weight == Inf) ||
      any(!is.finite(values)) || anyNA(entry) || anyNA(censor))
    return(fallback())
  if (is.unsorted(times)) {
    ord <- order(times); tt <- times[ord]; lw <- log_weight[ord]; v <- values[ord, , drop = FALSE]
  } else {
    tt <- times; lw <- log_weight; v <- values
  }
  lo <- findInterval(entry, tt) + 1L; hi <- findInterval(censor, tt)
  z <- lw_liang_interval_core(lw, v, lo, hi)
  if (is.null(z)) return(fallback())
  outl <- z$log_sum; outm <- z$mean; ii <- z$bad
  if (length(ii)) {
    stable <- .liang_marker_stable_interval_loop(times, log_weight, values, entry[ii], censor[ii])
    outl[ii] <- stable$log_sum; outm[ii, ] <- stable$mean
  }
  list(log_sum = outl, mean = outm)
}

.liang_marker_stable_compensator <- function(eta,V,tu,dn,logs0,vbar,entry,censor){
  z<-.liang_marker_stable_interval(tu,log(dn)-logs0,vbar,entry,censor)
  scale<-exp(eta+z$log_sum)
  (V-z$mean)*scale
}

.liang_marker_check<-function(data,vars){d<-as.data.frame(data);m<-setdiff(unique(vars),names(d));if(length(m))stop("Missing columns: ",paste(m,collapse=", "),call.=FALSE);if(anyNA(d[[vars[1L]]]))stop("Subject id cannot be missing.",call.=FALSE);d}
.liang_marker_inv<-function(A,tol=sqrt(.Machine$double.eps)){
  A<-as.matrix(A);z<-svd(A)
  if(!length(z$d)||max(z$d)==0)stop("Singular Liang equation.",call.=FALSE)
  if(all(z$d>tol*max(z$d)))return(z$v%*%(t(z$u)/z$d))
  cs<-apply(abs(A),2L,max);cs[cs==0]<-1
  As<-sweep(A,2L,cs,"/");rs<-apply(abs(As),1L,max);rs[rs==0]<-1
  As<-As/rs;z<-svd(As)
  if(!length(z$d)||max(z$d)==0||any(z$d<=tol*max(z$d)))
    stop("Liang outcome design is not identified.",call.=FALSE)
  sweep((z$v%*%(t(z$u)/z$d))/cs,2L,rs,"/")
}
.liang_marker_risk <- function(g,V,tu,entry,censor){
  z<-drop(V%*%g);risk<-.liang_marker_stable_risk(z,V,tu,entry,censor)
  if(any(!is.finite(risk$log_sum)))stop("Empty risk set at an event time.",call.=FALSE)
  list(z=z,vbar=risk$mean,logs0=risk$log_sum,covariance=risk$covariance)
}
.liang_marker_gamma_evaluator <- function(V,ei,et,entry,censor) {

  n<-nrow(V);q<-ncol(V);tu<-sort(unique(et))
  em<-match(et,tu);dn<-tabulate(em,nbins=length(tu))
  Ve<-V[ei,,drop=FALSE];geometry<-.liang_marker_stable_risk_geometry(tu,entry,censor);atem<-geometry$at[em]
  cached_g<-NULL;cached_value<-NULL;cached_risk<-NULL
  evaluate<-function(g) {
    g<-as.numeric(g)

    if (!is.null(cached_g) && identical(g,cached_g)) return(cached_value)
    z<-drop(V%*%g);R<-.liang_marker_stable_risk_states(z,V,tu,entry,censor,geometry)
    if(any(!is.finite(R$log_sum)))stop("Empty risk set at an event time.",call.=FALSE)
    states<-!is.null(R[["at"]]);at<-if(states)R$at else seq_along(tu)
    information<-lw_indexed_cov_sum(R$covariance,dn,at,q)
    U<-lw_centered_group_sum(Ve,R$mean,if(states)atem else em,ei,n)
    value<-list(objective=lw_indexed_dot(dn,R$log_sum,at)-lw_indexed_sum(z,ei),
                score=colSums(U),individual=U,information=information)
    cached_g<<-g;cached_value<<-value;cached_risk<<-list(z=z,risk=R,at=at)
    value
  }
  risk<-function(g) {
    evaluate(g);R<-cached_risk$risk;at<-cached_risk$at
    list(z=cached_risk$z,vbar=R$mean[at,,drop=FALSE],logs0=R$log_sum[at],geometry=geometry)
  }
  breslow<-function(g) {
    evaluate(g)
    log_jump<-log(dn)-cached_risk$risk$log_sum[cached_risk$at]
    list(time=tu,jump=exp(log_jump),log_jump=log_jump)
  }
  list(evaluate=evaluate,breslow=breslow,risk=risk)
}
.liang_marker_gamma <- function(g,V,ei,et,entry,censor,individual=FALSE){
  .liang_marker_gamma_evaluator(V,ei,et,entry,censor)$evaluate(g)
}
.liang_marker_breslow <- function(g,V,et,entry,censor){
  tu<-sort(unique(et));dn<-tabulate(match(et,tu),nbins=length(tu))
  R<-.liang_marker_risk(g,V,tu,entry,censor);log_jump<-log(dn)-R$logs0
  list(time=tu,jump=exp(log_jump),log_jump=log_jump)
}

.liang_marker_baseline_design <- function(time_values,time_col,estimate_intercept,baseline_formula,
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

.liang_marker_outcome_case_derivative <- function(Dsub,Dbar,Dc,coef,y,oi,om,tu,
                                                  log_weight,log_risk_sum,entry,censor) {

  n<-nrow(Dsub);p<-ncol(Dsub);K<-length(tu)
  residual<-y-drop(Dsub[oi,,drop=FALSE]%*%coef)
  direct<-lw_group_sum(Dc*residual,oi,n)
  residual_sum<-lw_group_sum(matrix(residual,ncol=1L),om,K)[,1L]
  z<-.liang_marker_stable_interval(tu,-log_risk_sum,
                                   cbind(residual_sum,Dbar*residual_sum),entry,censor)
  mult<-exp(log_weight+z$log_sum)
  profile<-(-Dsub*z$mean[,1L]+z$mean[,1L+seq_len(p),drop=FALSE])*mult
  psi<-direct+profile
  if(any(!is.finite(psi)))stop("Non-finite Liang conditional case-weight derivative.",call.=FALSE)
  list(individual=psi,direct=direct,profile=profile)
}

.liang_marker_stacked <- function(V, gamma, gs, bl, ei, et, entry, censor,
                                  mu, m, sigma2, theta_identified, Q0, coef_all, design, Hi, psi,
                                  y, oi, estimate_intercept, profile, joint_names, subject_ids,
                                  retain_influence = FALSE, rate_risk = NULL) {
  n <- nrow(V); p <- length(coef_all)
  eta <- drop(V %*% gamma)
  tu <- bl$time; em <- match(et, tu); dn <- tabulate(em, length(tu))
  if (is.null(rate_risk)) rate_risk <- .liang_marker_risk(gamma, V, tu, entry, censor)
  sum_subject <- function(value, index) lw_group_sum(as.matrix(value), index, n)
  gamma_score <- gs$individual - .liang_marker_stable_compensator(
    eta, V, tu, dn, rate_risk$logs0, rate_risk$vbar, entry, censor)
  gamma_inverse <- .liang_marker_inv(gs$information)
  if_gamma <- gamma_score %*% t(gamma_inverse)

  # Adjoint derivative: sum_i G_i d mu_i.
  apply_mu <- function(G) {
    G <- as.matrix(G)
    if (nrow(G) != n || any(!is.finite(G)))
      stop("Non-finite Liang exposure-chain derivative.", call. = FALSE)
    rg <- .liang_marker_stable_risk(eta, G, tu, entry, censor, rate_risk[["geometry"]], FALSE)$mean
    direct <- sum_subject(rg[em, , drop = FALSE], ei)
    integral <- .liang_marker_stable_interval(tu, bl$log_jump, rg, entry, censor)
    compensator <- integral$mean * exp(eta + integral$log_sum)
    derivative_gamma <- crossprod(V, G * mu) - crossprod(rate_risk$vbar, rg * dn)
    direct - compensator + if_gamma %*% derivative_gamma
  }

  if_sigma2 <- numeric(n)
  if (theta_identified) {
    h <- m * (m - 1) - (1 + sigma2) * mu^2
    if_sigma2 <- (h + drop(apply_mu(matrix(-2 * (1 + sigma2) * mu, n, 1L)))) / sum(mu^2)
  }

  residual <- y - drop(design %*% coef_all)
  residual_subject <- drop(sum_subject(residual, oi))
  G_B <- matrix(0, n, p)
  if (theta_identified) {
    iq <- seq.int(p - ncol(Q0) + 1L, p)
    derivative_design <- matrix(0, n, p)
    derivative_design[, iq] <- Q0
    theta_mean <- drop(Q0 %*% coef_all[iq])
    if (estimate_intercept) {
      instrument_sum <- sum_subject(design, oi)
      G_B <- derivative_design * residual_subject - instrument_sum * theta_mean
    } else {
      # profile$profile is -w_i integral (D_i-Dbar) sum(residual)/S0.
      ot <- profile$outcome_time
      tuo <- profile$time_grid
      om <- match(ot, tuo)
      residual_time <- lw_group_sum(matrix(residual, ncol = 1L), om, length(tuo))[, 1L]
      z <- .liang_marker_stable_interval(tuo,
                                         -profile$log_risk_sum, matrix(residual_time, ncol = 1L), entry, censor)
      center_residual <- z$mean[, 1L] * exp(profile$log_weight + z$log_sum)
      instrument_sum <- sum_subject(profile$instrument, oi)
      G_B <- derivative_design * (residual_subject - center_residual) - instrument_sum * theta_mean
    }
  }
  dB_mu <- if (theta_identified) -sigma2 * (1 + m * sigma2) / (1 + mu * sigma2)^2 else numeric(n)
  dB_sigma2 <- if (theta_identified) (m - mu) / (1 + mu * sigma2)^2 else numeric(n)
  # w_i = m_i / Lambda_i.
  G_logLambda <- if (estimate_intercept) matrix(0, n, p) else -profile$profile
  G_mu <- G_B * dB_mu
  positive <- mu > 0
  if (any(!positive & rowSums(abs(G_logLambda)) > 0))
    stop("Liang centering derivative is nonzero at zero exposure.", call. = FALSE)
  G_mu[positive, ] <- G_mu[positive, , drop = FALSE] + G_logLambda[positive, , drop = FALSE] / mu[positive]
  exposure_chain <- apply_mu(G_mu) - if_gamma %*% crossprod(V, G_logLambda)
  frailty_chain <- outer(if_sigma2, colSums(G_B * dB_sigma2))
  effective <- psi + exposure_chain + frailty_chain
  if_outcome <- effective %*% t(Hi)
  colnames(if_gamma) <- paste0("gamma:", colnames(V))
  colnames(if_outcome) <- joint_names
  influence <- cbind(if_gamma, sigma2 = if_sigma2, if_outcome)
  if (any(!is.finite(influence)))
    stop("Non-finite Liang all-stage case influence.", call. = FALSE)
  raw_sum <- max(abs(colSums(influence)))
  scaled_sum <- raw_sum / max(1, sqrt(sum(influence^2)))
  if (scaled_sum > 1e-5)
    stop("Liang stacked influence fails the uniform-case-weight check.", call. = FALSE)
  influence <- sweep(influence, 2L, colMeans(influence), "-")
  rownames(influence) <- as.character(subject_ids)
  covariance <- crossprod(influence)
  if (!theta_identified) covariance["sigma2", ] <- covariance[, "sigma2"] <- NA_real_
  outcome_covariance <- covariance[joint_names, joint_names, drop = FALSE]
  gamma_names <- paste0("gamma:", colnames(V))
  gamma_covariance <- covariance[gamma_names, gamma_names, drop = FALSE]
  dimnames(gamma_covariance) <- list(colnames(V), colnames(V))
  scope <- if (theta_identified)
    "all-stage patient stacked sandwich: rate, profiled Breslow exposure, frailty moment, posterior frailty and outcome equation" else
      "all-stage patient stacked sandwich conditional on the selected no-frailty branch; frailty-variance Wald inference unavailable"
  if (!estimate_intercept) scope <- paste0(scope, "; empirical instrument center and estimated centering weights differentiated")
  else scope <- paste0(scope, "; chosen outcome baseline basis treated as fixed")
  answer <- list(vcov_all = covariance, vcov_outcome = outcome_covariance,
                 vcov_gamma = gamma_covariance,
                 se_sigma2 = if (theta_identified) sqrt(max(covariance["sigma2", "sigma2"], 0)) else NA_real_,
                 regular = theta_identified, scope = scope,
                 raw_influence_sum_max = raw_sum, scaled_influence_sum_max = scaled_sum,
                 nuisance_estimation_propagated = TRUE,
                 conditional_active_branch_inference_available = TRUE)
  if (retain_influence) {
    if (!theta_identified) influence[, "sigma2"] <- NA_real_
    answer$influence_all <- influence
    answer$effective_outcome_score <- effective
    answer$conditional_outcome_score <- psi
    answer$exposure_chain <- exposure_chain
    answer$frailty_chain <- frailty_chain
  }
  answer
}


.fit_liang_marker<-function(
    data,id="id",time="time",outcome="Y",obs_indicator="R",
    visit_covs=c("Z","X"),long_covs=c("Z","X"),random_covs="Z",
    process=c("marker","visit"),entry_col=NULL,censor_col=NULL,
    optimizer_control=list(maxit=1000L),estimate_intercept=FALSE,baseline_formula=~1,
    inference="stacked",retain_influence=FALSE){
  inference<-match.arg(inference,"stacked")
  if(!is.logical(retain_influence)||length(retain_influence)!=1L||is.na(retain_influence))
    stop("retain_influence must be TRUE or FALSE.",call.=FALSE)
  process<-match.arg(process);d<-.liang_marker_check(data,c(id,time,outcome,obs_indicator,visit_covs,long_covs,random_covs,entry_col,censor_col))
  ids<-unique(d[[id]]);si<-match(d[[id]],ids);first<-match(ids,d[[id]]);n<-length(ids)
  V<-as.matrix(d[first,visit_covs,drop=FALSE]);Xs<-as.matrix(d[first,long_covs,drop=FALSE]);Qs<-if(length(random_covs))as.matrix(d[first,random_covs,drop=FALSE])else matrix(numeric(),n,0)
  storage.mode(V)<-"double";storage.mode(Xs)<-"double";storage.mode(Qs)<-"double"
  if(any(!is.finite(cbind(V,Xs,Qs))))stop("Baseline covariates must be finite.",call.=FALSE)
  for(v in unique(c(visit_covs,long_covs,random_covs)))if(any(is.finite(d[[v]])&abs(d[[v]]-d[[v]][first][si])>1e-10,na.rm=TRUE))stop("Liang requires time-invariant covariates: ",v,call.=FALSE)
  ft<-is.finite(d[[time]]);if(!any(ft))stop("No visits.",call.=FALSE);entry<-if(is.null(entry_col))rep(-Inf,n)else as.numeric(d[[entry_col]][first]);censor<-if(is.null(censor_col))rep(max(d[[time]][ft]),n)else as.numeric(d[[censor_col]][first])
  if(anyNA(entry)||any(entry==Inf)||any(!is.finite(censor)))stop("Invalid entry/censor times.",call.=FALSE)
  if(any(entry>censor))stop("Entry time cannot exceed censoring time.",call.=FALSE)
  inwin<-ft&d[[time]]>entry[si]
  marker<-inwin&!is.na(d[[obs_indicator]])&d[[obs_indicator]]==1;ev<-if(process=="marker")marker else inwin
  oy<-marker&is.finite(d[[outcome]])
  er<-which(ev);or<-which(oy);if(!length(er)||!length(or))stop("No process events/outcomes.",call.=FALSE);ei<-si[er];et<-d[[time]][er];oi<-si[or];ot<-d[[time]][or];y<-d[[outcome]][or]
  baseline<-.liang_marker_baseline_design(ot,time,estimate_intercept,baseline_formula)
  rate_engine<-.liang_marker_gamma_evaluator(V,ei,et,entry,censor)
  gamma_at<-rate_engine$evaluate
  og<-stats::optim(rep(0,ncol(V)),function(g)gamma_at(g)$objective,gr=function(g)-gamma_at(g)$score,method="BFGS",control=optimizer_control,hessian=TRUE)

  .pg<-og$par
  for(.it in seq_len(25L)){
    .pp<-gamma_at(.pg)
    if(max(abs(.pp$score))<=1e-9*(1+length(et)))break
    .H<-.pp$information
    .st<-tryCatch(drop(solve(.H,.pp$score)),error=function(e)NULL);if(is.null(.st))break
    .f<-1;.acc<-FALSE
    while(.f>=2^-20){.c<-.pg+.f*.st;.cv<-gamma_at(.c)$objective
    if(is.finite(.cv)&&.cv<=.pp$objective+1e-12*(1+abs(.pp$objective))){.pg<-.c;.acc<-TRUE;break};.f<-.f/2}
    if(!.acc)break
  }
  og$par<-.pg;gs<-gamma_at(og$par)
  og$hessian<-gs$information
  if(max(abs(gs$score))>1e-5*(1+length(et)))stop("Rate nuisance did not converge.",call.=FALSE)
  bl<-rate_engine$breslow(og$par)

  exposure<-.liang_marker_stable_interval(bl$time,bl$log_jump,
                                          matrix(1,length(bl$time),1L),entry,censor)
  logLambda<-exposure$log_sum;m<-tabulate(ei,nbins=n)
  mu<-exp(drop(V%*%og$par)+logLambda)
  den<-sum(mu^2);if(!is.finite(den)||den<=0)stop("Frailty variance moment is unidentified.",call.=FALSE)
  raw_sigma2<-sum(m*(m-1)-mu^2)/den;sigma2<-max(raw_sigma2,0);B<-(1+m*sigma2)/(1+mu*sigma2)-1
  Q0<-cbind(`frailty intercept`=1,Qs);colnames(Q0)<-c("frailty_intercept",if(ncol(Qs))paste0("frailty_",colnames(Qs)))
  if(sigma2<=sqrt(.Machine$double.eps)){Dsub<-Xs;theta_identified<-FALSE}else{Dsub<-cbind(Xs,Q0*B);theta_identified<-TRUE}
  if(estimate_intercept) {

    F<-baseline$design;design<-cbind(F,Dsub[oi,,drop=FALSE])
    if(anyDuplicated(colnames(design)))stop("Baseline and outcome coefficient names must be distinct.",call.=FALSE)
    bread<-crossprod(design);Hi<-.liang_marker_inv(bread)
    coef_all<-drop(Hi%*%crossprod(design,y));names(coef_all)<-colnames(design)
    residual<-y-drop(design%*%coef_all)
    psi<-lw_group_sum(design*residual,oi,n)
    ix<-seq_len(ncol(F)+ncol(Xs))
    baseline_coef<-coef_all[seq_len(ncol(F))]
    joint_names<-c(colnames(F),paste0("beta:",colnames(Xs)),
                   if(theta_identified)paste0("theta:",colnames(Q0)))
    profile<-NULL
    centered_rank<-NA_integer_
  } else {

    tuo<-sort(unique(ot));om<-match(ot,tuo)
    lw<-rep(-Inf,n);positive<-m>0 & is.finite(logLambda)
    lw[positive]<-log(m[positive])-logLambda[positive]
    drisk<-.liang_marker_stable_risk(lw,Dsub,tuo,entry,censor,covariance=FALSE)
    if(any(!is.finite(drisk$log_sum)))stop("Liang centering denominator is zero at an outcome time.",call.=FALSE)
    Dbar<-drisk$mean;Dc<-Dsub[oi,,drop=FALSE]-Dbar[om,,drop=FALSE]
    design<-Dsub[oi,,drop=FALSE]
    bread<-crossprod(Dc,design);Hi<-.liang_marker_inv(bread)
    coef_all<-drop(Hi%*%crossprod(Dc,y));names(coef_all)<-colnames(Dsub)
    influence<-.liang_marker_outcome_case_derivative(Dsub,Dbar,Dc,coef_all,y,oi,om,tuo,
                                                     lw,drisk$log_sum,entry,censor)
    psi<-influence$individual;ix<-seq_len(ncol(Xs));baseline_coef<-NULL
    profile<-c(influence,list(outcome_time=ot,time_grid=tuo,log_risk_sum=drisk$log_sum,
                              log_weight=lw,instrument=Dc))
    joint_names<-c(paste0("beta:",colnames(Xs)),
                   if(theta_identified)paste0("theta:",colnames(Q0)))
    centered_rank<-qr(Dc)$rank
  }
  Vconditional<-Hi%*%crossprod(psi)%*%t(Hi)
  dimnames(Vconditional)<-list(joint_names,joint_names)
  stacked<-.liang_marker_stacked(V,og$par,gs,bl,ei,et,entry,censor,
                                 mu,m,sigma2,theta_identified,Q0,coef_all,design,Hi,psi,y,oi,
                                 estimate_intercept,profile,joint_names,ids,retain_influence,rate_engine$risk(og$par))
  Va<-stacked$vcov_outcome
  dimnames(Va)<-list(names(coef_all),names(coef_all))
  beta<-coef_all[ix];Vb<-Va[ix,ix,drop=FALSE]
  theta<-if(theta_identified)coef_all[-ix]else setNames(rep(NA_real_,ncol(Q0)),colnames(Q0))

  Vgamma_model<-tryCatch(.liang_marker_inv(og$hessian),error=function(e)NULL)
  if(!is.null(Vgamma_model))dimnames(Vgamma_model)<-list(visit_covs,visit_covs)
  Voutcome_theta<-Va
  dimnames(Voutcome_theta)<-list(joint_names,joint_names)
  joint_coef<-setNames(coef_all,joint_names)
  scope<-stacked$scope
  gamma_se<-setNames(sqrt(pmax(diag(stacked$vcov_gamma),0)),visit_covs)
  theta_vcov<-if(theta_identified)Va[-ix,-ix,drop=FALSE]else
    matrix(NA_real_,ncol(Q0),ncol(Q0),dimnames=list(colnames(Q0),colnames(Q0)))
  dimnames(theta_vcov)<-list(names(theta),names(theta))
  all_parameters<-c(setNames(og$par,paste0("gamma:",visit_covs)),sigma2=sigma2,joint_coef)
  ans<-list(method_key="liang_marker",
            method=if(estimate_intercept)"Liang-time estimator (Pullenayegum et al., 2023)" else "Liang centered/profile estimator",
            estimand="marginal longitudinal covariate effect adjusted for informative visits",
            coefficients=beta,vcov=Vb,se=setNames(sqrt(pmax(diag(Vb),0)),names(beta)),
            vcov_all=stacked$vcov_all,vcov_type=scope,
            variance_label=if(stacked$regular)"variance (stacked sandwich)" else
              "variance (stacked; conditional on selected branch)",variance_scope=scope,
            nuisance=list(gamma=setNames(og$par,visit_covs),gamma_se=gamma_se,
                          vcov_gamma=stacked$vcov_gamma,baseline_rate=bl,
                          frailty_variance=sigma2,frailty_variance_untruncated=raw_sigma2,se_sigma2=stacked$se_sigma2,
                          posterior_frailty_minus_one=B,theta=theta,vcov_theta=theta_vcov,
                          se_theta=setNames(sqrt(pmax(diag(theta_vcov),0)),names(theta)),
                          baseline_coefficients=baseline_coef,baseline_terms=baseline$terms),
            inference=list(vcov_gamma_model_based=Vgamma_model,
                           vcov_all=stacked$vcov_all,full_parameters=all_parameters,
                           vcov_outcome_theta=Voutcome_theta,
                           vcov_outcome_theta_conditional=Vconditional,coefficients_outcome_all=joint_coef,
                           influence_all=stacked$influence_all,
                           effective_outcome_score=stacked$effective_outcome_score,
                           exposure_chain=stacked$exposure_chain,frailty_chain=stacked$frailty_chain,
                           regular=stacked$regular,scope=scope,
                           conditional_active_branch_inference_available=TRUE,
                           outcome_centering_profiled=!estimate_intercept,
                           conditional_influence=if(estimate_intercept)
                             "Subject score of the uncentered expanded independence-GEE design; upstream rate/frailty outputs held fixed" else
                               "Empirical subject-case derivative of the H'(Y-D b) profile equation; upstream rate/frailty outputs held fixed"),
            converged=TRUE,
            diagnostics=list(ok=TRUE,process=process,n_subjects=n,n_visit_events=sum(inwin),
                             n_marker_events=sum(marker,na.rm=TRUE),n_process_events=length(er),n_outcomes=length(or),
                             zero_event_subjects=sum(m==0),estimate_intercept=estimate_intercept,
                             mode=if(estimate_intercept)"Liang-time" else "centered/profile",
                             baseline_formula=baseline$formula,
                             censor_source=if(is.null(censor_col))"common maximum observed time"else censor_col,
                             gamma_score_max=max(abs(gs$score)),frailty_on_boundary=sigma2==0,
                             frailty_boundary=!theta_identified,regular=stacked$regular,covariance_regular=stacked$regular,
                             conditional_inference_valid=TRUE,inference_scope=scope,
                             nuisance_estimation_propagated=TRUE,
                             raw_influence_sum_max=stacked$raw_influence_sum_max,
                             scaled_influence_sum_max=stacked$scaled_influence_sum_max,
                             theta_identified=theta_identified,centered_design_rank=centered_rank,
                             outcome_design_rank=qr(design)$rank,
                             conditional_influence_sum_max=max(abs(colSums(psi)))),
            assumptions=c("The selected process follows a proportional-rate gamma-frailty model",
                          "The conditional outcome mean is linear in posterior frailty and the specified random-effect design",
                          baseline$assumption,
                          "All-stage patient stacked covariance propagates rate, profiled Breslow exposure, frailty moment, posterior frailty and estimated centering weights",
                          "At an inactive frailty component inference conditions on the selected no-frailty branch; boundary-selection uncertainty is excluded",
                          "Any data-selected outcome baseline basis is treated as fixed for covariance estimation",
                          "Marker is the default process because recorded markers, not generic visits, determine observed Y"),call=match.call())
  baseline_prediction <- if(estimate_intercept)
    .liang_trajectory_parametric_summary(baseline$terms, baseline_coef) else
      .liang_trajectory_increment_summary(ot, y-drop(design%*%coef_all),
                                          drisk$log_sum, tuo)
  ans$prediction <- .liang_trajectory_spec(baseline_prediction, d, id, time,
                                           long_covs, beta, .liang_trajectory_window(d,time,censor_col,entry_col))
  class(ans)<-c("liang_marker");ans
}

Liang <- function(data,process=c('marker','visit'),...) {
  process<-match.arg(process);fit_method(method=if(process=='visit')'liang_visit' else 'liang_marker',data=data,...)
}

Joint_modeling_visiting_and_longitudinal_Liang <- Liang
