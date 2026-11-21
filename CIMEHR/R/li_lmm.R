.li_lmm_path_prepare <- function(d,id,time,structure,intercept) {
  columns <- structure$columns
  if(!is.list(columns) || (length(columns) && is.null(names(columns))) || any(!nzchar(names(columns))) || anyDuplicated(names(columns)))
    stop('Random columns must be a uniquely named list of modifier/power tables.',call.=FALSE)
  origin <- if(is.null(structure$time_origin))0 else structure$time_origin
  scale <- if(is.null(structure$time_scale))1 else structure$time_scale
  if(length(origin)!=1L || !is.finite(origin) || length(scale)!=1L || !is.finite(scale) || scale<=0)
    stop('Random-effect time origin/scale must be finite with positive scale.',call.=FALSE)
  if(!all(c(id,time) %in% names(d)))stop('Missing subject/time columns.',call.=FALSE)
  s <- (d[[time]]-origin)/scale
  active <- is.finite(s)
  path <- character(); coef_names <- character()
  constant <- function(v) {
    if(v %in% c('(Intercept)','Intercept'))return(rep(1,nrow(d)))
    if(!v %in% names(d) || !is.numeric(d[[v]]) || any(!is.finite(d[[v]][active])))
      stop('Random trajectory modifiers must be finite numeric columns: ',v,call.=FALSE)
    by_id <- split(d[[v]][active],as.character(d[[id]][active]))
    if(any(vapply(by_id,function(z)length(unique(z))>1L,logical(1))))
      stop('Random trajectory modifiers must be subject-level: ',v,call.=FALSE)
    d[[v]]
  }
  for(name in names(columns)) {
    tt <- columns[[name]]
    if(!is.data.frame(tt) || !all(c('modifier','power') %in% names(tt)) || !nrow(tt) ||
       anyNA(tt$modifier) || anyNA(tt$power) || any(!tt$power %in% c(0,1)))
      stop('Each random trajectory requires modifier/power rows with powers 0 or 1: ',name,call.=FALSE)
    value <- rep(0,nrow(d))
    for(k in seq_len(nrow(tt)))value <- value+constant(as.character(tt$modifier[k]))*
      if(tt$power[k]==0)1 else s
    if(name %in% c('Intercept','(Intercept)')) {
      if(!intercept || any(abs(value[active]-1)>1e-10))
        stop('Declared intercept must equal one and outcome_random_intercept must be TRUE.',call.=FALSE)
      next
    }
    if(name %in% names(d) && (any(!is.finite(d[[name]][active])) ||
                              any(abs(d[[name]][active]-value[active])>1e-8*pmax(1,abs(value[active])))))
      stop('Declared trajectory does not reproduce observed random covariate: ',name,call.=FALSE)
    nm <- paste0('.li_lmm_path_',make.names(name))
    if(nm %in% names(d))stop('Reserved random-trajectory column already exists.',call.=FALSE)
    d[[nm]] <- value;path <- c(path,nm);coef_names <- c(coef_names,name)
  }
  group <- structure$covariance_group
  trajectory <- structure$trajectory
  if(!is.null(group)) {
    if(length(group)!=1L || !is.character(group))stop('covariance_group must name one binary subject-level column.',call.=FALSE)
    g <- constant(group)
    if(!all(g[active] %in% c(0,1)))stop('covariance_group must be coded 0/1.',call.=FALSE)
    if(!intercept || length(trajectory)!=1L || !trajectory %in% coef_names)
      stop('Grouped covariance requires an intercept and a declared trajectory.',call.=FALSE)
    main <- path[match(trajectory,coef_names)]
    others <- setdiff(path,main)
    if(length(others)>1L || (length(others) &&
                             any(abs(d[[others]][active]-g[active]*d[[main]][active])>1e-8*pmax(1,abs(d[[main]][active])))))
      stop('Grouped covariance permits only the trajectory and its binary-group interaction.',call.=FALSE)
  }
  if(!intercept && !length(path))stop('The random design must contain a coefficient.',call.=FALSE)
  config <- list(covariate_paths=TRUE,path_columns=path,coefficient_names=coef_names,
                 intercept=intercept,covariance_group=group,trajectory=trajectory,
                 time_origin=origin,time_scale=scale)
  list(data=d,random_covs=unique(c(path,group)),config=config)
}

.li_lmm_path_spec <- function(dd,id,config) {
  group <- config$covariance_group
  if(!is.null(group)) {
    if(!setequal(unique(dd[[group]]),c(0,1)))stop('Grouped random covariance needs both binary groups.',call.=FALSE)
    main <- config$path_columns[match(config$trajectory,config$coefficient_names)]
    terms <- character(); blocks <- vector('list',2L)
    for(k in 1:2) {
      nm <- paste0('.li_lmm_group',k,c('_intercept','_slope'))
      dd[[nm[1L]]] <- as.numeric(dd[[group]]==k-1L)
      dd[[nm[2L]]] <- dd[[nm[1L]]]*dd[[main]]
      blocks[[k]] <- nlme::pdSymm(stats::reformulate(nm,intercept=FALSE))
      terms <- c(terms,nm)
    }
    spec <- stats::setNames(list(nlme::pdBlocked(blocks)),id)
    Z <- as.matrix(dd[,terms,drop=FALSE])
    label <- paste0('group-specific intercept/',config$trajectory,' covariance by ',group)
    formula <- paste0('group-specific (1 + ',config$trajectory,') | ',id)
  } else {
    ff <- if(!length(config$path_columns)) ~ 1 else
      stats::reformulate(config$path_columns,intercept=config$intercept)
    Z <- stats::model.matrix(ff,dd)
    spec <- stats::setNames(list(if(ncol(Z)==1L)nlme::pdDiag(ff) else nlme::pdSymm(ff)),id)
    label <- paste0(if(config$intercept)'random intercept' else 'no random intercept',
                    if(length(config$coefficient_names))paste0('; random covariate slopes: ',paste(config$coefficient_names,collapse=', ')) else '')
    formula <- paste0('~ ',if(config$intercept)'1' else '0',
                      if(length(config$coefficient_names))paste0(' + ',paste(config$coefficient_names,collapse=' + ')) else '', ' | ',id)
  }
  rank <- qr(Z)$rank
  if(rank<ncol(Z))stop('The declared random design is rank deficient.',call.=FALSE)
  list(spec=spec,data=dd,parameterization=label,formula=formula,structurally_identified=TRUE,
       random_design_rank=rank,random_design_columns=ncol(Z))
}

.li_lmm_tv_prepare <- function(d, id, time, random_covs, structure, intercept) {
  if (!is.logical(intercept) || length(intercept) != 1L || is.na(intercept))
    stop("outcome_random_intercept must be TRUE or FALSE.", call. = FALSE)
  if (is.null(structure)) {
    if (isTRUE(intercept)) return(list(data=d, random_covs=random_covs, config=NULL))
    structure <- list(static_covs=setdiff(random_covs,time),
                      time_covs=if(time %in% random_covs)"(Intercept)" else character())
  }
  if (!is.list(structure)) stop("outcome_random_structure must be a list.", call.=FALSE)
  if(!is.null(structure$columns))return(.li_lmm_path_prepare(d,id,time,structure,intercept))
  st <- unique(as.character(structure$static_covs))
  tv <- unique(as.character(structure$time_covs))
  st <- setdiff(st,"(Intercept)")
  origin <- if(is.null(structure$time_origin))0 else structure$time_origin
  scale <- if(is.null(structure$time_scale))1 else structure$time_scale
  if(length(origin)!=1L || !is.finite(origin) || length(scale)!=1L ||
     !is.finite(scale) || scale<=0)
    stop("Random-effect time origin/scale must be finite, with positive scale.",call.=FALSE)
  vars <- unique(c(st,setdiff(tv,"(Intercept)")))
  missing <- setdiff(c(id,time,vars),names(d))
  if(length(missing))stop("Missing random-design columns: ",paste(missing,collapse=", "),call.=FALSE)
  active <- is.finite(d[[time]])
  for(v in vars) {
    if(!is.numeric(d[[v]]) || any(!is.finite(d[[v]][active])))
      stop("Random modifiers must be finite numeric subject-level covariates: ",v,call.=FALSE)
    zz <- split(d[[v]][active],as.character(d[[id]][active]))
    if(any(vapply(zz,function(x)length(unique(x))>1L,logical(1))))
      stop("Random modifiers must be subject-level; declare time via time_covs: ",v,call.=FALSE)
  }
  if(!length(tv) && isTRUE(intercept))
    return(list(data=d,random_covs=st,config=NULL))
  norm_time <- ".li_lmm_re_time"
  cols <- paste0(".li_lmm_re_t",seq_along(tv))
  reserved <- c(norm_time,cols)
  if(any(reserved %in% names(d)))stop("Reserved random-design columns already exist.",call.=FALSE)
  d[[norm_time]] <- (d[[time]]-origin)/scale
  for(k in seq_along(tv))d[[cols[k]]] <- d[[norm_time]] *
    if(tv[k]=="(Intercept)")1 else d[[tv[k]]]
  config <- list(static_covs=st,time_covs=tv,time_columns=cols,
                 normalized_time=norm_time,intercept=intercept,
                 time_origin=origin,time_scale=scale)
  list(data=d,random_covs=unique(c(st,cols,norm_time)),config=config)
}

.li_lmm_tv_spec <- function(dd, id, config) {
  if(isTRUE(config$covariate_paths))return(.li_lmm_path_spec(dd,id,config))
  st <- config$static_covs; tv <- config$time_covs
  intercept <- config$intercept
  terms <- character(); blocks <- NULL
  if(length(st)==1L && intercept &&
     setequal(tv,c("(Intercept)",st))) {
    group <- st[1L]; levels <- sort(unique(dd[[group]]))
    if(length(levels)!=2L)stop("The interaction random model needs both binary groups.",call.=FALSE)
    blocks <- vector("list",2L)
    for(k in 1:2) {
      nm <- paste0(".li_lmm_group",k,c("_intercept","_time"))
      dd[[nm[1L]]] <- as.numeric(dd[[group]]==levels[k])
      dd[[nm[2L]]] <- dd[[nm[1L]]] * dd[[config$normalized_time]]
      ff <- stats::reformulate(nm,intercept=FALSE)
      blocks[[k]] <- nlme::pdSymm(ff)
      terms <- c(terms,nm)
    }
    spec <- stats::setNames(list(nlme::pdBlocked(blocks)),id)
    label <- paste0("group-specific intercept/time covariance by ",group,
                    " (groups ",paste(levels,collapse=","),"; identified quotient)")
    formula <- paste0("two independent 2x2 covariance blocks by ",group," | ",id)
  } else {
    if(length(st) && length(tv))
      stop("Supported interaction structure uses one binary static covariate and both time/time-by-group terms.",call.=FALSE)
    terms <- c(if(intercept)"(Intercept)",st,config$time_columns)
    if(!length(terms))stop("The random design must contain at least one term.",call.=FALSE)
    ff <- stats::reformulate(setdiff(terms,"(Intercept)"),intercept=intercept)
    spec <- stats::setNames(list(if(length(terms)==1L)nlme::pdDiag(ff) else nlme::pdSymm(ff)),id)
    label <- if(!intercept && !length(st) && identical(tv,"(Intercept)"))
      "random time slope without random intercept" else "declared separable random design"
    formula <- paste(paste(deparse(ff),collapse=" "),"|",id)
  }
  Z <- if(!is.null(blocks))as.matrix(dd[,terms,drop=FALSE]) else
    stats::model.matrix(ff,dd)
  rank <- qr(Z)$rank
  if(rank<ncol(Z))stop("The declared random design is rank deficient.",call.=FALSE)
  list(spec=spec,data=dd,parameterization=label,formula=formula,
       structurally_identified=TRUE,random_design_rank=rank,random_design_columns=ncol(Z))
}

.li_lmm_lmm_variance_payload <- function(fit) {
  re <- fit$modelStruct$reStruct
  if(length(re)!=1L)stop("Expected one subject random-effects grouping factor.",call.=FALSE)
  pd <- re[[1L]]; group <- names(re)[1L]
  blocks <- if(inherits(pd,"pdBlocked"))as.list(pd) else list(pd)
  kinds <- vapply(blocks,function(b)if(inherits(b,"pdDiag"))"diagonal" else
    if(inherits(b,"pdIdent"))"identity" else
      if(inherits(b,c("pdSymm","pdLogChol","pdNatural")))"unstructured" else "unsupported","")
  if(any(kinds=="unsupported"))stop("Unsupported nlme random covariance representation.",call.=FALSE)
  estimates <- numeric(); coord_counts <- integer(length(blocks))
  for(k in seq_along(blocks)) {
    M <- as.matrix(blocks[[k]])*fit$sigma^2; tt <- colnames(M); q <- nrow(M)
    coord_counts[k] <- switch(kinds[k],diagonal=q,identity=1L,unstructured=q*(q+1L)/2L)
    if(kinds[k]=="identity") {
      estimates[paste0("var_random_shared[",group,"]")] <- M[1L,1L]
    } else {
      estimates[paste0("var_random[",group,"].",tt)] <- diag(M)
      if(kinds[k]=="unstructured" && q>1L) {
        pairs <- which(upper.tri(M),arr.ind=TRUE)
        for(j in seq_len(nrow(pairs))) {
          a <- pairs[j,1L]; b <- pairs[j,2L]
          estimates[paste0("cov_random[",group,"].",tt[a],":",tt[b])] <- M[a,b]
        }
      }
    }
  }
  estimates["var_residual"] <- fit$sigma^2
  ap <- fit$apVar; pars <- if(is.matrix(ap))attr(ap,"Pars") else NULL
  valid <- is.matrix(ap) && length(ap) && all(is.finite(ap)) &&
    !is.null(pars) && isTRUE(attr(ap,"natural")) &&
    length(pars)==sum(coord_counts)+1L
  V <- matrix(NA_real_,length(estimates),length(estimates),
              dimnames=list(names(estimates),names(estimates)))
  message <- if(valid)"" else "Finite nlme natural apVar is unavailable; variance point estimates retained."
  if(valid) {
    J <- matrix(0,length(estimates),length(pars)); ri <- 0L; ci <- 0L
    for(k in seq_along(blocks)) {
      q <- nrow(as.matrix(blocks[[k]])); ix <- ci+seq_len(coord_counts[k])
      th <- as.numeric(pars[ix])
      if(kinds[k]=="identity") {
        ri <- ri+1L; J[ri,ix[1L]] <- 2*exp(2*th[1L])
      } else {
        sd <- exp(th[seq_len(q)])
        for(a in seq_len(q)) {ri<-ri+1L;J[ri,ix[a]]<-2*sd[a]^2}
        if(kinds[k]=="unstructured" && q>1L) {
          pairs <- which(upper.tri(matrix(0,q,q)),arr.ind=TRUE)
          for(j in seq_len(nrow(pairs))) {
            a <- pairs[j,1L]; b <- pairs[j,2L]; rho <- tanh(th[q+j]/2)
            value <- rho*sd[a]*sd[b];ri<-ri+1L
            J[ri,ix[a]]<-value;J[ri,ix[b]]<-value
            J[ri,ix[q+j]]<-.5*(1-rho^2)*sd[a]*sd[b]
          }
        }
      }
      ci <- ci+coord_counts[k]
    }
    J[length(estimates),length(pars)] <- 2*exp(2*tail(pars,1L))
    V <- J %*% ap %*% t(J);dimnames(V)<-list(names(estimates),names(estimates))
  }
  table <- data.frame(parameter=names(estimates),estimate=unname(estimates),
                      se=sqrt(pmax(diag(V),0)),scale=ifelse(grepl("^cov_",names(estimates)),"covariance","variance"),
                      row.names=NULL,stringsAsFactors=FALSE)
  list(ok=isTRUE(valid),message=message,estimate=estimates,table=table,vcov=V,
       transformed_parameters=if(is.null(pars))numeric() else pars,
       apVar=if(is.matrix(ap))ap else NULL,
       parameterization=if(length(blocks)>1L)"group-specific blocked covariance" else kinds,
       group=group)
}

.li_lmm_bt <- function(x) paste0("`", gsub("`", "", x, fixed = TRUE), "`")
.li_lmm_check <- function(data, vars) {
  d <- as.data.frame(data); bad <- setdiff(unique(vars), names(d))
  if (length(bad)) stop("Missing columns: ", paste(bad, collapse = ", "), call. = FALSE)
  if (anyNA(d[[vars[1L]]])) stop("Subject id cannot be missing.", call. = FALSE)
  d
}

.li_lmm_inv <- function(A, tol = 1e-10) {
  z <- svd(as.matrix(A)); keep <- z$d > tol * max(z$d, 1)
  if (sum(keep) < ncol(A)) stop("Linear-increment design is rank deficient.", call. = FALSE)
  z$v[, keep, drop = FALSE] %*% (t(z$u[, keep, drop = FALSE]) / z$d[keep])
}

.li_lmm_li_loop <- function(d, id, time, outcome, obs_indicator, li_covs,
                            covariance) {

  d$.li_lmm_row <- seq_len(nrow(d))
  o <- order(d[[id]], d[[time]], na.last = TRUE)
  z <- d[o, , drop = FALSE]; rownames(z) <- NULL
  actual <- is.finite(z[[time]])
  observed <- actual & !is.na(z[[obs_indicator]]) &
    z[[obs_indicator]] == 1 & is.finite(z[[outcome]])
  z[[outcome]][actual & !observed] <- NA_real_
  ids <- unique(z[[id]]); si <- match(z[[id]], ids); p <- 2L + length(li_covs)
  nm <- c("(Intercept)", li_covs, paste0("lag_", outcome))
  pair_W <- list(); pair_rate <- numeric(); pair_gap <- numeric(); pair_id <- integer()
  kk <- 0L
  for (ii in seq_along(ids)) {
    ix <- which(si == ii & actual)
    if (length(ix) < 2L) next
    for (j in 2:length(ix)) {
      a <- ix[j - 1L]; b <- ix[j]; gap <- z[[time]][b] - z[[time]][a]
      if (!observed[a] || !observed[b] || !is.finite(gap) || gap <= 0 ||
          !all(is.finite(as.numeric(z[a, li_covs, drop = FALSE])))) next
      kk <- kk + 1L
      pair_W[[kk]] <- c(1, as.numeric(z[a, li_covs, drop = FALSE]), z[[outcome]][a])
      pair_rate[kk] <- (z[[outcome]][b] - z[[outcome]][a]) / gap
      pair_gap[kk] <- gap; pair_id[kk] <- ii
    }
  }
  if (kk < max(10L, p + 2L))
    stop("Too few genuinely observed adjacent increments for recursive LI.", call. = FALSE)
  W <- do.call(rbind, pair_W); colnames(W) <- nm
  ww <- if (identical(covariance, "randomwalk")) pair_gap else rep(1, kk)
  A <- crossprod(W, W * ww); Ai <- .li_lmm_inv(A)
  eta <- as.vector(Ai %*% crossprod(W, ww * pair_rate)); names(eta) <- nm
  er <- pair_rate - as.vector(W %*% eta)
  us <- matrix(0, length(ids), p)
  tmp <- rowsum(W * (ww * er), pair_id, reorder = FALSE)
  us[as.integer(rownames(tmp)), ] <- tmp
  Veta <- Ai %*% crossprod(us) %*% t(Ai); dimnames(Veta) <- list(nm, nm)

  grad <- matrix(NA_real_, nrow(z), p, dimnames = list(NULL, nm))
  reconstructed <- rep(FALSE, nrow(z)); unanchored <- rep(FALSE, nrow(z))
  eta_y <- eta[p]
  for (ii in seq_along(ids)) {
    ix <- which(si == ii & actual); anchored <- FALSE; yprev <- NA_real_
    gprev <- rep(NA_real_, p); prev <- NA_integer_
    for (r in ix) {
      if (observed[r]) {
        yprev <- z[[outcome]][r]; gprev <- rep(0, p); grad[r, ] <- gprev
        anchored <- TRUE; prev <- r; next
      }
      if (!anchored || is.na(prev)) { unanchored[r] <- TRUE; prev <- r; next }
      gap <- z[[time]][r] - z[[time]][prev]
      xv <- as.numeric(z[prev, li_covs, drop = FALSE])
      if (!is.finite(gap) || gap <= 0 || !all(is.finite(xv))) {
        unanchored[r] <- TRUE; prev <- r; anchored <- FALSE; next
      }
      wv <- c(1, xv, yprev)
      ynew <- yprev + gap * sum(wv * eta)
      gnew <- (1 + gap * eta_y) * gprev + gap * wv
      z[[outcome]][r] <- ynew; grad[r, ] <- gnew; reconstructed[r] <- TRUE
      yprev <- ynew; gprev <- gnew; prev <- r
    }
  }
  back <- order(z$.li_lmm_row); out <- z[back, , drop = FALSE]
  gout <- grad[back, , drop = FALSE]; recout <- reconstructed[back]
  unout <- unanchored[back]; out$.li_lmm_row <- NULL; rownames(out) <- NULL
  list(data = out, n_imputed = sum(recout), n_unreconstructed = sum(unout),
       reconstructed = recout, gradient = gout, eta = eta, vcov_eta = Veta,
       eta_scores = us, eta_bread = A, ids = ids,
       n_observed_increments = kk, increment_design_rank = qr(W)$rank,
       increment_design_columns = ncol(W), increment_condition = kappa(A),
       formula = paste0("d", outcome, "/dt ~ ",
                        paste(c(li_covs, paste0("lag_", outcome)), collapse = " + ")),
       weighting = if (identical(covariance, "randomwalk"))
         "WLS weight dt (Var increment proportional to dt)" else "equal-weight increment rates")
}

.li_lmm_li <- function(d, id, time, outcome, obs_indicator, li_covs, covariance,
                       full = TRUE) {
  numeric_columns <- unique(c(time, outcome, li_covs))
  ordinary_numeric <- all(vapply(d[, numeric_columns, drop = FALSE], function(x)
    is.numeric(x) && !is.object(x) && is.null(dim(x)), logical(1)))
  if (outcome %in% li_covs || !ordinary_numeric)
    return(.li_lmm_li_loop(d, id, time, outcome, obs_indicator, li_covs, covariance))

  s <- .li_sorted_increments(d, id, time, outcome, obs_indicator, li_covs, ".li_lmm_row")
  ids <- s$ids; p <- 2L + length(li_covs)
  nm <- c("(Intercept)", li_covs, paste0("lag_", outcome))
  a <- s$a; b <- s$b; gapv <- s$gap; k <- length(a)
  if (k < max(10L, p + 2L)) stop("Too few genuinely observed adjacent increments for recursive LI.", call. = FALSE)
  W <- cbind(1, s$x[a, , drop = FALSE], s$y[a]); colnames(W) <- nm

  rate <- (s$y[b] - s$y[a]) / gapv
  ww <- if (identical(covariance, "randomwalk")) gapv else rep(1, k)
  A <- crossprod(W, W * ww); Ai <- .li_lmm_inv(A)
  eta <- as.vector(Ai %*% crossprod(W, ww * rate)); names(eta) <- nm
  fw <- .li_forward_recursion(s, d, time, eta)
  out <- s$data; out[[outcome]] <- fw$y
  li <- list(data = out, n_imputed = sum(fw$rec), n_unreconstructed = sum(s$unobserved & !fw$rec),
             reconstructed = fw$rec, eta = eta)
  if (!full) return(li)
  er <- rate - as.vector(W %*% eta); us <- matrix(0, length(ids), p)
  tmp <- rowsum(W * (ww * er), s$si[a], reorder = FALSE)
  us[as.integer(rownames(tmp)), ] <- tmp
  Ve <- Ai %*% crossprod(us) %*% t(Ai); dimnames(Ve) <- list(nm, nm)
  c(li, list(vcov_eta = Ve, eta_scores = us, eta_bread = A, ids = ids,
             n_observed_increments = k, increment_design_rank = qr(W)$rank,
             increment_design_columns = ncol(W), increment_condition = kappa(A),
             formula = paste0("d", outcome, "/dt ~ ",
                              paste(c(li_covs, paste0("lag_", outcome)), collapse = " + ")),
             weighting = if (identical(covariance, "randomwalk"))
               "WLS weight dt (Var increment proportional to dt)" else "equal-weight increment rates"))
}

.li_lmm_random_spec <- function(dd, id, random_covs) {
  quote_name <- function(x) paste0("`", gsub("`", "", x, fixed = TRUE), "`")
  out <- list(structurally_identified = TRUE, random_design_rank = 1L,
              random_design_columns = 1L)
  if (!length(random_covs)) {
    out$spec <- stats::as.formula(paste("~ 1 |", quote_name(id)))
    out$parameterization <- "random intercept"; out$formula <- paste0("~ 1 | ", id)
    return(out)
  }
  subject <- dd[[id]]
  key <- if (is.integer(subject) && !is.object(subject)) subject else as.character(subject)
  first <- match(key, key)
  within_varies <- vapply(random_covs, function(v) {
    x <- dd[[v]]
    if (.li_plain_numeric(x) && all(is.finite(x))) return(any(x != x[first]))
    zz <- split(dd[[v]], as.character(dd[[id]]), drop = TRUE)
    any(vapply(zz, function(x) length(unique(x)) > 1L, logical(1)))
  }, logical(1))
  if (length(random_covs) > 1L && any(!within_varies))
    stop("Mixed subject-constant and time-varying random slopes require an explicit identifiable quotient.",
         call. = FALSE)
  if (length(random_covs) == 1L && !within_varies[1L]) {
    slope <- random_covs[1L]; values <- unique(dd[[slope]]); values <- values[!is.na(values)]
    if (length(values) == 1L) {
      out$spec <- stats::as.formula(paste("~ 1 |", quote_name(id)))
      out$parameterization <- paste0("random-intercept quotient; `", slope,
                                     "` has one observed level")
      out$formula <- paste0("~ 1 | ", id, " [constant ", slope, "]")
    } else if (length(values) == 2L) {
      labels <- sort(unique(as.character(values)))
      dd$.li_lmm_re_group_1 <- as.numeric(as.character(dd[[slope]]) == labels[1L])
      dd$.li_lmm_re_group_2 <- as.numeric(as.character(dd[[slope]]) == labels[2L])
      out$spec <- stats::setNames(
        list(nlme::pdDiag(~ 0 + .li_lmm_re_group_1 + .li_lmm_re_group_2)), id)
      out$parameterization <- paste0(
        "two group-specific random-intercept variances: identified quotient of ~1+",
        slope, "|", id)
      out$formula <- paste0("group-specific random intercept by ", slope, " | ", id)
      out$random_design_rank <- 2L; out$random_design_columns <- 2L
    } else if (is.numeric(dd[[slope]])) {
      z <- vapply(split(dd[[slope]], as.character(dd[[id]]), drop = TRUE),
                  function(x) x[1L], numeric(1))
      out$structurally_identified <- qr(cbind(1, 2 * z, z^2))$rank == 3L
      if (!out$structurally_identified)
        stop("Subject-constant random slope covariance is not identified.", call. = FALSE)
      out$spec <- stats::as.formula(paste("~ 1 +", quote_name(slope), "|", quote_name(id)))
      out$parameterization <- paste0(
        "unstructured random intercept and identifiable subject-constant slope `", slope, "`")
      out$formula <- paste0("~ 1 + ", slope, " | ", id)
      out$random_design_rank <- qr(cbind(1, dd[[slope]]))$rank
      out$random_design_columns <- 2L
    } else {
      stop("A subject-constant nonnumeric random slope with more than two levels is unsupported.",
           call. = FALSE)
    }
  } else {
    rhs <- paste(vapply(random_covs, quote_name, character(1)), collapse = " + ")
    out$spec <- stats::as.formula(paste("~ 1 +", rhs, "|", quote_name(id)))
    out$parameterization <- paste0(
      "unstructured random intercept and genuine within-subject slope(s): ",
      paste(random_covs, collapse = ", "))
    out$formula <- paste0("~ 1 + ", paste(random_covs, collapse = " + "), " | ", id)
    Z <- stats::model.matrix(stats::reformulate(random_covs), data = dd)
    out$random_design_rank <- qr(Z)$rank; out$random_design_columns <- ncol(Z)
    out$structurally_identified <- out$random_design_rank == out$random_design_columns
  }
  out$data <- dd
  out
}

# full = FALSE returns fixed effects.
.li_lmm_lmm <- function(d, id, time, outcome, long_covs, random_covs,
                        method, optimizer, random_config=NULL, full = TRUE) {
  if (!requireNamespace("nlme", quietly = TRUE)) stop("LI-LMM requires 'nlme'.", call. = FALSE)
  actual <- is.finite(d[[time]])
  .li_require_completed(d, id, time, outcome, "LI+LMM")
  .li_require_design(d, id, unique(c(long_covs, random_covs)), "LI+LMM", rows = actual)
  dd <- d[actual, , drop = FALSE]
  fixed <- stats::reformulate(unique(c(long_covs, time)), response = outcome)
  Xm <- stats::model.matrix(fixed, data = dd)
  fixed_rank <- qr(Xm)$rank
  if (fixed_rank < ncol(Xm)) stop("LMM fixed-effect design is rank deficient.", call. = FALSE)
  rs <- if(is.null(random_config)).li_lmm_random_spec(dd,id,random_covs) else
    .li_lmm_tv_spec(dd,id,random_config)
  if (!is.null(rs$data)) dd <- rs$data
  ctrl <- nlme::lmeControl(opt = optimizer, msMaxIter = 5000,
                           msMaxEval = 10000, returnObject = FALSE)
  fit <- tryCatch(
    nlme::lme(fixed, random = rs$spec, data = dd, method = method,
              control = ctrl, na.action = stats::na.fail),
    error = function(e) {
      if (!identical(optimizer, "nlminb") ||
          !grepl("false convergence (8)", conditionMessage(e), fixed = TRUE))
        stop(e)
      retry_ctrl <- ctrl
      retry_ctrl$opt <- "optim"
      retry <- tryCatch(
        withCallingHandlers(
          nlme::lme(fixed, random = rs$spec, data = dd, method = method,
                    control = retry_ctrl, na.action = stats::na.fail),
          warning = function(w) stop(conditionMessage(w), call. = FALSE)),
        error = function(e2) stop(
          conditionMessage(e), "; optim retry failed: ", conditionMessage(e2),
          call. = FALSE))
      if (!is.finite(as.numeric(stats::logLik(retry))) ||
          any(!is.finite(nlme::fixef(retry))) ||
          any(!is.finite(stats::vcov(retry))))
        stop("The optim retry returned non-finite LMM estimates.", call. = FALSE)
      attr(retry, "optimizer_retry") <- list(
        initial = "nlminb", final = "optim", reason = conditionMessage(e))
      retry
    })
  b <- nlme::fixef(fit)
  if (!full) return(list(coef = b))
  V <- as.matrix(stats::vcov(fit))
  ap_ok <- is.matrix(fit$apVar) && all(is.finite(fit$apVar))
  ap_msg <- if (ap_ok) "" else if (is.character(fit$apVar)) {
    paste(fit$apVar, collapse = " ")
  } else "Non-finite apVar."
  vp <- .li_lmm_lmm_variance_payload(fit)
  list(coef = b, vcov = V, fit = fit, n = nrow(dd),
       converged = all(is.finite(b)) && all(is.finite(V)),
       structurally_identified = rs$structurally_identified,
       identified = isTRUE(rs$structurally_identified && fixed_rank==ncol(Xm)),
       covariance_regular = isTRUE(ap_ok),
       apVar_ok = ap_ok, apVar_message = ap_msg,
       fixed_rank = fixed_rank, fixed_columns = ncol(Xm),
       random_design_rank = rs$random_design_rank,
       random_design_columns = rs$random_design_columns,
       random_parameterization = rs$parameterization,
       random_formula = rs$formula,
       varcorr = tryCatch(nlme::VarCorr(fit), error = function(e) NULL),
       variance_payload = vp)
}

.li_lmm_with_seed <- function(seed, expr) {
  if (length(seed) != 1L || !is.finite(seed)) stop("Invalid bootstrap seed.", call. = FALSE)
  had <- exists(".Random.seed", .GlobalEnv, inherits = FALSE)
  if (had) old <- get(".Random.seed", .GlobalEnv, inherits = FALSE)
  on.exit(if (had) assign(".Random.seed", old, .GlobalEnv) else if
          (exists(".Random.seed", .GlobalEnv, inherits = FALSE))
    rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
  set.seed(as.integer(seed)); force(expr)
}

.fit_li_lmm <- function(
    data, id = "id", time = "time", outcome = "Y", obs_indicator = "R",
    visit_covs = c("Z", "X"), long_covs = c("Z", "X"), random_covs = "Z",
    li_covariance = c("randomwalk", "rate_independence"),
    lmm_method = c("REML", "ML"), optimizer = c("nlminb", "optim"),
    inference = c("conditional", "patient_bootstrap"), bootstrap_reps = 200L,
    bootstrap_seed = 1701L, keep_completed = FALSE,
    outcome_random_structure=NULL, outcome_random_intercept=TRUE,
    leading_imputation = c("reverse_li", "error"),
    unobserved_subjects = c("exclude", "error")) {
  li_covariance <- match.arg(li_covariance); lmm_method <- match.arg(lmm_method)
  optimizer <- match.arg(optimizer); inference <- match.arg(inference)
  leading_imputation <- match.arg(leading_imputation)
  unobserved_subjects <- match.arg(unobserved_subjects)
  d <- .li_lmm_check(data, c(id, time, outcome, obs_indicator, visit_covs, long_covs))
  selection <- .li_select_subjects(d, id, time, outcome, obs_indicator,
                                   unobserved_subjects, "LI+LMM")
  d <- selection$data
  fit_pipeline <- function(db, full = TRUE) {
    .li_require_design(db, id, unique(c(visit_covs, long_covs)), "LI+LMM",
                       rows = is.finite(db[[time]]))
    prepared <- .li_lmm_tv_prepare(db, id, time, random_covs,
                                   outcome_random_structure, outcome_random_intercept)
    model <- .li_lmm_check(prepared$data,
                           c(id, time, outcome, obs_indicator, visit_covs, long_covs, prepared$random_covs))
    .li_require_design(model, id, prepared$random_covs, "LI+LMM",
                       rows = is.finite(model[[time]]))
    modifiers <- if (is.null(outcome_random_structure)) character() else
      if (!is.null(outcome_random_structure$columns))
        unlist(lapply(outcome_random_structure$columns, function(x)
          as.character(x$modifier)), use.names = FALSE) else
            c(outcome_random_structure$static_covs, outcome_random_structure$time_covs)
    reverse_covs <- setdiff(unique(c(visit_covs, long_covs,
                                     intersect(random_covs, names(db)), modifiers,
                                     outcome_random_structure$covariance_group)), c("(Intercept)", "Intercept"))
    li <- .li_lmm_li(model, id, time, outcome, obs_indicator, visit_covs, li_covariance, full)
    explicit_covs <- unique(c(visit_covs, long_covs, intersect(random_covs, names(db)),
                              outcome_random_structure$covariance_group))
    automatic_covs <- setdiff(reverse_covs, explicit_covs)
    if (leading_imputation == "reverse_li" && li$n_unreconstructed > 0L &&
        length(automatic_covs)) {
      actual <- is.finite(db[[time]])
      .li_require_design(db, id, reverse_covs, "LI+LMM", rows = actual)
      retained <- setdiff(reverse_covs, automatic_covs)
      for (v in automatic_covs) {
        x <- db[[v]][actual]
        constant <- length(x) > 0L && all(x == x[1L])
        duplicate <- any(vapply(retained, function(k)
          all(x == db[[k]][actual]), logical(1)))
        if (!constant && !duplicate) retained <- c(retained, v)
      }
      reverse_covs <- reverse_covs[reverse_covs %in% retained]
    }
    li <- .li_reverse_reconstruct(li, model, id, time, outcome, obs_indicator,
                                  reverse_covs, li_covariance, leading_imputation, "LI+LMM", full)
    one <- .li_lmm_lmm(li$data, id, time, outcome, long_covs, prepared$random_covs,
                       lmm_method, optimizer, prepared$config, full)
    list(li = li, one = one)
  }
  fitted <- fit_pipeline(d)
  li <- fitted$li; one <- fitted$one
  Vout <- one$vcov; boot_success <- 0L; boot_failure <- character()
  if (inference == "patient_bootstrap") {
    if (length(bootstrap_reps) != 1L || !is.finite(bootstrap_reps) ||
        bootstrap_reps < 2L || bootstrap_reps != as.integer(bootstrap_reps))
      stop("`bootstrap_reps` must be an integer >= 2.", call. = FALSE)
    resample <- .li_resampler(d, id)
    bb <- .li_lmm_with_seed(bootstrap_seed, lapply(seq_len(bootstrap_reps), function(b) {
      tryCatch({
        db <- resample()
        fit_pipeline(db, FALSE)$one$coef
      }, error = function(e) structure(list(message = conditionMessage(e)),
                                       class = "li_lmm_failure"))
    }))
    nm <- names(one$coef)
    good <- vapply(bb, function(x) is.numeric(x) && length(x) == length(nm) &&
                     !anyDuplicated(names(x)) && setequal(names(x), nm) &&
                     all(is.finite(x[nm])), logical(1)); boot_success <- sum(good)
    boot_failure <- vapply(bb[!good], function(x) if (inherits(x, "li_lmm_failure"))
      x$message else "coefficient schema mismatch or non-finite coefficient", "")
    if (boot_success < ceiling(0.95 * bootstrap_reps))
      stop("Fewer than 95% successful patient bootstrap fits: ", boot_success, "/", bootstrap_reps,
           call. = FALSE)
    Q <- do.call(rbind, lapply(bb[good], `[`, nm))
    Vout <- stats::cov(Q); dimnames(Vout) <- list(nm, nm)
  }
  conv <- isTRUE(one$converged)
  ans <- list(method_key = "li_lmm", method = "LI+LMM",
              estimand = paste0("Fixed-effect coefficient in the recursively LI-reconstructed Gaussian LMM",
                                if (selection$n_subjects_excluded > 0L)
                                  " in the retained cohort after excluding subjects without any observed outcome" else ""),
              coefficients = one$coef, vcov = Vout,
              se = sqrt(pmax(diag(Vout), 0)),
              vcov_type = if (inference == "patient_bootstrap")
                "patient-cluster bootstrap of the full LI+LMM procedure" else
                  "variance (plug-in): nlme model-based, conditional on LI reconstruction",
              nuisance = list(li_formula = li$formula, li_covariance = li_covariance,
                              increment_coefficients = li$eta,
                              increment_vcov = li$vcov_eta,
                              backward_increment_coefficients = li$backward$coefficients,
                              backward_increment_vcov = li$backward$vcov,
                              conditional_vcov = one$vcov,
                              lmm_variance_parameters =
                                one$variance_payload$table,
                              lmm_variance_vcov_delta=one$variance_payload$vcov,
                              lmm_apVar_parameters =
                                one$variance_payload$transformed_parameters),
              inference = list(
                lmm_variance_vcov_delta = one$variance_payload$vcov,
                lmm_apVar_internal = one$variance_payload$apVar),
              converged = conv,
              diagnostics = list(ok = isTRUE(conv && one$identified &&
                                               all(is.finite(one$coef)) && all(is.finite(one$vcov))),
                                 identified = one$identified,
                                 regular = isTRUE(one$covariance_regular),
                                 full_stage_regular = isTRUE(one$covariance_regular),
                                 outcome_inference_available = .lmm_outcome_inference_available(
                                   one$vcov, one$coef, converged = conv,
                                   identified = isTRUE(one$identified)) &&
                                   .lmm_outcome_inference_available(Vout, one$coef,
                                                                    converged = conv, identified = isTRUE(one$identified)),
                                 outcome_inference_scope = if (inference == "patient_bootstrap")
                                   "patient-cluster bootstrap of the full LI+LMM procedure; covariance-parameter apVar checked separately" else
                                     "model-based fixed-effect covariance conditional on LI reconstruction; covariance-parameter apVar checked separately",
                                 structurally_identified = one$structurally_identified,
                                 apVar_ok = one$apVar_ok,
                                 apVar_message = one$apVar_message,
                                 unobserved_subjects = unobserved_subjects,
                                 n_subjects_input = selection$n_subjects_input,
                                 n_visits_input = selection$n_visits_input,
                                 n_subjects_excluded_unobserved = selection$n_subjects_excluded,
                                 n_visits_excluded_unobserved = selection$n_visits_excluded,
                                 analysis_population = if (selection$n_subjects_excluded > 0L)
                                   "retained cohort after excluding subjects without any observed outcome" else
                                     "input cohort; no subjects excluded",
                                 bootstrap_population = "retained eligible subjects; conditional on cohort eligibility",
                                 n_subjects = length(unique(d[[id]])),
                                 n_visits = sum(is.finite(d[[time]])),
                                 zero_visit_subjects = length(setdiff(unique(d[[id]]),
                                                                      unique(d[[id]][is.finite(d[[time]])]))),
                                 n_observed_before_imputation = sum(is.finite(d[[time]]) &
                                                                      !is.na(d[[obs_indicator]]) & d[[obs_indicator]] == 1 &
                                                                      is.finite(d[[outcome]])),
                                 n_imputed = li$n_imputed, n_analysis = one$n,
                                 n_imputed_forward = li$n_imputed_forward,
                                 n_imputed_backward = li$n_imputed_backward,
                                 backward_model_used = !is.null(li$backward),
                                 leading_imputation = leading_imputation,
                                 backward_model = li$backward[c("formula", "predictors", "weighting",
                                                                "n_observed_increments", "rank", "columns", "condition")],
                                 n_unreconstructed = li$n_unreconstructed,
                                 all_visit_outcome_coverage = one$n == sum(is.finite(d[[time]])),
                                 unreconstructed_policy = "error; preserve all actual visits of retained subjects",
                                 n_observed_increments = li$n_observed_increments,
                                 increment_design_rank = li$increment_design_rank,
                                 increment_design_columns = li$increment_design_columns,
                                 increment_condition_number = li$increment_condition,
                                 increment_weighting = li$weighting,
                                 original_R_preserved = identical(li$data[[obs_indicator]],
                                                                  d[[obs_indicator]]),
                                 downstream_process = "completed outcomes at actual visits; R is provenance only",
                                 li_uncertainty_included = inference == "patient_bootstrap",
                                 inference = inference,
                                 bootstrap_requested = if (inference == "patient_bootstrap")
                                   as.integer(bootstrap_reps) else 0L,
                                 bootstrap_success = boot_success,
                                 bootstrap_failures = boot_failure,
                                 fixed_rank = one$fixed_rank,
                                 fixed_columns = one$fixed_columns,
                                 random_formula = one$random_formula,
                                 random_parameterization = one$random_parameterization,
                                 random_design_rank = one$random_design_rank,
                                 random_design_columns = one$random_design_columns,
                                 varcorr = one$varcorr,
                                 lmm_variance_vcov_ok =
                                   one$variance_payload$ok,
                                 lmm_variance_vcov_message =
                                   one$variance_payload$message,
                                 lmm_variance_parameterization =
                                   one$variance_payload$parameterization,
                                 lmm_variance_group =
                                   one$variance_payload$group),
              assumptions = c("Pooled linear increment-rate model and discrete-time independent censoring (DTIC)",
                              "Observed adjacent R=1 pairs train increments; forward recursion re-anchors at returned observations",
                              "Re-anchoring after nonmonotone missingness requires a compatible return-to-observation mechanism",
                              "Every actual visit has a finite observed or LI-reconstructed outcome; at least one observed anchor is needed per retained subject with visits",
                              if (selection$n_subjects_excluded > 0L)
                                "Inference concerns the retained cohort; visit/recording-based exclusion can change the outcome model and does not establish unbiasedness for the original cohort",
                              if (!is.null(li$backward)) c(
                                "Leading outcomes use a separately fitted reverse-time increment-rate model, not inversion of forward coefficients",
                                "Reverse conditional means given later outcomes and covariates follow the pooled linear model; observed adjacent pairs represent leading missing runs",
                                "Forward DTIC alone does not imply the reverse conditional-mean assumptions"),
                              "The reconstructed outcomes and selected random structure must be compatible with the Gaussian LMM; completed outcomes do not establish that model",
                              if (inference == "conditional")
                                "Conditional covariance omits estimated-reconstruction uncertainty; use inference='patient_bootstrap' for full-procedure inference" else
                                  "Subjects are the independent resampling units"),
              call = match.call())
  if (keep_completed) ans$completed_data <- li$data
  class(ans) <- c("li_lmm"); ans
}

Linear_increment_IP <- function(data,ip_method=c('LMM','Liang','IIRR'),...) {
  ip_method<-match.arg(ip_method);fit_method(method=c(LMM='li_lmm',Liang='li_liang',IIRR='li_iirr')[ip_method],data=data,...)
}
