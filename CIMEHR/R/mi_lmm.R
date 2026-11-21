.mi_lmm_impute_paths <- function(data,id,time,outcome,obs_indicator,
                                 fixed_vars,m,maxit,seed,config) {
  impute_one <- function(dd,fv,rv,seed_value) {
    fv <- setdiff(fv,config$coefficient_names)
    if(!config$intercept) {
      dd$.mi_lmm_fixed_intercept <- 1
      fv <- unique(c(fv,'.mi_lmm_fixed_intercept'))
    }
    ans <- .mi_lmm_impute(dd,id,time,outcome,obs_indicator,fv,rv,m,maxit,seed_value,
                          pan_intercept=config$intercept,include_time_fixed=TRUE,initialize_predictor_matrix=FALSE)
    if(!config$intercept)for(k in seq_along(ans))ans[[k]]$.mi_lmm_fixed_intercept <- NULL
    ans
  }
  group <- config$covariance_group
  if(is.null(group))return(impute_one(data,fixed_vars,config$path_columns,seed))
  if(!setequal(unique(data[[group]]),c(0,1)))stop('Grouped imputation needs both binary groups.',call.=FALSE)
  if('.mi_lmm_group_row' %in% names(data))stop('Reserved imputation row column already exists.',call.=FALSE)
  data$.mi_lmm_group_row <- seq_len(nrow(data))
  main <- config$path_columns[match(config$trajectory,config$coefficient_names)]
  parts <- lapply(0:1,function(g) {
    dd <- data[data[[group]]==g,,drop=FALSE]
    fv <- setdiff(fixed_vars,group)
    fv <- fv[vapply(dd[fv],function(x)length(unique(x[is.finite(x)]))>1L,logical(1))]
    impute_one(dd,fv,main,as.integer((as.double(seed)+1009*g)%%2147483646))
  })
  ans <- lapply(seq_len(m),function(k) {
    dd <- do.call(rbind,lapply(parts,`[[`,k));dd <- dd[order(dd$.mi_lmm_group_row),,drop=FALSE]
    dd$.mi_lmm_group_row <- NULL;rownames(dd) <- NULL;dd
  })
  attr(ans,'imputation_random_used') <- main
  attr(ans,'imputation_random_dropped') <- character()
  attr(ans,'imputation_group_covariate') <- group
  attr(ans,'imputation_fixed_used') <- lapply(parts,attr,'imputation_fixed_used')
  attr(ans,'imputation_logged_events') <- lapply(parts,attr,'imputation_logged_events')
  ans
}

.mi_lmm_impute_structured <- function(data,id,time,outcome,obs_indicator,
                                      fixed_vars,random_vars,m,maxit,seed,config=NULL) {
  if(is.null(config))return(.mi_lmm_impute(data,id,time,outcome,obs_indicator,
                                           fixed_vars,random_vars,m,maxit,seed))
  if(isTRUE(config$covariate_paths))return(.mi_lmm_impute_paths(data,id,time,outcome,
                                                                obs_indicator,fixed_vars,m,maxit,seed,config))
  st <- config$static_covs; tv <- config$time_covs
  mixed <- length(st)==1L && config$intercept && setequal(tv,c("(Intercept)",st))
  if(mixed) {
    group <- st[1L]; levels <- sort(unique(data[[group]]))
    if(length(levels)!=2L)stop("Group-specific imputation needs both binary groups.",call.=FALSE)
    data$.mi_lmm_group_row <- seq_len(nrow(data))
    parts <- lapply(seq_along(levels),function(g) {
      sub <- data[data[[group]]==levels[g],,drop=FALSE]
      fv <- setdiff(fixed_vars,c(group,time))
      fv <- fv[vapply(sub[fv],function(x)length(unique(x[is.finite(x)]))>1L,logical(1))]
      .mi_lmm_impute(sub,id,time,outcome,obs_indicator,fv,
                     config$normalized_time,m,maxit,
                     as.integer((as.double(seed)+1009*(g-1L))%%2147483646),
                     pan_intercept=TRUE,include_time_fixed=FALSE)
    })
    ans <- lapply(seq_len(m),function(k) {
      z <- do.call(rbind,lapply(parts,`[[`,k))
      z <- z[order(z$.mi_lmm_group_row),,drop=FALSE]
      z$.mi_lmm_group_row <- NULL;rownames(z)<-NULL;z
    })
    attr(ans,"imputation_random_used")<-config$normalized_time
    attr(ans,"imputation_random_dropped")<-character()
    attr(ans,"imputation_group_covariate")<-group
    return(ans)
  }
  fv <- setdiff(fixed_vars,time)
  if(!config$intercept) {
    data$.mi_lmm_fixed_intercept <- 1
    fv <- unique(c(fv,".mi_lmm_fixed_intercept"))
  }
  ans <- .mi_lmm_impute(data,id,time,outcome,obs_indicator,fv,
                        config$time_columns,m,maxit,seed,
                        pan_intercept=config$intercept,
                        include_time_fixed=!"(Intercept)" %in% tv)
  if(!config$intercept)for(k in seq_along(ans))ans[[k]]$.mi_lmm_fixed_intercept<-NULL
  ans
}

.mi_lmm_pool_variance <- function(fits) {
  payload <- lapply(fits,`[[`,"variance_payload")
  nm <- names(payload[[1L]]$estimate)
  if(any(!vapply(payload,function(x)identical(names(x$estimate),nm),logical(1))))
    stop("Random covariance parameter names differ across imputations.",call.=FALSE)
  Q <- do.call(rbind,lapply(payload,`[[`,"estimate"))
  estimate <- colMeans(Q)
  W <- Reduce(`+`,lapply(payload,`[[`,"vcov"))/length(payload)
  B <- if(length(payload)>1L)stats::cov(Q) else matrix(0,length(nm),length(nm))
  V <- W+(1+1/length(payload))*B;dimnames(V)<-list(nm,nm)
  table <- data.frame(parameter=nm,estimate=unname(estimate),
                      se=sqrt(pmax(diag(V),0)),scale=ifelse(grepl("^cov_",nm),"covariance","variance"),
                      row.names=NULL,stringsAsFactors=FALSE)
  list(table=table,vcov=V)
}

.mi_lmm_path_prepare <- function(d,id,time,structure,intercept) {
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
    if(!v %in% names(d) || !is.numeric(d[[v]]) || any(!is.finite(d[[v]])))
      stop('Random trajectory modifiers must be finite numeric columns: ',v,call.=FALSE)
    by_id <- split(d[[v]],as.character(d[[id]]))
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
    nm <- paste0('.mi_lmm_path_',make.names(name))
    if(nm %in% names(d))stop('Reserved random-trajectory column already exists.',call.=FALSE)
    d[[nm]] <- value;path <- c(path,nm);coef_names <- c(coef_names,name)
  }
  group <- structure$covariance_group
  trajectory <- structure$trajectory
  if(!is.null(group)) {
    if(length(group)!=1L || !is.character(group))stop('covariance_group must name one binary subject-level column.',call.=FALSE)
    g <- constant(group)
    if(!all(g %in% c(0,1)))stop('covariance_group must be coded 0/1.',call.=FALSE)
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

.mi_lmm_path_spec <- function(dd,id,config) {
  group <- config$covariance_group
  if(!is.null(group)) {
    if(!setequal(unique(dd[[group]]),c(0,1)))stop('Grouped random covariance needs both binary groups.',call.=FALSE)
    main <- config$path_columns[match(config$trajectory,config$coefficient_names)]
    terms <- character(); blocks <- vector('list',2L)
    for(k in 1:2) {
      nm <- paste0('.mi_lmm_group',k,c('_intercept','_slope'))
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

.mi_lmm_tv_prepare <- function(d, id, time, random_covs, structure, intercept) {
  if (!is.logical(intercept) || length(intercept) != 1L || is.na(intercept))
    stop("outcome_random_intercept must be TRUE or FALSE.", call. = FALSE)
  if (is.null(structure)) {
    if (isTRUE(intercept)) return(list(data=d, random_covs=random_covs, config=NULL))
    structure <- list(static_covs=setdiff(random_covs,time),
                      time_covs=if(time %in% random_covs)"(Intercept)" else character())
  }
  if (!is.list(structure)) stop("outcome_random_structure must be a list.", call.=FALSE)
  if(!is.null(structure$columns))return(.mi_lmm_path_prepare(d,id,time,structure,intercept))
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
  for(v in vars) {
    if(!is.numeric(d[[v]]) || any(!is.finite(d[[v]])))
      stop("Random modifiers must be finite numeric subject-level covariates: ",v,call.=FALSE)
    zz <- split(d[[v]],as.character(d[[id]]))
    if(any(vapply(zz,function(x)length(unique(x))>1L,logical(1))))
      stop("Random modifiers must be subject-level; declare time via time_covs: ",v,call.=FALSE)
  }
  if(!length(tv) && isTRUE(intercept))
    return(list(data=d,random_covs=st,config=NULL))
  norm_time <- ".mi_lmm_re_time"
  cols <- paste0(".mi_lmm_re_t",seq_along(tv))
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

.mi_lmm_tv_spec <- function(dd, id, config) {
  if(isTRUE(config$covariate_paths))return(.mi_lmm_path_spec(dd,id,config))
  st <- config$static_covs; tv <- config$time_covs
  intercept <- config$intercept
  terms <- character(); blocks <- NULL
  if(length(st)==1L && intercept &&
     setequal(tv,c("(Intercept)",st))) {
    group <- st[1L]; levels <- sort(unique(dd[[group]]))
    if(length(levels)!=2L)stop("The interaction random model needs both binary groups.",call.=FALSE)
    blocks <- vector("list",2L)
    for(k in 1:2) {
      nm <- paste0(".mi_lmm_group",k,c("_intercept","_time"))
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

.mi_lmm_lmm_variance_payload <- function(fit) {
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

.mi_lmm_bt <- function(x) paste0("`", gsub("`", "", x, fixed = TRUE), "`")

.mi_lmm_check <- function(data, vars) {
  if (!is.data.frame(data)) data <- as.data.frame(data)
  bad <- setdiff(unique(vars), names(data))
  if (length(bad)) stop("Missing columns: ", paste(bad, collapse = ", "), call. = FALSE)
  if (anyNA(data[[vars[1L]]])) stop("Subject id cannot be missing.", call. = FALSE)
  data
}

.mi_lmm_with_seed <- function(seed, expr) {
  if (is.null(seed) || length(seed) != 1L || !is.finite(seed))
    stop("`seed` must be one finite integer; MI must be reproducible.", call. = FALSE)
  had <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had) old <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (had) assign(".Random.seed", old, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(as.integer(seed))
  force(expr)
}

.mi_lmm_imputation_random <- function(d, id, vars) {
  vars <- unique(vars)
  if (!length(vars)) return(list(used = character(), dropped = character()))
  varies <- vapply(vars, function(v) {
    by_id <- split(d[[v]], as.character(d[[id]]), drop = TRUE)
    any(vapply(by_id, function(x) length(unique(x[is.finite(x)])) > 1L,
               logical(1)))
  }, logical(1))

  list(used = vars[varies], dropped = vars[!varies])
}

.mi_lmm_impute <- function(data, id, time, outcome, obs_indicator,
                           fixed_vars, random_vars, m, maxit, seed,
                           pan_intercept=TRUE,include_time_fixed=TRUE,initialize_predictor_matrix=TRUE) {
  if (!requireNamespace("mice", quietly = TRUE) ||
      !requireNamespace("pan", quietly = TRUE))
    stop("MI-LMM, MI-Liang and MI-IIRR require CRAN packages 'mice' and 'pan'.", call. = FALSE)

  d <- data
  d$.mi_lmm_row <- seq_len(nrow(d))
  visit <- is.finite(d[[time]])
  visited <- d[visit, , drop = FALSE]
  unvisited <- d[!visit, , drop = FALSE]
  if (!nrow(visited)) stop("There are no visit rows.", call. = FALSE)

  marker_observed <- !is.na(visited[[obs_indicator]]) &
    visited[[obs_indicator]] == 1 & is.finite(visited[[outcome]])
  visited[[outcome]][!marker_observed] <- NA_real_

  fixed_vars <- unique(c(fixed_vars, if(include_time_fixed)time))
  random_info <- .mi_lmm_imputation_random(visited, id, random_vars)
  random_vars <- random_info$used
  pred_vars <- unique(c(fixed_vars, random_vars, random_info$dropped))
  bad_numeric <- pred_vars[!vapply(visited[pred_vars], is.numeric, logical(1))]
  if (length(bad_numeric))
    stop("2l.pan predictors must be numeric: ", paste(bad_numeric, collapse = ", "),
         call. = FALSE)

  miss <- !is.finite(visited[[outcome]])
  if (!any(miss)) {
    completed <- rbind(visited, unvisited)
    completed <- completed[order(completed$.mi_lmm_row), , drop = FALSE]
    completed$.mi_lmm_row <- NULL
    ans <- rep(list(completed), m)
    attr(ans, "imputation_random_used") <- random_info$used
    attr(ans, "imputation_random_dropped") <- random_info$dropped
    attr(ans, "imputation_fixed_used") <- setdiff(pred_vars,random_vars)
    return(ans)
  }

  cluster <- ".mi_lmm_cluster"
  while (cluster %in% names(visited)) cluster <- paste0(cluster, "_")
  visited[[cluster]] <- match(visited[[id]], unique(visited[[id]]))

  .mi_lmm_with_seed(seed, {
    if(initialize_predictor_matrix) {
      ini <- mice::mice(visited,maxit=0,printFlag=FALSE,remove.constant=pan_intercept)
      pm <- ini$predictorMatrix;meth <- ini$method
    } else {
      pm <- matrix(0,ncol(visited),ncol(visited),dimnames=list(names(visited),names(visited)))
      meth <- stats::setNames(rep('',ncol(visited)),names(visited))
    }
    pm[,] <- 0
    meth[] <- ""
    meth[outcome] <- "2l.pan"
    pm[outcome, cluster] <- -2L
    for (v in pred_vars) {
      if (v != outcome && v %in% colnames(pm))
        pm[outcome, v] <- if (v %in% random_vars) 2L else 1L
    }
    where <- matrix(FALSE, nrow(visited), ncol(visited),
                    dimnames = list(NULL, names(visited)))
    where[miss, outcome] <- TRUE
    imp <- mice::mice(visited, m = as.integer(m), maxit = as.integer(maxit),
                      predictorMatrix = pm, method = meth, where = where,
                      printFlag = FALSE,intercept=pan_intercept,
                      remove.constant=pan_intercept,
                      eps=if(pan_intercept)1e-4 else 0)
    ans <- lapply(seq_len(m), function(k) {
      z <- mice::complete(imp, k)
      z[[cluster]] <- NULL
      z <- rbind(z, unvisited)
      z <- z[order(z$.mi_lmm_row), , drop = FALSE]
      z$.mi_lmm_row <- NULL
      rownames(z) <- NULL
      z
    })
    attr(ans, "imputation_random_used") <- random_info$used
    attr(ans, "imputation_random_dropped") <- random_info$dropped
    attr(ans, "imputation_fixed_used") <- setdiff(pred_vars,random_vars)
    attr(ans, "imputation_logged_events") <- imp$loggedEvents
    ans
  })
}

.mi_lmm_random_spec <- function(dd, id, random_covs) {
  quote_name <- function(x) paste0("`", gsub("`", "", x, fixed = TRUE), "`")
  out <- list(structurally_identified = TRUE, random_design_rank = 1L,
              random_design_columns = 1L)
  if (!length(random_covs)) {
    out$spec <- stats::as.formula(paste("~ 1 |", quote_name(id)))
    out$parameterization <- "random intercept"
    out$formula <- paste0("~ 1 | ", id)
    return(out)
  }
  within_varies <- vapply(random_covs, function(v) {
    zz <- split(dd[[v]], as.character(dd[[id]]), drop = TRUE)
    any(vapply(zz, function(x) length(unique(x)) > 1L, logical(1)))
  }, logical(1))
  if (length(random_covs) > 1L && any(!within_varies)) {
    stop("Mixed subject-constant and time-varying random slopes require an explicit identifiable quotient.",
         call. = FALSE)
  }
  if (length(random_covs) == 1L && !within_varies[1L]) {
    slope <- random_covs[1L]
    values <- unique(dd[[slope]]); values <- values[!is.na(values)]
    if (length(values) == 1L) {
      out$spec <- stats::as.formula(paste("~ 1 |", quote_name(id)))
      out$parameterization <- paste0("random-intercept quotient; `", slope,
                                     "` has one observed level")
      out$formula <- paste0("~ 1 | ", id, " [constant ", slope, "]")
    } else if (length(values) == 2L) {
      labels <- sort(unique(as.character(values)))
      dd$.mi_lmm_re_group_1 <- as.numeric(as.character(dd[[slope]]) == labels[1L])
      dd$.mi_lmm_re_group_2 <- as.numeric(as.character(dd[[slope]]) == labels[2L])
      out$spec <- stats::setNames(
        list(nlme::pdDiag(~ 0 + .mi_lmm_re_group_1 + .mi_lmm_re_group_2)), id)
      out$parameterization <- paste0(
        "two group-specific random-intercept variances: identified quotient of ~1+",
        slope, "|", id)
      out$formula <- paste0("group-specific random intercept by ", slope, " | ", id)
      out$random_design_rank <- 2L; out$random_design_columns <- 2L
    } else if (is.numeric(dd[[slope]])) {
      z <- vapply(split(dd[[slope]], as.character(dd[[id]]), drop = TRUE),
                  function(x) x[1L], numeric(1))
      vb <- cbind(1, 2 * z, z^2)
      out$structurally_identified <- qr(vb)$rank == 3L
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

.mi_lmm_one_lmm <- function(data, id, time, outcome, long_covs, random_covs,
                            method, optimizer, random_config=NULL) {
  if (!requireNamespace("nlme", quietly = TRUE))
    stop("MI-LMM requires CRAN package 'nlme'.", call. = FALSE)
  keep <- is.finite(data[[time]]) & is.finite(data[[outcome]])
  vars <- unique(c(id, time, outcome, long_covs, random_covs))
  keep <- keep & stats::complete.cases(data[, vars, drop = FALSE])
  dd <- data[keep, , drop = FALSE]
  if (length(unique(dd[[id]])) < 3L) stop("Too few subjects for LMM.", call. = FALSE)
  fixed <- stats::reformulate(unique(c(long_covs, time)), response = outcome)
  Xm <- stats::model.matrix(fixed, data = dd)
  fixed_rank <- qr(Xm)$rank
  if (fixed_rank < ncol(Xm)) stop("LMM fixed-effect design is rank deficient.", call. = FALSE)
  rs <- if(is.null(random_config)).mi_lmm_random_spec(dd,id,random_covs) else
    .mi_lmm_tv_spec(dd,id,random_config)
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
  V <- as.matrix(stats::vcov(fit))
  ap_ok <- is.matrix(fit$apVar) && all(is.finite(fit$apVar))
  ap_msg <- if (ap_ok) "" else if (is.character(fit$apVar)) {
    paste(fit$apVar, collapse = " ")
  } else "Non-finite apVar."
  list(coef = b, vcov = V, fit = fit, n = nrow(dd), n_subject = length(unique(dd[[id]])),
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
       variance_payload=.mi_lmm_lmm_variance_payload(fit))
}

.mi_lmm_compact_analysis_fit <- function(z) {
  b <- z$coef
  V <- z$vcov
  se <- if (is.matrix(V) && nrow(V) == length(b)) {
    stats::setNames(sqrt(pmax(diag(V), 0)), names(b))
  } else stats::setNames(rep(NA_real_, length(b)), names(b))
  ap <- z$fit$apVar
  if (!is.matrix(ap) || !all(is.finite(ap))) ap <- NULL
  list(
    coefficients = b,
    coef = b,
    se = se,
    vcov = V,
    varcorr = z$varcorr,
    lmm_variance_parameters=z$variance_payload$table,
    lmm_variance_vcov_delta=z$variance_payload$vcov,
    apVar = ap,
    n = z$n,
    n_subject = z$n_subject,
    converged = z$converged,
    identified = z$identified,
    covariance_regular = z$covariance_regular,
    structurally_identified = z$structurally_identified,
    random_parameterization = z$random_parameterization,
    random_formula = z$random_formula
  )
}

.mi_lmm_pool <- function(fits) {
  nm <- Reduce(intersect, lapply(fits, function(z) names(z$coef)))
  if (!length(nm)) stop("No common coefficients across imputations.", call. = FALSE)
  Q <- do.call(rbind, lapply(fits, function(z) z$coef[nm]))
  W <- Reduce(`+`, lapply(fits, function(z) z$vcov[nm, nm, drop = FALSE])) / length(fits)
  B <- if (length(fits) > 1L) stats::cov(Q) else matrix(0, length(nm), length(nm))
  if (length(nm) == 1L) B <- matrix(B, 1L, 1L)
  dimnames(B) <- list(nm, nm)
  T <- W + (1 + 1 / length(fits)) * B
  mm <- length(fits); u <- pmax(diag(W), 0); b <- pmax(diag(B), 0)
  riv <- ifelse(u > 0, (1 + 1 / mm) * b / u,
                ifelse(b > 0, Inf, 0))
  df <- if (mm <= 1L) rep(Inf, length(nm)) else
    ifelse(b > 0, (mm - 1) * (1 + 1 / pmax(riv, .Machine$double.eps))^2, Inf)
  fmi <- ifelse(is.finite(riv),
                (riv + 2 / (df + 3)) / (riv + 1), 1)
  names(riv) <- names(df) <- names(fmi) <- nm
  list(coef = colMeans(Q), vcov = T, within = W, between = B,
       relative_increase_variance = riv, rubin_df = df,
       fraction_missing_information = fmi)
}

.fit_mi_lmm <- function(
    data, id = "id", time = "time", outcome = "Y", obs_indicator = "R",
    visit_covs = c("Z", "X"), long_covs = c("Z", "X"), random_covs = "Z",
    m = 5L, maxit = 5L, seed = 1L,
    lmm_method = c("REML", "ML"), optimizer = c("nlminb", "optim"),
    keep_completed = FALSE,
    outcome_random_structure=NULL, outcome_random_intercept=TRUE) {
  lmm_method <- match.arg(lmm_method)
  optimizer <- match.arg(optimizer)
  if (length(m) != 1L || !is.finite(m) || m < 1 || m != as.integer(m) ||
      length(maxit) != 1L || !is.finite(maxit) || maxit < 1 || maxit != as.integer(maxit))
    stop("`m` and `maxit` must be positive integers.", call. = FALSE)
  prepared <- .mi_lmm_tv_prepare(as.data.frame(data),id,time,random_covs,
                                 outcome_random_structure,outcome_random_intercept)
  random_covs <- prepared$random_covs
  d <- .mi_lmm_check(prepared$data, c(id, time, outcome, obs_indicator,
                                      visit_covs, long_covs, random_covs))
  completed <- .mi_lmm_impute_structured(d, id, time, outcome, obs_indicator,
                                         fixed_vars = unique(c(visit_covs, long_covs)),
                                         random_vars = random_covs, m = m, maxit = maxit, seed = seed,
                                         config=prepared$config)
  imp_random_used <- attr(completed, "imputation_random_used")
  imp_random_dropped <- attr(completed, "imputation_random_dropped")
  fits <- lapply(completed, .mi_lmm_one_lmm, id = id, time = time, outcome = outcome,
                 long_covs = long_covs, random_covs = random_covs,
                 method = lmm_method, optimizer = optimizer,random_config=prepared$config)
  pooled <- .mi_lmm_pool(fits)
  variance_pool <- .mi_lmm_pool_variance(fits)
  sig <- vapply(completed, function(z) {
    yy <- z[[outcome]]; sum((seq_along(yy) %% 1009L) * ifelse(is.finite(yy), yy, 0))
  }, 0.0)
  conv <- all(vapply(fits, `[[`, logical(1), "converged"))
  identified <- all(vapply(fits, `[[`, logical(1), "identified"))
  ans <- list(
    method_key = "mi_lmm", method = "MI+LMM",
    estimand = "Fixed-effect coefficient in the completed-data Gaussian LMM, pooled over MAR multiple imputations",
    coefficients = pooled$coef, vcov = pooled$vcov,
    se = sqrt(pmax(diag(pooled$vcov), 0)), vcov_type = "Rubin total variance; within-imputation variance (plug-in): nlme model-based fitted-covariance GLS",
    nuisance = list(within_vcov = pooled$within, between_vcov = pooled$between,
                    lmm_variance_parameters=variance_pool$table,
                    lmm_variance_vcov_delta=variance_pool$vcov),
    converged = conv,
    diagnostics = list(ok = isTRUE(conv && identified && all(is.finite(pooled$coef)) &&
                                     all(is.finite(pooled$vcov))),
                       identified = identified,
                       regular = all(vapply(fits, `[[`, logical(1), "covariance_regular")),
                       full_stage_regular = all(vapply(fits, `[[`, logical(1), "covariance_regular")),
                       outcome_inference_available = all(vapply(fits, function(z)
                         .lmm_outcome_inference_available(z$vcov, z$coef,
                                                          df = z$fit$fixDF$X, converged = isTRUE(z$converged),
                                                          identified = isTRUE(z$identified)), logical(1))) &&
                         .lmm_outcome_inference_available(pooled$vcov, pooled$coef,
                                                          df = pooled$rubin_df, converged = conv, identified = identified),
                       outcome_inference_scope = "Rubin pooled fixed-effect covariance; complete-data covariance-parameter apVar checked separately",
                       structurally_identified = all(vapply(fits, `[[`, logical(1),
                                                            "structurally_identified")),
                       apVar_ok = vapply(fits, `[[`, logical(1), "apVar_ok"),
                       apVar_message = vapply(fits, `[[`, character(1), "apVar_message"),
                       m_requested = as.integer(m), m_success = length(fits),
                       rubin_df = pooled$rubin_df,
                       relative_increase_variance = pooled$relative_increase_variance,
                       fraction_missing_information = pooled$fraction_missing_information,
                       maxit = as.integer(maxit), seed = as.integer(seed),
                       n_subjects = length(unique(d[[id]])),
                       n_visits = sum(is.finite(d[[time]])),
                       n_observed = sum(is.finite(d[[time]]) & !is.na(d[[obs_indicator]]) &
                                          d[[obs_indicator]] == 1 & is.finite(d[[outcome]])),
                       n_imputed_per_dataset = vapply(completed, function(z)
                         sum(is.finite(d[[time]]) &
                               (is.na(d[[obs_indicator]]) | d[[obs_indicator]] != 1 |
                                  !is.finite(d[[outcome]])) & is.finite(z[[outcome]])), 0L),
                       original_R_preserved = all(vapply(completed, function(z)
                         identical(z[[obs_indicator]], d[[obs_indicator]]), logical(1))),
                       downstream_process = "completed outcomes at actual visits; R is provenance only",
                       imputation_random_used = imp_random_used,
                       imputation_group_covariate=attr(completed,"imputation_group_covariate"),
                       imputation_fixed_used=attr(completed,"imputation_fixed_used"),
                       imputation_logged_events=attr(completed,"imputation_logged_events"),
                       imputation_random_dropped_as_aliased = imp_random_dropped,
                       random_formula = fits[[1L]]$random_formula,
                       fixed_rank = fits[[1L]]$fixed_rank,
                       fixed_columns = fits[[1L]]$fixed_columns,
                       random_parameterization = fits[[1L]]$random_parameterization,
                       random_design_rank = fits[[1L]]$random_design_rank,
                       random_design_columns = fits[[1L]]$random_design_columns,
                       varcorr = lapply(fits, `[[`, "varcorr"),
                       imputation_signature = sig),
    assumptions = c("2l.pan imputation is valid under a congenial MAR model, not latent-variable MNAR",
                    "LMM covariance is the complete-data within-imputation covariance"),
    call = match.call()
  )
  storage_started <- proc.time()[["elapsed"]]
  ans$analysis_fits <- lapply(fits, .mi_lmm_compact_analysis_fit)
  ans$diagnostics$analysis_fit_storage_seconds <-
    proc.time()[["elapsed"]] - storage_started
  ans$diagnostics$analysis_fit_storage_bytes <-
    as.numeric(object.size(ans$analysis_fits))
  if (keep_completed) ans$completed_data <- completed
  class(ans) <- c("mi_lmm")
  ans
}

Multiple_imputation_IP <- function(data,ip_method=c('LMM','Liang','IIRR'),...) {
  ip_method<-match.arg(ip_method);fit_method(method=c(LMM='mi_lmm',Liang='mi_liang',IIRR='mi_iirr')[ip_method],data=data,...)
}
