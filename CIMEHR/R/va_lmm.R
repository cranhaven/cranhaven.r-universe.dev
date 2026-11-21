.va_lmm_path_prepare <- function(d,id,time,structure,intercept) {
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
    nm <- paste0('.va_lmm_path_',make.names(name))
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

.va_lmm_path_spec <- function(dd,id,config) {
  group <- config$covariance_group
  if(!is.null(group)) {
    if(!setequal(unique(dd[[group]]),c(0,1)))stop('Grouped random covariance needs both binary groups.',call.=FALSE)
    main <- config$path_columns[match(config$trajectory,config$coefficient_names)]
    terms <- character(); blocks <- vector('list',2L)
    for(k in 1:2) {
      nm <- paste0('.va_lmm_group',k,c('_intercept','_slope'))
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

.va_lmm_tv_prepare <- function(d, id, time, random_covs, structure, intercept) {
  if (!is.logical(intercept) || length(intercept) != 1L || is.na(intercept))
    stop("outcome_random_intercept must be TRUE or FALSE.", call. = FALSE)
  if (is.null(structure)) {
    if (isTRUE(intercept)) return(list(data=d, random_covs=random_covs, config=NULL))
    structure <- list(static_covs=setdiff(random_covs,time),
                      time_covs=if(time %in% random_covs)"(Intercept)" else character())
  }
  if (!is.list(structure)) stop("outcome_random_structure must be a list.", call.=FALSE)
  if(!is.null(structure$columns))return(.va_lmm_path_prepare(d,id,time,structure,intercept))
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
  norm_time <- ".va_lmm_re_time"
  cols <- paste0(".va_lmm_re_t",seq_along(tv))
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

.va_lmm_tv_spec <- function(dd, id, config) {
  if(isTRUE(config$covariate_paths))return(.va_lmm_path_spec(dd,id,config))
  st <- config$static_covs; tv <- config$time_covs
  intercept <- config$intercept
  terms <- character(); blocks <- NULL
  if(length(st)==1L && intercept &&
     setequal(tv,c("(Intercept)",st))) {
    group <- st[1L]; levels <- sort(unique(dd[[group]]))
    if(length(levels)!=2L)stop("The interaction random model needs both binary groups.",call.=FALSE)
    blocks <- vector("list",2L)
    for(k in 1:2) {
      nm <- paste0(".va_lmm_group",k,c("_intercept","_time"))
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
      "random time slope without random intercept" else "random intercept and within-subject time slope"
    formula <- paste(paste(deparse(ff),collapse=" "),"|",id)
  }
  Z <- if(!is.null(blocks))as.matrix(dd[,terms,drop=FALSE]) else
    stats::model.matrix(ff,dd)
  rank <- qr(Z)$rank
  if(rank<ncol(Z))stop("The declared random design is rank deficient.",call.=FALSE)
  list(spec=spec,data=dd,parameterization=label,formula=formula,
       structurally_identified=TRUE,random_design_rank=rank,random_design_columns=ncol(Z))
}

.va_lmm_lmm_variance_payload <- function(fit) {
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

.fit_va_lmm <- function(
    d, focal = "Z", id = "id", time = "time", outcome = "Y",
    obs_indicator = "R", visit_covs = c("Z", "X"),
    long_covs = c("Z", "X"), random_covs = NULL,
    outcome_random_structure = NULL, outcome_random_intercept = TRUE) {

  method_key <- "va_lmm"
  method_name <- "VA-LMM"
  estimand_text <- paste0(
    "Working estimator of the underlying longitudinal association, adjusted for the ",
    "cumulative number of visits. In simulations it is evaluated against the same ",
    "data-generating association beta as every comparator; its fitted coefficient is ",
    "conditional on the count, so any pseudo-true displacement is reported as ",
    "association-recovery bias rather than treated as a different truth"
  )
  assumptions_text <- c(
    "a visit is a row with finite time, irrespective of R",
    "R == 1 with finite outcome defines an analysed observed marker",
    "visit count is the cumulative number of finite-time visits at or before the row",
    "rows are sorted by subject and time before constructing the count",
    "the conditional mean given visit count and covariates is correctly specified",
    "Gaussian random effects and homoskedastic independent Gaussian residuals",
    "subjects are independent; model-based nlme covariance is reported"
  )
  empty_coef <- function() data.frame(
    term = character(), estimate = numeric(), se = numeric(), df = numeric(),
    statistic = numeric(), p_value = numeric(), row.names = NULL,
    check.names = FALSE
  )
  failure <- function(message, warnings = character(), diagnostics = list()) {
    z <- list(
      ok = FALSE, converged = FALSE, identified = FALSE,
      structurally_identified = FALSE, apVar_ok = FALSE,
      apVar_message = NA_character_, fixed_rank = NA_integer_,
      fixed_columns = NA_integer_, random_design_rank = NA_integer_,
      random_design_columns = NA_integer_, random_parameterization = NA_character_,
      n_subjects_input = NA_integer_, n_subjects_analysis = 0L,
      n_rows_input = if (is.data.frame(d)) nrow(d) else NA_integer_,
      n_rows_visits = 0L, n_rows_observed = 0L, n_rows_analysis = 0L,
      count_type = "cumulative visit count", count_range = NULL,
      warnings = unique(warnings), message = message, varcorr = NULL
    )
    z[names(diagnostics)] <- diagnostics
    list(
      method_key = method_key, method = method_name, estimand = estimand_text,
      coefficients = empty_coef(), vcov = NULL, se = numeric(),
      vcov_type = "variance (plug-in): nlme model-based fitted-covariance GLS (ML)", fit = NULL,
      diagnostics = z, assumptions = assumptions_text
    )
  }
  if (!requireNamespace("nlme", quietly = TRUE)) {
    return(failure("CRAN package `nlme` is required."))
  }
  d <- tryCatch(as.data.frame(d), error = function(e) NULL)
  if (is.null(d)) return(failure("`d` cannot be converted to a data.frame."))
  long_covs <- unique(as.character(long_covs))
  random_covs <- unique(as.character(random_covs))
  prepared <- tryCatch(.va_lmm_tv_prepare(d, id, time, random_covs,
    outcome_random_structure, outcome_random_intercept), error=function(e)e)
  if(inherits(prepared,"error"))return(failure(conditionMessage(prepared)))
  d <- prepared$data; random_covs <- prepared$random_covs
  required <- unique(c(id, time, outcome, obs_indicator, long_covs, random_covs))
  missing_cols <- setdiff(required, names(d))
  if (length(missing_cols)) {
    return(failure(paste("Missing required column(s):", paste(missing_cols, collapse = ", "))))
  }
  if (!is.numeric(d[[time]]) || !is.numeric(d[[outcome]])) {
    return(failure("The time and outcome columns must be numeric."))
  }

  n_subjects_input <- length(unique(d[[id]][!is.na(d[[id]])]))
  work <- d[, required, drop = FALSE]
  work$.original_row <- seq_len(nrow(work))
  keep <- which(!is.na(work[[id]]))
  work$.valid_visit <- !is.na(work[[time]]) & is.finite(work[[time]])
  work$.observed_marker <- work$.valid_visit & !is.na(work[[obs_indicator]]) &
    work[[obs_indicator]] == 1 & is.finite(work[[outcome]])
  ids <- .lmm_id_codes(work[[id]][keep])
  sorted <- order(ids$rank, work[[time]][keep], work$.original_row[keep], na.last = TRUE)
  ordering <- keep[sorted]
  history <- integer(nrow(work))
  history[ordering] <- plm_group_cumsum(
    as.integer(work$.valid_visit[ordering]), ids$group[sorted], ids$n
  )
  work$.history_count <- history

  fixed_terms <- unique(c(long_covs, time, ".history_count"))
  model_cols <- unique(c(id, outcome, fixed_terms, random_covs))
  rows <- ordering[work$.observed_marker[ordering]]
  analysis <- work[rows[stats::complete.cases(work[model_cols])[rows]], model_cols, drop = FALSE]
  n_rows_visits <- sum(work$.valid_visit[keep])
  n_rows_observed <- sum(work$.observed_marker[keep])
  n_subjects_analysis <- length(unique(analysis[[id]]))
  count_range <- if (nrow(analysis)) range(analysis$.history_count) else NULL
  common_diag <- list(
    n_subjects_input = n_subjects_input, n_subjects_analysis = n_subjects_analysis,
    n_rows_input = nrow(d), n_rows_visits = n_rows_visits,
    n_rows_observed = n_rows_observed, n_rows_analysis = nrow(analysis),
    count_range = count_range
  )
  if (!nrow(analysis) || n_subjects_analysis < 2L) {
    return(failure("Fewer than two subjects have complete observed outcomes.",
                   diagnostics = common_diag))
  }

  fixed_formula <- stats::reformulate(fixed_terms, response = outcome)
  fixed_matrix <- tryCatch(stats::model.matrix(fixed_formula, data = analysis),
                           error = function(e) e)
  if (inherits(fixed_matrix, "error")) {
    return(failure(conditionMessage(fixed_matrix), diagnostics = common_diag))
  }
  fixed_rank <- qr(fixed_matrix)$rank
  fixed_full_rank <- fixed_rank == ncol(fixed_matrix)
  if (!fixed_full_rank) {
    return(failure(
      "The fixed-effect design is rank deficient (often time/count collinearity).",
      diagnostics = c(common_diag, list(
        fixed_rank = fixed_rank, fixed_columns = ncol(fixed_matrix)
      ))
    ))
  }

  quote_name <- function(x) paste0("`", gsub("`", "", x, fixed = TRUE), "`")
  random_parameterization <- "random intercept"
  structurally_identified <- TRUE
  random_design_rank <- 1L
  random_design_columns <- 1L
  if (!is.null(prepared$config)) {
    rs <- tryCatch(.va_lmm_tv_spec(analysis,id,prepared$config),error=function(e)e)
    if(inherits(rs,"error"))return(failure(conditionMessage(rs)))
    analysis <- rs$data; random_spec <- rs$spec
    random_parameterization <- rs$parameterization
    random_design_rank <- rs$random_design_rank
    random_design_columns <- rs$random_design_columns
    structurally_identified <- rs$structurally_identified
  } else if (!length(random_covs)) {
    random_spec <- stats::as.formula(paste("~ 1 |", quote_name(id)))
  } else {
    within_varies <- vapply(random_covs, function(v) {
      g <- split(analysis[[v]], as.character(analysis[[id]]), drop = TRUE)
      any(vapply(g, function(x) length(unique(x)) > 1L, logical(1)))
    }, logical(1))
    if (length(random_covs) == 1L && !within_varies[1L]) {
      slope <- random_covs[1L]
      values <- unique(analysis[[slope]])
      values <- values[!is.na(values)]
      if (length(values) == 1L) {
        random_spec <- stats::as.formula(paste("~ 1 |", quote_name(id)))
        random_parameterization <- paste0(
          "random-intercept quotient; `", slope, "` has one observed level"
        )
      } else if (length(values) == 2L) {
        labels <- sort(unique(as.character(values)))
        analysis$.re_group_1 <- as.numeric(as.character(analysis[[slope]]) == labels[1L])
        analysis$.re_group_2 <- as.numeric(as.character(analysis[[slope]]) == labels[2L])
        random_spec <- stats::setNames(
          list(nlme::pdDiag(~ 0 + .re_group_1 + .re_group_2)), id
        )
        random_parameterization <- paste0(
          "two group-specific random-intercept variances (identified quotient of ~1+",
          slope, "|", id, ")"
        )
        random_design_rank <- 2L
        random_design_columns <- 2L
      } else {
        return(failure(
          paste0(
            "Subject-constant random slope `", slope,
            "` is only supported here for one or two observed levels; use a genuine time-varying slope."
          ),
          diagnostics = c(common_diag, list(
            fixed_rank = fixed_rank, fixed_columns = ncol(fixed_matrix),
            random_parameterization = "rejected subject-constant random slope"
          ))
        ))
      }
    } else if (all(within_varies)) {
      rhs <- paste(vapply(random_covs, quote_name, character(1)), collapse = " + ")
      random_spec <- stats::as.formula(paste("~ 1 +", rhs, "|", quote_name(id)))
      random_parameterization <- paste0(
        "unstructured random intercept and genuine within-subject slope(s): ",
        paste(random_covs, collapse = ", ")
      )
      random_matrix <- stats::model.matrix(stats::reformulate(random_covs), data = analysis)
      random_design_rank <- qr(random_matrix)$rank
      random_design_columns <- ncol(random_matrix)
      structurally_identified <- random_design_rank == random_design_columns
    } else {
      return(failure(
        "Mixing subject-constant and time-varying random slopes needs an explicit quotient parameterization.",
        diagnostics = c(common_diag, list(
          fixed_rank = fixed_rank, fixed_columns = ncol(fixed_matrix),
          random_parameterization = "rejected nonidentified mixed-slope specification"
        ))
      ))
    }
  }

  caught_warnings <- character()
  control <- nlme::lmeControl(opt = "optim", msMaxIter = 5000, msMaxEval = 5000)
  fit <- tryCatch(
    withCallingHandlers(
      nlme::lme(
        fixed = fixed_formula, data = analysis, random = random_spec,
        method = "ML", control = control, na.action = stats::na.fail
      ),
      warning = function(w) {
        caught_warnings <<- c(caught_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    return(failure(conditionMessage(fit), caught_warnings,
      c(common_diag, list(
        structurally_identified = structurally_identified,
        fixed_rank = fixed_rank, fixed_columns = ncol(fixed_matrix),
        random_design_rank = random_design_rank,
        random_design_columns = random_design_columns,
        random_parameterization = random_parameterization
      ))))
  }

  fixed_value <- nlme::fixef(fit)
  fixed_se <- sqrt(diag(as.matrix(fit$varFix)))
  fixed_df <- fit$fixDF[["X"]]
  fixed_t <- fixed_value / fixed_se
  tt <- cbind(Value = fixed_value, Std.Error = fixed_se, DF = fixed_df,
              `t-value` = fixed_t, `p-value` = 2 * stats::pt(-abs(fixed_t), fixed_df))
  se <- tt[, "Std.Error"]
  names(se) <- rownames(tt)
  coef_table <- data.frame(
    term = rownames(tt), estimate = unname(tt[, "Value"]), se = unname(se),
    df = unname(tt[, "DF"]), statistic = unname(tt[, "t-value"]),
    p_value = unname(tt[, "p-value"]), row.names = NULL, check.names = FALSE
  )
  V <- tryCatch(stats::vcov(fit), error = function(e) NULL)
  apVar_ok <- is.matrix(fit$apVar) && all(is.finite(fit$apVar))
  apVar_message <- if (apVar_ok) "" else if (is.character(fit$apVar)) {
    paste(fit$apVar, collapse = " ")
  } else "Non-finite apVar."
  varcorr <- tryCatch(nlme::VarCorr(fit), error = function(e) NULL)
  vp <- .va_lmm_lmm_variance_payload(fit)
  focal_ok <- focal %in% coef_table$term &&
    is.finite(coef_table$estimate[match(focal, coef_table$term)])

  list(
    method_key = method_key, method = method_name, estimand = estimand_text,
    coefficients = coef_table, vcov = V, se = se,
    nuisance = list(lmm_variance_parameters=vp$table,
                    lmm_variance_vcov_delta=vp$vcov),
    vcov_type = "variance (plug-in): nlme model-based fitted-covariance GLS (ML)", fit = fit,
    diagnostics = c(common_diag, list(
      ok = isTRUE(focal_ok), converged = TRUE,
      identified = isTRUE(structurally_identified && fixed_full_rank),
      regular = isTRUE(apVar_ok), full_stage_regular = isTRUE(apVar_ok),
      outcome_inference_available = .lmm_outcome_inference_available(
        V, tt[, "Value"], se, tt[, "DF"], converged = isTRUE(focal_ok),
        identified = isTRUE(structurally_identified && fixed_full_rank)),
      outcome_inference_scope = "model-based fixed-effect covariance; covariance-parameter apVar checked separately",
      structurally_identified = structurally_identified, apVar_ok = apVar_ok,
      apVar_message = apVar_message, fixed_rank = fixed_rank,
      fixed_columns = ncol(fixed_matrix), random_design_rank = random_design_rank,
      random_design_columns = random_design_columns,
      random_parameterization = random_parameterization,
      count_type = "cumulative visit count",
      warnings = unique(caught_warnings),
      message = if (apVar_ok) "" else apVar_message, varcorr = varcorr
    )),
    assumptions = assumptions_text
  )
}
