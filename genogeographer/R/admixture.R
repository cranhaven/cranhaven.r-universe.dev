xlx <- function(x) if(x<=0) 0 else x*log(x) ## x*log(x), per definition 0 for x = 0.
## it also catches the case of x negative in loops below.

EM <- function(x,n){ ## x = (xj, xk), n = (nj, nk)
  dev <- Inf
  ## dev_trace <- dev
  p <- x/n                  ## Init estimate of pj and pk: xj/(2n)
  ## p_trace <- list(p)
  ## pY_trace <- NULL
  p[p == 0] <- 1e-6
  p[p == 1] <- 1 - 1e-6
  if(sum(p)==0) p <- p+1e-6 ## if both 0, set p to (eps, eps)
  if(sum(p)==2) p <- p-1e-6 ## if both 1, set p to (1-eps, 1-eps)
  while(dev>1e-8){
      p0 <- p                         ##
      p. <- prod(p)                   ## p. = pj*pk
      pY_x0_1 <- (p-p.)/(sum(p)-2*p.) ## P(Y | x0 = 1)
      p <- (x+pY_x0_1)/(n+1)          ## p_j^{(t+1)} = {x_j + E(y|x0 = 1, xj,xk)} / {2nj + 1}
      dev <- mean(abs(p-p0))          ## difference between p^{(t)} and p^{(t+1)}
      ## dev_trace <- c(dev_trace, dev)
      ## p_trace <- c(p_trace, list(p))
      ## pY_trace <- c(pY_trace, list(pY_x0_1))
  }
  # list(pY_x0_1, # er fordelingen af Y givet at x0 = x0^1 + x0^2 = 1
  #     dev = dev_trace, p_trace = p_trace, pY_trace = pY_trace) 
  pY_x0_1
  
}

## EM(x = c(1238, 150), n = c(1244, 150))

logQ <- function(y, x, n){ ## -2 log-likelihood ratio
    -2*(xlx(n)-xlx(n+1) + xlx(x+y)-xlx(x) + xlx(n-x+1-y)-xlx(n-x) - xlx(y)-xlx(1-y))
}

het_marker <- function(x,n){
  pY_x0_1 <- EM(x, n)
  dev <- 0
  for(y in 1:0){
    dev <- dev + pY_x0_1[2-y]*(logQ(y,x[1],n[1])+logQ(1-y,x[2],n[2]))
  }
  list(dev = dev, pY_x0_1 = pY_x0_1)
}

dev_marker <- function(x0, x, n){
  if(x0==1){ ## if heterozygote - do this;
    return(het_marker(x, n))
  }
  else{ ## else add contributions from x0=0 and x0=2 (in cases: a/2 equals 0 or 1)
    return(list(dev = logQ(x0/2,x[1],n[1]) + logQ(x0/2,x[2],n[2])))
  }
}

p_x0 <- function(x, n){
  p <- x/n
  p. <- prod(p)
  c(prod(1-p), sum(p)-2*p., p.)
}

marker_z <- function(x0, xj, nj, xk, nk){  ## we assume that n is 2*n
  # x0: count main allele (0:2)
  # x: count af main allele in pop_j and pop_k
  # n: n1 and n2 in pop_j and pop_k ## ASSUMES n is 2*n_individuals
    #pe <- pest(a,p,n)
  marker_stats <- lapply(0:2, dev_marker, x=c(xj,xk), n=c(nj,nk))
  pY_x0_1 <- marker_stats[[2]]$pY_x0_1
  devs <- unlist(lapply(marker_stats, "[[", "dev"))
  obs <- devs[x0+1]
  p_x0 <- p_x0(c(xj,xk), c(nj,nk))
  E_z <- sum(p_x0*devs)
  V_z <- sum(p_x0*(devs-E_z)^2)
  list(tibble(z_center = obs-E_z, V_z = V_z, pY_x0_1 = list(pY_x0_1)))
}

marker_z_ <- Vectorize(marker_z, vectorize.args = list("x0", "xj", "nj", "xk", "nk"))

hypothesis_pairs <- function(hyp = NULL, df, grouping = "meta", pure = FALSE){
  if(is.list(hyp) && length(hyp) > 1) return(hyp)
  if(length(hyp) > 1) hyp_ <- hyp
  else hyp_ <- df %>% pull(grouping) %>% unique()
  if(length(hyp) == 1) hyp_j <- hyp
  else hyp_j <- hyp_
  hyp_pairs <- crossing(hyp = hyp_j, hyp_ = hyp_)
  if(pure) hyp_pairs <- hyp_pairs %>% filter(hyp <= hyp_)
  else hyp_pairs <- hyp_pairs %>% filter(hyp < hyp_)
  hyp_pairs <- hyp_pairs %>% rowwise() %>% 
    mutate(pair = paste(hyp, hyp_, sep =":"), 
           hyp = list(c(hyp,hyp_))) %>% 
    select(-hyp_)
  split(hyp_pairs$hyp, hyp_pairs$pair) %>% lapply("[[", 1)
}

profile_admix <- function(x0, df, hyp = NULL, grouping = "meta", return_all = FALSE, calc_logP = TRUE, brow = FALSE){
  ## build fix
  . <- NULL
  locus <- NULL
  z_center <- NULL
  V_z <- NULL
  lopP <- NULL
  n_x1 <- NULL
  val <- NULL
  x1 <- NULL
  nx_pop <- NULL
  pY_x0_1 <- NULL
  varlogP <- NULL
  logP <- NULL
  ## 
  if(length(hyp) != 2 | is.list(hyp)) stop("'hyp' needs to be a vector of length 2")
  grouping_ <- quo(!!sym(grouping))
  df <- df %>% select(starts_with(grouping)) %>% select(grouping, ends_with("data")) %>% 
    distinct_(.dots = grouping, .keep_all = TRUE) %>% 
    filter(!!grouping_ %in% hyp)
  grouping_data <- grep(pattern = "_data", x = names(df), value = TRUE)
  if(nrow(df) == 0) stop(paste("No",grouping,"matches",paste(hyp, collapse = " and ")))
  ## browser()
  df <- df %>% 
    mutate_at(grouping_data, 
              funs(purrr::map(.x = ., .f = ~ .x %>% filter(x0 == 0, X0 == 0) %>%  distinct(locus, n,x1)))) %>% 
    unnest() %>% 
    gather(key = n_x1, value = val, n, x1) %>% 
    unite(nx_pop, n_x1, !!grouping_) %>% 
    spread(key = nx_pop, value = val)
  nn <- paste(rep(c("x1","n","freq", "p"), 2), rep(hyp, each = 4), sep = "_") %>% 
    set_names(nm = paste0(rep(c("x","n","f","p"),2), rep(c("j", "k"), each = 4)))
  nn_ <- lapply(nn, function(y) quo(!!sym(y)))
  df <- inner_join(df, x0, by = "locus")
  df <- df %>% mutate(res = marker_z_(x0 = x0, xj = !!(nn_$xj), xk = !!(nn_$xk), nj = !!(nn_$nj), nk = !!(nn_$nk))) %>%
    unnest()
  if(calc_logP){
    if(brow) browser()
    df <- df %>% mutate(p = purrr::map(pY_x0_1, .f = ~.x %>% t() %>% as_tibble() %>% set_names(hyp))) %>% 
      select(-pY_x0_1) %>% unnest(.sep = "_") %>% 
      mutate(!!nn_$fj := (!!nn_$xj + ifelse(x0==1, !!nn_$pj, x0/2))/(!!nn_$nj + 1), 
             !!nn_$fk := (!!nn_$xk + ifelse(x0==1, !!nn_$pk, x0/2))/(!!nn_$nk + 1)) %>% 
      select(locus, ends_with(hyp[1]), ends_with(hyp[2]), everything()) %>% 
      ## slice(37) %>% 
      mutate(
        # P = case_when(
        #   x0 == 0 ~ (1-!!nn_$fj)*(1-!!nn_$fk),
        #   x0 == 1 ~ (!!nn_$fj)*(1-!!nn_$fk) + (1-!!nn_$fj)*(!!nn_$fk),
        #   x0 == 2 ~ (!!nn_$fj)*(!!nn_$fk)
        # ),
        logP = case_when(
          x0 == 0 ~ log10(1-!!nn_$fj) + log10(1-!!nn_$fk),
          x0 == 1 ~ log10(!!nn_$fj*(1-!!nn_$fk) + (1-!!nn_$fj)*!!nn_$fk),
          x0 == 2 ~ log10(!!nn_$fj) + log10(!!nn_$fk)
        )) %>% ## print(n = Inf) ## %>% 
      mutate(
        varlogP = case_when(
          x0 == 0 ~ 
            1/(2*(1-!!nn_$fj)*(1-!!nn_$fk))*( (!!nn_$fj)*(!!nn_$fk)/(2*!!nn_$nj*!!nn_$nk) + 
                                      (1-!!nn_$fj)*(!!nn_$fk)/!!nn_$nj +
                                      (1-!!nn_$fk)*(!!nn_$fj)/!!nn_$nk ),
          x0 == 1 ~ 
            (!!nn_$nk*!!nn_$fj*(1-!!nn_$fj)*(1-2*!!nn_$fk) + !!nn_$nj*!!nn_$fk*(1-!!nn_$fk)*(1-2*!!nn_$fj) + 
               2*!!nn_$fj*!!nn_$fk*( (1-!!nn_$fj)*(1-!!nn_$fk) + 2*!!nn_$nk*!!nn_$fk*(1-!!nn_$fj) + 2*!!nn_$nj*!!nn_$fj*(1-!!nn_$fk) )) /
            (2*!!nn_$nk*!!nn_$nk*(!!nn_$fj*(1-!!nn_$fk) + !!nn_$fk*(1-!!nn_$fj))^2),
          x0 == 2 ~ 
            1/(2*!!nn_$fj*!!nn_$fk)*( (1-!!nn_$fj)*(1-!!nn_$fk)/(2*!!nn_$nj*!!nn_$nk) + 
                                        (1-!!nn_$fj)*(!!nn_$fk)/!!nn_$nj +
                                        (1-!!nn_$fk)*(!!nn_$fj)/!!nn_$nk )
        ),
        varlogP = varlogP / log(10)^2
      )
  }
  if(return_all){
    return(df)
  }
  if(!("logP" %in% names(df))){
    df <- df %>% mutate(logP = NA, varlogP = NA)
  }
  df %>% summarise(
    z = sum(z_center)/sqrt(sum(V_z)), 
    n = paste(median(!!nn_$nj, na.rm = TRUE)/2, median(!!nn_$nk, na.rm = TRUE)/2, sep = ":"),
    logP = sum(logP), 
    varlogP = sum(varlogP)
    )
    #pull(z)
}

####

add_results <- function(result, admix_result, CI = 0.95){
  hyp_j <- NULL
  hyp_k <- NULL
  hypo_j <- NULL
  hypo_k <- NULL
  . <- NULL
  logP <- NULL
  varlogP <- NULL
  z <- NULL
  p_value <- NULL
  z_CI <- qnorm(1-(1-CI)/2)
  grouping <- names(result)[1]
  grouping_ <- names(result)[2]
  admix_result <- admix_result %>% 
    inner_join(result %>% select(1,hypo_j = 2), c("hyp_j" = grouping)) %>% 
    inner_join(result %>% select(1,hypo_k = 2), c("hyp_k" = grouping))
  admix_result <- admix_result %>% 
    unite(!!grouping, hyp_j, hyp_k, sep=":") %>% 
    unite(!!grouping_, hypo_j, hypo_k, sep=":") %>%
    mutate(labs = .[[!!grouping]]) %>% 
    mutate(
      logP_upr = logP + z_CI*sqrt(varlogP),
      logP_lwr = logP - z_CI*sqrt(varlogP),
      p_value = pnorm(z, lower.tail = FALSE),
      accept = (p_value > (1-CI)),
      labs = paste0(labs, " (",n,")")
    ) %>% 
    rename(z_score = z)
  result %>% 
    mutate(labs = paste(labs)) %>% 
    mutate_at(vars(n), paste) %>% 
    bind_rows(admix_result) %>% 
    mutate(labs = fct_reorder(labs, logP)) %>% 
    arrange(desc(logP))
}

#####

#' Compute the z-score (and more) for admixed hypotheses
#' @name profile_admixture
#' @param x0 A data frame/tibble with two columns: `locus` and `x0`
#' @param df A tibble of reference profiles (as for `genogeo`)
#' @param hyp If NULL all levels of `grouping` is crossed and looped over as pairwise hypotheses. 
#' If a single level of `grouping`, this value is crossed with the remaining levels.
#' If vector of two levels this is the only tested hypothesis.
#' @param grouping Should the calculations be for meta populations ("meta") or sample populations ("pop")?
#' @param return_all Should z-score be returned (FALSE) or all locus results (TRUE)?
#' @param calc_logP Should log P(Geno|Hyp) be calculated (TRUE) or not (FALSE)?
#' @param ... additional arguments passed on to other functions
#' @return A tibble of z-scores, or a list of pairwise results if `return_all = TRUE`
#' @export

profile_admixture <- function(x0, df, hyp = NULL, grouping = "meta", return_all = FALSE, calc_logP = TRUE, ...){
  hyp_pairs <- hypothesis_pairs(hyp, df, grouping = grouping, ...)
  res <- lapply(hyp_pairs, function(h){
   profile_admix(x0 = x0, df = df, hyp = h, grouping = grouping, return_all = return_all, calc_logP = calc_logP, ...)
  })
  # res <- replicate(length(hyp_pairs), NULL, simplify = FALSE)
  # for(i in seq_along(res)){
  #   ## if(i == 7) browser()
  #   res[[i]] <- profile_admix(x0 = x0, df = df, hyp = hyp_pairs[[i]], brow = (i == 7), 
  #                             grouping = grouping, return_all = return_all, calc_logP = calc_logP, ...)
  #   cat(paste0(i,", "))
  # }
  if(return_all){
    if(length(res) == 1) return(res[[1]])
    else return(res)
  }
  res_df <- res %>% 
    enframe(name = "hyp", value = "z_score") %>% 
    unnest() %>% 
    extract(into = c("hyp_j", "hyp_k"), regex = "(.*):(.*)", hyp)
  res_df
}
