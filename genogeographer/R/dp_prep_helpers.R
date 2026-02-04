## Helper functions

dist_x0_cond_x <- function(n,x) {
  x1 <- n+2-x
  c(x1*(x1-1),2*x*x1,x*(x-1))/((n+2)*(n+1))
}

dist_x0_cond_x_ <- function(n,x) {
  x1 <- n+2-x
  data.frame(p0 = x1*(x1-1), p1 = 2*x*x1, p2 = x*(x-1))/((n+2)*(n+1))
}

xlx <- function(x){
  x[x<=0] <- 1
  log(x)*x
}

ss_moments <- function(x0,x1,N){
  x <- x1+x0
  p <- dist_x0_cond_x(N,x)
  xx <- (x-(0:2))[p>0]
  stat <- xlx(xx)+xlx(N-xx)-((0:2)[p>0]==1)*2*log(2)
  p <- p[p>0]
  Estat <- sum(p*stat)
  stat <- stat-Estat
  Vstat <- sum(p*stat^2)
  c(Estat, Vstat)
}

ss_moments_ <- function(x0, x1, N){
  x <- x1+x0
  p <- matrix(dist_x0_cond_x(N,x), ncol = 3, byrow = FALSE)
  x <- matrix(rep(x, each = 3), ncol = 3, byrow = TRUE)
  N <- matrix(rep(N, each = 3), ncol = 3, byrow = TRUE)
  m02 <- matrix(0:2, ncol = 3, nrow = nrow(x), byrow = TRUE)
  xx <- x - m02*(p>0)
  stat <- xlx(xx)+xlx(N-xx)-(m02*(p>0) == 1)*2*log(2)
  Estat <- rowSums((p*stat)*(p>0))
  stat <- stat-Estat
  Vstat <- rowSums((p*stat^2)*(p>0))
  data.frame(E_score = Estat, V_score = Vstat)
}

log_P <- function(x0, p){
  if(x0==0) 2*log(1-p)
  else if(x0==1) log(2) + log(p) + log(1-p)
  else if(x0==2) 2*log(p)
  else NA
}

log_P_ <- function(x0, p){
  case_when(
    x0==0 ~ 2*log(1-p),
    x0==1 ~ log(2) + log(p) + log(1-p),
    x0==2 ~ 2*log(p),
    TRUE ~ NA_real_
    )
}

varlog_P <- function(x0, n, p){
  q <- 1-p
  if(x0==0) 4*p/(q*n) + 2*p*(3-5*q)/(q*n)^2 + p*(1-6*q*p)/(q*n)^3
  else if(x0==1) ( (1-4*p*q)/n + (10*p*q-2)/n^2 - (6*p*q-1)/n^3 )/(p*q)
  else if(x0==2) 4*q/(p*n) + 2*q*(3-5*p)/(p*n)^2 + q*(1-6*p*q)/(p*n)^3
  else NA
}

varlog_P_ <- function(x0, n, p){
  q <- 1-p
  case_when(
    x0==0 ~ 4*p/(q*n) + 2*p*(3-5*q)/(q*n)^2 + p*(1-6*q*p)/(q*n)^3,
    x0==1 ~ ( (1-4*p*q)/n + (10*p*q-2)/n^2 - (6*p*q-1)/n^3 )/(p*q),
    x0==2 ~ 4*q/(p*n) + 2*q*(3-5*p)/(p*n)^2 + q*(1-6*p*q)/(p*n)^3,
    TRUE ~ NA_real_
  )
}

score_add_df <- function(db, grouping = "pop", .out_of_place = NULL){
  ## build fixes : start ##
  out_of_place <- NULL
  x0 <- NULL
  X0 <- NULL
  meta <- NULL
  x1 <- NULL
  lat <- NULL
  n_ <- NULL
  lon <- NULL
  . <- NULL
  locus <- NULL
  X1 <- NULL
  freq <- NULL
  ## build fixes : end ##
  grouping <- match.arg(grouping, c("pop", "meta", "cluster"))
  if(!(grouping %in% names(db))) return(NULL)
  if(!("out_of_place" %in% names(db))){
    if(!is.null(.out_of_place)){
      db <- db %>% 
        full_join(tibble(pop = .out_of_place, out_of_place = TRUE), by = "pop") %>% 
        mutate(out_of_place = !is.na(out_of_place))
      
    }
    else db <- db %>% mutate(out_of_place = FALSE)
    db <- db %>% 
      select_(.dots = append(names(db), "out_of_place", grep("locus",names(db))-1))
  }
  if(grouping %in% c("meta", "cluster")){
    db <- db %>% filter(x0 == X0, x0==0) %>% 
      select_(.dots = vars(meta:x1, grouping)) %>% 
      mutate(n_ = ifelse(out_of_place, 0, n)) %>% 
      group_by_(.dots = c(grouping, "locus", "main_allele", "other_allele")) %>% 
      summarise(
        out_of_place = out_of_place[1],
        lat = weighted.mean(lat, w = n_, na.rm = TRUE),
        lon = weighted.mean(lon, w = n_, na.rm = TRUE),
        n = sum(n),
        x1 = sum(x1)
      ) %>% ungroup() %>% 
      select_(.dots = vars(grouping, n, lat:lon, out_of_place, locus:x1))
  }
  ## X0 and X1 are relevant for simulations
  dd <- crossing(db, x0 = 0:2, X0 = 0:2) %>% 
    mutate(X1 = x1 + x0 - X0)
  ## NB! if x0 == X0 we have that x1 == X1
  dd <- dd %>% bind_cols(ss_moments_(x0 = .$X0, x1 = .$X1, N = .$n))
  ## Fix correct simulation probabilities
  dd <- dd %>% mutate(
    score = xlx(X1) + xlx(n-X1) - 2*log(2)*(X0==1), ## The unstandardised locus score
    freq = (x1 + x0)/(n + 2), ## freqs relevant for log P
    logP = log_P_(x0 = x0, p = freq)/log(10), ## log_10 scale
    varlogP = varlog_P_(x0 = x0, n = n, p = freq)/(log(10)^2)) ## log_10 scale
  ## Meta population
  dd
}

#' Pre-compute the scores for a given reference database
#' 
#' Convert the counts from each population over a range of AIMs SNPs q
#' to observed likelihood ratio test, its mean and variance.
#' Based on these pre-computed the evaluation of a specific profile is done
#' using \code{genogeo} with the resulting dataframe as \code{df}.
#' @param db A dataframe with columns similar to those of \code{simulate_pops()}.
#' If \code{db} contains information (recommended!) about "meta" (meta population)
#' and "lat"/"lon" (location) these are carried over into the calculations
#' @param ... Additional arguments passed to \code{score_add_df}
#' @return A tibble with population and locus specific score information
#' @export
#' @examples
#' df_ <- simulate_pops(pop_n = 4, aims_n = 50)
#' df_db <- pops_to_DB(df_)

pops_to_DB <- function(db, ...){
  ## build fixes : start ##
  meta <- NULL
  lon <- NULL
  lat <- NULL
  n_row <- NULL
  pop <- NULL
  population <- NULL
  metapopulation <- NULL
  cluster <- NULL
  ## build fixes : end ##
  dd_pop <- score_add_df(db, ...)
  if("meta" %in% names(dd_pop)){
  meta_hulls <- convex_hulls(dd_pop) %>% 
    select(meta, lat, lon , n_row) %>% nest(hulls_meta = -c(meta)) %>% group_by(meta)
  browser()
  dd_meta <- score_add_df(db = dd_pop, grouping = "meta") %>% 
    inner_join(meta_hulls, by = "meta") %>% 
    nest(meta_data = -c(meta)) %>% 
    group_by(meta)
  dd <- inner_join(
    dd_pop %>% nest(population_data = -c(pop, population, meta, metapopulation)) %>% 
      group_by(pop, population, meta, metapopulation),
    dd_meta, 
    by = "meta")
  }
  else dd <- dd_pop %>% nest(population_data = -c(pop, population)) %>% group_by(pop, population)
  if("cluster" %in% names(db)){
    cluster_hulls <- convex_hulls(dd_pop, grouping = "cluster") %>% 
      select(cluster, lat, lon , n_row) %>% nest(hulls_cluster = -c(cluster)) %>% group_by(cluster)
    dd_cluster <- score_add_df(db = dd_pop, grouping = "cluster") %>% 
      inner_join(cluster_hulls, by = "cluster") %>% 
      nest(cluster_data = -c(cluster)) %>% 
      group_by(cluster)
    dd <- inner_join(
      dd_pop %>% nest(population_data = -c(pop, population, meta, metapopulation, cluster)) %>% 
        group_by(pop, population, meta, metapopulation, cluster),
      dd_meta, by = "meta") %>% 
      inner_join(dd_cluster, by = "cluster")
  }
  dd
}

DB_locus_set <- function(db, locus){
  . <- NULL
  db <- db %>% 
    mutate(population_data = purrr::map(.$population_data, ~ semi_join(.x, locus, by = "locus")),
           meta_data = purrr::map(.$meta_data, ~ semi_join(.x, locus, by = "locus")))
  if("cluster" %in% names(db)){
    db <- db %>% 
      mutate(cluster_data = purrr::map(.$cluster_data, ~ semi_join(.x, locus, by = "locus")))
  }
  db
}

