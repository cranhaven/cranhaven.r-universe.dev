pmix_vec_loop <-
  function(qs,
           locations_list,
           scales_list,
           weights_list,
           distr.k) {
    mean_over_list(qs, locations_list, scales_list, weights_list, distr.k, pmix)
  }

dmix_vec_loop <-
  function(xs,
           locations_list,
           scales_list,
           weights_list,
           distr.k) {
    mean_over_list(xs, locations_list, scales_list, weights_list, distr.k, dmix)
  }

qmix_vec_loop <-
  function(ps,
           locations_list,
           scales_list,
           weights_list,
           distr.k) {
    mean_over_list(ps, locations_list, scales_list, weights_list, distr.k, qmix)
  }

mean_over_list <-
  function(xs,
           locations_list,
           scales_list,
           weights_list,
           distr.k,
           mixdistfun) {
    res <- 0.0 * xs
    nit <- length(locations_list)
    for (it in 1:nit) {
      res <- res + mixdistfun(
        xs,
        locations_list[[it]],
        scales_list[[it]],
        weights_list[[it]],
        distr.k
      )
    }
    return(res / nit)
  }


mixdistfun <-
  function(xs,
           locations,
           scales,
           weights,
           distr.k,
           distfun) {
    res <- 0.0 * xs
    for (cmp in seq_along(locations)) {
      res <-
        res + weights[cmp] * distfun(xs,
          distr = distr.k,
          mu = locations[cmp],
          sigma = scales[cmp]
        )
    }
    return(res)
  }

dmix <- function(xs, locations, scales, weights, distr.k) {
  mixdistfun(xs, locations, scales, weights, distr.k, dk)
}
pmix <- function(qs, locations, scales, weights, distr.k) {
  mixdistfun(qs, locations, scales, weights, distr.k, pk)
}

mixdistfun_cens <-
  function(xlefts,
           xrights,
           c_code_filters,
           locations,
           scales,
           weights,
           distr.k,
           distfun) {
    res <- 0.0 * xlefts
    for (cmp in seq_along(locations)) {
      res <-
        res + weights[cmp] * distfun(xlefts,
          xrights,
          c_code_filters,
          distr = distr.k,
          mu = locations[cmp],
          sigma = scales[cmp]
        )
    }
    return(res)
  }
dmixcens <- function(xlefts,
                     xrights,
                     c_code_filters,
                     locations,
                     scales,
                     weights,
                     distr.k) {
  mixdistfun_cens(xlefts, xrights, c_code_filters, locations, scales, weights, distr.k, dkcens2)
}



qmix_one_val_with_scales <- function(p, locations, scales, weights, distr.k, max_scale, min_loc, max_loc) {
  f_help_vec <- function(qs) {
    p - pmix(qs, locations, scales, weights, distr.k)
  }
  if (distr.k == 2 | distr.k == 5) {
    lowerbound <- 0
    upperbound <- max_loc + 100 * max_scale
  } else if (distr.k == 3) {
    lowerbound <- 0
    upperbound <- 1
  } else {
    lowerbound <- min_loc - 100 * max_scale
    upperbound <- max_loc + 100 * max_scale
  }
  uniroot(f = f_help_vec, lower = lowerbound, upper = upperbound)$root
}

qmix_one_val <- function(p, locations, scales, weights, distr.k) {
  max_scale <- max(scales)
  max_loc <- max(locations)
  min_loc <- min(locations)
  qmix_one_val_with_scales(p, locations, scales, weights, distr.k, max_scale, min_loc, max_loc)
}

qmix <- function(ps, locations, scales, weights, distr.k, parallel = TRUE) {
  if (Sys.info()[["sysname"]] == "Windows") parallel <- FALSE

  max_scale <- max(scales)
  max_loc <- max(locations)
  min_loc <- min(locations)
  unlist(parallel::mclapply(ps,
    FUN = function(p) qmix_one_val_with_scales(p, locations, scales, weights, distr.k, max_scale, min_loc, max_loc),
    mc.cores = ifelse(test = parallel, yes = parallel::detectCores(), no = 1)
  ))
}
