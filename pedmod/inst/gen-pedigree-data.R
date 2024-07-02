sim_pedigree_data <- local(envir = new.env(), {
#####
# util function. Do not call these functions. Skip to the end of the file
# and see the plots
invisible(list2env(local({
  idcount <- 0L
  .get_id <- function(n)
    if(n > 0L)
      replicate(n, idcount <<- idcount + 1L) else NULL
  .reset_id <- function()
    (idcount <<- 0L)

  list(.get_id = .get_id, .reset_id = .reset_id)
}), environment()))

.sim_mating <- function(rchild, rmatch, max_depth = 1L, lvl, dadid,
                        momid){
  if(is.null(dadid) || is.null(momid)){
    id <- .get_id(2L)
    father <- mother <- rep(NA_integer_, 2L)
    sex <- 1:2 # 1: male, 2: female
    dadid <- id[1]
    momid <- id[2]
    obslvl <- rep(0L, 2L)
    do_match <- needs_match <- rep(FALSE, 2L)

  } else
    id <- father <- mother <- obslvl <- do_match <- sex <-  NULL

  # sample first lvl
  n_child     <- rchild(1L)
  sex         <- c(sex, (runif(n_child) > .5) + 1L)

  id     <- c(id, .get_id(n_child))
  father <- c(father, rep(dadid, n_child))
  mother <- c(mother, rep(momid, n_child))
  obslvl <- c(obslvl, rep(lvl, n_child))

  do_match    <- c(do_match, rmatch(n_child))
  needs_match <- do_match

  list(id = id, father = father, mother = mother, obslvl = obslvl,
       do_match = do_match, needs_match = needs_match, sex = sex)
}

.sim_household <- function(rchild, rmatch, max_depth, lvl, dadid, momid,
                           max_members = 100L, n_members = 0L){
  out <- list(.sim_mating(rchild, rmatch, max_depth, lvl, dadid, momid))

  get_n_members <- function()
    sum(sapply(out, function(x) length(x[["id"]]))) + n_members
  get_needs_match <- function(obj)
    obj$needs_match & max_depth > obj$obslvl
  shuffle <- function(x)
    if(length(x) == 1L) x else sample(x)

  finish_next <- FALSE
  while(any(
    unlist(sapply(out, get_needs_match))) && !finish_next){
    for(i in shuffle(seq_along(out))){
      if(finish_next)
        break

      for(j in which(get_needs_match(out[[i]]))){
        if(finish_next)
          break

        while(out[[i]]$needs_match[j]){
          # find a matching family
          new_hou <- .sim_household(
            rchild, rmatch, max_depth = out[[i]]$obslvl[j], lvl,
            dadid = NULL, momid = NULL, n_members = get_n_members())
          is_match <- which(
            new_hou$needs_match & new_hou$sex != out[[i]]$sex[j] &
              new_hou$obslvl == out[[i]]$obslvl[j])

          if(length(is_match) == 0L)
            next

          # create the new family
          is_match <- is_match[1L]
          new_hou$needs_match[is_match] <- out[[i]]$needs_match[j] <- FALSE

          if(get_n_members() + length(new_hou$id) >= max_members)
            finish_next <- TRUE # we do not add anymore

          new_pai <- .sim_mating(
            rchild, rmatch, max_depth, lvl = out[[i]]$obslvl[j] + 1L,
            dadid =
              if(out[[i]]$sex[j] == 1L)
                out[[i]]$id[j] else new_hou$id[is_match],
            momid =
              if(out[[i]]$sex[j] == 1L)
                new_hou$id[is_match] else out[[i]]$id[j])

          if(length(new_pai$id) > 0L)
            out <- do.call(c, list(out, list(new_hou), list(new_pai)))
        }

      }
    }
  }

  nam <- names(out[[1L]])
  structure(
    lapply(nam, function(name) do.call(c, lapply(out, "[[", name))),
    names = nam)
}

# simulates a family starting with the oldest members.
#
# Args:
#   rchild: simulation function to sample number of children. Takes an
#           integer with the number of samples to draw.
#   rmatch: simulation function to sample whether a given person meets a
#           partner. Takes an integer with the number of samples to draw.
#   max_depth: max depth of the families.
#   max_members: roughly the amount of members to return at most.
#
# TODO: starting from the bottom might be smarter?
sim_fam <- function(rchild, rmatch, max_depth = 2L, max_members = 100L){
  .reset_id()

  for(i in 1:100){
    out <- as.data.frame(.sim_household(
      rchild, rmatch, max_depth, lvl = 1L, dadid = NULL, momid = NULL,
      max_members = max_members))

    if(NROW(out) > 2L)
      # drop the boring
      break
  }
  old_ids <- out$id
  within(out, {
    id     <- match(id    , old_ids, NA_integer_)
    father <- match(father, old_ids, NA_integer_)
    mother <- match(mother, old_ids, NA_integer_)
    needs_match <- NULL
    do_match <- NULL
  })
}

#####
# parameters. Start with family settings
function(
  max_depth = 2L, max_members = 150L,
  sc = c(Genetic = .5, Maternal = .33),
  n_families = 100L, do_plot = FALSE,
  gen_X = function(n)
    cbind(`(Intercept)` = 1, X1 = rnorm(n), X2 = scale(1:n)),
  beta = c(`(Intercept)` = -1, X1 = .3, X2 = .2)){
  # generate families and outcomes
  library(kinship2)
  dat <- replicate(n_families, {
    for(ii in 1:100){
      dat <- sim_fam(
        rchild = function(n)
          sample.int(size = n, 4L, prob = c(.2, .4, .3, .1)),
        rmatch = function(n) runif(n) > .1,
        max_depth = max_depth, max_members = max_members)
      pedAll <- pedigree(id = dat$id, dadid = dat$father, momid = dat$mother,
                         sex = dat$sex, famid = rep(1, NROW(dat)))["1"]

      rel_mat_full <- t(2  * kinship(pedAll))
      dimnames(rel_mat_full) <- list(dat$id, dat$id)
      rel_mat <- rel_mat_full
      keep <- dat$obslvl == max(dat$obslvl)
      if(sum(keep) > max_members / 10L)
        break
    }

    # matrix for the genetic effect
    rel_mat <- rel_mat[keep, keep, drop = FALSE]
    n_obs <- NROW(rel_mat)

    # matrix for the maternal effect
    Z <- matrix(0., length(dat$mother), length(dat$mother))
    rownames(Z) <- dat$id
    for(i in 1:length(dat$mother)){
      m_id <- dat$mother[i]
      if(!is.na(m_id))
        Z[i, m_id] <- 1
    }
    met_mat <- tcrossprod(Z %*% rel_mat_full, Z)
    met_mat <- met_mat[keep, keep, drop = FALSE]

    # simulate the error term
    Sig <- sc[1] * rel_mat + sc[2] * met_mat + diag(NCOL(met_mat))
    epsilon <- drop(rnorm(n_obs) %*% chol(Sig))

    # simulate the covariates
    X <- gen_X(n_obs)

    # simulate the outcomes and return
    y <- drop(X %*% beta) + epsilon > 0

    list(y = y, X = X, rel_mat = rel_mat, met_mat = met_mat,
         rel_mat_full = rel_mat_full, pedAll = pedAll)
  }, simplify = FALSE)

  # add the true parameter values
  list(beta = beta, sc = sc, sim_data = dat)
}
})
