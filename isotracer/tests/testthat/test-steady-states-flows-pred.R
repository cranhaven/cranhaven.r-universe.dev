### * Setup

n_cores <- min(2, parallel::detectCores())
n_chains <- max(n_cores, 2)
N_ITERS <- 20

run_mcmc <- function(...) {
  isotracer:::run_mcmc(..., cores = n_cores, chains = n_chains)
}

new_networkModel <- function() {
  isotracer::new_networkModel(quiet = TRUE)
}

library(dplyr)
library(tibble)
library(purrr)

### * Closed network, single unit, no steady, no split

test_that("Predictions for closed network, single unit, no steady, no split", {
  m <- aquarium_mod %>%
    set_priors(constant_p(0), "lambda")
  capture_warnings(capture_output({ r <- run_mcmc(m, iter = N_ITERS) }))
  ss <- tidy_steady_states(m, r)
  ff <- tidy_flows(m, r, steady_state = TRUE)
  zs <- ss %>%
    mutate(group_iter_label = pmap_chr(
      list(mcmc.chain, mcmc.iteration),
      function(c, i) {
        paste(c, i, sep = "|")
      }
    ))
  zf <- ff %>%
    mutate(group_iter_label = pmap_chr(
      list(mcmc.chain, mcmc.iteration),
      function(c, i) {
        paste(c, i, sep = "|")
      }
    ))
  zf <- left_join(zf, zs %>% select(group_iter_label, stable_sizes)) %>%
    select(group_iter_label, mcmc.parameters, stable_sizes, flows)
  
  check_row_i <- function(row_i) {
    f <- zf$flows[[row_i]]
    p <- enframe(zf$mcmc.parameters[[row_i]])
    s <- enframe(zf$stable_sizes[[row_i]]) %>%
      rename(from = name, source_size = value)
    f <- left_join(f, s, by = "from") %>%
      mutate(portion_act = 1) %>%
      mutate(source_size_act = source_size * portion_act) %>%
      mutate(upsilon_param = paste0("upsilon_", from, "_to_", to)) %>%
      mutate(lambda_param = paste0("lambda_", from)) %>%
      mutate(param = ifelse(is.na(to), lambda_param, upsilon_param)) %>%
      left_join(p %>% rename(param = name, param_value = value),
                by = "param") %>%
      mutate(expected_flow = source_size_act * param_value) %>%
      select(-portion_act, -upsilon_param, -lambda_param, -source_size, -param)
    cp <- unique(unlist(comps(m))) %>%
      enframe() %>%
      mutate(from = map_dbl(value, function(v) {
        f %>% filter(from == v) %>% pull(expected_flow) %>% sum
      })) %>%
      mutate(to = map_dbl(value, function(v) {
        f %>% filter(to == v) %>% pull(expected_flow) %>% sum
      }))
    cp
  }

  for (i in seq_len(nrow(zf))) {
    x <- check_row_i(i)
    expect_true(all(x[["from"]] - x[["to"]] < 1e-12))
  }
})

### * Closed network, multiple units, no steady, no split

test_that("Predictions for closed network, multiple units, no steady, no split", {
  exp0 <- tibble::tribble(
    ~time.day,  ~species, ~biomass, ~prop15N,
    0,   "algae",     1.02,  0.00384,
    1,   "algae",       NA,   0.0534,
    1.5,   "algae",    0.951,       NA,
    2,   "algae",    0.889,   0.0849,
    2.5,   "algae",       NA,   0.0869,
    3,   "algae",    0.837,   0.0816,
    0, "daphnia",     1.74,  0.00464,
    1, "daphnia",       NA,  0.00493,
    1.5, "daphnia",     2.48,       NA,
    2, "daphnia",       NA,  0.00831,
    2.5, "daphnia",     2.25,       NA,
    3, "daphnia",     2.15,   0.0101,
    0,     "NH4",    0.208,     0.79,
    1,     "NH4",    0.227,       NA,
    1.5,     "NH4",       NA,    0.482,
    2,     "NH4",    0.256,    0.351,
    2.5,     "NH4",       NA,    0.295,
    3,     "NH4",     0.27,        NA
  )
  exp <- bind_rows(rep(list(exp0), 4)) %>%
    mutate(stream = rep(rep(c("LL", "UL"), times = c(18, 18)), 2),
           intro = rep(c("gup", "riv"), times = c(36, 36))) %>%
    mutate(biomass = biomass * runif(72, 0.5, 2),
           prop15N = prop15N * runif(72, 0.8, 1.2))
  inits <- exp %>% dplyr::filter(time.day == 0)
  obs <- exp %>% dplyr::filter(time.day > 0)

  m <- new_networkModel() %>%
    set_topo("NH4 -> algae -> daphnia -> NH4") %>%
    set_init(inits, comp = "species", size = "biomass",
             prop = "prop15N", group_by = c("stream", "intro")) %>%
    set_obs(obs, comp = "species", size = "biomass",
            prop = "prop15N", time = "time.day") %>%
    add_covariates(upsilon + lambda ~ stream + intro)
  m <- m %>% 
    set_priors(normal_p(0, 0.5), "") %>%
    set_priors(constant_p(0), "lambda")
  capture_warnings(capture_output({ r <- run_mcmc(m, iter = N_ITERS) }))

  ss <- tidy_steady_states(m, r)
  ff <- tidy_flows(m, r, steady_state = TRUE)

  # Check consistency
  zs <- ss %>%
    mutate(group_iter_label = pmap_chr(
      list(group, mcmc.chain, mcmc.iteration),
      function(g, c, i) {
        paste(g[1], g[2], c, i, sep = "|")
      }
    ))
  zf <- ff %>%
    mutate(group_iter_label = pmap_chr(
      list(group, mcmc.chain, mcmc.iteration),
      function(g, c, i) {
        paste(g[1], g[2], c, i, sep = "|")
      }
    ))
  zf <- left_join(zf, zs %>% select(group_iter_label, stable_sizes)) %>%
    select(group, group_iter_label, mcmc.parameters, stable_sizes, flows)

  check_row_i <- function(row_i) {
    f <- zf$flows[[row_i]]
    g <- zf$group[[row_i]]
    c <- paste(g[2], g[1], sep = "|")
    p <- enframe(zf$mcmc.parameters[[row_i]])
    s <- enframe(zf$stable_sizes[[row_i]]) %>%
      rename(from = name, source_size = value)
    f <- left_join(f, s, by = "from") %>%
      mutate(portion_act = 1) %>%
      mutate(source_size_act = source_size * portion_act) %>%
      mutate(upsilon_param = paste0("upsilon_", from, "_to_", to, "|", c)) %>%
      mutate(lambda_param = paste0("lambda_", from, "|", c)) %>%
      mutate(param = ifelse(is.na(to), lambda_param, upsilon_param)) %>%
      left_join(p %>% rename(param = name, param_value = value),
                by = "param") %>%
      mutate(expected_flow = source_size_act * param_value) %>%
      select(-portion_act, -upsilon_param, -lambda_param, -source_size, -param)
    cp <- unique(unlist(comps(m))) %>%
      enframe() %>%
      mutate(from = map_dbl(value, function(v) {
        f %>% filter(from == v) %>% pull(expected_flow) %>% sum
      })) %>%
      mutate(to = map_dbl(value, function(v) {
        f %>% filter(to == v) %>% pull(expected_flow) %>% sum
      }))
    cp
  }

  for (i in seq_len(nrow(zf))) {
    x <- check_row_i(i)
    expect_true(all(x[["from"]] - x[["to"]] < 1e-12))
  }
  
})

### * Open network, multiple units, one steady, no split

test_that("Predictions for open network, multiple units, one steady, no split", {
  exp0 <- tibble::tribble(
    ~time.day,  ~species, ~biomass, ~prop15N,
    0,   "algae",     1.02,  0.00384,
    1,   "algae",       NA,   0.0534,
    1.5,   "algae",    0.951,       NA,
    2,   "algae",    0.889,   0.0849,
    2.5,   "algae",       NA,   0.0869,
    3,   "algae",    0.837,   0.0816,
    0, "daphnia",     1.74,  0.00464,
    1, "daphnia",       NA,  0.00493,
    1.5, "daphnia",     2.48,       NA,
    2, "daphnia",       NA,  0.00831,
    2.5, "daphnia",     2.25,       NA,
    3, "daphnia",     2.15,   0.0101,
    0,     "NH4",    0.208,     0.79,
    1,     "NH4",    0.227,       NA,
    1.5,     "NH4",       NA,    0.482,
    2,     "NH4",    0.256,    0.351,
    2.5,     "NH4",       NA,    0.295,
    3,     "NH4",     0.27,        NA
  )
  exp <- bind_rows(rep(list(exp0), 4)) %>%
    mutate(stream = rep(rep(c("LL", "UL"), times = c(18, 18)), 2),
           intro = rep(c("gup", "riv"), times = c(36, 36))) %>%
    mutate(biomass = biomass * runif(72, 0.5, 2),
           prop15N = prop15N * runif(72, 0.8, 1.2))
  inits <- exp %>% dplyr::filter(time.day == 0)
  obs <- exp %>% dplyr::filter(time.day > 0)

  m <- new_networkModel() %>%
    set_topo("NH4 -> algae -> daphnia -> NH4") %>%
    set_steady("NH4") %>%
    set_init(inits, comp = "species", size = "biomass",
             prop = "prop15N", group_by = c("stream", "intro")) %>%
    set_obs(obs, comp = "species", size = "biomass",
            prop = "prop15N", time = "time.day") %>%
    add_covariates(upsilon + lambda ~ stream + intro)
  
  m <- m %>% 
    set_priors(normal_p(0, 0.5), "") %>%
    set_priors(constant_p(1), "lambda_NH4")

  capture_warnings(capture_output({ r <- run_mcmc(m, iter = N_ITERS) }))

  ss <- tidy_steady_states(m, r)
  ff <- tidy_flows(m, r, steady_state = TRUE)
  
  # Check consistency
  zs <- ss %>%
    mutate(group_iter_label = pmap_chr(
      list(group, mcmc.chain, mcmc.iteration),
      function(g, c, i) {
        paste(g[1], g[2], c, i, sep = "|")
      }
    ))
  zf <- ff %>%
    mutate(group_iter_label = pmap_chr(
      list(group, mcmc.chain, mcmc.iteration),
      function(g, c, i) {
        paste(g[1], g[2], c, i, sep = "|")
      }
    ))
  zf <- left_join(zf, zs %>% select(group_iter_label, stable_sizes)) %>%
    select(group, group_iter_label, mcmc.parameters, stable_sizes, flows)

  check_row_i <- function(row_i) {
    f <- zf$flows[[row_i]]
    g <- zf$group[[row_i]]
    c <- paste(g[2], g[1], sep = "|")
    p <- enframe(zf$mcmc.parameters[[row_i]])
    s <- enframe(zf$stable_sizes[[row_i]]) %>%
      rename(from = name, source_size = value)
    f <- left_join(f, s, by = "from") %>%
      mutate(portion_act = 1) %>%
      mutate(source_size_act = source_size * portion_act) %>%
      mutate(upsilon_param = paste0("upsilon_", from, "_to_", to, "|", c)) %>%
      mutate(lambda_param = paste0("lambda_", from, "|", c)) %>%
      mutate(param = ifelse(is.na(to), lambda_param, upsilon_param)) %>%
      left_join(p %>% rename(param = name, param_value = value),
                by = "param") %>%
      mutate(expected_flow = source_size_act * param_value) %>%
      select(-portion_act, -upsilon_param, -lambda_param, -source_size, -param)
    cp <- unique(unlist(comps(m))) %>%
      enframe() %>%
      mutate(from = map_dbl(value, function(v) {
        f %>% filter(from == v) %>% pull(expected_flow) %>% sum
      })) %>%
      mutate(to = map_dbl(value, function(v) {
        f %>% filter(to == v) %>% pull(expected_flow) %>% sum
      }))
    cp[cp$value != "NH4", ]
  }

  for (i in seq_len(nrow(zf))) {
    x <- check_row_i(i)
    expect_true(all(x[["from"]] - x[["to"]] < 1e-12))
  }

})

### * Open network, multiple units, one steady, one split

test_that("Predictions for open network, multiple units, one steady, one split", {
  exp0 <- tibble::tribble(
    ~time.day,  ~species, ~biomass, ~prop15N,
    0,   "algae",     1.02,  0.00384,
    1,   "algae",       NA,   0.0534,
    1.5,   "algae",    0.951,       NA,
    2,   "algae",    0.889,   0.0849,
    2.5,   "algae",       NA,   0.0869,
    3,   "algae",    0.837,   0.0816,
    0, "daphnia",     1.74,  0.00464,
    1, "daphnia",       NA,  0.00493,
    1.5, "daphnia",     2.48,       NA,
    2, "daphnia",       NA,  0.00831,
    2.5, "daphnia",     2.25,       NA,
    3, "daphnia",     2.15,   0.0101,
    0,     "NH4",    0.208,     0.79,
    1,     "NH4",    0.227,       NA,
    1.5,     "NH4",       NA,    0.482,
    2,     "NH4",    0.256,    0.351,
    2.5,     "NH4",       NA,    0.295,
    3,     "NH4",     0.27,        NA
  )
  exp <- bind_rows(rep(list(exp0), 4)) %>%
    mutate(stream = rep(rep(c("LL", "UL"), times = c(18, 18)), 2),
           intro = rep(c("gup", "riv"), times = c(36, 36))) %>%
    mutate(biomass = biomass * runif(72, 0.5, 2),
           prop15N = prop15N * runif(72, 0.8, 1.2))
  inits <- exp %>% dplyr::filter(time.day == 0)
  obs <- exp %>% dplyr::filter(time.day > 0)

  m <- new_networkModel() %>%
    set_topo("NH4 -> algae -> daphnia -> NH4") %>%
    set_steady("NH4") %>%
    set_split("algae") %>%
    set_init(inits, comp = "species", size = "biomass",
             prop = "prop15N", group_by = c("stream", "intro")) %>%
    set_obs(obs, comp = "species", size = "biomass",
            prop = "prop15N", time = "time.day") %>%
    add_covariates(upsilon + lambda ~ stream + intro)
  
  m <- m %>% 
    set_priors(normal_p(0, 0.5), "") %>%
    set_priors(constant_p(1), "lambda_NH4") %>%
    set_priors(uniform_p(0, 1), "portion")

  capture_warnings(capture_output({ r <- run_mcmc(m, iter = N_ITERS) }))

  ss <- tidy_steady_states(m, r)
  ff <- tidy_flows(m, r, steady_state = TRUE)

  # Check consistency
  zs <- ss %>%
    mutate(group_iter_label = pmap_chr(
      list(group, mcmc.chain, mcmc.iteration),
      function(g, c, i) {
        paste(g[1], g[2], c, i, sep = "|")
      }
    ))
  zf <- ff %>%
    mutate(group_iter_label = pmap_chr(
      list(group, mcmc.chain, mcmc.iteration),
      function(g, c, i) {
        paste(g[1], g[2], c, i, sep = "|")
      }
    ))
  zf <- left_join(zf, zs %>% select(group_iter_label, stable_sizes)) %>%
    select(group, group_iter_label, mcmc.parameters, stable_sizes, flows)

  check_row_i <- function(row_i) {
    f <- zf$flows[[row_i]]
    g <- zf$group[[row_i]]
    c <- paste(g[2], g[1], sep = "|")
    p <- enframe(zf$mcmc.parameters[[row_i]])
    s <- enframe(zf$stable_sizes[[row_i]]) %>%
      rename(from = name, source_size = value)
    f <- left_join(f, s, by = "from") %>%
      mutate(portion_param = paste0("portion.act_", from)) %>%
      left_join(p %>% rename(portion_param = name, portion_act = value)) %>%
      mutate(portion_act = ifelse(is.na(portion_act), 1, portion_act)) %>%
      mutate(source_size_act = source_size * portion_act) %>%
      mutate(upsilon_param = paste0("upsilon_", from, "_to_", to, "|", c)) %>%
      mutate(lambda_param = paste0("lambda_", from, "|", c)) %>%
      mutate(param = ifelse(is.na(to), lambda_param, upsilon_param)) %>%
      left_join(p %>% rename(param = name, param_value = value),
                by = "param") %>%
      mutate(expected_flow = source_size_act * param_value) %>%
      select(-portion_act, -upsilon_param, -lambda_param, -source_size, -param)
    cp <- unique(unlist(comps(m))) %>%
      enframe() %>%
      mutate(from = map_dbl(value, function(v) {
        f %>% filter(from == v) %>% pull(expected_flow) %>% sum
      })) %>%
      mutate(to = map_dbl(value, function(v) {
        f %>% filter(to == v) %>% pull(expected_flow) %>% sum
      }))
    cp[cp$value != "NH4", ]
  }

  for (i in seq_len(nrow(zf))) {
    x <- check_row_i(i)
    expect_true(all(x[["from"]] - x[["to"]] < 1e-12))
  }
})

### * Open network, single unit, one steady, one split

test_that("Predictions for open network, single unit, one steady, one split", {
  exp <- tibble::tribble(
    ~time.day,  ~species, ~biomass, ~prop15N,
    0,   "algae",     1.02,  0.00384,
    1,   "algae",       NA,   0.0534,
    1.5,   "algae",    0.951,       NA,
    2,   "algae",    0.889,   0.0849,
    2.5,   "algae",       NA,   0.0869,
    3,   "algae",    0.837,   0.0816,
    0, "daphnia",     1.74,  0.00464,
    1, "daphnia",       NA,  0.00493,
    1.5, "daphnia",     2.48,       NA,
    2, "daphnia",       NA,  0.00831,
    2.5, "daphnia",     2.25,       NA,
    3, "daphnia",     2.15,   0.0101,
    0,     "NH4",    0.208,     0.79,
    1,     "NH4",    0.227,       NA,
    1.5,     "NH4",       NA,    0.482,
    2,     "NH4",    0.256,    0.351,
    2.5,     "NH4",       NA,    0.295,
    3,     "NH4",     0.27,        NA
  )
  inits <- exp %>% dplyr::filter(time.day == 0)
  obs <- exp %>% dplyr::filter(time.day > 0)

  m <- new_networkModel() %>%
    set_topo("NH4 -> algae -> daphnia -> NH4") %>%
    set_steady("NH4") %>%
    set_split("algae") %>%
    set_init(inits, comp = "species", size = "biomass",
             prop = "prop15N") %>%
    set_obs(obs, comp = "species", size = "biomass",
            prop = "prop15N", time = "time.day")

  m <- m %>% 
    set_priors(normal_p(0, 0.5), "") %>%
    set_priors(constant_p(1), "lambda_NH4") %>%
    set_priors(uniform_p(0, 1), "portion")

  capture_warnings(capture_output({ r <- run_mcmc(m, iter = N_ITERS) }))

  ss <- tidy_steady_states(m, r)
  ff <- tidy_flows(m, r, steady_state = TRUE)
  
  # Check consistency
  zs <- ss %>%
    mutate(group_iter_label = pmap_chr(
      list(mcmc.chain, mcmc.iteration),
      function(c, i) {
        paste(c, i, sep = "|")
      }
    ))
  zf <- ff %>%
    mutate(group_iter_label = pmap_chr(
      list(mcmc.chain, mcmc.iteration),
      function(c, i) {
        paste(c, i, sep = "|")
      }
    ))
  zf <- left_join(zf, zs %>% select(group_iter_label, stable_sizes)) %>%
    select(group_iter_label, mcmc.parameters, stable_sizes, flows)

  check_row_i <- function(row_i) {
    f <- zf$flows[[row_i]]
    p <- enframe(zf$mcmc.parameters[[row_i]])
    s <- enframe(zf$stable_sizes[[row_i]]) %>%
      rename(from = name, source_size = value)
    f <- left_join(f, s, by = "from") %>%
      mutate(portion_param = paste0("portion.act_", from)) %>%
      left_join(p %>% rename(portion_param = name, portion_act = value),
                by = "portion_param") %>%
      mutate(portion_act = ifelse(is.na(portion_act), 1, portion_act)) %>%
      mutate(source_size_act = source_size * portion_act) %>%
      mutate(upsilon_param = paste0("upsilon_", from, "_to_", to)) %>%
      mutate(lambda_param = paste0("lambda_", from)) %>%
      mutate(param = ifelse(is.na(to), lambda_param, upsilon_param)) %>%
      left_join(p %>% rename(param = name, param_value = value),
                by = "param") %>%
      mutate(expected_flow = source_size_act * param_value) %>%
      select(-portion_act, -upsilon_param, -lambda_param, -source_size, -param)
    cp <- unique(unlist(comps(m))) %>%
      enframe() %>%
      mutate(from = map_dbl(value, function(v) {
        f %>% filter(from == v) %>% pull(expected_flow) %>% sum
      })) %>%
      mutate(to = map_dbl(value, function(v) {
        f %>% filter(to == v) %>% pull(expected_flow) %>% sum
      }))
    cp[cp$value != "NH4", ]
  }

  for (i in seq_len(nrow(zf))) {
    x <- check_row_i(i)
    expect_true(all(x[["from"]] - x[["to"]] < 1e-12))
  }

})
