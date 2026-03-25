## Load packages ---------------------------------------------------------------

library(tidyverse)
devtools::load_all()

## Simulated Example -----------------------------------------------------------
set.seed(1)
n_repeticoes <- 1

simular <- function() {
  d_sim <- ltm_sim(
    ns = 500, nk = 2, ni = 1,
    vmu = matrix(c(.5,.5), nrow = 2),
    mPhi = diag(2) * c(.99, .99),
    mSigs = c(.1,.1),
    dsig = .15,
    vd = matrix(c(.4,.4), nrow = 2),
    alpha = 0
  )
  binder <- array(runif(500)-.5, c(1, 500, 1))
  d_sim$mx <- abind::abind(d_sim$mx, binder, along = 3)
  d_sim$mb <- cbind(d_sim$mb, 0)
  d_sim
}

simulacoes <- tibble(
  id = seq_len(n_repeticoes),
  sim = rerun(n_repeticoes, simular())
)

ajustar <- function(i) {
  d_sim <- with(simulacoes, sim[[i]])
  res <- ltm_mcmc(d_sim$mx, d_sim$vy, burnin = 1000, iter = 5000, K = 3)
  tibble(id = i, data = list(d_sim), result = list(res))
}

res <- ajustar(1)

# ------------------------------------------------------------------------------
a <- res$result[[1]]
a <- res

# tabelas resumo ---------------------------------------------------------------
# deixei explicito para ficar facil de editar, caso queira

true <- tibble::tribble(
  ~key, ~p, ~true_value,
  "d", 1, .4,
  "d", 2, .4,
  "d", 3, .4,
  "mu", 1, .5,
  "mu", 2, .5,
  "mu", 3, .0,
  "phi", 1, .99,
  "phi", 2, .99,
  "phi", 3, .99,
  "sig_eta", 1, .10,
  "sig_eta", 2, .10,
  "sig_eta", 3, .10,
  "sig", 1, .15,
  "alpha", 1, 0
)

tabelas <- a[,!str_detect(colnames(a), "beta\\[")] %>%
  as.data.frame() %>%
  tibble::as_tibble() %>%
  tibble::rowid_to_column() %>%
  tidyr::gather(key, val, -rowid) %>%
  dplyr::group_by(key) %>%
  dplyr::summarise(
    mediana = median(val),
    sd = sd(val),
    q025 = quantile(val, 0.025),
    q975 = quantile(val, 0.975)
  ) %>%
  dplyr::mutate(
    p = as.numeric(stringr::str_extract(key, "[0-9]+(?=\\])")),
    key = stringr::str_extract(key, "[a-z_]+"),
    is_alpha = stringr::str_detect(key, "alpha|sig$")
  ) %>%
  dplyr::arrange(key, p) %>%
  dplyr::select(key, p, dplyr::everything()) %>%
  # dplyr::inner_join(true, c("key", "p")) %>%
  dplyr::group_split(is_alpha, keep = FALSE)

knitr::kable(tabelas[[1]])
knitr::kable(tabelas[[2]])

# graficos betas ---------------------------------------------------------------
grafico_betas(a, 1, real_values = res$data[[1]])
grafico_betas(a, 1:2, real_values = res$data[[1]])
grafico_betas(a, 1:3, real_values = res$data[[1]])

# MCMC traces ------------------------------------------------------------------
# colocar os indices ao lado do \\[ se desejar pegar algum especifico
bayesplot::mcmc_trace(a, regex_pars = "alpha\\[")
bayesplot::mcmc_trace(a, regex_pars = "phi\\[")
bayesplot::mcmc_trace(a, regex_pars = "d\\[")
bayesplot::mcmc_trace(a, regex_pars = "mu\\[")
bayesplot::mcmc_trace(a, regex_pars = "sig_eta\\[")
bayesplot::mcmc_trace(a, regex_pars = "sig\\[")
