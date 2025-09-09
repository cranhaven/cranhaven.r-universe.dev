# Generate sample dataset `df_oes`

N <- 10000
set.seed(38)
df_oes <- tibble(
  x1 = rbinom(n = N, size = 1, prob = .5),
  x2 = sample(0:3, size = N, replace = T),
  y1 = rbinom(
    n = N,
    size = 1,
    prob = .1 + .05 * x1
  ),
  y2 = rbinom(
    n = N,
    size = 1,
    prob = .1 + 0 * (x2 == 1) - .025 * (x2 == 2) + 0.05 * (x2 == 3)
  )
) %>%
  mutate(x2 = as.factor(x2)) %>%
  mutate(
    z1 = rnorm(n = n()),
    z2 = rbinom(n = n(), size = 1, prob = .5),
    z3 = rbinom(n = n(), size = 5, prob = .5) %>%
      as.factor
  )
