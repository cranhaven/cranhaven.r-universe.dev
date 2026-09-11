data(valentesnsList)
mymodel <- new_defm(
  id = valentesnsList$id,
  Y = valentesnsList$Y,
  X = valentesnsList$X,
  order = 0
)

td_logit_intercept(mymodel)
td_logit_intercept(mymodel, covar = "Hispanic")
init_defm(mymodel)

ans <- defm_mle(mymodel)

# Checking texreg fancy
expect_stdout({
  print(texreg_fancy(
    ans, texreg::screenreg,
    skip_intercept = TRUE
    ))
}, "Model 1")

expect_stdout({
  print(summary_table(ans))
  },
  "pvalues"
)

# Fitting individual models for each outcome
ans_alcohol <- glm(
  alcohol ~ Hispanic, data = valentesns,
  family = binomial(link = "logit")
)

ans_tobacco <- glm(
  tobacco ~ Hispanic, data = valentesns,
  family = binomial(link = "logit")
)

ans_mj <- glm(
  mj ~ Hispanic, data = valentesns,
  family = binomial(link = "logit")
)


coefs_defm <- coef(ans)
get_coef <- function(x, regex) {
  x[grepl(regex, names(x))]
}


# For alcohol
expect_equivalent(
  get_coef(coefs_defm, "alcohol"),
  coef(ans_alcohol),
  tolerance = 1e-4
)

# For tobacco
expect_equivalent(
  get_coef(coefs_defm, "tobacco"),
  coef(ans_tobacco),
  tolerance = 1e-4
)

# For marijuana
expect_equivalent(
  get_coef(coefs_defm, "mj"),
  coef(ans_mj),
  tolerance = 1e-4
)

# td_formula(mymodel, "{y1, 0y2} > {y1, y2}")

# motif_census(mymodel, locs = 0:1)

# mymodel_cs <- new_defm(
#   id = valentesnsList$id,
#   Y = valentesnsList$Y,
#   X = valentesnsList$X,
#   order = 0
# )

# motif_census(mymodel_cs, locs = 0:1)

mymodel <- new_defm(
  id = valentesnsList$id,
  Y = valentesnsList$Y,
  X = valentesnsList$X,
  order = 1
)

td_logit_intercept(mymodel)
td_formula(mymodel, "{y1, 0y2} > {y1, y2}", new_name = "y1y2")
init_defm(mymodel)

ans0 <- defm_mle(mymodel)

# Now, recreating the model using matrices
mymodel <- new_defm(
  id = valentesnsList$id,
  Y = valentesnsList$Y,
  X = valentesnsList$X,
  order = 1
)

td_logit_intercept(mymodel)
td_generic(mymodel, 
  matrix(c(NA, 1, 0, NA, 1, 1), nrow = 2, byrow = TRUE)
)

set_counters_names(mymodel, "3" = "y1y2")
init_defm(mymodel)

ans1 <- defm_mle(mymodel)

expect_equivalent(coef(ans0), coef(ans1))
