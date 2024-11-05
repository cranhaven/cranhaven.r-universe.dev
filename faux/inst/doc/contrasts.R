## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%",
  fig.width = 8,
  fig.height = 4
)

# run a simple analysis 
ctable <- function(df, predictor = "pet") {
  a <- paste("y ~", predictor) %>%
    formula() %>%
    lm(df)
  
  contrast_table <- kable(contrasts(df[[predictor]]), format = "html") %>% 
    kable_styling()
  
  analysis_table <- kable(broom::tidy(a), format = "html") %>% 
    kable_styling()
  
  paste(
    "<table class='ctable table'>",
    "<thead><tr><th>Contrasts</th><th>lm(y ~ pet, df)</th></thead>",
    "<tbody><tr><td>",
    contrast_table,
    "</td><td>",
    analysis_table,
    "</td></tr></tbody></table>",
    sep = "\n"
  )
}

options(digits = 3)
set.seed(8675309)

library(faux)
library(kableExtra)

## -----------------------------------------------------------------------------
df <- sim_design(between = list(pet = c("cat", "dog", "ferret")), 
                 n = c(50), mu = c(2, 4, 9), empirical = TRUE)

## -----------------------------------------------------------------------------
df$pet <- contr_code_treatment(df$pet)

## -----------------------------------------------------------------------------
df$pet <- contr_code_treatment(df$pet, base = "dog")

## -----------------------------------------------------------------------------
df$pet <- contr_code_treatment(df$pet, base = 3)

## -----------------------------------------------------------------------------
df$pet <- contr_code_anova(df$pet)

## -----------------------------------------------------------------------------
df$pet <- contr_code_anova(df$pet, base = "dog")

## -----------------------------------------------------------------------------
df$pet <- contr_code_anova(df$pet, base = 3)

## -----------------------------------------------------------------------------
df$pet <- contr_code_sum(df$pet)

## -----------------------------------------------------------------------------
df$pet <- contr_code_sum(df$pet, omit = "dog")

## -----------------------------------------------------------------------------
df$pet <- contr_code_sum(df$pet, omit = 1)

## -----------------------------------------------------------------------------
df$pet <- contr_code_difference(df$pet)

## -----------------------------------------------------------------------------
df$pet <- contr_code_difference(df$pet, levels = c("ferret", "cat", "dog"))

## ---- include = FALSE---------------------------------------------------------
# get levels back to normal
df$pet <- contr_code_difference(df$pet, levels = c("cat", "dog", "ferret"))

## -----------------------------------------------------------------------------
df$pet <- contr_code_helmert(df$pet)

## -----------------------------------------------------------------------------
df$pet <- contr_code_helmert(df$pet, levels = c("ferret", "dog", "cat"))

## -----------------------------------------------------------------------------
df <- sim_design(list(time = 1:5),
                 mu = 1:5 * 0.25 + (1:5 - 3)^2 * 0.5,
                 sd = 5, long = TRUE)

## -----------------------------------------------------------------------------
df$time <- contr_code_poly(df$time)

## -----------------------------------------------------------------------------
df <- sim_design(list(time = 1:5),
                 mu = 1:5 * 0.25 + (1:5 - 3)^2 * 0.5,
                 sd = 5, long = TRUE, plot = FALSE) %>%
  add_contrast("time", "poly")

## ---- results='hide'----------------------------------------------------------
# test only the linear and quadratic contrasts
lm(y ~ `time^1` + `time^2`, df) %>% broom::tidy()

## ---- echo = FALSE------------------------------------------------------------
lm(y ~ `time^1` + `time^2`, df) %>% 
  broom::tidy() %>%
  knitr::kable() %>% 
  kable_styling()

## -----------------------------------------------------------------------------
btwn <- list(condition = c("control", "experimental")) 

df <- sim_design(between = btwn, n = 1, plot = FALSE) %>%
  add_contrast("condition", "treatment", colnames = "cond.tr") %>%
  add_contrast("condition", "anova", colnames = "cond.aov") %>%
  add_contrast("condition", "difference", colnames = "cond.dif") %>%
  add_contrast("condition", "sum", colnames = "cond.sum") %>%
  add_contrast("condition", "helmert", colnames = "cond.hmt") %>%
  add_contrast("condition", "poly", colnames = "cond.poly")

## -----------------------------------------------------------------------------
btwn <- list(pet = c("cat", "dog", "ferret")) 

df <- sim_design(between = btwn, n = 1, plot = FALSE) %>%
  add_contrast("pet", "treatment") %>% 
  add_contrast("pet", "anova") %>%
  add_contrast("pet", "sum") %>%
  add_contrast("pet", "difference") %>%
  add_contrast("pet", "helmert") %>%
  add_contrast("pet", "poly")

## -----------------------------------------------------------------------------
mu <- c(0, 4, 6, 10)
df <- sim_design(between = list(time = c("am", "pm"),
                                pet = c("cat", "dog")),
                 n = c(50, 60, 70, 80), mu = mu, empirical = TRUE)

## ---- echo = FALSE------------------------------------------------------------

Yca = mu[[1]]
Yda = mu[[2]]
Ycp = mu[[3]]
Ydp = mu[[4]]
Yc. = ((Yca + Ycp)/2) %>% round(2)
Yd. = ((Yda + Ydp)/2) %>% round(2)
Y.a = ((Yca + Yda)/2) %>% round(2)
Y.p = ((Ycp + Ydp)/2) %>% round(2)
Y.. = ((Yca + Yda + Ycp + Ydp)/4) %>% round(2)


## -----------------------------------------------------------------------------
df %>%
  add_contrast("pet", "treatment") %>%
  add_contrast("time", "treatment") %>%
  lm(y ~ pet * time, .) %>% 
  broom::tidy() %>% kable() %>% kable_styling()

## -----------------------------------------------------------------------------
df %>%
  add_contrast("pet", "anova") %>%
  add_contrast("time", "anova") %>%
  lm(y ~ pet * time, .) %>% 
  broom::tidy() %>% kable() %>% kable_styling()

## -----------------------------------------------------------------------------
df %>%
  add_contrast("pet", "sum") %>%
  add_contrast("time", "sum") %>%
  lm(y ~ pet * time, .) %>%  
  broom::tidy() %>% kable() %>% kable_styling()

## -----------------------------------------------------------------------------
df %>%
  add_contrast("pet", "difference") %>%
  add_contrast("time", "difference") %>%
  lm(y ~ pet * time, .) %>% 
  broom::tidy() %>% kable() %>% kable_styling()

## -----------------------------------------------------------------------------
df %>%
  add_contrast("pet", "helmert") %>%
  add_contrast("time", "helmert") %>%
  lm(y ~ pet * time, .) %>% 
  broom::tidy() %>% kable() %>% kable_styling()

## -----------------------------------------------------------------------------
mu <- c(0, 5, 7, 6, 2, 1)
df <- sim_design(between = list(time = c("am", "pm"),
                                pet = c("cat", "dog", "ferret")),
                 n = c(50, 60, 70, 80, 90, 100), mu = mu, empirical = TRUE)

## ---- echo = FALSE------------------------------------------------------------

Yca = mu[[1]]
Yda = mu[[2]]
Yfa = mu[[3]]
Ycp = mu[[4]]
Ydp = mu[[5]]
Yfp = mu[[6]]
Yc. = ((Yca + Ycp)/2) %>% round(2)
Yd. = ((Yda + Ydp)/2) %>% round(2)
Yf. = ((Yfa + Yfp)/2) %>% round(2)
Y.a = ((Yca + Yda + Yfa)/3) %>% round(2)
Y.p = ((Ycp + Ydp + Yfp)/3) %>% round(2)
Y.. = ((Yca + Yda + Yfa + Ycp + Ydp + Yfp)/6) %>% round(2)


## -----------------------------------------------------------------------------
df %>%
  add_contrast("pet", "treatment") %>%
  add_contrast("time", "treatment") %>%
  lm(y ~ pet * time, .) %>% 
  broom::tidy() %>% kable() %>% kable_styling()

## -----------------------------------------------------------------------------
df %>%
  add_contrast("pet", "anova") %>%
  add_contrast("time", "anova") %>%
  lm(y ~ pet * time, .) %>% 
  broom::tidy() %>% kable() %>% kable_styling()

## -----------------------------------------------------------------------------
df %>%
  add_contrast("pet", "sum") %>%
  add_contrast("time", "sum") %>%
  lm(y ~ pet * time, .) %>% 
  broom::tidy() %>% kable() %>% kable_styling()

## -----------------------------------------------------------------------------
df %>%
  add_contrast("pet", "difference") %>%
  add_contrast("time", "difference") %>%
  lm(y ~ pet * time, .) %>% 
  broom::tidy() %>% kable() %>% kable_styling()

## -----------------------------------------------------------------------------
df %>%
  add_contrast("pet", "helmert") %>%
  add_contrast("time", "helmert") %>%
  lm(y ~ pet * time, .) %>% 
  broom::tidy() %>% kable() %>% kable_styling()

