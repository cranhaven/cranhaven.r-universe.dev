## ----include = FALSE------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(width = 100)
options(rmarkdown.html_vignette.check_title = FALSE)
options(marginaleffects_safe = FALSE)
modelsummary::config_modelsummary(startup_message = FALSE)

fixest::setFixest_notes(FALSE)

## -------------------------------------------------------------------------------------------------
# install.packages("did")
data("mpdta", package = "did")
head(mpdta)

## -------------------------------------------------------------------------------------------------
library(etwfe)

mod = etwfe(
  fml  = lemp ~ lpop, # outcome ~ controls
  tvar = year,        # time variable
  gvar = first.treat, # group variable
  data = mpdta,       # dataset
  vcov = ~countyreal  # vcov adjustment (here: clustered)
)

## -------------------------------------------------------------------------------------------------
mod

## -------------------------------------------------------------------------------------------------
emfx(mod)

## -------------------------------------------------------------------------------------------------
mod_es = emfx(mod, type = "event")
mod_es

## -------------------------------------------------------------------------------------------------
plot(mod_es)

## -------------------------------------------------------------------------------------------------
mod_es2 = etwfe(
  lemp ~ lpop, tvar = year, gvar = first.treat, data = mpdta, vcov = ~countyreal,
  cgroup = "never" # <= use never treated as control group
  ) |>
  emfx(type = "event")

plot(mod_es2)

## -------------------------------------------------------------------------------------------------
plot(
  mod_es2,
  type = "ribbon",
  col  = "darkcyan",
  xlab = "Years post treatment",
  main = "Minimum wage effect on (log) teen employment",
  sub  = "Note: Using never-treated as control group",
  # file = "event-study.png", width = 8, height = 5. ## uncomment to save file
)

## ----warning=FALSE, message=FALSE-----------------------------------------------------------------
# install.packages("ggplot2")
library(ggplot2)
theme_set(
  theme_linedraw() + theme(panel.grid = element_blank())
)
ggplot(mod_es2, aes(x = event, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, lty = 2, col = "grey50") +
  geom_vline(xintercept = -1, lty = 2, col = "grey50") +
  geom_pointrange(col = "darkcyan") +
  labs(
    x = "Years post treatment",
    y = "ATT",
    title = "Effect on log teen employment",
    caption = "Note: Using never-treated as control group"
  )

## -------------------------------------------------------------------------------------------------
# install.packages("modelsummary")
library(modelsummary)

# Quick renaming function to replace ".Dtreat" with something more meaningful
rename_fn = function(old_names) {
  new_names = gsub(".Dtreat", "Years post treatment =", old_names)
  setNames(new_names, old_names)
}

modelsummary(
  list(mod_es2, mod_es),
  shape       = term:event:statistic ~ model,
  coef_rename = rename_fn,
  gof_omit    = "Adj|Within|IC|RMSE",
  stars       = TRUE,
  title       = "Event study",
  notes       = "Std. errors are clustered at the county level"
)

## -------------------------------------------------------------------------------------------------
gls_fips = c("IL" = 17, "IN" = 18, "MI" = 26, "MN" = 27,
             "NY" = 36, "OH" = 39, "PA" = 42, "WI" = 55)

mpdta$gls = substr(mpdta$countyreal, 1, 2) %in% gls_fips

## -------------------------------------------------------------------------------------------------
hmod = etwfe(
  lemp ~ lpop, tvar = year, gvar = first.treat, data = mpdta,
  cgroup = "never",
  vcov = ~countyreal,
  xvar = gls           ## <= het. TEs by gls
)

# Simple heterogeneous ATTs (could also specify `type = "event"`, etc.) 
(hmod_mfx = emfx(hmod))

## ----warning=FALSE--------------------------------------------------------------------------------
emfx(hmod, hypothesis = "b1 = b2")

## -------------------------------------------------------------------------------------------------
plot(hmod_mfx)

## -------------------------------------------------------------------------------------------------
hmod |>
  emfx("event") |>
  plot(type = "ribbon")

## -------------------------------------------------------------------------------------------------
modelsummary(
  models      = list("GLS county" = hmod_mfx),
  shape       = term + statistic ~ model + gls, # add xvar variable (here: gls)
  coef_map    = c(".Dtreat" = "ATT"),
  gof_map     = NA,
  title       = "Comparing the ATT on GLS and non-GLS counties"
)

## ----warning=FALSE, message=FALSE-----------------------------------------------------------------
mpdta$emp = exp(mpdta$lemp)

etwfe(
  emp ~ lpop, tvar = year, gvar = first.treat, data = mpdta, vcov = ~countyreal,
  family = "poisson"
  ) |>
  emfx("event")

## -------------------------------------------------------------------------------------------------
# Step 0 already complete: using the same `mod` object from earlier...

# Step 1
emfx(mod, type = "event", vcov = FALSE)

# Step 2
emfx(mod, type = "event", vcov = FALSE, compress = TRUE)

# Step 3: Results from 1 and 2 are similar enough, so get approx. SEs
mod_es_compressed = emfx(mod, type = "event", compress = TRUE)

## -------------------------------------------------------------------------------------------------
modelsummary(
  list("Original" = mod_es, "Compressed" = mod_es_compressed),
  shape       = term:event:statistic ~ model,
  coef_rename = rename_fn,
  gof_omit    = "Adj|Within|IC|RMSE",
  title       = "Event study",
  notes       = "Std. errors are clustered at the county level"
)

## -------------------------------------------------------------------------------------------------
mod$fml_all

## -------------------------------------------------------------------------------------------------
# First construct the dataset
mpdta2 = mpdta |>
  transform(
    .Dtreat = year >= first.treat & first.treat != 0,
    lpop_dm = ave(lpop, first.treat, year, FUN = \(x) x - mean(x, na.rm = TRUE))
  )

# Then estimate the manual version of etwfe
mod2 = fixest::feols(
  lemp ~ .Dtreat:i(first.treat, i.year, ref = 0, ref2 = 2003) / lpop_dm |
    first.treat[lpop] + year[lpop],
  data = mpdta2,
  vcov = ~countyreal
)

## -------------------------------------------------------------------------------------------------
modelsummary(
  list("etwfe" = mod, "manual" = mod2),
  gof_map = NA # drop all goodness-of-fit info for brevity
)

## -------------------------------------------------------------------------------------------------
library(marginaleffects)

# Simple ATT
slopes(
  mod2, 
  newdata   = subset(mpdta2, .Dtreat), # we only want rows where .Dtreat is TRUE
  variables = ".Dtreat", 
  by        = ".Dtreat"
)

# Event study
slopes(
  mod2, 
  newdata   = transform(subset(mpdta2, .Dtreat), event = year - first.treat),
  variables = ".Dtreat", 
  by        = "event"
)

## ----message=FALSE, warning=FALSE-----------------------------------------------------------------
# fe = "feo" (fixed effects only)
mod_feo = etwfe(
  lemp ~ lpop, tvar = year, gvar = first.treat, data = mpdta, vcov = ~countyreal,
  fe = "feo"
)
# ... which is equivalent to the manual regression
mod_feo2 = fixest::feols(
  lemp ~ .Dtreat:i(first.treat, i.year, ref = 0, ref2 = 2003) / lpop_dm +
    lpop + i(first.treat, lpop, ref = 0) + i(year, lpop, ref = 2003) |
    first.treat + year,
  data = mpdta2, vcov = ~countyreal
)

# fe = "none"
mod_none = etwfe(
  lemp ~ lpop, tvar = year, gvar = first.treat, data = mpdta, vcov = ~countyreal,
  fe = "none"
)
# ... which is equivalent to the manual regression
mod_none2 = fixest::feols(
  lemp ~ .Dtreat:i(first.treat, i.year, ref = 0, ref2 = 2003) / lpop_dm +
    lpop + i(first.treat, lpop, ref = 0) + i(year, lpop, ref = 2003) +
    i(first.treat, ref = 0) + i(year, ref = 2003),
  data = mpdta2, vcov = ~countyreal
)

## -------------------------------------------------------------------------------------------------
mods = list(
  "etwfe"         = mod,
  "manual"        = mod2,
  "etwfe (feo)"   = mod_feo,
  "manual (feo)"  = mod_feo2,
  "etwfe (none)"  = mod_none,
  "manual (none)" = mod_none2
)

modelsummary(mods, gof_map = NA)

## -------------------------------------------------------------------------------------------------
mod_es_i = etwfe(
  lemp ~ lpop, tvar = year, gvar = first.treat, data = mpdta,
  ivar = countyreal  # NEW: Use unit-level (county) FEs
  ) |>
  emfx("event")

modelsummary(
  list("Group-level FEs (default)" = mod_es, "Unit-level FEs" = mod_es_i),
  shape       = term:event:statistic ~ model,
  coef_rename = rename_fn,
  gof_omit    = "Adj|Within|IC|RMSE"
)

