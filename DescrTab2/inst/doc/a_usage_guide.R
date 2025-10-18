## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=TRUE---------------------------------------------------------------
library(DescrTab2)

## -----------------------------------------------------------------------------
library(dplyr, warn.conflicts = FALSE)
library(forcats)
set.seed(123)
dat <- iris[, c("Species", "Sepal.Length")] %>%
  mutate(animal = c("Mammal", "Fish") %>% rep(75) %>% factor()) %>%
  mutate(food = c("fries", "wedges") %>% sample(150, TRUE) %>% factor())
head(dat)

## -----------------------------------------------------------------------------
descr(dat)

## -----------------------------------------------------------------------------
my_table <- descr(dat)

## -----------------------------------------------------------------------------
my_table$variables$Sepal.Length$results$Total$mean

## -----------------------------------------------------------------------------
my_table <- descr(dat) %>% print(silent=TRUE)

## -----------------------------------------------------------------------------
descr(dat, "Species")

## -----------------------------------------------------------------------------
descr(dat, "Species", group_labels=list(setosa="My custom group label"), var_labels = list(Sepal.Length = "My custom variable label"))

## -----------------------------------------------------------------------------
descr(dat, "Species", format_options = list(caption="Description of our example dataset."))

## -----------------------------------------------------------------------------
descr(dat, "animal")

## -----------------------------------------------------------------------------
descr(dat %>% select(-"Species"), "animal", test_options = list(exact=TRUE, nonparametric=TRUE))

## -----------------------------------------------------------------------------
descr(dat %>% select(c("Species", "Sepal.Length")), "Species", test_options = list(nonparametric=TRUE))

## -----------------------------------------------------------------------------
descr(dat %>% mutate(animal = fct_recode(animal, Before="Fish", After="Mammal")) %>% select(-"Species"), "animal", test_options = list(paired=TRUE, indices=rep(1:75, each=2)))

descr(dat %>% mutate(animal = fct_recode(animal, Before="Fish", After="Mammal"), idx = rep(1:75, each=2)) %>% select(-"Species"), "animal", test_options = list(paired=TRUE, indices="idx" ))

## -----------------------------------------------------------------------------
descr(dat, "Species", format_summary_stats = list(mean=function(x)formatC(x, digits = 4)) )

## -----------------------------------------------------------------------------
descr(dat, "Species", summary_stats_cont = list(N = DescrTab2:::.N, Nmiss = DescrTab2:::.Nmiss, mean =
    DescrTab2:::.mean, sd = DescrTab2:::.sd, median = DescrTab2:::.median, min = DescrTab2:::.min, max =
    DescrTab2:::.max))

## -----------------------------------------------------------------------------
# Create example dataset
dat2 <- iris
dat2$cat_var <- c(1,2) %>% sample(150, TRUE) %>% factor()
dat2 <- dat2[, c("Species", "cat_var")]

descr(dat2, "Species", summary_stats_cat=list(mean=DescrTab2:::.factormean))


## -----------------------------------------------------------------------------
descr(dat, "Species", format_options = c(combine_mean_sd=TRUE))

## -----------------------------------------------------------------------------
descr(dat, "animal", format_options = list(print_p = FALSE))

## -----------------------------------------------------------------------------
descr(dat, "animal", format_options = list(print_CI = FALSE))

## -----------------------------------------------------------------------------
descr(iris, "Species", var_options = list(Sepal.Length = list(
  format_summary_stats = list(
    mean = function(x)
      formatC(x, digits = 4)
  ),
  test_options = c(nonparametric = TRUE)
)))

## -----------------------------------------------------------------------------
custom_ttest <- list(
  name = "custom t-test",
  abbreviation = "ct",
  p = function(var) {
    return(t.test(var, alternative = "greater")$p.value)
  }
)

descr(iris %>% select(-Species), test_options = list(test_override = custom_ttest))

## -----------------------------------------------------------------------------
descr(factor(c("a", "b")), format_options=list(omit_factor_level = "last"))

## -----------------------------------------------------------------------------
summary_stats_cat <- list(
  CIL = DescrTab2:::.factor_firstlevel_CIlower,
  CIU = DescrTab2:::.factor_firstlevel_CIupper)

summary_stats_cont  <-  list(
  N = DescrTab2:::.N,
  Nmiss = DescrTab2:::.Nmiss,
  mean = DescrTab2:::.mean,
  sd = DescrTab2:::.sd,
  CILM = DescrTab2:::.meanCIlower,
  CIUM = DescrTab2:::.meanCIupper)

format_summary_stats <- list(
  CIL = scales::label_percent(),
  CIU = scales::label_percent(),
  CILM = function(x) format(x, digits = 2, scientific = 3),
  CIUM = function(x) format(x, digits = 2, scientific = 3),
  N = function(x) {
    format(x, digits = 2, scientific = 3)
  },
  mean = function(x) {
    format(x, digits = 2, scientific = 3)
  },
  sd = function(x) {
    format(x, digits = 2, scientific = 3)
  },
  CI = function(x) {
    format(x, digits = 2, scientific = 3)
  }
)

reshape_rows <- list(
  `CI` = list(
    args = c("CIL", "CIU"),
    fun = function(CIL, CIU) {
      paste0("[", CIL, ", ", CIU, "]")
    }
  ),
  `CI` = list(
    args = c("CILM", "CIUM"),
    fun = function(CILM, CIUM) {
      paste0("[", CILM, ", ", CIUM, "]")
    }
  )
)

set.seed(123)
dat <- tibble(a_factor = factor(c(rep("a", 70), rep("b", 30))),
              a_numeric = rnorm(100),
              group = sample(c("Trt", "Ctrl"), 100, TRUE)
)

descr(dat, "group",
  format_options=list(omit_factor_level = "last",
  categories_first_summary_stats_second = FALSE,
  combine_mean_sd = TRUE
  ),
  summary_stats_cat = summary_stats_cat,
  summary_stats_cont = summary_stats_cont,
  reshape_rows = reshape_rows,
  format_summary_stats = format_summary_stats)

