## ----message = FALSE, tidy = FALSE, echo = F----------------------------------
## knitr configuration: http://yihui.name/knitr/options#chunk_options
library(knitr)
showMessage <- FALSE
showWarning <- TRUE
set_alias(w = "fig.width", h = "fig.height", res = "results")
opts_chunk$set(comment = "##", error= TRUE, warning = showWarning, message = showMessage,
               tidy = FALSE, cache = FALSE, echo = TRUE,
               fig.width = 7, fig.height = 7,
               fig.path = "man/figures")

## -----------------------------------------------------------------------------
set.seed(138087069)
library(regmedint)
library(tidyverse)
## Prepare dataset
data(vv2015)
## Main fit
regmedint_obj <- regmedint(data = vv2015,
                           ## Variables
                           yvar = "y",
                           avar = "x",
                           mvar = "m",
                           cvar = c("c"),
                           eventvar = "event",
                           ## Values at which effects are evaluated
                           a0 = 0,
                           a1 = 1,
                           m_cde = 1,
                           c_cond = 0.5,
                           ## Model types
                           mreg = "logistic",
                           yreg = "survAFT_weibull",
                           ## Additional specification
                           interaction = TRUE,
                           casecontrol = FALSE)
coef(summary(regmedint_obj))

## -----------------------------------------------------------------------------
library(boot)
## Define a wrapper function
regmedint_boot <- function(data, ind)  {
    ## Note the change in the data argument.
    regmedint_obj <- regmedint(data = data[ind,],
                               ## Variables
                               yvar = "y",
                               avar = "x",
                               mvar = "m",
                               cvar = c("c"),
                               eventvar = "event",
                               ## Values at which effects are evaluated
                               a0 = 0,
                               a1 = 1,
                               m_cde = 1,
                               c_cond = 0.5,
                               ## Model types
                               mreg = "logistic",
                               yreg = "survAFT_weibull",
                               ## Additional specification
                               interaction = TRUE,
                               casecontrol = FALSE)
    coef(regmedint_obj)
}
## Run bootstrapping
ncpus <- 1
## For parallization, use the following instead.
## ncpus <- parallel::detectCores()
boot_obj <- boot(data = vv2015, statistic = regmedint_boot, R = 1000,
                 ## For palatalization
                 ## See https://cran.r-project.org/package=boot
                 parallel = "multicore",
                 ncpus = ncpus)
## Confidence interval for the pm
boot.ci(boot_obj, type = "basic", index = 7)

## -----------------------------------------------------------------------------
library(modelr)

library(future)
future::plan(sequential)
## For parallization, use the following instead.
## future::plan(multiprocess)
library(furrr)

## Bootstrapping
tib_obj <- vv2015 %>%
    modelr::bootstrap(n = 1000) %>%
    ## Resampled data objects are in the list column named strap.
    mutate(boot_fit = future_map(strap, function(strap) {
        ## Note the change in the data argument.
        regmedint_obj <- regmedint(data = as_tibble(strap),
                                   ## Variables
                                   yvar = "y",
                                   avar = "x",
                                   mvar = "m",
                                   cvar = c("c"),
                                   eventvar = "event",
                                   ## Values at which effects are evaluated
                                   a0 = 0,
                                   a1 = 1,
                                   m_cde = 1,
                                   c_cond = 0.5,
                                   ## Model types
                                   mreg = "logistic",
                                   yreg = "survAFT_weibull",
                                   ## Additional specification
                                   interaction = TRUE,
                                   casecontrol = FALSE)
        ## Trick to return a row tibble
        mat <- t(matrix(coef(regmedint_obj)))
        colnames(mat) <- names(coef(regmedint_obj))
        as_tibble(mat)
    })) %>%
    select(-strap) %>%
    unnest(cols = c(boot_fit))

tib_obj2 <- tib_obj %>%
    pivot_longer(-.id) %>%
    mutate(name = factor(name, levels = names(coef(regmedint_obj)))) %>%
    group_by(name) %>%
    summarize(lower_boot = quantile(value, probs = 0.025),
              upper_boot = quantile(value, probs = 0.975))
tib_obj2

## -----------------------------------------------------------------------------
tib_obj2 %>%
    mutate(lower_delta = confint(regmedint_obj)[,"lower"],
           upper_delta = confint(regmedint_obj)[,"upper"])

