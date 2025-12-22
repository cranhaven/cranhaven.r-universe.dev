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
library(regmedint)
library(tidyverse)
## Prepare dataset
data(vv2015)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
summary(regmedint_obj)
summary(regmedint_obj, exponentiate = TRUE)
summary(regmedint_obj, m_cde = 0, c_cond = 1)
summary(regmedint_obj, m_cde = 0, c_cond = 1, level = 0.99)

## -----------------------------------------------------------------------------
coef(regmedint_obj)
coef(regmedint_obj, m_cde = 0, c_cond = 1)

## -----------------------------------------------------------------------------
vcov(regmedint_obj)
vcov(regmedint_obj, m_cde = 0, c_cond = 1)

## -----------------------------------------------------------------------------
confint(regmedint_obj)
confint(regmedint_obj, m_cde = 0, c_cond = 1)
confint(regmedint_obj, m_cde = 0, c_cond = 1, level = 0.99)

## -----------------------------------------------------------------------------
regmedint_obj # Implicit printing
print(regmedint_obj)
print(regmedint_obj, m_cde = 0, c_cond = 1)

## -----------------------------------------------------------------------------
coef(summary(regmedint_obj))

## -----------------------------------------------------------------------------
regmedint_summary_obj <- summary(regmedint_obj)
regmedint_summary_obj # Implicit printing
print(regmedint_summary_obj)

