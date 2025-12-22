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
regmedint_obj1 <- regmedint(data = vv2015,
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
                            c_cond = 3,
                            ## Model types
                            mreg = "logistic",
                            yreg = "survAFT_weibull",
                            ## Additional specification
                            interaction = TRUE,
                            casecontrol = FALSE)
summary(regmedint_obj1)

## -----------------------------------------------------------------------------
regmedint_obj2 <- regmedint(data = vv2015,
                            ## Variables
                            yvar = "y",
                            avar = "x",
                            mvar = "m",
                            cvar = c("c"),
                            emm_ac_mreg = c("c"),
                            emm_ac_yreg = NULL,
                            emm_mc_yreg = NULL,
                            eventvar = "event",
                            ## Values at which effects are evaluated
                            a0 = 0,
                            a1 = 1,
                            m_cde = 1,
                            c_cond = 3,
                            ## Model types
                            mreg = "logistic",
                            yreg = "survAFT_weibull",
                            ## Additional specification
                            interaction = TRUE,
                            casecontrol = FALSE)
summary(regmedint_obj2)

## -----------------------------------------------------------------------------
regmedint_obj3 <- regmedint(data = vv2015,
                            ## Variables
                            yvar = "y",
                            avar = "x",
                            mvar = "m",
                            cvar = c("c"),
                            emm_ac_mreg = c("c"),
                            emm_ac_yreg = c("c"),
                            emm_mc_yreg = NULL,
                            eventvar = "event",
                            ## Values at which effects are evaluated
                            a0 = 0,
                            a1 = 1,
                            m_cde = 1,
                            c_cond = 3,
                            ## Model types
                            mreg = "logistic",
                            yreg = "survAFT_weibull",
                            ## Additional specification
                            interaction = TRUE,
                            casecontrol = FALSE)
summary(regmedint_obj3)

## -----------------------------------------------------------------------------
regmedint_obj4 <- regmedint(data = vv2015,
                            ## Variables
                            yvar = "y",
                            avar = "x",
                            mvar = "m",
                            cvar = c("c"),
                            emm_ac_mreg = c("c"),
                            emm_ac_yreg = c("c"),
                            emm_mc_yreg = c("c"),
                            eventvar = "event",
                            ## Values at which effects are evaluated
                            a0 = 0,
                            a1 = 1,
                            m_cde = 1,
                            c_cond = 3,
                            ## Model types
                            mreg = "logistic",
                            yreg = "survAFT_weibull",
                            ## Additional specification
                            interaction = TRUE,
                            casecontrol = FALSE)
summary(regmedint_obj4)

