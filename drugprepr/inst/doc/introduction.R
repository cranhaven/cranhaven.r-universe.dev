## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----dataset1, echo=FALSE-----------------------------------------------------
library(drugprepr)
library(kableExtra)
kable(dataset1, booktabs = TRUE,
      caption = 'Prescription data for two fictional individuals. Stored in \\texttt{dataset1}') %>%
  kable_styling(latex_options = 'scale_down')

## ----install, eval=FALSE------------------------------------------------------
#  install.packages('remotes')
#  remotes::install_github("belayb/drugprepr")

## ----setup--------------------------------------------------------------------
library(drugprepr)

## ----ndd, echo = 1------------------------------------------------------------
data_ndd <- compute_ndd(dataset1, min, min)
data_ndd %>%
  subset(!duplicated(text),
         select = c(patid, pracid, prodcode, text, ndd)) %>%
  kable(booktabs = TRUE, row.names = FALSE, position = 'ht',
        caption = 'Sample output from the \\texttt{compute\\_ndd} function, selecting minimum frequency and minimum dosage number')

## ----min_max_dat, echo=FALSE--------------------------------------------------
kable(min_max_dat, booktabs = TRUE, position = 'ht',
      caption = 'Example data frame to supply to \\texttt{min\\_max\\_dat}')

## ----drugprep, echo = 1-------------------------------------------------------
result <- drug_prep(data_ndd,
                    min_max_dat,
                    decisions = c('b', 'b1', 'b', 'b1', 'b_6',
                                  'c', 'a', 'd', 'a', 'b_15'))
kable(result[, -1], booktabs = TRUE, position = 'ht',
      caption = 'Result of \\texttt{drug\\_prep()}') %>%
  kable_styling(latex_options = 'scale_down')

## -----------------------------------------------------------------------------
decisions <- make_decisions('ignore',
                            'mean population',
                            'missing',
                            'mean practice',
                            'truncate 6',
                            'qty / ndd',
                            'mean individual',
                            'mean',
                            'allow',
                            'close 15')
decisions
# drug_prep(example_therapy, plausible_values, decisions)

