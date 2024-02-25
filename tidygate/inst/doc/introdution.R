## ----echo=FALSE, include=FALSE------------------------------------------------
library(knitr)
knitr::opts_chunk$set(cache = TRUE, warning = FALSE,
                      message = FALSE, cache.lazy = FALSE)

library(dplyr)
library(tidygate)


## ----eval = FALSE-------------------------------------------------------------
#  tidygate_gate <-
#    tidygate_data %>%
#    mutate( gate = gate_chr( Dim1, Dim2 ) )
#  
#  
#  tidygate_gate

## ----echo = FALSE-------------------------------------------------------------
my_gates = tidygate::gate_list

my_gates

## -----------------------------------------------------------------------------

tidygate_data %>%
  mutate( gate = gate_chr(
    Dim1, Dim2,
     # Pre-defined gates
    gate_list = my_gates
  ))


