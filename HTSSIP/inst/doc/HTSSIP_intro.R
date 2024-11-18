## ---- message=FALSE, warning=FALSE---------------------------------------
library(dplyr)
library(HTSSIP)
library(phyloseq)

## ---- message=FALSE, warning=FALSE---------------------------------------
physeq_S2D2

## ------------------------------------------------------------------------
physeq_S2D2 %>% sample_data %>% .[1:4,1:10]

## ------------------------------------------------------------------------
# Our phyloseq object should be formatted correctly (no errors)
physeq_S2D2 = physeq_format(physeq_S2D2)
physeq_S2D2 %>% sample_data %>% .[1:4,1:10]

# This phyloseq object should NOT be formatted correctly 
data(GlobalPatterns)
GlobalPatterns %>% sample_data %>% .[1:4,1:ncol(.)]
tryCatch(
  physeq_format(GlobalPatterns),
  error = function(e) e
  )

## ------------------------------------------------------------------------
m = sample_data(physeq_S2D2)

physeq_13C.Glu_D3 = prune_samples((m$Substrate=='12C-Con' & m$Day==3) | (m$Substrate=='13C-Glu' & m$Day==3), physeq_S2D2)
physeq_13C.Glu_D3

physeq_13C.Cel_D14 = prune_samples((m$Substrate=='12C-Con' & m$Day==3) | (m$Substrate=='13C-Cel' & m$Day==14), physeq_S2D2)
physeq_13C.Cel_D14

## ------------------------------------------------------------------------
ex = "(Substrate=='12C-Con' & Day=='${Day}') | (Substrate=='${Substrate}' & Day == '${Day}')"

## ------------------------------------------------------------------------
params = get_treatment_params(physeq_S2D2, c('Substrate', 'Day'), "Substrate != '12C-Con'")
params

## ------------------------------------------------------------------------
physeq_S2D2_l = phyloseq_subset(physeq_S2D2, params, ex)
physeq_S2D2_l

## ------------------------------------------------------------------------
sessionInfo()

