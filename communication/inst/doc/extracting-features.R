## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(communication)

## ---- eval = FALSE------------------------------------------------------------
#  ## extract features
#  wav.fnames = list.files(file.path('PATH_TO_YOUR_DIRECTORY'),
#                          pattern = 'wav$',
#                          recursive = TRUE,
#                          full.names = TRUE
#                          )
#  audio <- extractAudioFeatures(wav.fnames = wav.fnames,
#                                derivatives = 0
#                                )

## ---- eval = FALSE------------------------------------------------------------
#  ## standardize full training set together
#  audio$data <- standardizeFeatures(
#      lapply(audio$data, function(x) na.omit(x))
#      )

## ---- eval = FALSE------------------------------------------------------------
#  mod <- hmm(audio$data,
#      nstates = 2,
#      control = list(verbose = TRUE)
#      )

