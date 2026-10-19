## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(uqsa)
library(errors)

## -----------------------------------------------------------------------------
x <- parse_concise(
	c(
		"1.23(1)","1.23(1)e2","-1.234(1)e-6", # concise notation
		"1.23;0.01", "1.23±0.01"              # fall-back
	)
)
print(class(x))
print(as.data.frame(x)) # this looks better as a data-frame

## ----data---------------------------------------------------------------------
m <- model_from_tsv(uqsa_example("AKAP79"))
print(m$Reaction[,c(2,3,4)])           # an example table, the reactions
print(head(m$Experiment))   # the list of experiments

## ----label="data-and-output"--------------------------------------------------
print(m$Output)
rn <- rownames(m$Experiment)
print(head(m[[rn[18]]]))    # experiment 18, data found by name

## ----label="data-matrix"------------------------------------------------------
x <- experiments(m)
print(x[[18]]$data[,seq(8),drop=FALSE])     # a sub-set of the data-matrix

## ----measurements-------------------------------------------------------------
print(head(m[[rn[18]]],12))         # these two should be the same
print(head(x[[18]]$measurement,12)) # (a 12 row subset)

