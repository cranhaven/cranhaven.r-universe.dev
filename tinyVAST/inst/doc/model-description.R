## ----setup, echo=FALSE, cache=FALSE-------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = FALSE,
  error = FALSE,
  message = FALSE,
  warning = FALSE
)
# Install locally
#  devtools::install_local( R'(C:\Users\James.Thorson\Desktop\Git\tinyVAST)', force=TRUE )
# Build and PDF
#  setwd(R'(C:\Users\James.Thorson\Desktop\Git\tinyVAST)'); devtools::build_rmd("vignettes/model-description.Rmd"); rmarkdown::render( "vignettes/model-description.Rmd", rmarkdown::pdf_document())
#
# Recommended workflow:
#  * Open in Rstudio and knit using button there

## ----echo=TRUE, eval=FALSE----------------------------------------------------
# w1 -> w2, b_12
# w1 <-> w1, sd_1
# w2 <-> w2, sd_2

## ----arrow notation, echo=TRUE, eval=FALSE------------------------------------
# # Path, parameter_name, start value
# w1 -> w2, b_12, 0
# w2 -> w3, b_23, 0
# w1 <-> w1, s_1, 1
# w2 <-> w2, s_2, 1
# w3 <-> w3, s_3, 1

## ----echo=TRUE, eval=FALSE----------------------------------------------------
# w1 -> w1, 1, rho, 0.8

## ----echo=TRUE, eval=FALSE----------------------------------------------------
# count ~ year + offset(log_area) + s(depth) + s(species, bs="re")

## ----indices------------------------------------------------------------------
subscripts <- tibble::tribble(
  ~Symbol, ~Description,
  "$i$", "Index for each sample, $i$ in $(1,2,...,I)$",
  "$s[i]$", "spatial coordinate for sample $i$, $s$ in $(1,2,...,S)$",
  "$t[i]$", "time-interval for sample $i$, $t$ in $(1,2,...,T)$",
  "$c[i]$", "category for sample $i$, $c$ in $(1,2,...,C)$",
  "$e[i]$", "error distribution and link function for sample $i$"
)
knitr::kable(subscripts, caption = "Table 1: Subscript notation", escape = FALSE, booktabs = TRUE)

## ----symbols, results='asis'--------------------------------------------------
symbols <- tibble::tribble(
   ~Symbol, ~Code, ~Description,
  "$y$", "`y_i`", "Observed response data",
  "$p_1$", "`p_i`", "first linear predictor",
  "$p_2$", "`p2_i`", "second linear predictor",
)
knitr::kable(symbols, caption = "Table 2: Symbol notation, code representation (in model output or in model template code), and descriptions.", escape = FALSE, booktabs = TRUE,
   linesep = c(
    rep('', 7), # y, mean etc.
    '\\addlinespace',
    rep('', 7), # fields
    '\\addlinespace',
    rep('', 5), # covariance stuff
    '\\addlinespace',
    rep('', 5), # more covariance stuff
    '\\addlinespace',
    rep('', 99) # end
  ))

