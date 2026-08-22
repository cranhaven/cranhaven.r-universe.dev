## ----include = FALSE----------------------------------------------------------
has_dsem = requireNamespace("dsem", quietly = TRUE)
EVAL <- has_dsem
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = EVAL,
  purl = EVAL
)
start_time = Sys.time()
# Install locally
#  devtools::install_local( R'(C:\Users\James.Thorson\Desktop\Git\tinyVAST)', force=TRUE )
# Build and PDF
#  setwd(R'(C:\Users\James.Thorson\Desktop\Git\tinyVAST)'); devtools::build_rmd("vignettes/dsem.Rmd"); rmarkdown::render( "vignettes/dsem.Rmd", rmarkdown::pdf_document())

## ----setup, echo=TRUE, warning=FALSE, message=FALSE---------------------------
library(tinyVAST)
set.seed(101)
options("tinyVAST.verbose" = FALSE)

## ----eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=6, warning=FALSE----
data(isle_royale, package="dsem")

# Convert to long-form
data = expand.grid( "time"=isle_royale[,1], "var"=colnames(isle_royale[,2:3]) )
data$logn = unlist(log(isle_royale[2:3]))

# Define cross-lagged DSEM
dsem = "
  # Link, lag, param_name
  wolves -> wolves, 1, arW
  moose -> wolves, 1, MtoW
  wolves -> moose, 1, WtoM
  moose -> moose, 1, arM
  #wolves -> moose, 0, corr
  wolves <-> moose, 0, corr
"

# fit model
mytiny = tinyVAST( spacetime_term = dsem,
                 data = data,
                 times = isle_royale[,1],
                 variables = colnames(isle_royale[,2:3]),
                 formula = logn ~ 0 + var )
mytiny

# Deviance explained relative to both intercepts
# Note that a process-error-only estimate with have devexpl -> 1
deviance_explained( mytiny, 
                    null_formula = logn ~ 0 + var )

# See summary
knitr::kable( summary(mytiny,"spacetime_term"), digits=3 )

## ----eval=TRUE, echo=FALSE, message=FALSE, fig.width=6, fig.height=6----------
B = matrix( mytiny$internal$parlist$beta_z[1:4], nrow=2,
            dimnames=list(colnames(isle_royale[,2:3]),colnames(isle_royale[,2:3])) )
knitr::kable( B, digits=3)

## ----eval=requireNamespace("dsem"), echo=TRUE, message=FALSE, fig.width=6, fig.height=6, warning=FALSE----
library(dsem)

# Keep in wide-form
dsem_data = ts( log(isle_royale[,2:3]), start=1959)

# fit without delta0
# Fitting with family = "fixed" seems stable on CRAN checks
mydsem = dsem::dsem( 
  sem = dsem,
  tsdata = dsem_data,
  control = dsem_control(
    getsd = FALSE
  ), 
)
mydsem

# See summary
knitr::kable( summary(mydsem), digits=3 )

## ----eval=TRUE, echo=FALSE, message=FALSE, fig.width=6, fig.height=6----------
B = matrix( mydsem$obj$env$parList()$beta_z[1:4], nrow=2,
            dimnames=list(colnames(isle_royale[,2:3]),colnames(isle_royale[,2:3])) )
knitr::kable( B, digits=3)

## ----include = FALSE, warning=FALSE, message=FALSE----------------------------
run_time = Sys.time() - start_time

