## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(slickR)

## -----------------------------------------------------------------------------

get_help <- function(fn,pkg){
 
  ff <- get('index.search',envir = asNamespace('utils'))(
    topic = fn, 
    paths = find.package(pkg),
    TRUE)
  
  path    <- dirname(ff)
  dirpath <- dirname(path)
  pkgname <- basename(dirpath)
  RdDB    <- file.path(path, pkgname)
  
  paste0(
    utils::capture.output({
      tools::Rd2HTML(
        Rd = get('fetchRdDB', envir = asNamespace('tools'))(RdDB, basename(ff))
      )
    }),
  collapse='\n')
  
}


## -----------------------------------------------------------------------------

help_files <- lapply(
    ls("package:stats",pattern = '^r')[-1],
    get_help,
    pkg = 'stats')


## -----------------------------------------------------------------------------

slickR::slickR(help_files,
               slideType = 'iframe',
               height = 400,
               width='95%') + 
  settings(dots  = TRUE, 
           slidesToShow   = 2, 
           slidesToScroll = 2)

