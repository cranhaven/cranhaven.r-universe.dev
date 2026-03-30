## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup, include=FALSE-----------------------------------------------------
library(imagefluency)

bike <- './../inst/example_images/bike.jpg'
berries <- './../inst/example_images/berries.jpg'
bridge <- './../inst/example_images/bridge.jpg'
fireworks <- './../inst/example_images/fireworks.jpg'
office <- './../inst/example_images/office.jpg'
rails <- './../inst/example_images/rails.jpg'
romanesco <- './../inst/example_images/romanesco.jpg'
sky <- './../inst/example_images/sky.jpg'
trees <- './../inst/example_images/trees.jpg'
valley_green <- './../inst/example_images/valley_green.jpg'
valley_white <- './../inst/example_images/valley_white.jpg'

imglist <- list(img_read(valley_white), img_read(fireworks),
                img_read(valley_green))

## ----eval=FALSE---------------------------------------------------------------
#  install.packages('imagefluency')

## ----eval=FALSE---------------------------------------------------------------
#  # install remotes if necessary
#  if (!require('remotes')) install.packages('remotes')
#  # install imagefluency from github
#  remotes::install_github('stm/imagefluency')

## ----eval=FALSE---------------------------------------------------------------
#  library(imagefluency)

## ----eval=FALSE---------------------------------------------------------------
#  # Example image with relatively high contrast: berries
#  berries <- img_read(system.file('example_images', 'berries.jpg', package = 'imagefluency'))
#  # display image
#  grid::grid.raster(berries)
#  # get contrast
#  img_contrast(berries)

## ----eval=FALSE---------------------------------------------------------------
#  # Example image with relatively low contrast: bike
#  bike <- img_read(system.file('example_images', 'bike.jpg', package = 'imagefluency'))
#  # display image
#  grid::grid.raster(bike)
#  # get contrast
#  img_contrast(bike)

## ----eval=FALSE---------------------------------------------------------------
#  # Example image with high complexity: trees
#  trees <- img_read(system.file('example_images', 'trees.jpg', package = 'imagefluency'))
#  # display image
#  grid::grid.raster(trees)
#  # get complexity
#  img_complexity(trees)

## ----eval=FALSE---------------------------------------------------------------
#  # Example image with low complexity: sky
#  sky <- img_read(system.file('example_images', 'sky.jpg', package = 'imagefluency'))
#  # display image
#  grid::grid.raster(sky)
#  # get complexity
#  img_complexity(sky)

## ----eval=FALSE---------------------------------------------------------------
#  # Example image with high self-similarity: romanesco
#  romanesco <- img_read(system.file('example_images', 'romanesco.jpg', package = 'imagefluency'))
#  # display image
#  grid::grid.raster(romanesco)
#  # get self-similarity
#  img_self_similarity(romanesco)

## ----eval=FALSE---------------------------------------------------------------
#  # Example image with low self-similarity: office
#  office <- img_read(system.file('example_images', 'office.jpg', package = 'imagefluency'))
#  # display image
#  grid::grid.raster(office)
#  # get self-similarity
#  img_self_similarity(office)

## ----eval=FALSE---------------------------------------------------------------
#  # Example image with high vertical symmetry: rails
#  rails <- img_read(system.file('example_images', 'rails.jpg', package = 'imagefluency'))
#  # display image
#  grid::grid.raster(rails)
#  # get only vertical symmetry
#  img_symmetry(rails, horizontal = FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  # Example image with low vertical symmetry: bridge
#  bridge <- img_read(system.file('example_images', 'bridge.jpg', package = 'imagefluency'))
#  # display image
#  grid::grid.raster(bridge)
#  # get only vertical symmetry
#  img_symmetry(bridge, horizontal = FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  # Example images depicting valleys: valley_white, valley_green
#  # Example image depicting fireworks: fireworks
#  valley_white <- img_read(system.file('example_images', 'valley_white.jpg', package = 'imagefluency'))
#  valley_green <- img_read(system.file('example_images', 'valley_green.jpg', package = 'imagefluency'))
#  fireworks <- img_read(system.file('example_images', 'fireworks.jpg', package = 'imagefluency'))
#  
#  # create image set as list
#  imglist <- list(valley_white, fireworks, valley_green)
#  
#  # get typicality
#  img_typicality(imglist)

