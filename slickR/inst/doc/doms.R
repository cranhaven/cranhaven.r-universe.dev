## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(slickR)

## -----------------------------------------------------------------------------
img_bare <- htmltools::tags$img(src = nba_player_logo$uri[1])

## -----------------------------------------------------------------------------
slickR::slickR(img_bare)

## -----------------------------------------------------------------------------
imgs_bare <- lapply(nba_player_logo$uri[c(1:5)],function(x){
  htmltools::tags$img(src = x)
})
  

## -----------------------------------------------------------------------------
slickR::slickR(imgs_bare)

## -----------------------------------------------------------------------------
htmltools::css(marginLeft='auto',marginRight='auto')

## -----------------------------------------------------------------------------
slickR::slickR(slick_div(nba_player_logo$uri[c(1:5)]))

## -----------------------------------------------------------------------------
p <- htmltools::tags$p(
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
  style = htmltools::css(color='red','font-style' = 'italic')
  )

## -----------------------------------------------------------------------------

w <- slick_div(
    x = leaflet::addTiles(leaflet::leaflet()),
    css = htmltools::css(
    height = '400px',
    marginLeft ='auto',
    marginRight='auto')
    )


## -----------------------------------------------------------------------------
doms <- slick_list(img_bare,p,w)

## -----------------------------------------------------------------------------

slickR::slickR(doms) + settings(dots = TRUE, adaptiveHeight = TRUE)


