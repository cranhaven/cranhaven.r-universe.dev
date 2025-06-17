## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(htmlwidgets)
library(slickR)

## -----------------------------------------------------------------------------

slickR(obj = nba_player_logo$uri,height = 100, width = "95%")


## -----------------------------------------------------------------------------

slickR(obj = nba_player_logo$uri,height = 100, width = "95%") + 
  settings(dots = TRUE)


## -----------------------------------------------------------------------------

slickR(obj = nba_player_logo$uri,height = 100, width = "95%") + 
  settings(dots = TRUE, autoplay = TRUE)


## -----------------------------------------------------------------------------

opts <- settings(
  dots = TRUE,
  initialSlide = 0,
  slidesToShow = 5, 
  slidesToScroll = 5, 
  focusOnSelect = TRUE)

slickR(obj = nba_player_logo$uri,height = 100, width = "95%") + 
  opts


## -----------------------------------------------------------------------------

slick_link <- slickR(obj = nba_player_logo$uri,
                objLinks = nba_player_logo$player_home,
                height = 100, width = "95%") 

slick_link + opts

## -----------------------------------------------------------------------------

cP1 <- htmlwidgets::JS("function(slick,index) {
                            return '<a>'+(index+1)+'</a>';
                       }")

opts_dot_number <- settings(
    initialSlide = 0,
    slidesToShow = 5,
    slidesToScroll = 5,
    focusOnSelect = TRUE,
    dots = TRUE,
    customPaging = cP1
    )

slick_dots <- slickR(
  obj = nba_player_logo$uri,
  height = 100,
  width = "95%"
)

slick_dots + opts_dot_number


## -----------------------------------------------------------------------------

cP2 <- JS("function(slick,index) {
          return '<a><img src= ' + dotObj[index] + '  width=100% height=100%></a>';
          }")

opts_dot_logo <- 
  settings(
    initialSlide = 0,
    slidesToShow = 5,
    slidesToScroll = 5,
    focusOnSelect = TRUE,
    dots = TRUE,
    customPaging = cP2
  )

# Putting it all together in one slickR call
s2 <- htmltools::tags$script(
  sprintf("var dotObj = %s", jsonlite::toJSON(nba_team_logo$uri))
)

slick_dots_logo <- slickR(
  obj = nba_player_logo$uri,
  height = 100,
  width = "95%"
) + opts_dot_logo

htmltools::browsable(htmltools::tagList(s2, slick_dots_logo))

## -----------------------------------------------------------------------------

slick_up_stack <- slickR(obj = nba_player_logo$uri, height = 100, width = "95%")

slick_down_stack <- slickR(obj = nba_player_logo$uri, height = 100, width = "95%")

slick_up_stack %stack% slick_down_stack


## -----------------------------------------------------------------------------
slick_up_synch <- slickR(obj = nba_player_logo$uri, height = 100, width = "95%")

slick_down_synch <- slickR(obj = nba_player_logo$uri, height = 100, width = "95%")

slick_up_synch %synch% slick_down_synch


## -----------------------------------------------------------------------------

slickR(obj = nba_player_logo$uri[1:2], height = 100, width = "95%") %synch%
( slickR(nba_player_logo$name[1:2], slideType = 'p') + settings(arrows = FALSE) )



