## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = interactive()
)

## ----setup--------------------------------------------------------------------
#  library(svglite)
#  library(slickR)

## -----------------------------------------------------------------------------
#  
#  # Standard Plot
#    xmlSVG({
#      plot(1:10)
#    }, standalone = TRUE)
#  
#  #library(lattice)
#  
#  # xyplot
#    xmlSVG({
#      print(xyplot(x ~ x, data.frame(x = 1:10), type = "l"))
#    }, standalone = TRUE)
#  
#  # dotplot
#    xmlSVG({
#      print(dotplot(variety ~ yield | site,
#        data = barley, groups = year,
#        key = simpleKey(levels(barley$year), space = "right"),
#        xlab = "Barley Yield (bushels/acre) ",
#        aspect = 0.5, layout = c(1, 6), ylab = NULL
#      ))
#    }, standalone = TRUE)
#  
#  #library(ggplot2)
#  
#    xmlSVG({
#      show(ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
#        geom_point())
#    }, standalone = TRUE)
#  

## -----------------------------------------------------------------------------
#  plotsToSVG <- replicate(5,{
#                         svglite::xmlSVG(
#                           code = {
#                             x <- sample(1:5,1)
#                             plot(stats::density(stats::rnorm(10*x,sd=x)))
#                             },
#                           standalone = TRUE)
#                         },
#                        simplify = FALSE
#                       )

## -----------------------------------------------------------------------------
#  slickR::slickR(plotsToSVG, height = 200, width = "95%")

## -----------------------------------------------------------------------------
#  
#  slick_up <- slickR(plotsToSVG, height = 200, width = "95%") +
#    settings(slidesToShow = 1, slidesToScroll = 1)
#  
#  slick_down <- slickR(plotsToSVG, height = 100, width = "95%") +
#    settings(slidesToScroll = 1,  slidesToShow = 3,
#       centerMode = TRUE, focusOnSelect = TRUE)
#  
#  slick_up %synch% slick_down
#  

