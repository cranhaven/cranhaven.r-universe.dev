## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = FALSE-------------------------------------------------------------
htmltools::tags$script(src = "static/flexbox.js")
htmltools::tags$link(rel = "stylesheet", type = "text/css", href = "static/flexbox.css")

## ----echo = FALSE-------------------------------------------------------------
htmltools::includeHTML("static/flexbox-direction.html")

## ----echo = FALSE-------------------------------------------------------------
htmltools::includeHTML("static/flexbox-wrap.html")

## ----echo = FALSE-------------------------------------------------------------
htmltools::includeHTML("static/flexbox-justify-content.html")

## ----echo = FALSE-------------------------------------------------------------
htmltools::includeHTML("static/flexbox-align.html")

## ----echo = FALSE-------------------------------------------------------------
htmltools::includeHTML("static/flexbox-align-content.html")

## ----echo = FALSE-------------------------------------------------------------
htmltools::includeHTML("static/flex.html")

## ----echo = FALSE-------------------------------------------------------------
htmltools::includeHTML("static/flexbox-grow.html")

## ----echo = FALSE-------------------------------------------------------------
htmltools::includeHTML("static/flexbox-shrink.html")

