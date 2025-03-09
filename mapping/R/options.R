# Functions

mapping.options <- function(...)
{
  current <- get(".mapping", envir = asNamespace("mapping"))
  if(nargs() == 0) return(current)
  args <- list(...)
  if(length(args) == 1 && is.null(names(args)))
  { arg <- args[[1]]
  switch(mode(arg),
         list = args <- arg,
         character = return(.mapping[[arg]]),
         stop("invalid argument: ", dQuote(arg)))
  }
  if(length(args) == 0) return(current)
  n <- names(args)
  if(is.null(n)) stop("options must be given by name")
  current[n] <- args
  if(sys.nframe() == 1)
    assign(".mapping", current, envir = asNamespace("mapping"))
  invisible(current)
}


mapping.options.reset <- function()
{
  current <- get(".mapping_default", envir = asNamespace("mapping"))
  assign(".mapping", current, envir = asNamespace("mapping"))
  invisible(current)

}



# Default options

.mapping <- list(

  palette.cont = "YlGnBu",
  palette.cat = "Accent",

  nclass = 5,
  check.unit.names = TRUE,
  use_cache = TRUE,
  use_internet = TRUE,

  alpha = 0.8,
  breaks = NULL,
  interval.closure = "left",
  labels = NULL,
  NA.color = "grey",
  NA.text = "Missing",
  col.style = "cont",
  map.frame = FALSE,

  border.lwd = 0.5,
  border.col = "black",
  border.type = "solid",
  border.alpha = 0.8,

  title = NULL,
  title.position = "left",
  title.color = "black",
  title.fontface = 1,
  title.size = 1,

  legend.title = NA,
  legend.show = TRUE,
  legend.only = FALSE,
  legend.position = c("right","top"),
  legend.portrait = TRUE,
  legend.digits = 3,
  legend.outside = FALSE,
  legend.outside.facetes = FALSE,
  legend.width = 0.7,
  legend.title.position = c("right","top"),
  legend.title.size = 0.7,
  legend.title.fontface = 1,
  legend.title.color = "black",
  legend.text.color = "black",
  legend.text.size = 0.7,
  legend.text.align = "left",
  legend.text.fontface = 1,
  legend.frame = FALSE,
  legend.decimal.mark = ".",
  legend.format = "fg",
  legend.big.mark = ",",
  legend.text.separator = "-",

  facets.free.scale = FALSE,
  facetes.cols = NA,
  facetes.rows = NA,

  interactive.tiles = "CartoDB.Positron",
  interactive.popup.vars = NULL,
  interactive.popup.id = TRUE,
  interactive.popup.closeButton = TRUE,
  interactive.popup.width.max = 150,
  interactive.popup.width.min = 35,
  interactive.highlight.weight = 3,
  interactive.highlight.color = "black",
  interactive.highlight.alpha = 1,
  interactive.highlight.front = TRUE,
  interactive.control.collapse = TRUE,
  interactive.layer.control.position = c("left", "top"),
  interactive.hovered.id = TRUE,

  text.size = 0.5,
  text.col = "black",
  text.fontface = 1,
  text.shadow = FALSE,
  text.alpha =  NA,

  # bubbles.color = "white",
  # bubbles.scale = 4/3,

  credits.source = NULL,
  credits.author = NULL,
  credits.size = 0.7,
  credits.fontface = NA,
  credits.color = "black",
  credits.align = "left",
  credits.position = c("left", "bottom"),

  background.color = "grey85",
  popup.vars = NA,
  compass = NULL,
  style = "white",

  crs = "+init=epsg:4326"

)


.mapping_default <- list(

  palette.cont = "YlGnBu",
  palette.cat = "Accent",
  palette.cont.vector = NULL,
  palette.cont.vector = NULL,

  nclass = 5,
  check.unit.names = TRUE,
  use_cache = TRUE,
  use_internet = TRUE,

  alpha = 0.8,
  breaks = NULL,
  interval.closure = "left",
  labels = NULL,
  NA.color = "grey",
  NA.text = "Missing",
  col.style = "cont",
  map.frame = FALSE,

  border.lwd = 0.5,
  border.col = "black",
  border.type = "solid",
  border.alpha = 0.8,

  title = NULL,
  title.position = "left",
  title.color = "black",
  title.fontface = 1,
  title.size = 1,

  legend.title = NA,
  legend.show = TRUE,
  legend.only = FALSE,
  legend.position = c("right","top"),
  legend.portrait = TRUE,
  legend.digits = 3,
  legend.outside = FALSE,
  legend.outside.facetes = FALSE,
  legend.width = 0.7,
  legend.title.position = c("right","top"),
  legend.title.size = 0.7,
  legend.title.fontface = 1,
  legend.title.color = "black",
  legend.text.color = "black",
  legend.text.size = 0.7,
  legend.text.align = "left",
  legend.text.fontface = 1,
  legend.frame = FALSE,
  legend.decimal.mark = ".",
  legend.format = "fg",
  legend.big.mark = ",",
  legend.text.separator = "-",

  facets.free.scale = FALSE,
  facetes.cols = NA,
  facetes.rows = NA,

  interactive.tiles = "CartoDB.Positron",
  interactive.popup.vars = NULL,
  interactive.popup.id = TRUE,
  interactive.popup.closeButton = TRUE,
  interactive.popup.width.max = 150,
  interactive.popup.width.min = 35,
  interactive.highlight.weight = 3,
  interactive.highlight.color = "black",
  interactive.highlight.alpha = 1,
  interactive.highlight.front = TRUE,
  interactive.control.collapse = TRUE,
  interactive.layer.control.position = c("left", "top"),
  interactive.hovered.id = TRUE,

  text.size = 0.5,
  text.col = "black",
  text.fontface = 1,
  text.shadow = FALSE,
  text.alpha =  NA,

  credits.source = NULL,
  credits.author = NULL,
  credits.size = 0.7,
  credits.fontface = NA,
  credits.color = "black",
  credits.align = "left",
  credits.position = c("left", "bottom"),

  background.color = "grey85",
  popup.vars = NA,
  compass = NULL,
  style = "white",

  crs = "+init=epsg:4326"

)
