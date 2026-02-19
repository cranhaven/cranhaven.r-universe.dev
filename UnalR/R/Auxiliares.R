#' @import highcharter
#' @importFrom rlang quo_name
'%NotIN%' <- Negate('%in%')
PocentRelativo <- function(x) {
  TotPercent <- function(m){ m*100/sum(m, na.rm = TRUE) }
  RowPorcent <- round( t(apply(x, MARGIN = 1, FUN = TotPercent)), 1 )
  return(as.data.frame(RowPorcent))
}
cv <- function(x, na.rm = TRUE) {
  return( sd(x, na.rm = na.rm)/abs(mean(x, na.rm = na.rm)) )
}
vars2vec <- function(quosure) {
  Abc <- NULL
  for (i in seq_len(length(quosure))) { Abc <- c(Abc, rlang::quo_name(quosure[[i]])) }
  return(Abc)
}
Spanish.Highcharter <- function() {
  # https://api.highcharts.com/highcharts/lang
  # https://stackoverflow.com/questions/25266392/how-to-set-highchart-global-options-in-r
  # https://es.stackoverflow.com/questions/318416/cómo-cambiar-el-idioma-del-menú-de-opciones-desplegable-en-highcharts
  lang <- getOption("highcharter.lang")

  lang$contextButtonTitle <- "Men\u00fa Contextual del Gr\u00e1fico"
  lang$viewFullscreen     <- "Ver en pantalla completa"
  lang$printChart   <- "Imprimir gr\u00e1fico"
  lang$downloadPNG  <- "Descargar imagen PNG"
  lang$downloadJPEG <- "Descargar imagen JPEG"
  lang$downloadPDF  <- "Descargar documento PDF"
  lang$downloadSVG  <- "Descargar imagen vectorial SVG"
  lang$downloadCSV  <- "Descargar CSV"
  lang$downloadXLS  <- "Descargar XLS"
  lang$viewData     <- "Ver tabla de datos"
  lang$loading      <- "Cargando..."
  lang$noData       <- "No hay informaci\u00f3n para mostrar"
  lang$drillUpText  <- "<< Volver a {series.name}"
  lang$decimalPoint <- ','
  lang$thousandsSep <- '.'

  options(highcharter.lang = lang)
}
addMaxMin <- function(df, min, max) {
  Max_Min <- as.data.frame(matrix(c(max, min), ncol = length(colnames(df)), nrow = 2, byrow = FALSE))
  colnames(Max_Min) <- colnames(df)
  rownames(Max_Min) <- c("Max", "Min")
  return(rbind(Max_Min, df))
}
br2addline <- function(x) { gsub("<br>", "\n", x) }
theme_DNPE <- function() {
  font <- "Ancizar Sans Light"
  # hrbrthemes::theme_ipsum()
  theme_minimal() %+replace%
    theme(
      # Elementos de la Grilla (grid)
      # @ El tema padre ya elimina las líneas del eje (no es necesario hacerlo de nuevo)
      # panel.grid.major = element_blank(), # Quita las líneas de cuadrícula principal
      # panel.grid.minor = element_blank(), # Quita las líneas de cuadrícula menores
      # axis.ticks = element_blank(),       # Quita las líneas del axis ticks

      # Elementos de Texto
      plot.title = element_text(
        family = font   , # Familia de la fuente
        face   = "bold" , # Tipografía en negrita
        colour = "black", # Color de la fuente
        size   = 20     , # Tamaño de la fuente
        hjust  = 0.5    , # Centrando horizontalmente
        vjust  = 2        # Ligeramente elevado
      ),
      plot.subtitle = element_text(
        family = font     ,
        face   = "bold.italic",
        colour = "#94B43B",
        size   = 14       ,
        hjust  = 0.5
      ),
      plot.caption = element_text(
        family = font     ,
        face   = "italic" ,
        colour = "#94B43B",
        size   = 9        ,
        hjust  = 1
      ),
      plot.tag = element_text(
        family = font     ,
        face   = "plain"  ,
        colour = "#94B43B",
        size   = 9        ,
        vjust  = 2
      ),
      axis.title = element_text(
        family = font   ,
        colour = "black",
        size   = 12
      ),
      legend.background = element_blank(),
      panel.background  = element_blank(),
      plot.background   = element_blank()
    )
}
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#' @importFrom stats na.omit
.NAmat2xyList <- function(xy) {
  NAs <- unclass(attr(na.omit(xy), "na.action"))
  if ((length(NAs) == 1L) && (NAs == nrow(xy))) {
    xy  <- xy[-nrow(xy)]
    NAs <- NULL
  }
  diffNAs <- diff(NAs)
  if (any(diffNAs == 1)) {
    xy  <- xy[-(NAs[which(diffNAs == 1)] + 1), ]
    NAs <- unclass(attr(na.omit(xy), "na.action"))
  }
  nParts <- length(NAs) + 1L
  if (!is.null(NAs) && nrow(xy) == NAs[length(NAs)]) { nParts <- nParts - 1 }
  res  <- vector(mode = "list", length = nParts)
  from <- integer(nParts)
  to <- integer(nParts)
  from[1] <- 1
  to[nParts] <- nrow(xy)
  if (!is.null(NAs) && nrow(xy) == NAs[length(NAs)]) { to[nParts] <- to[nParts] - 1 }
  if (nParts > 1) {
    for (i in 2:nParts) {
      to[(i-1)] <- NAs[(i-1)]-1
      from[i]   <- NAs[(i-1)]+1
    }
  }
  for (i in 1:nParts) { res[[i]] <- xy[from[i]:to[i],, drop = FALSE] }
  res
}
.ringDirxy_gpc <- function (xy) {
  a <- xy[, 1]
  b <- xy[, 2]
  nvx <- length(b)
  if ((a[1] == a[nvx]) && (b[1] == b[nvx])) {
    a <- a[-nvx]
    b <- b[-nvx]
    nvx <- nvx - 1
  }
  if (nvx < 3) { return(1) }

  tX <- 0
  dfYMax <- max(b)
  ti <- 1
  for (i in 1:nvx) {
    if (b[i] == dfYMax && a[i] > tX)
      ti <- i
  }
  if ((ti > 1) & (ti < nvx)) {
    dx0 <- a[ti - 1] - a[ti]
    dx1 <- a[ti + 1] - a[ti]
    dy0 <- b[ti - 1] - b[ti]
    dy1 <- b[ti + 1] - b[ti]
  }
  else if (ti == nvx) {
    dx0 <- a[ti - 1] - a[ti]
    dx1 <- a[1] - a[ti]
    dy0 <- b[ti - 1] - b[ti]
    dy1 <- b[1] - b[ti]
  }
  else {
    dx1 <- a[2] - a[1]
    dx0 <- a[nvx] - a[1]
    dy1 <- b[2] - b[1]
    dy0 <- b[nvx] - b[1]
  }
  v3 <- ((dx0 * dy1) - (dx1 * dy0))
  if (v3 > 0) {
    return(as.integer(1))
  } else {
    return(as.integer(-1))
  }
}
#' @importFrom methods slot
#' @importFrom sp Polygon Polygons as.SpatialPolygons.PolygonsList
map2SpatialPolygons <- function(map, IDs, proj4string = CRS(as.character(NA)), checkHoles = FALSE) {
  #	require(maps)
  if (missing(IDs)) { stop("IDs required") }
  xyList <- .NAmat2xyList(cbind(map$x, map$y))
  if (length(xyList) != length(IDs)) { stop("map and IDs differ in length") }
  tab  <- table(factor(IDs))
  n    <- length(tab)
  IDss <- names(tab)
  reg  <- match(IDs, IDss)
  belongs <- lapply(1:n, function(x) which(x == reg))
  # assemble the list of Srings
  Srl <- vector(mode = "list", length = n)
  drop_Polygons <- logical(length = n)
  for (i in seq_len(n)) {
    nParts <- length(belongs[[i]])
    srl <- vector(mode = "list", length = nParts)
    ars <- logical(length = nParts)
    for (j in seq_len(nParts)) {
      crds <- xyList[[belongs[[i]][j]]]
      if (nrow(crds) == 2) { crds <- rbind(crds, crds[1,]) }
      if (nrow(crds) == 3) { crds <- rbind(crds, crds[1,]) }
      if (.ringDirxy_gpc(crds) == -1) { crds <- crds[rev(seq_len(nrow(crds))),] }
      srl[[j]] <- sp::Polygon(coords = crds, hole = FALSE)
      ars[j] <- slot(srl[[j]], "area") > 0
    }
    srl <- srl[ars]
    drop_Polygons[i] <- length(srl) <= 0L
    if (!drop_Polygons[i]) {
      Srl[[i]] <- sp::Polygons(srl, ID = IDss[i])
      # if (checkHoles) Srl[[i]] <- checkPolygonsHoles(Srl[[i]])
    }
  }
  if (sum(drop_Polygons) > 0) {
    warning(
      "map2SpatialPolygons: ", sum(drop_Polygons),
      " zero-area Polygons object(s) omitted"
    )
  }
  Srl <- Srl[!drop_Polygons]
  if (length(Srl) <= 0L) { stop("map2SpatialPolygons: no Polygons output") }
  res <- sp::as.SpatialPolygons.PolygonsList(Srl, proj4string = proj4string)
  res
}
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom gridSVG grid.export
#' @importFrom data.tree as.Node
#' @importFrom XML xpathSApply saveXML
d3tree <- function(data = NULL, rootname = NULL, id = "id", celltext = "name",
                   valueField = "size", width = NULL, height = NULL)
{
  meta   <- NULL
  legend <- NULL
  if (inherits(data, "list") && names(data)[1] == "tm") {
    meta   <- data[-1]
    data   <- convert_treemap(data$tm, ifelse(!is.null(rootname), rootname, deparse(substitute(data))))
    legend <- extract_legend()
  }
  if (inherits(data, c("character", "connection"))) {
    data <- jsonlite::toJSON(jsonlite::fromJSON(data), auto_unbox = TRUE, dataframe = "rows")
  }
  x <- list(
    data = data, meta = meta,
    options = list(id = id, celltext = celltext, valueField = valueField)
  )
  htmlwidgets::createWidget(
    name = "d3tree", x, width = width, height = height, package = "d3treeR"
  )
}

d3tree2 <- function(data = NULL, rootname = NULL, celltext = "name", id = "id",
                    valueField = "size", clickAction = NULL, width = NULL, height = NULL)
{
  meta   <- NULL
  legend <- NULL
  if (inherits(data, "list") && names(data)[1] == "tm") {
    meta   <- data[-1]
    data   <- convert_treemap(data$tm, ifelse(!is.null(rootname), rootname, deparse(substitute(data))))
    legend <- extract_legend()
  }
  if (inherits(data, c("character", "connection"))) {
    data <- jsonlite::toJSON(jsonlite::fromJSON(data), auto_unbox = TRUE, dataframe = "rows")
  }
  if (is.character(clickAction) && !inherits(clickAction, "JS_EVAL")) {
    clickAction <- htmlwidgets::JS(clickAction)
  }
  x <- list(
    data = data, meta = meta, legend = legend,
    options = list(celltext = celltext, id = id, valueField = valueField, clickAction = clickAction)
  )
  htmlwidgets::createWidget(
    name = "d3tree2", x, width = width, height = height, package = "d3treeR"
  )
}

convert_treemap <- function(treemap, rootname = "root") {
  attrPos <- match("vSize", names(treemap))
  treemap <- as.data.frame(lapply(treemap, function(x) {
    if (is.factor(x)) { as.character(x) } else { x }
  }), stringsAsFactors = FALSE)
  if (attrPos > 2) {
    treemap$pathString <- apply(
      treemap[, 1:(attrPos - 1)], MARGIN = 1,
      function(row) {
        paste0(c(rootname, row[which(!is.na(row))]), sep = "/~>/", collapse = "")
      }
    )
  }
  else { treemap$pathString <- paste(rootname, treemap[, 1], sep = "/~>/") }
  dt <- data.tree::as.Node(treemap[, -(1:(attrPos - 1))], pathDelimiter = "/~>/")
  dt$Set(id = 1:dt$totalCount)
  dt$Set(size = dt$Get("vSize"))
  as.list(dt, unname = TRUE, mode = "explicit")
}

extract_legend <- function() {
  if (!is.null(grid::grid.grep(".*legend.*", viewports = TRUE, grep = TRUE, no.match = NULL))) {
    suppressWarnings(
      XML::xpathSApply(
        gridSVG::grid.export(name = NULL)$svg,
        "*/*/*[local-name()='g' and starts-with(@id,'legend')]",
        XML::saveXML
      )
    )
  }
}
