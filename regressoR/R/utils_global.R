
# Edit > Folding > Collapse All (is of much help to visualize in an orderly way the code).

# FUNCTIONS ---------------------------------------------------------------------------------------------------------------

# To help reduce the huge list of imports:


#Funciones tomadas del paquete rpart
rpart.control <- function (minsplit = 20L, minbucket = round(minsplit/3), cp = 0.01, 
                           maxcompete = 4L, maxsurrogate = 5L, usesurrogate = 2L, xval = 10L, 
                           surrogatestyle = 0L, maxdepth = 30L, ...) 
{
  if (maxcompete < 0L) {
    warning("The value of 'maxcompete' supplied is < 0; the value 0 was used instead")
    maxcompete <- 0L
  }
  if (any(xval < 0L)) {
    warning("The value of 'xval' supplied is < 0; the value 0 was used instead")
    xval <- 0L
  }
  if (maxdepth > 30L) 
    stop("Maximum depth is 30")
  if (maxdepth < 1L) 
    stop("Maximum depth must be at least 1")
  if (missing(minsplit) && !missing(minbucket)) 
    minsplit <- minbucket * 3L
  if ((usesurrogate < 0L) || (usesurrogate > 2L)) {
    warning("The value of 'usesurrogate' supplied was out of range, the default value of 2 is used instead.")
    usesurrogate <- 2L
  }
  if ((surrogatestyle < 0L) || (surrogatestyle > 1L)) {
    warning("The value of 'surrogatestyle' supplied was out of range, the default value of 0 is used instead.")
    surrogatestyle <- 0L
  }
  list(minsplit = minsplit, minbucket = minbucket, cp = cp, 
       maxcompete = maxcompete, maxsurrogate = maxsurrogate, 
       usesurrogate = usesurrogate, surrogatestyle = surrogatestyle, 
       maxdepth = maxdepth, xval = xval)
}

# Functions taken from the dplyr package. near()
near <- function (x, y, tol = .Machine$double.eps^0.5) 
{
  abs(x - y) < tol
}

# Functions taken from the DUMMIES package
dummy <- function (x, data = NULL, sep = "", drop = TRUE, fun = as.integer, verbose = FALSE) {
  if (is.null(data)) {
    name <- as.character(sys.call(1))[2]
    name <- sub("^(.*\\$)", "", name)
    name <- sub("\\[.*\\]$", "", name)
  }
  else {
    if (length(x) > 1)
      stop("More than one variable provided to produce dummy variable.")
    name <- x
    x <- data[, name]
  }
  if (drop == FALSE && inherits(x, "factor")) {
    x <- factor(x, levels = levels(x), exclude = NULL)
  }
  else {
    x <- factor(x, exclude = NULL)
  }
  if (length(levels(x)) < 2) {
    if (verbose)
      warning(name, " has only 1 level. Producing dummy variable anyway.")
    return(matrix(rep(1, length(x)), ncol = 1, dimnames = list(rownames(x),
                                                               c(paste(name, sep, x[[1]], sep = "")))))
  }
  mm <- model.matrix(~x - 1, model.frame(~x - 1))
  colnames.mm <- colnames(mm)
  if (verbose)
    cat(" ", name, ":", ncol(mm), "dummy varibles created\n")
  mm <- matrix(fun(mm), nrow = nrow(mm), ncol = ncol(mm), dimnames = list(NULL,
                                                                          colnames.mm))
  colnames(mm) <- sub("^x", paste(name, sep, sep = ""), colnames(mm))
  if (!is.null(row.names(data)))
    rownames(mm) <- rownames(data)
  return(mm)
}

dummy.data.frame <- function (data, names = NULL, omit.constants = TRUE, dummy.classes = c("factor" ,"character"), all = TRUE, ...) {
  df <- data.frame(row.names = row.names(data))
  new.attr <- list()
  for (nm in names(data)) {
    old.attr <- attr(df, "dummies")
    if (nm %in% names || (is.null(names) && (dummy.classes == "ALL" || class(data[, nm]) %in% dummy.classes))) {
      dummies <- dummy(nm, data, ...)
      if (ncol(dummies) == 1 & omit.constants) {
        dummies <- matrix(nrow = nrow(data), ncol = 0)
      }
      if (ncol(dummies) > 0)
        new.attr[[nm]] <- (ncol(df) + 1):(ncol(df) + ncol(dummies))
    }
    else {
      if (!all)
        (next)()
      dummies <- data[, nm, drop = FALSE]
    }
    df <- cbind(df, dummies)
  }
  attr(df, "dummies") <- new.attr
  return(df)
}

#----------------------htmlwidgets------------------------
# Funciones tomadas del paquete htmlwidgets

#' Eval character vectors to JS code
#'
#' @param ... character vectors to evaluate
#'
#' @author Joseline Quiros <joseline.quiros@promidat.com>
#' @export e_JS
#' @examples
#' e_JS('5 * 3')
#' 
e_JS <- function (...) 
{
  x <- c(...)
  if (is.null(x)) 
    return()
  if (!is.character(x)) 
    stop("The arguments for JS() must be a character vector")
  x <- paste(x, collapse = "\n")
  structure(x, class = unique(c("JS_EVAL", oldClass(x))))
}


#----------------------colourpicker------------------------
colourInput <- function (inputId, label, value = "white", showColour = c("both", 
                                                          "text", "background"), palette = c("square", "limited"), 
          allowedCols = NULL, allowTransparent = FALSE, returnName = FALSE, 
          closeOnClick = FALSE) 
{
  showColour <- match.arg(showColour)
  palette <- match.arg(palette)
  value <- restoreInput(id = inputId, default = value)
  
  inputTag <- shiny::tags$input(id = inputId, type = "text", 
                                class = "form-control shiny-colour-input", `data-init-value` = value, 
                                `data-show-colour` = showColour, `data-palette` = palette)
  if (!is.null(allowedCols)) {
    allowedCols <- toJSON(allowedCols)
    inputTag <- shiny::tagAppendAttributes(inputTag, `data-allowed-cols` = allowedCols)
  }
  if (returnName) {
    inputTag <- shiny::tagAppendAttributes(inputTag, `data-return-name` = "true")
  }
  if (allowTransparent) {
    inputTag <- shiny::tagAppendAttributes(inputTag, `data-allow-alpha` = "true")
  }
  if (closeOnClick) {
    inputTag <- shiny::tagAppendAttributes(inputTag, `data-close-on-click` = "true")
  }
  inputTag <- shiny::div(class = "form-group shiny-input-container", 
                         `data-shiny-input-type` = "colour", label, inputTag)
}

#---------------------------jsonlite---------------------------
asJSON <- function (x, ...) 
{
  standardGeneric("asJSON")
}

toJSON <- function (x, dataframe = c("rows", "columns", "values"), matrix = c("rowmajor", 
                                                                    "columnmajor"), Date = c("ISO8601", "epoch"), POSIXt = c("string", 
                                                                                                                             "ISO8601", "epoch", "mongo"), factor = c("string", "integer"), 
          complex = c("string", "list"), raw = c("base64", "hex", "mongo", 
                                                 "int", "js"), null = c("list", "null"), na = c("null", 
                                                                                                "string"), auto_unbox = FALSE, digits = 4, pretty = FALSE, 
          force = FALSE, ...) 
{
  dataframe <- match.arg(dataframe)
  matrix <- match.arg(matrix)
  Date <- match.arg(Date)
  POSIXt <- match.arg(POSIXt)
  factor <- match.arg(factor)
  complex <- match.arg(complex)
  raw <- match.arg(raw)
  null <- match.arg(null)
  x <- force(x)
  if (!missing(na)) {
    na <- match.arg(na)
  }
  else {
    na <- NULL
  }
  indent <- if (isTRUE(pretty)) 
    0L
  else NA_integer_
  ans <- asJSON(x, dataframe = dataframe, Date = Date, POSIXt = POSIXt, 
                factor = factor, complex = complex, raw = raw, matrix = matrix, 
                auto_unbox = auto_unbox, digits = digits, na = na, null = null, 
                force = force, indent = indent, ...)
  if (is.numeric(pretty)) {
    #prettify(ans, pretty)
  }
  else {
    class(ans) <- "json"
    return(ans)
  }
}
#----------------------colourpicker------------------------