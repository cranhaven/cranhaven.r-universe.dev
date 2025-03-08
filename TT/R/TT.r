#' @title Display tree structured data using 'datatable' widget
#'
#' @description Wrapper of 'datatable' widget, allowing display of 'data.tree'
#' objects. All arguments of the 'data.tree' become columns and each node is a
#' row. Adds column with buttons allowing folding and unfolding the levels.
#'
#' @details Package consist of treetable function (wrapper of 'datatable') that
#' convert data.tree object to 'dataframe' and 'JS' callback function called
#' after creating the table. Treetable function ads hidden columns used by 'JS'
#' for formatting and folding/unfolding level rows. Hidden columns shall be
#' completely transparent for user \cr
#' Package also include 'DT::format...' functions wrappers, which are working
#' exactly as originals, but are necessary to protect special (helper) columns
#' used by 'JS' callback function for formatting.
#'
#' Color formatting is done by 'kolorWheel' 'JS' script done by Zalka Erno\cr
#'  e-mail: ern0\[at\]linkbroker.hu\cr
#'  <http://linkbroker.hu/stuff/kolorwheel.js/>
#'
#' @authors@R Michal Zielaskowski \email{michal.zielaskowski@@gmail.com}
#' @references \url{https://github.com/zielaskowski/tree-table}
#'
#' @usage treetable(data, color = "#0177A5", colnames = list(), ...)
#' @param data data.tree object. \code{treetable} will extract all arguments in
#'   alphabetical order - these will be a columns. For renaming and ordering of
#'   the columns see colnames.
#' @param color base color (hue) to color the table. Each level will differ with
#'   saturation and luminosity.
#' @param colnames if \code{list()} of characters provided, arguments of
#'   data.tree (columns) will be renamed. If \code{vector()} provided, columns
#'   will be renamed as for list input, additionally columns will be reordered
#'   according to vector level after renaming.
#' @param ... \link[DT]{datatable} parameters
#' @return Return 'HTML' widget using the 'JavaScript' library 'DataTables'
#' @examples
#' data("org")
#' data("col_order")
#' colnames <- factor(c("org",org$attributesAll),
#'                    levels =  col_order)
#' treetable(org, color="#FFFFFF", colnames=colnames)
#'
#' # still datatable works as expected when data.frame provided
#' treetable(data.frame(
#'      date = seq(as.Date("2015-01-01"), by = "day", length.out = 5), x = 1:5))
#'
#' @seealso
#' \code{\link{datatable}}\cr
#' \code{\link{data.tree}}
#' @export
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @import data.tree


treetable <- function(data,
                      color='#0177A5',
                      colnames = list(), ...)
{
  #INPUT:
  #   data      data.tree object
  #   colnames  if missing, the c(node$name, node$attributesAll in alphbetical order) will be the column names;
  #             otherway the colnames will be used; if is.factor(colnames) will sort per levels
  #   color     base color (hue) to colorize the levels; defoult is white
  # First 3 columns are protected and hidden: TT_path, TT_button, TT_on_off,
  # warn about styling: some of options will be overridden

  tree2DF <- function(node, allAttrs){
    if(!(node$isLeaf)) TT_button <- "&oplus;"
    else TT_button <- "|&mdash;"

    attrs <- node$attributes
    attrVal <- c(purrr::map_chr(allAttrs,
                                function(at){
                                    if (at %in% attrs) return(node[[at]])
                                    else return("")
                                  }))

    db <- c(node$path %>% paste0(collapse = "/"),
            TT_button,
            node$level,
            node$name,
            attrVal)
    names(db) <- c("TT_path", "TT_button", "TT_on_off", "name",allAttrs)

    return(db)
  }

  # if data is not in data.tree format, return standard datatable
  if(!"Node" %in% class(data)){
                   warning("Provided data is not in data.tree format. Creating standard datatable.")
                   if(purrr::is_empty(colnames)) colnames <- colnames(data)
                   dt <- DT::datatable(data,
                             colnames = colnames,
                             ...)
                   return(dt)
  }

  arg <- list(...)

  # extract data.frame from data.tree
  # nodes with missing attributes fill with ""
  allAttrs <- data$attributesAll # attributesAll is very time costly, better use once
  dt_data <- data$Get(tree2DF, allAttrs)
  dt_data <- apply(dt_data,2,list) %>%
    purrr::map(~.x) %>%
    unlist(dt_data, recursive = FALSE) %>%
    dplyr::bind_rows()
  # initialize with only top level
  dt_data[dt_data$TT_on_off != 1, "TT_on_off"] <- "0"

  # rownames always jump in as first so we need to shift by one
  # default behavior is to display rownames (when arg is missing or TRUE)
  if(purrr::is_empty(arg$rownames)) arg$rownames <- TRUE
  if(arg$rownames) shift <- 1
  else shift<-0

  # set width of button column based on max no of levels
  max_lev <- data$Get(function(node)node$level) %>% max()

  # arrange columns if collnames!=list()
  if(!purrr::is_empty(colnames))
  {
    dt_data %<>% dplyr::rename_with(function(x){
      c("TT_path","lev","TT_on_off", colnames %>% as.character())
    })
    dt_data %<>% dplyr::select("TT_path","lev","TT_on_off",dplyr::all_of(colnames %>% sort()))
  }

  #warn when overriding options
  if(any(arg$options$columnDefs %>% unlist %>% names %in% "orderable")){
    warning("option 'orderable' will be overwritten with FALSE", call. = FALSE)
  }
  if(!purrr::is_empty(arg$escape)) warning("option 'escape' will be overwritten with FALSE", call. = FALSE)
  if(!purrr::is_empty(arg$callback)) warning("option 'callback' not possible (yet)", call. = FALSE)
  #shift column options and protect classNames
  arg$options$columnDefs %>% purrr::map(function(x){
    if(is.numeric(x$targets)) x$targets <- x$targets + 3
    if(!purrr::is_empty(x$className) && x$className %in% c("button-col","path-col","onoff-col")) {
      warning(paste0("renamed protected className: ", x$className,". Renamed to X_", x$className), call. = FALSE)
    }
  })

  #hardcoded options
  arg$escape = FALSE
  arg$callback = htmlwidgets::JS("lev(table)")

  arg$options$columnDefs <- c(arg$options$columnDefs,list(
    list(className = 'button-col', targets = 1 +shift), #TT_button
    list(className = 'path-col', targets = 0 + shift), #TT_path
    list(className = 'onoff-col', targets= 2 + shift), #TT_on_off
    list(visible = FALSE, targets = 0 + shift), #TT_path
    list(visible = FALSE, targets= 2 + shift), #TT_on_off
    list(orderable = FALSE, targets = '_all'),
    list(width = paste0(max_lev*20,'px'),targets = 1 +shift)
  ))

  arg$options$color <- color
  arg$data <- dt_data
  arg$colnames <- colnames(dt_data)
  dt <- do.call(DT::datatable, arg)

  ## add JS scripts####
  #scripts are in "sysdata.rda" as kw, lev
  #"sysdata.rda" is loaded silently and avilable...just like this
  #####################
  ###kolorWHEEL########
  #credits to:
  #Zalka Erno
  #e-mail: ern0@linkbroker.hu
  #http://linkbroker.hu/stuff/kolorwheel.js/

  dt <- htmlwidgets::appendContent(dt,kw,lev)

  return (dt)

}
