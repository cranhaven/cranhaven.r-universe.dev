#' @name rave-input-output-card
#' @title Input and output card (front-end element)
#' @seealso \code{\link[shidashi]{card}}
#' @param title title of the card
#' @param href hyper reference link of the card
#' @param ... additional elements to be included in the card, see
#' \code{\link[shidashi]{card}}
#' @param class the 'HTML' class for card
#' @param class_header the 'HTML' class for card header; default is
#' \code{'shidashi-anchor'}, which will generate shortcuts at the page footers
#' @param class_body the 'HTML' class for card body; default is
#' \code{"padding-10"}, with \code{'10px'} at each direction
#' @param class_foot the 'HTML' class for card footer; default is
#' \code{"padding-10"}, with \code{'10px'} at each direction
#' @param tools a list of additional card tools, see
#' \code{\link[shidashi]{card_tool}}
#' @param append_tools whether to append \code{tools} to the default list;
#' default is true
#' @param module_id the 'RAVE' module ID
#' @param footer footer elements
#' @param toggle_advanced whether to show links in the footer to toggle
#' elements with 'HTML' class \code{'rave-optional'}
#' @return 'HTML' tags
#'
#' @examples
#'
#'
#' input_card(title = "Condition selector",
#'            "Please select experimental conditions:",
#'            shiny::selectInput(
#'              inputId = "condition", label = "Condition",
#'              choices = c("Audio", "Visual")
#'            ))
#'
NULL

#' @name card_url
#' @title Set 'URL' scheme for modules
#' @description Automatically generates \code{href} for \code{\link{input_card}}
#' and \code{\link{output_card}}
#' @param module_id the module ID
#' @param root 'URL' default route
#' @param sep separation
#' @param title a title string that will be used to generate 'URL'
#' @param type type of the card; choices are \code{'input'} or \code{'output'}
#' @returns The hyper reference of suggested card 'URL'
#' @examples
#'
#' set_card_url_scheme(
#'   module_id = "power_explorer",
#'   root = "https://openwetware.org/wiki/RAVE:ravebuiltins",
#'   sep = ":")
#'
#' card_href("Set Electrodes", type = "input", module_id = "power_explorer")
#'
#'
#' @export
set_card_url_scheme <- function(module_id, root, sep = "/"){
  stopifnot2(length(root) == 1 && is.character(root), msg = "`root` must be a character with length of one")
  stopifnot2(length(sep) == 1 && is.character(sep), msg = "`sep` must be a character with length of one")
  urls <- getOption("ravedash.urls", NULL)
  if(!inherits(urls, "fastmap2")){
    urls <- dipsaus::list_to_fastmap2(as.list(urls))
    options("ravedash.urls" = urls)
  }
  urls[[module_id]] <- list(
    root = root,
    sep = sep
  )

  file.path(
    fsep = sep,
    root, module_id,
    "<card type>_<Placeholder for card title>"
  )
}

#' @rdname card_url
#' @export
card_href <- function(title, type = "input",
                      module_id = NULL) {
  if(length(title) >= 2){
    title <- title[[1]]
  }
  if(!length(title)){
    return("#")
  }
  title <- tolower(gsub("[^a-zA-Z0-9]", "", title))

  if(!length(module_id)){
    session <- shiny::getDefaultReactiveDomain()
    if(length(session)){
      module_id <- session$ns(NULL)
    }
  }

  if(!length(module_id) || module_id == "" || module_id == "mock-session") {
    module <- get_active_module_info()
    if(is.list(module)){
      module_id <- module$id
    } else {
      module_id <- "<placeholder for module ID>"
    }
  }

  urls <- getOption("ravedash.urls", dipsaus::fastmap2())
  url_scheme <- urls$`@get`(module_id, missing = list(
    root = "https://openwetware.org/wiki/RAVE",
    sep = ":"
  ))

  file.path(
    fsep = c(url_scheme$sep, ":")[[1]],
    url_scheme$root, module_id,
    sprintf("%s_%s", type, title)
  )

}

#' @rdname rave-input-output-card
#' @export
input_card <- function(title, ...,
                       class = "",
                       class_header = "shidashi-anchor",
                       class_body = "padding-10",
                       class_foot = "padding-10",
                       href = "auto", tools = NULL,
                       footer = NULL, append_tools = TRUE,
                       toggle_advanced = FALSE,
                       module_id = get0("module_id", ifnotfound = NULL, envir = parent.frame())){

  if(identical(href, "auto")){
    href <- card_href(title, type = "input",
                      module_id = module_id)
  }

  if(href %in% c("", "#", "/")) {
    if( append_tools ) {
      all_tools <- list(
        shidashi::card_tool(widget = "collapse"),
        tools
      )
    } else {
      all_tools <- list(
        tools,
        shidashi::card_tool(widget = "collapse")
      )
    }

  } else {
    if( append_tools ) {
      all_tools <- list(
        shidashi::card_tool(widget = "link", href = href, icon = shiny_icons$help),
        shidashi::card_tool(widget = "collapse"),
        tools
      )
    } else {
      all_tools <- list(
        tools,
        shidashi::card_tool(widget = "link", href = href, icon = shiny_icons$help),
        shidashi::card_tool(widget = "collapse")
      )
    }
  }

  if(toggle_advanced){
    footer <- shiny::tagList(
      footer,
      shiny::tags$small(
        class = "fill-width display-block",
        shiny::a(href = "#", class = "toggle-advance-options float-right", "Show/Hide advanced options")
      )
    )
  }
  class <- dipsaus::combine_html_class(class, "ravedash-input-card")
  shidashi::card(title = title, ..., class = class, tools = all_tools, class_body = class_body, class_foot = class_foot, footer = footer, class_header = class_header)
}


#' @rdname rave-input-output-card
#' @export
output_card <- function(title, ..., class = "", class_body = "padding-10",
                        class_foot = "padding-10", href = "auto",
                        tools = NULL, append_tools = TRUE,
                        module_id = get0("module_id", ifnotfound = NULL, envir = parent.frame())){

  if(identical(href, "auto")){
    href <- card_href(title, type = "output", module_id = module_id)
  }

  if(href %in% c("", "#", "/")) {
    if( append_tools ){
      all_tools <- list(
        shidashi::card_tool(widget = "collapse"),
        shidashi::card_tool(widget = "maximize"),
        tools
      )
    } else {
      all_tools <- list(
        tools,
        shidashi::card_tool(widget = "collapse"),
        shidashi::card_tool(widget = "maximize")
      )
    }

  } else {
    if( append_tools ){
      all_tools <- list(
        shidashi::card_tool(widget = "link", href = href, icon = shiny_icons$help),
        shidashi::card_tool(widget = "collapse"),
        shidashi::card_tool(widget = "maximize"),
        tools
      )
    } else {
      all_tools <- list(
        tools,
        shidashi::card_tool(widget = "link", href = href, icon = shiny_icons$help),
        shidashi::card_tool(widget = "collapse"),
        shidashi::card_tool(widget = "maximize")
      )
    }

  }

  class <- dipsaus::combine_html_class(class, "ravedash-output-card")
  shidashi::card(title = title, ..., class = class, tools = all_tools, class_body = class_body, class_foot = class_foot)
}
