# icons
#' @title Shiny icons
#' @details The goal of create this list is to keep 'shiny' icons (which are
#' essentially 'font-awesome' icons) up-to-date.
#' @export
shiny_icons <- structure(list(), class = "ravedash_shiny_icons")

.shiny_icons_methods <- local({
  li <- NULL
  dep <- NULL

  ensure_li <- function(){
    if(is.null(li)){
      li <<- list(
        brain = "brain",
        bars = "bars",
        grid = "th",
        keyboard = "keyboard",
        help = 'question-circle',
        sync = 'sync',
        expand = 'expand',
        tasks = 'tasks',
        angle_right = 'angle-right',
        angle_left = 'angle-left',
        angle_up = 'angle-up',
        angle_down = 'angle-down',
        angle_double_right = 'angle-double-right',
        angle_double_left = 'angle-double-left',
        angle_double_up = 'angle-double-up',
        angle_double_down = 'angle-double-down',
        arrow_right = 'arrow-right',
        arrow_left = 'arrow-left',
        arrow_up = 'arrow-up',
        arrow_down = 'arrow-down',
        external_link = 'external-link-alt',
        plus = 'plus',
        minus = 'minus',
        download = "download",
        save = "save",
        trash = "trash",
        export = "file-export",
        puzzle = "puzzle-piece",
        user_md = "user-md",
        image = "file-image",
        magic = "magic",
        check = "check",
        simplybuilt = "simplybuilt",
        table = "table",
        times = "times",
        code = "code",
        terminal = "terminal",
        filter = "filter",
        tools = "tools",
        wrench = "wrench"
      )
    }
    if(is.null(dep)) {
      dep <<- htmltools::htmlDependency(
        name = "fontawesome-free-ravedash",
        version = "5.15.4",
        package = 'ravedash',
        src = c(file = "assets"),
        stylesheet = "css/all.min.css",
        all_files = TRUE
      )
    }
    li
  }

  get_icon <- function(name, class = NULL){
    ensure_li()
    re <- li[[name]]
    if(is.null(re)){
      # warning("Icon `", name, "` not found, please file an issue to the 'RAVE' team to support your icon")
      re <- name
    }
    # class <- dipsaus::combine_html_class(sprintf("fa-%s", re), class)
    # if(is.null(class) || !grepl("fa[sb]{0,1}", class)) {
    #   class <- dipsaus::combine_html_class("fa", class)
    # }
    # re <- htmltools::tags$li(
    #   class = class,
    #   role = "presentation",
    #   `aria-label` = sprintf("%s icon", re),
    #   dep
    # )
    re <- shiny::icon(re, class = class, verify_fa = FALSE, html_dependency = dep)
    attr(re, "browsable_html") <- TRUE
    re
  }

  get_name <- function(){
    ensure_li()
    names(li)
  }

  set_name <- function(name, icon){
    if(name %in% get_name()){
      stop("Icon with name `", name, "` has been registered. Please consider other names")
    }
    if(!is.character(icon)) {
      stop("`set_name` icon must be characters")
    }
    li[[name]] <<- icon
  }

  list(
    get_icon = get_icon,
    get_name = get_name,
    set_name = set_name,
    ensure_li = ensure_li
  )

})

#' @export
names.ravedash_shiny_icons <- function(x){
  .shiny_icons_methods$get_name()
}

#' @export
`$.ravedash_shiny_icons` <- function(x, name){
  .shiny_icons_methods$get_icon(name)
}

#' @export
`[[.ravedash_shiny_icons` <- `$.ravedash_shiny_icons`

#' @export
`[.ravedash_shiny_icons` <- function(x, i, ...){
  .shiny_icons_methods$get_icon(i, dipsaus::combine_html_class(c(...)))
}


#' @export
`$<-.ravedash_shiny_icons` <- function(x, name, value){
  stop("Cannot set shiny_icons")
}

#' @export
`[[<-.ravedash_shiny_icons` <- `$<-.ravedash_shiny_icons`

#' @export
`[<-.ravedash_shiny_icons` <- `$<-.ravedash_shiny_icons`

#' @name rave-runtime-events
#' @title 'RAVE' run-time events
#' @description A set of preset behaviors used by 'RAVE' modules
#' @param session shiny session, usually automatically determined
#' @param rave_id,.rave_id internally used to store unique session
#' identification key
#' @param key event key to fire or to monitor
#' @param value event value
#' @param global whether to notify other sessions (experimental and not
#' recommended)
#' @param force whether to force firing the event even the \code{value} hasn't
#' changed
#' @param default default value if not found
#' @param .internal_ok internally used
#' @details These goal of these event functions is to  simplify the dashboard
#' logic without understanding the details or passing global variables around.
#' Everything starts with \code{register_rave_session}. This function registers
#' a unique identification to session, and adds bunch of registry to
#' monitor the changes of themes, built-in, and custom events. If you have
#' called \code{\link{module_server_common}}, then \code{register_rave_session}
#' has already been called.
#' \describe{
#' \item{\code{register_rave_session}}{make initial registries, must be called,
#' returns a list of registries}
#' \item{\code{fire_rave_event}}{send signals to make changes to a event;
#' returns nothing}
#' \item{\code{get_rave_event}}{watch and get the event values; must run in
#' shiny reactive context}
#' \item{\code{open_loader}}{fire an event with a special key
#' \code{'open_loader'} to open the data-loading panel; returns nothing}
#' \item{\code{close_loader}}{reset an event with a special key
#' \code{'open_loader'} to close the data-loading panel if possible;
#' returns nothing}
#' \item{\code{watch_loader_opened}}{watch in shiny reactive context whether
#' the loader is opened; returns a logical value, but raise errors when
#' reactive context is missing}
#' \item{\code{watch_data_loaded}}{watch a special event with key
#' \code{'data_loaded'}; returns a logical value of whether new data has been
#' loaded, or raise errors when reactive context is missing}
#' \item{\code{current_shiny_theme}}{watch and returns a list of theme
#' parameters, for example, light or dark theme}
#' }
#' @section Built-in Events:
#' The following event keys are built-in. Please do not fire them using
#' \code{fire_rave_event} or the 'RAVE' application might will crash
#' \describe{
#' \item{\code{'simplify_toggle'}}{toggle visibility of 'HTML' elements with
#' class \code{'rave-option'}}
#' \item{\code{'run_analysis'}}{notifies the module to run pipeline}
#' \item{\code{'save_pipeline'}, \code{'load_pipeline'}}{notifies the module to
#' save or load pipeline}
#' \item{\code{'data_loaded'}}{notifies the module that new data has been
#' loaded}
#' \item{\code{'open_loader'}, \code{'toggle_loader'}}{notifies the internal
#' server code to show or hide the data loading panel}
#' \item{\code{'active_module'}}{internally used to store current active
#' module information}
#' }
#' @return See 'Details'
#'
#' @examples
#'
#'
#' library(shiny)
#' library(ravedash)
#'
#' ui <- fluidPage(
#'   actionButton("btn", "Fire event"),
#'   actionButton("btn2", "Toggle loader")
#' )
#'
#' server <- function(input, output, session) {
#'   # Create event registries
#'   register_rave_session()
#'
#'   shiny::bindEvent(
#'     shiny::observe({
#'       fire_rave_event("my_event_key", Sys.time())
#'     }),
#'     input$btn,
#'     ignoreInit = TRUE,
#'     ignoreNULL = TRUE
#'   )
#'   shiny::bindEvent(
#'     shiny::observe({
#'       cat("An event fired with value:", get_rave_event("my_event_key"), "\n")
#'     }),
#'     get_rave_event("my_event_key"),
#'     ignoreNULL = TRUE
#'   )
#'
#'   shiny::bindEvent(
#'     shiny::observe({
#'       if(watch_loader_opened()){
#'         close_loader()
#'       } else {
#'         open_loader()
#'       }
#'     }),
#'     input$btn2,
#'     ignoreInit = TRUE,
#'     ignoreNULL = TRUE
#'   )
#'
#'   shiny::bindEvent(
#'     shiny::observe({
#'       cat("Loader is", ifelse(watch_loader_opened(), "opened", "closed"), "\n")
#'     }),
#'     watch_loader_opened(),
#'     ignoreNULL = TRUE
#'   )
#'
#' }
#'
#' if(interactive()){
#'   shinyApp(ui, server)
#' }
#'
#' @export
register_rave_session <- function(session = shiny::getDefaultReactiveDomain(), .rave_id = NULL){
  if(is.null(session)){
    session <- shiny::MockShinySession$new()
  }
  sync_tools <- shidashi::register_session_id(session = session)
  event_data <- shidashi::register_session_events(session = session)
  rave_event <- shidashi::register_global_reactiveValues(name = "rave_reactives", session = session)

  root_session <- session$rootScope()

  if(!inherits(session$userData$ravedash_reactive_handlers, "fastmap2")){
    handler_map <- dipsaus::fastmap2()
    handler_map$output_options <- dipsaus::fastmap2()
    session$userData$ravedash_reactive_handlers <- handler_map

    # Probably first time registering session
    clean_shiny_sessions(active = FALSE)
    session$onEnded(function() {
      clean_shiny_sessions(sessions = session, active = TRUE)
    })
  }

  if(!root_session$cache$exists('rave_id')){
    if(!missing(.rave_id) && length(.rave_id)){
      rave_id <- paste(unlist(.rave_id), collapse = "")
      rave_id <- gsub("[^a-zA-Z0-9]", "", rave_id)
      if(nchar(rave_id) == 0){
        rave_id <- rand_string()
      }
    } else {
      rave_id <- rand_string()
    }
    root_session$cache$set('rave_id', rave_id)
    root_session$userData$rave_id <- rave_id
  }
  rave_id <- root_session$cache$get("rave_id")
  if(!root_session$cache$exists('rave_loop_events')){
    rave_loop_events <- dipsaus::fastqueue2()
    root_session$cache$set('rave_loop_events', rave_loop_events)
  }
  rave_loop_events <- root_session$cache$get("rave_loop_events")

  if(!inherits(session, "MockShinySession")){
    # register session
    item <- list(
      rave_id = rave_id,
      register_ns = session$ns(NULL),
      root_session = root_session,
      rave_event = rave_event
    )

    sess <- get(x = '.sessions')
    sess$set(rave_id, item)
  }

  list(
    rave_id = rave_id,
    sync_tools = sync_tools,
    theme_event = event_data,
    rave_event = rave_event,
    loop_event = rave_loop_events
  )
}

clean_shiny_sessions <- function(sessions = NULL, active = FALSE) {
  sess <- get(".sessions")
  current_size <- sess$size()

  # Do not actively clean ended sessions
  if(!is.null(sessions) && length(sessions)) {
    active <- TRUE
    if(!is.list(sessions)) {
      sessions <- list(sessions)
    }
  }
  if( !active && current_size <= 20 ) {
    return()
  }
  if(is.null(sessions)) {
    rave_ids <- sess$keys()
  } else {
    rave_ids <- unlist(lapply(sessions, function(session) {
      tryCatch({
        session$userData$rave_id
      }, error = function(e){ NULL })
    }))
  }

  lapply(rave_ids, function(key){
    try({
      item <- sess$get(key, missing = NULL)
      if(is.null(item)){ return() }
      if(!is.list(item)){
        sess$remove(key)
        return()
      }
      if(!is.environment(item$root_session)){
        sess$remove(key)
        return()
      }
      root_session <- item$root_session
      if( root_session$isEnded() ){
        sess$remove(key)
        return()
      }
    }, silent = TRUE)
  })
  invisible(sess$size())
}

#' @rdname rave-runtime-events
#' @export
get_default_handlers <- function(session = shiny::getDefaultReactiveDomain()){
  if(is.null(session)){
    session <- shiny::MockShinySession$new()
  }
  if(!inherits(session$userData$ravedash_reactive_handlers, "fastmap2")){
    register_rave_session(session = session)
  }
  session$userData$ravedash_reactive_handlers
}

#' @rdname rave-runtime-events
#' @export
fire_rave_event <- function(key, value, global = FALSE, force = FALSE,
                            session = shiny::getDefaultReactiveDomain(),
                            .internal_ok = FALSE){
  force(key)

  if(!.internal_ok && key %in% c("active_module")){
    logger("`fire_rave_event`: key 'active_module' is reserved. Do not manually set this key.", level = "error")
    return(invisible())
  }

  force(value)

  tool <- register_rave_session(session)

  logger("Firing RAVE-event: ", key, level = "trace")

  if(global) {
    sess <- get(x = '.sessions')
    lapply(sess$keys(), function(key){
      item <- sess$get(key, missing = NULL)
      if(is.null(item)){ return() }
      if(!is.list(item)){
        sess$remove(key)
        return()
      }
      if(!is.environment(item$root_session)){
        sess$remove(key)
        return()
      }
      root_session <- item$root_session
      if( root_session$isEnded() ){
        sess$remove(key)
        return()
      }
      # shiny::isolate({
      #   # impl <- .subset2(item$rave_event, "impl")
      #   # impl$set(key, value, isTRUE(force))
      #
      # })
      item$rave_event[[key]] <- value

    })
  } else if(!session$isEnded()){
    # impl <- .subset2(tool$rave_event, "impl")
    # impl$set(key, value, isTRUE(force))
    tool$rave_event[[key]] <- value
  }
  invisible()
}

#' @rdname rave-runtime-events
#' @export
get_session_by_rave_id <- function(rave_id) {
  sess <- get(x = '.sessions')
  re <- sess$get(rave_id)
  if(is.list(re)) {
    return(re$root_session)
  }
  return()
}

#' @rdname rave-runtime-events
#' @export
get_rave_event <- function(key, session = shiny::getDefaultReactiveDomain()){
  force(key)
  tool <- register_rave_session(session)

  return(tool$rave_event[[key]])
}

#' @rdname rave-runtime-events
#' @export
open_loader <- function(session = shiny::getDefaultReactiveDomain()){
  fire_rave_event('open_loader', Sys.time())
}

#' @rdname rave-runtime-events
#' @export
close_loader <- function(session = shiny::getDefaultReactiveDomain()){
  fire_rave_event('open_loader', NULL)
}

#' @rdname rave-runtime-events
#' @export
watch_loader_opened <- function(session = shiny::getDefaultReactiveDomain()){
  tool <- register_rave_session(session)
  res <- tool$rave_event$open_loader
  structure(!is.null(res), timestamp = res)
}

#' @rdname rave-runtime-events
#' @export
watch_data_loaded <- function(session = shiny::getDefaultReactiveDomain()){
  tool <- register_rave_session(session)
  res <- tool$rave_event$data_loaded

  # 1. load from pipeline settings only
  # 2. combinations of subject default and pipeline settings

  if(length(res) && is.list(res)){
    return(structure(
      TRUE,
      timestamp = res$timestamp,
      force = isTRUE(res$force)
    ))
  }
  structure(length(res) && !isFALSE(res),
            timestamp = res,
            force = FALSE)
}

#' @rdname rave-runtime-events
#' @export
current_shiny_theme <- function(default, session = shiny::getDefaultReactiveDomain()){
  if(dipsaus::shiny_is_running()) {
    tool <- register_rave_session(session = session)
    return(shidashi::get_theme(tool$theme_event, session = session))
  } else {
    if(missing(default)){
      default <- list(theme = "light", background = "#FFFFFF", foreground = "#000000")
    } else {
      default <- as.list(default)[c("theme", "background", "foreground")]
      default$theme %?<-% "light"
      default$background %?<-% "#FFFFFF"
      default$foreground %?<-% "#000000"
    }
    return(default)
  }
}

#' A hovering footer at bottom-right
#' @description Internally used. Do not call explicitly
#' @param module_id 'RAVE' module ID
#' @param label run-analysis button label; default is \code{"Run Analysis"}
#' @param auto_recalculation whether to show the automatic calculation button;
#' default is true
#' @param class additional class for the footer
#' @param style additional style for the footer
#' @return 'HTML' tags
#'
#' @examples
#'
#' library(shiny)
#' # dummy variables for the example
#' data_loaded <- TRUE
#'
#' # UI code
#' ravedash_footer("my_module")
#'
#' # server code to set message
#' server <- function(input, output, session){
#'
#'   module_server_common(input, output, session, function(){
#'
#'     # check if data has been loaded
#'     if(data_loaded) {
#'
#'       # if yes, then set the footer message
#'       fire_rave_event("loader_message",
#'                       "my_project/subject - Epoch: Auditory")
#'       return(TRUE)
#'     } else {
#'
#'       # No data found, unset the footer message
#'       fire_rave_event("loader_message", NULL)
#'       return(FALSE)
#'     }
#'
#'   })
#' }
#'
#' @export
ravedash_footer <- function(
    module_id = NULL, label = "Run Analysis",
    auto_recalculation = TRUE, class = NULL, style = NULL
){
  ns <- shiny::NS(module_id)
  shiny::div(
    class = dipsaus::combine_html_class("ravedash-back-to-top ravedash-footer", class),
    style = style,
    shiny::div(
      class = "btn-group dropup",
      role="group",
      # shiny::a(
      #   type="button", class="btn btn-default btn-go-top border-right-1", href="#",
      #   shidashi::as_icon("rocket")
      # ),
      shiny::tags$button(
        type="button",
        id = ns("__loader_short_message__"),
        class="btn btn-default border-right-1 btn-go-top shiny-text-output rave-button",
        `data-toggle` = "tooltip",
        title = "Click to toggle the data loader",
        `rave-action` = '{"type": "toggle_loader"}'
      ),
      shiny::tags$button(
        type="button",
        class="btn btn-default border-right-1 border-left-1 rave-button rave-button-autorecalculate",
        `data-toggle` = "tooltip",
        title = "Keyboard shortcut: CTRL+Enter / Command+Return (OSX)",
        `rave-action` = '{"type": "run_analysis"}',
        label
      ),
      shiny::tags$button(
        type="button",
        class="btn btn-default btn-go-top border-left-1 dropdown-toggle dropdown-toggle-split", href="#",
        # "data-toggle"="dropdown",
        "aria-haspopup"="false",
        "aria-expanded"="false",
        shiny::span(
          class = "sr-only",
          "Dropdown-Open"
        )
      ),
      shiny::div(
        class = "dropdown-menu dropdown-menu-right",
        shiny::h6(
          class="dropdown-header",
          "Controllers"
        ),
        # shiny::a(
        #   class = "dropdown-item rave-button",
        #   href = "#",
        #   `rave-action` = '{"type": "toggle_auto_recalculation"}',
        #   shiny_icons$sync,
        #   "Auto re-calculation: ", shiny::textOutput(
        #     outputId = ns("__recalculation_message__"),
        #     container = function(...){
        #       shiny::span(style = "color: #007bff", ...)
        #     })
        # ),
        shiny::div(
          class = "px-3 py-1",
          local({
            if(auto_recalculation) {
              shiny::a(
                class = "btn btn-default rave-button",
                href = "#",
                `rave-action` = '{"type": "toggle_auto_recalculation"}',
                `data-toggle` = "tooltip",
                title = "Toggle auto re-calculation",
                shiny_icons["sync"],
                shiny::textOutput(
                  outputId = ns("__recalculation_message__"),
                  container = function(..., class = NULL){
                    shiny::span(style = "color: #007bff", ...,
                                class = dipsaus::combine_html_class(
                                  class, "pointer-events-none"
                                ))
                  })
              )
            } else { NULL }
          }),
          shiny::a(
            class = "btn btn-default shidashi-button",
            href = "#",
            `shidashi-action` = '{"method": "card", "args": [{"selector": ".ravedash-input-card", "method": "expand"}]}',
            `data-toggle` = "tooltip",
            title = "Expand all input cards",
            shiny_icons['plus']
          ),
          shiny::a(
            class = "btn btn-default shidashi-button",
            href = "#",
            `shidashi-action` = '{"method": "card", "args": [{"selector": ".ravedash-input-card", "method": "collapse"}]}',
            `data-toggle` = "tooltip",
            title = "Collapse all input cards",
            shiny_icons['minus']
          ),
          shiny::a(
            class = "btn btn-default rave-button",
            href = "#",
            `rave-action` = '{"type": "simplify_toggle"}',
            shiny_icons['simplybuilt'],
            `data-toggle` = "tooltip",
            title = "Show more/fewer options"
          )
        ),
        # shiny::a(
        #   class = "dropdown-item shidashi-button",
        #   href = "#",
        #   `shidashi-action` = '{"method": "card", "args": [{"selector": ".ravedash-input-card", "method": "expand"}]}',
        #   shiny_icons$plus,
        #   "Expand all input cards"
        # ),
        # shiny::a(
        #   class = "dropdown-item shidashi-button",
        #   href = "#",
        #   `shidashi-action` = '{"method": "card", "args": [{"selector": ".ravedash-input-card", "method": "collapse"}]}',
        #   shiny_icons$minus,
        #   "Collapse all input cards"
        # ),
        # shiny::a(
        #   class = "dropdown-item rave-button",
        #   href = "#",
        #   `rave-action` = '{"type": "simplify_toggle"}',
        #   shiny_icons$simplybuilt,
        #   "Show more/fewer options"
        # ),
        shiny::div(
          class = "dropdown-divider"
        ),
        shiny::h6(
          class="dropdown-header",
          "Quick Access"
        )
      )
    )
  )

}

#' Get current active module information, internally used
#' @param session shiny reactive domain, default is current domain
#' @return A named list, including module ID, module label, internal
#' \code{'rave_id'}.
#' @export
get_active_module_info <- function(session = shiny::getDefaultReactiveDomain()){
  if(is.environment(session)){
    rave_events <- session$cache$get("rave_reactives", missing = NULL)
    if(shiny::is.reactivevalues(rave_events)){
      info <- shiny::isolate({
        rave_events$active_module
      })
      # make sure module_id is inside!!!
      if(!'id' %in% names(info)){ return(NULL) }
      rave_id <- session$cache$get("rave_id", missing = "")
      info$rave_id <- rave_id
      return(info)
    }
  }
  return(NULL)
}


#' Button to trigger analysis
#' @description A button that triggers \code{'run_analysis'} event;
#' see also \code{\link{get_rave_event}}
#' @param label label to display
#' @param icon icon before the label
#' @param type used to calculate \code{class}
#' @param btn_type button style, choices are \code{'button'} or \code{'link'}
#' @param width,class,style,... passed to 'HTML' tag
#' @return A 'HTML' button tag
#' @export
run_analysis_button <- function(
    label = "Run analysis (Ctrl+Enter)",
    icon = NULL, width = NULL, type = "primary",
    btn_type = c("button", "link"), class = "", style = "", ...){
  if (length(type) > 1) {
    type <- type[[1]]
  }

  args <- list(...)
  width <- c(width, "auto")[[1]]
  style <- paste0("width: ", shiny::validateCssUnit(width),
                  ";", style)

  btn_type <- match.arg(btn_type)

  if(btn_type == "button") {
    stopifnot2(length(type) == 0 || type[[1]] %in% c("default",
                                                     "primary", "info", "success", "warning", "danger"), msg = "type must be in 'default', 'primary', 'info', 'success', 'warning', 'danger'")
    class <- dipsaus::combine_html_class(
      sprintf("btn btn-%s rave-button %s", type, class))

    shiny::tags$button(
      class = class,
      style = style,
      type = "button",
      "rave-action" = '{"type": "run_analysis"}',
      list(shidashi::as_icon(icon), label),
      ...
    )
  } else {

    class <- dipsaus::combine_html_class("rave-button", class)
    shiny::tags$a(
      href = "#",
      class = class,
      style = style,
      "rave-action" = '{"type": "run_analysis"}',
      ...,
      list(label, shidashi::as_icon(icon))
    )
  }


}
