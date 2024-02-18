#' Creates a container for preset components
#' @param module_id 'RAVE' module ID
#' @param pipeline_name the name of pipeline to run
#' @param pipeline_path path of the pipeline
#' @param settings_file the settings file of the pipeline, usually stores the
#' pipeline input information; default is \code{"settings.yaml"}
#' @return A \code{'RAVEShinyComponentContainer'} instance
#'
#' @examples
#'
#'
#' f <- tempfile()
#' dir.create(f, showWarnings = FALSE, recursive = TRUE)
#' file.create(file.path(f, "settings.yaml"))
#'
#' container <- new_rave_shiny_component_container(
#'   module_id = "module_power_phase_coherence",
#'   pipeline_name = "power_phase_coherence_pipeline",
#'   pipeline_path = f
#' )
#'
#' loader_project <- presets_loader_project()
#' loader_subject <- presets_loader_subject()
#'
#' container$add_components(
#'   loader_project, loader_subject
#' )
#'
#'
#' @export
new_rave_shiny_component_container <- function(
  module_id, pipeline_name,
  pipeline_path = raveio::pipeline_find(pipeline_name),
  settings_file = "settings.yaml"
){

  RAVEShinyComponentContainer$new(
    module_id = module_id, pipeline_name = pipeline_name,
    pipeline_path = pipeline_path, settings_file = settings_file
  )
}

RAVEShinyComponentContainer <- R6::R6Class(
  classname = "RAVEShinyComponentContainer",
  portable = TRUE,
  private = list(
    .module_id = character(0L),
    .pipeline_name = character(0L),
    .pipeline_path = character(0L),
    .settings_path = character(0L),
    .settings_file = character(0L)
  ),
  public = list(
    data = NULL,

    components = NULL,
    cache = NULL,

    initialize = function(
      module_id = NULL,
      pipeline_name = module_id,
      pipeline_path = raveio::pipeline_find(pipeline_name),
      settings_file = "settings.yaml"
    ){
      settings_path <- file.path(pipeline_path, settings_file)
      sel <- file.exists(settings_path)
      settings_path <- settings_path[sel]
      pipeline_path <- pipeline_path[sel]
      if(!length(settings_path)){
        stop("Invalid pipeline_path and settings_file combinations: cannot find pipeline settings file")
      }
      private$.settings_file <- settings_file
      self$data <- dipsaus::fastmap2()
      private$.module_id <- module_id
      private$.pipeline_name <- pipeline_name
      private$.pipeline_path <- pipeline_path[[1]]
      private$.settings_path <- settings_path[[1]]
      self$components <- dipsaus::fastmap2()
      self$cache <- dipsaus::fastmap2()
    },

    add_components = function(..., .list = list()){
      comps <- c(list(...), .list)
      for(comp in comps){
        stopifnot(inherits(comp, 'RAVEShinyComponent'))
        self$components[[comp$id]] <- comp
        comp$container <- self
      }
    },

    get_cache = function(key, default = NULL){
      if(self$cache$`@has`(key)){
        item <- self$cache[[key]]
        if(Sys.time() - item$timestamp < item$expire_after) {
          return(item$value)
        } else {
          self$cache$`@remove`(key)
        }
      }
      default
    },

    set_cache = function(key, value, expire_after = Inf){
      self$cache[[key]] <- list(
        timestamp = Sys.time(),
        value = value,
        expire_after = expire_after
      )
    },

    initialize_with_new_data = function(){
      ravedash::logger("Initializing ", private$.module_id, " with new data", level = "trace")

      # # Do we need to reset data? or should we manually reset?
      # self$data[["@reset"]]()
      lapply(names(self$components), function(id){
        try({
          tool <- self$components[[id]]$get_tool("initialize_with_new_data", missing_ok = TRUE)
          if(is.function(tool)){
            ravedash::logger("Initializing ", id, level = "trace")
            tool()
          }
        })
      })
    },

    validate_server = function(session) {

      if(!identical(session$ns(NULL), private$.module_id)){
        stop("RAVEShinyComponentContainer$register_server: session namespace inconsistent with module_id")
      }

    },

    collect_settings = function(ids, map = NULL){
      if(!inherits(map, "fastmap2")){
        map <- dipsaus::fastmap2()
      }
      if(missing(ids)){
        ids <- names(self$components)
      } else {
        ids <- ids[ids %in% names(self$components)]
      }
      for(nm in ids){
        self$components[[nm]]$collect_settings(map = map)
      }
      map
    },

    get_input_ids = function(ids){
      if(missing(ids)){
        ids <- names(self$components)
      } else {
        ids <- ids[ids %in% names(self$components)]
      }
      re <- lapply(ids, function(nm){
        comp <- self$components[[nm]]
        sub_ids <- c('', comp$sub_elements)
        sub_ids <- sapply(sub_ids, function(sub_id){
          if(sub_id == ''){ sub_id <- NULL }
          comp$get_sub_element_id(sub_id, with_namespace = FALSE)
        })
        sub_ids
      })
      unique(unlist(re))
    },

    reset_data = function(){
      self$data[['@reset']]()
      self$cache[['@reset']]()
    }

  ),
  active = list(
    module_id = function(){
      private$.module_id
    },
    pipeline_name = function(){
      private$.pipeline_name
    },
    pipeline_path = function(){
      private$.pipeline_path
    },
    settings_path = function(){
      private$.settings_path
    },
    settings_file = function(){
      private$.settings_file
    }
  )
)
