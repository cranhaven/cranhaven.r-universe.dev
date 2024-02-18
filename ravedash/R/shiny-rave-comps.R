

RAVEShinyComponent <- R6::R6Class(
  classname = "RAVEShinyComponent",
  portable = TRUE,
  private = list(
    .ui_func = NULL,  # function(id, value)
    .server_func = NULL,
    .sv = NULL,
    .initialized = FALSE,
    .tools = NULL,
    server_func0 = function(input, output, session){
      if(private$.initialized){ return() }

      depends <- self$container$components[self$depends]

      deps_has_validator <- FALSE
      for(dep in depends){
        if(!inherits(dep, "RAVEShinyComponent")) {
          next
        }
        if(!dep$initialized){
          dep$server_func(input = input, output = output, session = session)
        }
        if(!is.null(dep$sv)){
          deps_has_validator <- TRUE
        }
      }
      sv <- shinyvalidate::InputValidator$new(session = session)
      if(deps_has_validator || self$validators$size()){
        for(dep in depends){
          if(!is.null(dep$sv)){
            sv$add_validator(validator = dep$sv)
          }
        }
        lapply(self$validators$as_list(), function(v){
          if(is.function(v$rule)){
            sv$add_rule(
              inputId = self$get_sub_element_id(
                sub_id = v$sub_id, with_namespace = FALSE),
              rule = v$rule, session. = session)
          }
        })
        sv$enable()
      }
      private$.sv <- sv
      if(is.function(private$.server_func)){
        private$.server_func(input = input, output = output, session = session)
      }
      private$.initialized <- TRUE
      return()
    }
  ),
  public = list(
    container = NULL,
    id = character(0L),
    varname = character(0L),
    validators = NULL,
    depends = NULL,

    no_save = NULL,
    sub_elements = character(0L),
    ready_to_collect = NULL,

    repository_name = character(0L),

    initialize = function(id, varname = id){
      self$id <- id
      self$varname <- varname
      self$no_save <- FALSE
      self$validators <- dipsaus::fastqueue2()
      private$.tools <- dipsaus::fastmap2()
    },

    add_rule = function(rule, sub_id = NULL){
      if(!is.function(rule) || length(formals(rule)) != 1){
        stop("`rule` must be a function that takes one parameters: `value`, i.e., the input value")
      }
      self$validators$add(list(
        rule = rule,
        sub_id = sub_id
      ))
    },

    get_repository = function(){
      if(length(self$repository_name) != 1){
        stop("A RAVE preset component cannot call its `get_repository` method unless `repository_name` is set.")
      }
      if(!inherits(self$container, "RAVEShinyComponentContainer")){
        stop("`get_repository` requires the preset to be registered to a component container.")
      }
      pipeline_repository <- self$repository_name
      if(!self$container$data[['@has']](pipeline_repository)) {
        repository <- raveio::pipeline_read(
          var_names = pipeline_repository,
          pipe_dir = self$container$pipeline_path
        )
        self$container$data[[pipeline_repository]] <- repository
      } else {
        repository <- self$container$data[[pipeline_repository]]
      }
      if(!inherits(repository, "rave_repository")) {
        return(NULL)
      }
      repository
    },

    get_subject = function(){
      repo <- self$get_repository()
      if(inherits(repo, "rave_repository")) {
        subject <- repo$subject
        return(subject)
      }
      return(NULL)
    },

    get_default = function(sub_id = NULL, missing = NULL, use_cache = TRUE, constraint = NULL) {
      vname <- self$get_sub_element_varnames(sub_id)
      try({
        subject <- get_subject()
        if(inherits(subject, "RAVESubject")) {
          missing <- subject$get_default(
            vname, default_if_missing = missing, simplify = TRUE)
        }
      }, silent = TRUE)


      self$get_settings_value(use_cache = use_cache, default = missing,
                              key = vname, constraint = constraint)
    },


    get_settings_value = function(default = NULL, constraint = NULL,
                                  use_cache = FALSE, key = NULL){
      if(!inherits(self$container, "RAVEShinyComponentContainer")){
        stop("Please add this components to a RAVEShinyComponentContainer")
      }
      settings <- NULL
      if(use_cache){
        settings <- self$container$get_cache("pipeline_settings", default = NULL)
      }
      if(is.null(settings)){
        settings <- raveio::pipeline_settings_get(
          pipeline_settings_path = self$container$settings_path)
        self$container$set_cache("pipeline_settings", settings, expire_after = 1)
      }

      if(!length(key)){
        key <- self$varname
      }

      if(!settings$`@has`(key)){
        re <- default
      } else {
        re <- settings[[key]]
      }

      if(length(constraint)){
        re <- re %OF% constraint
      }
      re

    },

    get_dependent_component = function(depend_id){
      if(!inherits(self$container, "RAVEShinyComponentContainer")){
        stop("Please add this components to a RAVEShinyComponentContainer")
      }
      self$container$components[[depend_id]]
    },

    get_sub_element_id = function(sub_id, with_namespace = FALSE){

      if(missing(sub_id) || !length(sub_id)){
        re <- self$id
      } else {
        if(length(sub_id) != 1){ stop("Please specify a valid `sub_id`") }
        if(!sub_id %in% self$sub_elements) {
          self$sub_elements <- c(self$sub_elements, sub_id)
        }
        re <- sprintf("%s__%s", self$id, sub_id)
      }

      if(with_namespace){
        ns <- shiny::NS(self$container$module_id)
        re <- ns(re)
      }

      re
    },

    get_sub_element_varnames = function(sub_id){

      if(missing(sub_id) || !length(sub_id)){
        re <- self$varname
      } else {
        if(length(sub_id) != 1){ stop("Please specify a valid `sub_id`") }
        if(!sub_id %in% self$sub_elements) {
          self$sub_elements <- c(self$sub_elements, sub_id)
        }
        re <- sprintf("%s__%s", self$varname, sub_id)
      }
      re
    },

    get_sub_element_input = function(sub_id){
      session <- shiny::getDefaultReactiveDomain()
      if(!is.environment(session)){ return(NULL) }
      session$input[[self$get_sub_element_id(sub_id, with_namespace = FALSE)]]
    },

    get_tool = function(name, missing_ok = FALSE){

      tool <- private$.tools$`@get`(name, missing = {
        if(!missing_ok) {
          stop("cannot find component tool named: ", name)
        }
        list(
          server_needed = FALSE,
          value = NULL
        )
      })
      if(tool$server_needed) {
        if(!is.function(private$.server_func)){
          stop("Cannot get component tool `", name, "`: the server is required but hasn't been registered.")
        }
      }

      tool$value

    },

    set_tool = function(name, value, server_needed = TRUE) {
      private$.tools[[name]] <- list(
        value = value,
        server_needed = server_needed
      )
    },

    collect_settings = function(sub_ids, map = NULL) {
      session <- shiny::getDefaultReactiveDomain()
      if(!is.environment(session)){
        stop("`collect_settings` must be called within shiny reactive context!")
      }
      if(!inherits(map, 'fastmap2')){
        map <- dipsaus::fastmap2()
      }
      if(isTRUE(self$no_save)){
        return(map)
      }

      if(missing(sub_ids)) {
        sub_ids <- c("", self$sub_elements)
      } else if (!length(sub_ids)){
        sub_ids <- ""
      }
      sub_ids <- unique(sub_ids)
      if(is.character(self$no_save)) {
        sub_ids <- sub_ids[!sub_ids %in% self$no_save]
      }
      if(!length(sub_ids)){
        return(map)
      }

      # check shiny validator
      # if(length(private$sv))
      if(!isTRUE(shiny::isolate(private$.sv$is_valid()))) {
        stop("`collect_settings` validation checking failed: ", self$id)
      }

      if(is.function(self$ready_to_collect)){
        ready <- shiny::isolate(self$ready_to_collect())
        if(!isTRUE(ready)){
          stop("`collect_settings` not ready to collect: ", self$id)
        }
      }

      vnames <- sprintf("%s__%s", self$varname, sub_ids)
      vnames[sub_ids == ''] <- self$varname
      re <- structure(shiny::isolate(lapply(sub_ids, function(id){
        if(id == ''){
          return(session$input[[self$id]])
        } else {
          return(session$input[[self$get_sub_element_id(id, with_namespace = FALSE)]])
        }
      })), names = vnames)

      map[["@mset"]](.list = re)

      map
    }

  ),
  active = list(
    sv = function(){
      private$.sv
    },

    initialized = function(){
      private$.initialized
    },

    ui_func = function(ui){
      if(!missing(ui)){
        if(!is.function(ui) || !all(c('id', 'value', 'depends') %in% names(formals(ui)))){
          stop("`ui` must be a function that takes three parameters: `id`, `value`, and `depends`")
        }
        private$.ui_func <- function(){
          # self = comp
          ns <- shiny::NS(self$container$module_id)
          value <- self$get_settings_value(use_cache = TRUE)
          depends <- self$container$components[self$depends]
          ui(id = ns(self$id), value = value, depends = depends)
        }
      }
      private$.ui_func
    },
    server_func = function(server){
      if(!missing(server)){
        if(!is.function(server) || !all(c('input', 'output', 'session') %in% names(formals(server)))){
          stop("`server` must be a function that takes three parameters: `input`, `output`, and `session`")
        }
        private$.server_func <- server
      }
      private$server_func0
    },

    current_value = function(){
      session <- shiny::getDefaultReactiveDomain()
      if(!is.environment(session)){ return(NULL) }
      session$input[[self$id]]
    }
  )
)


