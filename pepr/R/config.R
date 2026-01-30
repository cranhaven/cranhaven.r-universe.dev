#' Config objects and specialized list obejcts and expand string attributes
#'
#' Config objects are used with the \code{"\linkS4class{Project}"} object
#'
#' @importFrom methods callNextMethod
#'
#' @exportClass Config
setClass("Config", contains = "list")


setMethod("initialize", "Config", function(.Object, data) {
  .Object = methods::callNextMethod(.Object, data)  # calls list initialize
  return(.reformat(.Object))
})

#' The constructor of a class representing PEP config
#'
#' @param file a character with project configuration yaml file
#' @param amendments a character with the amendments names to be activated
#'
#' @return an object of \code{"\linkS4class{Config}"} class
#' @examples
#' projectConfig = system.file("extdata", "example_peps-master",
#' "example_amendments1", "project_config.yaml", package="pepr")
#' c=Config(projectConfig)
#' @export
#' @rdname Config-class
Config = function(file, amendments = NULL) {
  message("Loading config file: ", file)
  cfg_data = .loadConfig(filename = file, amendments = amendments)
  config = methods::new("Config", data = cfg_data)
  config = makeSectionsAbsolute(config, REQ_ABS, file)
  .listAmendments(config)
  return(config)
}


# Override the standard generic show function for our config-style lists
setMethod(
  "show",
  signature = "Config",
  definition = function(object) {
    cat("Config object. Class:", class(object), fill = T)
    .printNestedList(object)
    invisible(NULL)
  }
)


#' Recursively try to expand list of strings
#'
#' @param x list, possibly of strings that are paths to expand
#'
#' @return list of strings with paths expaned
#'
#' @examples
#' x = list(a=list(b=list(c="~/test.txt")))
#' .expandList(x)
#' @export
#' @keywords internal
.expandList <- function(x) {
  if (is.list(x))
    return(lapply(x, .expandList))
  if (length(x) > 1)
    return(unname(sapply(x, .expandList)))
  return(suppressWarnings(.expandPath(x)))
}

#' Get list subscript
#'
#' Based on available list element names and subscript value determine
#' index of the element requested
#'
#' @param lst list to search subscript for
#' @param i character or numeric to determine final list index
#'
#' @return numeric index of the requested element in the list
#'
#' @examples
#' l = list(a="a", b="b")
#' .getSubscript(l, 1) == .getSubscript(l, "a")
#' @export
#' @keywords internal
.getSubscript <- function(lst, i) {
  if (is.character(i))
    return(grep(paste0("^", i, "$"), names(lst)))
  return(i)
}


#' Access \code{"\linkS4class{Config}"} object elements
#'
#' You can subset \linkS4class{Config} by identifier or by position using the
#' \code{`[`}, \code{`[[`} or \code{`$`} operator.
#' The string will be expanded if it's a path.
#'
#' @param x a \code{"\linkS4class{Config}"} object.
#' @param i position of the identifier or the name of the identifier itself.
#' @param name name of the element to access.
#'
#' @return An element held in \code{"\linkS4class{Config}"} object
#' @importFrom methods as
#' @examples
#' projectConfig = system.file("extdata", "example_peps-master",
#' "example_amendments1", "project_config.yaml", package="pepr")
#' c=Config(projectConfig)
#' c[[2]]
#' c[2]
#' c[["sample_table"]]
#' c$sample_table
#'
#' @name select-config
NULL

#' @rdname select-config
#' @export
setMethod("[", c("Config"), function(x, i) {
  xList = as(x, "list", strict = TRUE)
  subscript = .getSubscript(x, i)
  if (length(subscript) == 0)
    return(NULL)
  element = xList[subscript]
  return(.expandList(element))
})

#' @rdname select-config
#' @export
setMethod("[[", "Config", function(x, i) {
  xList = as(x, "list", strict = TRUE)
  subscript = .getSubscript(x, i)
  if (length(subscript) == 0)
    return(NULL)
  element = xList[[subscript]]
  return(.expandList(element))
})


.DollarNames.Config <- function(x, pattern = "")
  grep(paste0("^", pattern), grep(names(x), value = TRUE))

#' @rdname select-config
#' @export
setMethod("$", "Config", function(x, name) {
  matches = grep(paste0("^", name), names(x))
  if (length(matches) == 0)
    return(NULL)
  hits = x[[matches]]
  return(.expandList(hits))
})


#' Make selected sections absolute using config path
#'
#' @param object \code{"\linkS4class{Config}"}
#' @param sections character set of sections to make absolute
#' @param cfgPath character absolute path to the config YAML file
#'
#' @return Config with selected sections made absolute
#' @export
setGeneric("makeSectionsAbsolute", function(object, sections, cfgPath)
  standardGeneric("makeSectionsAbsolute"))

#' @describeIn makeSectionsAbsolute Make selected sections absolute using config path from \code{"\linkS4class{Project}"}
setMethod(
  "makeSectionsAbsolute",
  signature = signature(
    object = "Config",
    sections = "character",
    cfgPath = "character"
  ),
  definition = function(object, sections, cfgPath) {
    # Enable creation of absolute path using given parent folder path.
    absViaParent = pryr::partial(.makeAbsPath, parent = dirname(cfgPath))
    for (section in sections) {
      if (section %in% names(object))
        object[[section]] = absViaParent(object[[section]])
    }
    return(object)
  }
)


#' Check config spec version and reformat if needed
#'
#' @param object an object of \code{"\linkS4class{Config}"}
#'
#' @return an object of \code{"\linkS4class{Config}"}
#' @keywords internal
setGeneric(".reformat", function(object)
  standardGeneric(".reformat"))

setMethod(
  ".reformat",
  signature = "Config",
  definition = function(object) {
    if (CFG_VERSION_KEY %in% names(object)) {
      split = strsplit(object[[CFG_VERSION_KEY]], "[.]")[[1]]
      if (length(split) < 3)
        stop("PEP version string is not tripartite")
      majorVer = as.numeric(split[1])
      if (majorVer < 2) {
        if (CFG_S_MODIFIERS_KEY %in% names(object)) {
          stop(
            "Project configuration file subscribes to specification
               >= 2.0.0, since ",
            CFG_S_MODIFIERS_KEY,
            " section is defined. Set ",
            CFG_VERSION_KEY,
            " to 2.0.0 in your config"
          )
        } else{
          stop("Config file reformatting is not supported.
               Reformat the config manually.")
        }
      }
    } else{
      stop("Config file is missing ",
           CFG_VERSION_KEY,
           " key.
           Add it to the config manually.")
    }
    return(object)
  }
)

#' Check for existence of a section in the Project config
#'
#' This function checks for the section/nested sections in the config YAML file.
#'  Returns \code{TRUE} if it exist(s) or \code{FALSE} otherwise.
#'
#' Element indices can be used instead of the actual names, see \code{Examples}.
#'
#' @param object object of \code{"\linkS4class{Config}"}
#' @param sectionNames the name of the section or names of the
#'        nested sections to look for
#'
#' @return a logical indicating whether the section exists
#'
#' @examples
#' projectConfig = system.file("extdata", "example_peps-master",
#' "example_amendments1", "project_config.yaml", package="pepr")
#' p=Project(projectConfig)
#' checkSection(config(p),sectionNames = c("amendments","newLib"))
#' checkSection(config(p),sectionNames = c("amendments",1))
#' @export
setGeneric("checkSection", function(object, sectionNames)
  standardGeneric("checkSection"))

#' @describeIn checkSection checks for existence of a section in \code{"\linkS4class{Config}"} objects
setMethod(
  "checkSection",
  signature = "Config",
  definition = function(object, sectionNames) {
    # try co convert the section name to numeric, return original name if
    # not possible this enables the outer method to check the sections
    # existance by index and by name at the same time
    .checkSection(object, sectionNames)
  }
)


#' Load the config of a PEP
#'
#' Loads a PEP config file
#'
#' @param amendments amendments to activate
#' @param filename file path to config file
#'
#' @seealso \url{https://pep.databio.org/}
#' @keywords internal
.loadConfig = function(filename = NULL,
                       amendments = NULL) {
  if (!file.exists(filename)) {
    stop("Config file not found: ", filename)
  }
  # Initialize config object
  cfg_data = yaml::yaml.load_file(filename)
  if (!is.list(cfg_data))
    stop("The config file has to be a YAML formatted file.
         See: http://yaml.org/start.html")
  # Update based on imports inm the config file
  cfg_data = .applyImports(cfg_data, filename)
  # Update based on amendments if any specified
  cfg_data = .applyAmendments(cfg_data, amendments)
  # make bioconductor$readFunPath value absolute, used in BiocProject
  if (!is.null(cfg_data$bioconductor$readFunPath)) {
    path = gsub("\\./", "", cfg_data$bioconductor$readFunPath)
    cfg_data$bioconductor$readFunPath =
      .makeAbsPath(path, parent = dirname(filename))
  }
  cfg_data$name = .inferProjectName(cfg_data, filename)
  return(cfg_data)
}

.listAmendments = function(cfg, style = "message") {
  # this function can be used in object show method, where cat is preferred
  # or for user information when the Project is created, where message
  # is preferred
  if (!style == "message") {
    printFun = pryr::partial(cat, fill = T)
  } else{
    printFun = message
  }
  if (CFG_P_MODIFIERS_KEY %in% names(cfg) &&
      CFG_AMEND_KEY %in% names(cfg[[CFG_P_MODIFIERS_KEY]]) &&
      length(names(cfg[[CFG_P_MODIFIERS_KEY]][[CFG_AMEND_KEY]])) > 0) {
    # If there are any show a cat and return if needed
    printFun("  amendments: ", paste0(names(cfg[[CFG_P_MODIFIERS_KEY]][[CFG_AMEND_KEY]]), collapse =
                                        ","))
    invisible(names(cfg[[CFG_P_MODIFIERS_KEY]][[CFG_AMEND_KEY]]))
  } else{
    # Otherwise return NULL for testing purposes
    NULL
  }
}

#' Apply amendments
#'
#' Overwrite and/or add Project attributes from the amendments section
#'
#' @param cfg config
#' @param amendments list of amendments to apply
#'
#' @return possibly updated config
#'
#' @return config
#' @keywords internal
.applyAmendments = function(cfg, amendments = NULL) {
  if (!is.null(amendments)) {
    for (amendment in amendments) {
      if (!CFG_P_MODIFIERS_KEY %in% names(cfg) ||
          !CFG_AMEND_KEY %in% names(cfg[[CFG_P_MODIFIERS_KEY]]) ||
          is.null(cfg[[CFG_P_MODIFIERS_KEY]][[CFG_AMEND_KEY]][[amendment]])) {
        warning("Amendment not found: ", amendment)
        message("Amendment was not activated")
        return(cfg)
      }
      cfg = utils::modifyList(cfg, cfg[[CFG_P_MODIFIERS_KEY]][[CFG_AMEND_KEY]][[amendment]])
      message("Activating amendment: ", amendment)
    }
  }
  return(cfg)
}

#' Function for recursive config data imports
#'
#' @param cfg_data config data, possibly including imports statement
#' @param filename path to the file to get the imports for
#'
#' @return config data enriched in imported sections, if imports existed in the
#'  input
#' @keywords internal
.applyImports = function(cfg_data, filename) {
  if (!CFG_P_MODIFIERS_KEY %in% names(cfg_data) ||
      !CFG_IMPORT_KEY %in% names(cfg_data[[CFG_P_MODIFIERS_KEY]]))
    return(cfg_data)
  for (externalPath in cfg_data[[CFG_P_MODIFIERS_KEY]][[CFG_IMPORT_KEY]]) {
    externalPath = .makeAbsPath(externalPath, parent = dirname(filename))
    extCfg = .loadConfig(filename = externalPath)
    cfg_data = utils::modifyList(cfg_data, extCfg)
    message("  Loaded external config file: ", externalPath)
  }
  return(cfg_data)
}

#' Infer project name
#'
#' Based on dedicated config section or PEP enclosing dir
#'
#' @param cfg config data
#' @param filename path to the config file
#'
#' @return string project name
#' @keywords internal
.inferProjectName = function(cfg, filename) {
  if (!is.null(cfg$name))
    return(cfg$name)
  return(basename(dirname(path.expand(filename))))
}