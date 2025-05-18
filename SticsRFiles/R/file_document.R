#' @include global.R
#' An S4 class to represent a file or dir.
#'
#' @slot type A file type ('file','dir')
#' @slot name A file name (with an extension or not)
#' @slot dir A file dir or path
#' @slot ext A file extension
#' @slot con A file connexion
#' @slot content A file content
#' @slot warn Logical, to display (TRUE) or not (FALSE) warnings (default)
#'
#' @keywords internal
#' @noRd
#'
setClass(
  "file_document",
  representation(
    type = "character", name = "character", dir = "character",
    ext = "character", con = "ANY", content = "ANY",
    warn = "logical"
  ),
  prototype(
    type = character(length = 0), name = character(length = 0),
    dir = character(length = 0), ext = character(length = 0),
    con = NULL, content = "", warn = FALSE
  )
)

# constructor
setMethod("filedocument", signature(file = "character", type = "character"),
          function(file = character(length = 0), type = character(length = 0)) {
            return(methods::new("file_document", file, type))
          }
)


# file only
setMethod("filedocument", signature(file = "character", type = "missing"),
          function(file = character(length = 0), type = character(length = 0)) {
            return(methods::new("file_document", file))
          }
)


setMethod("initialize", "file_document",
          function(.Object,
                   file = character(length = 0),
                   type = character(length = 0)) {
            if (missing(file)) {
              stop("missing file name")
            }


            .Object@name <- basename(file)
            .Object@dir <- normalizePath(dirname(file))
            .Object@ext <- calc_ext(.Object@name)

            .Object@type <- calc_type(.Object)

            .Object@warn <- FALSE

            methods::validObject(.Object)
            return(.Object)
          }
)



# setter method
# replace

# set
setReplaceMethod("set_warn", signature(object = "file_document"),
                 function(object, value) {
                   object@warn <- value
                   return(object)
                 }
)


setReplaceMethod("set_name", signature(object = "file_document"),
                 function(object, value) {
                   object@name <- value
                   return(object)
                 }
)

setReplaceMethod("set_dir", signature(object = "file_document"),
                 function(object, value) {
                   object@dir <- normalizePath(value)
                   return(object)
                 }
)

setReplaceMethod("set_ext", signature(object = "file_document"),
                 function(object, value) {
                   object@ext <- value
                   # add reconstruct file name !!!!!!!!!
                   return(object)
                 }
)

# set
setMethod("set_name", signature(object = "file_document"),
          function(object, value) {
            object@name <- value
            return(object)
          }
)


# getter methods
setMethod("get_name", signature(object = "file_document"),
          function(object) {
            return(object@name)
          }
)

#
setMethod("get_dir", signature(object = "file_document"),
          function(object) {
            return(object@dir)
          }
)

#
setMethod("get_ext", signature(object = "file_document"),
          function(object) {
            return(object@ext)
          }
)

setMethod("get_type", signature(object = "file_document"),
          function(object) {
            return(object@type)
          }
)

setMethod("get_path", signature(object = "file_document"),
          function(object) {
            return(file.path(object@dir, object@name))
          }
)

setMethod("exist", signature(object = "file_document"),
          function(object) {
            message <- FALSE
            # TODO: make distinction between dir and file !!!
            p <- get_path(object)
            ret <- file.exists(p)

            if (ret) {
              if (isdir(object)) {
                ret <- ret & get_type(object) == "dir"
              } else {
                ret <- ret & get_type(object) == "file"
              }
            }
            if (!ret & message) {
              message(paste0("   File doesn't exist: ", p))
            }
            return(ret)
          }
)

setMethod("show", "file_document",
          function(object) {
            print(paste0("   name : ", object@name))
            print(paste0("   type : ", object@type))
            print(paste0("   dir : ", object@dir))
            print(paste0("   ext : ", object@ext))
          }
)

#
setMethod("create", signature(object = "file_document"),
          function(object) {
            p <- get_path(object)
            if (!exist(object)) {
              if (object@type == "file") {
                file.create(p)
              }
              if (object@type == "dir") {
                dir.create(p)
              }
            } else {
              warning(paste0("   File already exists : ", p))
            }
          }
)

#
setMethod("move", signature(object = "file_document"),
          function(object, to_file) {
            # cas : rename, move
            if (exist(object)) {
              if (dir.exists(to_file)) {
                to_file <- file.path(to_file, object@name)
              }
              file.rename(get_path(object), to_file)

              object <- methods::new(class(object)[[1]], to_file)
            } else {
              set_name(object) <- to_file
            }
            return(object)
          }
)

#
setMethod("rename", signature(object = "file_document"),
          function(object, to_file) {
            move(object, to_file)
          }
)

#
setMethod("delete", signature(object = "file_document"),
          function(object) {
            if (exist(object)) {
              file.remove(get_path(object))
            }
          }
)




# Methods with a static like behaviour TODO: see to extract these apart
# from this class ???

#
setMethod("infos", signature(object = "ANY"),
          function(object, type) {
            if (methods::is(object, "character")) {
              p <- object
            } else {
              p <- get_path(object)
            }
            # type: all,size,mtime,isdir
            ret <- file.info("")
            if (type == "all") {
              ret <- file.info(p)
            } else {
              ret <- file.info(p)[[type]]
            }
            return(ret)
          }
)

#
setMethod("isdir", signature(object = "ANY"),
          function(object) {
            ret <- infos(object, "isdir")
            if (is.na(ret)) {
              stop("Unavailable information: file or dir doesn't exist !")
            }
            return(ret)
          }
)

#
setMethod("isempty", signature(object = "ANY"),
          function(object) {
            # for files and dirs
            ret <- infos(object, "size") == 0
            if (is.na(ret)) {
              stop("Unavailable information: file doesn't exist !")
            }
            if (isdir(object)) {
              if (methods::is(object, "character")) {
                l <- list.files(object)
              } else {
                l <- list.files(get_path(object))
              }
              ret <- length(l) == 0
            }
            return(ret)
          }
)

#
setMethod("calc_ext", signature(object = "ANY"),
          function(object) {
            ext <- ""
            if (methods::is(object, "character")) {
              name <- object
            } else {
              name <- object@name
            }
            char_vec <- unlist(strsplit(name, "[.]"))
            n_char <- length(char_vec)
            if (n_char > 1) {
              ext <- char_vec[[n_char]]
            }

            return(ext)
          }
)

setMethod("calc_type", signature(object = "ANY"),
          function(object) {
            # default type
            type <- "file"
            if (methods::is(object, "character")) {
              name <- object
            } else {
              name <- get_path(object)
            }
            # keep this order for identifying file from dir
            if (file.exists(name)) type <- "file"
            if (dir.exists(name)) type <- "dir"
            return(type)
          }
)
