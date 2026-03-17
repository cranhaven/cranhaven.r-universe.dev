

#' @title `S3` methods table in vignette
#' 
#' @param generic.function,class see function \link[utils]{methods}
#' 
#' @param package \link[base]{character} scalar
#' 
#' @param package_pattern \link[base]{character} scalar of \link[base]{regex}
#' 
#' @param backtick \link[base]{logical} scalar, whether to put backticks around function names.
#' Default `TRUE` for Markdown/Quarto rendering.
#' 
#' @param ... additional parameters of the function \link[utils]{methods}
#' 
#' @returns 
#' The function [methods2kable()] returns a \link[base]{data.frame}.
#' 
#' @keywords internal
#' @importFrom utils methods packageVersion
#' @importFrom knitr kable
#' @export
methods2kable <- function(generic.function, class, package, package_pattern, backtick = TRUE, ...) {
  
  if (!missing(package)) {
    cl <- quote(from %in% package)
    kcaption <- if (!missing(generic.function)) {
      if (length(generic.function) > 1L) {
        sprintf(fmt = '`%s::%s.*`', package, generic.function) |>
          paste(collapse = ', ') |>
          sprintf(fmt = '`S3` methods %s (v%s)', . = _, packageVersion(package))
      } else {
        generic.function |>
          .ns_generic(backtick = TRUE, ver = TRUE) |>
          sprintf(fmt = '`S3` methods of %s')
      }
    } else if (!missing(class)) {
      sprintf(fmt = '`S3` methods `%s::*.%s` (v%s)', package, class, packageVersion(package))
    } else stop()
  } else if (!missing(package_pattern)) {
    cl <- quote(grepl(pattern = package_pattern, x = from))
    kcaption <- NULL # lazy way out :))
  } else stop('unspecified `package`')
  
  MFinfo <- function(...) {
    methods(...) |> 
      attr(which = 'info', exact = TRUE)
  }
  
  mf_info <- if (!missing(generic.function)) {
    if (length(generic.function) > 1L) {
      generic.function |>
        lapply(FUN = MFinfo, ...) |>
        do.call(what = rbind.data.frame, args = _)
    } else {
      MFinfo(generic.function = generic.function, ...)
    }
  } else if (!missing(class)) {
    MFinfo(class = class, ...)
  } else stop()
  
  x <- mf_info |>
    subset.data.frame(subset = eval(cl)) |>
    within.data.frame(expr = {
      
      if (length(from) > 1L & all(duplicated.default(from)[-1L])) {
        from <- NULL
      }
      
      if (length(generic) > 1L & all(duplicated.default(generic)[-1L])) {
        generic <- NULL
      } else {
        generic <- generic |> 
          vapply(FUN = .ns_generic, backtick = backtick, ver = FALSE, FUN.VALUE = '')
      }
      
    })
  
  if (backtick) {
    rownames(x) <- x |>
      rownames() |> 
      sprintf(fmt = '`%s`')
  }
  
  x |> 
    kable(caption = kcaption)
  
}


#' @title Generic Function with Namespace
#' 
#' @param x \link[base]{character} scalar
#' 
#' @param backtick \link[base]{logical} scalar
#' 
#' @param ver \link[base]{logical} scalar
#' 
#' @examples
#' .ns_generic('names<-')
#' .ns_generic('Math', ver = TRUE)
#' .ns_generic('update')
#' 
#' @keywords internal
#' @importFrom methods isGroup
#' @importFrom utils packageVersion
#' @export
.ns_generic <- function(
    x, 
    backtick = TRUE,
    ver = FALSE
) {
  
  .sugar <- endsWith(x, suffix = '<-')
  
  # base::parse(text = _) cannot deal with syntactic sugar
  fn <- x |> 
    get()
  
  if (is.primitive(fn)) {
    ns <- 'base' 
  } else if (isGroup(x)) { # groupGeneric
    ns <- 'methods'
  } else {
    ev <- fn |>
      environment()
    if (!isNamespace(ev)) stop(x, 'dont support yet..')
    ns <- ev |> 
      getNamespaceName()
  }
  
  z <- sprintf(fmt = if (.sugar) '%s::`%s`' else '%s::%s', ns, x)
  
  if (backtick) {
    z <- sprintf(fmt = if (.sugar) '`` %s ``' else '`%s`', z)
  }
  
  if (ver) {
    z <- sprintf(fmt = '%s (v%s)', z, packageVersion(ns))
  }
  
  return(z)
  
}



#' @title [rds2versiondate]
#' 
#' @param x \link[base]{matrix}, e.g., from \url{https://cran.r-project.org/web/packages/packages.rds}
#' 
#' @param pkg \link[base]{character} \link[base]{vector}
#' 
#' @param format \link[base]{Date} format, see functions \link[base]{format.Date} and \link[base]{strptime} for detail
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @keywords internal
#' @export
rds2versiondate <- function(x, pkg, format = '%a %b %d, %Y', ...) {
  x |>
    as.data.frame.matrix() |>
    subset(subset = Package %in% pkg, select = c('Package', 'Version', 'Date')) |> # ?base::subset.data.frame
    within(expr = {
      Package = Package |> 
        sprintf(fmt = '**`%s`**')
      Date = Date |>
        as.Date.character(format = '%Y-%m-%d') |>
        format.Date(format = format)
      Version = ifelse(
        test = is.na(Date),
        yes = Version,
        no = sprintf(fmt = '%s \U0001f5d3\ufe0f %s', Version, Date)
      )
      Date = NULL
    }) # ?base::within.data.frame
}
