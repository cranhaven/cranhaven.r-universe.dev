#' @rdname is_r_current
#' @export
assert_is_r_current <- function(severity = getOption("assertive.severity", "stop"))
{
  assert_engine(is_r_current, severity = severity)
}

#' @rdname is_r_current
#' @export
assert_is_current_r <- function(severity = getOption("assertive.severity", "stop"))
{
  .Deprecated("is_r_current")
  assert_engine(is_r_current, severity = severity)
}

#' @rdname is_package_current
#' @export
assert_is_package_current <- function(...)
{
  .Deprecated("assert_all_are_current_packages")
  assert_all_are_current_packages(...)
  
}

#' @rdname is_package_current
#' @export
assert_all_are_current_packages <- function(x, lib.loc = .libPaths(), 
  repos = getOption("repos"), type = getOption("pkgType"),
  severity = getOption("assertive.severity", "stop"))
{
  .xname <- get_name_in_parent(x)
  msg <- gettextf("Some packages in %s are out of date.", .xname)
  assert_engine(
    is_package_current, 
    x, 
    .xname = .xname,
    msg = msg,
    lib.loc = lib.loc, 
    repos = repos, 
    type = type, 
    severity = severity
  )
}

#' @rdname is_package_current
#' @export
assert_any_are_current_packages <- function(x, lib.loc = .libPaths(), 
  repos = getOption("repos"), type = getOption("pkgType"),
  severity = getOption("assertive.severity", "stop"))
{
  .xname <- get_name_in_parent(x)
  msg <- gettextf("Some packages in %s are out of date.", .xname)
  assert_engine(
    is_package_current, 
    x, 
    .xname = .xname,
    msg = msg,
    what = "any",
    lib.loc = lib.loc, 
    repos = repos, 
    type = type, 
    severity = severity
  )
}
