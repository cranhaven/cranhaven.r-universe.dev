#' @rdname is_windows
#' @export
is_bsd <- function()
{
  if(!grepl("BSD", Sys.info()[["sysname"]]))
  {
    return(not_this_os("BSD-based"))
  }
  TRUE
}

#' @rdname is_windows
#' @export
is_linux <- function()
{
  if(Sys.info()["sysname"] != "Linux")
  {
    return(not_this_os("Linux"))
  }
  TRUE
}

#' @rdname is_windows
#' @export
is_mac <- function()
{
  if(Sys.info()["sysname"] != "Darwin")
  {
    return(not_this_os("OS X"))
  }
  TRUE
}

#' @rdname is_windows
#' @export
is_osx <- is_mac
  
#' @rdname is_windows
#' @export
is_osx_cheetah <- function()
{
  is_osx_version("Cheetah")
}

#' @rdname is_windows
#' @export
is_osx_puma <- function()
{
  is_osx_version("Puma")
}

#' @rdname is_windows
#' @export
is_osx_jaguar <- function()
{
  is_osx_version("Jaguar")
}

#' @rdname is_windows
#' @export
is_osx_panther <- function()
{
  is_osx_version("Panther")
}

#' @rdname is_windows
#' @export
is_osx_tiger <- function()
{
  is_osx_version("Tiger")
}

#' @rdname is_windows
#' @export
is_osx_leopard <- function()
{
  is_osx_version("Leopard")
}

#' @rdname is_windows
#' @export
is_osx_snow_leopard <- function()
{
  is_osx_version("Snow Leopard")
}

#' @rdname is_windows
#' @export
is_osx_lion <- function()
{
  is_osx_version("Lion")
}

#' @rdname is_windows
#' @export
is_osx_mountain_lion <- function()
{
  is_osx_version("Mountain Lion")
}

#' @rdname is_windows
#' @export
is_osx_mavericks <- function()
{
  is_osx_version("Mavericks")
}

#' @rdname is_windows
#' @export
is_osx_yosemite <- function()
{
  is_osx_version("Yosemite")
}

#' @rdname is_windows
#' @export
is_osx_el_capitan <- function()
{
  is_osx_version("El Capitan")
}

#' @rdname is_windows
#' @export
is_macos_sierra <- function()
{
  is_osx_version("Sierra")
}

#' @rdname is_windows
#' @export
is_macos_high_sierra <- function()
{
  is_osx_version("High Sierra")
}

#' @rdname is_windows
#' @export
is_macos_mojave <- function()
{
  is_osx_version("Mojave")
}

#' @rdname is_windows
#' @export
is_macos_catalina <- function()
{
  is_osx_version("Catalina")
}

#' @rdname is_windows
#' @export
is_macos_big_sur <- function()
{
  is_osx_version("Big Sur")
}

#' @rdname is_windows
#' @export
is_solaris <- function()
{
  if(Sys.info()["sysname"] != "SunOS")
  {
    return(not_this_os("Solaris"))
  }
  TRUE
}

#' @rdname is_windows
#' @export
is_unix <- function()
{
  if(.Platform$OS.type != "unix")
  {
    return(not_this_os("Unix-based"))
  }
  TRUE
}

#' What OS is running?
#' 
#' Is the operating system in this machine Windows/Unix/Mac based.
#' 
#' @param severity How severe should the consequences of the assertion be?  
#' Either \code{"stop"}, \code{"warning"}, \code{"message"}, or \code{"none"}.
#' @return \code{is_windows} returns \code{TRUE} if the OS on the current 
#' platform is Microsoft windows-based.  \code{is_unix} returns \code{TRUE} if 
#' the OS is Unix based (pretty much anything that isn't Windows, including OS 
#' X). 
#' \code{is_mac}, \code{is_linux}, \code{is_bsd}, \code{is_solaris} return 
#' \code{TRUE} if the OS is Apple OS X, Linux, FreeBSD/NetBSD, or Solaris 
#' respectively.
#' \code{is_64_bit_os} returns \code{TRUE} when the operating system is 64-bit.
#' The \code{assert_*} functions return nothing but throw an error if the 
#' corresponding \code{is_*} functions return \code{FALSE}.
#' @references With the exception of \code{is_windows} and \code{is_unix} that 
#' use \code{.Platform$OS.type}, the OS is determined from 
#' \code{Sys.info()[["sysname"]]}, which (not on Windows) is calculated via the 
#' OS \code{uname} program.  GNU has more information on the return value: 
#' \url{https://www.gnu.org/software/libc/manual/html_node/Platform-Type.html}
#' and Wikipedia has a nice list of possible values: 
#' \url{https://en.wikipedia.org/wiki/Uname#Examples}
#' The names for different versions of Windows are decribed in:
#' \url{http://svn.r-project.org/R/trunk/src/library/utils/src/windows/util.c}
#' @seealso \code{\link[base]{.Platform}}, \code{\link[base]{Sys.info}}, 
#' \code{\link[base]{version}}, and \code{win.version}.
#' @examples
#' is_unix()
#' is_linux()
#' is_bsd()
#' is_solaris()
#' if(is_windows())
#' {
#'   assertive.base::dont_stop({
#'     assert_is_windows_vista()
#'     assert_is_windows_7()
#'     assert_is_windows_8()
#'     assert_is_windows_8.1()
#'     assert_is_windows_10()
#'     assert_is_windows_server_2008()
#'     assert_is_windows_server_2008_r2()
#'     assert_is_windows_server_2012()
#'     assert_is_windows_server_2012_r2()
#'   })
#' }
#' if(is_osx()) # is_mac is a synonym
#' {
#'   assertive.base::dont_stop({
#'     assert_is_osx_cheetah()
#'     assert_is_osx_puma()
#'     assert_is_osx_jaguar()
#'     assert_is_osx_panther()
#'     assert_is_osx_tiger()
#'     assert_is_osx_leopard()
#'     assert_is_osx_snow_leopard()
#'     assert_is_osx_lion()
#'     assert_is_osx_mountain_lion()
#'     assert_is_osx_mavericks()
#'     assert_is_osx_yosemite()
#'     assert_is_osx_el_capitan()
#'     assert_is_macos_sierra() # note the change from OSX to macOS
#'   })
#' }
#' is_32_bit()
#' is_64_bit()
#' assertive.base::dont_stop(assert_is_windows())
#' assertive.base::dont_stop(assert_is_unix())
#' @export
is_windows <- function()
{
  if(.Platform$OS.type != "windows")
  {
    return(not_this_os("Windows"))
  }
  TRUE
}

#' @rdname is_windows
#' @export
is_windows_vista <- function()
{
  is_windows_version("Vista")
}

#' @rdname is_windows
#' @export
is_windows_7 <- function()
{
  is_windows_version("7")
}

#' @rdname is_windows
#' @export
is_windows_8 <- function()
{
  is_windows_version("8")
}

#' @rdname is_windows
#' @export
is_windows_8.1 <- function()
{
  is_windows_version("8.1")
}

#' @rdname is_windows
#' @export
is_windows_10 <- function()
{
  is_windows_version("10")
}

#' @rdname is_windows
#' @export
is_windows_server_2008 <- function()
{
  is_windows_version("Server 2008")
}

#' @rdname is_windows
#' @export
is_windows_server_2008_r2 <- function()
{
  is_windows_version("Server 2008 R2")
}

#' @rdname is_windows
#' @export
is_windows_server_2012 <- function()
{
  is_windows_version("Server 2012")
}

#' @rdname is_windows
#' @export
is_windows_server_2012_r2 <- function()
{
  is_windows_version("Server 2012 R2")
}

#' @rdname is_windows
#' @export
is_windows_server_2016 <- function()
{
  is_windows_version("Server 2016")
}

#' @rdname is_windows
#' @export
is_windows_server_2019 <- function()
{
  is_windows_version("Server 2019")
}

#' Failure for bad OS
#' 
#' Wrapper to \code{false} for failure messages when the OS is not as 
#' expected.
#' @param os A string giving the name of the OS that was desired.
#' @return A string showing the results of \code{.Platform$OS} and 
#' \code{Sys.info()['sysname']}.
#' @seealso \code{\link[base]{.Platform}} and \code{\link[base]{Sys.info}}
#' @examples
#' \donttest{
#' assertive.reflection:::not_this_os("Windows")
#' assertive.reflection:::not_this_os("BSD-based")
#' }
#' @noRd
not_this_os <- function(os)
{
  false(
    gettext(
      "The operating system is not %s. R reports it as: Sys.info()['sysname'] = %s, .Platform$OS = %s."
    ), 
    os, 
    Sys.info()["sysname"],
    .Platform$OS
  )
}

is_windows_version <- function(version)
{
  if(!(ok <- is_windows()))
  {
    return(ok)
  }
  # wmic available on Windows 2000 onwards, but for safety also allow
  # (slower) alternative via systeminfo
  windows_name_text <- tryCatch(
    trimws(shell("wmic os get caption", intern = TRUE)[2]),
    error = function(e) {
      sub("OS Name:\\s+", "", shell('systeminfo | findstr /B /C:"OS Name"'))
    }
  )
  windows_version <- sub(
    "Windows (10|8.1|8\\b|7|Vista|Server 2019|Server 2016|Server 2012|Server 2012 R2|Server 2008 R2|Server 2008).*", 
    "\\1",
    windows_name_text
  )
  if(windows_version != version)
  {
    return(
      false(
        gettext("The operating system is not Windows %s. R reports it as: Windows %s."), 
        version,
        windows_version
      )
    )
  }
  TRUE
}

is_osx_version <- function(version)
{
  if(!(ok <- is_mac()))
  {
    return(ok)
  }
  mac_version_text <- system("sw_vers -productVersion", intern = TRUE)
  mac_version <- unlist(as.numeric_version(mac_version_text))
  major_version <- mac_version[1]
  minor_version <- mac_version[2]
  actual_os_details <- apple_os_data[
    apple_os_data$major_version == major_version & apple_os_data$minor_version == minor_version, 
  ]
  expected_os_details <- apple_os_data[apple_os_data$version_name == version, ]
  if(version != actual_os_details$version_name)
  {
    return(
      false(
        gettext("The operating system is not %s %s. R reports it as: %s %s."), 
        expected_os_details$os_name,
        expected_os_details$version_name, 
        actual_os_details$os_name,
        actual_os_details$version_name
      )
    )
  }
  TRUE
}

apple_os_data <- data.frame(
  major_version = rep(c(10, 11), c(16, 1)),
  minor_version = c(0:15, 0),
  os_name = rep.int(c("OS X", "macOS"), c(12, 5)),
  version_name = c(
    "Cheetah", "Puma", "Jaguar", "Panther", "Tiger", 
    "Leopard", "Snow Leopard", "Lion", "Mountain Lion", "Mavericks", 
    "Yosemite", "El Capitan", "Sierra", "High Sierra", "Mojave", 
    "Catalina", "Big Sur"
  ),
  stringsAsFactors = FALSE
)
