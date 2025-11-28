.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Welcome to depigner: we are here to un-stress you!"
  )
}


.onLoad <- function(libname, pkgname) {
  op <- options()

  op_depigner <- list(
    depigner.dev.test_telegram_bot = FALSE,

    depigner.bot_name = NULL,
    depigner.chat_id = NULL
  )

  toset <- !(names(op_depigner) %in% names(op))

  if (any(toset)) options(op_depigner[toset])

  invisible(TRUE)
}
