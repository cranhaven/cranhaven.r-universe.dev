#' Divert output errors to the telegram bot
#'
#' @param chat_name a
#' @param bot_name a
#'
#' @return a
#' @export
#'
#' @examples
#' if (interactive()) {
#'   library(depigner)
#'   errors_to_telegram()
#' }
errors_to_telegram <- function(
   chat_name = Sys.getenv("R_telegram_error_chat_name"),
   bot_name = getOption("depigner.bot_name")
) {
  if (is.null(getOption("depigner.bot"))) {
    start_bot_for_chat(chat_name, bot_name)
  }

  has_error_handler <- !is.null({
    op <- getOption("error")
  })

  telegram_error <- function() {
    msg <- function() geterrmessage()
    send_to_telegram(msg())
  }

  new_error_handler <- function() {
    telegram_error()
    if (has_error_handler) eval(op)
  }

  options(error = new_error_handler)

  if (has_error_handler) {
    ui_warn(
      "Error handler is changed\nfrom: {op}\n to: {getOption('error')}"
    )
  }
}
