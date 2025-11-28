#' Send something to telegram
#'
#' This is a generic function to send some object to telegram.
#'
#' By default it use the bot and chat_id configured by
#' \code{\link{start_bot_for_chat}}. The user can pass a custom bot or
#' chat_id providing them to the corresponding argument.
#'
#'
#' @param chat_id chat_id in which send the object. By default all the
#'        methods consider the chat_id defined in the previous run of
#'        \code{\link{start_bot_for_chat}}.
#' @param x object to send (often a character string)
#' @param ... further argument to pass to the sending methods (see
#'        \code{\link[telegram.bot]{Bot}} for specifications.)
#' @param bot (\code{\link[telegram.bot]{Bot}}) the bot object.
#'        By default all the methods consider the bot created by a
#'        previous run of \code{\link{start_bot_for_chat}}.
#'
#' @return invisible the object x.
#' @export
#' @examples
#' \dontrun{
#'   library(depigner)
#'   library(ggplot2)
#'
#'   start_bot_for_chat()
#'   send_to_telegram("hello world")
#'
#'   gg <- ggplot(mtcars, aes(x = mpg, y = hp, colour = cyl)) +
#'   geom_point()
#'
#'   send_to_telegram(
#'     "following an `mtcars` coloured plot",
#'     parse_mode = "Markdown"
#'   )
#'   send_to_telegram(gg)
#' }
send_to_telegram <- function(x, ..., chat_id, bot) {
  UseMethod("send_to_telegram", x)
}


#' Send message or general object to telegram
#'
#' @rdname send_to_telegram
#' @param type (chr, default = "message") the type of text represent
#'       \code{x}. I.e., if not "message", it is normally considered as
#'       a path to the corresponding object. For further clarification
#'       see \code{\link[telegram.bot]{Bot}} help page.
#'
#' @export
send_to_telegram.character <- function(
  x,
  type = c(
    "message", "photo", "document", "audio", "animation", "video",
    "voice", "sticker", "location", "videonote"
  ),
  ...,
  chat_id = getOption("depigner.chat_id"),
  bot = getOption("depigner.bot")
) {
  check_for_bot_options(chat_id, bot)

  type <- match.arg(type)
  type_of_message <- paste0("send_", type)

  bot[[type_of_message]](chat_id = chat_id, x, ...)
  invisible(x)
}

#' Send ggplot plots to telegram
#'
#' @rdname send_to_telegram
#' @param fileext (chr, default "png") one of the possible file format
#'        supported: "png", "pdf", "jpeg", "tiff" or "bmp"
#'
#' @export
send_to_telegram.gg <- function(
  x,
  fileext = c("png", "pdf", "jpeg", "tiff", "bmp"),
  ...,
  chat_id = getOption("depigner.chat_id"),
  bot = getOption("depigner.bot")
) {
  check_for_bot_options(chat_id, bot)

  fileext <- match.arg(fileext)
  tmp <- tempfile(fileext = paste0(".", fileext))

  ggplot2::ggsave(tmp, x)
  bot[["send_photo"]](chat_id = chat_id, photo = tmp)
  unlink(tmp)
  invisible(x)
}
