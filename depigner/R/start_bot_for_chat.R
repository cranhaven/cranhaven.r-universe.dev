#' Set up a Telegram bot
#'
#' This function set up what is necessary to \code{\link{telegram.bot}}
#' package to be used in a more easy way by the function provided by
#' the depigner.
#'
#' @details
#' Before you can use the \code{\link{depigner}} facilities (or the
#' \code{\link{telegram.bot}} ones) to use your bot to chat with
#' Telegram from R, you have to set the bot up.
#'
#' To set up a bot in telegram, find \code{@BotFather} on telegram. Send
#' the message \code{\\start} to it, and then send the message
#' \code{\\newbot} to it too. Next you have to follow the very simple
#' instruction it gives you. At the end of the process, save your bot
#' token and never share it publicly!!
#'
#' After your bot is created, go to your bot default chat profile and
#' send the message \code{\\start}.
#'
#' Now you can return to R and put both the bot's name and token into
#' the .Renviron file. To access to it you can use
#' \code{\link[usethis]{edit}_r_environ} which will open the
#' `.Renviron` file, ready to be modified.
#'
#' You need to insert two lines, namely the one for your bot's name:
#'
#' `R_telegram_bot_name=<name_of_my_bot>`
#'
#' and one for its token:
#'
#' `R_TELEGRAM_BOT_<name_of_my_bot>="1234567879:AbcD..."
#'
#' Next, restart R and you are ready to use al the (simple)
#' functionality of the \code{\link{depigner}} package, or the flexible
#' and complete ones from the \code{\link{telegram.bot}} package.
#'
#' @param chat_name (chr, NA) The name of the chat you want to the bot
#'     send.is linked to. If NA (default) it uses the \emph{default}
#'     chat of the bot.
#'
#' @param bot_name (chr, NULL) This argument should be left NULL. If
#'     NULL, the function bring the bot name from the environmental
#'     variable "R_telegram_bot_name". You can pass another bot's name
#'     here as a character string too (note that in this case in the
#'     .Renviron you must have an entry like
#'     `R_TELEGRAM_BOT_<yourbotname>=......` containing the token
#'     related to the bot).
#'
#' @return invisible()
#' @export
#'
#' @examples
#' \dontrun{
#'   library(depigner)
#'   start_bot_for_chat()
#' }
start_bot_for_chat <- function(
  chat_name = Sys.getenv("R_telegram_default_chat_name"),
  bot_name = getOption("depigner.bot_name")
) {
  if (is.null(bot_name)) {
    if (Sys.getenv("R_telegram_bot_name") == "") {
      ui_stop(
        ".Renviron variable {ui_field('R_telegram_bot_name')} is required
        if {ui_field('bot_name')} argument is not provided directly from
        the user.

        Please, set it up, or pass the name of your bot, directly
        to the function {ui_code('prepare_telegram_bot()')}.
      "
      )
    }
    bot_name <- Sys.getenv("R_telegram_bot_name")
  }

  bot <- telegram.bot::Bot(token = telegram.bot::bot_token(bot_name))
  options(depigner.bot = bot)

  chat_id <- tryCatch(chat_id_from_name(chat_name),
    error = function(e) {
      ui_stop("
  The bot {ui_field(bot_name)} does not have a {ui_value(chat_name)} chat.
  Have you provided a {ui_field(chat_name)} of a chat in which the bot is
  a member?
      ")
    }
  )

  options(depigner.chat_id = chat_id)

  chat_name <- ifelse(is.na(chat_name), "default", chat_name)
  ui_line(
    "Bot {ui_value(bot_name)} is now linked to the {ui_value(chat_name)}
      chat (ID: {ui_value(chat_id)})."
  )

  invisible()
}
