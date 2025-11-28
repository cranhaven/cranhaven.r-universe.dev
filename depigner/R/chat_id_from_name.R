chat_id_from_name <- function(.title = NA) {
  if (is.null(getOption("depigner.bot"))) {
    ui_stop(
      "Bot not set up. Please run `start_bot_for_chat()` first"
    )
  }

  bot_updates <- getOption("depigner.bot")[["get_updates"]]()
  bot_message <- purrr::map(bot_updates, "message")
  bot_message <- bot_message[!purrr::map_lgl(bot_message, is.null)]
  bot_chats <- purrr::map_df(bot_message, ~ dplyr::tibble(
    id = .x[["chat"]][["id"]],
    title = ifelse(is.null(.x[["chat"]][["title"]]),
      NA_character_,
      .x[["chat"]][["title"]]
    ),
    first_name = ifelse(is.null(.x[["chat"]][["first_name"]]),
      NA_character_,
      .x[["chat"]][["first_name"]]
    )
  ))


  if (is.na(.title)) {
    return(
      dplyr::filter(bot_chats, is.na(.title))[["id"]] %>%
        unique()
    )
  }

  res <- dplyr::filter(bot_chats, .data[["title"]] == .title)[["id"]] %>%
    unique()

  if (length(res) == 0L) {
    ui_stop(c(
      "The chat name {.title} provided does not exist in the chat for ",
      "which your bot has access."
    ))
  }

  res
}
