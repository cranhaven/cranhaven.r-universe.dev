.onAttach <- function(...) {
  attached <- redistverse_attach()
  inform_startup(redistverse_attach_message(attached))
}
