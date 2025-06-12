completed <- function(msg) {
  packageStartupMessage(crayon::green(cli::symbol$tick), " ", msg)
}

not_completed <- function(msg) {
  packageStartupMessage(crayon::red(cli::symbol$circle_cross), " ", msg)
}

notify <- function(msg) {
  packageStartupMessage(crayon::blue(cli::symbol$star), " ", msg)
}
