.onAttach <- function(...) {
  packageStartupMessage("CENSO 2017\nLa documentacion del paquete y ejemplos de uso se encuentran en https://pacha.dev/censo2017/.\nVisita https://buymeacoffee.com/pacha si deseas donar para contribuir al desarrollo de este software.\nEsta libreria necesita 3.5 GB libres para la crear la base de datos localmente. Una vez creada la base, esta ocupa 1.0 GB en disco.\n")
  if (interactive() && Sys.getenv("RSTUDIO") == "1"  && !in_chk()) {
    censo_pane()
  }
  if (interactive()) censo_status()
}
