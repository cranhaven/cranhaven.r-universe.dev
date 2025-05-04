
#'  Configuration of the BDF Proxy server
#'
#' ========================================================
#' =====  THIS FUNCTION IS FOR BDF INTERNAL USE ONLY  =====
#' ========================================================
#'
#' (It should not do anything if accidentally used outside of the BDF environment.)
#'
#' Cette fonction permet de configurer R pour se connecter au proxy BDF.
#' Rentrer son mot de passe INTRA et lancer cette fonction à chaque session.
#'
#' @keywords internal
#' @import getPass
#' @export

proxy_bdf <- function() {

  # DEMANDER MDP SI POSTE BDF
  if (nchar(Sys.getenv("BDF_OSVER")) > 0) {
    matr <- Sys.getenv("USERNAME")
    pwd <- getPass::getPass("Password")
    identifiants <- c(matr, pwd)
    full_matricule <- paste("intra\\", identifiants[1], sep = "")


    # CONFIGURER PROXY --------------------------------------------------------
    httr::set_config(httr::use_proxy(url = "serai2", port = 8080, username = full_matricule, password = identifiants[2], auth = "ntlm"))
    httr::set_config(httr::config(ssl_verifypeer = 0L))
  }
}
