#' Metodo S3 que imprime na tela um data.frame que representa um objeto da classe `TipologiaRodizio`
#' @author Bruno M. S. S. Melo
#' @param x objeto da classe `TipologiaRodizio`
#' @param ...	eventuais parametros adicionais.
#' @examples
#' \dontrun{
#' casosSuspeitos <- TipologiaRodizio(dados)
#' print(casosSuspeitos)
#' }
#' @export
print.TipologiaRodizio <- function (x, ...){
  print(x$tabela)
}
