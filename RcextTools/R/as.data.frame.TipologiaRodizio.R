#' Metodo S3 que retorna a representacao rm `data.frame` de um objeto da classe TipologiaRisco
#' @author Bruno M. S. S. Melo
#' @description Este metodo simplesmente retorna o atributo `tabela` de um objeto da classe TipologiaRisco.
#' @param x objeto da classe `TipologiaRodizio`.
#' @param ... eventuais argumentos adicionais.
#' @examples
#' \dontrun{
#' casosSuspeitos <- TipologiaRodizio(dados)
#' dfCasosSuspeitos <- as.data.frame.TipologiaRodizio(casosSuspeitos)
#' }
#' @export
as.data.frame.TipologiaRodizio <- function(x, ...){
  return(x$tabela)
}
