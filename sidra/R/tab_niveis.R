#' Obtenção de dados via API SIDRA - IBGE
#'
#' Esta função retorna uma lista com níveis territoriais disponíveis
#' de uma das tabelas da SIDRA.
#' @param tabela Número da tabela.
#' @return Um `data.frame` (especificamente, um `data.table`) que lista todas as
#'  localidades disponíveis para a tabela, detalhando o ID e o nome de cada
#'  localidade, bem como o ID e o nome do nível geográfico correspondente
#'  (ex: 'N3' para "Unidade da Federação").
#' @keywords IBGE SIDRA dados localidade
#' @export
#' @examples
#' niveis_ipca15 <- tab_niveis(1705)
#' tab_niveis(1705) # imprime os níveis territoriais da tabela solicitada

tab_niveis <- \(tabela) {
  niveis <- tab_meta(tabela)$nivelTerritorial
  if (is.null(niveis)) {
    return(invisible(NULL))
  }
  niveis <- rev(niveis[!grepl("N7",niveis)])
  baseref <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/", tabela)

  rota <- paste0(baseref,"/localidades/",paste0(niveis,collapse="|"))

  resp <- call_ibge(httr::GET(rota,config = httr::timeout(2)))
  if (is.null(resp)) {
    return(invisible(NULL))
  }

  nivtab <- httr::content(resp)

    flatniv <- \(x){
    f <- cbind(nivtab[[x]][-3],as.data.frame(t(unlist(nivtab[[x]]$nivel))))
    f[1] <- as.numeric(f[1])
    names(f)[3:4] <- paste0("nivel.",names(f)[3:4])
    f
  }

  nivtab <- data.table::rbindlist(lapply(1:length(nivtab),flatniv))

  nivtab
}
