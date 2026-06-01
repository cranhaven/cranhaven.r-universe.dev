#' @keywords internal
limpa_espaco_acento_til_apostrofe <- function(c) {
  if(!inherits(c, 'character')) stop('c must be a character vector')
  c <- c |> 
    stringr::str_replace(regex_cAO, "\\1AO\\2") |>
    stringr::str_replace(regex__AO, "\\1CAO\\2") |> 
    stringr::str_replace(regex_J_AO, "\\1JOAO\\2") |>
    stringr::str_replace(regex_apostrofe, function(x) gsub(" ", "", x)) |> 
    stringr::str_replace(regex_d_a_falsopositivo_apostrofo,\(x) gsub("\\bD A","DA ",x))|>
    stringr::str_replace(regex_d_e_falsopositivo_apostrofo_melhora_sobrenome,\(x) gsub("\\bD E","DE ",x))|>
    stringr::str_replace(regex_d_o_falsopositivo_apostrofo,\(x) gsub("\\bD O","DO ",x))|>
    stringr::str_replace(regex_d_vogal_candidato_apostrofo,\(x) gsub(" ","",x))|>
    stringr::str_replace(regex_nome_acento, function(x) gsub(" ", "", x))
  
  return(c)
}


#' @keywords internal
# Função de substituição personalizada
resolver_sequencia_particulas <- function(match_completo) {
  # match_completo é a string inteira da sequência detectada, ex: "E DE DO"
  # Precisamos dos espaços originais para reconstruir corretamente
  # No entanto, para a lógica de decisão, apenas as partículas importam.
  
  # Extrair as partículas individuais da sequência
  particulas_na_sequencia <- stringr::str_extract_all(match_completo, regex_qualquer_particula)[[1]]
  
  
  # Lógica de Prioridade para manter a partícula "menos comum" / "mais específica"
  particula_a_manter <- NA_character_
  prioridade_maxima_encontrada <- -1
  
  for (p in particulas_na_sequencia) {
    posicao_na_lista_prioridade <- match(p, regex_base_nome_EDADOS) # regex_base_nome_EDADOS
    if (!is.na(posicao_na_lista_prioridade)) {
      # Como a lista está de MAIS ESPECÍFICA para MENOS ESPECÍFICA,
      # a primeira que encontrarmos com uma prioridade válida é a melhor até agora.
      # Se quisermos a que tem o MENOR índice (mais específica):
      if (is.na(particula_a_manter) || posicao_na_lista_prioridade < prioridade_maxima_encontrada) {
        particula_a_manter <- p
        prioridade_maxima_encontrada <- posicao_na_lista_prioridade
      }
    }
  }
  
  # Se por algum motivo nenhuma partícula válida for encontrada (improvável com a regex),
  # ou se a lógica falhar, retorne a primeira da sequência como fallback seguro,
  # ou a string original para evitar remoção indevida.
  if (is.na(particula_a_manter) || nchar(particula_a_manter) == 0) {
    if (length(particulas_na_sequencia) > 0) {
      return(particulas_na_sequencia[1]) # Fallback: manter a primeira
    } else {
      return(match_completo) # Fallback muito seguro
    }
  }
  
  return(particula_a_manter)
}
