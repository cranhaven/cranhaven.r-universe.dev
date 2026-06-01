#' Tabula Problemas Detectados nos Nomes
#'
#' @description
#' Cria uma tabela resumo contabilizando o número de ocorrências para cada
#' tipo de problema detectado pela função `marcar_problemas_e_limpar_nomes`.
#'
#' @param d O `data.table` retornado por `marcar_problemas_e_limpar_nomes`.
#' @param s O nome da coluna original (string) que foi processada.
#'
#' @return Um `data.table` com as colunas:
#'   \itemize{
#'     \item `condition`: O nome da condição/problema verificado.
#'     \item `N_detected`: Número de vezes que a condição foi detectada.
#'     \item `N_made_NA`: Número de detecções que resultaram na limpeza para `NA`.
#'     \item `N_replaced`: Número de detecções onde o nome foi alterado (não para `NA`).
#'   }
#' @export
#' @import data.table
#' @examples
#' DT_limpo <- data.table::data.table(nome = c("JOSEE SILVA", 
#' "RAIMUNDA DA DA SILVA"), nome_clean = c("JOSE SILVA",
#' "RAIMUNDA DA SILVA"),
#' falecido = NA, cartorio = NA, 
#' espaco_TilAcentoApostrofe = NA, 
#' nome_P_M_S_N = NA, nada_nao = NA, 
#' nada_nao_consta2 = NA, final_missing = NA, Xartigo = NA, sr_sra = NA,
#' ignorado = NA, dededada = 1, letra_repetida = 1)
#' sumario <- tabular_problemas_em_nomes(DT_limpo, "nome")
#' print(sumario)
tabular_problemas_em_nomes <- function(d, s) {
  # Determine the name of the cleaned variable (e.g., if s = "nome", then s_clean = "nome_clean")
  s_clean <- paste0(s, "_clean")
  
  # Build a named list of conditions (each condition is a logical vector)
  cond_list <- list(
    falecido            = d$falecido == 1,
    cartorio            = d$cartorio == 1,
    espaco_TilAcentoApostrofe = d$espaco_TilAcentoApostrofe == 1,
    nome_P_M_S_N        = d$nome_P_M_S_N == 1,
    nada_nao            = d$nada_nao == 1,
    consta              = d$consta == 1,
    nada_nao_consta     = d$nada_nao_consta == 1,
    nada_nao_consta2    = d$nada_nao_consta2 == 1,
    final_missing       = (d$nada_nao == 1 | d$consta == 1) & is.na(d[[s_clean]]),
    Xartigo             = d$Xartigo == 1,
    sr_sra              = d$sr_sra == 1,
    ignorado            = d$ignorado == 1,
    dede_dada           = d$dede_dada == 1,
    letra_repetida      = d$letra_repetida == 1
  )
  
  # For each condition, compute:
  # - N_detected: count of rows where the condition is TRUE.
  # - N_made_NA: count of those rows where the cleaned column is NA.
  # - N_replaced: count of rows where both the original and cleaned values are non-NA and different.
  summary_table <- data.table::rbindlist(lapply(names(cond_list), function(cond_name) {
    cond_vec <- cond_list[[cond_name]]
    N_detected <- sum(cond_vec, na.rm = TRUE)
    N_made_NA  <- sum(cond_vec & is.na(d[[s_clean]]), na.rm = TRUE)
    N_replaced <- sum(
      cond_vec &
        !is.na(d[[s]]) & 
        !is.na(d[[s_clean]]) &
        (d[[s]] != d[[s_clean]]),
      na.rm = TRUE
    )
    data.table::data.table(
      condition  = cond_name,
      N_detected = N_detected,
      N_made_NA  = N_made_NA,
      N_replaced = N_replaced
    )
  }))
  
  return(summary_table)
}

#' @rdname tabular_problemas_em_nomes
#' @export
tabulate_name_poblems <- tabular_problemas_em_nomes