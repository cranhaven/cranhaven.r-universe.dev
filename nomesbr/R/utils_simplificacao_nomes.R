#' Remove Partículas, Agnomes e algumas Patentes de Nomes
#' @param s Vetor de caracteres contendo nomes.
#' @return Vetor de caracteres com nomes simplificados.
#' @export
#' @importFrom stringr str_replace_all
#' @examples
#' vct_nomes <- c("JOAO DA SILVA FILHO","CORONEL JACINTO")
#' remove_PARTICULAS_AGNOMES(vct_nomes)
#' 
remove_PARTICULAS_AGNOMES <- function(s){
  s |> stringr::str_replace_all(regex_nome_EDADEDOS, " ") |> 
    stringr::str_replace_all(regex_nome_AGNOMES, '')|>
  stringr::str_replace_all(regex_DR_CORONEL,'')
  
  
  
  
}

#' Cria coluna com agnomes, algumas patentes/cargos as remove, remove partículas
#' @param d um objeto `data.table`
#' @param s string com nome da coluna de caracteres contendo nomes para 
#' simplificar. Por padrão, "nome_clean".
#' @return data.table com novas colunas de nome simplificado e de marca  
#' agnomes_titulos
#' @export
#' @import data.table
#' @importFrom stringr str_replace_all str_extract
#' @examples
#' dt_nomes <- data.table::data.table(nome = c("JOAO DA SILVA FILHO",
#' "CORONEL JACINTO"))
#' dt_nomes <- simplifica_PARTICULAS_AGNOMES_PATENTES(d=dt_nomes,s="nome")
#' print(dt_nomes)
#' 
simplifica_PARTICULAS_AGNOMES_PATENTES <- function(d,s="nome_clean"){
  tictoc::tic('Starting - All substeps')
  tictoc::tic('0. Making copy of dataset and add the s2(the var to be cleaned)')
  #Remove _clean
  s2 <- paste0(gsub("_clean","",s), "_simp")
  d <- data.table::copy(d)
  d[, (s2) := get(s)]
  tictoc::toc()
  
  tictoc::tic('1. remove particles e da de do(s)')
  d[,(s2):= stringr::str_replace_all(get(s2),regex_nome_EDADEDOS, " ") ]
  tictoc::toc()
  
  tictoc::tic("2. Detect and remove AGNOMES and titles")
  d[,agnomes_titulos:=
      stringr::str_extract(get(s2),paste0("(",regex_nome_AGNOMES,
                                         "|",regex_DR_CORONEL,")"))|>
      stringr::str_trim('both')]
  
  d[!is.na(agnomes_titulos),(s2):=
      stringr::str_replace_all(get(s2),regex_nome_AGNOMES, '')|>
      stringr::str_replace_all(regex_DR_CORONEL,'')]
  tictoc::toc()
  
  d[,(s2):=str_trim(get(s2),'both')]
  
  tictoc::toc()
  
  return(d)
  
  
}



#' Adiciona Colunas com Partes do Nome (w1, w2, w3, w2p, w12p)
#' @param dt Um `data.table`.
#' @param s Nome da coluna (string) em `dt` contendo os nomes.
#' @return O `data.table` `dt` modificado por referência, com novas colunas.
#' @export
#' @import data.table
#' @importFrom stringr str_detect
#' @examples
#' dt_nomes <- data.table::data.table(nome=c("MARIA DO SOCORRO SILVA",
#' "ANA PAULA DE OLIVEIRA"))
#' dt_nomes <- segmentar_nomes(dt_nomes,"nome")
#' print(dt_nomes)
#' 
segmentar_nomes <- function(dt, s){
  s1     <- paste0(s, "_w1")
  s2     <- paste0(s, "_w2")
  s3     <- paste0(s, "_w3")
  s2p    <- paste0(s, "_w2p")
  s12p   <- paste0(s, "_w12p")
  dt[, c(s1, s2, s3) := data.table::tstrsplit(get(s), " ", fixed=TRUE, keep=1:3)]
  dt[!is.na(get(s2)) &  stringr::str_detect(get(s2), regex_nome_EDADEDOS2) & !is.na(get(s3)),  (s2p) := paste( get(s2), get(s3), sep = " ")]
  dt[!is.na(get(s2)) & !stringr::str_detect(get(s2), regex_nome_EDADEDOS2)                , (s2p) := get(s2)]
  dt[ is.na(get(s2)) , (s12p) :=        get(s1)]
  dt[!is.na(get(s2)) , (s12p) := trimws(paste( get(s1), get(s2p), sep = " "))]
  
  return(invisible(dt))
}

#' @rdname segmentar_nomes
#' @export
add_string_w1_w2_w3_and_w2p <- segmentar_nomes



#' Adiciona Nome Próprio Validado de `nomes_proprios_compostos` .
#' @param dt Um `data.table`.
#' @param s Nome da coluna (string) base para derivação das colunas de palavras
#' (por exemplo, se `s = "nome_simpl"`, espera `nome_simpl1`, `nome_simpl2p`).
#' @return O `data.table` `dt` com colunas `_v2` adicionadas.
#' @export
#' @import data.table
#' @importFrom stringr str_detect
#' @examples
#' \donttest{
#' dt_nomes <- data.table::data.table(nome=c("MARIA DO SOCORRO SILVA",
#' "ANA PAULA DE OLIVEIRA","JOSE DAS FLORES"))
#' dt_nomes <- identificar_adicionar_nome_proprio(dt_nomes,"nome")
#' print(dt_nomes)
#' }
identificar_adicionar_nome_proprio <- function(dt, s) {
  np2 <- readRDS(obter_dic_nomes_proprios_compostos())
  # Build dt key names
  s1     <- paste0(s, "1")
  s2p    <- paste0(s, "2p")
  s1_v2  <- paste0(s, "1_v2")
  s2p_v2 <- paste0(s, "2p_v2")
  
  # Separate the first three words from column s into new columns
  dt[, c(s1, "word2", "word3") := tstrsplit(get(s), " ", fixed = TRUE, keep = 1:3)]
  
  # Create s2p based on conditions and your regex (regex_nome_EDADEDOS2 must be defined)
  dt[!is.na(word2) & !is.na(word3) & stringr::str_detect(word2, regex_nome_EDADEDOS2), 
     (s2p) := paste(get(s1), word2, word3, sep = " ")]
  dt[!is.na(word2) & !stringr::str_detect(word2, regex_nome_EDADEDOS2), 
     (s2p) := paste(get(s1), word2, sep = " ")]
  
  # Remove temporary columns
  dt[, c("word2", "word3") := NULL]
  
  # --- Rename np2 Variables ---
  np2_names <- names(np2)
  np2_candidate1  <- np2_names[stringr::str_detect(np2_names, "1$")]
  np2_candidate2p <- np2_names[stringr::str_detect(np2_names, "2p$")]
  if (length(np2_candidate1) == 0 || length(np2_candidate2p) == 0) stop("Could not find matching key columns in np2")
  if (length(np2_candidate1) > 1 || length(np2_candidate2p) > 1  )   stop("More than one variable found for key columns ending with '1' or '2p'")
  np2_key1  <- np2_candidate1[1]
  np2_key2p <- np2_candidate2p[1]
  setnames(np2, old = c(np2_key1, np2_key2p), new = c(s1, s2p)) #rename key people
  #print(names(np2))  # For debugging: print new names of np2
  
  dt <- np2[dt, on = c(s1, s2p)]
  
  
  # Create "_v2" columns: if the joined column 'nome_proprio' matches one of the keys, use it; otherwise, keep original.
  dt[, (s1_v2) := ifelse(!is.na(nome_proprio) & (nome_proprio == get(s1) | nome_proprio == get(s2p)),
                         nome_proprio, get(s1))]
  dt[, (s2p_v2) := ifelse(!is.na(nome_proprio) & (nome_proprio == get(s1) | nome_proprio == get(s2p)),
                          nome_proprio, get(s2p))]
  
  return(dt)
}

#' @rdname identificar_adicionar_nome_proprio
#' @export

add_nome_proprio_to_word1_and_word2p <- identificar_adicionar_nome_proprio


