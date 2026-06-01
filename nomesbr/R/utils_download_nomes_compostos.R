#' Carrega o dataset nomes_proprios_compostos 
#' Verifica se o dataset nomes_proprios_compostos existe em uma pasta de cache local.
#' Se inexistir, baixa-o de um release do GitHub e o salva no cache.
#' @return Um data.table contendo os dados de nomes_proprios_compostos.
#' @keywords internal
#' @importFrom tools R_user_dir
#' @importFrom utils download.file
#' @importFrom httr2 request req_perform resp_body_raw req_retry req_error req_user_agent req_progress
obter_dic_nomes_proprios_compostos <- function() {
  pkg_name <- "nomesbr" 
  cache_dir <- tools::R_user_dir(package = pkg_name, which = "cache")
  nomes_proprios_compostos_file_path <- file.path(cache_dir, "nomes_proprios_compostos.rds")
  
  # URL do arquivo nomes_proprios_compostos.rds no GitHub Release
  # Substitua pela URL real do seu release!
  nomes_proprios_compostos_url <- "https://github.com/ipeadata-lab/nomesbr/releases/download/v0.0.1-alpha/nomes_proprios_compostos.rds"
  
  if (!file.exists(nomes_proprios_compostos_file_path)) {
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }
    message(paste0("Baixando o arquivo de dados nomes_proprios_compostos.rds (aprox. 109MB).\n",
                   "Isso vai acontecer apenas uma vez.\n",
                   "Salvando em: ", nomes_proprios_compostos_file_path))
    
    # Construir a requisição com httr2
    req <- httr2::request(nomes_proprios_compostos_url) |>
      httr2::req_user_agent(paste0(pkg_name, " (https://github.com/ipeadata-lab/nomesbr)")) 
    
    req <- req|>
      httr2::req_retry(max_tries = 3, is_transient = \(resp) httr2::resp_status(resp) %in% c(401,403,404,429, 500, 502, 503, 504)) |> # Tentar novamente em caso de falhas temporárias
      httr2::req_error(is_error = function(resp) FALSE) # Lidar com erros manualmente abaixo
    
    # Usar tempfile para download seguro
    temp_file <- tempfile(fileext = ".rds")

    tryCatch({
      # Performar a requisição e salvar o corpo diretamente em um arquivo
      # req_perform_disk foi considerado, mas para salvar como RDS, melhor ler o corpo e escrever
      # httr2::req_perform(req, path = temp_file) # Isso salvaria o corpo diretamente
      
      message("Iniciando download...")
      resp <- httr2::req_perform(req) # verbosity = 0 para httr2::req_progress funcionar
      
      # Verificar se o request foi bem sucedido (status 2xx)
      if (httr2::resp_is_error(resp)) {
        httr2::resp_check_status(resp) # Isso vai gerar um erro com detalhes
      }
      
      # Salvar o corpo da resposta (binário) no arquivo temporário
      # writeBin garante que o conteúdo seja escrito como está
      writeBin(httr2::resp_body_raw(resp), temp_file)
      
      # Mover o arquivo baixado para o local de cache
      # file.rename pode falhar entre diferentes dispositivos/sistemas de arquivos
      # file.copy é mais robusto, depois remove o original
      if (file.copy(temp_file, nomes_proprios_compostos_file_path, overwrite = TRUE)) {
        file.remove(temp_file)
        message("Download finalizado e arquivo salvo no cache.")
      } else {
        file.remove(temp_file) # Remove o temp_file mesmo se a cópia falhar
        stop("Falha ao mover o arquivo baixado para a pasta de cache.", call. = FALSE)
      }
    }, error = function(e) {
      # Limpar o arquivo temporário em caso de erro
      if (file.exists(temp_file)) {
        file.remove(temp_file)
      }
      stop(paste0("Falha ao baixar o arquivo nomes_proprios_compostos.rds de ", nomes_proprios_compostos_url, "\nErro: ", e$message), call. = FALSE)
    })
  } else {
    
    message(paste0("Usando arquivo nomes_proprios_compostos.rds do cache: ", nomes_proprios_compostos_file_path))
  }
  

  return(nomes_proprios_compostos_file_path)
}