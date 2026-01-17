
linhas_pesquisa <- function(xml_data, nome_instituicao){

  vlinhasp <- NULL
  # identifica as tags de atuacao profissional
  atuacao_prof <- xml_data$"DADOS-GERAIS"$"ATUACOES-PROFISSIONAIS"
  if(!is.null(atuacao_prof)){
    # para cada atuacao ...
    for(i in 1:length(atuacao_prof)){
      # se for uma lista, prossiga ...
      if(is.list(atuacao_prof[[i]])){
        # verifica a instituicao  ...
        if(str_to_lower(atuacao_prof[[i]]$.attrs["NOME-INSTITUICAO"]) %in% nome_instituicao){
          # recupera as tags de pesquisa e desenvolvimento ...
          linhasp <- atuacao_prof[[i]]$"ATIVIDADES-DE-PESQUISA-E-DESENVOLVIMENTO"$"PESQUISA-E-DESENVOLVIMENTO"
          # e filtra as referentes ah linha de pesquisa
          linhasp <- linhasp[names(linhasp) == "LINHA-DE-PESQUISA"]

          if(!is.null(linhasp) & length(linhasp)!=0){
            # para cada linha de pesquisa, recupera o nome
            for(j in 1:length(linhasp)){
              if(typeof(linhasp[[j]]) == "list"){
                vlinhasp <- c(vlinhasp, linhasp[[j]]$.attrs["TITULO-DA-LINHA-DE-PESQUISA"])
              } else{
                vlinhasp <- c(vlinhasp, linhasp[[j]]["TITULO-DA-LINHA-DE-PESQUISA"])
              }
            }
          }
        }
      }
    }
  }

  if(!is.null(vlinhasp)){
    linhasp <- NULL
    n <- length(vlinhasp)
    if(n > 1){
      for(i in 1:n) linhasp <- paste0(linhasp, "; ", vlinhasp[i])
      linhasp <- str_sub(linhasp, 3, str_length(linhasp))
    } else{
      linhasp <- vlinhasp
    }
    linhasp <- linhasp
  } else {
    linhasp <- ""
  }

  linhasp
}
