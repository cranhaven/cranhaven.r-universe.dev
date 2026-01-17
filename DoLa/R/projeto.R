
projeto <- function(id, xml_data,
                       ano_ini, ano_fim){

  producao_projeto <- NULL

  pprojeto <- xml_data$"DADOS-GERAIS"$"ATUACOES-PROFISSIONAIS"
  n1 <- length(pprojeto)

  if(n1 > 0){
    # para todos os itens em ATUACOES-PROFISSIONAIS
    for(i in 1:n1){

      n2 <- length(pprojeto[[i]])

      # verifica se eh ATIVIDADES-DE-PARTICIPACAO-EM-PROJETO
      for(l in (1:n2)[names(pprojeto[[i]]) == "ATIVIDADES-DE-PARTICIPACAO-EM-PROJETO"]){

        p <- pprojeto[[i]][[l]]
        n3 <- length(p)

        # para cada PARTICIPACAO-EM-PROJETO
        for(j in (1:n3)){

          # verifica se eh PROJETO-DE-PESQUISA
          if("PROJETO-DE-PESQUISA" %in% names(p[[j]])){

            ano <- as.vector(p[[j]]$".attrs"["ANO-FIM"])

            if((ano >= ano_ini & ano <= ano_fim) | (ano == "")){
              ano_i <- as.vector(p[[j]]$".attrs"["ANO-INICIO"])

              # verifica se o proprietario do Lattes eh coordenador
              fl_responsavel <- "NAO"
              ep <- p[[j]]$"PROJETO-DE-PESQUISA"$"EQUIPE-DO-PROJETO"
              if((n4 <- length(ep)) > 0){
                for(k in 1:n4){
                  if(as.vector(ep[[k]]["FLAG-RESPONSAVEL"] == "SIM")) {
                    if(!is.na(ep[[k]]["NRO-ID-CNPQ"])){
                      if((as.vector(ep[[k]]["NRO-ID-CNPQ"]) == id)) {
                        fl_responsavel <- "SIM"
                      }
                    }
                  }
                  # OCAMPO NRO-ID-CNPQ NEM SEMPRE EXISTE
                  #if((as.vector(ep[[k]]["NRO-ID-CNPQ"]) == id)){
                  #  fl_responsavel <- as.vector(ep[[k]]["FLAG-RESPONSAVEL"])
                  #}
                }
              }

              # verifica se o projeto eh financiado por agencia de fomento
              fl_financiado <- "NAO"
              financiadores <- NULL
              fp <- p[[j]]$"PROJETO-DE-PESQUISA"$"FINANCIADORES-DO-PROJETO"
              if((n4 <- length(fp)) > 0){
                fl_financiado <- "SIM"
                for(k in 1:n4){
                  if(k == 1){
                    financiadores <- fp$"FINANCIADOR-DO-PROJETO"["NOME-INSTITUICAO"]
                  } else{
                    financiadores <- paste0(financiadores, "; ", fp$"FINANCIADOR-DO-PROJETO"["NOME-INSTITUICAO"])
                  }
                }
              }
              if(is.null(financiadores)){
                financiadores = ""
              }


              tipo <- as.vector(p[[j]]$"PROJETO-DE-PESQUISA"$".attrs"["DESCRICAO-DO-PROJETO"])
              if(!is.null(tipo)){ # se diferente de NULL, recupera a primeira linha
                # tipo <- str_split(tipo[1],"\n")[[1]][1]
                tp <- NA
                for(niv in c("GPDP", "GPII", "GPEC")){
                  tp <- str_extract(tipo, niv)[1]
                  if(!is.na(tp)) break
                }
                if(is.na(tp)){
                  tipo <- ""
                } else tipo <- tp
              }
              if(str_length(tipo) > 6){
                tipo <- ""
              }


              nome <- as.vector(p[[j]]$"PROJETO-DE-PESQUISA"$".attrs"["NOME-DO-PROJETO"])

              # pontua quando for instituicao/curso definidos
              pontos <-1
              # tipo <- gsub(">", "", gsub("<", "", tipo))

              ap <- data.frame(
                id = id,
                ano_ini = ano_i, # acrescentei o ano_ini
                ano_fim = ano,
                nome = nome,
                coordenador = fl_responsavel,
                financiado = fl_financiado,
                financiador = financiadores,
                pontos = pontos
              )

              producao_projeto <- rbind(producao_projeto, ap, row.names = NULL)

            }
          }
        }
      }
    }
  }


  #####################################
  # insere linha de totais
  ap <- data.frame(
    id = id,
    ano_ini = "",
    ano_fim = "",
    nome = "",
    coordenador = "",
    financiado = "",
    financiador = "TOTAL",
    pontos = sum(producao_projeto$pontos, na.rm = TRUE)
  )
  producao_projeto <- rbind(producao_projeto, ap, row.names = NULL)

  # retorna o dataframe
  producao_projeto
}
