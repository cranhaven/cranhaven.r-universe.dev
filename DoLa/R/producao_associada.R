prodass <- function(id, xml_data,
                    este_doc_id_cit, doc_id_cit, dic_id_cit,
                    ano_ini, ano_fim){

  producao_associada = NULL
  pass <- xml_data$"DADOS-GERAIS"$"ATUACOES-PROFISSIONAIS"
  n1 <- length(pass)

  if(n1 > 0){
    # para todos os itens em ATUACOES-PROFISSIONAIS
    for(i in 1:n1){

      n2 <- length(pass[[i]])

      # verifica se eh ATIVIDADES-DE-PARTICIPACAO-EM-PROJETO
      for(l in (1:n2)[names(pass[[i]]) == "ATIVIDADES-DE-PARTICIPACAO-EM-PROJETO"]){

        p <- pass[[i]][[l]]
        n3 <- length(p)

        # para cada PARTICIPACAO-EM-PROJETO
        for(j in (1:n3)){

          # verifica se eh PROJETO-DE-PESQUISA
          if("PROJETO-DE-PESQUISA" %in% names(p[[j]])){

            ano <- as.vector(p[[j]]$".attrs"["ANO-FIM"])
            projeto = ""
            equipe = ""
            tipo = ""
            producao = ""

            if((ano >= ano_ini & ano <= ano_fim) | (ano == "")){
              projeto <- as.vector(p[[j]]$"PROJETO-DE-PESQUISA"$".attrs"["NOME-DO-PROJETO"])

              # verifica se ha docente e discente do programa na equipe do projeto
              ep <- p[[j]]$"PROJETO-DE-PESQUISA"$"EQUIPE-DO-PROJETO"
              if((n4 <- length(ep)) > 0){
                for(k in 1:n4){
                  docente = ""
                  discente = ""
#                  if(as.vector(ep[[k]]["NRO-ID-CNPQ"]) != as.vector(este_doc_id_cit) |
#                     as.vector(ep[[k]]["NOME-COMPLETO-DO-AUTOR"]) != as.vector(este_doc_id_cit) |
#                     as.vector(ep[[k]]["NOME-PARA-CITACAO"]) != as.vector(este_doc_id_cit)){

                  if(as.vector(ep[[k]]["NRO-ID-CNPQ"]) != as.vector(este_doc_id_cit[1])){


                    if(!is.na(ep[[k]]["NRO-ID-CNPQ"])){
#                      if(as.vector(ep[[k]]["NRO-ID-CNPQ"]) %in% as.vector(clattes$id)) { #### mudei aqui pro dola... ver se funciona?
                      if(as.vector(ep[[k]]["NRO-ID-CNPQ"]) %in% str_to_lower(doc_id_cit)) {
                          docente <- "Docente"
                      }
                      if(as.vector(ep[[k]]["NRO-ID-CNPQ"]) %in% as.vector(dic_id_cit)) {
                        discente <- "Discente"
                      }
                    }
                    if(str_to_lower(as.vector(as.vector(ep[[k]]["NOME-COMPLETO-DO-AUTOR"]))) %in% str_to_lower(doc_id_cit))
                      equipe <- "Docente"
                    if(str_to_lower(as.vector(as.vector(ep[[k]]["NOME-PARA-CITACAO"]))) %in% str_to_lower(doc_id_cit))
                      equipe <- "Docente"
                    if(str_to_lower(as.vector(as.vector(ep[[k]]["NOME-COMPLETO-DO-AUTOR"]))) %in% str_to_lower(dic_id_cit))
                      equipe <- "Discente"
                    if(str_to_lower(as.vector(as.vector(ep[[k]]["NOME-PARA-CITACAO"]))) %in% str_to_lower(dic_id_cit))
                      equipe <- "Discente"
                  }
                  equipe<-paste0(docente," ", discente)
                }
                ap <- data.frame(
                  id = id,
                  projeto = projeto,
                  equipe = equipe,
                  tipo = "",
                  producao = ""
                )
                producao_associada <- rbind(producao_associada, ap, row.names = NULL)
              }


              equipe<-""
              n2<-length(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"ARTIGOS-PUBLICADOS"[i]$"ARTIGO-PUBLICADO")
              for(k in (1:n2)[names(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"ARTIGOS-PUBLICADOS"[i]$"ARTIGO-PUBLICADO") == "AUTORES"]) {
                if(!is.na(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"ARTIGOS-PUBLICADOS"[[i]][k]$"AUTORES"["NRO-ID-CNPQ"])){
                  if(as.vector(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"ARTIGOS-PUBLICADOS"[[i]][k]$"AUTORES"["NRO-ID-CNPQ"]) %in% dic_id_cit)
                    equipe <- "Discente"
                }
                if(str_to_lower(as.vector(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"ARTIGOS-PUBLICADOS"[[i]][k]$"AUTORES"["NOME-COMPLETO-DO-AUTOR"])) %in% str_to_lower(dic_id_cit))
                  equipe <- "Discente"
                if(str_to_lower(as.vector(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"ARTIGOS-PUBLICADOS"[[i]][k]$"AUTORES"["NOME-PARA-CITACAO"])) %in% str_to_lower(dic_id_cit))
                  equipe <- "Discente"
              }

              # cada producao
              pa <- p[[j]]$"PROJETO-DE-PESQUISA"$"PRODUCOES-CT-DO-PROJETO"

              if((n5 <- length(pa)) > 0){
                for(y in 1:n5){
                  prod_assoc<-as.vector(pa[[y]]["TITULO-DA-PRODUCAO-CT"])
                  tipo<-as.vector(pa[[y]]["TIPO-PRODUCAO-CT"])

                if(y == 1){
                    producao_associada$tipo[nrow(producao_associada)] <- tipo
                    producao_associada$producao[nrow(producao_associada)] <- prod_assoc

                  } else {
                    ap <- data.frame(
                    id = id,
                    projeto = "",
                    equipe = "",
                    tipo = tipo,
                    producao = prod_assoc
                  )
                  producao_associada <- rbind(producao_associada, ap, row.names = NULL)
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  # retorna o dataframe
  producao_associada
}

