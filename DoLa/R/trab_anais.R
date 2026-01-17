trab_anais <- function(id, xml_data,
                       dic_id_cit,
                       ano_ini, ano_fim){

  producao_tanais <- NULL

  n <- length(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"TRABALHOS-EM-EVENTOS")

  if(n > 0){
    for(i in 1:n){

      # identifica o ano
      ano <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"TRABALHOS-EM-EVENTOS"[[i]]$"DADOS-BASICOS-DO-TRABALHO"["ANO-DO-TRABALHO"]
      ano <- as.vector(ano) # transforma o named vector em somente vetor

      if(ano >= ano_ini & ano <= ano_fim){

        # identifica se eh resumo ou trabalhho completo
        natureza <- str_to_lower(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"TRABALHOS-EM-EVENTOS"[[i]]$"DADOS-BASICOS-DO-TRABALHO"["NATUREZA"])
        # recupera o titulo
        titulo <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"TRABALHOS-EM-EVENTOS"[[i]]$"DADOS-BASICOS-DO-TRABALHO"["TITULO-DO-TRABALHO"]
        # recupera o pais onde foi realizado o evento
        pais <- as.vector(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"TRABALHOS-EM-EVENTOS"[[i]]$"DADOS-BASICOS-DO-TRABALHO"["PAIS-DO-EVENTO"])

        # identifica o evento
        evento <- as.vector(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"TRABALHOS-EM-EVENTOS"[[i]]$"DETALHAMENTO-DO-TRABALHO"["NOME-DO-EVENTO"])
        # identifica se eh nacional ou internacional

        # identifica a cidade do evento
        cidade <- as.vector(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"TRABALHOS-EM-EVENTOS"[[i]]$"DETALHAMENTO-DO-TRABALHO"["CIDADE-DO-EVENTO "])
        if(is.na(cidade)) {cidade <-""}

        # verifica se ha docente e discente do programa na publicacao
        equipe = ""
        n2<-length(xml_data$"PRODUCAO-BIBLIOGRAFICA"$`TRABALHOS-EM-EVENTOS`[i]$"TRABALHO-EM-EVENTOS")

        for(k in (1:n2)[names(xml_data$"PRODUCAO-BIBLIOGRAFICA"$`TRABALHOS-EM-EVENTOS`[i]$"TRABALHO-EM-EVENTOS") == "AUTORES"]) {
#            docente = ""
            discente = ""
          # busca pelo ID-CNPq
            if(!is.na(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"TRABALHOS-EM-EVENTOS"[i]$"TRABALHO-EM-EVENTOS"$"AUTORES"["NRO-ID-CNPQ"])){
              if(as.vector(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"TRABALHOS-EM-EVENTOS"[i]$"TRABALHO-EM-EVENTOS"$"AUTORES"["NRO-ID-CNPQ"]) %in% str_to_lower(dic_id_cit)) discente <- "Discente"
            }
          # busca pelo nome completo
            if(as.vector(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"TRABALHOS-EM-EVENTOS"[i]$"TRABALHO-EM-EVENTOS"$"AUTORES"["NOME-COMPLETO-DO-AUTOR"]) %in% str_to_lower(dic_id_cit)) discente <- "Discente"
          # busca pelo nome na citacao
            if(as.vector(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"TRABALHOS-EM-EVENTOS"[i]$"TRABALHO-EM-EVENTOS"$"AUTORES"["NNOME-PARA-CITACAO"]) %in% str_to_lower(dic_id_cit)) discente <- "Discente"

#        equipe<-paste0(docente," ", discente)
         equipe<-discente
        }

        pontos <- 1

        ap <- data.frame(
          id = id,
          ano = ano,
          natureza = natureza,
          titulo = titulo,
          evento = evento,
          pais = pais,
          cidade = cidade,
          equipe = equipe,
          pontos = pontos
          )

        producao_tanais <- rbind(producao_tanais, ap, row.names = NULL)
      }
    }

    # consolida pontuacao do bloco
    pontos <- producao_tanais$pontos
    if(length(pontos) == 0) {
      titulo = "NAO HA PRODUCAO NO PERIODO."
    } else {
      titulo = ""
    }
    pontos <- sum(pontos, na.rm = TRUE)

} else {
    pontos <- 0
    titulo = "NAO HA PRODUCAO NO PERIODO."
  }


#####################################
  # insere linha de totais

  ap <- data.frame(
    id = id,
    ano = "",
    natureza = "",
    titulo = titulo,
    evento = "",
    pais = "",
    cidade = "",
    equipe = "TOTAL",
    pontos = pontos
  )

  producao_tanais <- rbind(producao_tanais, ap, row.names = NULL)

  # retorna o dataframe
  producao_tanais
}
