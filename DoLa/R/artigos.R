artigos <- function(id, xml_data, dic_id_cit, qualis, nome_area,
                    encode_xml2, ano_ini, ano_fim){

  producao_artigos <- NULL

  n <- length(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"ARTIGOS-PUBLICADOS")
  nome <- encode_xml2(xml_data$"DADOS-GERAIS"$.attrs["NOME-COMPLETO"])

  if(n > 0){
    for(i in 1:n){

      # identifica o ano do artigo
      ano = xml_data$"PRODUCAO-BIBLIOGRAFICA"$"ARTIGOS-PUBLICADOS"[[i]]$"DADOS-BASICOS-DO-ARTIGO"["ANO-DO-ARTIGO"]
      ano <- as.vector(ano) # transforma o named vector em somente vetor
      # flag relevancia
      fl_rlvnc <- xml_data$`PRODUCAO-BIBLIOGRAFICA`$`ARTIGOS-PUBLICADOS`[[i]]$`DADOS-BASICOS-DO-ARTIGO`['FLAG-RELEVANCIA']

      if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc == "SIM"){
        # identifica o periodico e recupera o estrato qualis para CP
        issn <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"ARTIGOS-PUBLICADOS"[[i]]$"DETALHAMENTO-DO-ARTIGO"["ISSN"]

        # estrato qualis
        if(issn %in% qualis$ISSN == F) {
          eq <- "NC"
          areas <- "NC"
        }
        if(issn %in% qualis$ISSN == T) {
        eq <- as.character(levels(as.factor(as.character((qualis[qualis$ISSN == issn, "Estrato"])))))
        eq <- eq[order(eq)][1]

        # area especifica ou outras areas
        if((nome_area  %in% qualis$Area) & (issn %in% qualis$ISSN) == T) areas <- nome_area
        if((nome_area  %in% qualis$Area) & (issn %in% qualis$ISSN) == F) {
          area <- as.character(qualis[qualis$ISSN == issn, "Area"])
          for(j in 1:length(area)) {
          if(j == 1) areas <- area[j]
          if(j > 1) areas <- paste0(
            areas,"; ",area[j])
          }
          }
        }

        # recupera o titulo do artigo
        titulo <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"ARTIGOS-PUBLICADOS"[[i]]$"DADOS-BASICOS-DO-ARTIGO"["TITULO-DO-ARTIGO"]
        periodico <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"ARTIGOS-PUBLICADOS"[[i]]$"DETALHAMENTO-DO-ARTIGO"["TITULO-DO-PERIODICO-OU-REVISTA"]

        pontos <- 1
        if(!(ano >= ano_ini & ano <= ano_fim)){
          pontos <- 0
        }

        # verifica se a producao eh com discente e egresso
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

        ap <- data.frame(
          id = id,
          nome = nome,
          ano = ano,
          titulo = titulo,
          issn = paste0(str_sub(issn,1,4),"-",str_sub(issn,5,8)),
          periodico = periodico,
          estrato_ = eq,
          areas = areas,
          equipe = equipe,
          pontos = pontos,
          flag_relevancia = fl_rlvnc
        )

        producao_artigos <- rbind(producao_artigos, ap, row.names = NULL)
      }
    }

    # consolida pontuacao do bloco
    pontos <- producao_artigos$pontos
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

# insere linha de totais
  ap <- data.frame(
    id = id,
    nome = nome,
    ano = "",
    titulo = titulo,
    issn = "",
    periodico = "",
    estrato_ = "",
    areas = "",
    equipe = "TOTAL",
    pontos = pontos,
    flag_relevancia = ""
  )
  producao_artigos <- rbind(producao_artigos, ap, row.names = NULL)

  producao_artigos

}
