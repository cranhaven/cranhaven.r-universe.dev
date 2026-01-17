
tecnica <- function(id, xml_data,
                              qualis, nome_area, ano_ini, ano_fim){

  producao_tec <- NULL

#`ORGANIZACAO-DE-EVENTO`
ptec <- xml_data$"PRODUCAO-TECNICA"$"DEMAIS-TIPOS-DE-PRODUCAO-TECNICA"
for(i in (1:length(ptec))[names(ptec) == "ORGANIZACAO-DE-EVENTO"]){

  ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DA-ORGANIZACAO-DE-EVENTO"["ANO"])

  # flag relevancia
  fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DA-ORGANIZACAO-DE-EVENTO"['FLAG-RELEVANCIA']

  if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
    titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DA-ORGANIZACAO-DE-EVENTO"["TITULO"])
    inter<-ptec[[i]]$"INFORMACOES-ADICIONAIS"

    PTT <- "--"
    if(is.null(inter)){
      inter <- "<NACIONAL>"
      tipo <- "organizacao de evento - Nacional"
      PTT <- "OEN"
    }
    if(!is.null(inter)){ # se diferente de NULL, recupera a primeira linha
      inter <- str_split(inter[1],"\n")[[1]][1]
      if(inter == "<INTERNACIONAL>") {
        tipo <- "organizacao de evento - Internacional"
        PTT <- "OEI"
      }
      if(inter != "<INTERNACIONAL>") {
        tipo <- "organizacao de evento - Nacional"
        PTT <- "OEN"
      }
    }

    pontos <- list("T4",1)
    if(!(ano >= ano_ini & ano <= ano_fim)){
      pontos[[2]] <- 0
    }

    ap <- data.frame(
      id = id,
      ano = ano,
      titulo = titulo,
      tipo = tipo,
      PTT = PTT,
      estrato_ = pontos[[1]],
      pontos = pontos[[2]],
      flag_relevancia = fl_rlvnc
    )

  producao_tec <- rbind(producao_tec, ap, row.names = NULL)
  }
}


#`APRESENTACAO-DE-TRABALHO` ou "Outras palestras e apresentacoes"
ptec <- xml_data$"PRODUCAO-TECNICA"$"DEMAIS-TIPOS-DE-PRODUCAO-TECNICA"
for(i in (1:length(ptec))[names(ptec) == "APRESENTACAO-DE-TRABALHO"]){

  ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DA-APRESENTACAO-DE-TRABALHO"["ANO"])

  # flag relevancia
  fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DA-APRESENTACAO-DE-TRABALHO"['FLAG-RELEVANCIA']

  if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){

    titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DA-APRESENTACAO-DE-TRABALHO"["TITULO"])

    pontos <- list("T4",1)
    if(!(ano >= ano_ini & ano <= ano_fim)){
      pontos[[2]] <- 0
      }

    ap <- data.frame(
      id = id,
      ano = ano,
      titulo = titulo,
      tipo = "Outras palestras e apresentacoes",
      PTT = "PEPal",
      estrato_ = pontos[[1]],
      pontos = pontos[[2]],
      flag_relevancia = fl_rlvnc
    )

    producao_tec <- rbind(producao_tec, ap, row.names = NULL)
  }
}


# Participacao em mesa redonda; Palestrante, conferencista
ptec <- xml_data$"DADOS-COMPLEMENTARES"$"PARTICIPACAO-EM-EVENTOS-CONGRESSOS"
if(length(ptec) > 0){
  for(i in (1:length(ptec))){

    ano <- as.numeric(ptec[[i]][[1]]["ANO"])

    # flag relevancia
    fl_rlvnc <- ptec[[i]][[1]]['FLAG-RELEVANCIA']

    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){

      titulo <- as.vector(ptec[[i]][[1]]["TITULO"])
      natureza <- as.vector(ptec[[i]][[1]]["NATUREZA"])

      forma_participacao <- str_to_lower(ptec[[i]][[1]]["FORMA-PARTICIPACAO"])
      tipo_participacao <- str_to_lower(ptec[[i]][[1]]["TIPO-PARTICIPACAO"])

      pontos <- list("T4",1)
      if(!(ano >= ano_ini & ano <= ano_fim)){
        pontos[[2]] <- 0
        }

        # 3 - participacao em eventos e congressos
      PTT <- "--"
      if(forma_participacao == "convidado") PTT <- "PEPal"
      if(forma_participacao == "participante" & tipo_participacao == "apresentacao oral" |
                  forma_participacao == "participante" & tipo_participacao == "oral presentation")
          PTT <- "PEPal"
      if(forma_participacao == "participante" & tipo_participacao == "poster / painel")
          PTT <- "PEPos"

      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = str_to_lower(paste("Particiapacao em evento", "-", forma_participacao, "-", tipo_participacao)),
        PTT = PTT,
        estrato_ = pontos[[1]],
        pontos = pontos[[2]],
        flag_relevancia = fl_rlvnc
      )

      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }
}


#`DESENVOLVIMENTO-DE-MATERIAL-DIDATICO-OU-INSTRUCIONAL`
# DEMAIS-TIPOS-DE-PRODUCAO-TECNICA
ptec <- xml_data$"PRODUCAO-TECNICA"$"DEMAIS-TIPOS-DE-PRODUCAO-TECNICA"
for(i in (1:length(ptec))[names(ptec) == "DESENVOLVIMENTO-DE-MATERIAL-DIDATICO-OU-INSTRUCIONAL"]){

  ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DO-MATERIAL-DIDATICO-OU-INSTRUCIONAL"["ANO"])

  # flag relevancia
  fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DO-MATERIAL-DIDATICO-OU-INSTRUCIONAL"['FLAG-RELEVANCIA']

  if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
    titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-MATERIAL-DIDATICO-OU-INSTRUCIONAL"["TITULO"])

    pontos <- list("T4",1)
    if(!(ano >= ano_ini & ano <= ano_fim)){
      pontos[[2]] <- 0
    }

    ap <- data.frame(
      id = id,
      ano = ano,
      titulo = titulo,
      tipo = "desenvolvimento de material didatico",
      PTT = "MatDid",
      estrato_ = pontos[[1]],
      pontos = pontos[[2]],
      flag_relevancia = fl_rlvnc
    )

    producao_tec <- rbind(producao_tec, ap, row.names = NULL)
  }
}


#`CURSO-DE-CURTA-DURACAO-MINISTRADO` OU "Curso de formacao profissional e atividade de capacitacao"
ptec <- xml_data$"PRODUCAO-TECNICA"$"DEMAIS-TIPOS-DE-PRODUCAO-TECNICA"
for(i in (1:length(ptec))[names(ptec) == "CURSO-DE-CURTA-DURACAO-MINISTRADO"]){

  ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DE-CURSOS-CURTA-DURACAO-MINISTRADO"["ANO"])

  # flag relevancia
  fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DE-CURSOS-CURTA-DURACAO-MINISTRADO"['FLAG-RELEVANCIA']

  if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){

    titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DE-CURSOS-CURTA-DURACAO-MINISTRADO"["TITULO"])
    atuacao <- as.vector(ptec[[i]]$"DETALHAMENTO-DE-CURSOS-CURTA-DURACAO-MINISTRADO"["PARTICIPACAO-DOS-AUTORES"])
    atuacao<- str_to_lower(atuacao)

#    pontos <- pontos_tec(5, atuacao)
    pontos <- list("T4",1)
    if(!(ano >= ano_ini & ano <= ano_fim)){
      pontos[[2]] <- 0
    }

    PTT <- "--"
    # 5 - Curso de forma??o profissional e atividade de capacita??o
    if(atuacao == "organizador") PTT <- "CForO"
    if(atuacao == "docente") PTT <- "CForD"

    ap <- data.frame(
      id = id,
      ano = ano,
      titulo = titulo,
      tipo = paste("curso de formacao profissional e atividade de capacitacao -", atuacao),
      PTT = PTT,
      estrato_ = pontos[[1]],
      pontos = pontos[[2]],
      flag_relevancia = fl_rlvnc
    )

    producao_tec <- rbind(producao_tec, ap, row.names = NULL)
  }
}


# Produ??o e participa??o em m?dias (entrevista, mesas redondas, programas e coment?rios)
ptec <- xml_data$"PRODUCAO-TECNICA"$"DEMAIS-TIPOS-DE-PRODUCAO-TECNICA"
for(i in (1:length(ptec))[names(ptec) == "PROGRAMA-DE-RADIO-OU-TV"]){

  ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DO-PROGRAMA-DE-RADIO-OU-TV"["ANO"])

  # flag relevancia
  fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DO-PROGRAMA-DE-RADIO-OU-TV"['FLAG-RELEVANCIA']

  if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){

    titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-PROGRAMA-DE-RADIO-OU-TV"["TITULO"])
    atuacao <- ptec[[i]]$"INFORMACOES-ADICIONAIS"["DESCRICAO-INFORMACOES-ADICIONAIS"] # <PRODUCAO> ou <PARTICIPACAO>

    PTT <- "--"
    if(is.null(atuacao)){
      atuacao <- "<PARTICIPANTE>"
      tipo <- "Programa de midia - participante"
      PTT <- "MidPa"
    }
    if(!is.null(atuacao)){ # se diferente de NULL, recupera a primeira linha
      atuacao <- str_split(atuacao[1],"\n")[[1]][1]
      if(atuacao == "<ORGANIZADOR>") {
        tipo <- "Programa de midia - organizador"
        PTT <- "MidOr"
      } else if(atuacao != "<ORGANIZADOR>") {
        tipo <- "Programa de midia - participante"
        PTT <- "MidPa"
      }
    }

    pontos <- list("T4",1)
    if(!(ano >= ano_ini & ano <= ano_fim)){
      pontos[[2]] <- 0
    }

    ap <- data.frame(
      id = id,
      ano = ano,
      titulo = titulo,
      tipo = tipo,
      PTT = PTT,
      estrato_ = pontos[[1]],
      pontos = pontos[[2]],
      flag_relevancia = fl_rlvnc
    )

    producao_tec <- rbind(producao_tec, ap, row.names = NULL)
  }
}


# MIDIA-SOCIAL-WEBSITE-BLOG
ptec <- xml_data$"PRODUCAO-TECNICA"$"DEMAIS-TIPOS-DE-PRODUCAO-TECNICA"
for(i in (1:length(ptec))[names(ptec) == "MIDIA-SOCIAL-WEBSITE-BLOG"]){

  ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DA-MIDIA-SOCIAL-WEBSITE-BLOG"["ANO"])

  # flag relevancia
  fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DA-MIDIA-SOCIAL-WEBSITE-BLOG"['FLAG-RELEVANCIA']

  if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){ ##########################aqui, inserir um "desde"... para identificar apenas o VINCULO

    titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DA-MIDIA-SOCIAL-WEBSITE-BLOG"["TITULO"])
    tipo <- str_to_lower(as.vector(ptec[[i]]$"DADOS-BASICOS-DA-MIDIA-SOCIAL-WEBSITE-BLOG"["NATUREZA"]))

    pontos <- list("T4",1)
    if(!(ano >= ano_ini & ano <= ano_fim)){
      pontos[[2]] <- 0
    }

    ap <- data.frame(
      id = id,
      ano = ano,
      titulo = titulo,
      tipo = paste("Producao de ", tipo),
      PTT = "MidOr",
      estrato_ = pontos[[1]],
      pontos = pontos[[2]],
      flag_relevancia = fl_rlvnc
    )

    producao_tec <- rbind(producao_tec, ap, row.names = NULL)
  }
}


#`EDITORIA`
# DEMAIS-TIPOS-DE-PRODUCAO-TECNICA
ptec <- xml_data$"PRODUCAO-TECNICA"$"DEMAIS-TIPOS-DE-PRODUCAO-TECNICA"
for(i in (1:length(ptec))[names(ptec) == "EDITORACAO"]){

  ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DE-EDITORACAO"["ANO"])

  # flag relevancia
  fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DE-EDITORACAO"['FLAG-RELEVANCIA']

  if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
    titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DE-EDITORACAO"["TITULO"])
    tipo <- str_to_lower(as.vector(ptec[[i]]$"DADOS-BASICOS-DE-EDITORACAO"["NATUREZA"]))

    pontos <- list("T4",1)
    if(!(ano >= ano_ini & ano <= ano_fim)){
      pontos[[2]] <- 0
    }

    ap <- data.frame(
      id = id,
      ano = ano,
      titulo = titulo,
      tipo = paste("Editoracao de ", tipo),
      PTT = "Edit",
      estrato_ = pontos[[1]],
      pontos = pontos[[2]],
      flag_relevancia = fl_rlvnc
    )

    producao_tec <- rbind(producao_tec, ap, row.names = NULL)
  }
}


# TEXTOS-EM-JORNAIS-OU-REVISTAS
ptec <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"TEXTOS-EM-JORNAIS-OU-REVISTAS"
if(length(ptec) > 0){
  for(i in (1:length(ptec))){

    ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DO-TEXTO"["ANO-DO-TEXTO"])

    # flag relevancia
    fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DO-TEXTO"['FLAG-RELEVANCIA']

    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){

      titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-TEXTO"["TITULO-DO-TEXTO"])
      pais <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-TEXTO"["PAIS-DE-PUBLICACAO"])
      tipo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-TEXTO"["NATUREZA"])

      pontos <- list("T4",1)
      if(!(ano >= ano_ini & ano <= ano_fim)){
        pontos[[2]] <- 0
      }

      PTT <- "--"
      # 9 - artigo publicado em revista de divulgacao; artigo em jornal
      if(!is.null(pais)) {
      if(pais == "Brasil") {PTT <- "ArDN"
      } else PTT <- "ArDI" # internacional
    }
      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = str_to_lower(paste("Artigo publicado em ", tipo," - ", pais)),
        PTT = PTT,
        estrato_ = pontos[[1]],
        pontos = pontos[[2]],
        flag_relevancia = fl_rlvnc
      )

      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }
}


# OUTRA-PUBLICACAO-BIBLIOGRAFICA (revista tecnica, resenha e taxonomias e tesauros)
ptec <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"DEMAIS-TIPOS-DE-PRODUCAO-BIBLIOGRAFICA"
if(length(ptec) > 0){
  for(i in (1:length(ptec))[names(ptec) == "OUTRA-PRODUCAO-BIBLIOGRAFICA"]){

    ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DE-OUTRA-PRODUCAO"["ANO"])

    # flag relevancia
    fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DE-OUTRA-PRODUCAO"['FLAG-RELEVANCIA']

    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){

      titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DE-OUTRA-PRODUCAO"["TITULO"])
      natureza <- as.vector(ptec[[i]]$"DADOS-BASICOS-DE-OUTRA-PRODUCAO"["NATUREZA"])

      pontos <- list("T4",1)
      if(!(ano >= ano_ini & ano <= ano_fim)){
        pontos[[2]] <- 0
      }

      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = str_to_lower(paste("publicacao tecnica - ", natureza)),
        PTT = "PubTec",
        estrato_ = pontos[[1]],
        pontos = pontos[[2]],
        flag_relevancia = fl_rlvnc
      )

      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }
}


# PREFACIO-POSFACIO
ptec <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"DEMAIS-TIPOS-DE-PRODUCAO-BIBLIOGRAFICA"
if(length(ptec) > 0){
  for(i in (1:length(ptec))[names(ptec) == "PREFACIO-POSFACIO"]){

    ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DO-PREFACIO-POSFACIO"["ANO"])

    # flag relevancia
    fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DO-PREFACIO-POSFACIO"['FLAG-RELEVANCIA']

    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){

      titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-PREFACIO-POSFACIO"["TITULO"])
      tipo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-PREFACIO-POSFACIO"["TIPO"])
      natureza <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-PREFACIO-POSFACIO"["NATUREZA"])

      pontos <- list("T4",1)
      if(!(ano >= ano_ini & ano <= ano_fim)){
        pontos[[2]] <- 0
      }

      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = str_to_lower(paste("publicacao tecnica - ", tipo, "de", natureza)),
        PTT = "PubTec",
        estrato_ = pontos[[1]],
        pontos = pontos[[2]],
        flag_relevancia = fl_rlvnc
      )

      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }
}


# TRADUCAO
ptec <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"DEMAIS-TIPOS-DE-PRODUCAO-BIBLIOGRAFICA"
if(length(ptec) > 0){
  for(i in (1:length(ptec))[names(ptec) == "TRADUCAO"]){

    ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DA-TRADUCAO"["ANO"])

    # flag relevancia
    fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DA-TRADUCAO"['FLAG-RELEVANCIA']

    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){

      titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DA-TRADUCAO"["TITULO"])
      tipo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DA-TRADUCAO"["NATUREZA"])

      pontos <- list("T4",1)
      if(!(ano >= ano_ini & ano <= ano_fim)){
        pontos[[2]] <- 0
      }

      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = paste("traducao de ", str_to_lower(tipo)),
        PTT = "Trad",
        estrato_ = pontos[[1]],
        pontos = pontos[[2]],
        flag_relevancia = fl_rlvnc
      )

      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }
}


#`RELATORIO-DE-PESQUISA`
ptec <- xml_data$"PRODUCAO-TECNICA"$"DEMAIS-TIPOS-DE-PRODUCAO-TECNICA"
for(i in (1:length(ptec))[names(ptec) == "RELATORIO-DE-PESQUISA"]){

  ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DO-RELATORIO-DE-PESQUISA"["ANO"])

  # flag relevancia
  fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DO-RELATORIO-DE-PESQUISA"['FLAG-RELEVANCIA']

  if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){

    titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-RELATORIO-DE-PESQUISA"["TITULO"])

    pontos <- list("T4",1)
    if(!(ano >= ano_ini & ano <= ano_fim)){
      pontos[[2]] <- 0
    }

    ap <- data.frame(
      id = id,
      ano = ano,
      titulo = titulo,
      tipo = "Relatorio tecnico conclusivo: relatorio de pesquisa aplicada",
      PTT = "relTec",
      estrato_ = pontos[[1]],
      pontos = pontos[[2]],
      flag_relevancia = fl_rlvnc
    )

    producao_tec <- rbind(producao_tec, ap, row.names = NULL)
  }
}


# TRABALHO-TECNICO (parecer, ELABORACAO_DE_PROJETO, RELATORIO_TECNICO, ASSESSORIA,CONSULTORIA)
ptec <- xml_data$"PRODUCAO-TECNICA"
for(i in (1:length(ptec))[names(ptec) == "TRABALHO-TECNICO"]){

  ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DO-TRABALHO-TECNICO"["ANO"])

  # flag relevancia
  fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DO-TRABALHO-TECNICO"['FLAG-RELEVANCIA']

  if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){

    titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-TRABALHO-TECNICO"["TITULO-DO-TRABALHO-TECNICO"])
    natureza <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-TRABALHO-TECNICO"["NATUREZA"])
    natureza <- str_to_lower(natureza)
    eqcp <- "--"

    # quando o PARECER for "parecer de artigo de revista"
    if(natureza == "parecer" & as.vector(ptec[[i]]$"DETALHAMENTO-DO-TRABALHO-TECNICO"["FINALIDADE"]) == "Parecer de artigo de revista"){
    natureza <- "Parecer de artigo de revista"
    # recupera o ISSN no inicio do titulo
    issn <- gsub("-", "", str_sub(titulo, 1, 9))

    # verifica o estrato na area especificada no parametro nome_area
    eqcp <- as.character(qualis[(qualis$Area %in% nome_area ) & (qualis$ISSN == issn), "Estrato"][1])
    if(is.na(eqcp)) eqcp <- "--"
    }

    # parecer para agencia de fomento
    if(natureza == "parecer" & as.vector(ptec[[i]]$"DETALHAMENTO-DO-TRABALHO-TECNICO"["FINALIDADE"]) == "Parecer de projeto de agencia de fomento"){
    natureza <- "Parecer de projeto de agencia de fomento"
    }
    pontos <- list("T4",1)
    if(!(ano >= ano_ini & ano <= ano_fim)){
      pontos[[2]] <- 0
    }

    ap <- data.frame(
      id = id,
      ano = ano,
      titulo = titulo,
      tipo = paste("Relatorio tecnico conclusivo - ", natureza),
      PTT = "relTec",
      estrato_ = pontos[[1]],
      pontos = pontos[[2]],
      flag_relevancia = fl_rlvnc
    )

    producao_tec <- rbind(producao_tec, ap, row.names = NULL)
  }
}


#`OUTRA-PRODUCAO-TECNICA`
# "Manual ou protocolo", "Base de dados t?cnico-cient?fica", "Norma ou marco regulat?rio", "Acervo",
# "Empresa ou organiza??o social inovadora", "Tecnologia social", "Taxonomia, ontologias e tesauros", "Nota tecnica ou laudo tecnico"
ptec <- xml_data$"PRODUCAO-TECNICA"$"DEMAIS-TIPOS-DE-PRODUCAO-TECNICA"
for(i in (1:length(ptec))[names(ptec) == "OUTRA-PRODUCAO-TECNICA"]){

  ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DE-OUTRA-PRODUCAO-TECNICA"["ANO"])

  # flag relevancia
  fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DE-OUTRA-PRODUCAO-TECNICA"['FLAG-RELEVANCIA']

  if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){

    titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DE-OUTRA-PRODUCAO-TECNICA"["TITULO"])
    natureza <- str_trim(str_to_lower(as.vector(ptec[[i]]$"DADOS-BASICOS-DE-OUTRA-PRODUCAO-TECNICA"["NATUREZA"])))

    # mudar o nome de "Nota tecnica ou laudo tecnico"
    if(natureza == "Nota tecnica ou laudo tecnico") {natureza<-"Relatorio tecnico conclusivo: Nota tecnica ou laudo tecnico" }

    pontos <- list("T4",1)
    if(!(ano >= ano_ini & ano <= ano_fim)){
      pontos[[2]] <- 0
    }

    PTT <- "--"
    # 15 - OUTRA-PRODUCAO-TECNICA
    if(natureza == "Manual ou protocolo" | natureza == "Norma ou marco regulatorio") PTT <- "PubTec"
    if(natureza == "Base de dados tecnico-cientifica") PTT <- "BD"
    if(natureza == "Acervo") PTT <- "Acer"
    if(natureza == "Empresa ou organizacao social inovadora") PTT <- "EmpSoc"
    if(natureza == "Tecnologia social") PTT <- "TecSoc"
    if(natureza == "Taxonomia, ontologias e tesauros") PTT <- "PubTec"
    if(natureza == "Relatorio tecnico conclusivo: Nota tecnica ou laudo tecnico") PTT <- "relTec"

    ap <- data.frame(
      id = id,
      ano = ano,
      titulo = titulo,
      tipo = natureza,
      PTT = PTT,
      estrato_ = pontos[[1]],
      pontos = pontos[[2]],
      flag_relevancia = fl_rlvnc
    )

    producao_tec <- rbind(producao_tec, ap, row.names = NULL)
  }
}


# carta, mapa ou similar
ptec <- xml_data$"PRODUCAO-TECNICA"$"DEMAIS-TIPOS-DE-PRODUCAO-TECNICA"
for(i in (1:length(ptec))[names(ptec) == "CARTA-MAPA-OU-SIMILAR"]){

  ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DE-CARTA-MAPA-OU-SIMILAR"["ANO"])

  # flag relevancia
  fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DE-CARTA-MAPA-OU-SIMILAR"['FLAG-RELEVANCIA']

  if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){

    titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DE-CARTA-MAPA-OU-SIMILAR"["TITULO"])

    pontos <- list("T4",1)
    if(!(ano >= ano_ini & ano <= ano_fim)){
      pontos[[2]] <- 0
    }

    ap <- data.frame(
      id = id,
      ano = ano,
      titulo = titulo,
      tipo = "carta, mapa ou similar",
      PTT = "Map",
      estrato_ = pontos[[1]],
      pontos = pontos[[2]],
      flag_relevancia = fl_rlvnc
    )

    producao_tec <- rbind(producao_tec, ap, row.names = NULL)
  }
}


# SOFTWARE
ptec <- xml_data$"PRODUCAO-TECNICA"
for(i in (1:length(ptec))[names(ptec) == "SOFTWARE"]){

  ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DO-SOFTWARE"["ANO"])

  # flag relevancia
  fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DO-SOFTWARE"['FLAG-RELEVANCIA']

  if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){

    titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-SOFTWARE"["TITULO-DO-SOFTWARE"])

    if("REGISTRO-OU-PATENTE" %in% names(ptec[[i]]$"DETALHAMENTO-DO-SOFTWARE")){
      tipo <- "registro de software"
      titulo <- paste0(titulo, " - Registro (",
                       ptec[[i]]$"DETALHAMENTO-DO-SOFTWARE"$"REGISTRO-OU-PATENTE"["CODIGO-DO-REGISTRO-OU-PATENTE"],
                       ")")
    } else tipo <- "software"

    pontos <- list("T4",1)
    if(!(ano >= ano_ini & ano <= ano_fim)){
      pontos[[2]] <- 0
    }

    ap <- data.frame(
      id = id,
      ano = ano,
      titulo = titulo,
      tipo = tipo,
      PTT = "softw",
      estrato_ = pontos[[1]],
      pontos = pontos[[2]],
      flag_relevancia = fl_rlvnc
    )

    producao_tec <- rbind(producao_tec, ap, row.names = NULL)
  }
}


# Produto n?o patenteavel
ptec <- xml_data$"PRODUCAO-TECNICA"
for(i in (1:length(ptec))[names(ptec) == "PRODUTO-TECNOLOGICO"]){

  ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DO-PRODUTO-TECNOLOGICO"["ANO"])

  # flag relevancia
  fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DO-PRODUTO-TECNOLOGICO"['FLAG-RELEVANCIA']

  if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){

    titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-PRODUTO-TECNOLOGICO"["TITULO-DO-PRODUTO"])
    natureza <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-PRODUTO-TECNOLOGICO"["NATUREZA"])
    tipo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-PRODUTO-TECNOLOGICO"["TIPO-PRODUTO"])

    pontos <- list("T4",1)
    if(!(ano >= ano_ini & ano <= ano_fim)){
      pontos[[2]] <- 0
    }

    ap <- data.frame(
      id = id,
      ano = ano,
      titulo = titulo,
      tipo = str_to_lower(paste("Produto nao patenteavel - ", tipo," - ", natureza)),
      PTT = "PNPTec",
      estrato_ = pontos[[1]],
      pontos = pontos[[2]],
      flag_relevancia = fl_rlvnc
    )

    producao_tec <- rbind(producao_tec, ap, row.names = NULL)
  }
}


# Processo ou tecnica
ptec <- xml_data$"PRODUCAO-TECNICA"
for(i in (1:length(ptec))[names(ptec) == "PROCESSOS-OU-TECNICAS"]){

  ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DO-PROCESSOS-OU-TECNICAS"["ANO"])

  # flag relevancia
  fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DO-PROCESSOS-OU-TECNICAS"['FLAG-RELEVANCIA']

  if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){

    titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-PROCESSOS-OU-TECNICAS"["TITULO-DO-PROCESSO"])
    natureza <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-PROCESSOS-OU-TECNICAS"["NATUREZA"])

    pontos <- list("T4",1)
    if(!(ano >= ano_ini & ano <= ano_fim)){
      pontos[[2]] <- 0
    }

    ap <- data.frame(
      id = id,
      ano = ano,
      titulo = titulo,
      tipo = str_to_lower(paste("Processo ou tecnica - ", natureza)),
      PTT = "PNPTec",
      estrato_ = pontos[[1]],
      pontos = pontos[[2]],
      flag_relevancia = fl_rlvnc
    )

    producao_tec <- rbind(producao_tec, ap, row.names = NULL)
  }
}


# Patente
ptec <- xml_data$"PRODUCAO-TECNICA"
for(i in (1:length(ptec))[names(ptec) == "PATENTE"]){

  ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DA-PATENTE"["ANO-DESENVOLVIMENTO"])

  # flag relevancia
  fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DA-PATENTE"['FLAG-RELEVANCIA']

  if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){

    titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DA-PATENTE"["TITULO"])

    pontos <- list("T4",1)
    if(!(ano >= ano_ini & ano <= ano_fim)){
      pontos[[2]] <- 0
    }

    ap <- data.frame(
      id = id,
      ano = ano,
      titulo = titulo,
      tipo = "Patente",
      PTT = "Patent",
      estrato_ = pontos[[1]],
      pontos = pontos[[2]],
      flag_relevancia = fl_rlvnc
    )

    producao_tec <- rbind(producao_tec, ap, row.names = NULL)
  }
}


# Culitivar registrada
ptec <- xml_data$"PRODUCAO-TECNICA"
for(i in (1:length(ptec))[names(ptec) == "CULTIVAR-REGISTRADA"]){

  ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DA-CULTIVAR"["ANO-SOLICITACAO"])

  # flag relevancia
  fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DA-CULTIVAR"['FLAG-RELEVANCIA']

  if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){

    titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DA-CULTIVAR"["DENOMINACAO"])

    pontos <- list("T4",1)
    if(!(ano >= ano_ini & ano <= ano_fim)){
      pontos[[2]] <- 0
    }

    ap <- data.frame(
      id = id,
      ano = ano,
      titulo = titulo,
      tipo = "cultivar",
      PTT = "Patent",
      estrato_ = pontos[[1]],
      pontos = pontos[[2]],
      flag_relevancia = fl_rlvnc
    )

    producao_tec <- rbind(producao_tec, ap, row.names = NULL)
  }
}


  #####################################
  # insere linha de totais
  ap <- data.frame(
    id = id,
    ano = "",
    titulo = "",
    tipo = "",
    PTT = "",
    estrato_ = "TOTAL",
    pontos = sum(producao_tec$pontos, na.rm = TRUE),
    flag_relevancia = ""
  )
  producao_tec <- rbind(producao_tec, ap, row.names = NULL)

  # retorna o dataframe
  producao_tec
}
