
livros <- function(id, xml_data, qualis, ano_ini, ano_fim, dic_id_cit, encode_xml2) {

  producao_livros <- NULL

  n <- length(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"LIVROS-PUBLICADOS-OU-ORGANIZADOS")
  nome <- encode_xml2(xml_data$"DADOS-GERAIS"$.attrs["NOME-COMPLETO"])

  if(n > 0){
    for(i in 1:n){
      # identifica o ano do livro
      ano = xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"LIVROS-PUBLICADOS-OU-ORGANIZADOS"[[i]]$"DADOS-BASICOS-DO-LIVRO"["ANO"]

      # flag relevancia
      fl_rlvnc <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"LIVROS-PUBLICADOS-OU-ORGANIZADOS"[[i]]$"DADOS-BASICOS-DO-LIVRO"['FLAG-RELEVANCIA']

      if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc == "SIM"){
        # recupera dados basicos do livro
        isbn <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"LIVROS-PUBLICADOS-OU-ORGANIZADOS"[[i]]$"DETALHAMENTO-DO-LIVRO"["ISBN"] %>% str_trim
        tipo <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"LIVROS-PUBLICADOS-OU-ORGANIZADOS"[[i]]$"DADOS-BASICOS-DO-LIVRO"["TIPO"]
        titulo <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"LIVROS-PUBLICADOS-OU-ORGANIZADOS"[[i]]$"DADOS-BASICOS-DO-LIVRO"["TITULO-DO-LIVRO"]
        edicao <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"LIVROS-PUBLICADOS-OU-ORGANIZADOS"[[i]]$"DETALHAMENTO-DO-LIVRO"["NUMERO-DA-EDICAO-REVISAO"]
        editora <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"LIVROS-PUBLICADOS-OU-ORGANIZADOS"[[i]]$"DETALHAMENTO-DO-LIVRO"["NOME-DA-EDITORA"]

        # estrato qualis
        if(is.null(qualis) != T) {
        eq <- qualis[qualis$ISBN == isbn, c("ISBN", "Estrato")]
        nlinhas <- nrow(eq)
        if(nlinhas > 1) {
          eq <- '>1'
        } else if(nlinhas == 0) {
          eq <- "--"
        } else{
          eq <- eq$Estrato
        }
        }
        if(is.null(qualis) == T) {
          eq <- "--"
        }

        pontos <- 1
        #        pontos <- pontos_livro(eq, tipo)
        if(!(ano >= ano_ini & ano <= ano_fim)){
          pontos <- 0
        }

        # verifica se a producao eh com discente e egresso
        equipe<-""
        n2<-length(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"LIVROS-PUBLICADOS-OU-ORGANIZADOS"[i]$"LIVRO-PUBLICADO-OU-ORGANIZADO")
        for(k in (1:n2)[names(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"LIVROS-PUBLICADOS-OU-ORGANIZADOS"[i]$"LIVRO-PUBLICADO-OU-ORGANIZADO") == "AUTORES"]) {
          if(!is.na(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"LIVROS-PUBLICADOS-OU-ORGANIZADOS"[[i]][k]$"AUTORES"["NRO-ID-CNPQ"])){
            if(as.vector(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"LIVROS-PUBLICADOS-OU-ORGANIZADOS"[[i]][k]$"AUTORES"["NRO-ID-CNPQ"]) %in% dic_id_cit)
              equipe <- "Discente"
          }
          if(str_to_lower(as.vector(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"LIVROS-PUBLICADOS-OU-ORGANIZADOS"[[i]][k]$"AUTORES"["NOME-COMPLETO-DO-AUTOR"])) %in% str_to_lower(dic_id_cit))
            equipe <- "Discente"
          if(str_to_lower(as.vector(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"LIVROS-PUBLICADOS-OU-ORGANIZADOS"[[i]][k]$"AUTORES"["NOME-PARA-CITACAO"])) %in% str_to_lower(dic_id_cit))
            equipe <- "Discente"
        }

        ap <- data.frame(
          id = id,
          nome = nome,
          ano = ano,
          titulo = titulo,
          isbn = isbn,
          tipo = tipo,
          edicao = edicao,
          editora = editora,
          estrato_ = eq,
          equipe = equipe,
          pontos = pontos,
          flag_relevancia = fl_rlvnc
        )

        producao_livros <- rbind(producao_livros, ap, row.names = NULL)
      }
    }
  }

  # CAPITULOS
  n <- length(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"CAPITULOS-DE-LIVROS-PUBLICADOS")
  if(n > 0){
    for(i in 1:n){
      # identifica o ano do livro
      ano = xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"CAPITULOS-DE-LIVROS-PUBLICADOS"[[i]]$"DADOS-BASICOS-DO-CAPITULO"["ANO"]

      # flag relevancia
      fl_rlvnc <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"CAPITULOS-DE-LIVROS-PUBLICADOS"[[i]]$"DADOS-BASICOS-DO-CAPITULO"['FLAG-RELEVANCIA']

      if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
        # recupera dados basicos do livro
        isbn <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"CAPITULOS-DE-LIVROS-PUBLICADOS"[[i]]$"DETALHAMENTO-DO-CAPITULO"["ISBN"] %>% str_trim
        tipo <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"CAPITULOS-DE-LIVROS-PUBLICADOS"[[i]]$"DADOS-BASICOS-DO-CAPITULO"["TIPO"]
        titulo <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"CAPITULOS-DE-LIVROS-PUBLICADOS"[[i]]$"DADOS-BASICOS-DO-CAPITULO"["TITULO-DO-CAPITULO-DO-LIVRO"]
        edicao <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"CAPITULOS-DE-LIVROS-PUBLICADOS"[[i]]$"DETALHAMENTO-DO-CAPITULO"["NUMERO-DA-EDICAO-REVISAO"]
        editora <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"CAPITULOS-DE-LIVROS-PUBLICADOS"[[i]]$"DETALHAMENTO-DO-CAPITULO"["NOME-DA-EDITORA"]

        titulo <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"CAPITULOS-DE-LIVROS-PUBLICADOS"[[i]]$"DADOS-BASICOS-DO-CAPITULO"["TITULO-DO-CAPITULO-DO-LIVRO"]
        titulodolivro = xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"CAPITULOS-DE-LIVROS-PUBLICADOS"[[i]]$"DETALHAMENTO-DO-CAPITULO"["TITULO-DO-LIVRO"]
        titulo <- paste0(titulo," In: ",titulodolivro)

        # estrato qualis
        if(is.null(qualis) != T) {
          eq <- qualis[qualis$ISBN == isbn, c("ISBN", "Estrato")]
          nlinhas <- nrow(eq)
          if(nlinhas > 1) {
            eq <- '>1'
          } else if(nlinhas == 0) {
            eq <- "--"
          } else{
            eq <- eq$Estrato
          }
        }
        if(is.null(qualis) == T) {
          eq <- "--"
        }

        pontos <- 1
        if(!(ano >= ano_ini & ano <= ano_fim)){
          pontos <- 0
        }

        # verifica se a producao eh com discente e egresso
        equipe<-""
        n2<-length(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"CAPITULOS-DE-LIVROS-PUBLICADOS"[i]$"CAPITULO-DE-LIVRO-PUBLICADO")
        for(k in (1:n2)[names(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"CAPITULOS-DE-LIVROS-PUBLICADOS"[i]$"CAPITULO-DE-LIVRO-PUBLICADO") == "AUTORES"]) {
          if(!is.na(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"CAPITULOS-DE-LIVROS-PUBLICADOS"[[i]][k]$"AUTORES"["NRO-ID-CNPQ"])){
            if(as.vector(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"CAPITULOS-DE-LIVROS-PUBLICADOS"[[i]][k]$"AUTORES"["NRO-ID-CNPQ"]) %in% dic_id_cit)
              equipe <- "Discente"
          }
          if(str_to_lower(as.vector(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"CAPITULOS-DE-LIVROS-PUBLICADOS"[[i]][k]$"AUTORES"["NOME-COMPLETO-DO-AUTOR"])) %in% str_to_lower(dic_id_cit))
            equipe <- "Discente"
          if(str_to_lower(as.vector(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"CAPITULOS-DE-LIVROS-PUBLICADOS"[[i]][k]$"AUTORES"["NOME-PARA-CITACAO"])) %in% str_to_lower(dic_id_cit))
            equipe <- "Discente"
        }

        ap <- data.frame(
          id = id,
          nome = nome,
          ano = ano,
          titulo = titulo,
          isbn = isbn,
          tipo = tipo,
          edicao = edicao,
          editora = editora,
          estrato_ = eq,
          equipe = equipe,
          pontos = pontos,
          flag_relevancia = fl_rlvnc
        )

        producao_livros <- rbind(producao_livros, ap, row.names = NULL)
      }
    }
  }

  # consolida pontuacao do bloco
  pontos <- producao_livros$pontos
  if(length(pontos) == 0) {
    titulo = "NAO HA PRODUCAO NO PERIODO."
  } else {
    titulo = ""
  }

  # insere linha de totais
  ap <- data.frame(
    id = id,
    nome = nome,
    ano = "",
    titulo = titulo,
    isbn = "",
    tipo = "",
    edicao = "",
    editora = "",
    estrato_ = "",
    equipe = "TOTAL",
    pontos = sum(pontos, na.rm = TRUE),
    flag_relevancia = ""
  )
  producao_livros <- rbind(producao_livros, ap, row.names = NULL)

  producao_livros

}
