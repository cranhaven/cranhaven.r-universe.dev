
banca <- function(id, xml_data, nome_instituicao, nome_curso, ano_ini, ano_fim){

  nome_curso <- str_replace_all(nome_curso, "[^[:alnum:]]", "")
  nome_instituicao <- str_replace_all(nome_instituicao, "[^[:alnum:]]", "")

  producao_banca <- NULL

  pbanca <- xml_data$"DADOS-COMPLEMENTARES"$"PARTICIPACAO-EM-BANCA-TRABALHOS-CONCLUSAO"
  n <- length(pbanca)

  # para todos os itens em
  # PARTICIPACAO-EM-BANCA-TRABALHOS-CONCLUSAO
  if(n > 0){
    for(i in (1:n)){

      ano <- as.numeric(pbanca[[i]][[1]]["ANO"])

      if(ano >= ano_ini & ano <= ano_fim){

        # DADOS-BASICOS- ..... NATUREZA
        nivel <- as.vector(pbanca[[i]][[1]]["NATUREZA"])
        nivel <- str_to_lower(gsub("_", " ", nivel))

        # DADOS-BASICOS- ..... TITULO
        titulo <- as.vector(pbanca[[i]][[1]]["TITULO"])

        # DETALHAMENTO- ... NOME-DO-CANDIDATO
        candidato <- as.vector(pbanca[[i]][[2]]["NOME-DO-CANDIDATO"])

        # DETALHAMENTO- ... NOME-DA-INSTITUICAO
        instituicao <- as.vector(pbanca[[i]][[2]]["NOME-INSTITUICAO"])
        if(is.null(instituicao)) instituicao <- ""
        instituicao <- str_to_lower(instituicao)
        instituicao_aux <- str_replace_all(instituicao, "[^[:alnum:]]", "")

        # DETALHAMENTO- ... NOME-DO-CURSO
        curso <- as.vector(pbanca[[i]][[2]]["NOME-CURSO"])
        if(is.null(curso)) curso <- ""
        curso <- str_to_lower(curso)
        curso_aux <- str_replace_all(curso, "[^[:alnum:]]", "")

        pontos <- 1

        ap <- data.frame(
          id = id,
          ano = ano,
          titulo = titulo,
          nivel = nivel,
          candidato = candidato,
          curso = curso,
          instituicao = instituicao,
          pontos = pontos
        )

        producao_banca <- rbind(producao_banca, ap, row.names = NULL)
      }
    }
  }


  pbanca <- xml_data$"DADOS-COMPLEMENTARES"$"PARTICIPACAO-EM-BANCA-JULGADORA"
  n <- length(pbanca)

  # para todos os itens em
  # PARTICIPACAO-EM-BANCA-JULGADORA
  if(n > 0){
    for(i in (1:n)){

      ano <- as.numeric(pbanca[[i]][[1]]["ANO"])

      if(ano >= ano_ini & ano <= ano_fim){

        # DADOS-BASICOS- ..... NATUREZA
        nivel <- as.vector(pbanca[[i]][[1]]["NATUREZA"])
        nivel <- str_to_lower(gsub("_", " ", nivel))

        # DADOS-BASICOS- ..... TITULO
        titulo <- as.vector(pbanca[[i]][[1]]["TITULO"])

        candidato <- ""

        # DETALHAMENTO- ... NOME-DA-INSTITUICAO
        instituicao <- as.vector(pbanca[[i]][[2]]["NOME-INSTITUICAO"])
        if(is.null(instituicao)) instituicao <- ""
        instituicao <- str_to_lower(instituicao)
        instituicao_aux <- str_replace_all(instituicao, "[^[:alnum:]]", "")

        # quando for outro tipo de banca julgadora
          curso <- ""
          pontos <-1

        ap <- data.frame(
          id = id,
          ano = ano,
          titulo = titulo,
          nivel = paste("banca de comissao julgadora -", nivel),
          candidato = candidato,
          curso = curso,
          instituicao = instituicao,
          pontos = pontos
        )

        producao_banca <- rbind(producao_banca, ap, row.names = NULL)
      }
    }
  }

  #####################################
  # insere linha de totais
  ap <- data.frame(
    id = id,
    ano = "",
    titulo = "",
    nivel = "",
    candidato = "",
    curso = "",
    instituicao = "TOTAL",
    pontos = sum(producao_banca$pontos, na.rm = TRUE)
  )
  producao_banca <- rbind(producao_banca, ap, row.names = NULL)

  # retorna o dataframe
  producao_banca
}
