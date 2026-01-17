orientacao <- function(id, xml_data, nome_instituicao, nome_ppg, ano_ini, ano_fim){

  nome_ppg <- str_replace_all(nome_ppg, "[^[:alnum:]]", "")
  nome_instituicao <- str_replace_all(nome_instituicao, "[^[:alnum:]]", "")

  producao_ori <- NULL

  pori <- xml_data$"OUTRA-PRODUCAO"$"ORIENTACOES-CONCLUIDAS"
  n <- length(pori)

  # para todos os itens em
  # ORIENTACOES-CONCLUIDAS
  if(n > 0){
    for(i in (1:n)){

      ano <- as.numeric(pori[[i]][[1]]["ANO"])

      # flag relevancia
      fl_rlvnc <- pori[[i]][[1]]['FLAG-RELEVANCIA']


      if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){

        # PORI[[i]][[1]] - DADOS-BASICOS-
        # PORI[[i]][[2]] - DETALHAMENTO-

        # DADOS-BASICOS- ..... NATUREZA
        nivel <- as.vector(pori[[i]][[1]]["NATUREZA"])
        nivel <- str_to_lower(gsub("_", " ", nivel))

        # DADOS-BASICOS- ..... TITULO
        titulo <- as.vector(pori[[i]][[1]]["TITULO"])

        # DETALHAMENTO- ... NOME-DO-ORIENTADO
        orientando <- as.vector(pori[[i]][[2]]["NOME-DO-ORIENTADO"])

        # DETALHAMENTO- ... TIPO-DE-ORIENTACAO
        tipo <- as.vector(pori[[i]][[2]]["TIPO-DE-ORIENTACAO"])
        if(is.na(tipo)){
          # tipo <- as.vector(pori[[i]][[2]]["TIPO-DE-ORIENTACAO-CONCLUIDA"])
          tipo <- "orientador principal"

          # o Lattes nao admite informar o tipo de orientacao para especializacao,
          # iniciacao cientfica, profissionalizacao etc
          # contudo, marca automaticamente como CO-ORIENTADOR, o que eh irreal
          # portanto, corrige-se o tipo para "orientador principal"
        }
        tipo <- str_to_lower(sub("_", " ", tipo))

        # DETALHAMENTO- ... NOME-DA-INSTITUICAO
        instituicao <- as.vector(pori[[i]][[2]]["NOME-DA-INSTITUICAO"])
        if(is.null(instituicao)) instituicao <- ""
        instituicao <- str_to_lower(instituicao)
        instituicao_aux <- str_replace_all(instituicao, "[^[:alnum:]]", "")

        # DETALHAMENTO- ... NOME-DO-CURSO
        curso <- as.vector(pori[[i]][[2]]["NOME-DO-CURSO"])
        if(is.null(curso)) curso <- ""
        curso <- str_to_lower(curso)
        curso_aux <- str_replace_all(curso, "[^[:alnum:]]", "")

        # pontua somente para instituicao/curso definidos
        if((instituicao_aux %in% nome_instituicao) & (curso_aux %in% nome_ppg) &
           (nivel == "monografia de conclusao de curso aperfeicoamento e especializacao" | nivel == "dissertacao de mestrado" | nivel == "tese de doutorado")) {

          pontos <- 1
            } else pontos <- 0

        if(!(ano >= ano_ini & ano <= ano_fim)){
          pontos <- 0
        }

        ap <- data.frame(
          id = id,
          ano = ano,
          titulo = titulo,
          tipo = tipo,
          nivel = nivel,
          orientando = orientando,
          situacao = "concluida",
          curso = curso,
          instituicao = instituicao,
          pontos = pontos,
          flag_relevancia = fl_rlvnc
        )

        producao_ori <- rbind(producao_ori, ap, row.names = NULL)
      }
    }
  }


  pori <- xml_data$"DADOS-COMPLEMENTARES"$"ORIENTACOES-EM-ANDAMENTO"
  n <- length(pori)

  # para todos os itens em
  # ORIENTACOES-EM-ANDAMENTO
  if(n > 0){
    for(i in (1:n)){

      ano <- as.numeric(pori[[i]][[1]]["ANO"])

      if(ano >= ano_ini & ano <= ano_fim){

        # PORI[[i]][[1]] - DADOS-BASICOS-
        # PORI[[i]][[2]] - DETALHAMENTO-

        # DADOS-BASICOS- ..... NATUREZA
        nivel <- as.vector(pori[[i]][[1]]["NATUREZA"])
        nivel <- str_to_lower(gsub("_", " ", nivel))

        # DADOS-BASICOS- ..... TITULO-DO-TRABALHO
        titulo <- as.vector(pori[[i]][[1]]["TITULO-DO-TRABALHO"])

        # DETALHAMENTO- ... NOME-DO-ORIENTANDO
        orientando <- as.vector(pori[[i]][[2]]["NOME-DO-ORIENTANDO"])

        # DETALHAMENTO- ... TIPO-DE-ORIENTACAO
        tipo <- as.vector(pori[[i]][[2]]["TIPO-DE-ORIENTACAO"])
        if(is.na(tipo)){
          # tipo <- as.vector(pori[[i]][[2]]["TIPO-DE-ORIENTACAO-EM-ANDAMENTO"])
          tipo <- "orientador principal"

          # o Lattes n?o admite informar o tipo de orientacao para especializacao,
          # iniciacao cientfica, profissionalizacao etc
          # contudo, marca automaticamente como CO-ORIENTADOR, o que eh irreal
          # portanto, corrige-se o tipo para "orientador principal"
        }
        tipo <- str_to_lower(sub("_", " ", tipo))

        # DETALHAMENTO- ... NOME-INSTITUICAO
        instituicao <- as.vector(pori[[i]][[2]]["NOME-INSTITUICAO"])
        if(is.null(instituicao)) instituicao <- ""
        instituicao <- str_to_lower(instituicao)
        instituicao_aux <- str_replace_all(instituicao, "[^[:alnum:]]", "")

        # DETALHAMENTO- ... NOME-CURSO
        curso <- as.vector(pori[[i]][[2]]["NOME-CURSO"])
        if(is.null(curso)) curso <- ""
        curso <- str_to_lower(curso)
        curso_aux <- str_replace_all(curso, "[^[:alnum:]]", "")

        # pontua somente para instiruicao/curso definidos
        if((instituicao_aux %in% nome_instituicao) & (curso_aux %in% nome_ppg) &
           (nivel == "monografia de conclusao de curso aperfeicoamento e especializacao" | nivel == "dissertacao de mestrado" | nivel == "tese de doutorado")) {

          pontos <-1
        } else pontos <- 0

        ap <- data.frame(
          id = id,
          ano = ano,
          titulo = titulo,
          tipo = tipo,
          nivel = nivel,
          orientando = orientando,
          situacao = "andamento",
          curso = curso,
          instituicao = instituicao,
          pontos = pontos,
          flag_relevancia = ""
        )

        producao_ori <- rbind(producao_ori, ap, row.names = NULL)
      }
    }
  }





  #####################################
  # insere linha de totais
  ap <- data.frame(
    id = id,
    ano = "",
    titulo = "",
    tipo = "",
    nivel = "",
    orientando = "",
    situacao = "",
    curso = "",
    instituicao = "TOTAL",
    pontos = sum(producao_ori$pontos, na.rm = TRUE),
    flag_relevancia = ""
  )
  producao_ori <- rbind(producao_ori, ap, row.names = NULL)

  # retorna o dataframe
  producao_ori
}
