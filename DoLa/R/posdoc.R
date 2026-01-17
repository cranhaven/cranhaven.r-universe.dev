
posdoc <- function(id, xml_data, ano_ini, ano_fim) {

  producao_posdoc <- NULL

  # identifica as tags de formacao
  posd <- xml_data$"DADOS-GERAIS"$"FORMACAO-ACADEMICA-TITULACAO"
  n1 <- length(posd)

  # seleciona cada atuacao profissional
  if(n1 > 0){
    for(i in 1:n1){

      if(names(posd[i]) == "POS-DOUTORADO") {
        formacao<-"Pos-doutorado"

      if(is.list(posd[[i]])) {
        instituicao <- str_to_lower(as.vector(posd[[i]]$".attrs"["NOME-INSTITUICAO"]))
        ano<- str_to_lower(as.vector(posd[[i]]$".attrs"["ANO-DE-CONCLUSAO"]))
        ano_inicio <-str_to_lower(as.vector(posd[[i]]$".attrs"["ANO-DE-INICIO"]))
      } else {
        instituicao <- str_to_lower(posd[[i]]["NOME-INSTITUICAO"])
        ano<- str_to_lower(as.vector(posd[[i]]["ANO-DE-CONCLUSAO"]))
        ano_inicio <-str_to_lower(posd[[i]]["ANO-DE-INICIO"])
      }

        if((ano >= ano_ini & ano <= ano_fim) | (ano == "")){

          pontos <- 1

          ap <- data.frame(
            id = id,
            ano_inicio = ano_inicio,
            ano_fim = ano,
            formacao = formacao,
            instituicao = instituicao,
            pontos = pontos
          )
          producao_posdoc <- rbind(producao_posdoc, ap, row.names = NULL)
        }
      }

      if(names(posd[i]) == "LIVRE-DOCENCIA") {
        formacao<-"Livre-docencia"
        ano_inicio <-""

        if(is.list(posd[[i]])) {
          instituicao <- str_to_lower(as.vector(posd[[i]]$".attrs"["NOME-INSTITUICAO"]))
          ano<- str_to_lower(as.vector(posd[[i]]$".attrs"["ANO-DE-OBTENCAO-DO-TITULO"]))
        } else {
          instituicao <- str_to_lower(posd[[i]]["NOME-INSTITUICAO"])
          ano<- str_to_lower(posd[[i]]["ANO-DE-OBTENCAO-DO-TITULO"])
        }

        if((ano >= ano_ini & ano <= ano_fim) | (ano == "")){

          #          pontos <- pontos_posdoc(1)
          pontos <- 1

          ap <- data.frame(
            id = id,
            ano_inicio = ano_inicio,
            ano_fim = ano,
            formacao = formacao,
            instituicao = instituicao,
            pontos = pontos
          )
          producao_posdoc <- rbind(producao_posdoc, ap, row.names = NULL)
        }
      }
    }
  }
  #####################################
  # insere linha de totais
  ap <- data.frame(
    id = id,
    ano_inicio = "",
    ano_fim = "",
    formacao = "",
    instituicao = "TOTAL",
    pontos = sum(producao_posdoc$pontos, na.rm = TRUE)
  )
  producao_posdoc <- rbind(producao_posdoc, ap, row.names = NULL)

  # retorna o dataframe
  producao_posdoc

}

