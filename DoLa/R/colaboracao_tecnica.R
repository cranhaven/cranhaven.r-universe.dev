
coltec <- function(id, xml_data, qualis_periodicos,  nome_area, ano_ini, ano_fim){

  producao_coltec <- NULL

  pcoltec <- xml_data$"DADOS-GERAIS"$"ATUACOES-PROFISSIONAIS"
  n1 <- length(pcoltec)

  if(n1 > 0){
    # para todos os itens em ATUACOES-PROFISSIONAIS
    for(i in 1:n1){

      n2 <- length(pcoltec[[i]])

      # verifica se eh VINCULOS
      for(l in (1:n2)[names(pcoltec[[i]]) == "VINCULOS"]){

        p <- pcoltec[[i]][[l]]
        instituicao <- as.vector(pcoltec[[i]]$".attrs"["NOME-INSTITUICAO"])
        vinculo <- as.vector(p[12]) # OUTRO-VINCULO-INFORMADO

        # verifica se eh um item de Colaboracao Tecnica
        ctec <- c("Membro de comite assessor", "Membro de corpo editorial")

        if(vinculo %in% ctec){

          anoI <- as.vector(p[7]) # ANO-inicio
          ano <- as.vector(p[9]) # ANO-FIM

          if((ano >= ano_ini & ano <= ano_fim) | (ano == "")){

            # vinclulo == "Membro de comite assessor"
            if(vinculo == "Membro de comite assessor"){

              pontos <-1

              # vinclulo == "Membro de corpo editorial"
            } else if(vinculo == "Membro de corpo editorial"){

              outras_inf <- as.vector(p[10]) # OUTRAS-INFORMACOES

              # procura o ISSN na primeira linha
              ISSN <- str_split(outras_inf[1],"\n")[[1]][1]

              # seta flag qualificacao
              ISSN <- gsub("-", "", ISSN)
              ISSN <- gsub(">", "", gsub("<", "", ISSN))

              eq <- as.character(qualis_periodicos[(qualis_periodicos$Area %in% nome_area) &
                                                     qualis_periodicos$ISSN == ISSN, "Estrato"][1])
              if(!is.na(eq)) vinculo <- paste(vinculo, "-", eq)
              fl_qualificado <- eq %in% c("A4","A3", "A2", "A1")


              # procura a indica??o de EDITOR na segunda linha
              editor <- str_split(outras_inf[1],"\n")[[1]][2]

              # seta flag editor
              if(!is.na(editor)){
                fl_editor <- editor == "<EDITOR>"
                if(fl_editor) vinculo <- paste(vinculo, "-", gsub(">", "", gsub("<", "", editor)))
              } else fl_editor = FALSE

              pontos <-1
            }

            ap <- data.frame(
              id = id,
              ano_inicio = anoI,
              ano_fim = ano,
              vinculo = vinculo,
              instituicao = instituicao,
              pontos = pontos
            )

            producao_coltec <- rbind(producao_coltec, ap, row.names = NULL)

          }
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
    vinculo = "",
    instituicao = "TOTAL",
    pontos = sum(producao_coltec$pontos, na.rm = TRUE)
  )
  producao_coltec <- rbind(producao_coltec, ap, row.names = NULL)

  # retorna o dataframe
  producao_coltec
}
