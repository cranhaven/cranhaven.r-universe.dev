disciplina <- function(id, xml_data, nome_instituicao, nome_curso, ano_ini, ano_fim){

  producao_disciplina <- NULL

  pdisciplina <- xml_data$"DADOS-GERAIS"$"ATUACOES-PROFISSIONAIS"
  n1 <- length(pdisciplina)

  if(n1 > 0){
    # para todos os itens em ATUACOES-PROFISSIONAIS
    for(i in 1:n1){

      if(is.list(pdisciplina[[i]])){
        instituicao <- str_to_lower(as.vector(pdisciplina[[i]]$".attrs"["NOME-INSTITUICAO"]))
      } else {
        instituicao <- str_to_lower(pdisciplina[[i]]["NOME-INSTITUICAO"])
      }
      n2 <- length(pdisciplina[[i]])

      # verifica se eh ATIVIDADES-DE-ENSINO
      for(j in (1:n2)[names(pdisciplina[[i]]) == "ATIVIDADES-DE-ENSINO"]){

        p <- pdisciplina[[i]][[j]]
        n3 <- length(p)

        # para cada atividade de ensino, lista as disciplinas
        for(k in (1:n3)){

          ano_i <- as.vector(p[k]$"ENSINO"$".attrs"["ANO-INICIO"])
          mes_i <- as.vector(p[k]$"ENSINO"$".attrs"["MES-INICIO"])
          ano_f <- as.vector(p[k]$"ENSINO"$".attrs"["ANO-FIM"])
          mes_f <- as.vector(p[k]$"ENSINO"$".attrs"["MES-FIM"])
          curso <- str_to_lower(as.vector(p[k]$"ENSINO"$".attrs"["NOME-CURSO"]))
          nivel <- str_to_lower(as.vector(p[k]$"ENSINO"$".attrs"["TIPO-ENSINO"]))
          fl_programa <- (instituicao %in% nome_instituicao) & (curso %in% nome_curso)

          if((ano_i >= ano_ini & (ano_f <= ano_fim | ano_f == ""))){

            n4 <- length(p[k]$"ENSINO")

            # para cada disciplina
            for(l in (1:n4)[names(p[k]$"ENSINO") == "DISCIPLINA"]){

              pontos <-1

              disciplina <- p[k]$"ENSINO"[[l]]$"text"

              ap <- data.frame(
                id = id,
                mes_ini = mes_i,
                ano_ini = ano_i,
                mes_fim = mes_f,
                ano_fim = ano_f,
                nivel = nivel,
                disciplina = disciplina,
                instituicao = instituicao,
                curso = curso,
                pontos = pontos
              )

              producao_disciplina <- rbind(producao_disciplina, ap, row.names = NULL)

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
    mes_ini = "",
    ano_ini = "",
    mes_fim = "",
    ano_fim = "",
    nivel = "",
    disciplina = "",
    instituicao = "",
    curso = "TOTAL",
    pontos = sum(producao_disciplina$pontos, na.rm = TRUE)
  )
  producao_disciplina <- rbind(producao_disciplina, ap, row.names = NULL)

  # retorna o dataframe
  producao_disciplina
}
