
premio <- function(id, xml_data, ano_ini, ano_fim) {

producao_premio <- NULL

# identifica as tags de premios
prem <- xml_data$"DADOS-GERAIS"$"PREMIOS-TITULOS"
n1 <- length(prem)

# seleciona cada premio
if(n1 > 0){
for(i in 1:length(prem)) {
  titulo<-prem[i]$"PREMIO-TITULO"["NOME-DO-PREMIO-OU-TITULO"]
  instituicao<-prem[i]$"PREMIO-TITULO"["NOME-DA-ENTIDADE-PROMOTORA"]
  ano<- as.vector(prem[i]$"PREMIO-TITULO"["ANO-DA-PREMIACAO"])

  if((ano >= ano_ini & ano <= ano_fim) | (ano == "")){

    pontos <- 1

    ap <- data.frame(
      id = id,
      ano = ano,
      instituicao = instituicao,
      titulo = titulo,
      pontos = pontos
    )
    producao_premio <- rbind(producao_premio, ap, row.names = NULL)
    }
}
}
#####################################
# insere linha de totais
ap <- data.frame(
  id = id,
  ano = "",
  instituicao = "",
  titulo = "TOTAL",
  pontos = sum(producao_premio$pontos, na.rm = TRUE)
)

producao_premio <- rbind(producao_premio, ap, row.names = NULL)

# retorna o dataframe
producao_premio
}
