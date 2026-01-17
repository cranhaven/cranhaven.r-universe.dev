
pvecd <- function(id, xml_data,
                   ano_ini, ano_fim) {

producao_pvecd <- NULL

# identifica as tags de prof visitante e estagio de curta duracao
pv_ect <- xml_data$"DADOS-GERAIS"$"ATUACOES-PROFISSIONAIS"
n1 <- length(pv_ect)

# seleciona cada atuacao profissional
if(n1 > 0){
for(i in 1:length(pv_ect)){
  this.pv_ect<-pv_ect[i]
  instituicao<-this.pv_ect$"ATUACAO-PROFISSIONAL"$.attrs["NOME-INSTITUICAO"]

    for(k in (1:length(this.pv_ect$"ATUACAO-PROFISSIONAL"))[names(this.pv_ect$"ATUACAO-PROFISSIONAL") == "VINCULOS"]) {
    ano<- as.vector(this.pv_ect$"ATUACAO-PROFISSIONAL"[k]$"VINCULOS"["ANO-FIM"])

    if((ano >= ano_ini & ano <= ano_fim) | (ano == "")){

    if(this.pv_ect$"ATUACAO-PROFISSIONAL"[k]$"VINCULOS"["TIPO-DE-VINCULO"] == "PROFESSOR_VISITANTE") {
      vinculo<-"Professor visitante"
      mi<- this.pv_ect$"ATUACAO-PROFISSIONAL"[k]$"VINCULOS"["MES-INICIO"]
      ai<- this.pv_ect$"ATUACAO-PROFISSIONAL"[k]$"VINCULOS"["ANO-INICIO"]
      mf<- this.pv_ect$"ATUACAO-PROFISSIONAL"[k]$"VINCULOS"["MES-FIM"]

      pontos <-1

      ap <- data.frame(
        id = id,
        mes_inicio = mi,
        ano_inicio = ai,
        mes_fim = mf,
        ano_fim = ano,
        instituicao = instituicao,
        vinculo = vinculo,
        pontos = pontos
      )
      producao_pvecd <- rbind(producao_pvecd, ap, row.names = NULL)
    }

    if(this.pv_ect$"ATUACAO-PROFISSIONAL"[k]$"VINCULOS"["OUTRO-VINCULO-INFORMADO"] == "Estagio de curta duracao") {
      vinculo<-"Estagio de curta duracao"
    mi<- this.pv_ect$"ATUACAO-PROFISSIONAL"[k]$"VINCULOS"["MES-INICIO"]
    ai<- this.pv_ect$"ATUACAO-PROFISSIONAL"[k]$"VINCULOS"["ANO-INICIO"]
    mf<- this.pv_ect$"ATUACAO-PROFISSIONAL"[k]$"VINCULOS"["MES-FIM"]

    pontos <-1

    ap <- data.frame(
      id = id,
      mes_inicio = mi,
      ano_inicio = ai,
      mes_fim = mf,
      ano_fim = ano,
      instituicao = instituicao,
      vinculo = vinculo,
      pontos = pontos
    )
    producao_pvecd <- rbind(producao_pvecd, ap, row.names = NULL)
    }
   }
  }
}
}
#####################################
# insere linha de totais
ap <- data.frame(
  id = id,
  mes_inicio = "",
  ano_inicio = "",
  mes_fim = "",
  ano_fim = "",
  instituicao = "",
  vinculo = "TOTAL",
  pontos = sum(producao_pvecd$pontos, na.rm = TRUE)
)
producao_pvecd <- rbind(producao_pvecd, ap, row.names = NULL)

# retorna o dataframe
producao_pvecd
}
