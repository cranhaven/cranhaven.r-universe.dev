
# devtools::build(path = "C:\\Users\\roger\\Downloads\\Temporario")


{
  library("shiny", quietly = TRUE, warn.conflicts = FALSE)
  library("dplyr", quietly = TRUE, warn.conflicts = FALSE)
  library("ggplot2", quietly = TRUE, warn.conflicts = FALSE)
  library("pwr2", quietly = TRUE, warn.conflicts = FALSE)
  # library(PSS.Health)
}



.help_buttom <- function(local, linguagem = "pt", body, title = "Ajuda"){

  # Os botoes de ajuda aparecerao apenas na versao em portugues
  if (linguagem == "pt") {
    shinyhelper::helper(
      shiny_tag = local,
      type = "inline",
      title = title,
      content = body,
      buttonLabel = "Fechar",
      fade = TRUE,
      colour = "#006338",
      size = "m"
    )
  } else {
    local
  }

}




#-----------------.
# Referencias ----
#-----------------.


# .data <- "2021/04/19"
# .data_citacao_f1 <- format.Date(Sys.Date(), "%Y %b. %e")
.data_citacao_f2 <- format.Date(Sys.Date(), "%e %b. %Y")
# .data_citacao_f3 <- format.Date(Sys.Date(), "%Y-%m-%d")
# .data_publicacao1 <- format.Date(as.Date(.data), "%Y %b. %e")
# .data_publicacao2 <- format.Date(as.Date(.data), "%e %b. %Y")






.txt_referencia_tap <- paste0(
  "<br><br><br><i>Sugestões de citação/ Suggested reference:</i><br>",
  "<p style=\"font-size:75% \">",

  paste0("<b>ABNT: </b> BORGES, Rogério Boff et al. Power and Sample Size for Health Researchers: uma ferramenta para cálculo de tamanho amostral e poder do teste voltado a pesquisadores da área da saúde. ",
         "<b>Clinical & Biomedical Research</b>, [S.l.], v. 40, n. 4, apr. 2021. ISSN 2357-9730. Available at: ",
         "<a href='https://doi.org/10.22491/2357-9730.109542' target='_blank'>&lthttps://doi.org/10.22491/2357-9730.109542&gt</a>",
         ". Date accessed: ", .data_citacao_f2, ".<br><br>"),

  paste0("<b>APA:</b> Borges, R., Mancuso, A., Camey, S., Leotti, V., Hirakata, V., Azambuja, G., & Castro, S. (2021). ",
         "Power and Sample Size for Health Researchers: uma ferramenta para cálculo de tamanho amostral e poder do teste voltado a pesquisadores da área da saúde. ",
         "<i>Clinical & Biomedical Research</i>, 40(4). Retrieved from ",
         "<a href='https://doi.org/10.22491/2357-9730.109542' target='_blank'>https://doi.org/10.22491/2357-9730.109542</a>",
         "<br><br>"),

  paste0("<b>BibTex: </b>",
         "@article{PSSHealth,<br>",
         "author = {Rogério Borges and Aline Mancuso and Suzi Camey and Vanessa Leotti and Vânia Hirakata and Guilherme Azambuja and Stela Castro},<br>",
         "title = {Power and Sample Size for Health Researchers: uma ferramenta para cálculo de tamanho amostral e poder do teste voltado a pesquisadores da área da saúde.},<br>",
         "journal = {Clinical & Biomedical Research},<br>",
         "volume = {40},<br>",
         "number = {4},<br>",
         "year = {2021},<br>",
         "keywords = {tamanho de amostra, poder do teste, estimação de parâmetros, comparação de grupos, R},<br>",
         "issn = {2357-9730},<br>",
         "url = {https://doi.org/10.22491/2357-9730.109542}<br>",
         "}"
  ),
  "</p>"
)








# Versão online? ----
.versao_online <- TRUE


.txt_citacao_tap <- paste(
  "ferramenta PSS Health versão",
  ifelse(!.versao_online, packageVersion("PSS.Health"), "on-line"),
  "(citação abaixo)"
)


.txt_citacao_tap_ingles <- paste(
  "PSS Health tool version",
  ifelse(!.versao_online, packageVersion("PSS.Health"), "on-line"),
  "(citation below)"
)


.txt_citacao_pss <- paste0(
  "Este cálculo foi realizado por meio da ferramenta PSS Health versão ",
  if (!.versao_online) {
    packageVersion("PSS.Health")
  } else{
    "on-line"
  },
  " (citação abaixo)."
)

