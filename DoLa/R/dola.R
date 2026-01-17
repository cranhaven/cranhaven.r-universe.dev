#' @title Do Currículo Lattes Para o Programa de Pós-Graduação
#'
#' @description
#' Extrai os dados de arquivos xml exportados pelo currículo Lattes
#' e elabora um relatório do periodo selecionado para o programa de pós-graduação (PPG).
#' Elabora uma sintese por período e por curso (especialização - lato sensu,
#' mestrado e doutorado - stricto sensu) da produção bibliográfica com e sem
#' a participação de discentes, de trabalhos em eventos,
#' da produção técnica ou tecnológica,
#' das orientações em andamento e concluídas, dos projetos de pesquisa,
#' dos intercâmbios (professor visitante, pós-doutorado ou afastamento de curta duração),
#' das premiações e de indicadores gerais de atividades.
#' Ao final, elabora um relatório detalhado para cada docente das informações
#' acima e da participação em bancas, da produção associada aos projetos de pesquisa,
#' das colaborações técnicas (comitê assessor, corpo editorial) e das disciplinas ministradas.
#'
#' @param ano_ini ano de inicio da avaliação
#' @param ano_fim ano final da avaliação
#' @param nome_instituicao nome da Instituição. Aceita diversos nomes concatenados.
#' @param nome_ppg nome do Programa de Pós-graduação. Aceita diversos nomes concatenados.
#' @param nome_area nome da Área de Avaliação do PPG na CAPES.
#' @param xlsx_qualis nome do arquivo Excel com a classificação Qualis Periódicos CAPES.
#' @param xlsx_qualis_livros nome do arquivo Excel com a classificação Qualis Livros CAPES. Padrão é \code{NULL}
#' @param cv_docentes diretório com os currículos Lattes dos Docentes do PPG (arquivos xml zipados)
#' @param cv_discentes diretório com os currículos Lattes dos Discentes do PPG (arquivos xml zipados)
#'
#' @details
#' Para o nome da instituição e do PPG podem ser informados vários nomes concatenados,
#' para garantir a correta extração das informações, uma vez que pode não haver uma
#' padronização na escrita dos nomes no currículo dos diferentes docentes.
#'
#' O nome da área é usado para verificar se as publicações estão
#' classificadas na Área de Avaliação da Capes selecionada.
#' Quando não estão são indicadas as áreas onde há a classificação.
#'
#' O arquivo xlsx Qualis Periódicos pode ser acessado na
#' \href{https://sucupira-legado.capes.gov.br/sucupira/public/consultas/coleta/veiculoPublicacaoQualis/listaConsultaGeralPeriodicos.jsf}{Plataforma Sucupira}.
#'
#' O arquivo xlsx Qualis Livros pode ser acessado na página das
#' \href{https://www.gov.br/capes/pt-br/acesso-a-informacao/acoes-e-programas/avaliacao/sobre-a-avaliacao/areas-avaliacao/sobre-as-areas-de-avaliacao/sobre-as-areas-de-avaliacao}{Áreas de Avaliação da CAPES}.
#' Caso não esteja lá, solicite a coordenação de Área
#' para que disponibilize o documento público. Como não há uma padronização única
#' para os arquivos com as informações sobre o Qualis Livros,
#' o arquivo deve ser configurado para conter, no mínimo, as colunas com os
#' seguintes nomes: "Titulo", "ISBN" e "Estrato". O carregamento desse arquivo
#' é facultativo.
#'
#' Os currículos dos docentes e discentes devem ser acessados pelos próprios
#' pesquisadores na \href{https://lattes.cnpq.br/}{Plataforma Lattes}, acessando
#' a aba "atualizar currículo". Uma vez logado na plataforma, o arquivo no
#' formato xml (zipado) pode ser exportado acessando o
#' "menu secundário - Exportar" (aba lateral).
#'
#' Os arquivos dos currículos dos docentes e dos discentes
#' devem ser armazenados em diretórios separados.
#' Os currículos dos discentes são utilizados
#' para identificar as publicações com discente, a partir do id Lattes, do
#' nome completo, ou do nome em citações dos discentes. Para a correta identificação
#' dos discentes, sugere-se manter atualizada a lista de autores citados no
#' currículo Lattes, bem como a identificação dos co-autores (ambos presentes
#' no Menu Secundário da página inicial de atualização do currículo Lattes).
#'
#' Como há divergências na padronização das informações entre
#' a Plataforma Sucupira (CAPES) e o Currículo Lattes (CNPq), algumas
#' informações estão em campos distintos com nomes distintos nas duas plataformas.
#' De modo semelhante, algumas informações podem ser colocadas em diferentes campos do
#' currículo Lattes, a depender da interpretação do docente ou mesmo de uma certa
#' tradição da área de pesquisa. Outras informações podem ainda ser alimentadas na
#' Plataforma Lattes mas não há identificador específico (tag) para que este possa
#' ser extraído e utilizado na Plataforma Sucupira.
#' Assim, a fim de auxiliar no preenchimento da Plataforma Sucupira utilizando
#' o máximo possível de informações constantes no currículo Lattes, sugerimos
#' seguir as orientações de preenchimento constantes em Pagliosa e Nascimento (2020)
#' bem como o uso de algumas tags em campos específicos do
#' currículo Lattes para que estes possam ser corretamente identificados e extraídos.
#' Da mesma forma, na seção do relatório detalhado por docente incluímos
#' a classificação das atividades de acordo com os nomes utilizados
#' na Plataforma Sucupira.
#'
#' @returns
#' Retorna um \code{documento html} contendo tabelas de informação.
#'
#' @author
#' Paulo Pagliosa \email{paulo.pagliosa@ufsc.br}
#'
#' Fabiano Peruzzo Schwartz \email{fabiano.schwartz@camara.leg.br}
#'
#' @references
#' Pagliosa, P.R.; Nascimento, P.O. 2021. \href{https://repositorio.ufsc.br/bitstream/handle/123456789/231602/ManualLattesGeociencias11_2021_versaobeta%20%281%29.pdf?sequence=1&isAllowed=y}{Manual de Preenchimento do
#' Currículo Lattes: com ênfase na área de Geociências da CAPES}. 57p.
#'
#' @examples
#' \dontrun{
#' path_to_DoLa<- dola(ano_ini = 2019, ano_fim = 2024,
#'     nome_instituicao = c("Universidade Federal de Santa Catarina"),
#'     nome_ppg = c("Programa de Pós-graduação em Oceanografia","oceanografia"),
#'     nome_area = c("Geociências"),
#'     xlsx_qualis = system.file("extdata","Qualis.xlsx", package="DoLa"),
#'     cv_docentes = system.file("extdata","cv_do", package="DoLa"),
#'     cv_discentes = system.file("extdata","cv_di", package="DoLa"))
#' path_to_DoLa}
#'
#'
#' @import XML
#' @import stringr
#' @import knitr
#' @import dplyr
#' @import reshape2
#' @import openxlsx
#' @import rmarkdown
#' @import utils
#'
#' @include artigos.R
#' @include banca.R
#' @include colaboracao_tecnica.R
#' @include disciplina.R
#' @include encode_xml2.R
#' @include linhas_pesquisa.R
#' @include livros.R
#' @include orientacao.R
#' @include posdoc.R
#' @include premio.R
#' @include producao_associada.R
#' @include projeto.R
#' @include pvecd.R
#' @include tecnica.R
#' @include trab_anais.R
#'
#' @export

dola<- function(ano_ini,
                ano_fim,
                nome_instituicao,
                nome_ppg,
                nome_area,
                xlsx_qualis,
                xlsx_qualis_livros = NULL,
                cv_docentes,
                cv_discentes) {
  wd<-system.file("extdata", package="DoLa")
  dl<-system.file("extdata","DoLa.Rmd", package="DoLa")
  rmarkdown::render(input = dl,
                    output_format = "html_document",
                    output_file = "DoLa.html",
                    output_dir = wd,
                    knit_root_dir = wd,
                    envir = new.env()
  )
  path<- file.path(wd,"DoLa.html")
  if(!file.exists(path)) message("Verifique seus dados. Algo saiu errado.")
  if(interactive()) browseURL(path)

  return(path)
}
