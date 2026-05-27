#'
#' @title This function will start the Shiny application
#' @description Run locally an interactive Shiny application for Power and Sample Size determination.
#' @importFrom DT dataTableOutput  renderDataTable datatable JS
#' @importFrom easypower n.multiway
#' @import epiR
#' @importFrom EnvStats aovN aovPower ciNormN ciBinomN propTestPower propTestN
#' @importFrom ICC.Sample.Size calculateIccSampleSize
#' @importFrom kappaSize PowerBinary Power3Cats Power4Cats Power5Cats
#' @importFrom plotly plotlyOutput renderPlotly ggplotly layout
#' @importFrom powerMediation ssLongFull ss.SLR ss.SLR.rho SSizeLogisticCon SSizeLogisticBin
#' @importFrom powerSurvEpi ssizeCT.default ssizeEpiCont.default
#' @import presize
#' @importFrom pROC power.roc.test
#' @importFrom pwr pwr.t.test pwr.p.test pwr.2p.test ES.h pwr.chisq.test pwr.r.test pwr.anova.test
#' @importFrom pwr2 pwr.1way ss.2way
#' @importFrom shinycssloaders withSpinner
#' @importFrom shiny actionButton	br	checkboxInput	code	column	conditionalPanel	div	downloadButton	downloadHandler	fluidPage	fluidRow	h3	hr	HTML	includeMarkdown	mainPanel	navbarMenu	navbarPage	numericInput	observeEvent	p	plotOutput	radioButtons	reactive	renderPlot	renderText	renderUI	req	selectInput	sidebarLayout	sidebarPanel	sliderInput	tabPanel	tabsetPanel	textAreaInput	textInput	titlePanel	uiOutput	validate	wellPanel	withMathJax
#' @import shinyFeedback
#' @importFrom  shinyhelper helper observe_helpers
#' @import dplyr
#' @import ggplot2
#' @importFrom  writexl write_xlsx
#' @export
#' @author Unidade de Bioestatística, Diretoria de Pesquisa, Hospital de Clínicas de Porto Alegre.
#' @encoding UTF-8
#' @examples
#' if(interactive()){
#' PSS_Health()
#' }
#'
#' @seealso {
#'
#' Borges, R., Mancuso, A., Camey, S., Leotti, V., Hirakata, V., Azambuja, G., & Castro, S. (2021). Power and Sample Size for Health Researchers: uma ferramenta para cálculo de tamanho amostral e poder do teste voltado a pesquisadores da área da saúde. Clinical & Biomedical Research, 40(4). Retrieved from \url{https://seer.ufrgs.br/hcpa/article/view/109542}
#'
#' Castro, S. M. de J., Branco, A. C., Camey, S. A., Leotti, V. B., Hirakata, V. N., & Borges, R. B. (2021). PSS Health: como calcular tamanho de amostra para estimar média, proporção e correlação. Clinical and Biomedical Research, 41(3). Retrieved from de \url{https://seer.ufrgs.br/index.php/hcpa/article/view/112466}
#'
#' Hirakata, V. N., Mancuso, A. C. B., Castro, S. M. de J., Camey, S. A., Leotti, V. B., & Borges, R. B. (2022). PSS Health: como calcular tamanho de amostra para testes de comparação de médias de dois grupos. Clinical and Biomedical Research, 42(2). Retrieved from de \url{https://seer.ufrgs.br/index.php/hcpa/article/view/120997}
#'
#' Leotti, V. B., Castro, S. M. de J., Mancuso, A. C. B., S. A. & Borges, Hirakata, V. N., Camey, R. B. (2023). PSS Health: como calcular tamanho de amostra para testar relações de variáveis com um desfecho binário. Clinical and Biomedical Research, 42(4). Retrieved from de \url{https://seer.ufrgs.br/index.php/hcpa/article/view/126843}
#' }



PSS_Health <- function(){

  appDir <- system.file("PSS.Health", package = "PSS.Health")
  if (appDir == "") {
    stop("Could not find PSS.Health package. Try re-installing `PSS.Health`.", call. = FALSE)
  }

  shiny::runApp(appDir = appDir, launch.browser = TRUE)
}

