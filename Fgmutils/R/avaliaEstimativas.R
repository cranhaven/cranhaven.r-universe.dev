##' @title calculate Estimates
##' @description given a list of observations and an estimated list of these observations this function evaluates how close it is the estimated value of observed and saves the differences
##' @param observado list containing the observations of variable
##' @param estimado list containing estimates of variable
##' @param estatisticas list of arg to calc estatistics
##' @param ajuste is ajust obtained a function like lm or nlsLM
##' @param graficos list of arg to plot graphics
##' @param salvarEm directory to save files
##' @param nome name of files will be save
##' @return will be returned
##' @export
avaliaEstimativas <- function(observado, estimado, estatisticas, ajuste = NULL, graficos = NULL,
                              salvarEm = NULL, nome = "observadoXestimado"){
  retorno = list()
  if ("funcoes" %in% labels(estatisticas)) {
    addParametros = c("observado", "estimado", "dfEstatisticas",
                      "coeficientes")
    if (!is.null(ajuste))
      addParametros = c(addParametros, "ajuste")
    if (!is.null(contemParametros(estatisticas$funcoes, estatisticas,
                                  addParametros)))
      stop(paste0("ERROR: quantidade de parameteros insuficiente para estatisticas, falta: ",
                  toString(contemParametros(estatisticas$funcoes,
                                            estatisticas, addParametros))))
  }
  else stop("informe as funcoes de estatisticas. avaliaEstimativas(estatisticas = list(funcoes = c(..")
  if (!is.null(ajuste)) {
    if (is.null(estimado) || is.nan(estimado))
      estimado = fitted(ajuste)
    retorno$ajuste = ajuste
    retorno$coeficientes.ajuste = coef(summary(ajuste))
  }
  testF = lm(estimado ~ observado)
  coefs = coef(summary(testF))
  retorno$coeficientes.testF = coefs
  if (!is.null(ajuste))
    coefs = rbind(coef(summary(ajuste)), coefs)
  dfEst = NULL
  args = ""
  for (i in 1:length(labels(estatisticas))) args = paste0(args,
                                                          ", ", labels(estatisticas)[[i]], " = estatisticas$",
                                                          labels(estatisticas)[[i]])
  if (1 <= length(estatisticas$funcoes))
    for (i in 1:length(estatisticas$funcoes)) {
      eval(parse(text = paste0("dfEst = estatisticas$funcoes[[i]](\n        observado = observado,\n        estimado = estimado,\n        dfEstatisticas = dfEst,\n        ajuste = ajuste,\n        coeficientes = coefs",
                               args, ")")))
    }
  if (!is.null(salvarEm)) {
    nome = paste0(salvarEm, nome)
    if (!dir.exists(salvarEm))
      dir.create(salvarEm, showWarnings = TRUE, recursive = TRUE,
                 mode = "0777")
    if (!is.null(ajuste))
      capture.output(summary(ajuste), file = paste0(nome,
                                                    " ajuste summary.txt"))
    write.csv(x = dfEst$estatisticas, file = paste0(nome,
                                                    " - estatisticas.csv"), row.names = F)
    write.csv(x = dfEst$estatisticasDoModelo, file = paste0(nome,
                                                            " - estatisticas do modelo.csv"), row.names = F)
    write.csv(x = coefs, file = paste0(nome, " - coeficientes.csv"))
  }
  if (!is.null(graficos)) {
    if (!is.null(contemParametros(graficos$funcoes, graficos,
                                  c(addParametros, "estatisticas"))))
      stop(paste0("ERROR: quantidade de parameteros insuficiente para graficos, falta: ",
                  toString(contemParametros(graficos$funcoes, graficos,
                                            c(addParametros, "estatisticas")))))
    args = ""
    for (i in 1:length(labels(graficos))) args = paste0(args,
                                                        ", ", labels(graficos)[[i]], " = graficos$", labels(graficos)[[i]])
    for (i in 1:length(graficos$funcoes)) {
      eval(parse(text = paste0("graficos$funcoes[[i]](\n        observado = observado,\n        estimado = estimado,\n        ajuste = ajuste,\n        coeficientes = coefs,\n        estatisticas = dfEst",
                               args, ")")))
    }
  }
  B0 = abs(testF$coefficients[1])
  B1 = abs(testF$coefficients[2] - 1)
  retorno$ranking = data.frame(b0 = testF$coefficients[1],
                               b1 = testF$coefficients[2], rankingB0 = B0, rankingB1 = B1)
  retorno$observado = observado
  retorno$estimado = estimado
  retorno$estatisticas = dfEst
  return(retorno)
}
