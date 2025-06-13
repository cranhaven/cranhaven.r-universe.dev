#' @import lme4
#' @import stats4
#' @import stats
#' @import plyr
#' @import methods
#' @import ggplot2
#' @import graphics
#' @import grDevices
#' @export
glimem <-
function(simData, sim0, sim1, family = binomial, link = "logit", nAGQ, control = glmerControl(optimizer = "bobyqa")){
    m <- NULL
    n <- NULL

    simData$predMu = NULL
    simData$r = NULL
    coefficients = NULL
    rows = NULL

    for (i in sim0:sim1){
    Datarows = which(simData$simulation == i)
    NewData <- simData[Datarows,]
    NewData <- within(NewData,{
      dose <- factor(dose)
      Trial <- as.integer(Trial)
      n <- as.numeric(n)
      x <- as.numeric(x)
    })

    m <- glmer(x/n ~ dose + (1|Trial), data = NewData, family = family,
               na.action = "na.fail", nAGQ = nAGQ, control = control, weights = n)
    
    coefficients <- coef(m)

    pred <- predict(m, type = "response")
    resid <- residuals(m, type = "pearson")
    simData[Datarows, "PredMu"] = pred
    simData[Datarows, "r"] = resid
    rows = c(rows, Datarows)
    }
    
    res <- list(m = m, coeff = coefficients, simData = simData[rows, ])
    return(res)
}
