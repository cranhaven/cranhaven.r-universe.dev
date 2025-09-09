#'Estimates using polynomial equations.
#'@description
#'Determination of maximum technical efficiency (MTE) and plateau regression.
#'@param indep Name of the column with the independent variable.
#'@param dep Name of the dependent variable column
#'@param type Type of analysis to be carried out. Use 'MTE' to extract the
#'maximum technical efficiency or 'plateau' for plateau regression.
#'@param alpha Significance of the test.
#'@param verbose Logical argument. Runs the code silently if FALSE.
#' @return Calculates the maximum technical efficiency (MTE) based on a quadratic
#' polynomial model, if it is significant. The MTE is given by:
#' \deqn{MTE = -\frac{\beta_1}{2\beta_2}}
#'
#' It also calculates plateau regression parameters, returning:
#' - The plateau value: \deqn{Y_{plateau} = \beta_0 + \beta_1 X_{plateau} + \beta_2 X_{plateau}^2}
#' - The growth rate: \deqn{\beta_1}
#' - The inflection point: \deqn{X_{inflection} = -\frac{\beta_1}{2\beta_2}}
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@examples
#'library(EstimateBreed)
#'data("mtcars")
#'
#'met<-with(mtcars,linearest(wt,mpg,type = "MTE",verbose=TRUE))
#'@export

linearest <- function(indep,dep,type=NULL,alpha=0.05,verbose=FALSE){
  if (is.null(type)) {
    stop("Enter the analysis to be carried out in the 'type'")
  }

  if(type=="MTE") {
    mod1 <- lm(dep ~ indep + I(indep^2))
    resumo <- summary(mod1)
    pvalue <- resumo$coefficients[3, 4]

    if(pvalue < alpha) {
      intercept <- as.numeric(mod1$coefficients["(Intercept)"])
      bval <- as.numeric(mod1$coefficients["indep"])
      cval <- as.numeric(mod1$coefficients["I(indep^2)"])
      MET <- -bval / (2 * cval)
      est_y <- intercept + bval * MET + cval * MET^2
      if(verbose==TRUE){
        print(resumo)
        cat("MTE =", paste0(round(MET, digits = 5), "\n"))
        cat("y_MTE =", paste0(round(est_y, digits = 5), "\n"))
      }
      return(resumo)
    } else {
      print(resumo)
      return("The quadratic coefficient is not significant")
    }}
  if (type == "plateau") {
    tryCatch({
      L_init <- max(dep)
      k_init <- 0.1
      x0_init <- mean(indep)

      modelo_logistico <- nls(dep ~ L / (1 + exp(-k * (indep - x0))),
                              start = list(L = L_init, k = k_init, x0 = x0_init),
                              control = nls.control(maxiter = 1000))

      resumo_log <- summary(modelo_logistico)
      print(resumo_log)

      L <- coef(modelo_logistico)["L"]
      k <- coef(modelo_logistico)["k"]
      x0 <- coef(modelo_logistico)["x0"]
      if(verbose==TRUE){
        cat("Plateau value (L):", round(L, digits = 5), "\n")
        cat("Growth rate (k):", round(k, digits = 5), "\n")
        cat("Inflection point (x0):", round(x0, digits = 5), "\n")
      }
      return(list(plato = L, taxa_crescimento = k, ponto_inflexao = x0))
    }, error = function(e) {
      cat("Model fitting error:", e$message, "\n")
      return(NULL)
    })
  }
}
