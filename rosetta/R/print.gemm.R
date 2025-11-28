
#' print method of object of class gemm
#'
#' @param x object of class gemm
#' @param ... additional pars
#' @param digits  number of digits
#' @param silence boolean, if true out is not printed
#'
#' @export
#'
print.gemm <- function(x, ..., digits=2, silence = FALSE) {

  options(digits = digits)
  res <- list()
  ns <- "\n"
  if(isTRUE(getOption('knitr.in.progress'))) ns <- "\n\n"

  if (!silence) {
cat("The dependent variable is:     ", (x$input$yvar), ns)
cat("The predictor variable is:     ", (x$input$xvar), ns)
cat("The model contains", length(x$input$mvars),"mediators:",x$input$mvars,ns)

  if(length(x$input$xmmod) > 0) {cat("The moderators for the x-m path(s):",x$input$xmmod,ns)
  } else {cat("No moderators for the x-m path(s)",ns)}
  if(length(x$input$mymod) > 0) {cat("The moderators for the m-y path(s):",x$input$mymod,ns)
  } else {cat("No moderators for the m-y path(s)",ns)}
  if(length(x$input$cmvars) > 0) {cat("The covariates for the mediators:",x$input$cmvars, ns)
  } else {cat("No covariates for the mediators",ns)}
  if(length(x$input$cyvars) > 0) {cat("The covariates for the dependent:",x$input$cyvars, ns)
  } else {cat("No covariates for the dependent variable",ns)}

  }

  table1 <- data.frame(R2 = x$output$Rsq)
  row.names(table1) <- NULL
  table1$Dependent <- c(x$input$mvars, x$input$yvar)
  table1 <- table1[,c(2,1)]
  table1[,2] <- format(round(table1[,2], digits = 3), nsmall = 2)
  res$R2 <- table1
  if (!silence) {
     cat("\n\n")
     cat("Explained variance (R-square) of the mediators and dependent variable:\n");
     if(isTRUE(getOption('knitr.in.progress'))) {
       a <- knitr::kable(table1, format = "html", padding = 2, align = c("l", "r"),
                         caption = " ")
       a <- kableExtra::kable_styling(a, full_width = F, position = "left")
       print(a)
     } else {
       pander::pander(table1, justify = c("left", "right"))
     }

  }

  lmout <- x$output$parameterEstimates.total
  a <- summary(lmout)$coefficients
  b <- stats::confint(lmout)
  a <- as.data.frame(cbind(a,b))
  row.names(a) <- NULL
  terms <- c("intercept", paste0(x$input$xvar, " --> ", x$input$yvar))
  a[,"effect"] <- terms
  table2 <- a[,c(7,1:6)]
  table2[,c(2:7)] <- format(round(table2[,c(2:7)], digits = 3), nsmall = 2)
  colnames(table2) <- c("effect","est","se","t","pvalue", "ci.lower", "ci.upper")
  res$totalEff <- table2
  if (!silence) {
    cat("\n\n")
    cat("Estimate of total effect");
    if(isTRUE(getOption('knitr.in.progress'))) {
      a <- knitr::kable(table2, format = "html", padding = 2, align = c("l", "r", "r", "r","r","r","r"))
      a <- kableExtra::kable_styling(a, full_width = F, position = "left")
      print(a)
    } else {
      pander::pander(table2, justify = c("left", "right", "right", "right","right","right","right"))
    }

  }


  table3 <- x$output$parameterEstimates.apath
  row.names(table3) <- NULL
  names(table3) <- c("path",names(table3)[-1])
  if (is.null(x$input$xmmod)) {
    terms <- paste0(x$input$xvar, " --> ", x$input$mvars)
  } else {terms <- c(paste0(x$input$xvar, " --> ", x$input$mvars),
                     paste0(x$input$xmmod, " --> ", x$input$mvars),
                     paste0(x$input$xvar, " x ",x$input$xmmod, " --> ", x$input$mvars)) }
  table3[,1] <- terms
  table3[,c(2:7)] <- format(round(table3[,c(2:7)], digits = 3), nsmall = 2)
  res$aPaths <- table3
  if (!silence) {
    cat("\n\n")
    cat("Estimates of a-paths");
    if(isTRUE(getOption('knitr.in.progress'))) {
      a <- knitr::kable(table3, format = "html", padding = 2, align = c("l", "r", "r", "r","r","r","r"))
      a <- kableExtra::kable_styling(a, full_width = F, position = "left")
      print(a)
    } else {
      pander::pander(table3, justify = c("left", "right", "right", "right","right","right","right"))
    }
  }

  table4 <- x$output$parameterEstimates.bpath
  row.names(table4) <- NULL
  names(table4) <- c("path",names(table4)[-1])
  if (is.null(x$input$mymod)) {
    terms <- paste0(x$input$mvars, " --> ", x$input$yvar)
  } else {terms <- c(paste0(x$input$mvars, " --> ", x$input$yvar),
                     paste0(x$input$mymod, " --> ", x$input$yvar),
                     paste0(x$input$mvars," x ", x$input$mymod, " --> ", x$input$yvar))}
  table4[,1] <- terms
  table4[,c(2:7)] <- format(round(table4[,c(2:7)], digits = 3), nsmall = 2)
  res$bPaths <- table4
  if (!silence) {
    cat("\n\n")
    cat("Estimates of b-paths");
    if(isTRUE(getOption('knitr.in.progress'))) {
      a <- knitr::kable(table4, format = "html", padding = 2, align = c("l", "r", "r", "r","r","r","r"))
      a <- kableExtra::kable_styling(a, full_width = F, position = "left")
      print(a)
    } else {
      pander::pander(table4, justify = c("left", "right", "right", "right","right","right","right"))
    }
  }


  table5 <- x$output$parameterEstimates.direct
  row.names(table5) <- NULL
  table5[,1] <- paste0(x$input$xvar," --> ", x$input$yvar)
  names(table5) <- c("path",names(table5)[-1])
  table5[,c(2:7)] <- format(round(table5[,c(2:7)], digits = 3), nsmall = 2)
  res$directEff <- table5
  if (!silence) {
    cat("\n\n")
    cat("Direct effect (c') ");
    if(isTRUE(getOption('knitr.in.progress'))) {
      a <- knitr::kable(table5, format = "html", padding = 2, align = c("l", "r", "r", "r","r","r","r"))
      a <- kableExtra::kable_styling(a, full_width = F, position = "left")
      print(a)
    } else {
      pander::pander(table5, justify = c("left", "right", "right", "right","right","right","right"))
    }
  }


  table6 <- x$output$parameterEstimates.indirect.raw
  row.names(table6) <- NULL
  names(table6) <- c("through",names(table6)[-1])
  table6[,1] <- c(x$input$mvars, "total")
  table6[,c(2:7)] <- format(round(table6[,c(2:7)], digits = 3), nsmall = 2)
  res$indirectEff <- table6
  if (!silence) {
    cat("\n\n")
    cat("Indirect effects (a*b) ");
    if(isTRUE(getOption('knitr.in.progress'))) {
      a <- knitr::kable(table6, format = "html", padding = 2, align = c("l", "r", "r", "r","r","r","r"))
      a <- kableExtra::kable_styling(a, full_width = F, position = "left")
      print(a)
    } else {
      pander::pander(table6, justify = c("left", "right", "right", "right","right","right","right"))
    }
  }

  if (!is.null(x$input$cmvars) | !is.null(x$input$cyvars)) {
    table7 <- x$output$parameterEstimates.covs
    row.names(table7) <- terms1 <- terms2 <- NULL
    names(table7) <- c("path",names(table7)[-1])
    if (!is.null(x$input$cyvars)) {
      terms1 <- paste0(x$input$cyvars, " --> ", x$input$yvar) }
    if (!is.null(x$input$cmvars)) {
      lmv <- length(x$input$mvars)
      terms2 <- paste0(rep(x$input$cmvars, each=lmv), " --> ", x$input$mvars) }
    terms <- c(terms2,terms1)
    table7[,1] <- terms
    table7[,c(2:7)] <- format(round(table7[,c(2:7)], digits = 3), nsmall = 2)
    res$covs <- table7
    if (!silence) {
      cat("\n\n")
      cat("Estimates of covariates");
      if(isTRUE(getOption('knitr.in.progress'))) {
        a <- knitr::kable(table7, format = "html", padding = 2, align = c("l", "r", "r", "r","r","r","r"))
        a <- kableExtra::kable_styling(a, full_width = F, position = "left")
        print(a)
      } else {
        pander::pander(table7, justify = c("left", "right", "right", "right","right","right","right"))
      }
    }
  }

  table8 <- x$output$parameterEstimates.indirect.es_std
  row.names(table8) <- NULL
  table8[,7] <- c(x$input$mvars, "total")
  table8 <- table8[,c(7,1:6)]
  names(table8)[c(1,2)] <- c("through","est")
  table8[,c(2:7)] <- format(round(table8[,c(2:7)], digits = 3), nsmall = 2)
  res$csES <- table8
  if (!silence) {
    cat("\n\n")
    cat("Completely standardized effect sizes  ");
    if(isTRUE(getOption('knitr.in.progress'))) {
      a <- knitr::kable(table8, format = "html", padding = 2, align = c("l", "r", "r", "r","r","r","r"))
      a <- kableExtra::kable_styling(a, full_width = F, position = "left")
      print(a)
    } else {
      pander::pander(table8, justify = c("left", "right", "right", "right","right","right","right"))
    }
  }

  table9 <- x$output$parameterEstimates.indirect.es_rat
  row.names(table9) <- NULL
  table9[,7] <- c(x$input$mvars, "total")
  table9 <- table9[,c(7,1:6)]
  names(table9)[1] <- c("through")
  table9[,c(2:7)] <- format(round(table9[,c(2:7)], digits = 3), nsmall = 2)
  res$ratES <- table9
  if (!silence) {
    cat("\n\n")
    cat("Ratio (indirect to total) based effect sizes  ");
    if(isTRUE(getOption('knitr.in.progress'))) {
      a <- knitr::kable(table9, format = "html", padding = 2, align = c("l", "r", "r", "r","r","r","r"))
      a <- kableExtra::kable_styling(a, full_width = F, position = "left")
      print(a)
    } else {
      pander::pander(table9, justify = c("left", "right", "right", "right","right","right","right"))
    }
  }

  invisible(res)
}


