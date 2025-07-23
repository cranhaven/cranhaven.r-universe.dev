####################################################
### File name: MTest.r
####################################################
MTest <- function (object, nboot = 100, nsam = NULL, trace = FALSE, seed = NULL, 
                      valor_vif = 0.9) 
{
  datos <- object$model
  ff <- formula(object)
  if (is.null(nsam)) {
    nsam = nrow(datos)
  }
  vals <- 1:nrow(datos)
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  maux <- lm(ff, data = datos)
  
  if (any(attr(terms(object), "order") > 1)) {
    vif.global <- suppressMessages(car::vif(maux, type = "predictor"))
    vif.global <- vif.global[, 3]
  }else {
    vif.global <- car::vif(maux, type = "terms")
  }
  R.aux.global <- (vif.global - 1)/vif.global
  sm <- summary(maux)
  R.aux.global <- c(sm$r.squared, R.aux.global)
  names(R.aux.global)[1] <- "global"
  
  
  
  sol.rsq <- NULL
  sol.vif <- NULL
  i = 1
  
  while (i <= nboot) {
    sam <- sample(vals, nsam, replace = TRUE)
    aux <- datos[sam, ]
    maux <- lm(ff, data = aux)
    sm <- summary(maux)
    if (any(attr(terms(object), "order") > 1)) {
      vif.vals <- suppressMessages(car::vif(maux, type = "predictor"))
      vif.vals <- vif.vals[, 3]
    }
    else {
      vif.vals <- car::vif(maux, type = "terms")
    }
    Raux <- (vif.vals - 1)/vif.vals
    s1 <- c(sm$r.squared, Raux)
    sol.rsq <- rbind(sol.rsq, s1)
    sol.vif <- rbind(sol.vif, vif.vals)
    if (trace) {
      cat("Iteration", i, "out of ", nboot, "\n")
    }
    i = i + 1
  }
  
  pval_vif <- NULL
  for (j in 2:ncol(sol.rsq)) {
    pval_vif <- c(pval_vif, sum(sol.rsq[, j] > valor_vif)/nboot)
  }
  names(pval_vif) <- colnames(sol.rsq)[2:ncol(sol.rsq)]
  pval_klein <- NULL
  for (z in 2:ncol(sol.rsq)) {
    pval_klein <- c(pval_klein, sum(sol.rsq[, 1] < sol.rsq[, 
                                                           z])/nboot)
  }
  names(pval_klein) <- colnames(sol.rsq)[2:ncol(sol.rsq)]
  colnames(sol.rsq) <- c("global", paste(names(datos)[-1], 
                                         sep = ""))
  rownames(sol.rsq) <- 1:nrow(sol.rsq)
  
  MTest <- (list(Bvals = sol.rsq, pval_vif = pval_vif, pval_klein = pval_klein,
              vif.tot = vif.global, R.tot = R.aux.global,nsam = nsam))
  
  structure(c(MTest, call = call), class = c("MTest"))
}



print.MTest <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
  navals <- is.na(x$Bvals)
  if(any(navals!=0)) stop("Bvals can't have missing values")
  
  xs <- colnames(x$Bvals)
  nboot <- nrow(x$Bvals)
  nsam <- (x$nsam)
  Rtot <- x$R.tot
  Rest <- colMeans(x$Bvals)
  smc <- cbind(x$pval_vif,x$pval_klein)  
  colnames(smc) <- c("vif p-value","klein p-value")
  
  cat('\n##################################################################')
  cat('\nMTest: a nonparametric test based on bootstrap for detecting multicollinearity\n')
  cat('\nBootstrap samples:', nboot, '\n')
  cat('\nSample size:', nsam, '\n')
  options(digits = digits)
  cat('\nEstimated pvalues:\n')
  
  smc <- format(smc, digits = digits,nsmall = digits)
  print.default(smc, digits = digits, print.gap = 2,
                quote = FALSE)
  
  
  cat('\nObserved Rglobal & Raux:\n')
  print.default(Rtot, digits = digits, print.gap = 2,
                quote = FALSE)
  
  cat('\nBootstrap Rglobal & Raux:\n')
  print.default(Rest, digits = digits, print.gap = 2,
                quote = FALSE)
  
  cat('\nObserved VIF:\n')
  print.default(x$vif.tot, digits = digits, print.gap = 2,
                quote = FALSE)
  
  
  cat('\n##################################################################\n')
  invisible(x)
}


plot.MTest <- function(x,type = 1,plotly = FALSE,...)
{
  if(!inherits(x,"MTest"))       stop("Enter an object obtained from the function MTest\n")
  ind <- values <- NULL
  boot.sol <- x
  boot.sol <- stack(data.frame(boot.sol$Bvals))
  boot.global <- boot.sol[boot.sol["ind"]=="global",]
  var = 
    boot.aux <- boot.sol[boot.sol["ind"]!="global",]

    # ****** ECDF
  g_ecdf_global <- ggplot2::ggplot(boot.global, 
                                   ggplot2::aes(values,color = ind)) + 
    ggplot2::stat_ecdf(geom = "step")
  g_ecdf_aux <- ggplot2::ggplot(boot.aux, 
                                ggplot2::aes(values,color = ind)) + 
    ggplot2::stat_ecdf(geom = "step")
  g_ecdf_sol <- ggplot2::ggplot(boot.sol, 
                                ggplot2::aes(values,color = ind)) + 
    ggplot2::stat_ecdf(geom = "step")
  
  # ******* Density
  g_dens_sol <- ggplot2::ggplot(boot.sol, 
                                ggplot2::aes(x=values,color = ind)) + 
    ggplot2::geom_density()
  
  if(type == 2 & plotly == FALSE)
  {
    print(g_ecdf_sol)
  }
  if(type == 1 & plotly == FALSE)
  {
    print(g_dens_sol)
  }
  if(plotly==TRUE & type == 1)
  {
    g_dens_sol <- plotly::ggplotly(g_dens_sol)
    print(g_dens_sol)
  }
  if(plotly==TRUE & type == 2)
  {
    g_ecdf_sol <- plotly::ggplotly(g_ecdf_sol)
    print(g_ecdf_sol)
  }
}