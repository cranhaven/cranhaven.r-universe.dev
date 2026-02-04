# Funciones tomadas del paquete htmlwidgets

#' Eval character vectors to JS code
#'
#' @param ... character vectors to evaluate
#'
#' @author Joseline Quiros <joseline.quiros@promidat.com>
#' @export e_JS
#' @examples
#' e_JS('5 * 3')
#' 
e_JS <- function (...) 
{
  vec <- c(...)
  vec <- paste(vec, collapse = "\n")
  structure(vec, class = unique(c("JS_EVAL", oldClass(vec))))
}

#' Convierte toda la tabla a código dummy.
#'
#' @param DF a data.frame.
#' @param exclude variables of data.frame exclude of conversion.
#'
#' @author Diego Jimenez <diego.jimenezs@promidat.com>
#' @export data.frame.dummy
#' @examples
#' data.frame.dummy(iris)
#' 
data.frame.dummy <- function (DF, exclude = NULL) {
  res <- data.frame(matrix(NA, nrow = nrow(DF)))
  
  for (x in names(DF)) {
    if(x %in% exclude) {
      res[[x]] <- DF[[x]]
    } else if(is.numeric(DF[[x]])) {
      res[[x]] <- DF[[x]]
    } else {
      categorias <- levels(DF[[x]])
      if(length(categorias) > 1) {
        categorias <- categorias[-1]
      }
      for (categoria in categorias) {
        nueva.var <- as.numeric(DF[[x]] == categoria)
        res[[paste0(x, ".", categoria)]] <- nueva.var
      }
    }
  }
  
  return(res[, -1])
}

# Validacion comun para todos los modelos
validar.datos <- function(print = TRUE,variable.predecir,datos.aprendizaje) {
  # Validaciones
  if (is.null(variable.predecir) & print) {
    showNotification(tr("tieneVP"), duration = 10, type = "error")
  }
  if (is.null(datos.aprendizaje) & print) {
    showNotification(tr("tieneDAP"), duration = 10, type = "error")
  }
  return(!is.null(variable.predecir) & !is.null(datos.aprendizaje))
}

# Crea la tabla de comparación entre predicción y datos reales (datos de prueba)
obj.predic <- function(prediction = NULL, idioma, test, var.pred){
  real <- test[, var.pred]
  if(is.numeric(prediction)) {
    prediction <- factor(prediction, labels = levels(real))
  }
  real   <- as.character(real)
  predi  <- as.character(prediction)
  acerto <- paste0("<span style='color:green'><b>",tr("acerto",idioma),"</b></span>")
  fallo  <- paste0("<span style='color:red'><b>",tr("fallo",idioma),"</b></span>")
  df     <- cbind(real, predi, ifelse(real == predi,
                                  rep(acerto, length(real)),
                                  rep(fallo, length(real)) ))
  colns  <- c(tr("reald", idioma), tr("pred", idioma), " ")
  colnames(df) <- colns
  sketch <- htmltools::withTags(table(tableHeader(colns)))
  return(DT::datatable(df,
                       selection = "none",
                       editable = FALSE,
                       escape = FALSE,
                       container = sketch,
                       options = list(dom = "frtip", pageLength = 10)))
}

#' Cierra un menú según su tabName
#' @noRd
close.menu <- function(tabname = NA, valor = T) {
  select <- paste0("a[href^='#shiny-tab-", tabname, "']")
  if(valor){
    shinyjs::hide(selector = "ul.menu-open")
    shinyjs::disable(selector = select)
  } else {
    shinyjs::enable(selector = select)
  }
}

# Hace el gráfico de la matriz de confusión
plot_MC_code <- function(cm, idioma) {
  return(paste0("
plot.MC <<- function(cm,idioma) {
  par(mar = c(2, 2, 2, 2))
  plot(c(1, 600), c(-100, 500), type = 'n', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
  title('",tr("mc",idioma),"', cex.main = 2)

  start <- 80
  len <- 500 - start

  n.class <- ncol(cm)
  names.class <- colnames(cm)
  prec.cat <- diag(cm) / rowSums(cm)
  error.cat <- 1 - prec.cat

  ancho <- len / n.class
  alto <- len / (n.class)
  x2 <- (x1 <- start) + ancho
  y2 <- (y1 <- len) - alto

  text(310, 485, '",tr("pred",idioma),"', cex = 1.3, font = 2)
  text(start - 55, 250, 'Real', cex = 1.3, srt = 90, font = 2)

  for (i in 0:(n.class - 1)) {
    for (j in 0:(n.class - 1)) {
      x1.aux <- x1 + j * (ancho + 3)
      y1.aux <- y1 - i * (alto + 5)
      x2.aux <- x2 + j * (ancho + 3)
      y2.aux <- y2 - i * (alto + 5)
      if (j < (n.class)) {
        rect(x1.aux, y1.aux, x2.aux, y2.aux, col = ifelse(i == j, '#3f72af', '#11999e'))
        text(mean(c(x1.aux, x2.aux)),
          mean(c(y1.aux, y2.aux)),
          paste0(cm[(i + 1), (j + 1)], ' (', round(cm[(i + 1), (j + 1)] / sum(cm[(i + 1), ]), 2) * 100, '%)'),
          cex = 1.1, font = 2, col = 'white')
      }
    }
    text(mean(c((x2 + i * (ancho + 3)), (x1 + i * (ancho + 3)))), y1 + 20, names.class[i + 1], cex = 1)
    text(x1 - 20, mean(c((y1 - i * (alto + 5)), (y2 - i * (alto + 5)))), names.class[i + 1], cex = 1)
  }
  text(mean(c((x2 + (i + 1) * (ancho + 3)), (x1 + (i + 1) * (ancho + 3)))), y1 + 20, names.class[i + 2], cex = 1.2)
  text(mean(c((x2 + (i + 2) * (ancho + 3)), (x1 + (i + 2) * (ancho + 3)))), y1 + 20, names.class[i + 3], cex = 1.2)
  text(mean(c((x2 + (i + 3) * (ancho + 3)), (x1 + (i + 3) * (ancho + 3)))), y1 + 20, names.class[i + 4], cex = 1.2)
}"))
}

#Código del calculo de los indices
indices.generales <- function(MC) {
  if(1 == dim(MC)[2]) {
    MC <- cbind(MC, 0)
  }
  precision.global <- (sum(diag(MC)) / sum(MC)) * 100
  error.global     <- (1 - (sum(diag(MC)) / sum(MC))) * 100
  precision.clase  <- diag(MC)/rowSums(MC) * 100
  error.clase      <- 100 - precision.clase
  return(list(precision.global = precision.global,
              error.global     = error.global,
              precision.clase  = precision.clase,
              error.clase      = error.clase))
}

#Crea la tabla de errores que se grafica en los indices de todos los modelos
indices.error.table <- function(indices, nombre = ""){
  err            <- rbind(indices[[4]])
  colnames(err)  <- paste0(c("Error."), colnames(err))
  row.names(err) <- nombre
  return(err)
}

#Crea la tabla de precisiones que se grafica en los indices de todos los modelos
indices.prec.table <- function(indices, nombre = "", idioma){
  prec            <- rbind(indices[[3]])
  colnames(prec)  <- paste0(tr("prec", idioma),".",colnames(prec))
  row.names(prec) <- nombre
  return(prec)
}

#' Gauge Plot
#' 
#' @param value a number specifying the value of the graph.
#' @param label a character specifying the title to use on legend.
#' @param color1 a color for the gauge.
#' @param color2 a shadowColor for the gauge.
#' 
#' @author Joseline Quiros <joseline.quiros@promidat.com>
#' @return echarts4r plot
#' @export e_global_gauge
#' @import echarts4r
#' @examples
#' e_global_gauge(87, "Global Precision")
#'
e_global_gauge<- function(value = 100, label = "Label", color1 = "#B5E391", color2 = "#90C468"){
  e_charts() |>  
    e_gauge(value, "",
            itemStyle = list(color         = color1,  
                             shadowColor   = color2,
                             shadowBlur    = 10,
                             shadowOffsetX = 2,
                             shadowOffsetY = 2),
            progress = list(
              show   = TRUE,
              width  = 30
            ),
            startAngle = 180,
            endAngle   = 0
            ,
            pointer = list(
              show  = FALSE
            ),
            axisLine    = list(
              lineStyle = list(
                width   = 30
              )
            ),
            axisTick = list(
              distance    = -39,
              splitNumber = 5,
              lineStyle   = list(
                color     = "#999",
                width     = 1
              )
            ),
            splitLine  = list(
              distance = -42,
              length   = 10,
              lineStyle= list(
                color = "#999",
                width = 2
              )
            ),
            axisLabel  = list(
              distance = 0,
              color    = "#999",
              fontSize = 10
            ),
            detail = list(
              borderRadius = 8,
              fontSize     = 15,
              offsetCenter = c(0, '-20%'),
              formatter    =   paste0('{value}%\n',label),
              color        = "#54564D"
            )
    ) 
}

# Concatena y ejecuta un string como código
exe <- function(...){
  eval(parse(text = paste0(...)))
}

#Extrae el código de una función
extract.code <- function(funcion) {
  code <- paste(head(exe(funcion), 100), collapse = "\n")
  code <- paste(funcion, "<-", code)
  return(code)
}

#
as.string.c <- function(vect, .numeric = FALSE){
  if(.numeric){
    return(paste0("c(",paste0(vect, collapse = ","),")"))
  }
  else{
    return(paste0("c('",paste0(vect, collapse = "','"),"')"))
  }
}

#Funciones tomadas del paquete rpart
rpart.control <- function (minsplit = 20L, minbucket = round(minsplit/3), cp = 0.01, 
          maxcompete = 4L, maxsurrogate = 5L, usesurrogate = 2L, xval = 10L, 
          surrogatestyle = 0L, maxdepth = 30L, ...) 
{
  if (maxcompete < 0L) {
    warning("The value of 'maxcompete' supplied is < 0; the value 0 was used instead")
    maxcompete <- 0L
  }
  if (any(xval < 0L)) {
    warning("The value of 'xval' supplied is < 0; the value 0 was used instead")
    xval <- 0L
  }
  if (maxdepth > 30L) 
    stop("Maximum depth is 30")
  if (maxdepth < 1L) 
    stop("Maximum depth must be at least 1")
  if (missing(minsplit) && !missing(minbucket)) 
    minsplit <- minbucket * 3L
  if ((usesurrogate < 0L) || (usesurrogate > 2L)) {
    warning("The value of 'usesurrogate' supplied was out of range, the default value of 2 is used instead.")
    usesurrogate <- 2L
  }
  if ((surrogatestyle < 0L) || (surrogatestyle > 1L)) {
    warning("The value of 'surrogatestyle' supplied was out of range, the default value of 0 is used instead.")
    surrogatestyle <- 0L
  }
  list(minsplit = minsplit, minbucket = minbucket, cp = cp, 
       maxcompete = maxcompete, maxsurrogate = maxsurrogate, 
       usesurrogate = usesurrogate, surrogatestyle = surrogatestyle, 
       maxdepth = maxdepth, xval = xval)
}

#' Funciones tomadas del paquete PSYCH
#' @noRd
pairs.panels <- function (x, smooth = TRUE, scale = FALSE, density = TRUE, ellipses = TRUE,
                          digits = 2, method = "pearson", pch = 20, lm = FALSE, cor = TRUE,
                          jiggle = FALSE, factor = 2, hist.col = "cyan", show.points = TRUE,
                          rug = TRUE, breaks = "Sturges", cex.cor = 1, wt = NULL, smoother = FALSE,
                          stars = FALSE, ci = FALSE, alpha = 0.05, ...){
  "panel.hist.density" <- function(x, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1], usr[2], 0, 1.5))
    tax <- table(x)
    if (length(tax) < 11) {
      breaks <- as.numeric(names(tax))
      y <- tax/max(tax)
      interbreak <- min(diff(breaks)) * (length(tax) -
                                           1)/41
      rect(breaks - interbreak, 0, breaks + interbreak,
           y, col = hist.col)
    }
    else {
      h      <- hist(x, breaks = breaks, plot = FALSE)
      breaks <- h$breaks
      nB <- length(breaks)
      y  <- h$counts
      y  <- y/max(y)
      rect(breaks[-nB], 0, breaks[-1], y, col = hist.col)
    }
    if (density) {
      tryd <- try(d <- density(x, na.rm = TRUE, bw = "nrd",
                               adjust = 1.2), silent = TRUE)
      if (!inherits(tryd, "try-error")) {
        d$y <- d$y/max(d$y)
        lines(d)
      }
    }
    if (rug)
      rug(x)
  }
  "panel.cor" <- function(x, y, prefix = "", ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    if (is.null(wt)) {
      r <- cor(x, y, use = "pairwise", method = method)
    }
    else {
      r <- cor.wt(data.frame(x, y), w = wt[, c(1:2)])$r[1,
                                                        2]
    }
    txt <- format(c(round(r, digits), 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (stars) {
      pval <- r.test(sum(!is.na(x * y)), r)$p
      symp <- symnum(pval, corr = FALSE, cutpoints = c(0,
                                                       0.001, 0.01, 0.05, 1), symbols = c("***", "**",
                                                                                          "*", " "), legend = FALSE)
      txt <- paste0(txt, symp)
    }
    cex <- cex.cor * 0.8/(max(strwidth("0.12***"), strwidth(txt)))
    if (scale) {
      cex1 <- cex * abs(r)
      if (cex1 < 0.25)
        cex1 <- 0.25
      text(0.5, 0.5, txt, cex = cex1)
    }
    else {
      text(0.5, 0.5, txt, cex = cex)
    }
  }
  "panel.smoother" <- function(x, y, pch = par("pch"), col.smooth = "red",
                               span = 2/3, iter = 3, ...) {
    xm <- mean(x, na.rm = TRUE)
    ym <- mean(y, na.rm = TRUE)
    xs <- sd(x, na.rm = TRUE)
    ys <- sd(y, na.rm = TRUE)
    r = cor(x, y, use = "pairwise", method = method)
    if (jiggle) {
      x <- jitter(x, factor = factor)
      y <- jitter(y, factor = factor)
    }
    if (smoother) {
      smoothScatter(x, y, add = TRUE, nrpoints = 0)
    }
    else {
      if (show.points)
        points(x, y, pch = pch, ...)
    }
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) {
      if (smooth & ci) {
        lml   <- loess(y ~ x, degree = 1, family = "symmetric")
        tempx <- data.frame(x = seq(min(x, na.rm = TRUE),
                                    max(x, na.rm = TRUE), length.out = 47))
        pred  <- predict(lml, newdata = tempx, se = TRUE)
        if (ci) {
          upperci <- pred$fit + confid * pred$se.fit
          lowerci <- pred$fit - confid * pred$se.fit
          polygon(c(tempx$x, rev(tempx$x)), c(lowerci,
                                              rev(upperci)), col = adjustcolor("light grey",
                                                                               alpha.f = 0.8), border = NA)
        }
        lines(tempx$x, pred$fit, col = col.smooth, ...)
      }
      else {
        if (smooth)
          lines(stats::lowess(x[ok], y[ok], f = span,
                              iter = iter), col = col.smooth)
      }
    }
    if (ellipses)
      draw.ellipse(xm, ym, xs, ys, r, col.smooth = col.smooth,
                   ...)
  }
  "panel.lm" <- function(x, y, pch = par("pch"), col.lm = "red",
                         ...) {
    ymin <- min(y)
    ymax <- max(y)
    xmin <- min(x)
    xmax <- max(x)
    ylim <- c(min(ymin, xmin), max(ymax, xmax))
    xlim <- ylim
    if (jiggle) {
      x <- jitter(x, factor = factor)
      y <- jitter(y, factor = factor)
    }
    if (smoother) {
      smoothScatter(x, y, add = TRUE, nrpoints = 0)
    }
    else {
      if (show.points) {
        points(x, y, pch = pch, ylim = ylim, xlim = xlim,
               ...)
      }
    }
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) {
      lml <- lm(y ~ x)
      if (ci) {
        tempx <- data.frame(x = seq(min(x, na.rm = TRUE),
                                    max(x, na.rm = TRUE), length.out = 47))
        pred    <- predict.lm(lml, newdata = tempx, se.fit = TRUE)
        upperci <- pred$fit + confid * pred$se.fit
        lowerci <- pred$fit - confid * pred$se.fit
        polygon(c(tempx$x, rev(tempx$x)), c(lowerci,
                                            rev(upperci)), col = adjustcolor("light grey",
                                                                             alpha.f = 0.8), border = NA)
      }
      if (ellipses) {
        xm <- mean(x, na.rm = TRUE)
        ym <- mean(y, na.rm = TRUE)
        xs <- sd(x, na.rm = TRUE)
        ys <- sd(y, na.rm = TRUE)
        r = cor(x, y, use = "pairwise", method = method)
        draw.ellipse(xm, ym, xs, ys, r, col.smooth = col.lm,
                     ...)
      }
      abline(lml, col = col.lm, ...)
    }
  }
  "draw.ellipse" <- function(x = 0, y = 0, xs = 1, ys = 1,
                             r = 0, col.smooth, add = TRUE, segments = 51, ...) {
    angles      <- (0:segments) * 2 * pi/segments
    unit.circle <- cbind(cos(angles), sin(angles))
    if (!is.na(r)) {
      if (abs(r) > 0)
        theta <- sign(r)/sqrt(2)
      else theta = 1/sqrt(2)
      shape <- diag(c(sqrt(1 + r), sqrt(1 - r))) %*% matrix(c(theta,
                                                              theta, -theta, theta), ncol = 2, byrow = TRUE)
      ellipse <- unit.circle %*% shape
      ellipse[, 1] <- ellipse[, 1] * xs + x
      ellipse[, 2] <- ellipse[, 2] * ys + y
      if (show.points)
        points(x, y, pch = 19, col = col.smooth, cex = 1.5)
      lines(ellipse, ...)
    }
  }
  "panel.ellipse" <- function(x, y, pch = par("pch"), col.smooth = "red",
                              ...) {
    segments = 51
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1] - abs(0.05 * usr[1]), usr[2] + abs(0.05 *
                                                            usr[2]), 0, 1.5))
    xm <- mean(x, na.rm = TRUE)
    ym <- mean(y, na.rm = TRUE)
    xs <- sd(x, na.rm = TRUE)
    ys <- sd(y, na.rm = TRUE)
    r = cor(x, y, use = "pairwise", method = method)
    if (jiggle) {
      x <- jitter(x, factor = factor)
      y <- jitter(y, factor = factor)
    }
    if (smoother) {
      smoothScatter(x, y, add = TRUE, nrpoints = 0)
    }
    else {
      if (show.points) {
        points(x, y, pch = pch, ...)
      }
    }
    angles <- (0:segments) * 2 * pi/segments
    unit.circle <- cbind(cos(angles), sin(angles))
    if (!is.na(r)) {
      if (abs(r) > 0)
        theta <- sign(r)/sqrt(2)
      else theta = 1/sqrt(2)
      shape <- diag(c(sqrt(1 + r), sqrt(1 - r))) %*% matrix(c(theta,
                                                              theta, -theta, theta), ncol = 2, byrow = TRUE)
      ellipse <- unit.circle %*% shape
      ellipse[, 1] <- ellipse[, 1] * xs + xm
      ellipse[, 2] <- ellipse[, 2] * ys + ym
      points(xm, ym, pch = 19, col = col.smooth, cex = 1.5)
      if (ellipses)
        lines(ellipse, ...)
    }
  }
  
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  if (missing(cex.cor))
    cex.cor <- 1
  for (i in 1:ncol(x)) {
    if (is.character(x[[i]])) {
      x[[i]] <- as.numeric(as.factor(x[[i]]))
      colnames(x)[i] <- paste(colnames(x)[i], "*", sep = "")
    }
  }
  n.obs <- nrow(x)
  confid <- qt(1 - alpha/2, n.obs - 2)
  if (!lm) {
    if (cor) {
      pairs(x, diag.panel = panel.hist.density, upper.panel = panel.cor,
            lower.panel = panel.smoother, pch = pch, ...)
    }
    else {
      pairs(x, diag.panel = panel.hist.density, upper.panel = panel.smoother,
            lower.panel = panel.smoother, pch = pch, ...)
    }
  }
  else {
    if (!cor) {
      pairs(x, diag.panel = panel.hist.density, upper.panel = panel.lm,
            lower.panel = panel.lm, pch = pch, ...)
    }
    else {
      pairs(x, diag.panel = panel.hist.density, upper.panel = panel.cor,
            lower.panel = panel.lm, pch = pch, ...)
    }
  }
}

r.test <- function (n, r12, r34 = NULL, r23 = NULL, r13 = NULL, r14 = NULL, 
          r24 = NULL, n2 = NULL, pooled = TRUE, twotailed = TRUE) 
{
  cl <- match.call()
  if (is.null(r34) & is.null(r13) & is.null(r23)) {
    t <- r12 * sqrt(n - 2)/sqrt(1 - r12^2)
    p <- 1 - pt(abs(t), n - 2)
    if (twotailed) 
      p <- 2 * p
    ci <- r.con(r12, n)
    result <- list(Call = cl, Test = "Test of significance of a  correlation", 
                   t = t, p = p, ci = ci)
  }
  else {
    if (is.null(r23)) {
      if (is.null(r34)) {
        stop("You seem to be testing two dependent correlations, but have not specified the other correlation(s)  correctly.")
      }
      if (!is.null(r13)) {
        stop("You seem to be testing two dependent correlations, but have not specified the correlation(s)  correctly.")
      }
      xy.z <- 0.5 * log((1 + r12)/(1 - r12))
      xz.z <- 0.5 * log((1 + r34)/(1 - r34))
      if (is.null(n2)) 
        n2 <- n
      se.diff.r <- sqrt(1/(n - 3) + 1/(n2 - 3))
      diff <- xy.z - xz.z
      z <- abs(diff/se.diff.r)
      p <- (1 - pnorm(z))
      if (twotailed) 
        p <- 2 * p
      result <- list(Call = cl, Test = "Test of difference between two independent correlations", 
                     z = z, p = p)
    }
    else {
      if (is.null(r14)) {
        if (!is.null(r34)) {
          if (is.null(r13)) {
            r13 <- r34
          }
        }
        if (is.null(r13)) {
          stop("You seem to be trying to test two dependent correlations, but have not specified the other correlation(s)")
        }
        diff <- r12 - r13
        determin = 1 - r12 * r12 - r23 * r23 - r13 * 
          r13 + 2 * r12 * r23 * r13
        av = (r12 + r13)/2
        cube = (1 - r23) * (1 - r23) * (1 - r23)
        t2 = diff * sqrt((n - 1) * (1 + r23)/(((2 * (n - 
                                                       1)/(n - 3)) * determin + av * av * cube)))
        p <- pt(abs(t2), n - 3, lower.tail = FALSE)
        if (twotailed) 
          p <- 2 * p
        cl <- paste("r.test(n = ", n, ",  r12 = ", r12, 
                    ",  r23 = ", r23, ",  r13 = ", r13, ")")
        result <- list(Call = cl, Test = "Test of difference between two correlated  correlations", 
                       t = t2, p = p)
      }
      else {
        z12 <- fisherz(r12)
        z34 <- fisherz(r34)
        pooledr <- (r12 + r34)/2
        if (pooled) {
          r1234 = 1/2 * ((r13 - pooledr * r23) * (r24 - 
                                                    r23 * pooledr) + (r14 - r13 * pooledr) * 
                           (r23 - pooledr * r13) + (r13 - r14 * pooledr) * 
                           (r24 - pooledr * r14) + (r14 - pooledr * 
                                                      r24) * (r23 - r24 * pooledr))
          z1234 <- r1234/((1 - pooledr^2) * (1 - pooledr^2))
        }
        else {
          r1234 = 1/2 * ((r13 - r12 * r23) * (r24 - r23 * 
                                                r34) + (r14 - r13 * r34) * (r23 - r12 * r13) + 
                           (r13 - r14 * r34) * (r24 - r12 * r14) + (r14 - 
                                                                      r12 * r24) * (r23 - r24 * r34))
          z1234 <- r1234/((1 - r12^2) * (1 - r34^2))
        }
        ztest <- (z12 - z34) * sqrt(n - 3)/sqrt(2 * (1 - 
                                                       z1234))
        z <- ztest
        p <- (1 - pnorm(abs(z)))
        if (twotailed) 
          p <- 2 * p
        result <- list(Call = cl, Test = "Test of difference between two dependent correlations", 
                       z = z, p = p)
      }
    }
  }
  class(result) <- c("psych", "r.test")
  return(result)
}
cor.wt <- function (data, vars = NULL, w = NULL, sds = NULL, cor = TRUE) 
{
  cl <- match.call()
  if (is.list(data) && !is.data.frame(data)) {
    w <- data$n
    sds <- data$sd
    x <- data$mean
  }
  else {
    x <- data
  }
  if (!is.null(vars)) {
    x <- x[, vars]
    w <- w[, vars]
    sds <- sds[, vars]
  }
  if (is.null(w)) 
    w <- matrix(rep(rep(1/nrow(x), nrow(x)), ncol(x)), nrow = nrow(x), 
                ncol = ncol(x))
  if (is.null(ncol(w))) {
    wt <- w/sum(w)
  }
  else {
    wt <- t(t(w)/colSums(w))
  }
  cnames <- colnames(x)
  for (i in 1:ncol(x)) {
    if (is.factor(x[, i]) || is.logical(x[, i])) {
      x[, i] <- as.numeric(x[, i])
      colnames(x)[i] <- paste(cnames[i], "*", sep = "")
    }
  }
  means <- colSums(x * wt, na.rm = TRUE)
  xc <- scale(x, center = means, scale = FALSE)
  if (is.null(sds)) {
    xs <- xc/sqrt(w)
  }
  else {
    xs <- xc * sds/sqrt(w)
  }
  xwt <- sqrt(wt) * xc
  if (any(is.na(xwt))) {
    cov <- apply(xwt, 2, function(x) colSums(xwt * x, na.rm = TRUE))
  }
  else {
    cov <- crossprod(xwt)
  }
  if (cor) {
    r <- cov2cor(cov)
  }
  else {
    r <- cov
  }
  xw <- wt * xc
  result <- list(r = r, xwt = xwt, wt = wt, mean = means, xc = xc, 
                 xs = xs)
  result$Call <- cl
  class(result) <- c("psych", "cor.wt")
  return(result)
}

r.con <- function (rho, n, p = 0.95, twotailed = TRUE) 
{
  z <- fisherz(rho)
  if (n < 4) {
    stop("number of subjects must be greater than 3")
  }
  se <- 1/sqrt(n - 3)
  p <- 1 - p
  if (twotailed) 
    p <- p/2
  dif <- qnorm(p)
  zlow <- z + dif * se
  zhigh <- z - dif * se
  ci <- c(zlow, zhigh)
  ci <- fisherz2r(ci)
  return(ci)
}

fisherz2r <- function (z) 
{
  (exp(2 * z) - 1)/(1 + exp(2 * z))
}

fisherz <- function (rho) 
{
  0.5 * log((1 + rho)/(1 - rho))
}

#Colores de ggplot2
gg_color_hue <- function(n) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# Funciones tomadas del paquete kknn

#' Returns a matrix of contrasts for the train.kknn.
#'
#' @param n A vector containing levels of a factor, or the number of levels.
#' @param contrasts A logical value indicating whether contrasts should be computed.
#'
#' @author Joseline Quiros <joseline.quiros@promidat.com>
#' @export contr.dummy
#' @examples
#' contr.dummy(5)
#' 
contr.dummy <- function (n, contrasts = TRUE) 
{
  if (length(n) <= 1) {
    if (is.numeric(n) && length(n) == 1 && n > 1) 
      levels <- 1:n
    else stop("contrasts are not defined for 0 degrees of freedom")
  }
  else levels <- n
  lenglev <- length(levels)
  cont <- array(0, c(lenglev, lenglev), list(levels, levels))
  cont[col(cont) == row(cont)] <- 1
  cont
}

#' Returns a matrix of contrasts for the train.kknn.
#'
#' @param n A vector containing levels of a factor, or the number of levels.
#' @param contrasts A logical value indicating whether contrasts should be computed.
#'
#' @author Joseline Quiros <joseline.quiros@promidat.com>
#' @export contr.metric
#' @examples
#' contr.metric(5)
#' 
contr.metric <- function (n, contrasts = TRUE) 
{
  if (length(n) <= 1) {
    if (is.numeric(n) && length(n) == 1 && n > 1) 
      levels <- 1:n
    else stop("contrasts are not defined for 0 degrees of freedom")
  }
  else levels <- n
  lenglev <- length(levels)
  cont <- array((1:lenglev) - (1 + lenglev)/2, c(lenglev, 1), 
                list(levels, NULL))
  cont
}

#' Returns a matrix of contrasts for the train.kknn.
#'
#' @param n A vector containing levels of a factor, or the number of levels.
#' @param contrasts A logical value indicating whether contrasts should be computed.
#'
#' @author Joseline Quiros <joseline.quiros@promidat.com>
#' @export contr.ordinal
#' @examples
#' contr.ordinal(5)
#' 
contr.ordinal <- function (n, contrasts = TRUE) 
{
  if (length(n) <= 1) {
    if (is.numeric(n) && length(n) == 1 && n > 1) 
      levels <- 1:n
    else stop("contrasts are not defined for 0 degrees of freedom")
  }
  else levels <- n
  lenglev <- length(levels)
  cont <- array(0.5, c(lenglev, lenglev - 1), list(levels, 
                                                   NULL))
  cont[lower.tri(cont)] <- -0.5
  cont
}