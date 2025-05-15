#' Function for plotting a symbolic object
#'
#' @author Andres Navarro
#' @param x The symbolic object.
#' @param col A specification for the default plotting color.
#' @param matrix.form A vector of the form c(num.rows,num.columns).
#' @param border A logical value indicating whether border should be plotted.
#' @param size The magnification to be used for each graphic.
#' @param title A logical value indicating whether title should be plotted.
#' @param show.type A logical value indicating whether type should be plotted.
#' @param font.size The font size of graphics.
#' @param hist.angle.x The angle of labels in y axis. Only for histogram plot
#' @param reduce A logical value indicating whether values different from zero should be plotted in modal and set graphics.
#' @param ... Arguments to be passed to methods.
#'
#' @return A plot of the symbolic data table.
#' @keywords Plot Symbolic data table
#' @export
#' @importFrom randomcoloR distinctColorPalette
#' @examples
#' \dontrun{
#' data(oils)
#' plot(oils)
#' plot(oils, border = T, size = 1.3)
#' }
plot.symbolic_tbl <- function(x, col = NA, matrix.form = NA, border = FALSE, size = 1,
                              title = TRUE, show.type = FALSE, font.size = 1, reduce = FALSE, hist.angle.x = 60, ...) {
  if (!("symbolic_tbl" %in% class(x))) { # El tipo de dato es el incorrecto
    stop("The data type is wrong, only sym.data.table are accepted")
  }
  x <- to.v2(x)
  if (any(is.na(col))) { # No se ingresaron colores
    col <- randomcoloR::distinctColorPalette(max(x$sym.var.length))
  } # Cantidad de colores correspondiente a la cantidad maxima de variables

  title <- !(x$N > 1 && x$M == 1) # si filas > 1 y columnas == 1 tenemos que recorrer en una columna y no mostrar titulo


  if (any(!is.na(matrix.form))) { # Si se tiene matrix.form
    if (!is.vector(matrix.form) || length(matrix.form) != 2) {
      stop("Wrong format on matrix.form")
    }
    if (prod(matrix.form) < x$M * x$N) { # El numero de espacios tiene que ser igual o superior al de variables
      stop("Wrong dimensions on matrix.form")
    }
  } else { # Si no hay matriz se crea una, segun la orientacion de la fila
    matrix.form <- c(x$N, x$M)
  }

  size.factor <- ifelse(is.numeric(size), 1.74 * (1 / size), 1.74) # Determina un tamaño por defecto de los graficos (proporcion agregada)

  # Guarda la configuracion de par original
  def.par <- par(no.readonly = T)
  # Cambia la configuracion de los graficos
  graphics::par(mfrow = matrix.form)
  graphics::par(mar = c(0, 0, 1, 0))
  graphics::par(pin = (par()$din / (rep(max(matrix.form), 2) * size.factor)))
  # par(cex.axis = 0.7 * font.size)
  graphics::par(cex = 0.7 * font.size)

  # Grafica las variables
  for (index.row in 1:x$N) {
    for (index.col in 1:x$M) {
      var.data <- x[index.row, index.col]
      switch(var.data$sym.var.types,
        "$I" = sym.interval.plot(var.data, col, border, show.type),
        "$C" = sym.continuos.plot(var.data, col, border, show.type),
        "$H" = sym.hist.plot(var.data, col, border, show.type, hist.angle.x),
        "$M" = sym.modal.plot(var.data, col, border, show.type, reduce),
        "$S" = sym.set.plot(var.data, col, border, show.type, reduce)
      )
    }
  }

  # Pone el titulo
  if (title) {
    graphics::mtext(toupper(x$sym.obj.names), outer = TRUE, cex = 1.5, side = 3)
  }

  tryCatch(expr = {
    graphics::par(def.par) # retorna al estado original
  }, error = function(err) {
    suppressMessages(grDevices::dev.off())
    message("The size of the device is too small or the \"size\" parameter needs to be adjusted.")
  }, warning = function(war) {
    message(paste0("ImputeValues: ", war))
  })
}

#' sym.interval.plot
#' @keywords internal
#' @importFrom graphics rect text box
sym.interval.plot <- function(info, col = c("blue"), border = FALSE, show.type = TRUE) {
  if (info$sym.var.types != "$I") { # El tipo de dato es el incorrecto
    stop("The data type is wrong, only $I are accepted")
  }

  interval <- as.numeric(info$data[1, ]) # sacamos el intervalo
  name <- paste("[", round(interval[1], 2), ",", round(interval[2], 2), "]") # El label que va en el centro del grafico

  # grafica el plano
  plot(interval + c(-0.4, 0.4), c(0, 3.5),
    type = "n", xlab = "", ylab = "", yaxt = "n",
    main = paste(toupper(info$sym.var.names), ifelse(show.type, " (Interval)", ""))
  )
  graphics::rect(interval[1], -1, interval[2], 3.5, col = col) # rectangulo del intervalo
  center <- c(mean(c(interval[1], interval[2])), mean(c(-1, 4))) # encuentra el centro del rectangulo
  graphics::text(center[1], center[2],
    labels = name, # pone el label con el intevalo en el centro
    cex = ifelse(par()$pin[1] <= 1.5, par()$pin[1], 1.5)
  ) # decide ele tamaño del label
  if (border) { # se pone el borde en negro
    graphics::box("figure", col = "black")
  }
}

#' sym.continuos.plot
#' @keywords internal
sym.continuos.plot <- function(info, col = c("blue"), border = FALSE, show.type = TRUE) {
  if (info$sym.var.types != "$C") { # El tipo de dato es el incorrecto
    stop("The data type is wrong, only $C are accepted")
  }

  continuos <- as.numeric(info$data) # obtiene el valor continuo

  # grafica el plano
  plot(continuos + c(-0.5, 0.5), c(0, 4.1), type = "n", xlab = "", ylab = "", main = paste(info$sym.var.names, ifelse(show.type, " (Continuos)", "")), yaxt = "n")
  graphics::abline(v = continuos, col = col, lty = 2, lwd = 2) # agrega la linea vertical con el valor continuo
  graphics::text(continuos, 2, labels = as.character(round(continuos, 2)), cex = ifelse(par()$pin[1] <= 1.5, par()$pin[1], 1.5)) # agrega el label con el valor continuo en mitad del plano
  if (border) { # se pone el borde en negro
    graphics::box("figure", col = "black")
  }
}

#' sym.hist.plot
#' @keywords internal
#' @importFrom graphics box axis text par
sym.hist.plot <- function(info, col = c("blue"), border = FALSE, show.type = TRUE, angle = 60) {
  if (info$sym.var.types != "$H") { # El tipo de dato es el incorrecto
    stop("The data type is wrong, only $H are accepted")
  }
  dataset <- as.matrix(info$data) # obtenemos los datos

  plt <- barplot(dataset,
    ylim = c(0, 1), names.arg = colnames(dataset), col = col,
    yaxt = "n", xaxt = "n", xlab = "", ylab = "",
    main = paste(info$sym.var.names, ifelse(show.type, " (Histogram)", ""))
  )
  graphics::axis(2, at = seq(0, 1, 0.2), labels = sprintf(round(seq(0, 100, 20)), fmt = "%2.f%%"), las = 1)
  graphics::text(plt, graphics::par("usr")[3], labels = colnames(dataset), srt = angle, adj = 1.1, xpd = T)

  if (border) { # se pone el borde en negro
    graphics::box("figure", col = "black")
  }
}

#' sym.modal.plot
#' @keywords internal
#' @importFrom graphics barplot axis box
sym.modal.plot <- function(info, col = c("blue"), border = FALSE, show.type = TRUE, reduce = FALSE) {
  if (info$sym.var.types != "$M") { # El tipo de dato es el incorrecto
    stop("The data type is wrong, only $M are accepted")
  }

  mt <- info$data # obenemos los datos
  names <- colnames(info$data) # obtenemos los nombres

  if (reduce) { # Si el modo reduce esta activado
    if (any(mt == 0)) { # Si alguna de las columnas tiene cero
      mt <- cbind(mt[, select <- colSums(mt) != 0], 0) # Se crea "select" (las columnas con valores mayores a cero),
      # se seleccionan los valores mayores a cero y
      # se les agraga una columna extra en cero(representativa de los valores en cero)
      names <- c(names[select], "...") # Se seleccionan los nombres de columnas con valores distintos de cero y
      # se crea el nombre de la columna representativa
      col <- col[select] # selecciona los colores corespondientes
    }
  }
  mt <- as.matrix(mt) # obligatorio

  # graficamos las barras
  graphics::barplot(mt,
    main = paste(info$sym.var.names, ifelse(show.type, " (Modal)", "")), xlab = "", ylab = "", yaxt = "n",
    col = col, beside = TRUE, names.arg = names, cex.names = .8, space = c(0, 0.05)
  )
  graphics::axis(2, at = seq(0, 1, 0.2), labels = sprintf(round(seq(0, 100, 20)), fmt = "%2.f%%"), las = 1) # los y labels con %

  if (border) { # se pone el borde en negro
    graphics::box("figure", col = "black")
  }
}

#' sym.set.plot
#' @keywords internal
#' @importFrom graphics barplot axis
sym.set.plot <- function(info, col = c("blue"), border = FALSE, show.type = TRUE, reduce = FALSE) {
  if (info$sym.var.types != "$S") { # El tipo de dato es el incorrecto
    stop("The data type is wrong, only $S are accepted")
  }

  mt <- info$data # obenemos los datos
  mt[, mt > 0] <- 1 / sum(mt > 0) # arreglamos la escala de los set.
  names <- colnames(info$data) # obtenemos los nombres

  if (reduce) { # Si el modo reduce esta activado
    if (any(mt == 0)) { # Si alguna de las columnas tiene cero
      mt <- cbind(mt[, select <- colSums(mt) != 0], 0) # Se crea "select" (las columnas con valores mayores a cero),
      # se seleccionan los valores mayores a cero y
      # se les agraga una columna extra en cero(representativa de los valores en cero)
      names <- c(names[select], "...") # Se seleccionan los nombres de columnas con valores distintos de cero y
      # se crea el nombre de la columna representativa
      col <- col[select] # selecciona los colores corespondientes
    }
  }

  mt <- as.matrix(mt) # obligatorio

  # graficamos las barras
  graphics::barplot(mt,
    main = paste(info$sym.var.names, ifelse(show.type, " (Set)", "")), xlab = "", ylab = "", yaxt = "n",
    names.arg = names, ylim = c(0, ifelse(max(mt) < 0.5, 0.5, 1)), beside = TRUE, col = col, cex.names = .8
  )
  graphics::axis(2, at = seq(0, 1, 0.2), labels = sprintf(round(seq(0, 100, 20)), fmt = "%2.f%%"), las = 1) # los y labels con %

  if (border) { # se pone el borde en negro
    graphics::box("figure", col = "black")
  }
}
