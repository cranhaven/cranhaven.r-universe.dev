#' Display information about an equation
#'
#' @param eq The name of the equation.
#' @return Nothing.
#' @examples
#' devRateInfo(eq = davidson_44)
#' devRateInfo(eq = campbell_74)
#' devRateInfo(eq = taylor_81)
#' devRateInfo(eq = wang_82)
#' @export
devRateInfo <- function(eq){
  cat("----------------------------------------\n")
  cat(as.character(eq$name), "\n----------------------------------------\n")
  cat(strwrap(x = eq$ref, width = 80), sep = "\n")
  cat("\n")
  print(eq$eq)
  if(nrow(eq$startVal) > 0){
    cat("\nParameter estimates from the literature (eq$startVal): \n\n")
    print(eq$startVal)
  }
  if(length(eq$com) > 0){cat("\nComments: ", strwrap(x = eq$com, width = 80), sep = "\n")}
}


#' Plot thermal performance curves from the literature
#'
#' @param eq The name of the equation.
#' @param sortBy The filter to separate species ("ordersp", "familysp", "genussp", "species", "genSp").
#' @param stage The life stage of the organism ("all", "eggs", "L1", "L2", "L3", "L4", "L5",
#'   "larva", "pupa", "prepupa", "female", "male", ...)
#' @param ... Aditional arguments for the plot.
#' @return Nothing.
#' @examples
#' devRatePlotInfo(eq = davidson_44, sortBy = "genSp", xlim = c(0, 40), ylim = c(0, 0.05))
#' devRatePlotInfo(eq = campbell_74, sortBy = "familysp", xlim = c(-10, 30), ylim = c(0, 0.05))
#' devRatePlotInfo(eq = taylor_81, sortBy = "ordersp", xlim = c(-20, 60), ylim = c(0, 0.2))
#' devRatePlotInfo(eq = wang_82, sortBy = "ordersp", xlim = c(0, 50), ylim = c(0, 0.06))
#' devRatePlotInfo(eq = stinner_74, sortBy = "ordersp", xlim = c(0, 50), ylim = c(0, 0.06))
#' @export
devRatePlotInfo <- function(eq, sortBy = "genSp", stage = "all", ...){
  listPlot <- split(
    eq$startVal[(as.character(eq$startVal[,6]) == stage & !is.na(eq$startVal[,7])),],
    as.character(eq$startVal[sortBy][(as.character(eq$startVal[,6]) == stage  & !is.na(eq$startVal[,7])),])
  )
  if(length(listPlot) > 0){
  graphics::plot(0, type = "n", xlab = "Temperature", ylab = "Development rate", ...)
  x <- seq(from = 0, to = 50, length.out = 100)
  for(i in 1:length(listPlot)){
    for(j in 1:nrow(listPlot[[i]])){
      colparam <- 7
        switch(EXPR = eq$id,
               "eq010" = {
                 Dmin <- listPlot[[i]][j, colparam]
                 Topt <- listPlot[[i]][j, colparam + 1]
                 aa <- listPlot[[i]][j, colparam + 2]
                 bb <- listPlot[[i]][j, colparam + 3]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq020" = {
                 aa <- listPlot[[i]][j, colparam]
                 bb <- listPlot[[i]][j, colparam + 1]
                 K <- listPlot[[i]][j, colparam + 2]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq030" = {
                 aa <- listPlot[[i]][j, colparam]
                 bb <- listPlot[[i]][j, colparam + 1]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq040" = {
                 C <- listPlot[[i]][j, colparam]
                 k1 <- listPlot[[i]][j, colparam + 1]
                 k2 <- listPlot[[i]][j, colparam + 2]
                 Topt <- listPlot[[i]][j, colparam + 3]
                 fx <- as.function(alist(x=, eval(parse(text = eq$eqAlt[1]))))
                 graphics::curve(fx, add = TRUE, col = i, from = 0, to = Topt)
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt[2]))))
                 graphics::curve(fx, add = TRUE, col = i, from = Topt, to = 60)
               },
               "eq050" = {
                 phi <- listPlot[[i]][j, colparam]
                 bb <- listPlot[[i]][j, colparam + 1]
                 Tmax <- listPlot[[i]][j, colparam + 2]
                 deltaT <- listPlot[[i]][j, colparam + 3]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq060" = {
                 alpha <- listPlot[[i]][j, colparam]
                 bb <- listPlot[[i]][j, colparam + 1]
                 cc <- listPlot[[i]][j, colparam + 2]
                 Tmax <- listPlot[[i]][j, colparam + 3]
                 deltaT <- listPlot[[i]][j, colparam + 4]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq070" = {
                 aa <- listPlot[[i]][j, colparam]
                 bb <- listPlot[[i]][j, colparam + 1]
                 cc <- listPlot[[i]][j, colparam + 2]
                 dd <- listPlot[[i]][j, colparam + 3]
                 ff <- listPlot[[i]][j, colparam + 4]
                 gg <- listPlot[[i]][j, colparam + 5]
                 deg <- listPlot[[i]][j, colparam + 6]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq080" = {
                 aa <- listPlot[[i]][j, colparam]
                 bb <- listPlot[[i]][j, colparam + 1]
                 cc <- listPlot[[i]][j, colparam + 2]
                 Tmin <- listPlot[[i]][j, colparam + 3]
                 Tmax <- listPlot[[i]][j, colparam + 4]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq090" = {
                 p25 <- listPlot[[i]][j, colparam]
                 aa <- listPlot[[i]][j, colparam + 1]
                 bb <- listPlot[[i]][j, colparam + 2]
                 cc <- listPlot[[i]][j, colparam + 3]
                 dd <- listPlot[[i]][j, colparam + 4]
                 ee <- listPlot[[i]][j, colparam + 5]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq091" = {
                 p25 <- listPlot[[i]][j, colparam]
                 aa <- listPlot[[i]][j, colparam + 1]
                 dd <- listPlot[[i]][j, colparam + 2]
                 ee <- listPlot[[i]][j, colparam + 3]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq092" = {
                 p25 <- listPlot[[i]][j, colparam]
                 aa <- listPlot[[i]][j, colparam + 1]
                 bb <- listPlot[[i]][j, colparam + 2]
                 cc <- listPlot[[i]][j, colparam + 3]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq100" = {
                 Rm <- listPlot[[i]][j, colparam]
                 Tm <- listPlot[[i]][j, colparam + 1]
                 To <- listPlot[[i]][j, colparam + 2]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i, to = Tm + To)
               },
               "eq110" = {
                 a0 <- listPlot[[i]][j, colparam]
                 a1 <- listPlot[[i]][j, colparam + 1]
                 a2 <- listPlot[[i]][j, colparam + 2]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq120" = {
                 a0 <- listPlot[[i]][j, colparam]
                 a1 <- listPlot[[i]][j, colparam + 1]
                 a2 <- listPlot[[i]][j, colparam + 2]
                 a3 <- listPlot[[i]][j, colparam + 3]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq130" = {
                 a0 <- listPlot[[i]][j, colparam]
                 a1 <- listPlot[[i]][j, colparam + 1]
                 a2 <- listPlot[[i]][j, colparam + 2]
                 a3 <- listPlot[[i]][j, colparam + 3]
                 a4 <- listPlot[[i]][j, colparam + 4]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq140" = {
                 phi <- listPlot[[i]][j, colparam]
                 aa <- listPlot[[i]][j, colparam + 1]
                 Tb <- listPlot[[i]][j, colparam + 2]
                 Tmax <- listPlot[[i]][j, colparam + 3]
                 deltaT <- listPlot[[i]][j, colparam + 4]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq150" = {
                 Rm <- listPlot[[i]][j, colparam]
                 Tmax <- listPlot[[i]][j, colparam + 1]
                 To <- listPlot[[i]][j, colparam + 2]
                 T1 <- listPlot[[i]][j, colparam + 3]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt[1]))))
                 graphics::curve(fx, add = TRUE, col = i, from = 0, to = Tmax)
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt[2]))))
                 graphics::curve(fx, add = TRUE, col = i, from = Tmax, to = 60)
               },
               "eq160" = {
                 aa <- listPlot[[i]][j, colparam]
                 Tmax <- listPlot[[i]][j, colparam + 1]
                 deltaT <- listPlot[[i]][j, colparam + 2]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq170" = {
                 aa <- listPlot[[i]][j, colparam]
                 Tmax <- listPlot[[i]][j, colparam + 1]
                 deltaT <- listPlot[[i]][j, colparam + 2]
                 bb <- listPlot[[i]][j, colparam + 3]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq180" = {
                 aa <- listPlot[[i]][j, colparam]
                 Tmin <- listPlot[[i]][j, colparam + 1]
                 Tmax <- listPlot[[i]][j, colparam + 2]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq190" = {
                 aa <- listPlot[[i]][j, colparam]
                 Tmin <- listPlot[[i]][j, colparam + 1]
                 Tmax <- listPlot[[i]][j, colparam + 2]
                 bb <- listPlot[[i]][j, colparam + 3]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq200" = {
                 aa <- listPlot[[i]][j, colparam]
                 Tmin <- listPlot[[i]][j, colparam + 1]
                 Tmax <- listPlot[[i]][j, colparam + 2]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq210" = {
                 aa <- listPlot[[i]][j, colparam]
                 bb <- listPlot[[i]][j, colparam + 1]
                 cc <- listPlot[[i]][j, colparam + 2]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq220" = {
                 aa <- listPlot[[i]][j, colparam]
                 bb <- listPlot[[i]][j, colparam + 1]
                 cc <- listPlot[[i]][j, colparam + 2]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq230" = {
                 K <- listPlot[[i]][j, colparam]
                 r <- listPlot[[i]][j, colparam + 1]
                 T0 <- listPlot[[i]][j, colparam + 2]
                 TL <- listPlot[[i]][j, colparam + 3]
                 TH <- listPlot[[i]][j, colparam + 4]
                 aa <- listPlot[[i]][j, colparam + 5]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq240" = {
                 p1 <- listPlot[[i]][j, colparam]
                 p2 <- listPlot[[i]][j, colparam + 1]
                 p3 <- listPlot[[i]][j, colparam + 2]
                 p4 <- listPlot[[i]][j, colparam + 3]
                 p5 <- listPlot[[i]][j, colparam + 4]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq250" = {
                 cc <- listPlot[[i]][j, colparam]
                 k1 <- listPlot[[i]][j, colparam + 1]
                 k2 <- listPlot[[i]][j, colparam + 2]
                 T1 <- listPlot[[i]][j, colparam + 3]
                 T2 <- listPlot[[i]][j, colparam + 4]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq260" = {
                 phi <- listPlot[[i]][j, colparam]
                 bb <- listPlot[[i]][j, colparam + 1]
                 Tb <- listPlot[[i]][j, colparam + 2]
                 Tm <- listPlot[[i]][j, colparam + 3]
                 deltab <- listPlot[[i]][j, colparam + 4]
                 deltam <- listPlot[[i]][j, colparam + 5]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq270" = {
                 cc <- listPlot[[i]][j, colparam]
                 k1 <- listPlot[[i]][j, colparam + 1]
                 T1 <- listPlot[[i]][j, colparam + 2]
                 k2 <- listPlot[[i]][j, colparam + 3]
                 T2 <- listPlot[[i]][j, colparam + 4]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq280" = {
                 Tmin <- listPlot[[i]][j, colparam]
                 aa <- listPlot[[i]][j, colparam + 1]
                 Topt <- listPlot[[i]][j, colparam + 2]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq290" = {
                 bb <- listPlot[[i]][j, colparam]
                 Tb <- listPlot[[i]][j, colparam + 1]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq300" = {
                 cc <- listPlot[[i]][j, colparam]
                 T1 <- listPlot[[i]][j, colparam + 1]
                 k <- listPlot[[i]][j, colparam + 2]
                 T2 <- listPlot[[i]][j, colparam + 3]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq310" = {
                 mu <- listPlot[[i]][j, colparam]
                 Tb <- listPlot[[i]][j, colparam + 1]
                 aa <- listPlot[[i]][j, colparam + 2]
                 Tc <- listPlot[[i]][j, colparam + 3]
                 bb <- listPlot[[i]][j, colparam + 4]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq320" = {
                 aa <- listPlot[[i]][j, colparam]
                 bb <- listPlot[[i]][j, colparam + 1]
                 cc <- listPlot[[i]][j, colparam + 2]
                 dd <- listPlot[[i]][j, colparam + 3]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq330" = {
                 aa <- listPlot[[i]][j, colparam]
                 bb <- listPlot[[i]][j, colparam + 1]
                 cc <- listPlot[[i]][j, colparam + 2]
                 dd <- listPlot[[i]][j, colparam + 3]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq340" = {
                 aa <- listPlot[[i]][j, colparam]
                 bb <- listPlot[[i]][j, colparam + 1]
                 Tm <- listPlot[[i]][j, colparam + 2]
                 Tmin <- listPlot[[i]][j, colparam + 3]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq350" = {
                 rm <- listPlot[[i]][j, colparam]
                 T1 <- listPlot[[i]][j, colparam + 1]
                 T2 <- listPlot[[i]][j, colparam + 2]
                 Tm <- listPlot[[i]][j, colparam + 3]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               "eq360" = {
                 cc <- listPlot[[i]][j, colparam]
                 T1 <- listPlot[[i]][j, colparam + 1]
                 T2 <- listPlot[[i]][j, colparam + 2]
                 k <- listPlot[[i]][j, colparam + 3]
                 fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
                 graphics::curve(fx, add = TRUE, col = i)
               },
               {
                # otherwise nothing
               }
        )
    }
  }
  graphics::legend("topleft", legend = names(listPlot), col = 1:length(listPlot), lwd = 1)
  graphics::text(
    x = graphics::par("xaxp")[2],
    y = graphics::par("yaxp")[2],
    pos = 2,
    paste0(eq$name, " (", eq$refShort, ")"), ...
  )
  }
}
