plot.desk = function (x, ...){

  obj = x

  # if (missing(details)) {details = attr(obj, "details")}

  ##########################################
  # GENERAL FUNCTIONS ######################
  ##########################################

  mat.abline = function(X, ...){
    if(dim(X)[1] != 2) stop("Matrix must be 2 x n.", call. = F)
    for (i in 1:dim(X)[2]){
      abline(a = X[1,i], b = X[2,i], ...)
    }
  }

  ##########################################
  # PLOT-FUNCTIONS DEFINITIONS #############
  ##########################################
  switch(attr(obj, "type"),

         # OLS-REGRESSION (plot)
         ols = {
           draw.ols = function(obj, plot.what = "regline", ...){

             # REGRESSION LINES
             if (plot.what == "regline"){
               draw.regline = function(obj, pred.int = FALSE, conf.int = FALSE, residuals = FALSE, center = FALSE, sig.level = 0.05,
                                       xlim, ylim, ...){
                 # Check dimension
                 if((obj$has.const & obj$ncoef > 2) | (!obj$has.const & obj$ncoef > 1)){
                   stop("Can plot only models with one regressor. Sorry...", call. = F)
                 }

                 # Function to get ordered x values and intervals
                 get.vals = function(obj, interval, level, xnew){
                   vals = suppressWarnings(ols.interval(obj, type = interval, sig.level = level, xnew = xnew)$results)
                   colnames(vals) = c("center", "lwr", "upr")
                   idx = order(obj$data[,2])
                   vals = cbind(x = x[idx], vals[idx,])
                 }

                 # Extract values
                 y = as.matrix(obj$data[,1])
                 x = as.matrix(obj$data[,2])

                 # Calculate prediction intervals
                 if (pred.int){
                   v.pred = get.vals(obj, interval = "prediction", sig.level, xnew = x)
                 }

                 # Calculate confidence intervals
                 if (conf.int){
                   v.conf = get.vals(obj, interval = "confidence", level = sig.level, xnew = x)
                 }

                 # Calculate ylim
                 if (missing(xlim)){xlim = c(min(x),max(x))}
                 if (missing(ylim)){
                   if (pred.int){v = v.pred} else {if(conf.int) {v = v.conf} else {v = data.frame(lwr = y, upr = y)}}
                   ylim = c(min(v[,"lwr"]) - 0.2*abs(min(v[,"lwr"])),
                            max(v[,"upr"]) + 0.2*abs(max(v[,"upr"])))}

                 # Scatterplot and regression line
                 plot(x, y, xlim = xlim, ylim = ylim, ...)
                 abline(obj)

                 # Prognoseband
                 if (pred.int){# Plotte Prognoseband
                   polygon( c(v.pred[,"x"], rev(v.pred[,"x"])),
                            c(v.pred[,"lwr"], rev(v.pred[,"upr"])),
                            col = rgb(0.8, 0.8, 0.8, alpha = 0.25), border = NA)
                   matlines(v.pred[,"x"], v.pred[,c("lwr","upr")], col = "black", lty = 1, type = "l")
                 }

                 # Konfidenzband
                 if (conf.int){# Plotte Konfidenzband
                   polygon( c(v.conf[,"x"], rev(v.conf[,"x"])),
                            c(v.conf[,"lwr"], rev(v.conf[,"upr"])),
                            col = rgb(0.6, 0.6, 0.6, alpha = 0.25), border = NA)
                   matlines(v.conf[,"x"], v.conf[,c("lwr","upr")], col = "black", lty = 1, type = "l")
                 }

                 # Residuen
                 if (residuals){# Plotte die Residuen
                   segments(x0 = x, y0 = y, x1 = x, y1 = fitted(obj))
                 }

                 # Plot Center
                 if (center){
                   abline(h = mean(y), v = mean(x), lty = 2)
                 }
               }
               draw.regline(obj, ...)
             } # end of "regline" option (OLS)
           } # end of draw.ols
           draw.ols(obj, ...)
         }, # end of ols option

         # INTERVALS (plot)
         int = {
           print("SORRY, NOT IMPLEMENTED YET ...")
         },

         # INSTRUMENT VARIABLE REGRESSION (plot)
         ivr = {
           print("SORRY, NOT IMPLEMENTED YET ...")
         },

         # REPSAMPLE (plot)
         repsamp = {
           draw.repsample = function(obj, plot.what = "confint", ...){

             # CONFIDENCE INTERVALS
             if (plot.what == "confint"){
               draw.confint = function(C, which.coef = 2, center = TRUE, center.size = 1,
                                       xlim, lwd = 1, ... ){

                 coef = C$coef[which.coef,]
                 lower = C$confint[which.coef,"lower",]
                 upper = C$confint[which.coef,"upper",]
                 if (missing(xlim)){xlim = c(min(lower), max(upper))}
                 if (length(lower) > 50){l = 0} else {l = 0.04}
                 num.reps = length(coef)
                 tr.par = C$true.par[which.coef]
                 clrs = ifelse(tr.par < lower | tr.par > upper, "black", "gray40")
                 lwds = ifelse(tr.par < lower | tr.par > upper, 2*lwd, lwd)
                 plot(1, type = "n", yaxt = "n", ylab = NA,
                      xlim = xlim,
                      ylim = c(1, num.reps), ...)
                 arrows(x0 = lower, y0 = 1:num.reps, x1 = upper, y1 = 1:num.reps,
                        col = clrs, code = 3, angle = 90, length = l, lwd = lwds)
                 if(center){
                   points(coef, 1:num.reps, cex = center.size, col = clrs)
                 }
                 abline(v = tr.par, lwd = 1.5, lty = 3)# dashed line of true value
               }
               draw.confint(obj,...)
             } # end of "confint" option

             # REGRESSION LINES
             if (plot.what == "reglines"){
               draw.reglines = function(obj, xlim, ylim, ... ){
                 if(dim(obj$coef)[1] > 2) stop("Only simple linear regression models make sense here.", call. = F)
                 if (missing(xlim)){xlim = c(min(obj$x), max(obj$x))}
                 if (missing(ylim)){ylim = c(min(obj$y), max(obj$y))}
                 plot(0, xlim = xlim, ylim = ylim, type = "n", xlab = "x", ylab = "y")
                 mat.abline(obj$coef, col = rgb(0.1, 0.1, 0.1, alpha = 0.05), lwd = 2)
                 abline(coef = obj$true.par, lwd = 2, col = "grey20")
               }
               draw.reglines(obj, ...)
             } # end of "reglines" option

             # SCATTER PLOTS
             if (plot.what == "scatter"){
               draw.scatter = function(obj, xlim, ylim, ... ){
                 if(dim(obj$coef)[1] > 2) stop("Only simple linear regression models make sense here.", call. = F)
                 if (missing(xlim)){xlim = c(min(obj$x), max(obj$x))}
                 if (missing(ylim)){ylim = c(min(obj$y), max(obj$y))}
                 matplot(obj$x, obj$y, xlim = xlim, ylim = ylim,
                         xlab = "x", ylab = "y",
                         type = "p", pch = 16,
                         col = rgb(0.1, 0.1, 0.1, alpha = 0.05))
                 abline(coef = obj$true.par, lwd = 2, col = "grey40")
               }
               draw.scatter(obj, ...)
             } # end of "scatter" option

           } # end of draw.repsample
           draw.repsample(obj, ...)
         }, # end of repsamp option

         # HYPOTHESIS TESTS (plot)
         htest = {

           # Spezialgrafik beim QLR Test
           draw.qlr = function(obj, crit.val = TRUE, breakpoint = TRUE, ...){
             plot(obj$periods,obj$f.stats,
                  xlab = "period", ylab = "F-value",
                  ylim = c(0, max(obj$f.stats)),
                  type = "b", xaxt = "n", ...)
             axis(1, at = obj$periods)
             if(crit.val) abline(h = obj$f.crit, lty = "solid")
             if(breakpoint) abline(v = obj$breakpoint, lty = "dashed")
           }

           # Spezialgrafik beim F-Test (Ellipse)
           draw.ellipse = function (obj, sig.level = 0.05, type = "acceptance",
                                    center = TRUE, intervals = TRUE, test.point = TRUE,
                                    legcol, q = c(0,0), ...){

             mod = obj$mod
             q = obj$q
             t = sqrt(2 * qf(1-sig.level, 2, mod$df.residual))
             if (dim(obj$nh)[1] > 2){stop("Can not plot ellipses with more than 2 linear hypotheses...", call. = F)}
             if (type == "confidence"){
               center.loc = as.numeric(obj$nh %*% mod$coefficients)
               test.loc = as.numeric(q)
               center.name = NA
               test.name = "q-values"
             } else if (type == "acceptance") {
               if(missing(q)) {
                 message("No q-value specified against which the Null hypothesis is tested. Use zero as default value(s).")
               }
               center.loc = as.numeric(q)
               center.name = "q-values"
               test.name = "coeffs."
               test.loc = as.numeric(obj$nh %*% mod$coefficients)
             }
             # Calculate ellipse data
             covar = obj$nh %*% vcov(mod) %*% t(obj$nh)
             sd.nh = sqrt(diag(covar))
             if (is.matrix(covar)){
               cv = covar[1,2]
               if (sd.nh[1] > 0) cv = cv/sd.nh[1]
               if (sd.nh[2] > 0) cv = cv/sd.nh[2]
             } else {cv = covar}
             cv = min(max(cv,-1),1)# limit
             p = seq(0, 2*pi, len = 200)
             data = matrix(
               c(t * sd.nh[1] * cos(p + acos(cv)/2) + center.loc[1],
                 t * sd.nh[2] * cos(p - acos(cv)/2) + center.loc[2]),
               200, 2)
             # The plots
             if (test.point){
               # adjust x and y limits for test.point to be included in graph
               lims = cbind(c(min(data[,1]), max(data[,1])),
                            c(min(data[,2]), max(data[,2])))
               lims = rbind(lims, test.loc)
               x.lim = c(min(lims[,1]), max(lims[,1]))
               y.lim = c(min(lims[,2]), max(lims[,2]))
             }
             plot(data, type = "l", xlab = "1st lin. comb.", ylab = "2nd lin. comb.", xlim = 1.05*x.lim, ylim = 1.05*y.lim, ...)
             if (intervals){
               a = as.numeric(obj$results["sig.level"]/2)
               a = c(a, 1 - a)
               tval = qt(a, mod$df) # critical t_a/2 values
               ints = cbind(center.loc, center.loc + sd.nh %o% tval)
               abline(v = ints[1,], lty = 2, col = "gray70")
               abline(h = ints[2,], lty = 2, col = "gray70")
             }
             if (center){
               points(t(center.loc), pch = 3)
               abline(v = center.loc[1], lty = 3, col = "gray70")
               text(center.loc[1], c(center.loc[2]), center.name, col = "black", cex = 0.8, adj = c(1.2,-0.3))
               abline(h = center.loc[2], lty = 3, col = "gray70")
             }
             if (test.point){
               points(t(test.loc), pch = 16)
               text(t(test.loc), test.name, col = "black", cex = 0.8, adj = c(1.2,-0.3))
             }
             legend("topleft", legend = paste(c("cov = "), round(covar[1,2], digits = 2)),
                    text.col = "black", bg = legcol, x.intersp = -0.5, cex = 0.6)
           } # end of draw.ellipse

           draw.test = function (obj, xlim = c(0,4), plot.what = "dist", xlab, ylab, legpos = "topright", ...){

             # Welche Version des Buches wird kompiliert?
             if(Sys.getenv("PUBVERSION") == "PRINT"){
               # legendbg = rgb(242/255, 244/255, 248/255) # brighter blue
               # legendbg = rgb(229/255, 232/255, 240/255) # darker blue
               legendbg = rgb(218/255, 228/255, 240/255) # darker blue
             } else if(Sys.getenv("PUBVERSION") == "ONLINE") {
               # legendbg = rgb(1, 247/255, 229/255) # brighter orange
               # legendbg = rgb(254/255, 238/255, 229/255) # darker orange
               legendbg = rgb(218/255, 228/255, 240/255) # darker blue
             } else {
               legendbg = "white"}

             switch(plot.what,

                    dist = {
                      if (obj$nulldist[[1]] == "dw" & attr(obj, "direction") == "both"){
                        stop("Two sided test graphics for Durbin Watson Test not implemented,", call. = F)
                      }

                      # Funktion zum Aufrufen eines Verteilungsbefehls (z.B. qf)
                      call.dist = function(type = "d", dist = "norm", value = 1, param = c(0,1)){
                        if(is.matrix(param)){ # DW Test has design matrix as param
                          arg.list = list(value, param)
                        } else {
                          arg.list = vector("list", length(param)+1) # Preallocate list
                          arg.list[[1]] = value # value as first argument
                          for (i in 2:length(arg.list)) {arg.list[[i]] = param[i-1]} # Fill list with params
                        }
                        do.call(paste(type, dist, sep = ""), arg.list)
                      }

                      # Funktion Füllen einer Fläche unterhalb der Kurve
                      fill.area = function(a, b, points = 200, ...){
                        coord.x = c(a, seq(from = a, to = b, length.out = points), b)
                        coord.y = c(0, call.dist(type = "d", dist = dist, value = seq(from = a, to = b, length.out = points), param = param), 0)
                        if(max(coord.y) > 0.0001){
                          polygon(coord.x, coord.y, ...)
                        } #else {message("At least one area was too flat to be plotted meaningfully.")}
                      }

                      # Funktion zum plotten von vertikalen linien im Diagramm
                      vline = function(x, col = "black", lty = "dashed", lwd = 1.5){
                        lines(c(x,x), c(0, call.dist("d", dist, x, param)), col = col, lty = lty, lwd = lwd)
                        abline(v = x, col = col, lty = lty, lwd = lwd)
                      }
                      # Eigene Funktion zur Legende
                      my.legend = function(legcol = legendbg, legpos = legpos){
                        if (is.matrix(param)) param = ""
                        if (attr(obj, "direction") == "both") {
                          tmp = round(sig.level/2,3)} else {
                            tmp = round(sig.level,2)}

                        tmp = paste("(", tmp, ",", " ", paste(param, collapse = ","),")", collapse = ",", sep = "")
                        cv = eval(
                          substitute(expression(paste(x,scriptstyle(y))),
                                     list(x = dist, y = tmp))
                        )
                        legend(x = legpos,
                               legend = c(
                                 paste(dist, "-val.", sep = ""),
                                 cv,
                                 paste("a =",round(sig.level,2)),
                                 paste("p =",round(p.val,2))
                               ),
                               lty = c("solid", "dotted", "blank", "blank"),
                               lwd = c(2, lines.lwd, 0, 0),
                               border = c(NA, NA, "black", p.col),
                               col = c(p.col, tstat.col, rej.col, p.col),
                               fill = c(0, 0, rej.col, p.col),
                               density = c(0, 0, NA, 20),
                               bg = legcol,
                               angle = if (attr(obj, "direction") == "left") -45 else 45,
                               inset = 0.02,
                               text.col = "black",
                               cex = 0.7,
                               merge = TRUE)
                      }

                      dist = obj$nulldist[[1]] # Type of distribution (Character)
                      param = obj$nulldist[[2]] # # Parameter(s) of the distribution
                      tstat = as.numeric(obj$results[1])
                      cr.val = as.numeric(obj$results[2])
                      p.val = as.numeric(obj$results[3])
                      sig.level = as.numeric(obj$results[4])
                      if (missing(xlim)){
                        if (dist == "dw") {xlim = c(0,4)} else {
                          xlim[1] = round(call.dist(type = "q", dist = dist, value = 0.01, param = param),2)
                          xlim[2] = round(call.dist(type = "q", dist = dist, value = 0.99, param = param),2)
                        }
                      }

                      y.lab = if (obj$nulldist[1] == "chisq") {
                        expression(paste(chi^2,"-density", sep = ""),)} else {
                          paste(obj$nulldist[1],"-density", sep = "")}

                      if (missing(xlab)) xlab = "quantiles"
                      if (missing(ylab)) ylab = y.lab

                      # Plotte die Dichtefunktion der Nullverteilung
                      curve(call.dist("d", dist, x, param),
                            #col = "gray70",
                            lwd = 1.5,
                            las = 0,
                            xlim = xlim,
                            xlab = xlab,
                            ylab = ylab,
                            ...)

                      rej.col = "gray70"
                      p.col = "black"
                      tstat.col = "black"
                      cr.val.col = "black"
                      lines.lwd = 2

                      switch(attr(obj, "direction"),

                             both = {
                               # plot left rej. area in grey
                               fill.area(
                                 a = xlim[1], b = -abs(cr.val),
                                 col = rej.col, border = "black")

                               # plot right rej. area in grey
                               fill.area(
                                 a = abs(cr.val), b = xlim[2],
                                 col = rej.col, border = "black")

                               # plot left part of p-value
                               fill.area(
                                 a = xlim[1], b = -abs(tstat),
                                 density = 15, angle = 45, col = p.col)

                               # plot right part of p-value
                               fill.area(
                                 a = abs(tstat), b = xlim[2],
                                 density = 15, angle = 45, col = p.col)

                               # Test statistic line
                               vline(tstat, col = tstat.col, lty = "solid", lwd = 2)

                               # Critical values lines
                               vline(cr.val, col = tstat.col, lty = "dotted", lwd = 1)
                               vline(-cr.val, col = tstat.col, lty = "dotted", lwd = 1)

                             }, # end of option "both"

                             left = {
                               # plot left rej. area in grey
                               fill.area(
                                 a = xlim[1], b = cr.val,
                                 col = rej.col, border = "black")

                               # plot left p-value
                               fill.area(
                                 a = xlim[1], b = tstat,
                                 density = 15, angle = -45, col = p.col)

                               # Test statistic line
                               vline(tstat, col = tstat.col, lty = "solid", lwd = 2)

                               # Critical values lines
                               vline(cr.val, col = tstat.col, lty = "dotted", lwd = 1)
                             },

                             right = {
                               # plot right rej. area in grey
                               fill.area(
                                 a = abs(cr.val), b = xlim[2],
                                 col = rej.col, border = "black")

                               # plot right p-value
                               fill.area(
                                 a = tstat, b = xlim[2],
                                 density = 15, angle = 45, col = p.col)

                               # Critical values lines
                               vline(cr.val, col = tstat.col, lty = "dotted", lwd = 1)
                             }
                      ) # end of direction switch

                      # Test statistic line
                      vline(tstat, col = tstat.col, lty = "solid", lwd = 2)

                      my.legend(legcol = legendbg, legpos)
                    }, # end of plot.what = "dist"

                    ellipse = {
                      draw.ellipse(obj, legcol = legendbg, ...)
                    }
             ) #end of switch "plot.what"

           } # end of draw.test
           # ----------------------------------------------

           if (attr(obj, "test.type") == "qlrtest") {
             draw.qlr(obj, ...)
           } else {
             draw.test(obj, ...) # call main function
           }
           # ----------------------------------------------
         }, # end of htest option

         # COCHRANE-ORCUTT REGRESSION (plot)
         cochorc = {
           draw.cochorc = function(obj, ...){

             # Extract values
             y = as.numeric(obj$all.regs[,"rho.hat"])
             x = 1:length(y)
             #rho = round(as.numeric(obj$results["rho"]),2)
             #ssr = as.numeric(obj$results["SSR"])

             plot(x,y,
                  xlab = "iteration", ylab = expression(hat(rho)),
                  xlim = c(min(x),max(x)),
                  type = "b",
                  xaxp = c(min(x), max(x), max(x)-1),
                  ...
             )
             #abline(v = obj$results["rho"], lty = "dashed")
             #text(rho, 0.9*max(y), pos = 4, bquote(rho[min] == .(rho)))
           } # end of draw.cochorc
           draw.cochorc(obj, ...)
         },

         # HILDRETH-LU REGRESSION (plot)
         hilu = {
           draw.hilu = function(obj, xlab, ylab, ...){

             if (missing(xlab)) xlab = expression(rho)
             if (missing(ylab)) ylab = "SSR"
             # Extract values
             y = as.numeric(obj$all.regs[,"SSR"])
             x = as.numeric(obj$all.regs[,"rho"])
             #rho = round(as.numeric(obj$all.regs["rho"]),2)
             #ssr = as.numeric(obj$all.regs["SSR"])

             plot(x,y,
                  xlim = c(min(x),max(x)),
                  type = "l",
                  xlab = xlab,
                  ylab = ylab,
                  ...
             )
             abline(v = obj$rho.opt, lty = "dashed")
             # text(0.7*obj$rho.opt, 0.9*max(y), pos = 4, bquote(rho[min] == .(obj$rho.opt)))
           } # end of draw.hilu
           draw.hilu(obj, ...)
         },

         # AR(1) SIMULATION (plot)
         ar1sim = {
           draw.ar1 = function(obj, plot.what = "time", ...){

             # TIME PLOT
             if (plot.what == "time"){
               draw.time = function(obj, ylim, ylab, ...){
                 if (missing(ylim)) {ylim = c(min(obj$u.sim),max(obj$u.sim))}
                 if (missing(ylab)) {ylab = expression(u[t])}
                 plot(obj$u.sim[-1],
                      type = "p",
                      xlab = "t",
                      ylab = ylab,
                      ylim = ylim,
                      ...)
                 points(0, obj$u.sim[1],
                        pch = 1,
                        ...)
                 lines(0:(length(obj$u.sim)-1), obj$u.sim,
                       lty = "solid",
                       ...)
                 #legend("top", legend = bquote(rho == .(round(obj$rho, digits = 2))) ,
                 legend("top", legend = paste("rho = ", round(obj$rho, digits = 2)) ,
                        bty = "n", text.col ="black", bg = NA, cex = 0.7)
                 abline(h = 0, col = "black")
               } # end of function draw.time
               draw.time(obj, ...)
             } # end of time plot option

             # LAG 1 PLOT
             if (plot.what == "lag"){
               draw.lag = function(obj,
                                   lag = 1,
                                   legend = FALSE, cex.legend = 0.7,
                                   true.line = TRUE,
                                   ols.line = FALSE,
                                   acc.line = FALSE,
                                   xlim, ylim, ...){
                 if (missing(xlim)) {xlim = c(min(obj$u.sim),max(obj$u.sim))}
                 if (missing(ylim)) {ylim = c(min(obj$u.sim),max(obj$u.sim))}

                 # Generate lagged data
                 lag.data = lagk(obj$u.sim, lag = lag)

                 # lineares Modell ohne Niveauparameter
                 lag.mod = lm.fit(x = cbind(0,lag.data[,lag+1]), y = lag.data[, 1])

                 # Berechne Autokorrelationskoeffizient
                 rho.acc = acc(obj$u.sim, lag = lag)

                 # Berechne KQ-Schaetzer
                 rho.ols = lag.mod$coefficients[2]

                 # Berechne Korrelationskoeffizient
                 r = cor(lag.data[,lag+1], lag.data[,1])

                 # Erstelle Lagplot der Residuen
                 plot(lag.data[,lag+1], lag.data[,1],
                      xlab = expression(u[t-1]),
                      ylab = expression(u[t]),
                      xlim = xlim, ylim = ylim,
                      asp = 1, ...)

                 # Plotte Achsen
                 abline(h = 0, v = 0, col  = "black")

                 # Plotte Geraden
                 if (true.line)  abline(a = 0, b = obj$rho)
                 if (ols.line)  abline(a = 0, b = rho.ols, col = "gray70", lty = "dashed")
                 if (acc.line)  abline(a = 0, b = rho.acc, col = "gray70", lty = "dotted")

                 # Legende mit den Werten ein
                 if(legend){
                   legend("top", legend = paste(c("true rho = ", "rho.acc = ",
                                                  "rho.ols = ", "rho = "),
                                                round(c(obj$rho, rho.acc, rho.ols, r), digits = 2)),
                          text.col = "black", bg = "white", x.intersp = -0.5, cex = cex.legend)
                 }
               } # end of function draw.lag
               draw.lag(obj, ...)
             } # end of plot.what == "lag"

           } # end of draw.ar1
           draw.ar1(obj, ...)
         } # end of ar1sim option

  ) # END OF SWITCH
}
