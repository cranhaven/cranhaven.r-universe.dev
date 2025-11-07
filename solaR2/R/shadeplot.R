setGeneric('shadeplot', function(x, ...)standardGeneric('shadeplot'))

setMethod('shadeplot', signature(x = 'Shade'),
          function(x,
                   main = '',
                   xlab = expression(L[ew]),
                   ylab = expression(L[ns]),
                   n = 9, ...){
              red <- x@distances
              FS.loess <- x@FS.loess
              Yf.loess <- x@Yf.loess
              struct <- x@struct
              mode <- x@modeTrk
              if (mode=='two'){
                  Lew <- seq(min(red$Lew),max(red$Lew),length = 100)
                  Lns <- seq(min(red$Lns),max(red$Lns),length = 100)
                  Red <- expand.grid(Lew = Lew,Lns = Lns)
                  FS <- predict(FS.loess,Red)
                  Red$FS <- as.numeric(FS)
                  AreaG <- with(struct,L*W)
                  GRR <- Red$Lew*Red$Lns/AreaG
                  Red$GRR <- GRR
                  FS.m <- matrix(1-FS,
                                 nrow = length(Lew),
                                 ncol = length(Lns))
                  GRR.m <- matrix(GRR,
                                  nrow = length(Lew),
                                  ncol = length(Lns))
                  niveles <- signif(seq(min(FS.m),max(FS.m),l = n+1),3)
                  pruebaCB <- ("RColorBrewer" %in% .packages())
                  if (pruebaCB) {
                      paleta <- rev(brewer.pal(n, 'YlOrRd'))
                  } else {
                      paleta <- rev(heat.colors(n))}
                  oldpar <- par(no.readonly = TRUE)
                  on.exit(par(oldpar))
                  par(mar = c(4.1,4.1,2.1,2.1))
                  filled.contour(x = Lew,y = Lns,z = FS.m,#...,
                                 col = paleta, #levels = niveles,
                                 nlevels = n,
                                 plot.title = title(xlab = xlab,
                                                    ylab = ylab, main = main),
                                 plot.axes = {
                                     axis(1); axis(2);
                                     contour(Lew, Lns, FS.m,
                                             nlevels = n, #levels = niveles,
                                             col = "black", labcex = .8,  add = TRUE)
                                     contour(Lew, Lns, GRR.m,
                                             col = "black", lty = 3, labcex = .8, add = TRUE)
                                     grid(col = "white",lty = 3)},
                                 key.title = title("1-FS",cex.main = .8))
              }
              if (mode=='horiz') {
                  Lew <- seq(min(red$Lew),max(red$Lew),length = 100)
                  FS <- predict(FS.loess,Lew)
                  GRR <- Lew/struct$L
                  plot(GRR,1-FS,main = main,type = 'l',...)
                  grid()    }
              if (mode=='fixed'){
                  D <- seq(min(red$D),max(red$D),length = 100)
                  FS <- predict(FS.loess,D)
                  GRR <- D/struct$L
                  plot(GRR,1-FS,main = main,type = 'l',...)
                  grid()    }
          }
          )
