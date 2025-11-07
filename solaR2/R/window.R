setMethod('[',
          signature='Meteo',
          definition=function(x, i, j,...){
              if (!missing(i)) {
                  i <- truncDay(i)
              } else {
                  i <- indexD(x)[1]
              }
              if (!missing(j)) {
                  j <- truncDay(j)+86400-1 ##The end is the last second of the day
              } else {
                  nDays <- length(indexD(x))
                  j <- indexD(x)[nDays]+86400-1
              }
              stopifnot(j>i)
              if (!is.null(i)) i <- truncDay(i)
              if (!is.null(j)) j <- truncDay(j)+86400-1
              d <- indexD(x)
              x@data <- x@data[(d >= i & d <= j)]
              x
          }
          )


setMethod('[',
          signature='Sol',
          definition=function(x, i, j, ...){
              if (!missing(i)) {
                  i <- truncDay(i)
              } else {
                  i <- indexD(x)[1]
              }
              if (!missing(j)) {
                  j <- truncDay(j)+86400-1##The end is the last second of the day
              } else {
                  nDays <- length(indexD(x))
                  j <- indexD(x)[nDays]+86400-1
              }
              stopifnot(j>i)
              if(!is.null(i)) i <- truncDay(i)
              if(!is.null(j)) j <- truncDay(j)
              d1 <- indexD(x)
              d2 <- indexI(x)
              x@solD <- x@solD[(d1 >= i & d1 <= j)]
              x@solI <- x@solI[(d2 >= i & d2 <= j)]
              x
          }
          )

setMethod('[',
          signature='G0',
          definition=function(x, i, j, ...){
              sol <- as(x, 'Sol')[i=i, j=j, ...] ##Sol method
              meteo <- as(x, 'Meteo')[i=i, j=j, ...] ##Meteo method
              i <- indexI(sol)[1]
              j <- indexI(sol)[length(indexI(sol))]
              d1 <- indexD(x)
              d2 <- indexI(x)
              G0Iw <- x@G0I[(d2 >= i & d2 <= j)]
              Taw <- x@Ta[(d2 >= i & d2 <= j)]
              G0dw <- x@G0D[(d1 >= truncDay(i) & d1 <= truncDay(j))]
              G0dmw <- G0dw[, lapply(.SD/1000, mean, na.rm= TRUE),
                            .SDcols = c('G0d', 'D0d', 'B0d'),
                            by = .(month(Dates), year(Dates))]
              if (x@type == 'prom'){
                  G0dmw[, DayOfMonth := DOM(G0dmw)]
                  G0yw <- G0dmw[, lapply(.SD*DayOfMonth, sum, na.rm = TRUE),
                                .SDcols = c('G0d', 'D0d', 'B0d'),
                                by = .(Dates = year)]
                  G0dmw[, DayOfMonth := NULL]
              } else {
                  G0yw <- G0dw[, lapply(.SD/1000, sum, na.rm = TRUE),
                               .SDcols = c('G0d', 'D0d', 'B0d'),
                               by = .(Dates = year(unique(truncDay(Dates))))]
              }
              G0dmw[, Dates := paste(month.abb[month], year, sep = '. ')]
              G0dmw[, c('month', 'year') := NULL]
              setcolorder(G0dmw, 'Dates')
              result <- new('G0',
                            meteo,
                            sol,
                            G0D = G0dw,
                            G0dm = G0dmw,
                            G0y = G0yw,
                            G0I = G0Iw,
                            Ta = Taw)
              result
          }
          )


setMethod('[',
          signature = 'Gef',
          definition = function(x, i, j, ...){
              g0 <- as(x, 'G0')[i = i, j = j, ...] ##G0 method
              i <- indexI(g0)[1]
              j <- indexI(g0)[length(indexI(g0))]
              d1 <- indexD(x)
              d2 <- indexI(x)
              GefIw <- x@GefI[(d2 >= i & d2 <= j)]
              Thetaw <- x@Theta[(d2 >= i & d2 <= j)]
              Gefdw <- x@GefD[(d1 >= truncDay(i) & d1 <= truncDay(j))]
              nms <- c('Bod', 'Bnd', 'Gd', 'Dd',
                       'Bd', 'Gefd', 'Defd', 'Befd')
              Gefdmw <- Gefdw[, lapply(.SD/1000, mean, na.rm = TRUE),
                              .SDcols = nms,
                              by = .(month(Dates), year(Dates))]
              if (x@type == 'prom'){
                  Gefdmw[, DayOfMonth:= DOM(Gefdmw)]
                  Gefyw <- Gefdmw[, lapply(.SD*DayOfMonth, sum),
                                  .SDcols = nms,
                                  by = .(Dates = year)]
                  Gefdmw[, DayOfMonth := NULL]
              } else {
                  Gefyw <- Gefdw[, lapply(.SD/1000, sum, na.rm = TRUE),
                                 .SDcols = nms,
                                 by = .(Dates = year(Dates))]
              }
              Gefdmw[, Dates := paste(month.abb[month], year, sep = '. ')]
              Gefdmw[, c('month', 'year') := NULL]
              setcolorder(Gefdmw, 'Dates')
              result <- new('Gef',
                            g0,
                            GefD = Gefdw,
                            Gefdm = Gefdmw,
                            Gefy = Gefyw,
                            GefI = GefIw,
                            Theta = Thetaw,
                            iS = x@iS,
                            alb = x@alb,
                            modeTrk = x@modeTrk,
                            modeShd = x@modeShd,
                            angGen = x@angGen,
                            struct = x@struct,
                            distances = x@distances
                            )
              result
          }
          )


setMethod('[',
          signature = 'ProdGCPV',
          definition = function(x, i, j, ...){
              gef <- as(x, 'Gef')[i = i, j = j, ...] ##Gef method
              i <- indexI(gef)[1]
              j <- indexI(gef)[length(indexI(gef))]
              d1 <- indexD(x)
              d2 <- indexI(x)
              prodIw <- x@prodI[(d2 >= i & d2 <= j)]
              prodDw <- x@prodD[(d1 >= truncDay(i) & d1 <= truncDay(j))]
              prodDmw <- prodDw[, lapply(.SD/1000, mean, na.rm = TRUE),
                                .SDcols = c('Eac', 'Edc'),
                                by = .(month(Dates), year(Dates))]
              prodDmw$Yf <- prodDw$Yf
              if (x@type == 'prom'){
                  prodDmw[, DayOfMonth := DOM(prodDmw)]
                  prodyw <- prodDmw[, lapply(.SD*DayOfMonth, sum, na.rm = TRUE),
                                    .SDcols = c('Eac', 'Edc', 'Yf'),
                                    by = .(Dates = year)]
                  prodDmw[, DayOfMonth := NULL]
              } else {
                  prodyw <- prodDw[, lapply(.SD/1000, sum, na.rm = TRUE),
                                   .SDcols = c('Eac', 'Edc', 'Yf'),
                                   by = .(Dates = year(Dates))]
              }
              prodDmw[, Dates := paste(month.abb[month], year, sep = '. ')]
              prodDmw[, c('month', 'year') := NULL]
              setcolorder(prodDmw, c('Dates', names(prodDmw)[-length(prodDmw)]))
              result <- new('ProdGCPV',
                            gef,
                            prodD = prodDw,
                            prodDm = prodDmw,
                            prody = prodyw,
                            prodI = prodIw,
                            module = x@module,
                            generator = x@generator,
                            inverter = x@inverter,
                            effSys = x@effSys
                            )
              result
          }
          )

setMethod('[',
          signature = 'ProdPVPS',
          definition = function(x, i, j, ...){
              gef <- as(x, 'Gef')[i = i, j = j, ...] ##Gef method
              i <- indexI(gef)[1]
              j <- indexI(gef)[length(indexI(gef))]
              d1 <- indexD(x)
              d2 <- indexI(x)
              prodIw <- x@prodI[(d2 >= i & d2 <= j)]
              prodDw <- x@prodD[(d1 >= truncDay(i) & d1 <= truncDay(j))]
              prodDmw <- prodDw[, .(Eac = Eac/1000,
                                    Qd = Qd,
                                    Yf = Yf),
                                by = .(month(Dates), year(Dates))]
              if (x@type == 'prom'){
                  prodDmw[, DayOfMonth := DOM(prodDmw)]
                  prodyw <- prodDmw[, lapply(.SD*DayOfMonth, sum, na.rm = TRUE),
                                    .SDcols = c('Eac', 'Qd', 'Yf'),
                                    by = .(Dates = year)]
                  prodDmw[, DayOfMonth := NULL]
              } else {
                  prodyw <- prodDw[, .(Eac = sum(Eac, na.rm = TRUE)/1000,
                                       Qd = sum(Qd, na.rm = TRUE),
                                       Yf = sum(Yf, na.rm = TRUE)),
                                   by = .(Dates = year(Dates))]
              }
              prodDmw[, Dates := paste(month.abb[month], year, sep = '. ')]
              prodDmw[, c('month', 'year') := NULL]
              setcolorder(prodDmw, c('Dates', names(prodDmw)[-length(prodDmw)]))
              result <- new('ProdPVPS',
                            gef,
                            prodD = prodDw,
                            prodDm = prodDmw,
                            prody = prodyw,
                            prodI = prodIw,
                            pump = x@pump,
                            H = x@H,
                            Pg = x@Pg,
                            converter = x@converter,
                            effSys = x@effSys
                            )
              result
          }
          )
