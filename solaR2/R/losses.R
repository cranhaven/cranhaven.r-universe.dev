utils::globalVariables(c('Vmpp', 'Impp', 'Pdc', 'EffI', 'V1'))

setGeneric('losses', function(object){standardGeneric('losses')})

setMethod('losses',
          signature = (object = 'Gef'),
          definition=function(object){
            dat <- as.data.tableY(object, complete = TRUE)
            isShd <- ('Gef0d' %in% names(dat)) ##is there shadows?
            if (isShd) {
              shd <- with(dat, mean(1-Gefd/Gef0d))
              eff <- with(dat, mean(1-Gef0d/Gd))
            } else {
              shd <- 0
              eff <- with(dat, mean(1-Gefd/Gd))
            }
            result <- data.table(Shadows = shd, AoI = eff)
            result <- melt(result, measure.vars = names(result),
                           variable.name = 'id')
          }
          )

setMethod('losses',
          signature = (object = 'ProdGCPV'),
          definition = function(object){
              datY <- as.data.tableY(object, complete = TRUE)
              module0 <- object@module
              module0$CoefVT <- 0 ##No losses with temperature
              Pg <- object@generator$Pg
              datI <- as.data.tableI(object, complete = TRUE)
              if (object@type == 'prom'){
                  YfDC0 <- datI[, P2E(Vmpp*Impp, object@sample),
                                by = .(month(Dates), year(Dates))]
                  YfDC0 <- YfDC0[, V1 := V1/Pg*DOM(YfDC0)]
                  YfDC0 <- sum(YfDC0$V1)
                  YfAC0 <- datI[, P2E(Pdc*EffI, object@sample),
                                by = .(month(Dates), year(Dates))]
                  YfAC0 <- YfAC0[, V1 := V1/Pg*DOM(YfAC0)]
                  YfAC0 <- sum(YfAC0$V1)
              } else {
                  YfDC0 <- datI[, P2E(Vmpp*Impp, object@sample),
                                by = year(Dates)]
                  YfDC0 <- YfDC0[, V1 := V1/Pg]
                  YfDC0 <- sum(YfDC0$V1)
                  YfAC0 <- datI[, P2E(Pdc*EffI, object@sample),
                                by = year(Dates)]
                  YfAC0 <- YfAC0[, V1 := V1/Pg]
                  YfAC0 <- sum(YfAC0$V1)
              }
              gen <- mean(1-YfDC0/datY$Gefd)
              YfDC <- datY$Edc/Pg*1000
              DC <- mean(1-YfDC/YfDC0)
              inv <- mean(1-YfAC0/YfDC)
              AC <- mean(1-datY$Yf/YfAC0)
              result0 <- losses(as(object, 'Gef'))
              result1 <- data.table(Generator = gen,
                                    DC = DC,
                                    Inverter = inv,
                                    AC = AC)
              result1 <- melt(result1, measure.vars = names(result1),
                              variable.name = 'id')
              result <- rbind(result0, result1)
              result
          }
          )

###compareLosses

## compareLosses,ProdGCPV: no visible binding for global variable ‘name’
if(getRversion() >= "2.15.1") globalVariables(c('name'))

setGeneric('compareLosses', signature = '...', function(...){standardGeneric('compareLosses')})

setMethod('compareLosses', 'ProdGCPV',
          definition=function(...){
            dots <- list(...)
            nms0 <- substitute(list(...))
            if (!is.null(names(nms0))){ ##do.call
              nms <- names(nms0[-1])
            } else {
              nms <- as.character(nms0[-1])
            }
            foo <- function(object, label){
              yY <- losses(object)
              yY <- cbind(yY, name=label)
              yY
            }
            cdata <- mapply(FUN=foo, dots, nms, SIMPLIFY=FALSE)
            z <- do.call(rbind, cdata)
            z$id <- ordered(z$id, levels = c('Shadows', 'AoI', 'Generator',
                                             'DC', 'Inverter', 'AC'))
            p <- dotplot(id~value*100, groups = name, data = z,
                         par.settings = solaR.theme, type = 'b',
                         auto.key = list(corner = c(0.95,0.2), cex = 0.7),
                         xlab = 'Losses (%)')
            print(p)
            return(z)
          }
          )
