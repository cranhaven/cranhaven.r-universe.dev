## voc, isc, vmpp, impp : *cell* values
## Voc, Isc, Vmpp, Impp: *module/generator* values

## Compute Current - Voltage characteristic of a solar *cell* with Gef
## and Ta
iv <- function(vocn, iscn, vmn, imn,
               TONC, CoefVT = 2.3e-3,
               Ta, Gef,
               vmin = NULL, vmax = NULL)
{
    ##Cell Constants
    Gstc <- 1000
    Ct <- (TONC - 20) / 800
    Vtn <- 0.025 * (273 + 25) / 300
    m <- 1.3
    
    ##Cell temperature
    Tc <- Ta + Ct * Gef
    Vt <- 0.025 * (Tc + 273)/300
    
    ## Series resistance
    Rs <- (vocn - vmn + m * Vtn * log(1 - imn/iscn)) / imn

    ## Voc and Isc at ambient conditions
    voc <- vocn - CoefVT * (Tc - 25)
    isc <- iscn * Gef/Gstc

    ## Ruiz method for computing voltage and current characteristic of a *cell*
    rs <- Rs * isc/voc
    koc <- voc/(m * Vt)

    ## Maximum Power Point
    Dm0 <- (koc - 1)/(koc - log(koc))
    Dm <- Dm0 + 2 * rs * Dm0^2
    
    impp <- isc * (1 - Dm/koc)
    vmpp <- voc * (1 - log(koc/Dm)/koc - rs * (1 - Dm/koc))

    vdc <- vmpp
    idc <- impp

    ## When the MPP is below/above the inverter voltage limits, it
    ## sets the voltage point at the corresponding limit.


    ## Auxiliary functions for computing the current at a defined
    ## voltage.
    ilimit <- function(v, koc, rs) 
    {
        if (is.na(koc)) 
            result <- NA
        else
        {
            ## The IV characteristic is an implicit equation. The starting
            ## point is the voltage of the cell (imposed by the inverter
            ## limit). 
            
            izero <- function(i , v, koc, rs)
            {
                vp <- v + i * rs
                Is <- 1/(1 - exp(-koc * (1 - rs)))
                result <- i - (1 - Is * (exp(-koc * (1 - vp)) - exp(-koc * (1 - rs))))
            }

            result <- uniroot(f = izero,
                              interval = c(0,1),
                              v = v,
                              koc = koc,
                              rs = rs)$root
        }
        result
    }
    ## Inverter minimum voltage
    if (!is.null(vmin))
    {
        if (any(vmpp < vmin, na.rm = TRUE))
        {
            indMIN <- which(vmpp < vmin)
            imin <- sapply(indMIN, function(i)
            {
                vocMIN <- voc[i]
                kocMIN <- koc[i]
                rsMIN <- rs[i]
                vmin <- vmin/vocMIN
                ##v debe estar entre 0 y 1
                vmin[vmin < 0] <- 0
                vmin[vmin > 1] <- 1
                ilimit(vmin, kocMIN, rsMIN)
            })
            iscMIN <- isc[indMIN]
            idc[indMIN] <- imin * iscMIN
            vdc[indMIN] <- vmin
            warning('Minimum MPP voltage of the inverter has been reached')}
    }

    if (!is.null(vmax))
    {
        if (any(vmpp > vmax, na.rm = TRUE))
        {
            indMAX <- which(vmpp > vmax)
            imax <- sapply(indMAX, function(i)
            {
                vocMAX <- voc[i]
                kocMAX <- koc[i]
                rsMAX <- rs[i]
                vmax <- vmax / vocMAX
                ##v debe estar entre 0 y 1
                vmax[vmax < 0] <- 0
                vmax[vmax > 1] <- 1
                ilimit(vmax, kocMAX, rsMAX)
            })
            iscMAX <- isc[indMAX]
            idc[indMAX] <- imax * iscMAX
            vdc[indMAX] <- vmax
            warning('Maximum MPP voltage of the inverter has been reached')
        }
    }
    data.table(Ta, Tc, Gef, voc, isc, vmpp, impp, vdc, idc)
}

fProd <- function(inclin, 
                  module = list(), 
                  generator = list(), 
                  inverter = list(),
                  effSys = list()
                  )
{
    
    stopifnot(is.list(module),
              is.list(generator),
              is.list(inverter),
              is.list(effSys)
              )
    ## Extract data from objects
    if (class(inclin)[1]=='Gef') {
        indInclin <- indexI(inclin)
        gefI <- as.data.tableI(inclin, complete = TRUE)
        Gef <- gefI$Gef
        Ta <- gefI$Ta
    } else {
        Gef <- inclin$Gef
        Ta <- inclin$Ta
    }

    ## Module, generator, and inverter parameters
    module.default <- list(Vocn = 51.91,
                           Iscn = 14.07,
                           Vmn = 43.76,
                           Imn = 13.03,
                           Ncs = 24,
                           Ncp = 6,
                           CoefVT = 0.0049,
                           TONC = 45)
    module <- modifyList(module.default, module)
    ## Make these parameters visible because they will be used often.
    Ncs <- module$Ncs
    Ncp <- module$Ncp
    
    generator.default <- list(Nms = 22,
                              Nmp = 130)
    generator <- modifyList(generator.default, generator)
    generator$Pg <- (module$Vmn * generator$Nms) *
        (module$Imn * generator$Nmp)
    Nms <- generator$Nms
    Nmp <- generator$Nmp

    inverter.default <- list(Ki = c(0.002, 0.005, 0.008),
                             Pinv = 1.5e6,
                             Vmin = 822,
                             Vmax = 1300,
                             Gumb = 20)
    inverter <- modifyList(inverter.default, inverter)
    Pinv <- inverter$Pinv
    
    effSys.default <- list(ModQual = 3,
                           ModDisp = 2,
                           OhmDC = 1.5,
                           OhmAC = 1.5,
                           MPP = 1,
                           TrafoMT = 1,
                           Disp = 0.5)
    effSys <- modifyList(effSys.default, effSys)

    ## Solar Cell i-v
    vocn <- with(module, Vocn / Ncs)
    iscn <- with(module, Iscn/ Ncp)
    vmn <- with(module, Vmn / Ncs)
    imn <- with(module, Imn / Ncp)
    vmin <- with(inverter, Vmin / (Ncs * Nms))
    vmax <- with(inverter, Vmax / (Ncs * Nms))
    
    cell <- iv(vocn, iscn,
               vmn, imn,
               module$TONC, module$CoefVT,
               Ta, Gef,
               vmin, vmax)
    
    ## Generator voltage and current
    Idc <- Nmp * Ncp * cell$idc
    Isc <- Nmp * Ncp * cell$isc
    Impp <- Nmp * Ncp * cell$impp
    Vdc <- Nms * Ncs * cell$vdc
    Voc <- Nms * Ncs * cell$voc
    Vmpp <- Nms * Ncs * cell$vmpp
    
    ##DC power (normalization with nominal power of inverter)
    ##including losses
    PdcN <- with(effSys, (Idc * Vdc) / Pinv *
                         (1 - ModQual / 100) *
                         (1 - ModDisp / 100) *
                         (1 - MPP / 100) *
                         (1 - OhmDC / 100)
                 ) 

    ##Normalized AC power to the inverter
    Ki <- inverter$Ki
    if (is.matrix(Ki)) { #Ki is a matrix of nine coefficients-->dependence with tension
        VP <- cbind(Vdc, PdcN)
        PacN <- apply(VP, 1, solvePac, Ki)
    } else { #Ki is a vector of three coefficients-->without dependence on voltage
        A <- Ki[3]
        B <- Ki[2] + 1
        C <- Ki[1] - (PdcN)
        PacN <- (-B + sqrt(B^2 - 4 * A * C))/(2 * A)
    }
    EffI <- PacN / PdcN
    pacNeg <- PacN <= 0
    PacN[pacNeg] <- PdcN[pacNeg] <- EffI[pacNeg] <- 0

    
    ##AC and DC power without normalization
    Pac <- with(effSys, PacN * Pinv *
                        (Gef > inverter$Gumb) *
                        (1 - OhmAC / 100) *
                        (1 - TrafoMT / 100) *
                        (1 - Disp / 100))
    Pdc <- PdcN * Pinv * (Pac > 0)
    
    
    ## Result
    resProd <- data.table(Tc = cell$Tc,
                         Voc, Isc,
                         Vmpp, Impp,
                         Vdc, Idc,
                         Pac, Pdc,
                         EffI)
    if (class(inclin)[1] %in% 'Gef'){
        result <- resProd[, .SD,
                          by = .(Dates = indInclin)]
        attr(result, 'generator') <- generator
        attr(result, 'module') <- module
        attr(result, 'inverter') <- inverter
        attr(result, 'effSys') <- effSys
        return(result)
    } else {
        result <- cbind(inclin, resProd)
        return(result)
    }
}




