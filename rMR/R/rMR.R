

#'@export
#'@import graphics
plotRaw <-
    function(data, DO.var.name, time.var.name = "std.time",
             start.time = data$x[1],
             end.time = data$x[length(data$x)], ...){
        
        data$x <- eval(parse(text = paste("data$", time.var.name, sep = "")))
        data <- data[data$x >= start.time & data$x <= end.time,]
        data$y <- eval(parse(text = paste("data$", DO.var.name, sep = "")))
        
        plot(x=data$x, y=data$y, ...)
    }



#'@export
#'@import stats
sumsq <-
    function(x){
        mx<-mean(x)
        sq.dev<-((x-mx)^2)
        sumsquares<-sum(sq.dev)
        return(sumsquares)
    }


#'@export
#'@import stats
tot.rss <-
    function(data, break.pt, xvar, yvar){
        data$x <- eval(parse(text = paste("data$", xvar, sep="")))
        data$y <- eval(parse(text = paste("data$", yvar, sep="")))
        d1 <- data[data$x >= break.pt,]
        m1 <- lm(d1$y~d1$x)
        
        d2 <- data[data$x < break.pt,]
        m2 <- lm(d2$y~d2$x)
        
        trss <- (sumsq(m1$residuals)+sumsq(m2$residuals))
        return(trss)
    }

#'@export

#'@import stats
#'@import biglm
background.resp <-
    function(data, DO.var.name, time.var.name = "std.time",
             start.time, end.time, col.vec = c("black","red"), ...){
        
        orig <- "1970-01-01 00:00:00 UTC"
        
        data$y <- eval(parse(text = paste("data$", DO.var.name, sep = "")))
        data$x <- eval(parse(text = paste("data$", time.var.name, sep = "")))
        
        if(class(start.time) != class(end.time)) {
            stop ("start time and end time must be of same atomic class")
        }
        if(is.character(start.time)==T){
            data <- data[data$x >= as.POSIXct(start.time,
                                              origin=orig) &
                             data$x<= as.POSIXct(end.time,
                                                 origin=orig),]  
        }else if (class(start.time)[1]==("POSIXct") &
                  class(start.time)[2]==("POSIXt")){
            
            data <- data[data$x >= start.time &
                             data$x <= end.time] 
        }
        
        #calculate lm for background rate
        m1 <- biglm(y ~ x, data)
        
        MR1 <- coef(m1)[2]
        
        if (MR1>=0){warning("slope control 1 negative")}
        x<-data$x
        y<-data$y
        
        # Too keep point edges from getting obscured and hazy
        dots<-list(...)
        if(exists("dots$cex")==TRUE){
            cex.val<-0.7
        }else{cex.val<-dots$cex}
        
        if(exists("dots$pch")==TRUE){
            pch.val<-1
        }else{pch.val<-dots$pch}
        
        plot(x, y, type="n", ...)
        points(x=x, y=y, cex=cex.val, pch=pch.val, col=col.vec[1])
        abline(coefficients(m1), col = col.vec[2],...)
        
        return(summary(m1))
    }

#'@export
Barom.Press <-
    function(elevation.m, units = "atm"){
        if(units == "atm") {fact <-1}
        else if(units == "kpa") {
            fact <- 101.325
        }else if(units == "mmHg") {
            fact <- 760
        }else{
            stop("invalid pressure units, must be
                 'atm', 'kpa', or 'mmHg'")
        }
        P<-exp(-9.80665 * 0.0289644 * elevation.m / 
                   (8.31447 * 288.15))*fact
        return(P)
        }


#'@export
#'
DO.saturation <-
    function(DO.mgl, temp.C, elevation.m = NULL,
             bar.press = NULL, bar.units = NULL,
             salinity = 0 , salinity.units = "pp.thou"){
        if(is.null(bar.press) == TRUE){
            DO.sat<- DO.mgl / Eq.Ox.conc(temp.C, elevation.m = elevation.m,
                                         bar.press = NULL, bar.units=NULL,
                                         salinity = salinity,
                                         salinity.units = salinity.units)   
        }else{
            DO.sat<- DO.mgl / Eq.Ox.conc(temp.C, bar.press = bar.press,
                                         bar.units = bar.units,
                                         salinity = salinity,
                                         salinity.units = salinity.units) 
        }
        
        return(DO.sat)
    }

#'@export
DO.unit.convert <-
    function(x, DO.units.in, DO.units.out, 
             bar.units.in, bar.press, temp.C,
             bar.units.out = "mmHg",
             salinity = 0 , salinity.units = "pp.thou"){
        if(bar.units.in == bar.units.out){
            bar.press <- bar.press
        }else if(bar.units.in == "atm"){
            if(bar.units.out == "mmHg"){
                bar.press <- bar.press * 760
            }else if(bar.units.out == "kpa"){
                bar.press <- bar.press * 101.32501
            }else{
                stop("invalid 'bar.units.out' -- must be 'atm', 'mmHg', 'kpa'")
            }
        }else if(bar.units.in == "mmHg"){
            if(bar.units.out == "atm"){
                bar.press <- bar.press / 760
            }else if(bar.units.out == "kpa"){
                bar.press <- bar.press * 101.32501 / 760
            }else{
                stop("invalid 'bar.units.out' -- must be 'atm', 'mmHg', 'kpa'")
            }
        }else if(bar.units.in == "kpa"){
            if(bar.units.out == "atm"){
                bar.press <- bar.press / 101.32501
            }
            if(bar.units.out == "mmHg"){
                bar.press <- bar.press * 760 / 101.32501
            }
        }
        ## bar.press now set
        
        if (DO.units.in == "pct"){
            DO.pct <- x / 100
        }else{
            eq.o2.in <- Eq.Ox.conc(temp.C = temp.C, elevation.m = NULL,
                                   bar.press = bar.press,
                                   bar.units = bar.units.out, 
                                   out.DO.meas = DO.units.in,
                                   salinity = salinity,
                                   salinity.units = salinity.units)
            DO.pct <- x / eq.o2.in
        }
        
        if (DO.units.out == "pct"){
            DO.conc <- DO.pct * 100
        }else{
            eq.o2.out <- Eq.Ox.conc(temp.C, bar.units = bar.units.out,
                                    bar.press = bar.press,
                                    out.DO.meas = DO.units.out,
                                    salinity = salinity,
                                    salinity.units = salinity.units)
            DO.conc <- DO.pct * eq.o2.out
        }
        
        return(DO.conc)
    }

#'@export
Eq.Ox.conc <-
    function(temp.C, elevation.m = NULL,
             bar.press = NULL, bar.units = "mmHg",
             out.DO.meas = "mg/L", salinity = 0,
             salinity.units = "pp.thou"){
        tk <- 273.15 + temp.C
        if(is.null(elevation.m) == FALSE &&
           is.null(bar.press) == FALSE){
            stop("'bar.press' must be NULL if 'elevation.m' is assigned a value. ")
        }
        
        if( out.DO.meas == "PP"){
            
            if(is.null(bar.press) == FALSE && is.null(elevation.m) == TRUE){
                bar.press <- bar.press
            } else if(is.null(bar.press) == TRUE &&
                      is.null(elevation.m) == FALSE){
                bar.press <- Barom.Press (elevation.m,
                                          units = bar.units)
            }else{
                stop("EITHER 'elevation.m' or 'bar.press' must be assigned
                     a value. The other argument must be NULL.")
            }
            if(bar.units == "atm"){
                bar.press.atm <- bar.press
            }else if(bar.units == "kpa"){
                bar.press.atm <- bar.press / 101.32501
            }else if(bar.units == "mmHg"){
                bar.press.atm <- bar.press / 760
            }else{
                stop("invalid pressure units, must be
                     'atm', 'kpa', or 'mmHg'")
            }
            DO <- bar.press*0.20946
            
            
            }else if (out.DO.meas == "mg/L"){
                
                if(is.null(bar.press) == FALSE &&
                   is.null(elevation.m) == TRUE){
                    if(bar.units == "atm"){
                        bar.press.atm <- bar.press
                    }else if(bar.units == "kpa"){
                        bar.press.atm <- bar.press / 101.32501
                    }else if(bar.units == "mmHg"){
                        bar.press.atm <- bar.press / 760
                    }else{
                        stop("invalid pressure units, must be
                             'atm', 'kpa', or 'mmHg'")
                    }
                    } else if(is.null(bar.press) == TRUE &&
                              is.null(elevation.m) == FALSE){
                        bar.press.atm <- Barom.Press (elevation.m, units = "atm")
                    }else{
                        stop("EITHER 'elevation.m' or 'barom.press' must be assigned
                             a value. The other argument must be NULL.")
                    }
                ## Benson and Krause eqns, USGS 2011 ##
                
                A1 <- -139.34411
                A2 <- 1.575701e5
                A3 <- 6.642308e7
                A4 <- 1.243800e10
                A5 <- 8.621949e11
                
                DO <- exp(A1 + (A2/tk) -
                              (A3/(tk^2)) +
                              (A4/(tk^3)) -
                              (A5/(tk^4)))
                
                    }else{
                        stop("must specify 'out.DO.meas' as 'mg/L' or 'PP'")
                    }
        # salinity factor #
        if(salinity.units == "uS"){
            sal <- salinity*5.572e-4 + (salinity^2)*2.02e-9 
        }else if(salinity.units == "pp.thou"){
            sal <- salinity
        }else{
            stop("salinity.units must be 'uS' or 'pp.thou'")
        }
        
        Fs <- exp(-sal*(0.017674 - (10.754/tk) + (2140.7/(tk^2))))
        
        ## Pressure factor determination ##
        theta <- 0.000975 - 
            temp.C*1.426e-5 +
            (temp.C^2)*6.436e-8
        
        u <- exp(11.8571 - (3840.70/tk) - (216961/(tk^2)))
        
        # pressure factor #
        
        Fp <- ((bar.press.atm - u)*(1-(theta*bar.press.atm))) /
            ((1-u)*(1-theta))
        
        Cp <- DO * Fs * Fp
        
        return(Cp)
            }



#'@export

#'@import stats
#'@import graphics
#'@import biglm
get.pcrit <-
    function(data, DO.var.name, MR.var.name = NULL, Pcrit.below,
             time.interval, time.var = NULL,
             start.time, stop.time, time.units = "sec",
             Pcrit.type = "both", syst.vol = NULL,
             col.vec = c("black", "gray60", "red", "blue"),...){
        
        data$DO<- eval(parse(text = paste("data$", DO.var.name,
                                          sep = "")))
        
        
        ## set time denominator based on specified time.units ##
        
        if(is.null(time.var)==FALSE){
            if(time.units == "sec"){
                t.denom <- 1
            }else if(time.units == "min"){
                t.denom <- 60
            }else if(time.units == "hr"){
                t.denom <- 3600
            }
            data$time <- eval(parse(text =
                                        paste("data$", time.var, sep = "")))
            
            data <- data[data$time >= start.time
                         & data$time <= stop.time,]
            
            data$time <- as.numeric(data$time) - min(as.numeric(data$time)) 
        }
        
        
        
        if(any(is.na(data$DO)==TRUE)){
            warning("DO variable contains missing values")
        }
        
        
        if(is.null(MR.var.name) == TRUE){
            
            if(is.null(time.interval) == TRUE){
                stop("if using DO to calculate rates & Pcrit,
                     'time.interval' must be specified")
            }
            
            calc.MRs <- data.frame()
            
            i<-0
            
            while(i + time.interval < length(data$time)){
                the.interval <- data[data$time >= i &
                                         data$time < i + time.interval,]
                m <- lm(DO ~ time, data = the.interval)
                MR <- m$coefficients[2]*(-1)
                DO <- mean(the.interval$DO, na.rm = TRUE)
                time <- round(the.interval$time[1])
                df.row <- t(c(DO, MR))
                calc.MRs <- rbind(calc.MRs, df.row)
                i <- i + time.interval
                
            }
            
            
            data <- calc.MRs
            names(data) <- c("DO", "MR")
            data$MR <- data$MR * t.denom
            
            }else if(is.null(MR.var.name) == FALSE){
                data$MR <- eval(parse(text = paste("data$", MR.var.name,
                                                   sep = "")))
            }
        
        if(is.null(syst.vol) == FALSE){
            data$MR.vol.adj <- data$MR*syst.vol
        }
        
        
        minimum.DO <- 
            as.numeric(min(data$DO <= Pcrit.below, na.rm=TRUE))
        
        zone.of.interest <- data[data$DO <= Pcrit.below,]
        zone.of.interest <- zone.of.interest[order(zone.of.interest$DO, 
                                                   decreasing=TRUE),]
        
        RSS.table<-data.frame()
        for(w in 2:length(zone.of.interest[,1])-1){
            if(zone.of.interest$DO[w] != minimum.DO){
                break.point <- zone.of.interest$DO[w]
                if(is.null(syst.vol) == FALSE){
                    yv <- "MR.vol.adj"
                }else{yv <- "MR"}
                RSS <- tot.rss(data = data,
                               break.pt = break.point,
                               xvar = "DO", yvar = yv)
                RSS.row <- cbind(RSS, zone.of.interest$DO[w])
                RSS.table <- rbind(RSS.table,RSS.row)    
            }
        }
        
        DO.idx <- RSS.table[RSS.table$RSS == min(RSS.table$RSS), 2]
        idx.rname <- row.names(RSS.table[RSS.table[,2] == DO.idx,])
        DO.idx.low <- RSS.table[row.names(RSS.table) == 
                                    (as.numeric(idx.rname)-1), 2]
        
        midpoint.approx <- mean(c(DO.idx, DO.idx.low))
        
        
        ##creating linear models##
        dat1 <- data[data$DO > DO.idx,]
        dat2 <- data[data$DO <= DO.idx,]
        if(is.null(syst.vol) == FALSE){
            mod.1 <- lm(MR.vol.adj~DO, data=dat1)
            mod.2 <- lm(MR.vol.adj~DO, data=dat2)   
        }else{
            mod.1 <- lm(MR~DO, data=dat1)
            mod.2 <- lm(MR~DO, data=dat2)
        }
        
        
        sm1 <- summary(mod.1)
        sm2 <- summary(mod.2)
        
        adjr2.pre <- sm1$adj.r.squared
        adjr2.post <- sm2$adj.r.squared
        
        ## Plot: line intersect##
        
        plot(MR~DO, data, type= "n", ...)
        
        intersect<-(mod.2$coefficients[1] - mod.1$coefficients[1]) /
            (mod.1$coefficients[2] - mod.2$coefficients[2])
        names(intersect)<-NULL
        if(is.na(mod.2$coefficients[2])==T){
            abline(mod.1$coefficients[1], 0)
        }else{abline(coef = mod.1$coefficients, col = col.vec[2], ...)}
        abline(coef = mod.2$coefficients, col = col.vec[2], ...)
        
        
        if (Pcrit.type == "lm" | Pcrit.type == "both"){
            abline(v = intersect, col = col.vec[3], lty=2, ...)            
        }
        if (Pcrit.type == "midpoint" | Pcrit.type == "both"){
            abline(v = midpoint.approx, col = col.vec[4], lty=3, ...)            
        }
        # Too keep point edges from getting obscured and hazy
        dots<-list(...)
        if(exists("dots$cex")==TRUE){
            cex.val<-0.7
        }else{cex.val<-dots$cex}
        
        if(exists("dots$pch")==TRUE){
            pch.val<-1
        }else{pch.val<-dots$pch}
        if(is.null(syst.vol) == FALSE){
            points(x = c(dat1$DO, dat2$DO),
                   y = c(dat1$MR.vol.adj, dat2$MR.vol.adj), 
                   cex = cex.val, pch = pch.val, col = col.vec[1])
        }else{
            points(x = c(dat1$DO, dat2$DO), y = c(dat1$MR, dat2$MR), 
                   cex = cex.val, pch = pch.val, col = col.vec[1])  
        }
        
        
        
        dat.pre<-data[data$DO>=(2*intersect),]
        
        P.crit<-as.data.frame(cbind(intersect, midpoint.approx,
                                    adjr2.pre, adjr2.post))
        DATA <- as.data.frame(data)
        names(P.crit)<-c("Pcrit.lm", "Pcrit.midpoint",
                         "Adj.r2.above", "Adj.r2.below")
        
        above.Pc <- list(mod.1)
        names(above.Pc) <- "above"
        below.Pc <- list(mod.2)
        names(below.Pc) <- "below"
        
        Pc <- list(P.crit, DATA) 
        names(Pc) <- c("SummaryCrit", "DATA")
        Pcm <- c(Pc, above.Pc, below.Pc)
        return(Pcm)
        
    }

#'@export

#'@import stats
#'@import graphics
#'@import biglm
MR.loops <-
    function(data, DO.var.name, time.var.name = "std.time",
             in.DO.meas = "mg/L", out.DO.meas = "mg/L",
             start.idx, stop.idx, syst.vol = 1,
             background.consumption = 0,
             background.indices = NULL,
             temp.C, elevation.m = NULL,
             bar.press = NULL, bar.units = "atm",
             PP.units, time.units = "sec",
             col.vec = c("black", "red"),...){
        ## format the time vectors into POSIX ##
        orig = "1970-01-01 00:00:00 UTC"
        start.idx <- as.POSIXct((start.idx), origin = orig)
        stop.idx <- as.POSIXct((stop.idx), origin = orig)
        
        ## make sure num of start and stop indices agree ##
        if (length(start.idx) != length(stop.idx)){
            stop ("number of start times not equal
                  to number of stop times")
        }
        
        
        ## set time denominator based on specified time.units ##
        if(time.units == "sec"){
            t.denom <- 1
        }else if(time.units == "min"){
            t.denom <- 60
        }else if(time.units == "hr"){
            t.denom <- 3600
        }
        
        
        ## set response variable ##
        data$y <- eval(parse(text = paste("data$", DO.var.name, sep = "")))
        ## set time variable ##
        data$x <- eval(parse(text = paste("data$", time.var.name, sep = "")))
        
        ## set background DO consumption rate ##
        if(is.null(background.indices) == TRUE){
            bgd.slope.int <- background.consumption
            bgd.slope.slope <- 0
            
        }else if(is.null(background.indices) == FALSE) {
            if(length(background.indices) < 2){
                stop("background.indices must be NULL or have a length >= 2")
            }else{
                ## if there are multiple background calibrations ##
                bgd.mod <- lm(background.consumption ~ 
                                  as.POSIXct(background.indices))
                bgd.slope.int <- bgd.mod$coefficients[1]
                bgd.slope.slope <- bgd.mod$coefficients[2]
            }
            
        }
        
        if(is.null(bar.press) == FALSE &&
           is.null(elevation.m) == FALSE){
            stop("Either 'bar.press' or 'elevation.m' should be NULL")
        }
        
        ## barometric pressure ##
        if(is.null(bar.press) == FALSE){
            if(is.character(bar.press) == TRUE){
                bar.press <- eval(parse(
                    text = paste("data$",
                                 bar.press, sep = "")))
            }else if(is.numeric(bar.press) == TRUE){
                bar.press <- bar.press
            }else{
                stop("'bar.press' must be 'NULL', numeric, or
                     the col.name for barometric pressure")
            }
            }
        
        
        ## Temperature ##
        if (is.character(temp.C) == TRUE){
            temp.C <- eval(parse(
                text = paste("data$", temp.C, sep = "")))
        }else if(is.numeric(temp.C) == TRUE){
            temp.C <- temp.C
        }else{
            stop("invalid temp.C argument")
        }
        
        # DO sat conversions #
        if (in.DO.meas == "pct"){
            data$y <- (data$y /100) * 
                Eq.Ox.conc(temp.C = temp.C,
                           elevation.m = elevation.m,
                           bar.press = bar.press,
                           bar.units = bar.units,
                           out.DO.meas = out.DO.meas)
        }else if (in.DO.meas == "PP"){
            fraction <- data$y / 
                Eq.Ox.conc(temp.C = temp.C,
                           elevation.m = elevation.m,
                           bar.press = bar.press,
                           bar.units = bar.units,
                           out.DO.meas = "PP")
            data$y <- fraction *
                Eq.Ox.conc(temp.C = temp.C,
                           elevation.m = elevation.m,
                           bar.press = bar.press,
                           bar.units = bar.units,
                           out.DO.meas = out.DO.meas)
        }else if(in.DO.meas == "mg/L"){
            data$y <- data$y
        }else{
            stop("invalid 'in.DO.meas' argument:
                 must be 'pct', 'PP', or 'mg/L' ")
        }
        
        if(out.DO.meas == "mg/L"){
            data$y <- syst.vol * data$y
            # Now data$y in units of mg, not mg/L #
        }else if(out.DO.meas == "PP"){
            # converting MR to PP units #
            data$y <- DO.unit.convert(x = data$y,
                                      DO.units.in = "mg/L",
                                      DO.units.out = "PP",
                                      bar.units.in = "atm",
                                      bar.units.out = PP.units)
        }else if(out.DO.meas == "pct"){
            data$y <- fraction * 100
        }
        
        ## the following loop adds adj.y as a variable.     ##
        ## it adjusts DO level by iincorporating background ##
        ## respiration rate.                                ##
        
        data$adj.y <- rep(0, length(data[,1]))
        tab.wid <- length(data[1,])
        data.new <- NULL
        
        for(i in 1:length(start.idx)){
            
            dsn <- data[data$x >= as.POSIXct(start.idx[i]) &
                            data$x <= as.POSIXct(stop.idx[i]), ]
            dsn$adj.y <- (dsn$y- (as.numeric(dsn$x) -
                                      as.numeric(start.idx[i]))*
                              ((as.numeric(dsn$x) * bgd.slope.slope) +
                                   bgd.slope.int))
            
            data.new <- rbind(data.new, dsn)
        }
        
        data <- data.new[(data.new$x >= start.idx[1] - 600) &
                             data.new$x <= (tail(stop.idx,1) + 600),]
        x<-data$x
        y<-data$y
        
        plot(x, y, type="n",...)
        
        name.num<-as.character(c(1:length(start.idx)))
        ms<-list()
        MR.summary<-data.frame()
        
        dots<-list(...)
        if(exists("dots$cex")==TRUE){
            cex.val<-0.7
        }else{cex.val<-dots$cex}
        
        if(exists("dots$pch")==TRUE){
            pch.val<-1
        }else{pch.val<-dots$pch}
        
        for(i in 1:length(start.idx)){
            dat <- data.new[data.new$x >= start.idx[i]
                            & data.new$x <= stop.idx[i],]
            
            
            mk <- biglm(adj.y ~ x, data=dat)
            ms[[i]] <- mk
            
            points(dat$x, dat$adj.y, col = col.vec[1],
                   pch = pch.val, cex = cex.val)
            names(ms[[i]])<-paste(names(ms[[i]]), name.num[i], sep=".")
            abline(coef(ms[[i]]),
                   col = col.vec[2], ...)
            
            MR <- coef(mk)[2]*-1
            sds <- summary(mk)$mat[2,4]*sqrt(length(dat[,1]))
            rsquare <- summary(mk)$rsq
            mrrow <- t(c(MR, sds, rsquare))
            MR.summary <- rbind(MR.summary,mrrow)
        }
        
        names(MR.summary) <- c("MR", "sd.slope", "r.square")
        MR.summary[,c(1,2)] <- MR.summary[,c(1,2)] * t.denom
        
        
        ofthejedi <- list(MR.summary, ms)
        names(ofthejedi) <- c("MR.summary", "lm.details")
        return(ofthejedi)
        }

#'@export
#'@import utils
get.witrox.data <-
    function(data.name, lines.skip, delimit="tab", choose.names = F,
             chosen.names = NULL,
             format){
        if(delimit == "tab"){
            separate = "\t"
        }else if(delimit == "space"){
            separate = ""
        }else if(delimit == "comma"){
            separate = ","
        }
        
        if(choose.names==F){
            d<-read.table(data.name, sep=separate, skip=lines.skip,
                          header =T,check.names=F)
            invalid.names<-colnames(d)
            valid.names<-make.names(colnames(d))
            var.names<-NULL
            for(i in 1:length(invalid.names)){
                if(invalid.names[i] == valid.names[i]){
                    var.names[i] <- valid.names[i]
                }else{
                    split.name.period <- as.vector(
                        strsplit(valid.names[i], fixed = T,
                                 split = ".")[[1]])
                    split.name.period <- paste(split.name.period, sep="",
                                               collapse="")
                    
                }
            }
        }else if(choose.names==T){
            d<-read.table(data.name, sep = separate, skip = lines.skip,
                          header = F)
            var.names<-chosen.names
        }
        
        colnames(d) <- var.names
        d[,1]<-as.character(d[,1]) 
        d$std.time <- as.POSIXct(strptime(d[,1], format = format),
                                 origin = "1970-01-01 00:00:00 UTC")
        return(d)
        
    }


