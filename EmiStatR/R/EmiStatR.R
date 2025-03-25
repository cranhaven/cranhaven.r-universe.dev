# model EmiStatR version 1.2.3.0
# author: J.A. Torres-Matallana, K. Klepiszewski, U. Leopold, G.B.M. Heuvelink
# organization: Luxembourg Institute of Science and Technology (LIST)
#               Wagenigen university and Research Centre (WUR) 
# date: 11.02.2015 - 13.09.2021

# ns    : name of the structure
# 
# 1.    catchment data
# id    : identification number [-]
# ns    : name of the structure [-]
# nm    : name of the municipality [-]
# nc    : name of the catchment [-]
# numc  : number of the catchment [-]
# use   : use of the soil [-]
# Atotal  : total area [ha]
# Aimp  : reduced area - impervious area [ha]
# tfS   : time flow  structure [min]
# af    : reduction factor [-]
# pe    : population equivalent [PE]
# 
# 2.    Structure data
# Qd    : throttled outflow [l/s]:
# V     : volume [m3]
# te    : emptying time [min]
# 
# 3. Dry weather flow calculation
# Qs24  : dry weather flow [l/s]
# Qf  : infiltration flow [l/s] (Qf24)
# Qt24  : total dry weather flow [l/s]
# 
# Mean dry weather pollutants concentration
# CCOD   : Mean dry weather COD concentration [mg/l]
# CNH4   : Mean dry weather NH4 concentration [mg/l]
#
# table "Berechungen" (calculations)
# combined sewer flow, cs
# dt         : delta time [min]
# V_r_i      : rain part [m3]
# V_dw_i     : dry weather part [m3]
# cs_mr_i    : mixing ratio [-]
# overflow data, o
# o_cfyn_i   : is chamber filling up, yes=1/no=0
# V_Tank     : tank filling [m3]        (o_tf_i)
# V_Ov_i     : overflow volume [m3]     (o_ov_i)
# B_COD_Ov_i : overflow COD load [kg]   (o_COD_i)
# B_NH4_Ov_i : overflow NH4 load [kg]   (o_TKN_i)
# C_COD_Ov_i : overflow CCOD [mg/l]	    (o_CCOD_i)
# C_NH4_Ov_i : overflow CNH4	[mg/l]    (o_CTKN_i)
# d_Ov_i      : overflow duration [min]  (o_d_i)
# f_Ov_i      : overflow frequency [occurrence] (o_f_i)
#
# table overflow data (summary)
# p           : period [day]
# d_Ov        : overflow duration (CSO duration) [min/year] (o_d)
# f_Ov        : CSO frequency [occurrence]                  (o_f)
# V_Ov        : volume [m3/year]                            (o_ov)
# Q_Ov        : flow in [l/s]                               (o_of)
# B_COD_Ov    : COD load [kg/year]                          (o_COD)
# o_COD_av    : average COD concentration [mg/l]
# C_COD_Ov_max: maximum COD concentration [mg/l]            (o_CCOD_max)
# B_NH4_Ov    : NH4 load [kg/year]                          (o_TKN)
# C_NH4_Ov_av : average NH4 concentration [mg/l]            (o_CTKN_av)
# C_NH4_Ov_max: maximum NH4 concentration [mg/l]            (o_CTKN_max)

############### not implemented yet #############################################
# identification of Overflow events and their inflow and effluent volumes, io
# io_fyn: fill/overflow phase, yes=1/no=0
# io_eyn: emptying/overflow phase, yes=1/no=0
# io_iv : inflow volume [m3]
# io_ev : effluent volume [m3]
# Inflow and Outflow volume with  tank filled, but without an overflow, iov
# iov_i : inflow volume [m3]
# iov_o : outflow volume [m3]

#================================================================================
# spatial <- 0 # 1 = spatial input, 0 = non-spatial input
# zero <- 10^(-5) # aproximation to zero value
# mc   <- 0 # 0= no MC runs; 1 = MC runs (internal variable)

#================================================================================
setGeneric("EmiStatR", function(x) standardGeneric("EmiStatR"))

setMethod("EmiStatR", signature = "input", 
          
          function(x){
            ##===============================================================================================================================
            # library(EmiStatR)
            # data(Esch_Sure2010)
            # E1 <- list(id = 1, ns = "Goesdorf", nm = "Goesdorf", nc = "Obersauer", numc = NA,
            #            use = "Residencial/Industrial", Atotal = 16.5, Aimp = 7.6, tfS = 10,
            #            pe = 611, Qd = 9, V = 190)
            # x <- input(spatial = 0, zero = 1e-5, folder = system.file("shiny", package = "EmiStatR"),
            #            folderOutput = system.file("shiny", package = "EmiStatR"), cores = 4,
            #            ww = list(qs = 150, CODs = 50, NH4s = 1.4), inf = list(qf= 0.05, CODf = 0, NH4f =0),
            #            rw = list(CODr = 80, NH4r = 0, stat = "Dahl"), P1 = Esch_Sure2010,
            #            st = list(E1=E1),
            #            pe.daily.file = pe_factor, # after loading data(pe_factor)
            #            pe.weekly     = list(mon=1, tue=.83, wed=.83, thu=.83, fri=1, sat=1.25, sun=1.25),
            #            pe.seasonal   = list(jan=.79, feb=.79, mar=.79, apr=1.15, may=1.15, jun=1.15,
            #                                 jul=1.15, aug=1.15, sep=1.15, oct=1.15, nov=.79, dec=.79),
            #            qs.daily.file = qs_factor, # after loading data(qs_factor)
            #            qs.weekly     = list(mon=1, tue=1, wed=1, thu=1, fri=1, sat=1, sun=1),
            #            qs.seasonal   = list(jan=1, feb=1, mar=1, apr=1, may=1, jun=1,
            #                                 jul=1, aug=1, sep=1, oct=1, nov=1, dec=1),
            #            export = 1)
            ##===============================================================================================================================
            spatial <- slot(x, "spatial")
            zero <- slot(x, "zero")
            # mc <- slot(x, "mc")
            folder <- slot(x, "folder")
            #folderOutput <- slot(x, "folderOutput")
            cores <- slot(x, "cores")
            ww <- slot(x, "ww")
            inf <- slot(x, "inf")
            rw <- slot(x, "rw")
            P1 <- slot(x, "P1")
            st <- slot(x, "st")
            pe.ts.file <- slot(x, "pe.ts.file")
            pe.daily.file  <- slot(x, "pe.daily.file")
            pe.weekly <- slot(x, "pe.weekly")
            pe.seasonal <- slot(x, "pe.seasonal")
            qs.ts.file <- slot(x, "qs.ts.file")
            qs.daily.file  <- slot(x, "qs.daily.file")
            qs.weekly <- slot(x, "qs.weekly")
            qs.seasonal <- slot(x, "qs.seasonal")
            export <- slot(x, "export")
            
            #================================================================================
            ## checking if P or Runoff_Volume as input
            if(length(P1)==0){Runoff_Volume <- FALSE
            }else if(colnames(P1)[2] == "Runoff_Volume" | colnames(P1)[2] == "runoff_volume"){
              Runoff_Volume <- TRUE
            }else Runoff_Volume <- FALSE
              
            #================================================================================
            #================================================================================
            dir.current <- getwd()
            #================================================================================
            mc <- 0
            if (mc == 0){  # no MC runs
              ## load variables
              # dir.shiny <- folder
              # dir.input <- paste(dir.shiny, "/EmiStatR_input/input", sep="")
              # setwd(dir.input)
              
              e1 <- new.env()
              
              ifelse(length(ww) == 0, 
                     {
                       data("ww", envir = e1)
                       ww <- get("ww", envir = e1)
                       },
                     0)
              
              ifelse(length(inf) == 0,
                     {data("inf", envir=e1)
                       inf <- get("inf", envir = e1)
                       },
                     0)
              
              ifelse(length(rw) == 0,
                     {data("rw", envir=e1)
                       rw <- get("rw", envir=e1)
                       },
                     0)
              
              # load precipitation data
              ifelse(length(P1) == 0, {
                data("P1", envir=e1)
                P1 <- get("P1", envir=e1)
                }, 0)
              
              # load structure characteristics
              #     ifelse(length(st) == 0,
              #            {dir.inputCSO <- paste(dir.shiny, "/EmiStatR_inputCSO/inputCSO", sep="")
              #            setwd(dir.inputCSO)
              #            data.sources = list.files(pattern="*.RData")
              #            sapply(data.sources, load, envir = e1)},
              #            {for(i in 1:length(st)){
              #              assign(names(st)[i], st[[i]])
              #            }} # rm("E1", "E2")
              #            )
              
              if (length(st) == 0){
                # dir.inputCSO <- paste(dir.shiny, "/EmiStatR_inputCSO/inputCSO", sep="")
                # setwd(dir.inputCSO)
                # data.sources = list.files(pattern="*.RData")
                # sapply(data.sources, load, envir = e1)} else {
                data(E1, envir = e1)
                data(E2, envir = e1)
                data(E3, envir = e1)} else {
                  for(i in 1:length(st)){
                    assign(names(st)[i], st[[i]])
                  }} # rm("E1", "E2", "E3")
              
            }
            
            # E1 # Goesdorf 
            # E2 # Kaundorf
            # E3 # Nocher-Route
            
            # defining structures to read
            
            ifelse(length(st)==0,
                   lista <- ls(pattern="^[E]{1}[0-9]", envir = e1),
                   lista <- ls(pattern="^[E]{1}[0-9]"))
            
            #a<-as.name(paste(lista[1],"$nm", sep=""))
            
            # a<-read.csv("test1.csv", header=TRUE, sep="") 
            # class(a)
            
            ncol <- length(lista)+1
            out <- matrix(data=NA, nrow=35+2+2+1, ncol=ncol)
            
            #my.array <- array(1:24, dim=c(3,4,3), dimnames=c(lista))
            
            out[1:25,1] <- c("Input data for structure", "Name of the structure, ns [-]",
                             "Data of the Catchment", "Municipality, nm [-]",
                             "Name of the catchment, nc [-]", "Catchment number, numc [-]",
                             "Use [-]", "Total area, Atotal [ha]", 
                             "Impervious area, Aimp [ha]", "Flow time structure, tfS [min]", 
                             "", #"Reduction factor, af [-]:", 
                             "Mean water consumption, qs [l/(Pe*d)] ",
                             "Mean population equivalent, pe [PE]:", 
                             "Mean dry weather flow", "Mean dry weather flow, Qs24 [l/s]", 
                             "Infiltration flow, Qf [l/s]", "Mean total dry weather flow, Qt24 [l/s]", 
                             "Mean dry weather pollutants concentration", "Mean COD concentration, COD [mg/l]", 
                             "Mean NH4 concentration, NH4 [mg/l]", "Structure data", 
                             "Throttled outflow, Qd [l/s]", "Volume, V [m3]",
                             "Emptying time, te [min]", "Overflow data summary results")
            
            # defining empty lines in titles of the output table
            out[1,2:ncol]  <- c("")
            out[3,2:ncol]  <- c("")
            out[14,2:ncol] <- c("")
            out[18,2:ncol] <- c("")
            out[21,2:ncol] <- c("")
            out[25,2:ncol] <- c("")
            
            # creating a list of the structures (list of lists)
            li <- list(get(lista[1], envir = e1))
            for(i in 1:length(lista)){
              ifelse(i==length(lista),
                     break,
                     li <- append(li,list(get(lista[i+1], envir = e1))))
            }
            #li
            
            # extracting individual values from li
            ns      <- 0
            nm      <- 0
            nc      <- 0
            numc    <- 0
            use     <- 0
            Atotal  <- 0
            Aimp    <- 0
            Cimp    <- 0
            Cper    <- 0
            tfS     <- 0
            pe      <- 0
            Qd      <- 0
            Dd      <- 0
            Cd      <- 0
            V       <- 0
            lev     <- list(NULL)
            vol     <- list(NULL)
            lev.ini <- 0

            
            for(obj in lista){
              i          <- gsub("[^0-9]", "", unlist(obj))
              ns[i]      <- li[sapply(li, function(x) x["id"]==i)][[1]][["ns"]]
              nm[i]      <- li[sapply(li, function(x) x["id"]==i)][[1]][["nm"]]  
              nc[i]      <- li[sapply(li, function(x) x["id"]==i)][[1]][["nc"]]
              numc[i]    <- li[sapply(li, function(x) x["id"]==i)][[1]][["numc"]]  
              use[i]     <- li[sapply(li, function(x) x["id"]==i)][[1]][["use"]]  
              Atotal[i]  <- li[sapply(li, function(x) x["id"]==i)][[1]][["Atotal"]]  
              Aimp[i]    <- li[sapply(li, function(x) x["id"]==i)][[1]][["Aimp"]]
              Cimp[i]    <- li[sapply(li, function(x) x["id"]==i)][[1]][["Cimp"]]
              Cper[i]    <- li[sapply(li, function(x) x["id"]==i)][[1]][["Cper"]]
              tfS[i]     <- li[sapply(li, function(x) x["id"]==i)][[1]][["tfS"]]  
              pe[i]      <- li[sapply(li, function(x) x["id"]==i)][[1]][["pe"]]  
              Qd[i]      <- li[sapply(li, function(x) x["id"]==i)][[1]][["Qd"]]  
              Dd[i]      <- li[sapply(li, function(x) x["id"]==i)][[1]][["Dd"]]
              Cd[i]      <- li[sapply(li, function(x) x["id"]==i)][[1]][["Cd"]]
              V[i]       <- li[sapply(li, function(x) x["id"]==i)][[1]][["V"]] 
              lev[[i]]   <- li[sapply(li, function(x) x["id"]==i)][[1]][["lev2vol"]][["lev"]] 
              vol[[i]]   <- li[sapply(li, function(x) x["id"]==i)][[1]][["lev2vol"]][["vol"]] 
              lev.ini[i] <- li[sapply(li, function(x) x["id"]==i)][[1]][["lev.ini"]]
            }
            
            # delay of precipitation input time series
            P1 <- Delay(P1, tfS[2])   ## <-------------------- check for multiple time series
            
            
            # assigning values to the output table
            out[2,2:ncol]  <- ns[2:ncol]
            out[4,2:ncol]  <- nm[2:ncol]
            out[5,2:ncol]  <- nc[2:ncol]
            out[6,2:ncol]  <- numc[2:ncol]
            out[7,2:ncol]  <- use[2:ncol]
            out[8,2:ncol]  <- Atotal[2:ncol]
            out[9,2:ncol]  <- Aimp[2:ncol]
            out[10,2:ncol] <- tfS[2:ncol]
            out[13,2:ncol] <- pe[2:ncol]
            out[22,2:ncol] <- Qd[2:ncol]
            out[23,2:ncol] <- V[2:ncol]
            # a <- matrix
            
            ## calculating time series of pe
            # if(pe.daily.file != "" & pe.ts.file != ""){
            if(is.data.frame(pe.daily.file) == TRUE & pe.ts.file != ""){
              stop("too many -pe- objects are provided")
            }
            
            # if(pe.daily.file == "" & pe.ts.file == ""){
            if(is.data.frame(pe.daily.file) == FALSE & pe.ts.file == ""){
              pe.ts.season <- matrix(data = NA, nrow = length(P1[,1]), ncol = ncol-1)
              for(i in 1:(length(pe)-1)){
                pe.ts.season[,i] <- pe[i+1]
                out[13,i+1] <- pe[i+1]
              }
            }
            
            # if(pe.daily.file == "" & pe.ts.file != ""){
            if(is.data.frame(pe.daily.file) == FALSE & pe.ts.file != ""){
              pe.ts.season <- matrix(data = NA, nrow = nrow(P1), ncol = ncol-1)
              data.pe <- read.csv(pe.ts.file, header = T)
              
              if(nrow(P1) != nrow(data.pe)){
                stop("length differs, -data.pe- and -P1- lenghts are not equal")  
              }
              
              for(i in 1:(length(pe)-1)){
                pe.ts.season[,i] <- data.pe[,2]
                out[13,i+1] <- mean(data.pe[,2])
              }
            }
            
            # if(pe.daily.file != "" & pe.ts.file == ""){
            if(is.data.frame(pe.daily.file) == TRUE & pe.ts.file == ""){
              pe.ts <- list()
              # i <- 1
              for(i in 1:(length(pe)-1)){
                id <- lista[i]
                pe.ts[[id]] <-CInp2TS(cinp = pe[i+1], prec = P1, cinp.daily.file = pe.daily.file,
                                      cinp.weekly = pe.weekly, cinp.seasonal = pe.seasonal)
              }
                
              # str(pe.ts)
              # head(pe.ts[["E1"]][["data"]],1000)
              # plot(pe.ts[["E1"]][["xts"]][,4])
              
              pe.ts.season <- matrix(data = NA, nrow = length(pe.ts[[1]][["data"]][,4]), ncol = ncol-1)
              for(i in 1:(length(pe)-1)){
                out[13,i+1] <- mean(pe.ts[[i]][["data"]][,4])
                pe.ts.season[,i] <- pe.ts[[i]][["data"]][,4]
              }
            }

            ## calculating time series of qs
            # if(qs.daily.file != "" & qs.ts.file != ""){
            if(is.data.frame(qs.daily.file) == TRUE & qs.ts.file != ""){
              stop("too many -qs- objects are provided")
            }
            
            # if(qs.daily.file == "" & qs.ts.file == ""){
            if(is.data.frame(qs.daily.file) == FALSE & qs.ts.file == ""){
              qs.ts.season <- matrix(data = NA, nrow = length(P1[,1]), ncol = 1)
              qs.ts.season[,1] <- ww[["qs"]]
              out[12,i+1] <- mean(qs.ts.season[,1])
            }
            
            # if(qs.daily.file == "" & qs.ts.file != ""){
            if(is.data.frame(qs.daily.file) == FALSE & qs.ts.file != ""){
              data.qs <- read.csv(qs.ts.file, header = T)
              
              if(nrow(P1) != nrow(data.qs)){
                stop("length differs, -data.qs- and -P1- lenghts are not equal")  
              }
              
              qs.ts.season <- data.qs[,2] 
              out[12,i+1] <- mean(data.qs[,2])
            }
            
            # if(qs.daily.file != "" & pe.ts.file == ""){
            if(is.data.frame(qs.daily.file) == TRUE & qs.ts.file == ""){
              qs.ts <-CInp2TS(cinp = ww[["qs"]], prec = P1, cinp.daily.file = qs.daily.file,
                              cinp.weekly = qs.weekly, cinp.seasonal = qs.seasonal)
              qs.ts.season <- qs.ts[["data"]][,4]
              out[12,i+1] <- mean(qs.ts[["data"]][,4])
            }

            # calculating af, Qs24, Qf, COD and NH4
            # af        <- 0 # not needed anymore after inclusion of Delay function
            Qs24      <- matrix(data = NA, nrow = length(pe.ts.season[,1]), ncol = ncol-1)
            Qs24.mean <- 0
            Qf        <- 0
            CCOD      <- matrix(data = NA, nrow = length(pe.ts.season[,1]), ncol = ncol-1)
            CCOD.mean <- 0
            CNH4      <- matrix(data = NA, nrow = length(pe.ts.season[,1]), ncol = ncol-1)
            CNH4.mean <- 0
            # out[10,2] <- as.numeric(45) # checking the value of af
            # out[10,3] <- as.numeric(30) # checking the value of af
            environment(e1)
            
            for(i in 1:(ncol-1)){
              # af[i]        <- ifelse(as.numeric(out[10,i+1]) <= tf, 1, 0.5+50/((as.numeric(out[10,i+1]))+100)) # not needed anymore after inclusion of Delay function
              Qs24[,i]     <- pe.ts.season[,i]*qs.ts.season/(1440*60)
              Qs24.mean[i] <- mean(Qs24[,i])
              Qf[i]        <- as.numeric(out[9,i+1])*inf[["qf"]]
              CCOD[,i]     <- pe.ts.season[,i]*ww[["CODs"]]/(qs.ts.season*pe.ts.season[,i]+
                                                               as.numeric(out[9,i+1])*inf[["qf"]]*3600*24)*1000
              CCOD.mean[i] <- mean(CCOD[,i])
              CNH4[,i]     <- pe.ts.season[,i]*ww[["NH4s"]]/(qs.ts.season*pe.ts.season[,i]+
                                                               as.numeric(out[9,i+1])*inf[["qf"]]*3600*24)*1000
              CNH4.mean[i] <- mean(CNH4[,i])
            }
            # head(Qs24); tail(Qs24)
            # head(CCOD); tail(CCOD)
            # head(CNH4); tail(CNH4)
            
            ## assigning values to the output table
            # out[11,2:ncol] <- af # not needed anymore after inclusion of Delay function
            out[15,2:ncol] <- Qs24.mean # [l/s]
            out[16,2:ncol] <- Qf   # [l/s]
            out[19,2:ncol] <- CCOD.mean
            out[20,2:ncol] <- CNH4.mean
            
            # calculating Qt24
            Qt24      <- matrix(data = NA, nrow = length(pe.ts.season[,1]), ncol = ncol-1)
            Qt24.mean <- 0
            for(i in 1:(ncol-1)){
              Qt24[,i] <- Qs24[,i]+as.numeric(out[16,i+1])
              Qt24.mean[i] <- mean(Qt24[,i])
            }
            
            # assigning values to the output table
            out[17,2:ncol] <- Qt24.mean
            
            # emptying time
            # te <- D30/(Qd*0.06*dt-V_dw)*6
            
            # calculating "Berechungen spreadsheet"
            # defining rain time series for estructure
            #rm(P2,P3,P89)
            currentp <- ls(pattern="^P") #  
            listp <- gsub("E", "P", lista) # reemplacing E by P 
            listp
            if(spatial==1) {0   # <========------- spatial P
            } else {for (obj in listp){
              pp <- get(currentp)  
              assign(obj, pp) 
            }}
            
            # defining precipitations to read
            listp <- ls(pattern="^P")  
            #listp
            
            # calculating combined sewer flow, V_r, V_dw, cs_mr
            
            # defining maximum length of precipitation time series
            #dim <-0
            #for ( obj in listp ){
            #  p <- (get(obj))
            #  dim[obj] <- dim(p)[1]
            #  }
            
            # extracting numbers
            # a <- "E1 E2 E3"
            # gsub("[^0-9]", "", unlist(lista))
            
            # # assembling output table
            # setwd(folderOutput)
            # dir.create("EmiStatR_output", showWarnings=FALSE)
            # dir.output <- paste(folderOutput, "/EmiStatR_output", sep="")
            # setwd(dir.output)
            
            setwd(dir.current)
            if(export == 1){
              dir.create("EmiStatR_output", showWarnings=FALSE)
              dir.output <- paste(dir.current, "/EmiStatR_output", sep="")
              setwd(dir.output)
            }
            
            # parallel computing
            # install.packages("foreach")
            # install.packages("doParallel")
            # library(parallel) # required
            # detach("package:parallel", unload=TRUE)
            
            # library(foreach) # required
            # library(doParallel) # required
            
            # library(doMC)
            # registerDoMC() <--- evaluate alternative
            
            ini <- proc.time()   # starting timing of process
            
            # initializing parallel computing
            `%op%` <- ifelse(cores > 0, `%dopar%` , `%do%`)
            
            if(cores == 0){
              numCores <- detectCores() 
              #cl <- makePSOCKcluster(cores)
              #cores <- numCores
              #registerDoParallel(cl, cores=cores)
            }else{
              numCores <- detectCores()
              #  cl <- makeCluster(cores)
              cl <- makePSOCKcluster(cores)
              registerDoParallel(cl, cores=cores)
            }
            
            # if(cores == 0){stopCluster(cl)
            #                closeAllConnections()} 
            
            # i <- 1
            inputs <- 1:length(listp)
            # obj <- 1
            # foreach(obj = inputs, .packages=c("doParallel","parallel"), .export=c("listp")) %dopar% {
            output <- foreach(obj = inputs, .packages=c("doParallel","parallel"), .export=c("e1"), #.export=c("P1", "e1"),
                              .errorhandling = "pass", .verbose=FALSE) %op% {
                                
                                # obj <- 1
                                i <- obj
                                p <- data.frame(matrix(vector(), dim(get(currentp))[1], 28+3,    #<======----- check for spatial P 
                                                       dimnames=list(c(), c())), stringsAsFactors=F)
                                names(p) <- c("id", "Time [y-m-d h:m:s]", "P [mm]", "i[mm/h]",                 # 4
                                              "V_r [m3]","V_dw [m3]",                                              # 6
                                              "cs_mr [-]","o_cfyn [yes=1/no=0]","V_Chamber [m3]","V_Sv [m3]",         # 10
                                              "B_COD_Sv [gr]","B_NH4_Sv [gr]","C_COD_Sv [mg/l]","C_NH4_Sv [mg/l]", # 14
                                              "d_Sv [min]","f_Sv [occurrence]",                                    # 16   
                                              "V_InChamber [m3]","B_COD_InChamber [Kg]","B_NH4_InChamber [Kg]",             # 19
                                              "C_COD_InChamber [mg/l]","C_NH4_InChamber [mg/l]", "Q_Sv, [l/s]",          # 22
                                              "pe.season [PE]", "qs.season [l/PE/d]",                              # 24
                                              "B_COD_Chamber [Kg]","B_NH4_Chamber [Kg]",                                 # 26
                                              "C_COD_Chamber [mg/l]","C_NH4_Chamber [mg/l]",                             # 28
                                              "lev [m]","Qdi [l/s]", "dV [m3]") #, "io_fyn [yes=1/no=0]",                     # 30
                                # "io_eyn [yes=1/no=0]","io_iv [m3]","io_ev [m3]",
                                # "iov_i [m3]","iov_o [m3]")
                                
                                # head(p,10)
                                p[,1] <- c(1:dim(p)[1])
                                # p[,2:4]     <- get(listp[obj]) # <=========------for spatial P
                                p[,2:4]     <- get(currentp)
                                
                                #head(p,10)
                                #object.size(p)
                                
                                ## computing delta of time, dt
                                dtt   <- difftime(p[2,2], p[1,2], units="min")
                                dt    <- as.numeric(dtt) # delta time in minutes
                                
                                ## computation of precipitation depth
                                # p[,3] <- p[,3]
                                
                                
                                
                                ## computation
                                # p[,5] <- p[,3]*Aimp[i+1]*af[i]*10 # V_r_i (v1.2.0.3)
                                if(Runoff_Volume == TRUE){
                                  p[,5] <- p[,3] # V_r_i
                                }else{
                                  ## computation of intensity
                                  p[,4] <- p[,3]/dt*60
                                  
                                  ## computation of rain_volume
                                  p[,5] <- 10*p[,3]*(Cimp[i+1]*Aimp[i+1] + Cper[i+1]*(Atotal[i+1] - Aimp[i+1])) # V_r_i
                                }
                                
                                p[,6] <- Qt24[,i]*.06*dt # V_dw_i
                                p[,7] <- ifelse(p[,5] <= zero, zero, p[,6]/p[,5]) # cs_mr_i
                                
                                #=====================================================================
                                a   <- matrix(data=zero, nrow=nrow(p), ncol=1)
                                dV  <- matrix(data=zero, nrow=nrow(p), ncol=1)
                                Qdi <- matrix(data=zero, nrow=nrow(p), ncol=1)
                                
                                #     ###### original code (no Fortran code) ##########
                                #     ini <- proc.time()   # starting timing of process
                                #     a <- matrix(data=0, nrow=length(p[,2]), ncol=1)
                                #     for(j in 2:length(a)){
                                #       Qdo1 <- Volume2Level(a[j-1], lev2vol)
                                #       Qdo2 <- Volume2Level(a[j], lev2vol)
                                #       Qdo  <- abs(Qdo2-Qdo1)
                                #       Qd1  <- 0.06*dt*.0177*.02*1000*sqrt(2*9.81)*(Qdo^.5)   ## check if Qdo changes for other structure
                                #       ifelse(Qd1 > Qd[i+1], Qd1 <- Qd[i+1], Qd1 <- Qd1)
                                # 
                                #         dV <- p[j,5]+p[j,6]-Qd1
                                # #       dV <- p[j,5]+p[j,6]-0.06*dt*.0177*.02*1000*sqrt(2*9.81*(Qdo))   ## check if Qdo changes for other structure
                                # #       dV <- p[j,5]+p[j,6]-0.06*dt*Qd[i+1]
                                # 
                                #         ifelse(p[j,8]==1, ifelse(a[j-1]<V[i+1], a[j]<-(a[j-1]+dV), a[j]<-V[i+1]),
                                #                    ifelse((a[j-1]+dV) <= zero, a[j]<-0, a[j]<-(a[j-1]+dV)))
                                #     }
                                #     head(a)
                                #     plot(a, type="l")
                                # 
                                #     end <- proc.time()   # finish timing of process
                                #     elapsed <- end-ini; elapsed # 7.651
                                # 
                                #     ############################### end original code #########################
                                
                                ###### original code ##########
                                #ini <- proc.time()   # starting timing of process
                                #j <- 1
                                
                                lev2voli <-  list(lev = lev[[i+1]], vol = vol[[i+1]])
                                # j <- 1
                                # j <- 2
                                for(j in 1:length(a)){
                                  ### new implementation orifice (14.08.2017) ################################
                                  if(j == 1){
                                    p[j,"lev [m]"]   <- lev.ini[i+1] - min(lev[[i+1]]) - Dd[i+1]/2
                                    if(p[j,"lev [m]"] < 0) {p[j,"lev [m]"] <-0}
                                  }else{
                                    p[j,"lev [m]"]   <- Volume2Level(a[j-1], lev2vol = lev2voli) - min(lev[[i+1]]) - Dd[i+1]/2
                                    if(p[j,"lev [m]"] < 0) {p[j,"lev [m]"] <-0}
                                  }
                                  
                                  # p[j,"Qdi [l/s]"] <- ((pi*Dd[i+1]^2)/4*Cd[i+1]*(2*9.81*p[j,"lev [m]"])^0.5)*1000 # orifice equation
                                  Qdi[j] <- (pi*Dd[i+1]^2)/4*Cd[i+1]*(2*9.81*p[j,"lev [m]"])^0.5 # orifice equation [m3/s]
                                  Qdi[j] <- Qdi[j]*dt*60 # [m3]
                                  
                                  # #### begin commented v120_3 #######
                                  # if(j==1){
                                  #   p[j,"Qdi [l/s]"] <- p[j,"Qdi [l/s]"] - 0
                                  # }else{
                                  #   p[j,"Qdi [l/s]"] <- p[j,"Qdi [l/s]"] - p[(j-1),"Qdi [l/s]"]
                                  # }
                                  # #### end commented v120_3 #######
                                  
                                  # if(p[j,"Qdi [l/s]"] > Qd[i+1]){p[j,"Qdi [l/s]"] <- Qd[i+1]}
                                  if(Qdi[j] > Qd[i+1]*dt*60/1000){Qdi[j] <- Qd[i+1]*dt*60/1000}  # [m3]
                                
                                  p[j,8] <- ifelse((p[j,5]+p[j,6]) > Qdi[j], 1, 0) # o_cfyn_i  # o_tfyn_i
                                  
                                  dV[j] <- p[j,5] + p[j,6] - Qdi[j]
                                  p[j,"dV [m3]"] <- dV[j] 
                                  ### end new implementation orifice #########################################
                                  
                                  if(j > 1){
                                  ifelse(p[j,8] == 1, 
                                         ifelse(a[j-1] < V[i+1], 
                                                a[j] <- (a[j-1] + dV[j]), a[j] <- V[i+1]),
                                         ifelse((a[j-1] + dV[j]) <= zero, 
                                                a[j] <- 0, a[j] <- (a[j-1] + dV[j])))
                                  }else{a[j] <- 0}
                                }
                                
                                p[,"Qdi [l/s]"] <- Qdi/dt*1000/60 #[m3] -> [l/s]
                                
                                #end <- proc.time()   # finish timing of process
                                #elapsed <- end-ini; elapsed # 7.651
                                ##############################
                                #=====================================================================
                                
                                p[,9]  <- a # o_tf_i => V_Tank
                                # write.csv(p, "p.csv"); getwd(); head(p); tail(p)
                                
                                # dV <- p[,5]+p[,6]-Qd[i+1]*0.06*dt  ## before of orifice implementation
                                
                                p[,10] <- ifelse(p[,9] == V[i+1], dV, ifelse(p[,9] > V[i+1], p[,9]-V[i+1],zero)) # V_Ov_i (o_ov_i)
                                #p[,10] <- max(0,p[,9] + dV - V[i+1]) # V_Ov_i (o_ov_i)
                                #max(p[,10]) # 143.2294
                                #sum(p[,10]) # 8104.395
                                #mean(p[,10]) # 0.1541932
                                #sd(p[,10]) # 2.491797
                                
                                #rw[["CODr"]] <- 0 # check this value
                                
                                # B_COD_Ov_i (o_COD_i)
                                p[,11] <- ifelse(p[,10]>zero, p[,10]*p[,7]/(p[,7]+1)*CCOD[,i]+p[,10]/(p[,7]+1)*rw[["CODr"]],zero) 
                                # mean(p[,11]); sum(p[,11]); plot(p[,11], type="l")
                                
                                # B_COD_Ov_i (o_COD_i)
                                #p[,11] <- max(zero, p[,10]*p[,7]/(p[,7]+1)*COD[i]/1000+p[,10]/(p[,7]+1)*rw[["CODr"]]/1000) 
                                
                                #max(p[,11]) # 15.6363
                                #sum(p[,11]) # 997.8134
                                #mean(p[,11]) # 0.01898427
                                #sd(p[,11]) # 0.282776
                                
                                #B_NH4_Ov_i (o_NH4_i)
                                p[,12] <- ifelse(p[,10]>zero, p[,10]*p[,7]/(p[,7]+1)*CNH4[,i]+p[,10]/(p[,7]+1)*rw[["NH4r"]],zero) 
                                # mean(p[,12]); sum(p[,12]); plot(p[,12], type="l")
                                
                                p[,13] <- ifelse(p[,10]<=zero, zero, p[,11]/p[,10]) # C_COD_Ov_i (o_CCOD_i)
                                p[,14] <- ifelse(p[,10]<=zero, zero, p[,12]/p[,10]) # C_NH4_Ov_i (o_CNH4_i)
                                p[,15] <- ifelse(p[,10]<=zero, 0, 1) # d_Ov_i (o_d_i)
                                
                                j <- 1
                                a <- matrix(data=0, nrow=length(p[,9]), ncol=1)
                                # a <- matrix(data=1:9, nrow=9, ncol=1)
                                # min(a[3-27,1]:a[9,1])
                                for(j in 1:length(a)){
                                  if(j == 1)  {a[1] <- 0}
                                  if(j <  29 && p[j,15] == 1 && p[j-1,15] == 0 && (min(p[1,9]   :p[j,9])) <= 
                                     ((Qd[i+1] - Qt24[,i])*0.06*dt)){a[j] <- 1} 
                                  if(j >= 29 && p[j,15] == 1 && p[j-1,15] == 0 && (min(p[j-28,9]:p[j,9])) <= 
                                     ((Qd[i+1] - Qt24[,i])*0.06*dt)){a[j] <- 1} 
                                }
                                p[,16]  <- a # f_Ov_i (o_f_i) [occurrence]
                                # mean(p[,16]); sum(p[,16]); plot(p[,16], type="l")
                                
                                # new implementation InTank volume
                                p[,17] <- ifelse(p[,9] == V[i+1], Qd[i+1]*0.06*dt,
                                                 ifelse(p[,9] > V[i+1], max(p[,5]+p[,6]-(p[,9]-V[i+1]), zero),
                                                        p[,5]+p[,6])) # V_InTank_i [m3]

                                # new implementation InTank loads and concentrations (before Kai left) 
                                # Vsew      <- as.numeric(out[14,i+1])*0.06*dt # [m3] 
                                Vsew      <- Qs24[,i]*0.06*dt # [m3] 
                                # mean(Vsew); sum(Vsew); plot(Vsew, type="l")
                                # write.csv(Vsew, file="Vsew.csv"); getwd()
                                
                                Vinf      <- as.numeric(out[15,i+1])*0.06*dt # [m3]
                                
                                B_COD_sew <- (ww[["CODs"]]*1000/qs.ts.season)*1000/1000/1000*Vsew # [kg]
                                # mean(B_COD_sew); sum(B_COD_sew); plot(B_COD_sew, type="l")
                                # write.csv(B_COD_sew, file="B_COD_sew.csv"); getwd()
                                
                                # B_COD_inf <- ((inf[["CODf"]]*as.numeric(out[12,i+1])/86400/1000)/(inf[["qf"]]*
                                #                as.numeric(out[9,i+1])/1000))*Vinf # [kg]
                                B_COD_inf <- ((inf[["CODf"]]*pe.ts.season[,i]/86400/1000)/(inf[["qf"]]*
                                                                                             as.numeric(out[9,i+1])/1000))*Vinf # [kg]
                                # mean(B_COD_inf); sum(B_COD_inf); plot(B_COD_inf, type="l")
                                
                                C_COD_DWF <- (B_COD_sew+B_COD_inf)/(Vsew+Vinf)/1000*1000*1000 # [mg/l]
                                # mean(C_COD_DWF); sum(C_COD_DWF); plot(C_COD_DWF, type="l")
                                
                                B_COD_DWF <- C_COD_DWF*(Vsew+Vinf)*1000/1000/1000 # [kg]
                                # mean(B_COD_DWF); sum(B_COD_DWF); plot(B_COD_DWF, type="l")
                                
                                B_NH4_sew <- (ww[["NH4s"]]*1000/qs.ts.season)*1000/1000/1000*Vsew # [kg]
                                #mean(B_NH4_sew); sum(B_NH4_sew); plot(B_NH4_sew, type="l")
                                
                                B_NH4_inf <- ((inf[["NH4f"]]*pe.ts.season[,i]/86400/1000)/(inf[["qf"]]*
                                                                                             as.numeric(out[9,i+1])/1000))*Vinf # [kg]
                                
                                C_NH4_DWF <- (B_NH4_sew+B_NH4_inf)/(Vsew+Vinf)/1000*1000*1000 # [mg/l]
                                B_NH4_DWF <- C_NH4_DWF*(Vsew+Vinf)*1000/1000/1000 # [kg]
                                
                                p[,18]    <- ifelse(p[,7] > zero, p[,17]*p[,7]/(p[,7]+1)*CCOD[,i]/1000+p[,17]/(p[,7]+1)*
                                                      rw[["CODr"]]/1000, B_COD_DWF) # B_COD_InTank
                                # mean(p[,18]); sum(p[,18]); plot(p[,18], type="l")
                                # write.csv(p, file="p.csv"); getwd(); head(p)
                                
                                p[,19]    <- ifelse(p[,7] > zero, p[,17]*p[,7]/(p[,7]+1)*CNH4[i]/1000+p[,17]/(p[,7]+1)*
                                                      rw[["NH4r"]]/1000, B_NH4_DWF) # B_NH4_InTank
                                # mean(p[,19]); sum(p[,19]); plot(p[,19], type="l")
                                
                                bb        <- p[,18]*1000/p[,17] # C_COD_InTank
                                bb[bb<zero]  <- zero
                                p[,20]    <- bb # C_COD_InTank
                                # mean(p[,20]); sum(p[,20]); plot(p[,20], type="l")
                                
                                bb        <- p[,19]*1000/p[,17] # C_NH4_InTank
                                bb[bb<zero]  <- zero
                                p[,21]    <- bb # C_NH4_InTank
                                
                                
                                #b=9
                                #c <- ifelse(b>=0 & b==9,ifelse(b>=5, print("mayor que 5"), print("entre 0 y 5")),print("menor que 0"))  
                                #d <- ifelse(b>=0 & b==9, print("mayor que 0 y es 9"), print("diferente que 9"))  
                                
                                ## overflow flow
                                p[,22]    <- p[,10]/(dt*60)*1000  # overflow flow, Q_Ov, [l/s]
                                
                                ## pe time series
                                p[,23]    <- pe.ts.season[,i]
                                p[,24]    <- qs.ts.season
                                
                                ##---------------------------------------------------------------------------------
                                ## new implementation tank loads and concentrations (after Kai left - 24/04/2017) 
                                ##---------------------------------------------------------------------------------
                                Vinflow <- p[,5]+p[,6]
                                
                                # B_COD_tank_i 
                                p[,25] <- Vinflow*p[,7]/(p[,7]+1)*CCOD[,i]/1000+Vinflow/(p[,7]+1)*rw[["CODr"]]/1000 
                                
                                #B_NH4_tank_i
                                p[,26] <- Vinflow*p[,7]/(p[,7]+1)*CNH4[,i]/1000+Vinflow/(p[,7]+1)*rw[["NH4r"]]/1000
                                
                                # C_COD_tank_i
                                p[,27] <- p[,25]*1000/Vinflow 
                                
                                # C_NH4_tank_i
                                p[,28] <- p[,26]*1000/Vinflow
                                ##---------------------------------------------------------------------------------
                                ## end new implementation tank loads and concentrations
                                ##---------------------------------------------------------------------------------

                                ## preparing final output table out1
                                p1 <- p[,c(1:16,22:24,29:30)]


                                ## overflow data summary results
                                q <- data.frame(matrix(vector(), 13, 2, 
                                                       dimnames=list(c(), c())), stringsAsFactors=F)
                                q[1:(11+2+2),1] <- c("Period [day]", "Duration, d_Sv, [min]", "Frequency, f_Sv, [occurrence] (aprox.)",
                                                   "Total volume, V_Sv, [m3]", "Average flow, Q_Sv, [l/s]", 
                                                   "Total COD load, B_COD_Sv, [kg]",
                                                   "Average COD concentration, C_COD_Sv_av, [mg/l]", 
                                                   "99.9th percentile COD concentration, C_COD_Sv_99.9, [mg/l]",
                                                   "Maximum COD concentration, C_COD_Sv_max, [mg/l]",
                                                   "Total NH4 load, B_NH4_Sv, [kg]", "Average NH4 concentration,C_NH4_Sv_av, [mg/l]",
                                                   "99.9th percentile NH4 concentration, C_NH4_Sv_99.9, [mg/l]",
                                                   "Maximum NH4 concentration, C_NH4_Sv_max, [mg/l]",
                                                   "Structure summary results", "Volume Chamber, V_Chamber [m3]")
                                
                                peri   <- difftime(p[dim(p)[1],2], p[2,2], units="days")
                                q[1,2] <- peri  #round(peri/365, digits=3)              # period in [day],                  p
                                q[2,2] <- sum(p[,15])*dt                                # duration in [min],                d_Ov         (o_d)
                                q[3,2] <- sum(p[,16])                                   # frequency in [occurrence],        f_Ov         (o_f)
                                q[4,2] <- sum(p[,10])                                   # volume [m3],                      V_Ov         (o_ov)
                                
                                q[5,2] <- sum(p[,10])/(q[2,2]*60)*1000                  # flow in [l/s],                    Q_Ov         (o_of)
                                # q[5,2] <- mean(p[,22][p[,22]>0])                      # flow in [l/s],                    Q_Ov         (o_of)
                                
                                q[6,2] <- sum(p[,11])/1000                              # COD load [kg],                    B_COD_Ov     (o_COD)
                                
                                # q[7,2] <- round(q[6,2]/q[4,2]*1000,digits=5)          # average COD concentration [mg/l]  C_COD_Ov_av  (o_CCOD_av)
                                q[7,2] <- mean(p[,13][p[,13] > zero])                   # average COD concentration [mg/l]  C_COD_Ov_av  (o_CCOD_av)
                                
                                q[8,2] <- quantile(p[,13][p[,13] > zero], probs = 0.999)               # 99.9th percentile COD concentration [mg/l]  C_COD_Ov_99.9  
                                q[9,2] <- max(p[,13])               # Maximum COD concentration [mg/l]  C_COD_Ov_max (o_CCOD_max) 
                                
                                q[10,2] <- sum(p[,12])/1000                             # NH4 load [kg]                     B_NH4_Ov     (o_NH4)

                                # q[10,2]<- round(q[9,2]/q[4,2]*1000,digits=5)          # average NH4 concentration [mg/l]  C_NH4_Ov_av  (o_CNH4_av)
                                q[11,2]<- mean(p[,14][p[,14] > zero])                   # average NH4 concentration [mg/l]  C_NH4_Ov_av  (o_CNH4_av)
                                
                                q[12,2]<- quantile(p[,14][p[,14] > zero], probs= 0.999)                # 99.9th percentile concentration [mg/l]  C_NH4_Ov_99.9  
                                q[13,2]<- max(p[,14])                                   # Maximum NH4 concentration [mg/l]  C_NH4_Ov_max (o_CNH4_max)  
                                q[15,2]<- sum(p[,9])                                    # Volume tank [m3]                 VTank  
                                
                                ## creating variable to be saved
                                # assign(paste("out1_",lista[i], sep=""), p) # output all variables computed internally
                                assign(paste("out1_",lista[i], sep=""), p1)  # output only variables explained in the user manual
                                assign(paste("out2_",lista[i], sep=""), q)
                                
                                
                                # writing output                                            <-------------------
                                if(export == 1){
                                  # setwd(paste(folderOutput,"/EmiStatR_output", sep=""))
                                  setwd(dir.output)
                                  
                                  write.table(get(paste("out1_",lista[i], sep="")), file = paste("out1_",lista[i],".csv",sep=""), 
                                              sep = ",", qmethod = "double", row.names=FALSE)
                                  write.table(get(paste("out2_",lista[i], sep="")), file = paste("out2_",lista[i],".csv",sep=""), 
                                              sep = ",", qmethod = "double", row.names=FALSE)
                                }
                                
                                ## returning variables
                                # st.ls <- objects(pattern = "^E")
                                # st <- lapply(st.ls, FUN=get)

                                input.gen <- list(spatial = spatial, zero = zero, folder = folder, 
                                                  cores = cores,
                                                  ww = ww, inf = inf, rw = rw, prec = P1, 
                                                  st = li,
                                                  pe.daily.file = pe.daily.file, pe.weekly = pe.weekly, pe.seasonal = pe.seasonal,
                                                  qs.daily.file = qs.daily.file, qs.weekly = qs.weekly, qs.seasonal = qs.seasonal,
                                                  export = export)
                                
                                return(list(out1=get(paste("out1_",lista[i], sep="")), out2=get(paste("out2_",lista[i], sep="")),
                                            lista=lista, input.gen = input.gen))
                                
                                # updating counter
                                i <- i+1
                                
                              } ## end foreach
            
            if(cores > 0){
              stopCluster(cl)
              # closeAllConnections() # commented out for version 1.2.3.0 (to keep CRAN rules)
              } 
            
            
            end <- proc.time()
            elapsed <- end-ini; elapsed # c(19.419, 21.317) <- c(11.438,11.389)
            
            #   # uptating out table with overflow data summary results  
            #   setwd(paste(folderOutput,"/EmiStatR_output", sep=""))
            
            #   #sources = list.files(pattern="^out2")
            #   sources = ls(pattern="^out2")
            
            # i = 2
            # for(i in 1:length(sources)){
            environment(e1)
            
            
            for(i in 1:length(output)){
              ## tmpf <- read.csv(file=sources[i],header=TRUE,sep=",")
              tmpf <- output[[i]][[2]]
              # str(output)
              #write.csv(tmpf, file=paste("test",i,".csv", sep=""))
              out[26:(35 + 2 + 2 + 1), 1]   <- as.character(tmpf[1:(11 + 2 + 2), 1])
              out[26:(35 + 2 + 2 + 1), i + 1] <- tmpf[,2]  
              out[39,i+1] <- c("")  
            }
            
            # writing output
            if(export == 1){
              write.table(out, file = paste("out.csv",sep=""), 
                          sep = ",", qmethod = "double", row.names=FALSE)
            }
            
            ## plotting output 1
            # sources = list.files(pattern="^out1_E")
            i <- 1
            # for(i in 1:length(sources)){
            for(i in 1:length(output)){
              #tmpf <- read.csv(file=sources[i],header=TRUE,sep=",")
              tmpf <- output[[i]][[1]]
              #     warnings()
              #     head(tmpf, 10)
              #     head(tmpf[,2], 10)
              #     plot(tmpf[1:10,2], tmpf[1:10,3])
              #    
              #     dateini <- as.POSIXct(tmpf[1,2])
              #     dateend <- as.POSIXct(tmpf[dim(tmpf)[1],2])
              #     tt <- as.POSIXct(tmpf[,2])
              # 
              #     old.par <- par(mfrow=c(3, 1))
              #     
              #     plot(tt, tmpf[,3], type ="l", xaxt="n", xlab = "", ylab="Precipitation [mm]")
              #     axis.POSIXct(1, at=seq(dateini, dateend, by="month"), 
              #                  format="%b-%Y") #label the x axis by months
              #     
              #     plot(tt, tmpf[,10], type ="l", xaxt="n", ylab="Overflow volume [m3]")
              #     axis.POSIXct(1, at=seq(dateini, dateend, by="month"), 
              #                  format="%b-%Y") #label the x axis by months
              #     
              #     plot(tt, tmpf[,12], type ="l", xaxt="n", ylab="Overflow COD load [kg]")
              #     axis.POSIXct(1, at=seq(dateini, dateend, by="month"), 
              #                  format="%b-%Y") #label the x axis by months
              #     
              #     par(old.par)
              
              # arraging variables for plotting
              datP <- as.data.frame(tmpf[,1])
              datP[,2] <- "Precipitation, P [mm]" # variable label
              datP[,3] <- tmpf[,2] # time
              datP[,4] <- tmpf[,3] # precipitation, P
              colnames(datP) <- c("id", "variable", "time", "value")
              
              datOV <- as.data.frame(tmpf[,1])
              datOV[,2] <- "Volume, V_Ov [m3]" # variable label
              datOV[,3] <- tmpf[,2] # time
              datOV[,4] <- tmpf[,10] # overflow volume
              colnames(datOV) <- c("id", "variable", "time", "value")
              
              datOCOD <- as.data.frame(tmpf[,1])
              datOCOD[,2] <- "COD load, B_COD_Ov [kg]" # variable label
              datOCOD[,3] <- tmpf[,2] # time
              datOCOD[,4] <- tmpf[,11] # COD load
              colnames(datOCOD) <- c("id", "variable", "time", "value")
              
              datONH4 <- as.data.frame(tmpf[,1])
              datONH4[,2] <- "NH4 load, B_NH4_Ov [kg]" # variable label
              datONH4[,3] <- tmpf[,2] # time
              datONH4[,4] <- tmpf[,12] # NH4 load
              colnames(datONH4) <- c("id", "variable", "time", "value")
              
              datOCCOD <- as.data.frame(tmpf[,1])
              datOCCOD[,2] <- "COD concentration, C_COD_Ov [mg/l]" # variable label
              datOCCOD[,3] <- tmpf[,2] # time
              datOCCOD[,4] <- tmpf[,13] # COD concentration
              colnames(datOCCOD) <- c("id", "variable", "time", "value")
              
              datOCNH4 <- as.data.frame(tmpf[,1])
              datOCNH4[,2] <- "NH4 concentration, C_NH4_Ov [mg/l]" # variable label
              datOCNH4[,3] <- tmpf[,2] # time
              datOCNH4[,4] <- tmpf[,14] # NH4 concentration
              colnames(datOCNH4) <- c("id", "variable", "time", "value")
              
              dat     <- rbind(datP, datOV, datOCOD, datONH4, datOCCOD, datOCNH4)
              
              # creating plots by lattice
              # library(lattice) # required
              if(export == 1){
                #     fig <- xyplot(dat[,4]~as.POSIXct(dat[,3])|variable, data = dat,
                #                   main="Overflow data",
                #                   xlab="Time", ylab="", layout =c(2,3), index.cond=list(c(1,3,2,4,5,6)),
                #                   type="l", scales=list(y="free", x=list(at= seq(as.POSIXct(dat[1,3]), by="3 day", length=5), 
                #                                   labels=format(seq(as.POSIXct(dat[1,3]), by="3 day", length=5),"%D%M%Y"))))
                fig <- xyplot(dat[,4]~as.POSIXct(dat[,3])|variable, data = dat,
                              main="Overflow data",
                              xlab="Time", ylab="", layout =c(2,3), index.cond=list(c(1,3,2,4,5,6)),
                              type="l", scales=list(y="free"))
                
                # saving the plot
                # name <- sub("^([^.]*).*", "\\1", sources[i]) # extract filename without the extension 
                pdf(paste("plot_out1_", output[[1]][[3]][i], ".pdf", sep=""))
                print(fig)
                dev.off()
              }
            }
            
            
            numCores <- detectCores() 
            #   if(cores == 0){
            #     cores <- numCores - 1
            #   }
            
            if(export == 1 & cores != 0){
              print(paste("done, please check your output folder (used", cores, "of", numCores, "cores)", sep=" "))
            }
            
            if(export == 1 & cores == 0){
              print(paste("done, please check your output folder (used 1 of", numCores, "cores, no parallel mode, cores=0)", sep=" "))
            }
            
            if(export == 0 & cores != 0){
              print(paste("done (used", cores, "of", numCores, "cores)", sep=" "))
            }
            
            if(export == 0 & cores == 0){
              print(paste("done (used 1 of", numCores, "cores, no parallel mode, cores=0)", sep=" "))
            }
            
            setwd(dir.current)
            
            return(output)
          }
)
                                