##################################
###      PLOTTING SUITE        ###
##################################

# CONTAINS
# traffic.col - traffic light colour ramp
# trafficAttPlot() - traffic light plot of attribute performance
# expSpace2dViz() - plot 2d exposure space
# frontBoilerPlateInfo() - informative text on the target

#----------------------------
# COLOUR RAMP
traffic.col <- c("chartreuse3", "gold1", "red1")


measure.diff <- function(type = NULL,
                         simPt = NULL,
                         targetPt = NULL,
                         pc.lim = c(5, 10),
                         diff.lim = c(0.05, 0.1)) {
  # Determine difference and class lims
  switch(type,
    "pc" = {
      diff.att <- pc.calc(sim = simPt, target = targetPt)
      class.lim <- pc.lim
    },
    "frac" = {
      diff.att <- pc.calc(sim = simPt, target = targetPt)
      class.lim <- pc.lim
    },
    "diff" = {
      diff.att <- absDiff.calc(sim = simPt, target = targetPt)
      class.lim <- diff.lim
    },
    "val" = {
      diff.att <- absDiff.calc(sim = simPt, target = targetPt)
      class.lim <- diff.lim
    },
    {
      diff.att <- pc.calc(sim = simPt, target = targetPt)
      class.lim <- pc.lim
    }
  )
  out <- list(class.lim = class.lim, diff.att = diff.att)
  return(out)
}


# new functions based on trafficAttPlot to return the the trafficAtt calculations without plotting
# getTrafficAtt works on a single target, getSimTraffic works on the full simulation
# the initial portion of this function can be included in getSimTraffic (upto the attSel loop) - so that these calculations need to done for only one target

# Returns the numbers behind the traffic light plots (both original values [abs/perc], and performance classified as G/F/B)
# This data.frame will be used to plot the heatmap collating data from all the targets
# ========================================================================================
getTargetTraffic <- function(attSel = NULL,
                             attPerturb = NULL,
                             attPrim = NULL,
                             simPt = NULL, # simPt in targetType space
                             target = NULL, # target in targetType space
                             targetType = NULL) {
  # get indices of primary attributes
  # These attributes have to be listed first in the heatmap
  #---------------------------------------------------------
  indSec <- seq(1, length(attSel))

  if (length(attPrim) > 0) {
    get.ind <- function(x, y) {
      which(x == y)
    }
    indPrim <- vapply(attPrim, FUN = get.ind, FUN.VALUE = numeric(1), x = attSel, USE.NAMES = FALSE) # Indices of primary attributes
    indSec <- indSec[-indPrim]
  } else {
    indPrim <- integer(0) # all atributes are secondary
  }

  indPerturb <- which(attSel %in% attPerturb)

  # rearrage the order of attributes (Primary First)
  # + logical sequencing of attributes
  #---------------------------------------------------------

  attSecRearr <- getAttRearr(attSel[indSec])
  indSecRearr <- match(attSecRearr, attSel)
  indAtt <- c(indPrim, indSecRearr)

  # Fields of the data frame
  dfAttReorder <- rep(NA, length(attSel)) # primary atts first
  dfAttPrim <- rep(FALSE, length(attSel)) # is it primary?
  dfAttPerturb <- rep(FALSE, length(attSel)) # is it perturbed? (otherwise held)
  dfAttDiff <- rep(NA, length(attSel)) # sim-target (% or K)
  dfAttPerf <- rep(NA, length(attSel)) # sim-target (as performance)

  for (i in 1:length(attSel)) {
    # Index of the attribute
    ind <- indAtt[i]
    dfAttReorder[i] <- attSel[ind]
    if (ind %in% indPrim) dfAttPrim[i] <- TRUE
    if (ind %in% indPerturb) dfAttPerturb[i] <- TRUE

    # calculate differences
    mdiff <- measure.diff(
      type = targetType[ind],
      simPt = simPt[ind],
      targetPt = target[ind],
      pc.lim = trafficLim[["pc.lim"]],
      diff.lim = trafficLim[["diff.lim"]]
    )

    dfAttDiff[i] <- mdiff$diff.att[[1]]
    dfAttPerf[i] <- categ.fun(
      perf.lim = mdiff$class.lim,
      rel.diff = mdiff$diff.att
    )
  }

  outputDf <- data.frame(dfAttReorder, dfAttPrim, dfAttPerturb, dfAttDiff, dfAttPerf, stringsAsFactors = FALSE)
  colnames(outputDf) <- c("attName", "Prim", "Perturb", "Diff", "Performance")
  rownames(outputDf) <- NULL

  return(outputDf)
}

# function to return the indices of attSel sequenced for traffic heatmap plot

getAttRearr <- function(attSec) { # attributes to be rarranged as per logical sequencing
  attSplit <- strsplit(attSec, "_")
  attVar <- unlist(lapply(attSplit, `[[`, 1))
  attType <- unlist(lapply(attSplit, `[[`, 3))
  allVar <- unique(attVar)

  # variable types grouped, P may have additional att types
  grpAttType <- list()
  grpAttType[[1]] <- c("tot", "avg")
  grpAttType[[2]] <- c("dyWet")
  grpAttType[[3]] <- c("avgWSD", "maxWSD", "GSL", "CSL", "rng")
  grpAttType[[4]] <- c("avgDSD", "maxDSD")
  grpAttType[[5]] <- c("P99", "dyWet99p", "P5", "P95", "F0")
  grpAttType[[6]] <- c("nWet", "R10", "seasRatio")

  indAtt <- list()
  for (i in 1:length(allVar)) {
    indMaster <- which(attVar == allVar[i])

    indByVarList <- list()
    for (t in 1:length(grpAttType)) {
      indByVarList[[t]] <- which(attVar == allVar[i] & attType %in% grpAttType[[t]])
    }
    indByVar <- unlist(indByVarList)

    # if there are some extra attributes no part of the groups above
    indExtra <- indMaster[!indMaster %in% indByVar]
    indAtt[[i]] <- c(indByVar, indExtra)
  }
  attSecRearr <- attSec[unlist(indAtt)]
  return(attSecRearr)
}


getSimTraffic <- function(sim) { # simulations generated using generateScenarios (contains expSpace and nml information)

  # get simulation info
  expSpace <- sim[["expSpace"]]
  nml <- sim[["controlFile"]]

  # Anjana: [1] may be removed once controlFile from simple scaling just shows "scaling"
  if (nml[1] == "scaling") {
    stop("plotScenarios cannot be used on simulations generated using simple scaling")
  }

  # Attributes
  attSel <- colnames(expSpace$targetMat)
  attPerturb <- expSpace[["attPerturb"]]
  attPrim <- nml[["penaltyAttributes"]]

  # Variable and target type
  varType <- vapply(attSel, FUN = get.attribute.varType, FUN.VALUE = character(1), USE.NAMES = FALSE)
  # print('fix targetType')
  targetType <- sim$expSpace$targetType
  if (is.null(targetType)) {
    targetType <- vapply(varType, FUN = get.target.type, FUN.VALUE = character(1), USE.NAMES = FALSE)
  }

  # modelTag and variables
  simVar <- names(nml[["modelType"]])
  modelTag <- NULL
  for (v in simVar) {
    modelTag <- c(modelTag, getModelTag(nml = nml, v))
  }

  # get nameTarg and nameReps
  nameReps <- names(sim)[!(names(sim) %in% c("expSpace", "simDates", "controlFile"))]
  numReps <- 1:length(nameReps)

  nameTarg <- names(sim[[nameReps[[1]]]])
  numTarg <- 1:length(nameTarg)

  # Anjana: column name can be changed here to plot
  # "Diff" - the original percentage deviations from the target set
  # "Performance" - classified GOOD/FAIR/POOR
  colName <- "Diff"

  # matrix to store diagnostic information
  # mean across replicates
  diagMatMean <- matrix(NA, nrow = length(nameTarg), ncol = length(attSel))
  # standard deviation across replicates
  diagMatSD <- matrix(NA, nrow = length(nameTarg), ncol = length(attSel))

  for (t in 1:length(nameTarg)) {
    target <- expSpace$targetMat[t, ]
    diagData <- matrix(NA, nrow = length(nameReps), ncol = length(attSel))
    for (r in 1:length(nameReps)) {
      simTarget <- sim[[nameReps[r]]][[nameTarg[t]]][["targetSim"]]
      targetTraffic <- getTargetTraffic(attSel = attSel, attPerturb = attPerturb, attPrim = attPrim, simPt = simTarget, target = target, targetType = targetType)
      diagDataTemp <- targetTraffic[[colName]]
      if (colName == "Performance") {
        # Assign numbers for performance (1 = good, 2 = fair, 3 = good)
        pNum <- list(good = 1, fair = 2, poor = 3)
        for (p in c("good", "fair", "poor")) {
          ind <- which(diagDataTemp == p)
          diagData[r, ind] <- pNum[[p]]
        }
      } else {
        diagData[r, ] <- diagDataTemp
      }
    }
    # calculate stats across replicates
    diagMatMean[t, ] <- apply(diagData, 2, mean)
    diagMatSD[t, ] <- apply(diagData, 2, stats::sd)
  }

  # Names of att
  attName_temp <- targetTraffic[["attName"]]
  prim_temp <- targetTraffic[["Prim"]]
  perturbed_temp <- targetTraffic[["Perturb"]]
  markPrim_temp <- rep("", length = length(attName_temp))
  markPrim_temp[prim_temp] <- "*"
  # don't need this since we are using the full attribute names in the heatmap
  # attNameMarked <- paste0(markPrim, attName)

  df_diagMatMean_temp <- data.frame(diagMatMean)
  rownames(df_diagMatMean_temp) <- nameTarg
  colnames(df_diagMatMean_temp) <- attName_temp

  df_diagMatSD_temp <- data.frame(diagMatSD)
  rownames(df_diagMatSD_temp) <- nameTarg
  colnames(df_diagMatSD_temp) <- attName_temp

  # If difference is to be plotted, "Temp" needs to be separate from the other variables
  # this ordering takes priority over primary attributes
  # rearranging the df columns
  attSplit <- strsplit(attName_temp, "_")
  attVar <- sapply(attName_temp, get.attribute.varType)
  Tind <- which((attVar %in% c("Temp", "Temp/P")))

  # Temperature variables exist
  #  if (length(Tind) > 0) {
  if ((length(Tind) > 0) & (length(Tind) < length(attSel))) {
    attName <- splitInTwo(attName_temp, Tind)
    markPrim <- splitInTwo(markPrim_temp, Tind)
    perturbed <- splitInTwo(perturbed_temp, Tind)
    df_diagMatMean <- splitInTwo(df_diagMatMean_temp, Tind)
    df_diagMatSD <- splitInTwo(df_diagMatSD_temp, Tind)
  } else {
    # only one element in the list
    attName <- list()
    markPrim <- list()
    perturbed <- list()
    df_diagMatMean <- list()
    df_diagMatSD <- list()
    attName[[1]] <- attName_temp
    markPrim[[1]] <- markPrim_temp
    perturbed[[1]] <- perturbed_temp
    df_diagMatMean[[1]] <- df_diagMatMean_temp
    df_diagMatSD[[1]] <- df_diagMatSD_temp
  }

  # Return additional info needed for plots
  return(list(
    mean = df_diagMatMean,
    SD = df_diagMatSD,
    attName = attName,
    markPrim = markPrim,
    perturbed = perturbed
  ))
}


# data can be vector or df. If df ind corresponds to column indices
moveToEnd <- function(data, ind) {
  if (is.vector(data)) {
    data_temp <- data[ind]
    return(c(data[-ind], data_temp))
  } else if (is.matrix(data) | is.data.frame(data)) {
    data_temp <- data[, ind]
    return(cbind(data[, -ind], data_temp))
  }
}

# data can be vector or df. If df ind corresponds to column indices
splitInTwo <- function(data, ind) {
  if (is.vector(data)) {
    data_temp <- data[ind]
    return(list(data[-ind], data_temp))
  } else if (is.matrix(data) | is.data.frame(data)) {
    data_temp <- data[, ind]
    return(list(data[, -ind], data_temp))
  }
}


trafficAttPlot <- function(attSel = NULL,
                           attPrim = NULL,
                           simPt = NULL, # simPt in targetType space
                           target = NULL, # target in targetType space
                           targetType = NULL,
                           ...) {
  pc.lim <- c(5, 10) # SET FAIR AT 5-10 AND POOR 10+
  diff.lim <- c(0.5, 1)

  # setup layout as max 4 columns
  col.plot <- 4
  # n.row=ceiling(length(attSel)/col.plot)
  # #determine if any padding needed
  # nlast=length(attSel)%%col.plot
  # if(nlast==0){nlast=4} #update nlast (if evenly divisible)
  # npad=col.plot-nlast

  # par(mfrow=c(n.row,4),oma=c(2,2,2,2),mar=c(3,3,1,1),xpd=TRUE) #xpd=TRUE allow margin plotting
  graphics::par(mfrow = c(3, col.plot), oma = c(2, 2, 2, 2), mar = c(3, 3, 1, 1), xpd = TRUE) # xpd=TRUE allow margin plotting

  # get indices of primary attributes
  if (length(attPrim) > 0) {
    get.ind <- function(x, y) {
      which(x == y)
    }
    indPrim <- vapply(attPrim, FUN = get.ind, FUN.VALUE = numeric(1), x = attSel, USE.NAMES = FALSE) # Indices of primary attributes
  } else {
    indPrim <- integer(0)
    indSec <- seq(1, length(attSel)) # all atributes are secondary
  }


  if (!identical(indPrim, integer(0))) {
    # SET UP SECONDARY ATTRIBUTE TREATMENT
    nSec <- length(attSel) - length(attPrim) # no. of secondary attributes
    indSec <- seq(1, length(attSel))

    if (!identical(indPrim, integer(0))) {
      indSec <- indSec[-indPrim] # adjust secondary attribute indices
    }

    # PLOT PRIMARY ATTRIBUTES
    for (i in 1:length(indPrim)) {
      # GET PERCENT DIFF or ABS DIFF
      mdiff <- measure.diff(
        type = targetType[indPrim[i]],
        simPt = simPt[indPrim[i]],
        targetPt = target[indPrim[i]],
        pc.lim = pc.lim,
        diff.lim = diff.lim
      )

      # PLOT PERCENT/REL DIFF - PERFROMANCE CATEGORY INDICATED BY COLOUR
      plot_attrib_perf_solo(
        rel.diff = mdiff$diff.att,
        att.name = attSel[indPrim[i]],
        perf.lim = mdiff$class.lim,
        prim.lab = "Perturbed",
        targetType = targetType[indPrim[i]]
      )
    }
  }

  if (!identical(indSec, integer(0))) {
    # PLOT SECONDARY ATTRIBUTES
    for (i in 1:length(indSec)) {
      # GET PERCENT DIFF or ABS DIFF
      mdiff <- measure.diff(
        type = targetType[indSec[i]],
        simPt = simPt[indSec[i]],
        targetPt = target[indSec[i]],
        pc.lim = pc.lim,
        diff.lim = diff.lim
      )

      # PLOT PERCENT/REL DIFF - PERFROMANCE CATEGORY INDICATED BY COLOUR
      plot_attrib_perf_solo(
        rel.diff = mdiff$diff.att,
        att.name = attSel[indSec[i]],
        perf.lim = mdiff$class.lim,
        prim.lab = "",
        targetType = targetType[indSec[i]]
      )
    }
  }
  # PADDING PLOTS
  # if(npad>0){
  #   for (i in 1:npad){
  #     plot(1,type="n",ylab="",xlab="",xaxt="n",yaxt="n",frame.plot = FALSE)
  #   }
  # }
}

simTS.overlayMonthlyObsRange <- function(obsDat = NULL, # obsData
                                         simTS = NULL, # simulated timeseries
                                         datInd = NULL, # date index
                                         label = NULL, # plot labels
                                         range.mult = 0.5) {
  # MAKE POLYGON OF MONTHLY RANGES
  Tag <- "mon_min_dyAll"
  tmp.min <- rep(0, datInd$ndays)
  for (m in 1:12) {
    tmp.min[datInd$i.mm[[m]]] <- obsDat[[Tag]]$mean[m]
  }
  Tag <- "mon_max_dyAll"
  tmp.max <- rep(0, datInd$ndays)
  for (m in 1:12) {
    tmp.max[datInd$i.mm[[m]]] <- obsDat[[Tag]]$mean[m]
  }
  cord.y <- c(tmp.min[1:datInd$ndays], tmp.max[datInd$ndays:1], tmp.min[1])
  cord.x <- c(seq(1, datInd$ndays), seq(datInd$ndays, 1), 1)


  # windows(width=30,height=9)
  graphics::par(xpd = FALSE)
  # MAKE PLOTTING SPACE
  plot(1, type = "n", xlim = c(1, datInd$ndays), ylim = c(min(simTS), max(simTS) * range.mult), ylab = label, xlab = "days", xaxs = "i")

  # ADD MONTHLY POLYGON
  graphics::polygon(x = cord.x, y = cord.y, col = "lemonchiffon1", border = NA)

  # ADD SIMULATED TIMESERIES
  graphics::lines(x = seq(1, datInd$ndays), y = simTS[1:datInd$ndays], col = "black") # add rain time series

  # ADD OBS MEAN AS OVERLAY
  Tag <- "mon_mean_dyAll"
  dymean.mon <- rep(0, datInd$ndays)
  for (m in 1:12) {
    dymean.mon[datInd$i.mm[[m]]] <- obsDat[[Tag]]$mean[m]
  }
  graphics::lines(x = seq(1, datInd$ndays), y = dymean.mon, col = "red", lwd = 2)

  graphics::legend("topleft",
    legend = c("sim.", "obs. mean (mon)", "obs. range (mon)"), horiz = FALSE, seg.len = c(1, 1, 1),
    col = c("black", "red", NA), fill = c(NA, NA, "lemonchiffon1"), border = c(NA, NA, "black"), lwd = c(1, 1, 1), bg = "white"
  )
}

#-------------------------------
# exposure space visuals 2d or gridded layout
expSpace2dViz <- function(x = NULL, # vector of one attribute
                          y = NULL, # vector of one attribute
                          x.lab = NULL,
                          y.lab = NULL) {
  # determine ranges
  y.range <- c(min(y, na.rm = TRUE), max(y, na.rm = TRUE))
  x.range <- c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))

  # plot
  plot(x = x, y = y, type = "p", pch = 16, col = "blue", xlab = x.lab, ylab = y.lab, xlim = x.range, ylim = y.range, frame.plot = FALSE)
}
# tester
# expSpace2dViz(x=c(1,2,3,4,1,2,3,4,1,2,3,4),y=c(1,1,1,1,2,2,2,2,3,3,3,3),x.lab="Ptot%",y.lab="Temp(+deg)")


# #-------------------------------------------------
# frontBoilerPlateInfo<-function(modelTag=NULL,
#                                targetLocn=NULL,
#                                spot=NULL,
#                                nTarget=NULL,
#                                attSel=NULL,
#                                attPrim=NULL,
#                                optimArgs=NULL,
#                                sim=NULL,
#                                simVar=NULL
#                               ){
#   #text col
#   t.col="dodgerblue3"
#
#   # MAKE BLANK UN-BORDERED PLOTTING SPACE
#   graphics::par(mar=c(1,1,1,1),oma=c(1,1,1,1))
#   plot(1,type="n",xlim=c(1,100),ylim=c(1,100),ylab="",xlab="",xaxs="i",xaxt="n",yaxt="n",frame.plot=FALSE,xpd=FALSE)
#
#   #THINGS TO PLOT IN ALL CASES
#   graphics::polygon(c(0,100,100,0,0),
#           c(93,93,118,118,93),
#           border=FALSE,
#           col=grDevices::adjustcolor("green3",alpha.f=0.1))
#
#   line.no=0
#   #TARGET INFORMATION
#   graphics::text(x = 50,y=(98-line.no*10),labels=paste("Target", spot, "of",nTarget,sep=" " ),cex=2.5,col=t.col)
#   nacross=4
#   if(length(attSel)>nacross){
#     nlot=ceiling(length(attSel)/4)   #split into lots of 3
#     nrem=length(attSel)%%nacross           #get remainder
#     line.no=line.no+0.5
#     for(i in 1:nlot){
#       line.no=line.no+0.5
#       nstart=(i-1)*nacross+1
#       if((i == nlot)&(nrem>0)){nfin=nstart+(nrem-1) }else{ nfin=nstart+(nacross-1)}
#       chunk=seq(nstart,nfin)
#       if(i == 1){
#         graphics::text(x=50,y=(100-line.no*10),labels=paste("Target location:",paste(attSel[chunk],": ",targetLocn[chunk],sep="",collapse = ",    "),sep=" "),font = 2,col=t.col)
#       }else{
#         graphics::text(x=50,y=(100-line.no*10),labels=paste(attSel[chunk],": ",targetLocn[chunk],sep="",collapse = ",    "),font = 2,col=t.col)  #cap at 3 across
#       }
#     }
#   }else{
#     line.no=line.no+1.0
#     graphics::text(x=50,y=(100-line.no*10),labels=paste("Target location:",paste(attSel,": ",targetLocn,sep="",collapse = ",    "),sep=" "),font = 2,col=t.col)
#   }
#
#   #PRIMARY ATTRIBUTES (IF ANY)
#   if(!is.null(attPrim)){
#     line.no=line.no+0.75
#     graphics::text(x = 50,y=(100-line.no*10),labels=paste("Primary attributes:",paste(attPrim,collapse=",  ")),cex=1.0,font=2,col=t.col)
#   }
#
#   #RUN INFORMATION
#   line.no=line.no+1
#   graphics::text(x = 50,y=(100-line.no*10),labels=paste("Simulation run with the following properties"),cex=2.0,col=t.col)
#
#   line.no=line.no+0.75
#   graphics::text(x = 50,y=(100-line.no*10),labels=paste("Models Used:",paste(modelTag,collapse=",  ")),cex=1.0,font=2,col=t.col)
#
#   line.no=line.no+0.5
#   graphics::text(x = 50,y=(100-line.no*10),labels=paste("Variables perturbed:",paste(simVar,collapse=",  ")),cex=1.0,font=2,col=t.col)
#
#   #THINGS TO PLOT IF STOCHASTIC SIMULATION USED
#   if(modelTag[1] != "Simple-ann"){
#     line.no=line.no+0.5
#     graphics::text(x=50,y=(100-line.no*10),labels="Optimisation used: GA",font = 2,col=t.col)
#     line.no=line.no+0.5
#     graphics::text(x=50,y=(100-line.no*10),labels=paste("Max no. iterations:",optimArgs$maxiter,sep=" "),font = 2,col=t.col)
#     line.no=line.no+0.5
#     graphics::text(x=50,y=(100-line.no*10),labels=paste("Crossover:",optimArgs$pcrossover,sep=" "),font = 2,col=t.col)
#     line.no=line.no+0.5
#     graphics::text(x=50,y=(100-line.no*10),labels=paste("Mutation:",optimArgs$pmutation,sep=" "),font = 2,col=t.col)
#     line.no=line.no+0.5
#     graphics::text(x=50,y=(100-line.no*10),labels=paste("Population size:",optimArgs$popSize,sep=" "),font = 2,col=t.col)
#     line.no=line.no+0.5
#     graphics::text(x=50,y=(100-line.no*10),labels=paste0("Lambda(",attPrim,"): ",optimArgs$lambda.mult,collapse = ", "),font = 2,col=t.col)
#   }else{
#     line.no=line.no+0.5
#     graphics::text(x=50,y=(100-line.no*10),labels="Simple scaling used",font = 2,col=t.col)
#   }
#
#   #BOTTOM BORDER POLYGON
#   graphics::polygon(c(0,100,100,0,0),
#           c(0,0,5,5,0),
#           border=FALSE,
#           col=grDevices::adjustcolor("green3",alpha.f=0.1))
#
# }
#
# # heading: heading of the line
# # lineInfo: information to be printed on the line as a vector (eg: attPerturb)
# # Anjana: may need to change x here for portrait A4 page
#
# printLines <- function(heading, lineInfo, line.no, t.col, nacross = 3) {
#
#   if(length(lineInfo) > nacross){
#
#     nlot=ceiling(length(lineInfo)/nacross)         #split into lots
#     nrem=length(lineInfo)%%nacross           #get remainder
#     line.no.new=line.no+0.5
#
#     for(i in 1:nlot){
#       line.no.new=line.no.new+0.75
#       nstart=(i-1)*nacross+1
#       if((i == nlot)&(nrem>0)){nfin=nstart+(nrem-1) }else{ nfin=nstart+(nacross-1)}
#       chunk=seq(nstart,nfin)
#       if(i == 1){
#         graphics::text(x=50, y=(100-line.no.new*5), labels=paste(heading, ":",paste(lineInfo[chunk], sep="", collapse = ",    "), sep = " "),font = 2,col=t.col)
#       }else{
#         graphics::text(x=50, y=(100-line.no.new*5), labels=paste(lineInfo[chunk], sep="", collapse = ",    "), font = 2, col=t.col)  #cap at 3 across
#       }
#     }
#   }else{
#     line.no.new=line.no+1
#     graphics::text(x=50,y=(100-line.no.new*5),labels=paste(heading, ":",paste(lineInfo, sep="", collapse = ",    "),sep=" "),font = 2,col=t.col)
#   }
#   #line.no.out <- line.no
#   return(line.no.new)
# }

# #-------------------------------------------------
# frontPageScenarios <- function(sim,
#                                simName = NULL
#                                ) {
#   simNameNote <- NULL
#
#   # random simulation name
#   if (is.null(simName)) {
#     wordVector <- c(tools::toTitleCase(rcorpora::corpora("animals/common")[["animals"]]),
#                     tools::toTitleCase(rcorpora::corpora("plants/plants")[["instruments"]][["name"]]),
#                     tools::toTitleCase(rcorpora::corpora("geography/rivers")[["rivers"]][["name"]]),
#                  tools::toTitleCase(rcorpora::corpora("foods/fruits")[["fruits"]]),
#                  tools::toTitleCase(rcorpora::corpora("foods/vegetables")[["vegetables"]]),
#                  tools::toTitleCase(rcorpora::corpora("games/pokemon")[["pokemon"]][["name"]]),
#                  tools::toTitleCase(rcorpora::corpora("materials/gemstones")[["gemstones"]]))
#
#     simNameFull <- paste0("Simulation Name", expression("\206"), ": ", wordVector[round(stats::runif(1)*length(wordVector))])
#     simNameNote <- paste0(expression("\206"), "Name automatically generated")
#   } else {
#     simNameFull <- paste0("Simulation Name:", simName)
#   }
#
#   # get names of variables in simulation
#   # simFields <- names(sim[["Rep1"]][["Target1"]])
#   # varNames <- simFields[-which(simFields %in% c("attSim", "targetSim", "parS", "score"))]
#   varNames <- names(sim[["controlFile"]][["modelType"]])
#   varFull <- NULL
#   for (i in 1:length(varNames)) {
#     varFull[i] <- varShortToLong[varNames[i]]
#   }
#
#   simDate <- format(Sys.time(), "%b %d %Y %R")
#
#   # saving lengthly names here to save typing
#   n <- "controlFile"
#   m1 <- "modelType"
#   m2 <- "modelParameterVariation"
#   o <- "optimisationArguments"
#
#   m1Long <- "model type"
#   m2Long <- "model parameter variation"
#
#   # text col
#   t.col="dodgerblue3"
#   p.col <- "green3"
#
#   # MAKE BLANK UN-BORDERED PLOTTING SPACE
#   graphics::par(mar=c(1,1,1,1),oma=c(1,1,1,1))
#   plot(1,type="n",xlim=c(1,100),ylim=c(1,100),ylab="", xlab="", xaxs="i", xaxt="n",yaxt="n",frame.plot=FALSE,xpd=NA)
#   #plot(1,type="n",xlim=c(1,100),ylim=c(1,100),xaxt="n",yaxt="n",frame.plot=FALSE,xpd=NA)
#
#   #THINGS TO PLOT IN ALL CASES
#   graphics::polygon(c(0,100,100,0,0),
#           c(93,93,118,118,93),
#           border=FALSE,
#           col=grDevices::adjustcolor(p.col,alpha.f=0.1))
#
#   nTarg <- dim(sim$expSpace$targetMat)[1]
#   nAtt <- dim(sim$expSpace$targetMat)[2]
#   nRep <- length(sim) - 3
#
#   attPerturb <- sim[["expSpace"]][["attPerturb"]]
#   attHold <- sim[["expSpace"]][["attHold"]]
#
#   attPFull <- NULL
#   for (i in 1:length(attPerturb)){
#     attPFull[i] <- tagBlender(attPerturb[i])
#   }
#   attHFull <- NULL
#   for (i in 1:length(attHold)){
#     attHFull[i] <- tagBlender(attHold[i])
#   }
#
#   line.no=0
#   #SCENARIOS INFORMATION
#   graphics::text(x = 50,y=(98-line.no*5),labels=simNameFull,cex=1.5,col=t.col)
#   graphics::text(x = 88,y=(95-line.no*5),labels=simDate,cex=1,col=t.col)
#
#   line.no <- line.no + 2.25
#   # text(x = 50,y=(100-line.no*5),labels=paste("Number of Targets = ", nTarg, ", Attributes = ", nAtt, ", Replicates = ", nRep), font = 2, col=t.col)
#   # line.no <- line.no + 0.5
#   line.no.new <- printLines("Simulation variables", varFull, line.no, t.col)
#   line.no <- line.no.new + 0.5
#   line.no.new <- printLines("Perturbed attributes (P)", attPFull, line.no, t.col)
#   line.no <- line.no.new
#
#   if (!is.null(attHold)) {
#     line.no=line.no + 0.5
#     line.no.new <- printLines("Held attributes (H)", attHFull, line.no, t.col)
#     line.no <- line.no.new
#   } else {
#     line.no <- line.no + 1.5
#     graphics::text(x = 50,y=(100-line.no*5),labels=paste("Held attributes (H): There are no held attributes"), font = 2, col=t.col)
#   }
#
#   line.no <- line.no + 1.5
#   graphics::text(x = 50,y=(100-line.no*5),labels=paste("Number of Targets = ", nTarg, ", Attributes = ", nAtt, ", Replicates = ", nRep), font = 2, col=t.col)
#
#   #BOTTOM BORDER POLYGON
#   graphics::polygon(c(0,100,100,0,0),
#           c(0,0,5,5,0),
#           border=FALSE,
#           col=grDevices::adjustcolor(p.col,alpha.f=0.1))
#
#   if (!is.null(simNameNote)) {
#     graphics::text(x = 15, y = 2.5, labels = simNameNote, cex = 1, col = t.col)
#   }
#
# }
#
#
# advancedPageScenarios <- function(sim) {
#
#   # saving lengthly names here to save typing
#   n <- "controlFile"
#   m1 <- "modelType"
#   m2 <- "modelParameterVariation"
#   o <- "optimisationArguments"
#
#   m1Long <- "Model type"
#   m2Long <- "Model parameter variation"
#
#   # text col
#   t.col="dodgerblue3"
#   p.col <- "green3"
#
#   # MAKE BLANK UN-BORDERED PLOTTING SPACE
#   graphics::par(mar=c(1,1,1,1),oma=c(1,1,1,1))
#   plot(1,type="n",xlim=c(1,100),ylim=c(1,100),ylab="", xlab="", xaxs="i", xaxt="n",yaxt="n",frame.plot=FALSE,xpd=NA)
#   #plot(1,type="n",xlim=c(1,100),ylim=c(1,100),xaxt="n",yaxt="n",frame.plot=FALSE,xpd=NA)
#
#   #THINGS TO PLOT IN ALL CASES
#   graphics::polygon(c(0,100,100,0,0),
#           c(93,93,118,118,93),
#           border=FALSE,
#           col=grDevices::adjustcolor(p.col,alpha.f=0.1))
#
#   attPenalty <- sim[[n]][["penaltyAttributes"]]
#   penaltyWeights <- sim[[n]][["penaltyWeights"]]
#
#   line.no=0
#   #SCENARIOS INFORMATION
#   graphics::text(x = 50,y=(98-line.no*5),labels="Advanced Model and Optimisation Settings",cex=1.5,col=t.col)
#   #text(x = 90,y=(94-line.no*5),labels=simDate,cex=0.8,col=t.col)
#
#   line.no=line.no + 3
#
#
#   # line.no <- line.no + 0.75
#   # Anjana: add line or polygon here
#
#   # #RUN INFORMATION
#   # line.no=line.no + 1.25
#   # text(x = 50,y = (100-line.no*5),labels=paste("Simulation model and optimisation settings"),cex=2.0,col=t.col)
#
#   # line.no <- line.no + 0.75
#
#   if (is.character(sim[[n]])) {
#     if (sim[[n]] == "scaling") {
#       # line.no = line.no+1
#       graphics::text(x=50,y=(100-line.no*5),labels="Simple scaling used",font = 2,col=t.col)
#     }
#   } else {
#     nVars <- length(sim[[n]][[m1]])
#     varNames <- names(sim[[n]][[m1]])
#     varFull <- NULL
#     for (i in 1:nVars) {
#       varFull[i] <- varShortToLong[varNames[i]]
#     }
#
#     for (v in 1:nVars) {
#       labelText <- paste(varFull[v], paste(paste(m1Long, sim[[n]][[m1]][v], sep = " = "),
#                                                          paste(m2Long, sim[[n]][[m2]][v], sep = " = "), sep = ", "), sep = ": ")
#       #labelText <- paste("Variable ", labelText)
#       line.no=line.no + 0.75
#       graphics::text(x = 50,y = (100-line.no*5),labels=labelText, cex=1.0, font=2, col=t.col)
#     }
#
#     line.no=line.no+1.5
#     graphics::text(x=50,y=(100-line.no*5),labels="Optimisation used: GA",font = 2,col=t.col)
#     line.no=line.no+0.75
#     graphics::text(x=50,y=(100-line.no*5),labels=paste("Max no. iterations:",sim[[n]][[o]]$maxiter,sep=" "),font = 2,col=t.col)
#     line.no=line.no+0.75
#     graphics::text(x=50,y=(100-line.no*5),labels=paste("Crossover:",sim[[n]][[o]]$pcrossover,sep=" "),font = 2,col=t.col)
#     line.no=line.no+0.75
#     graphics::text(x=50,y=(100-line.no*5),labels=paste("Mutation:",sim[[n]][[o]]$pmutation,sep=" "),font = 2,col=t.col)
#     line.no=line.no+0.75
#     graphics::text(x=50,y=(100-line.no*5),labels=paste("Population size:",sim[[n]][[o]]$popSize,sep=" "),font = 2,col=t.col)
#   }
#
#   line.no <- line.no + 0.5
#   if (!is.null(attPenalty)) {
#     attPtyFull <- NULL
#     for (i in 1:length(attPenalty)){
#       attPtyFull[i] <- paste0(tagBlender(attPenalty[i]), " (Lambda = ", penaltyWeights[i], ")")
#     }
#     line.no.new <- printLines("Penalty attributes (*)", attPtyFull, line.no, t.col, nacross = 1)
#     line.no <- line.no.new
#     #line.no <- line.no + 0.5
#     #graphics::text(x = 50,y = (100-line.no*5),labels=paste0("Penalty weights: ", paste(penaltyWeights, collapse = ", ")),font = 2,col=t.col)
#   } else {
#     line.no <- line.no + 1
#     graphics::text(x = 50,y = (100-line.no*5),labels="There are no penalty attributes", font=2, col=t.col)
#   }
#
#   #BOTTOM BORDER POLYGON
#   graphics::polygon(c(0,100,100,0,0),
#           c(0,0,5,5,0),
#           border=FALSE,
#           col=grDevices::adjustcolor(p.col,alpha.f=0.1))
#
# }

