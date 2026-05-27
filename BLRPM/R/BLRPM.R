#################################################################################################################
##' Bartlett-Lewis Rectangular Pulse Model
##'
##' Model description (Rodriguez-Iturbe et al., 1987):
##'
##' The model is a combination of 2 poisson processes and simulates storms and cells.
##' During the given simulation time storms are generated in a poisson process with rate lambda.
##' Those storms are given a exponetially distributed duration with parameter gamma. 
##' During its duration the storm generates in a second poisson process cells with rate beta.
##' The first cell has to be instantaneous at the time of the storm arrival.
##' The cell duration is exponentially distributed with parameter eta. For the whole lifetime
##' each cell is given a constant intensity which is exponentially distributed with parameter 1/mux.
##'
##' Aggregation:
##'
##' The intensities of all cells alive at time t are summed up for total precipitation at time t.
##' 
##' Parameter estimation:
##' 
##' The model parameters (lambda,gamma,beta,eta,mux) can be estimated from simulated or 
##' observed precipitation time series using the method of moments. Certain moments, e.g. mean, variance 
##' can be calculated from the time series at different aggregation levels. These moments
##' can also be calculated theoretically from model parameters. Both sets of statistics can be 
##' compared in an objective function, similar to a squared error estimator. By numerical optimization
##' the model parameters can be tuned to match the time series characteristics. 
###############################################################################################################

#########################
## Part 1: Simulation ##
#########################

##' \code{BL.sim} generates model realisations of storms and cells by using given model
##' parameters \code{lambda,gamma,beta,eta,mux} for a given simulation time \code{t.sim}
##' @title Simulating storms and cells
##' @param lambda value specifying the generation rate of storms [1/h]
##' @param gamma value specifying the storm duration [1/h]
##' @param beta  value specifying the generation rate of cells [1/h]
##' @param eta value specifying the cell duration [1/h]
##' @param mux value specifying the cell intensity [mm/h]
##' @param t.sim value specifying the simulation time [h]
##' @return \code{BL.sim} returns storms; \code{data.frame} of all storms containing information about occurence time, end time and number of cells
##' @return \code{BL.sim} returns cells; \code{data.frame} of all cells containing information about occurence time, end time, intensity and storm index
##' @author Christoph Ritschel \email{christoph.ritschel@@met.fu-berlin.de}
##' @examples 
##' lambda <- 4/240
##' gamma <- 1/10
##' beta <- 0.3
##' eta <- 2
##' mux <- 4
##' t.sim <- 240
##' simulation <- BL.sim(lambda,gamma,beta,eta,mux,t.sim)
##' @export
BL.sim <- function(lambda=4/240,gamma=1/10,beta=0.3,eta=2,mux=4,t.sim=240) {
  
  ## initialize output list for cells
  cells <- NULL
  
  ################################################
  ## 1st layer: storms
  
  ## generate n.s storms in a poisson process with rate parameter lambda for a given
  ## simulation time t.sim
  ## expectation is lambda times t.sim
  n.s <- rpois(1,lambda*t.sim)
  
  if(n.s > 0) { ## if there is at least one storm... 
    
    ## storms are uniform distributed over the simulation time t.sim
    s.s <- runif(n.s,0,t.sim)           
    
    ## storm duration is exponentially distributed with parameter gamma
    ## expectation of storm duration d.s is 1/gamma
    ## calculating end time of storms
    d.s <- rexp(n.s,gamma)          
    e.s <- s.s+d.s              
    
    ## write information about storms into a data.frame
    PL <- array(NA,dim=c(n.s))  ## place holder for number of cells
    storms.matrix <- cbind(s.s,e.s,PL) 
    dimnames(storms.matrix)[[2]] <- c("start","end","n.cells")
    storms <- as.data.frame(storms.matrix)
    
    ########################################################
    ### 2nd layer: cell generation for each storm
    
    for(i in 1:n.s) {  # loop over storms
      
      ## generate n.c cells in a poisson process with rate parameter beta over storm duration
      ## there has to be at least one cell for each storm
      ## expectation of n.c is 1+beta/gamma
      n.c <- rpois(1,beta*d.s[i])+1   
      
      storms[i,3] <- n.c  # write number of cells into storm dataframe 
      
      ## cells are uniform distributed over storm duration
      ## constraint: first cell has to be active instantaneous at storm occurence time
      s.c <- c(s.s[i],runif(n.c-1,s.s[i],e.s[i])) 
      
      ## cell duration is exponentially distributed with parameter eta
      ## expectation ov cell duration is 1/eta
      d.c <- rexp(n.c,eta) 
      e.c <- s.c+d.c # end time of cells
      
      ## cell intensity is exponentially distributed with parameter 1/mux
      ## expectation of cell intensity is mux
      int <- rexp(n.c,1/mux)                     
      
      ## Schreibe Zellinformationen in einen data.frame
      cells.new <- cbind(s.c,e.c,int,i)      
      cells <- rbind(cells,cells.new) # bind cells of each storm
      
    }  # end loop over all storms
    
    dimnames(cells)[[2]] <- c("start","end","int","i")
    cells <- as.data.frame(cells)
    
  }else { ## if there is no storm
    
    cells <- NULL
    storms <- NULL
    
  } ## end if storms...
  
  ## return dataframes of storms and cells
  return(list("cells"=cells,"storms"=storms))
  
} # end of function BL.sim
########################################################################################

#####################################################################
### accumulated time series ###
#####################################################################

##' \code{BL.stepfun} calculates a continous stepfunction of precipitation from
##' the \code{data.frame} \code{cells}  
##' @title BLRPM continous stepfunction of precipitation
##' @param cells \code{data.frame} of all cells containing information about occurence time, end time, intensity and storm index
##' @return sfn returns stepfunction of precipitation
##' @usage BL.stepfun(cells)
##' @author Christoph Ritschel \email{christoph.ritschel@@met.fu-berlin.de}
##' @examples 
##' lambda <- 4/240
##' gamma <- 1/10
##' beta <- 0.3
##' eta <- 2
##' mux <- 4
##' t.sim <- 240
##' simulation <- BL.sim(lambda,gamma,beta,eta,mux,t.sim)
##' stepfun <- BL.stepfun(simulation$cells)
##' @export
BL.stepfun <- function(cells) {
  
  ## get start and end times of cells
  ## for start times associate positive cell intensities, for end times negative intensities
  t <- c(cells$start,cells$end) 
  p <- c(cells$int,cells$int*(-1))
  cells.new <- cbind(t,p)
  
  ### sort the cells for cumulative stepfunction
  cells.sort <- cells.new[sort.int(cells.new[,1],index.return=TRUE)$ix,]
  cells.sort[,2] <- cumsum(cells.sort[,2]) ## Cumsum 
  
  ## stepfunction
  sfn <- stepfun(cells.sort[,1],c(0,cells.sort[,2])) 
  
  ## return stepfunction
  return(sfn) 
  
} # End of Function BL.stepfun
#######################################################################################

##' \code{BL.acc} accumulates the BLRPM stepfunction for a given accumulation time \code{t.acc}
##' at a given accumulation level \code{acc.val}. An \code{offse} can be defined. The unit is typically hours. 
##' @title Accumulation of a precipitation stepfunction 
##' @param sfn stepfunction of precipitation 
##' @param t.acc \code{value} specifiying the length of accumulated time series [h]
##' @param acc.val \code{value} specifying the accumulation level [h]
##' @param offset \code{value} specifying the offset of the accumulated time series [h]
##' @return p.acc \code{data.frame} 
##' @author Christoph Ritschel \email{christoph.ritschel@@met.fu-berlin.de}
##' @examples 
##' lambda <- 4/240
##' gamma <- 1/10
##' beta <- 0.3
##' eta <- 2
##' mux <- 4
##' t.sim <- 240
##' t.acc <- t.sim
##' acc.val <- 1
##' offset <- 0
##' 
##' simulation <- BL.sim(lambda,gamma,beta,eta,mux,t.sim)
##' sfn <- BL.stepfun(simulation$cells)
##' ts <- BL.acc(sfn,t.acc,acc.val,offset)
##' @export
BL.acc <- function(sfn,t.acc=240,acc.val=1,offset=0) {
  
  ## end of accumulation time series 
  end <- t.acc+offset
  
  ## sequence of accumulation time steps
  bins <- seq(from=offset,to=end,by=acc.val) 
  
  ## sorting knotpoints of sfn and bins
  kn <- sort(unique(c(knots(sfn),bins)))
  delta.kn <- diff(kn)
  
  ## calculate cumulative sums
  csum <- cumsum(sfn(kn[1:(length(kn)-1)])*delta.kn)
  
  ### add zero for start
  csum <- c(0,csum)
  
  ## precipitiation is difference between sums of each bins
  p <- c(diff(csum[is.element(kn,bins)]))
  
  ## write precipitation and time information in data.frame
  p.acc <- as.data.frame(cbind(bins[2:length(bins)],p))
  names(p.acc) <- c("time","RR")
  
  ## return data.frame
  return(p.acc)
  
} # End of Function BL.acc
###########################################################################

##' \code{TS.acc} accumulates a given time series \code{x} at a given accumulation level \code{acc.val}. Minimum value
##' for acc.val is 2 [unit time]
##' @title Accumulation of a time series
##' @param x \code{vector} of a time series
##' @param acc.val \code{value} specifying the accumulation level, minimum value is 2
##' @return x.acc \code{TS.acc} returns a \code{vector} of an accumulated time series 
##' @author Christoph Ritschel \email{christoph.ritschel@@met.fu-berlin.de}
##' @usage TS.acc(x,acc.val)
##' @examples 
##' x <- rgamma(1000,1)
##' x.2 <- TS.acc(x,acc.val=2)
##' @export
TS.acc <- function(x,acc.val=2) {
  
  ## check for input value of acc.val
  if(acc.val<1) cat(paste("Warning: accumulation value acc.val too small for accumulation of the time series \n"))
  
  l.new <- length(x)%/%acc.val ## calculate new length of accumulated time series
  l.rest <- length(x)%%acc.val ## calculate values left over
  if(l.rest==0) {
    x.acc <- apply(matrix(x,nrow=l.new,byrow=T),1,sum) 
  }else{
    x.acc <- apply(matrix(x[1:(length(x)-l.rest)],nrow=l.new,byrow=T),1,sum)   
    cat(paste("Warning: ",l.rest,"time steps left and not used for accumulation \n"))
  }
  
  ## return accumulated time series
  return(x.acc)
  
} # End of function TS.acc
#####################################################################################


###################################################
### calculate statistics of a given time series ###
###################################################

##' \code{TS.stats} calculates statistics of a given time series \code{x} at given accumulation
##' levels \code{acc.vals}. The calculated statistics are the mean of the first accumulation level,
##' the variance, auto-covariance lag-1 and the probability of zero rainfall of all given accumulation 
##' levels of the time series. These statistics are needed for estimating the BLRPM parameters.
##' @title calculating statistics of a time series needed for parameter estimation
##' @param x \code{vector} of a time series
##' @param acc.vals \code{vector} of accumulation levels, first value should be 1 
##' @return stats \code{TS.stats} returns a \code{vector} of statistics calculated at given accumulation levels
##' @author Christoph Ritschel \email{christoph.ritschel@@met.fu-berlin.de}
##' @usage TS.stats(x,acc.vals)
##' @examples
##' time.series <- rgamma(1000,shape=1)
##' statistics <- TS.stats(time.series,acc.vals=c(1,3,12,24)) 
##' @export
TS.stats <- function(x,acc.vals=c(1,3,12,24)) {
  
  ## initialize array
  Statistik <- array(NA,dim=c(length(acc.vals),3))
  
  ## loop over accumulation values
  for(i in 1:length(acc.vals)) {
    
    ts <- TS.acc(x,acc.vals[i])
    
    if(i==1) stats.mean <- mean(ts,na.rm=T) # Mean for first level of aggregation
    Statistik[i,1] <- var(ts,na.rm=T) # Var for all agg levels
    Statistik[i,2] <- acf(ts,lag.max=1,type="covariance",plot=F)$acf[2,1,1] 
    Statistik[i,3] <- length(ts[ts==0])/length(ts) # Prob.Zero 24h   
  }
  stats <- c(stats.mean,as.vector(Statistik))
  names(stats) <- c(paste("mean.",acc.vals[1],sep=""),paste("var.",acc.vals,sep=""),
                    paste("cov.",acc.vals,sep=""),paste("pz.",acc.vals,sep=""))
  
  ## return vector of statistics
  return(stats)
  
} # End of function TS.stats
#########################################################################################





##' \code{BLRPM.class} defines a new class for objects of type BLRPM containing the information about storms,
##' cells, stepfunction and the precipitation time series.
##' @title BLRPM class
##' @author Christoph Ritschel \email{christoph.ritschel@@met.fu-berlin.de}
BLRPM.class <- R6Class("BLRPM",
                 public = list(
                   storms = NA,
                   cells = NA,
                   sfn = NA,
                   RR = NA,
                   time = NA,
                   initialize = function(storms, cells, sfn, RR, time) {
                     if (!missing(storms)) self$storms <- storms
                     if (!missing(cells)) self$cells <- cells
                     if (!missing(cells)) self$sfn <- sfn
                     if (!missing(cells)) self$RR <- RR
                     if (!missing(cells)) self$time <- time
                   })
)
###########################################################################

##' \code{BLRPM.sim} is the main function for simulating precipitation with the Bartlett-Lewis rectangular pulse model.
##' It generates storms and cells using the given five BLRPM parameters \code{lambda, gamma, beta, eta, mux} for a given
##' simulation time \code{t.sim}. The function \code{BLRPM.sim} then accumulates a precipitation time series of length 
##' \code{t.akk} (typically the same as t.sim) with an accumulation time step \code{interval} from the generated
##' storms and cells. An \code{offset} can be used to delay the precipitation time series for initialization reasons.
##' \code{BLRPM.sim} returns a list of different variables and data.frames: \code{Storms, Cells, Stepfun, Precip, time}.
##' @title Simulating precipitation with the BLRPM
##' @param lambda \code{value} specifying the expected storm generation rate [1/units.time]
##' @param gamma \code{value} specifying the expected storm duration[1/units.time]
##' @param beta \code{value} specifying the expected cell generation rate [1/units.time]
##' @param eta \code{value} specifying the expected cell duration [1/units.time]
##' @param mux \code{value} specifying the expected cell intenstity [mm/unit.time]
##' @param t.sim \code{value} specifying the simulation length [units.time]
##' @param t.acc \code{value} specifying the length of the accumulated time series [units.time]. 
##'                           Note: if longer than t.sim only zeros are added after t.sim.
##' @param interval \code{value} specifying the accumulation time step [units.time]
##' @param offset \code{value} specifying the offset of the accumulated time series with 
##' respect to the start time of the simulation [units.time]. Note: negative values are not allowed.
##' @return $storms returns \code{data.frame} containing information about storms: start, end, number of cells
##' @return $cells returns \code{data.frame} containing information about cells: start, end, intensity, storm index
##' @return $sfn returns \code{stepfunction} used to accumulate precipitation time series
##' @return $RR returns \code{vector} of accumulated precipitation with time step \code{interval} [mm/interval]
##' @return $time returns \code{vector} of time steps [interval]
##' @usage BLRPM.sim(lambda,gamma,beta,eta,mux,t.sim,t.acc,interval,offset)
##' @examples 
##' lambda <- 4/240
##' gamma <- 1/10
##' beta <- 0.3
##' eta <- 2
##' mux <- 4
##' t.sim <- 240
##' t.acc <- t.sim
##' interval <- 1
##' offset <- 0
##' simulation <- BLRPM.sim(lambda,gamma,beta,eta,mux,t.sim,t.acc=t.sim,interval,offset)
##' @author Christoph Ritschel \email{christoph.ritschel@@met.fu-berlin.de}
##' @export
BLRPM.sim <- function(lambda=4/240,gamma=1/10,beta=0.3,eta=2,mux=4,t.sim=240,t.acc=t.sim,interval=1,offset=0) {
  
  ## if offset negative: warning and abort
  if(offset<0) {cat("Warning: offset value negative \n")
  }else{
    
    t.sim <- t.sim+2*offset
    
    ## Simulate storms and cells with given parameters
    sim <- BL.sim(lambda,gamma,beta,eta,mux,t.sim)
    
    ## if at least one storm and one cell is generated
    ## accumulate precipitation time series
    if(!is.null(sim$cells)) {
      sfn <- BL.stepfun(sim$cells)
      p.acc <- BL.acc(sfn,t.acc,interval,offset)
      
      
      
      BLRPM <- BLRPM.class$new(sim$storms,sim$cells,sfn,p.acc$RR,p.acc$time)
      
      ## return data.frames and vectors
      return(BLRPM)
      
    }else cat("Warning: no storm generated. Possibly increase simulation time t.sim or storm generation parameter lambda \n") 
  }
  
} ## End of function BLRPM.sim
################################################################

#######################
### part2: plotting ###
#######################

##' \code{plot.BLRPM} plots an object of \code{class} BLRPM returned by the function \code{BLRPM.sim} with an option to plot
##' either only the storms and cells or to additionally plot the stepfunction and the precipitation time series
##' in a multiframe plot.
##' @title Plotting of an object of \code{class} BLRPM
##' @param x \code{class} BLRPM object which is returned by function \code{BLRPM.sim}
##' @param OSC \code{logical} determing type of plot. OSC=True only storms and cells are plotted. OSC=FALSE storms, cells, 
##' stepfunction and precipitation time series plotted.
##' @param start.time \code{numerical} value setting the starting time of a time window to be plotted. Default is NULL, therefore start time is 0
##' @param end.time \code{numerical} value setting the end time of a time window to be plotted. Default is NULL, meaning the plot will 
##' end with the last active cell
##' @param legend \code{logical} setting the option for legend to be plotted or not
##' @param c.axis \code{numerical} value for axis label size, default is 1.5
##' @param c.lab \code{numerical} value for plot label size, default is 1.5
##' @param c.legend \code{numerical} value for legend font size, default is 1.5  
##' @param ... Arguments to be passed to methods, such as \link{graphical parameters} (see \code{\link{par}}). 
##' @seealso \code{\link{plot}}
##' @examples 
##' lambda <- 4/240
##' gamma <- 1/10
##' beta <- 0.3
##' eta <- 2
##' mux <- 4
##' t.sim <- 240
##' t.acc <- t.sim
##' interval <- 1
##' offset <- 0
##' simulation <- BLRPM.sim(lambda,gamma,beta,eta,mux,t.sim,t.acc=t.sim,interval,offset)
##' plot(simulation,OSC=FALSE)
##' \donttest{
##' plot(simulation,OSC=TRUE,start.time=1,end.time=24)
##' }
##' @author Christoph Ritschel \email{christoph.ritschel@@met.fu-berlin.de}
##' @import R6
##' @method plot BLRPM
##' @export
plot.BLRPM <- function(x,...,OSC=FALSE,start.time=NULL,end.time=NULL,legend=TRUE,c.axis=1.5,c.lab=1.5,c.legend=1.5) {
  
  ## variables
  cells <- x$cells
  storms <- x$storms
  sfn <- x$sfn
  RR <- x$RR
  time <- x$time
  
  ## IF only Cells and Storms are to be plotted...
  if(OSC) {
    
    par(oma=c(0,0,0,0))
    par(mar=c(4.2,4,0.3,0))
    
    max.int <- max(cells$int)
    
    #max.cell <- max(storms$Anzahl.Zellen/(storms$end-storms$start))
    
    if(is.null(start.time)){
      start <- 0
      l <- max(cells$end)
    }else if(start.time < 0 | is.infinite(start.time)){
    cat("Warning: start time can not be negative or infinite \n")
  }else{
      start <- start.time
      l <- start.time + 2
      }
    
    if(is.null(end.time)){
      end <- max(cells$end)
    }else if(end.time < 0 | is.infinite(end.time)){
  cat("Warning: end time can not be negative or infinite \n")
  }else{end <- end.time}
    
    
    ## Plot Window and Legend
    par(oma=c(0,0,0,0))
    par(mar=c(5,5,1,1)) 
    plot(NULL,ylim=c(-1/10*(max.int+4.5*max.int/10),max.int+4.5*max.int/10),xlim=c(start,end),cex.lab=c.lab,xlab="time [h]",ylab="cell intensity [mm/h]",axes=F)
    axis(2,cex.axis=c.axis)
    axis(1,cex.axis=c.axis)
    
    if(legend) {
      
    if(is.null(start.time)){
      l <- max(cells$end)
      polygon(x=c(l/60,l/60,l/6.5,l/6.5),y=c(max.int*1,max.int*1.35,max.int*1.35,max.int*1),col=rgb(1,1,1,0))
      polygon(x=c(l/50,l/50,l/20,l/20),y=c(max.int*1.18,max.int*1.25,max.int*1.25,max.int*1.18),col=rgb(1,0,0,0.4),border=NA)
      polygon(x=c(l/50,l/50,l/20,l/20),y=c(max.int*1.08,max.int*1.15,max.int*1.15,max.int*1.08),col=rgb(0,0,1,0.4),border=NA)
      text(x=l/10,y=max.int*1.24,labels="storm",cex=c.legend)
      text(x=l/10,y=max.int*1.14,labels="cell",cex=c.legend)
    }else{
      l <- start.time 
      polygon(x=c(l,l,l+1.7*c.legend,l+1.7*c.legend),y=c(max.int*1.1,max.int*1.3,max.int*1.3,max.int*1.1),col=rgb(1,1,1,0))
      polygon(x=c(l+1.3*c.legend,l+1.3*c.legend,l+1.6*c.legend,l+1.6*c.legend),y=c(max.int*1.23,max.int*1.28,max.int*1.28,max.int*1.23),col=rgb(1,0,0,0.4),border=NA)
      polygon(x=c(l+1.3*c.legend,l+1.3*c.legend,l+1.6*c.legend,l+1.6*c.legend),y=c(max.int*1.13,max.int*1.18,max.int*1.18,max.int*1.13),col=rgb(0,0,1,0.4),border=NA)
      text(x=l,y=max.int*1.25,labels="storm",cex=c.legend,pos=4)
      text(x=l,y=max.int*1.15,labels="cell",cex=c.legend,pos=4)
      
    }
    }
      
    for(j in 1:length(storms$start)) { # Plot Polygon of each Storm [Start-End]
      
      Start <- storms$start[j]
      Ende <- storms$end[j]
      #cell.factor <- storms$Anzahl.Zellen[j]/(Ende-Start)/max.cell
      #system(paste("echo",cell.factor))
      polygon(x=c(Start,Start,Ende,Ende),y=c(0,-1/10*(max.int+1),-1/10*(max.int+1),0),col=rgb(1,0,0,0.4),border=NA)
    }
    
    for(k in 1:length(cells$start)) { # Plot Polygon of each Cell [Start-End]
      
      Start <- cells$start[k]
      Ende <- cells$end[k]
      int <- cells$int[k]
      polygon(x=c(Start,Start,Ende,Ende),y=c(0,0+int,0+int,0),col=rgb(0,0,1,0.5),border=NA)
    }
    
    
  ## else plot Storms and Cells, the cumulative stepfunction and the accumulated precipitation time series
  ## one plot window with 3 subfigures
  }else{
  
  
  ## Multiple Plot (3 Plots in 1 Window)  
  par(mfrow=c(3,1)) 
  par(mar=c(1,4,1,2))
  
  ## axes lim
  max.int <- max(cells$int)
  l <- max(time)
  
  if(is.null(start.time)){
    start <- min(time)
  }else{start <- start.time}
  
  if(is.null(end.time)){
    end <- max(time)
  }else{end <- end.time}
  
  ## initialize plot window
  plot(NULL,ylim=c(-1/10*(max.int+1),max.int+1),xlim=c(start,end),xlab="",ylab="cell intensity [mm/h]",axes=F)
  axis(2)
  
  ## legend
  polygon(x=c(l/60,l/60,l/10,l/10),y=c(max.int*0.82,max.int*1.13,max.int*1.13,max.int*0.82),col=rgb(1,1,1,0))
  polygon(x=c(l/50,l/50,l/30,l/30),y=c(max.int*0.99,max.int*1.07,max.int*1.07,max.int*0.99),col=rgb(1,0,0,0.4),border=NA)
  polygon(x=c(l/50,l/50,l/30,l/30),y=c(max.int*0.86,max.int*0.95,max.int*0.95,max.int*0.86),col=rgb(0,0,1,0.4),border=NA)
  text(x=l/15,y=max.int*1.035,labels="storm",font=1)
  text(x=l/15,y=max.int*0.92,labels="cell",font=1)
  
  ## plot storms as polygons [Start-End]
  for(j in 1:length(storms$start)) {
    
    Start <- storms$start[j]
    Ende <- storms$end[j]
    polygon(x=c(Start,Start,Ende,Ende),y=c(0,-1/10*(max.int+1),-1/10*(max.int+1),0),col=rgb(1,0,0,0.4),border=NA)
  }
  
  ## plot cells as polygons [Start-End]
  for(k in 1:length(cells$start)) { 
    
    Start <- cells$start[k]
    Ende <- cells$end[k]
    int <- cells$int[k]
    polygon(x=c(Start,Start,Ende,Ende),y=c(0,0+int,0+int,0),col=rgb(0,0,1,0.5),border=NA)
  }
  
  ## plot stepfunction
  par(mar=c(1,4,1,2))
  plot(sfn,do.points=0,xlim=c(start,end),xlab="",ylab="cell intensity [mm/h]",axes=F,main="")
  axis(2)
  
  ## plot accumulated time series
  par(mar=c(5,4,1,2))
  plot(start:end,RR[start:end],type="h",xlab="time [h]",ylab=paste("precipitation [mm/h]"),frame.plot=F)
  
  layout(1)
  } ## end if(OSC)
} ## End of Function plot.BLRPM
#####################################################################################################

####################################
### part 3: parameter estimation ###
####################################

##' \code{Beta.fun} is a help function for \code{OF}
##' @title Beta function needed in objective function
##' @param a \code{value} specifying Parameter a
##' @param b \code{value} specifying Parameter b
##' @return beta returns value of \code{Beta.fun} for parameters a and b
##' @author Christoph Ritschel \email{christoph.ritschel@@met.fu-berlin.de}
Beta.fun <- function(a,b) {
  
  Beta <- gamma(a)*gamma(b)/gamma(a+b)
  return(Beta)
  
} ## End of function Beta.fun
####################################################################

##' \code{Delta.fun} is a help function for \code{OF}
##' @title Delta function needed in objective function
##' @param kappa \code{value} specifying Parameter kappa
##' @param MStrich \code{value} specifying dimension of error correction in objective function
##' @return Delta returns value of \code{Delta.fun} for kappa and MStrich
##' @author Christoph Ritschel \email{christoph.ritschel@@met.fu-berlin.de}
Delta.fun <- function(kappa,MStrich) {
  
  MStrich.Reihe <- 0:MStrich
  
  SummeMstrich <- (kappa^MStrich.Reihe)/factorial(MStrich.Reihe)
  
  Delta <- exp(kappa)-sum(SummeMstrich)
  return(Delta) 
  
} # End of function Delta.fun
############################################################################

##' \code{BLRPM.OF} is the objective function used for parameter estimation of the BLRPM parameters. 
##' Given a set of BLRPM parameters \code{par} this function calculates a set of model statistics at
##' given accumulation time steps \code{acc.vals}. These model statistics are compared with given
##' time series statistics \code{stats} in the objective function. The user is able to define \code{weights}
##' for each statistic (has to be the same length as statistics input vector). Option for debugging is given.
##' A \code{scale} parameter defines a criterium for which different kinds of model statistics are calculated.
##' This criterium is mainly based on the timescale difference between storm duration parameter gamma and cell duration
##' parameter eta.
##' If \code{use.log} is true, the objective function needs logarithmic input parameters. The value
##' of \code{OF} defines the kind of objective function to be used: 1= quadratic 2= quadratic extended 3= absolute 4= absolute extended.
##' @title BLRPM objective function for parameter estimation
##' @param par \code{vector} specifying the five model parameters (lambda,gamma,beta,eta,mux) at which the objective function is to be calculated
##' @param stats \code{vector} specifying the time series statistics to which the model is compared to
##' @param acc.vals \code{vector} specifying the accumulation time steps at which the model statistics are calculated
##' @param weights \code{vector} specifying the weight of each statistic in the objective function. 
##' Note: has to have the same length as \code{stats}
##' @param debug \code{logical} defining if debuggging of function has to be done
##' @param scale \code{value} spacifying the scale factor for comparisson between duration parameters gamma and eta
##' @param use.log \code{logical} defining if input parameters are logarithmic
##' @param OF \code{value} specifying the type of objective function. 1: quadratic, 2: quad extended, 3: absolute, 4: abs extended
##' @return Z returns value of objective function for input parameters and input statistics
##' @author Christoph Ritschel \email{christoph.ritschel@@met.fu-berlin.de}
BLRPM.OF <- function(par,stats,acc.vals=c(1,3,12,24),
                             weights=rep(1,length(stats)),debug=FALSE,scale=1,
                             use.log=TRUE,OF=2) {
  
  ## Log-Paramter Input?
  ## logarithmierte Parameter verhindern negative Werte geschaetzter Parameter
  if(use.log){
    lambda <- exp(par[1])
    gamma <- exp(par[2])
    beta <- exp(par[3])
    eta <- exp(par[4])
    mux <- exp(par[5])  
  }else{
    lambda <- par[1]
    gamma <- par[2]
    beta <- par[3]
    eta <- par[4]
    mux <- par[5]
  }
  
  ## Debug erlaubt es die Ziefunktionsausgaben fuer jeden Zwischenschritt in eine
  ## Auslagerungsdatei zu schreiben und anschliessend zu kontrollieren
  if(debug==T){ 
    debug.values <- c(lambda,gamma,beta,eta,mux)
  }
  
  ## Fallunterscheidung: kleine Werte fuer Gamma und Beta im Vergleich zu eta
  
  ## Fall 1: Bedingung erfuellt
  if( beta<(scale*eta) && gamma<(scale*eta)) {
    
    ## Benoetigte Werte fuer Prob.Zero
    mu.T <- 1/gamma*(1+(gamma*(beta+(gamma/2))/(eta^2))-((gamma*(5*gamma*beta+(beta^2)+2*(gamma^2)))/(4*eta^3))+
                       (gamma*(4*(beta^3)+31*(beta^2)*gamma+99*beta*(gamma^2)+36*(gamma^3)))/(72*(eta^4)))
    G.PStern <- 1/gamma*(1-((beta+gamma)/eta)+((3*beta*gamma+2*(gamma^2)+(beta^2))/(2*(eta^2))))
    
    if(debug) debug.values <- c(debug.values,NA,NA,NA,NA)
      
    ## Bedingung nicht erfuellt
  } else {
    
    ## Grenzen fuer M und MStrich, je groesser, desto geringer der Fehler
    M.Reihe <- 1:10
    M <- 10
    MStrich.Reihe <- 0:10
    MStrich <- 10
    
    kappa <- beta/eta # Variablenumformulierung
    phi <- gamma/eta # Variablenumformulierung
    
    suppressWarnings( {
    beta.val1 <- beta(M.Reihe+1,phi)
    beta.val2 <- beta(MStrich.Reihe+phi,2)
    }
    )
    
    SummeM <- (-1*(kappa^(M.Reihe-1))*(kappa-(M.Reihe^2)-M.Reihe))/factorial(M.Reihe*(M.Reihe+1))*beta.val1
    SummeMstrich <- (kappa^MStrich.Reihe)/factorial(MStrich.Reihe)*beta.val2
    
    ## Benoetigte Werte fuer Prob.Zero
    mu.T <- 1/eta*(1+phi*sum(SummeM)+1/phi)
    G.PStern <- 1/eta*exp(-kappa)*sum(SummeMstrich)+Delta.fun(kappa,MStrich)/((MStrich+phi+1)*(MStrich+phi+2))
    if(debug) debug.values <- c(debug.values,kappa,phi,sum(beta.val1),sum(beta.val2))
    
  } # Ende Fallunterscheidung
  
  ## Bestimmung restlicher Variablen:
  mu.c <- 1+beta/gamma
  
  ## AkkumulationsZeiten:
  h <- acc.vals
  
  stats.tmp <- array(NA,dim=c(length(acc.vals),3))
  ## Schleife ueber alle Akkumulationszeiten
  for(i in 1:length(h)) {
      ## Mittel der Niederschlagsint  first aggregation step
      if(i==1) stats.tmp.mean <- (lambda*h[i]*mux*mu.c)/eta
      
      ## Varianz der Niederschlagsint
      stats.tmp[i,1] <- 2*lambda*mu.c/eta*(
          (2*(mux^2)+(beta*(mux^2)/gamma))*h[i]/eta+(
              ((mux^2)*beta*eta*(1-exp(-gamma*h[i])))/((gamma^2)*((gamma^2)-(eta^2))))-
          ((2*(mux^2)+((beta*gamma*(mux^2))/(gamma^2-eta^2)))*((1-exp(-eta*h[i]))/(eta^2))))
    
      ## Autokovarianz
      k <- 1 #lag
      stats.tmp[i,2] <- ((lambda*mu.c)/eta)*(
          ((2*(mux^2)+((beta*gamma*(mux^2))/((gamma^2)-(eta^2))))*((((1-exp(-1*eta*h[i]))^2)*exp(-1*eta*(k-1)*h[i]))/(eta^2)))-(
              ((mux^2)*beta*eta*((1-exp(-1*gamma*h[i]))^2)*exp(-1*gamma*(k-1)*h[i]))/((gamma^2)*((gamma^2)-(eta^2)))))
    
    ## Probability of zero Rainfall
      stats.tmp[i,3] <- max(1e-8,min(1,exp((-lambda*(h[i]+mu.T))+(lambda*G.PStern*(gamma+beta*exp(-(beta+gamma)*h[i])))/(beta+gamma)),na.rm=T),na.rm=T)
      
  } ## Ende Schleife ueber alle Akkumulationszeiten
  stats.model <- c(stats.tmp.mean,as.vector(stats.tmp))
  
  ## O.Funktionsformulierung
  ## Unterscheidung durch OF
  ## OF = 1 : quadratisch
  ## OF = 2 : quadratisch erweitert
  ## OF = 3 : absolut
  ## OF = 4 : absolut erweitert
  
#  stats <- c(stats[1,1],stats[2,],stats[3,],stats[4,])
#  stats.model <- c(stats.model[1,1],stats.model[2,],stats.model[3,],stats.model[4,])
  
#   if(OF==1) ZF <- sum(Gewichte %*% (((1-stats.model/stats))^2))
#   else if(OF==2) ZF <- sum(Gewichte * ((((1-stats.model/stats))^2)+(((1-stats/stats.model))^2)))
#   else if(OF==3) ZF <- sum(Gewichte * abs((stats.model/stats)-1))
#   else if(OF==4) ZF <- sum(Gewichte * (abs((stats.model/stats)-1)+abs(stats/stats.model-1)))

if(OF==1) Z <- weights %*% (((1-stats.model/stats))^2)
else if(OF==2) Z <- weights %*% ((((1-stats.model/stats))^2)+(((1-stats/stats.model))^2))
else if(OF==3) Z <- weights %*% abs((stats.model/stats)-1)
else if(OF==4) Z <- weights %*% (abs((stats.model/stats)-1)+abs(stats/stats.model-1))

  ## Debug?
  if(debug) {
    options(digits.secs=6)
    debug.values <- c(debug.values,stats.model,Z,as.character(Sys.time()))
    write(debug.values,file="optim.log",append=TRUE,ncolumns=length(debug.values))
  }
  
  ## Ausgabe des aktuellen Werts der O.Funktion
  return(Z)
 
} ## End of function BLRPM.OF
#############################################################################################################  


##' \code{BLRPM.est} estimates the five Bartlett-Lewis rectangular pulse model parameters \code{lambda,gamma,beta,eta,mux}
##' for a given time series \code{data}. At first the time series statistics at given accumulation levels \code{acc.vals}
##' are calculated. These statistics are given over to the parameter estimation algorithm together with 
##' parameter starting values \code{par}. An objective function \code{O.Fun} can be specified, default is \code{BLRPM.OF}.
##' In addition the weights for different statistics and accumulation levels \code{weights.mean, weights.var, weights.cov, weights.pz}
##' can be specified. For the BLRPM objective function the user can select the measure of distance between observation
##' and model with \code{OF}: =1 quadratic, =2: quad extended, =3: absolute, =4: abs extended.
##' A \code{scale} parameter controls different cases in the objective function for differences in the scale of duration
##' parameters gamma and eta.   
##' If a debugging is wished, \code{debug} can be set to \code{TRUE} and a log file is created in working directory.
##' Several \code{optim} parameters can be also defined. For specifics see \code{?optim}.
##' @title BLRPM Parameter Estimation function
##' @param RR \code{vector} of a precipitation time series
##' @param acc.vals \code{vector} of different accumulation levels at which statistics are to be calculated
##' @param pars.in \code{vector} specifying starting values of \code{lambda,gamma,beta,eta,mux} for optimization
##' @param O.Fun \code{objective function} to be used during optimization
##' @param weights.mean \code{value} for weight for mean value at first accumulation level
##' @param weights.var \code{vecotr} of weights for variances, has to have \code{length(acc.vals)}
##' @param weights.cov \code{vecotr} of weights for covariances, has to have \code{length(acc.vals)}
##' @param weights.pz \code{vecotr} of weights for probability of zero rainfall, has to have \code{length(acc.vals)}
##' @param OF \code{value} specifying the type of objective function. 1: quadratic, 2: quad symmetrized, 3: absolute, 4: abs symmetrized
##' Note: quadratic symmetrized proofed to be most effective and fastest
##' @param debug set \code{TRUE} if debugging is wished, default \code{FALSE}. Creates a log file in working directory
##' @param scale \code{value} specifying the scaling between gamma and eta in the objective function
##' @param method \code{character} defining the method to be used in \code{optim}, preferences are: "Nelder-Mead", "BFGS", "L-BFGS-B"e
##' @param lower \code{vector} specifying the lower boundary of parameters for "L-BFGS-B" method
##' @param upper \code{vector} specifying the upper boundary of parameters for "L-BFGS-B" method
##' @param use.log \code{logical}, set \code{TRUE} if logarithmic parameters during optimization should be used. Advantage: zero as lower boundary for parameters
##' @param maxit \code{value} specifying the maximum number of itereations durion optimization
##' @param ndeps \code{vector} specifying the change for each parameter during one interation step
##' @param trace \code{value} specifying output information of \code{optim}
##' @return $est returns \code{vector} of estimated parameters \code{lambda,gamma,beta,eta,mux}
##' @return $conv returns \code{value} of convergence of optimization, see \code{optim} for details
##' @return $mess returns \code{character} message about optimization if using "L-BFGS-B" method
##' @return $Z returns \code{value} of objective function for estimated parameters
##' @usage BLRPM.est(RR,acc.vals,pars.in,O.Fun,
##' weights.mean,weights.var,weights.cov,weights.pz,OF,debug,
##' scale,method,lower,upper,use.log,maxit,ndeps,trace)
##' @examples 
##' t.sim=240
##' 
##' lambda <- 4/240
##' gamma <- 1/10
##' beta <- 0.3
##' eta <- 2
##' mux <- 4
##' 
##' pars <- c(lambda,gamma,beta,eta,mux)
##' 
##' sim <- BLRPM.sim(lambda,gamma,beta,eta,mux,t.sim)
##' est <- BLRPM.est(sim$RR,pars.in=pars,method="BFGS",use.log=TRUE)
##' 
##' @author Christoph Ritschel \email{christoph.ritschel@@met.fu-berlin.de}
##' @export
BLRPM.est <- function(RR,acc.vals=c(1,3,12,24),pars.in=c(4/240,1/10,0.3,2,4),O.Fun=BLRPM.OF,
                      weights.mean=100,
                      weights.var=rep(1,length(acc.vals)),
                      weights.cov=rep(1,length(acc.vals)),
                      weights.pz=rep(1,length(acc.vals)),
                      OF=2,
                     debug=FALSE,scale=1e-3,method="BFGS",lower=-Inf,upper=Inf,
                     use.log=TRUE,maxit=2000,ndeps=rep(1e-3,length(pars.in)),trace=0) {

    ### calculate time series statistics at given accumulation levels
    stats <- TS.stats(RR,acc.vals)
    
  ## logarithmieren der Parameter und der Grenzen?
  if(use.log){
    pars.in <- log(pars.in) 
    
    if(method=="L-BFGS-B"){
      upper <- log(upper)
      lower <- log(lower)
    }
  }
  
  ## If Debug: initialisiere log Datei
  if(debug) {
    debug.names <- c("lambda","gamma","beta","eta","mux",
                     "kappa","phi","beta1","beta2",
                     "mean",
                     "var.1h","var.3h","var.12h","var.24h",
                     "acv.1h","acv.3h","acv.12h","acv.24h",
                     "probzero.1h","probzero.3h","probzero.12h","probzero.24h",
                     "ZF","Date","Time")
    write(debug.names,file="optim.log",append=FALSE,ncolumns=length(debug.names))
  }
  
  ## Check Statistik auf NAs
  if(sum(is.na(stats)) == 0)  {     
    
    ## Untersuche Startwert der O.Funktion: Kann Optimierung gestartet werden...
    ZF <- O.Fun(pars.in,acc.vals=acc.vals,stats=stats,use.log=use.log,OF=OF)
    
    ## Wenn Ok, dann Starte Optim
    #if((!is.infinite(ZF)) & (!is.na(ZF)) & (ZF<15000)) {
    if((!is.infinite(ZF)) & (!is.na(ZF))) { 
      
      ## Unterscheidung methodn -- Grenzen
      if(method=="L-BFGS-B") {  
        fit.optim <- optim(par=pars.in,O.Fun,stats=stats,OF=OF, #stats=Teststatistik[run,],
                           weights=c(weights.mean,weights.var,weights.cov,weights.pz),
                           debug=debug,scale=scale,method=method,use.log=use.log,lower=lower,upper=upper,
                           control=list(maxit=maxit,trace=trace,ndeps=ndeps))
        est <- fit.optim$par
        conv <- fit.optim$convergence
        mess <- fit.optim$message
        Z <- fit.optim$value
        
      }else{
        fit.optim <- optim(par=pars.in,O.Fun,stats=stats,OF=OF,acc.vals=acc.vals, #stats=Teststatistik[run,],
                           weights=c(weights.mean,weights.var,weights.cov,weights.pz),
                           debug=debug,scale=scale,method=method,use.log=use.log,#lower=lower,upper=upper,
                           control=list(maxit=maxit,ndeps=ndeps,trace=trace))
        est <- fit.optim$par
        conv <- fit.optim$convergence
        Z <- fit.optim$value
        
      }  
      
    }else{ # fuer den Fall fehlerhafter O.Funktionswerte
      
      est <- rep(NA,length(pars.in))
      conv <- NA
      mess <- NA
      Z <- NA
    } ## Ende Abfrage is.infinite...
    
  }else { ## Falls Statistiken Fehlerhaft...
    
    est <- rep(NA,length(pars.in))
    conv <- NA
    mess <- NA
    Z <- NA
  } ## Ende Abfrage Statistik
  
  if(use.log) { ## logarithmierte Parameter exponieren
    
    est <- exp(est)
    
  }
  
  names(est) <- c("lambda","gamma","beta","eta","mux")
  
  ## Ausgabe
  if(method=="L-BFGS-B") {
    return(list("est"=est,"conv"=conv,"mess"=mess,"Z"=Z))
  }else{
    return(list("est"=est,"conv"=conv,"Z"=Z))  
  }
  
} ## Ende Funktion BLRPM.est
###########################################################################################
