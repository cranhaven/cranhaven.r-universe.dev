#'
#' Find the otimum R value for a given data set
#'
#' @param object an RTIGER object
#' @param max_rigidity R values will be explored up the value given in this parameter. Default = 2^9
#' @param average_coverage For conservative results set it to the lowest average coverage of a sample in your experiment, or evne to the lowest average coverage in a (sufficiently large) region in one of your samples. The lower the value, the more conservative (higher) our estimates of the false positive segments rates. If it is not provided it will be computed as the average of all data points.
#' @param crossovers_per_megabase For conservative results set it to the highest ratio of a sample in your experiment. The higher the value, the more conservative (higher) our estimates of the false positive segments rates. If it is not provided it will be computed as the average of all samples.
#' @param save_it logical values if the results should be saved. Plots might be complicated to interpret. We suggest to read the manuscript to understand them (https://doi.org/10.1093/plphys/kiad191)
#' @param savedir if results are saved, in which directory.
#'
#' @return A value with the optimum rigidity for the data set.
#'
#' @usage optimize_R(object,
#' max_rigidity = 2^9, average_coverage = NULL, crossovers_per_megabase = NULL,
#' save_it = FALSE, savedir = NULL)
#'
#' @examples
#'
#' data("fittedExample")
#' bestR = optimize_R(myDat)
#'
#' @export optimize_R
#'

optimize_R = function(object,
                      max_rigidity = 2^9,
                      average_coverage = NULL,
                      crossovers_per_megabase = NULL,
                      save_it = FALSE,
                      savedir = NULL ){
  if(save_it & is.null(savedir)) stop("Please if you want to save the plots and results specify the path in savedir.\n")
  myDat = object
  seqlengths = seqlengths(myDat@Viterbi[[1]])
  if(is.null(average_coverage)){
    average_coverage = mean(sapply(myDat@Viterbi, function(x) mean(x$total)))
  }
  if(is.null(crossovers_per_megabase)){
    crossovers_per_megabase = mean(sapply(myDat@Viterbi, function(x) length(rle(x$Viterbi)$values)/sum(seqlengths)/1e6))
  }

  # EXTRACT EMISSIONS
  CO_min = crossovers_per_megabase # the CO frequency used to construct the lower bound
  CO_max = 10*CO_min # the CO frequency used to construct the upper bound

  picked_parameters = myDat@params
  transition_pars = picked_parameters$transition  # this is on the absolute scale
  emission_pars = extract_emissions(picked_parameters)
  number_of_samples = myDat@info$sample_nr

  chromosome_lengths = myDat@info$seqlengths
  n_chromosomes = length(chromosome_lengths)
  total_length = sum(chromosome_lengths)


# MAGIC CODE --------------------------------------------------------------
  states = c("mat","het","pat")

  # generate the grid for the u values on which FPR(u;r) and FNR(u;r) values will be evaluated
  max_segment_length = max_rigidity*2
  n_segments = 25
  segment_length_grid = pmin(2*ceiling(seq(1,(max_segment_length/2)^(1/4),length=n_segments)^4),max_segment_length)
  segment_length_grid = sort(unique(segment_length_grid))
  # ideally, we should use a grid with only ~3 significant binary digits (i.e., at most 3 columns in Delta_table
  # must be added up, this saves a lot of time)

  rigidity_gridpoints = 10
  rigidity_grid = unique(round((2^seq(from=3,to=log2(max_rigidity),
                                      length=rigidity_gridpoints))))
  n_rigidity = length(rigidity_grid)


  # generate the grid for the u values on which FPR(u;r) and FNR(u;r) values will be evaluated
  max_segment_length = max_rigidity*2
  n_segments = 25
  segment_length_grid = pmin(2*ceiling(seq(1,(max_segment_length/2)^(1/4),length=n_segments)^4),max_segment_length)
  segment_length_grid = sort(unique(segment_length_grid))
  # ideally, we should use a grid with only ~3 significant binary digits (i.e., at most 3 columns in Delta_table
  # must be added up, this saves a lot of time)


  Deltas = construct_Delta_table(n_obs = number_of_samples, # how many samples shall be constructed?
                                 max_segment_length = max_segment_length, # length of the largest segment to be evaluated
                                 coverage=average_coverage, # average number of observations per marker
                                 emissions=emission_pars # named (mat,het,pat) list with emission probabilities (named vector c(alpha=...,beta=...) in each list entry)
  )



  ### FPR

  # cat("Calculating FPRs.\n")
  # cat("Rigidity values (",n_rigidity,"): ",rigidity_grid[1],", ",sep="")
  FPR_grid = matrix(0,nrow=n_segments,ncol=n_rigidity)
  rownames(FPR_grid) = paste("Seg_",segment_length_grid,sep="")
  colnames(FPR_grid) = paste("r_",rigidity_grid,sep="")
  for (u in 1:n_segments){
    FPR_grid[u,1] = FPR(segment_length = segment_length_grid[u],
                        rigidity       = rigidity_grid[1],
                        Delta_table    = Deltas,
                        transitions    = transition_pars)$FPR
  }
  if (n_rigidity>1){
    for (r in 2:n_rigidity){
      #cat(rigidity_grid[r],", ")
      #if (all(FPR_grid[,r-1]==0)) break()
      for (u in 1:n_segments){
        FPR_grid[u,r] = FPR(segment_length = segment_length_grid[u],
                            rigidity       = rigidity_grid[r],
                            Delta_table    = Deltas,
                            transitions    = transition_pars)$FPR
      } # end for u
    } # end for r
  } # end if n_rigidity>1
  #cat("\n")


  ### FNR

  # cat("Calculating FNRs.\n")
  # cat("Rigidity values (",n_rigidity,"): ",rigidity_grid[1],", ",sep="")
  FNR_grid = matrix(0,nrow=n_segments,ncol=n_rigidity)
  rownames(FNR_grid) = paste("Seg_",segment_length_grid,sep="")
  colnames(FNR_grid) = paste("r_",rigidity_grid,sep="")
  for (u in 1:n_segments){
    FNR_grid[u,1] = FNR(segment_length = segment_length_grid[u],
                        rigidity       = rigidity_grid[1],
                        Delta_table    = Deltas,
                        transitions    = transition_pars)$FNR
  }
  if (n_rigidity>1){
    for (r in 2:n_rigidity){
      #cat(rigidity_grid[r],", ")
      for (u in 1:n_segments){
        FNR_grid[u,r] = FNR(segment_length = segment_length_grid[u],
                            rigidity       = rigidity_grid[r],
                            Delta_table    = Deltas,
                            transitions    = transition_pars)$FNR
      } # end for u
    } # end for r
  } # end if n_rigidity>1
  #cat("\n")



  ### SE plus

  # We need the chromosome lengths (in number of markeres)
  chromosome_lengths = c(35,22,25,20,31) * 10^6 # approx. A.thaliana chromosome lengths (according to TAIR)
  n_chromosomes = length(chromosome_lengths)
  total_length = sum(chromosome_lengths)

  # cat("Calculating SE+, lower and upper bound.\nRigidity values: ")
  SEplus = sapply(rigidity_grid,function(x){
    #cat(x,", ")

    inner_FPR = sapply(rigidity_grid,function(y){
      FPR(segment_length=y, # length of the segment for which the FNR is to be calculated
          rigidity=x, # rigidity value of the rHMM
          Delta_table=Deltas, # the Delta table constructed by the function above
          transitions=transition_pars)$FPR
    })

    selected = which(rigidity_grid >= x)
    minselected = min(selected)
    segment_number = total_length/(rigidity_grid[selected]+x)
    lower_SEplus = max(  segment_number * inner_FPR[selected] * (1-inner_FPR[minselected]) )
    upper_SEplus = total_length * max(inner_FPR / rigidity_grid)
    return(c(lower_SEplus = lower_SEplus,upper_SEplus=upper_SEplus))
  })
  # cat("\n")
  upper_SEplus = SEplus["upper_SEplus",]
  lower_SEplus = SEplus["lower_SEplus",]



  ### SE_minus

  # needs to be specified in the main script
  # CO_min = 0.05 # k_min in the manuscript (crossovers per megabase)
  # CO_max = 10*CO_min # k_max in the manuscript


  # cat("Calculating SE-, lower and upper bound.\nRigidity values.\n")

  lower_SEminus = SEminus(
    total_length, # the total size of the genome (in bp)
    n_chromosomes, # the number of chromosomes
    COs_per_megabase=CO_min, # the expected total number of COs per megabase
    FNR_grid,
    rigidity_grid,
    segment_length_grid)

  upper_SEminus = SEminus(
    total_length, # the total size of the genome (in bp)
    n_chromosomes, # the number of chromosomes
    COs_per_megabase=CO_max, # the expected total number of COs per megabase
    FNR_grid,
    rigidity_grid,
    segment_length_grid)


  ### SE_total and suggestion of the best rigidity value

  upper_SEtotal = upper_SEplus+upper_SEminus
  lower_SEtotal = lower_SEplus+lower_SEminus
  indx = which.min(upper_SEtotal)
  rigidity_suggestion = rigidity_grid[indx]
  best_SEtotal = upper_SEtotal[indx]


# Plotting results (save_it = TRUE) ---------------------------------------

  if(save_it){
    options(scipen=999)
    n_rigidity = length(rigidity_grid)
    appendix = "Optimization-step"

    # FPR plot

    pdf(file.path(savedir, paste("FPR_plot_",appendix,".pdf",sep="")))
    plot(c(0,0), ylim=c(0,max(FPR_grid*100)),xlim=c(0.95,max(segment_length_grid)*2),
         type="n", xlab="Segment_length",ylab="FPR [%]",log="x")
    abline(h=0,col="grey")
    title(paste(c("FPR (segment_length,rigidity) , coverage = ",coverage),collapse=""))
    colorpal = rainbow(n_rigidity+3)[1:n_rigidity]
    lwidths = rep(2,n_rigidity) # (rigidity_grid==150)*1.5 + 1
    for (j in 1:n_rigidity){
      points(segment_length_grid,FPR_grid[,j]*100,type="l",col=colorpal[j],lwd = lwidths[j])
    }
    legend("topright",bty="n",legend = c("rigidity",rigidity_grid),col=c("white",colorpal),lty=1,lwd=2)
    dev.off()


    # FNR plot

    pdf(file.path(savedir, paste("FNR_plot_",appendix,".pdf",sep="")))
    plot(c(0,0), ylim=c(0,max(FNR_grid*100)),xlim=c(0.95,max(segment_length_grid)*2),
         type="n", xlab="Segment_length",ylab="FNR [%]",log="x")
    abline(h=0,col="grey")
    title(paste(c("FNR (segment_length,rigidity) , coverage = ",coverage),collapse=""))
    colorpal = rainbow(n_rigidity+3)[1:n_rigidity]
    lwidths = rep(2,n_rigidity) # (rigidity_grid==150)*1.5 + 1
    for (j in 1:n_rigidity){
      points(segment_length_grid,FNR_grid[,j]*100,type="l",col=colorpal[j],lwd = lwidths[j])
    }
    legend("topright",bty="n",legend = c("rigidity",rigidity_grid),col=c("white",colorpal),lty=1,lwd=2)
    dev.off()



    # Segmentation error per sample: SE+, SE-, SE_total plot

    mindisplay = 10^-5
    transform = function(x){log10(x+mindisplay)}
    backtransform = function(x){10^x-mindisplay}

    y_minmax = transform(c(0,10^5)) # max(c(upper_SEplus,lower_SEplus,upper_SEminus,lower_SEminus))))
    axpos1 = pretty(y_minmax)
    axpos = transform( backtransform(axpos1) + mindisplay ) [-1]
    axlabs = as.character(signif(backtransform(axpos),digits=3))

    x_minmax = range(c(rigidity_grid,rigidity_grid))

    pdf(file.path(savedir, paste("Segmentation_error_plot_",appendix,".pdf",sep="")),width=8,height=6)
    par(mar=c(5, 5.5, 4, 2.5) + 0.1)
    plot(rigidity_grid,transform(upper_SEplus), log="x",
         main=paste("Segmentation errors per sample\ncoverage = ",coverage,sep=""),
         xlab="Rigidity",
         ylab="",
         yaxt="n",
         ylim=y_minmax,
         xlim=x_minmax,
         type="n")
    title(ylab = paste("Wrong segments",sep=""),
          mgp = c(4, 3, 1))
    axis(side=2,at=axpos,labels=axlabs,las=1)
    axis(side=2,at=transform(0),labels=0,las=1)
    abline(h=transform(c(0,1)),col="grey")


    abline(v=rigidity_suggestion,lty=1,col="grey")

    linecolors = c(total="violet",pos="red",neg="blue")
    linewidths = c(total=3.5,pos=2,neg=1.8)

    # Total error (upper and lower bound)
    points(rigidity_grid,transform(upper_SEminus+upper_SEplus),type="l",
           col=linecolors["total"],lty=1,lwd=linewidths["total"])
    points(rigidity_grid,transform(lower_SEminus+lower_SEplus),type="l",
           col=linecolors["total"],lty=2,lwd=linewidths["total"])
    # False segment calls (upper and lower bound)
    points(c(rigidity_grid,x_minmax[2]),c(transform(upper_SEplus),transform(0)),type="l",
           col=linecolors["pos"],lwd=linewidths["pos"])
    points(rigidity_grid,transform(lower_SEplus),type="l",
           col=linecolors["pos"],lty=2,lwd=linewidths["pos"])
    # Missed segments (upper and lower bound)
    points(rigidity_grid,transform(upper_SEminus),type="l",
           col=linecolors["neg"],lty=1,lwd=linewidths["neg"])
    points(rigidity_grid,transform(lower_SEminus),type="l",
           col=linecolors["neg"],lty=2,lwd=linewidths["neg"])
    # add the text which marks the minimum error
    vpos = ifelse(best_SEtotal>1,10^-3,10)
    hpos = ifelse(rigidity_suggestion>10^4,2,4)
    text(rigidity_suggestion,transform(vpos),
         labels = paste("min total error at\nrigidity = ",rigidity_suggestion,sep=""),
         pos=4,offset=0.5)
    # add the legend for line colors and style
    legend("topright",legend=c("false segments","missed segements","total error",
                               "","upper bound","lower bound"),
           col = c(linecolors[c("pos","neg","total")],"white","dark grey","dark grey"),
           lty = c(1,1,1, 1, 1,2),lwd=2,bty="n",bg="white")
    dev.off()

    # save the results file
    resultsfile = file.path(savedir, paste("Simulation_results_",appendix,".RData",sep=""))
    save(
      coverage,
      rigidity_grid,
      upper_SEplus,
      lower_SEplus,
      upper_SEminus,
      lower_SEminus,
      upper_SEtotal,
      lower_SEtotal,
      segment_length_grid,
      FPR_grid,
      FNR_grid,
      rigidity_suggestion,
      best_SEtotal,
      file = resultsfile)
  }
  return(rigidity_suggestion)


}


#  Auxiliar functions -----------------------------------------------------

#'  utility function converting dec to reverse binary
#' @param num decimal numbers
#'
#' @keywords internal
#' @noRd
#'
#'

# utility function converting dec to reverse binary
dec2bin <- function(num){
  if (num %/% 2 == 0) return((num %% 2)==1)
  return(c((num %% 2==1),dec2bin(num %/% 2)))
} #dec2bin


#' Delta_table[x,y,m=segment_lengths,n_obs=#samples] contains, for each segment length and each observation, the probability for the observations in a given segment length generated by state x, evaluated as coming from y
#' @param n_obs HOw many samples shall be constructed
#' @param max_segment_length length of the largest segment to be evaluated
#' @param coverage average number of observations per marker
#' @param emissions named (mat,het,pat) list with emission probabilities (named vector c(alpha=...,beta=...) in each list entry)
#'
#' @keywords internal
#' @noRd
#'
# Delta_table[x,y,m=segment_lengths,n_obs=#samples] contains, for each segment length and each observation,
# the probability for the observations in a given segment length generated by state x, evaluated as coming from y
construct_Delta_table = function(n_obs = 10^4, # how many samples shall be constructed?
                                 max_segment_length = 2^10-1, # length of the largest segment to be evaluated
                                 coverage=1, # average number of observations per marker
                                 emissions # named (mat,het,pat) list with emission probabilities (named vector c(alpha=...,beta=...) in each list entry)
){

  # pre-calculate a lookup table <probs> of dimension (states,0:max_n_counts+1,0:max_n_counts+1)
  # with the entries probs[state,k,n] = Betabin(k-1;n-1,alpha[state],beta[state])

  states = c("mat","het","pat")
  max_n = ceiling(coverage * 2.5 + 8) # the largest number of observations per marker we cover (should never be seen)

  probs = array(NA,dim=c(3,max_n+1,max_n+1))
  dimnames(probs) = list(states,NULL,NULL)
  log_probs = array(NA,dim=c(3,max_n+1,max_n+1))
  dimnames(log_probs) = list(states,NULL,NULL)

  for (state in states){
    for (n in 0:max_n){
      probs[state,,n+1] = dbbinom(0:max_n, size=n,
                                  alpha= emissions[[state]]["alpha"],
                                  beta = emissions[[state]]["beta"], log=FALSE) * dpois(n,lambda=coverage)
      log_probs[state,,n+1] = dbbinom(0:max_n, size=n,
                                      alpha= emissions[[state]]["alpha"],
                                      beta = emissions[[state]]["beta"], log=TRUE) + dpois(n,lambda=coverage,log=T)
    } # end for n
  } # end for state
  log_probs[log_probs== -Inf] = 0

  # Initialize the Delta_table array
  max_m = floor(log2(max_segment_length))+1
  Delta_table = array(NA, dim = c(3,3,max_m,n_obs))
  dimnames(Delta_table) = list(states,states,NULL,NULL)

  # construct the Delta_table values for segments of length 2^(m-1) , m = 1,...,max_m
  #cat("Constructing Delta table with ",n_obs," observations.\n")
  #cat("Segment lengths (",max_m,"): ",sep="")
  for (m in 1:max_m){

    segment_length = 2^(m-1)
    #cat(segment_length,", ")

    for (x_state in states){
      for (y_state in states){

        # "construct <n_obs> summary tables of observations (k,n) drawn from x_state
        tables = rmultinom(n_obs,size=segment_length,prob = probs[x_state,,])
        # evaluate these observations with the probabilities of the y_state
        Delta_table[x_state,y_state,m,] = colSums(tables * as.vector(log_probs[y_state,,]))

      } # end for y_state
    } # end for x_state

  } # end for m
  #cat("\n")

  return(Delta_table)
} # end construct_Delta_table

#' Compute the False positiv rate values
#' @param segment_length length of the segment for which the FPR is to be calculated
#' @param rigidity rigidity value of the rHMM
#' @param Delta_table the Delta table constructed by the construct_Delta_table funciton
#' @param transitions the transition matrix
#'
#' @keywords internal
#' @noRd
#'

FPR = function(segment_length, # length of the segment for which the FPR is to be calculated
               rigidity, # rigidity value of the rHMM
               Delta_table, # the Delta table constructed by the function above
               transitions # the transition matrix
){

  log_transitions = log(transitions)
  states = c("mat","het","pat")
  n_obs = dim(Delta_table)[4]
  FPR_xy = matrix(0,nrow=3,ncol=3,dimnames=list(states,states))
  if (segment_length<rigidity) return(list(FPR=0,FPR_xy=FPR_xy))

  # sum up the appropriate columns of the Delta_table values
  pick_m = which(dec2bin(segment_length))
  Delta_reduced = apply(Delta_table[,,pick_m,,drop=F],c(1,2,4),sum)

  for (flanking_state in states){
    for (central_state in setdiff(states,flanking_state)){

      # calculate the transition penalty for switching to another state
      constant = log_transitions[flanking_state,central_state] +
        log_transitions[central_state,flanking_state] -
        2 * log_transitions[flanking_state,flanking_state] +
        (segment_length-rigidity)*(log_transitions[flanking_state,flanking_state]-
                                     log_transitions[central_state,central_state])

      # calculate the relative frequency of the Delta value defined in the lyx being positive
      FPR_xy[flanking_state,central_state] = sum(Delta_reduced[flanking_state,central_state,] + constant >
                                                   Delta_reduced[flanking_state,flanking_state,]) / n_obs
    } # end central_state
  } # end flanking_state

  # weight the state-specific FPR rates to obtain a global FPR
  FPR = 1/4 * sum(FPR_xy["mat",]) + 1/4 * sum(FPR_xy["pat",]) + 1/2 * sum(FPR_xy["het",])
  return(list(FPR=FPR,FPR_xy=FPR_xy))
} # end FPR

#' Compte the False Negative Rate (FNR)
#' @param segment_length length of the segment for which the FPR is to be calculated
#' @param rigidity rigidity value of the rHMM
#' @param Delta_table the Delta table constructed by the construct_Delta_table funciton
#' @param transitions the transition matrix
#'
#' @keywords internal
#' @noRd
#'

FNR = function(segment_length, # length of the segment for which the FNR is to be calculated
               rigidity, # rigidity value of the rHMM
               Delta_table, # the Delta table constructed by the function above
               transitions # the transition matrix
){

  log_transitions = log(transitions)
  states = c("mat","het","pat")
  n_obs = dim(Delta_table)[4]
  FNR_xy = matrix(NA,nrow=3,ncol=3,dimnames=list(states,states))

  if (segment_length >= rigidity){

    pick_m = which(dec2bin(segment_length))
    Delta_reduced = apply(Delta_table[,,pick_m,,drop=F],c(1,2,4),sum)


    for (flanking_state in states){
      for (central_state in setdiff(states,flanking_state)){

        # calculate the transition penalty for switching to another state
        constant = log_transitions[flanking_state,central_state] +
          log_transitions[central_state,flanking_state] -
          2 * log_transitions[flanking_state,flanking_state] +
          (segment_length-rigidity)*(log_transitions[flanking_state,flanking_state]-
                                       log_transitions[central_state,central_state])

        # calculate the relative frequency of the Delta value defined in the lyx being positive
        FNR_xy[flanking_state,central_state] = sum(
          Delta_reduced[central_state,central_state,] + constant <=
            Delta_reduced[central_state,flanking_state,] ) / n_obs
      } # end central_state
    } # end flanking_state

  } else {
    # in case segment_length < rigidity do

    pick_m = which(dec2bin(segment_length))
    Deltay_reduced = apply(Delta_table[,,pick_m,,drop=F],c(1,2,4),sum)
    pick_mminusu = which(dec2bin(rigidity-segment_length))
    Deltax_reduced = apply(Delta_table[,,pick_mminusu,,drop=F],c(1,2,4),sum)

    for (flanking_state in states){
      for (central_state in setdiff(states,flanking_state)){

        # calculate the transition penalty for switching to another state
        constant = log_transitions[flanking_state,central_state] +
          log_transitions[central_state,flanking_state] -
          2 * log_transitions[flanking_state,flanking_state]

        # calculate the relative frequency of the Delta value defined in the lyx being positive
        FNR_xy[flanking_state,central_state] = sum(
          Deltay_reduced[central_state,central_state,] + Deltax_reduced[flanking_state,central_state,] + constant <=
            Deltay_reduced[central_state,flanking_state,] + Deltax_reduced[flanking_state,flanking_state,]) / n_obs
      } # end central_state
    } # end flanking_state
  } # end (if segment_length < rigidity)

  # weight the state-specific FNR rates to obtain a global FNR
  FNR = 1/3 * sum(FNR_xy["mat","het"]) + 1/3 * sum(FNR_xy["pat","het"]) + 1/6 * sum(FNR_xy["het",c("pat","mat")])
  return(list(FNR=FNR,FNR_xy=FNR_xy))
} # end FNR

#' Computations of the segment errors
#' @param total_length the total size of the genome (in bp)
#' @param n_chromosomes The number of chromosomes the organism has
#' @param COs_per_megabase The expected total number of COs per megabase
#' @param FNR_grid a matrix containing information of how many segments (rows) have been missed for each R (columns).
#' @param rigidity_grid The values of R for which it will be inspected the performance.
#'
#' @keywords internal
#' @noRd
#'

SEminus = function(total_length, # the total size of the genome (in bp)
                   n_chromosomes, # the number of chromosomes
                   COs_per_megabase, # the expected total number of COs per megabase
                   FNR_grid,
                   rigidity_grid,
                   segment_length_grid
){
  expected_COs = total_length/10^6*COs_per_megabase
  max_k = max(qpois(10^-7,expected_COs,lower.tail=FALSE),2)
  n_rigidity = length(rigidity_grid)
  n_segments = length(segment_length_grid)

  p_ulk = matrix(0, nrow = n_segments, ncol = max_k+1)
  for (k  in 1:max_k){
    p_ulk[,k+1] = diff(pbeta(c(0.5,segment_length_grid)/total_length,1,k+n_chromosomes))
  } # end for k

  k_vec = dpois(0:max_k, lambda = expected_COs)

  u_mat =  FNR_grid * diff(c(0,segment_length_grid))

  SE_minus = t(u_mat) %*% p_ulk %*% k_vec
  return(SE_minus)
} # end SEminus


