#' @title Frequentist Confidence-Adaptive Trial Simulation
#' @description Simulates a group sequential clinical trial whose result is evaluated via frequentist confidence analysis.
#'
#' @details
#' Run simulations of a confidence-based adaptive clinical trial for any number of arms and stages.
#' At each analysis point, the confidence in treatment benefit and futility is evaluated and arms may be dropped or continued based on the trial settings.
#' The trial may also be run perpetually, with new treatment arms being added once arms are dropped, as an adaptive platform trial.
#' The confidence-based thresholds are derived using an alpha spending function, or specified with a fixed alpha.
#' @seealso [confidenceCurves::makeConfidenceCurves()]
#' @references Frequentist confidence analysis is based on Marschner (2024) \doi{https://doi.org/10.1002/sim.10000}.

#' @param sim.no Simulation number, when running mutiple simulations of a trial.
#' @param inputs A list of items fed to the function which parameterize the trial to be simulated.
#' An example parameter list can be loaded using `data(inputs)`.
#' @param save.plot Whether or not to save confidence curve plot with the result.TRUE (yes) or FALSE (no).
#'  When running multiple simulations, `FALSE` is recommended. If `TRUE`, files will be saved to the specified directory.
#'  The filename is automatically generated according to trial settings. Default is FALSE. Passed to `makeConfidenceCurves`.
#' @param show If saving confidence curves, what to show on the confidence density plot. Options are "BENEFIT" (default), "LMB" (lack of meaningful benefit),
#' "MB" (meaningful benefit) or "EQUIV" (equivalence). Passed to `makeConfidenceCurves`.
#' @param save.text Whether or not to save results to directory. Default is TRUE.
#' @param directory Working directory. Used to save Random State, and trial results. A subdirectory is created based on the current node
#' to allow for parallel computing across multiple nodes. Random State is checkpointed throughout the code and saved in the subdirectory 'directory/node/'.
#' Results are saved in the same places as the Random States. If `save.text == FALSE` nothing is saved to `directory`.
#' @param reproduce To reproduce a result from saved Random States. If setting as TRUE, make sure the directory parameter points to the location (the node subdirectory)
#' where the Random States are saved. The results will be saved to this directory. If set as FALSE (default), results and Random States are saved to the node subdirectory.
#' @param verbose Whether to print out text (TRUE) or not (FALSE). Useful to observe the trial process and decision-making while the simulation is running.
#' Not recommended if running a high number of simulations.
#' @param seed Option to set the seed. Default is NULL.
#' @return Object where each line is the result of confidence analysis for a given arm, at a given stage.
#' The number of lines is the number experimental treatments * number of stages, e.g., A two-arm-two-stage trial returns a two-line object.
#' However, if the trial is stopped after the first stage, only one line is returned.
#' @return Attributes in output object:
#' \itemize{
#' \item{sim.no: simulation number}
#' \item{arm: arm number starting from 2 as 1 is control}
#' \item{interim.arm: stage number of this arm (will differ from interim.total if arm was added later)}
#' \item{interim.total: interim number for trial}
#' \item{mean: point estimate}
#' \item{standard.error: standard error associated with point estimate}
#' \item{resp.ctrl: for binary data, resulting control response rate}
#' \item{resp.trmt: for binary data, resulting treatment response rate}
#' \item{conf.benefit: confidence in treatment benefit}
#' \item{conf.lack.meaningful.benefit: confidence in lack of meaningful benefit}
#' \item{action: Decision taken at this analysis point e.g. stop early, continue}
#' \item{N.looks: Number of looks (analysis points) in this trial design}
#' \item{misc: Information passed into getparlist function by the 'special' parameter. Can be anything of interest}
#' \item{N.arm: Number recruited to this treatment arm}
#' \item{N.pair: Number of patients in this two-arm (pairwise) analysis against control}
#' \item{N.known: Number of patients with a known outcome (prespecified in trial settings)}
#' \item{N: Total number of patients recruited so far. Will be different from N.known if there is a follow-up period.}
#' }
#'
#' @export
#' @importFrom utils read.csv write.csv tail
#' @examples
#' # Example of input list to generate a two-arm-two-stage trial with binary outcome data
#'
#' inputs <- list(
#'  outcome.type = "BINARY", # binary outcome data
#'  estimator.type = 'risk diff', # primary outcome is risk difference
#'  lmb.threshold = 0.1, # risk difference < 0.1 lacks meaningful benefit
#'  multiarm.mode='MONITOR FUTILITY', # only monitor for futility
#'  alpha = 0.0125, # fixed alpha threshold to determine treatment efficacy
#'  alloc.ratio = c(1,1), # allocation ratio
#'  num.per.block = c(1,1), # number per block for blocked allocation
#'  final.visit = 0, # time in days after which follow-up data becomes available
#'  ppm = rep(25, 15), # patients accrued each month for the entire trial period.
#'  looks = c(107, 214), # number of patients accrued at each look time, nmax = 214.
#'  perpetual=FALSE, # not a perpetual trial.
#'  resprate =  c(0.5, 0.6), # response rate for each arm
#'  lmb.conf.thres=0.95, # treatment arm is futility is the confidence in LMB is greater than 0.95
#'  special = paste0(0.5, '_', 0.6) # passing the response rates to special to add to the output
#')
#' # run a single simulation with these settings
#'conf <- runSingleTrial(input=inputs, save.plot=FALSE,
#'save.text=FALSE, verbose=TRUE, directory = '')


####################
# RUN A SINGLE TRIAL
####################

runSingleTrial <- function(
    sim.no=0,
    inputs=NULL,
    save.plot=FALSE,
    save.text=TRUE,
    show="BENEFIT",
    directory="",
    reproduce=FALSE,
    verbose=FALSE,
    seed=NULL){

  # making sure directory ends with a slash
  if (! is.null(directory)){
    if ((! endsWith(directory, "/")) & (! endsWith(directory, "\\"))){
      directory = paste0(directory, "/")
    }
    # make for this node
    subdirectory = paste0(directory, Sys.getpid(), "/")
    # create it
    if (save.text){
      dir.create(file.path(subdirectory), showWarnings = FALSE, recursive = TRUE)
    }
  }

  # get the parameter list from "inputs"

  # check if inputs were provided
  if (is.null(inputs)){
    if (verbose){
      print("No inputs were provided. Using example design settings.")
    }
    parlist = getparlist()
  } else if (is.list(inputs)){
    parlist = do.call(getparlist, inputs)
  }

  # ***** CHECKPOINT******

  if (reproduce){
    # load saved seed
    load(paste0(directory, "random_state_accrual.RData"))
  } else {
    # check if a seed has been explicitly set
    if (!exists(".Random.seed", envir = .GlobalEnv)){
      if(!is.null(seed)){
         set.seed(seed)
      }
    }
    # save state
    if (save.text){
      save(".Random.seed",file=paste0(subdirectory, "random_state_accrual.RData"))
    }
  }

  # get time of patient arrival, in terms of months since trial commence
  arrival.month = getAccrual(
    parlist$nmax, parlist$ppm,
    follow.up=parlist$final.visit/30,
    cont.recruit = FALSE,
    perpetual = parlist$perpetual)

  # convert month to day
  arrival.day = arrival.month*365/12

  # get nmax based on trial conditions

  # nmax can be different from the max sample size if you continue to recruit while waiting for followup
  # or you are running perpetually

  nmax.followup = length(arrival.month)

  # get nmax for each arm
  # the arm will stop recruiting if it reaches the maximum number of interims
  # OR if it reaches n.max.arm, whichever is first
  nmax.arm = parlist$nmax/length(parlist$alloc.ratio)

  # ***** CHECKPOINT******
  if (reproduce){
    load(paste0(directory, "random_state_allocation.RData"))
  } else {
    if (save.text){
      save(".Random.seed",file=paste0(subdirectory, "random_state_allocation.RData"))
    }
  }

  # allocate patients to treatment arm
  arm = getBlockedArm(nmax.followup, parlist$num.per.block)

  ## time when the outcome would be known (arrival plus follow up)
  obstime = arrival.day + parlist$final.visit

  # is this a perpetual trial?
  perpetual = parlist$perpetual

  # update the number of looks for a perpetual trial
  if (perpetual){
    # what is the difference in subjects between looks
    diff = diff(parlist$looks)
    # check that its same difference between looks
    if (all(diff==diff)){
      # extend number of looks to cover nmax.followup
      parlist$num.looks = floor(nmax.followup/diff[1])
    } else{
      if (verbose){
        print("Interim length uneven, can't extend number of looks.")
      }
    }
  }

  #########################
  # generate response data
  #########################

  # ***** CHECKPOINT******
  if (reproduce){
    load(paste0(directory, "random_state_outcome.RData"))
  } else {
    if (save.text){
      save(".Random.seed",file=paste0(subdirectory, "random_state_outcome.RData"))
    }
  }

  # (binary/ordinal/continuous)
  if (parlist$outcome.type=='ORDINAL'){
    dat = vapply(arm, getDataOrd, resprate = parlist$resprate, FUN.VALUE = 0)
  } else if (parlist$outcome.type=='BINARY'){
    dat = vapply(arm, getDataBin, resprate = parlist$resprate, FUN.VALUE = 0)
  } else if (parlist$outcome.type=='CONTINUOUS'){
    dat = vapply(arm, getDataCont, resprate = parlist$resprate, FUN.VALUE = 0)
  }

  # make a list
  datlist = list(subjid = 1:nmax.followup, arm = arm, dat = dat,
                 arrival.day = arrival.day, obstime = obstime)

  # get number of treatment arms
  # this number grows as new treatments are added, in a perpetual setting

  num.treat.arms = length(parlist$alloc.ratio) - 1

  # to identify binary outcome data
  num.unique.resp = length(unique(datlist$dat))

  #############################
  # initiate values & variables
  #############################

  # create results matrix
  conf.all = matrix(list(), nrow=parlist$num.look, ncol=num.treat.arms)
  suffstats.all = vector(mode = "list", parlist$num.looks)

  # record which arms are have been dropped to skip them
  drop.arms = rep(0, num.treat.arms)
  # reshuffle is turned on once we have to reallocate arms
  # once it turns on it doesn't turn off
  reshuffle = FALSE

  # number of arms that have been added
  num.new.arms = 0
  # how many recruited to each arm so far, including control
  n.per.arm = rep(0, length(parlist$resprate))
  # how many interim for each arm so far, including control
  interims.per.arm = rep(0, length(parlist$resprate)) # to ensure concurrent controls, all arms

  # how many arms remain in the trial, including control
  rem.arms = c(1, seq(num.treat.arms) + 1)

  # What is the time at each interim analysis point (looktime), starting with 0
  looktimes = rep(0, parlist$num.looks + 1)

  # where to save results. Save it directory for REPRODUCE=TRUE and subdirectory for REPRODUCE=FALSE
  # but as we have no more checkpoints, we can merge the two

  if (!reproduce){
    directory = subdirectory
  }

  ##############################
  # LOOKS, AND PAIRWISE ANALYSES
  ##############################

  for (j in 1:(parlist$num.looks)){

    n.at.look = parlist$looks[j]

    # for perpetual setting
    # we assume the same number of patients between each interim
    if (is.na(n.at.look)){
      n.at.look = j * diff[1]
    }

    # print out
    if (verbose){
      print(paste0("Interim ", j, "/", parlist$num.looks, " (N=", n.at.look, ")."))
    }

    ######################
    # GET THE INTERIM DATA
    ######################

    # from start of the trial until now
    looktime.interim = arrival.day[n.at.look]
    currdatlist.interim = getCurrentData(
      datlist,
      looktime.interim,
      n = n.at.look,
      as.followup = TRUE)

    # if follow up time > 0, a different looktime will be returned by getCurrentData
    looktime.interim = currdatlist.interim$looktime

    # add to looktimes vector
    looktimes[j + 1] = looktime.interim

    # create a boolean vector for patients accumulated since the last interim i.e. new subjects
    this.interim = currdatlist.interim$arrival.day > looktimes[j]

    ##############
    # REALLOCATION
    ##############
    # If arms were dropped
    # either we add ("perpetual") or reallocate to remaining

    if (reshuffle){

      # which arms are bring dropped, where control is arm 1
      dropped.arms = which(drop.arms==1) + 1

      ###################
      # PERPETUAL SETTING
      ###################

      if (perpetual){

        # we add an arm when we drop an arm
        # reassign the patients that would be to the dropped arm, to the new arm

        # we also have to replace the arms that have been dropped earlier
        # to.replace: how many arms are we replacing?
        # new.arms: how arms are we added?

        # how many arms so far
        to.date.arms = length(parlist$alloc.ratio) + num.new.arms

        # how many need to be added
        to.add = length(dropped.arms) - num.new.arms

        # how many do we need to replace now
        num.to.replace = num.new.arms + to.add

        # what are their names
        to.replace = seq(length(parlist$alloc.ratio) + 1,
                         length(parlist$alloc.ratio) + num.to.replace, 1)

        # get the label of the new arm/s
        new.arms.from = to.date.arms + 1
        if (to.add > 0){
          new.arms = seq(new.arms.from, 100, 1)[1:to.add]
        } else (new.arms = 0)

        # now replace old arm assignments with the new arms we've added since the start
        for (x in 1:num.to.replace){

          # is this a new arm?
          if (x > num.new.arms){
            # new arm territory
            y = x - (num.to.replace - to.add)

            # check to see if we have any more treatments to add
            if (!length(parlist$resprate) >= new.arms[y]){

              # print out
              if (verbose){print("There are no more treatments to add.")}

              # turn off perpetual setting
              perpetual = FALSE

              # stop this loop and move to the non-perpetual setting
              break
            }

            # add new treatments

            # extend dropped arm vector
            drop.arms = c(drop.arms, 0)
            # extend conf.all matrix
            conf.all = cbind(conf.all, matrix(list(), ncol=1, nrow=parlist$num.looks))
            # add a treatment arm
            num.treat.arms = num.treat.arms + 1
            # add a new arm
            num.new.arms = num.new.arms + 1
          }

          # replace dropped arm assignment with new arm in *this interim*
          currdatlist.interim$arm[which(currdatlist.interim$arm==dropped.arms[x] & this.interim)] = to.replace[x]
          dat.new.arm = sapply(currdatlist.interim$arm, function(y){
            if (y==to.replace[x]){
              if (parlist$outcome.type=='ORDINAL'){
                getDataOrd(y, resprate =  parlist$resprate)
              } else if (parlist$outcome.type=='BINARY'){
                getDataBin(y, resprate =  parlist$resprate)
              } else if (parlist$outcome.type=='CONTINUOUS'){
                getDataCont(y, resprate =  parlist$resprate)
              }
            } else {0}
          }
          )

          # replace old response rates with new arm response rates
          currdatlist.interim$dat[which(dat.new.arm>0 & this.interim)] = dat.new.arm[which(dat.new.arm>0 & this.interim)]
        }

      }

      ######################
      # OFF-PERPETUAL SETTING
      ######################

      # we must reallocate all arms

      if (!perpetual){

        # put allocation probability of dropped arms to zero.
        # reallocated to remaining arms

        # print out
        if (verbose){
          print(paste0("Allocation probability for arms ",
                       paste(sapply(dropped.arms, paste, collapse=":"), collapse=", "),
                       " set to 0."))
        }

        # make a probability vector
        # base it off of drop.arms as this will have changed if we added a new arm above
        # start with 1 for each arm, including control
        for (k in 1:length(dropped.arms)){
          prob[[dropped.arms[k]]] = 0
        }

        # check that prob is same length as num.per.block
        if(length(prob) != length(parlist$num.per.block)){
          parlist$num.per.block = c(parlist$num.per.block, rep(tail(parlist$num.per.block,n=1), length(prob) - length(parlist$num.per.block)))
        }

        # reallocate arms
        arm.interim = getBlockedArm(length(currdatlist.interim$arm), parlist$num.per.block, prob=prob)
        # generate responses
        if (parlist$outcome.type=='ORDINAL'){
          dat.interim = vapply(arm.interim, getDataOrd, resprate = parlist$resprate, FUN.VALUE = 0)
        } else if (parlist$outcome.type=='BINARY'){
          dat.interim = vapply(arm.interim, getDataBin, resprate = parlist$resprate, FUN.VALUE = 0)
        } else if (parlist$outcome.type=='CONTINUOUS'){
          dat.interim = vapply(arm.interim, getDataCont, resprate = parlist$resprate, FUN.VALUE = 0)
        }

        # only replace the values from after the last interim
        currdatlist.interim$dat[this.interim] = dat.interim[this.interim]
        currdatlist.interim$arm[this.interim] = arm.interim[this.interim]

      }
    }

    # ARM ALLOCATION IS NOW UPDATED

    # how many subjects have so far been recruited to each arm?
    n.per.arm = sapply(1:length(n.per.arm),
                       function(x){
                         length(which(currdatlist.interim$arm==(x)))
                       }
    )

    # how many arms remain?
    # this is calculated again at the end of the interim
    rem.arms =  c(1, which(drop.arms==0) + 1)

    # update number of interims per arm so far
    interims.per.arm[unique(currdatlist.interim$arm[this.interim])] = interims.per.arm[unique(currdatlist.interim$arm[this.interim])] + 1

    # get sufficient statistics - but it is not used
    suffstats.interim = getSuffStats(currdatlist.interim)

    #  determine a one or two sided test
    if ((j==parlist$num.looks) & (perpetual == F)){
      sided <- 'two-sided'
    } else {sided = 'one-sided'}

    # tag used to save plots for this interim
    tag = paste0(parlist$outfilename,"-interim", j, "-n", n.at.look)

    # initiate empty vector for all pairwise analysis
    action.all.arms = vector(mode="list", num.treat.arms)

    ###################################################
    # CONFIDENCE ANALYSIS FOR EACH PAIRWISE COMPARISONS
    ###################################################


    ################
    # BINARY OUTCOME
    ################

    if(num.unique.resp == 2){

      # cycle through the remaining arms
      for (i in 1:num.treat.arms){

        # if we have dropped the arm at a previous stage, don't evaluate anymore
        if (drop.arms[[i]]==1){
          if (verbose) {print(paste0("Skipping treatment ", i ,"."))}
          if (!perpetual){
            next
          }
        }

        # create unique figure tag
        tag.arm = paste0(tag,'-trmt',  i, '-', parlist$resprate.str[[i+1]])

        # get the boolean array of subjects in pairwise analysis
        current.arm = i + 1

        # PAIRWISE ANALYSIS

        # what was the looktime when this arm was added?
        concurrent.looktime = looktimes[j + 1 - interims.per.arm[current.arm]]

        # only get control from concurrent time periods
        # if the arm was there from the beginning then it uses all controls
        arm.bool = (currdatlist.interim$arm == 1 & currdatlist.interim$arrival.day > concurrent.looktime
                    & currdatlist.interim$known == TRUE )|(currdatlist.interim$arm == current.arm &
                                                             currdatlist.interim$known == TRUE)
        # size of this analysis
        pairwise.size = sum(arm.bool)

        # print out
        if (verbose){
          print(paste0("Treatment ", i, "/", num.treat.arms," vs control (N=", pairwise.size, ")."))
        }

        resp <- tapply(currdatlist.interim$dat[arm.bool], currdatlist.interim$arm[arm.bool], sum)
        num <- table(currdatlist.interim$arm[arm.bool])

        ## let a and c be control responses
        a <- resp[[1]]
        c <- num[[1]]

        # let b and d be the corresponding numbers for treatment arms
        b <- resp[[i + 1]]
        d <- num[[i + 1]]

        # print out
        if (verbose){
          print(paste0("Treatment ", i, "/", num.treat.arms, " vs control ", "(N=", d + c, ")."))
        }

        # odds ratios are converted to log
        if (parlist$estimator.type %in% c('risk ratio', 'odds ratio')){
          parlist$lmb.threshold = log(parlist$lmb.threshold)
          dir.benefit = 0
        } else {dir.benefit = 1}

        confidence = confidenceCurves::makeConfidenceCurves(
          num.resp.ctrl = a,
          num.resp.trmt = b,
          num.ctrl = c,
          num.trmt = d,
          estimator.type = parlist$estimator.type,
          show=show,
          pval=sided,
          directory=directory,
          tag=tag.arm,
          save.plot=save.plot,
          return.plot = FALSE,
          min.effect = parlist$lmb.threshold,
          dir.benefit=dir.benefit)

        ## NOW CHECK CONFIDENCE AGAINST DECISION THRESHOLDS FOR EACH ARM

        # if the arm has the maximum number of looks OR accrued more than the maximum per arm

        if (interims.per.arm[i+1] == length(parlist$looks) | n.per.arm[i+1] >= nmax.arm){
          # FINAL ANALYSIS
          if (parlist$multiarm.mode=='MONITOR FUTILITY'){
            # used fixed alpha
            if (confidence$conf.benefit > (1 - parlist$alpha) ){
              action='efficacy.significant'
            } else if (confidence$conf.lack.meaningful.benefit > parlist$lmb.confidence.threshold){
              action = "futility.significant"
            } else {action='fail'}
          } else {
            if (confidence$conf.benefit > parlist$confidence.bounds.efficacy[interims.per.arm[i+1]] ){
              action='efficacy.significant'
            } else if (confidence$conf.benefit < parlist$confidence.bounds.inferiority[interims.per.arm[i+1]] ){
              action='inferiority.significant'
            } else if (confidence$conf.lack.meaningful.benefit > parlist$lmb.confidence.threshold ){
              action='futility.significant'
            } else {action='fail'}
          }

        } else {
          # INTERIM ANALYSIS
          if (parlist$multiarm.mode=='MONITOR FUTILITY'){
            if (confidence$conf.lack.meaningful.benefit > parlist$lmb.confidence.threshold){
              action = "stop.futile"
            } else {action = "continue"}
          } else{
            if (confidence$conf.benefit > parlist$confidence.bounds.efficacy[interims.per.arm[i+1]]){
              action = "stop.efficacy"
            } else if (confidence$conf.benefit < parlist$confidence.bounds.inferiority[interims.per.arm[i+1]]){
              action = "stop.inferior"
            } else if (confidence$conf.lack.meaningful.benefit > parlist$lmb.confidence.threshold){
              action = "stop.futile"
            } else {action = "continue"}
          }
        }

        ## Now append this to the stagewise action vector and the overall confidence list
        action.all.arms[[i]] = action
        res = list(interim.total=j,
                   mean=confidence$mean,
                   standard.error=confidence$s.error,
                   resp.ctrl=a/c,
                   resp.trmt = b/d,
                   conf.benefit=confidence$conf.benefit,
                   conf.lack.meaningful.benefit=confidence$conf.lack.meaningful.benefit,
                   action=action,
                   N.looks=length(parlist$looks),
                   misc=parlist$misc,
                   N.arm=n.per.arm[[i+1]],
                   N.pair = pairwise.size,
                   N.known=n.at.look,
                   N=length(currdatlist.interim$dat))
        suffstats.all[[j]] = suffstats.interim$formattedrate
        conf.all[[j,i]] = unlist(res)
      }
    } else if (parlist$outcome.type == 'ORDINAL'){

      ###################
      # ORDINAL OUTCOME #
      ###################
      for (i in 1:num.treat.arms){

        # if we have dropped the arm at a previous stage, don't evaluate anymore
        if (drop.arms[[i]]==1){
          if (verbose){
            print(paste0("Skipping treatment ", i ,"."))
          }
          next
        }

        # create unique figure tag
        tag.arm = paste0(tag,'-trmt',  i, '-', parlist$resprate.str[[i+1]])

        # get the boolean array of subjects in pairwise analysis
        current.arm = i + 1

        # PAIRWISE ANALYSIS

        # what was the looktime when this arm was added?
        concurrent.looktime = looktimes[j + 1 - interims.per.arm[current.arm]]

        # only get control from concurrent time periods
        # if the arm was there from the beginning then it uses all controls
        arm.bool = (currdatlist.interim$arm == 1 & currdatlist.interim$arrival.day > concurrent.looktime
                    & currdatlist.interim$known == TRUE )|(currdatlist.interim$arm == current.arm &
                                                             currdatlist.interim$known == TRUE)
        # size of this analysis
        pairwise.size = sum(arm.bool)

        # print out
        if (verbose){
          print(paste0("Treatment ", i, "/", num.treat.arms," vs control (N=", pairwise.size, ")."))
        }

        data <- data.frame(
          id=currdatlist.interim$subjid[arm.bool],
          arm=currdatlist.interim$arm[arm.bool],
          response=currdatlist.interim$dat[arm.bool])

        # calculate the log genOR
        x = genodds::genodds(data$response, data$arm)
        results =  x$results$`All data`
        genodds = log(results$odds)
        conf.int = log(results$conf.int)
        se = (conf.int[2] - conf.int[1]) / 3.92

        dir.benefit = 0 # assumes with the ordinal scale that 1 is better than 2.

        # odds ratios are converted to log
        if (parlist$estimator.type == 'odds ratio'){
          parlist$lmb.threshold = log(parlist$lmb.threshold)
        }

        confidence = confidenceCurves::makeConfidenceCurves(theta.estimator = genodds,
                                                            standard.error = se,
                                                            neutral.effect=0,
                                                            show=show,
                                                            pval=sided,
                                                            directory=directory,
                                                            tag=tag.arm ,
                                                            return.plot=FALSE,
                                                            save.plot = save.plot,
                                                            min.effect = parlist$lmb.threshold,
                                                            dir.benefit = dir.benefit)

        ## NOW CHECK CONFIDENCE AGAINST DECISION THRESHOLDS FOR EACH ARM

        # if the arm has the maximum number of looks OR accured more than the maximum per arm
        if (interims.per.arm[i+1] == length(parlist$looks) | n.per.arm[i+1] >= nmax.arm){
          # FINAL ANALYSIS
          if (parlist$multiarm.mode=='MONITOR FUTILITY'){
            # used fixed alpha
            if (confidence$conf.benefit > (1 - parlist$alpha) ){
              action='efficacy.significant'
            } else if (confidence$conf.lack.meaningful.benefit > parlist$lmb.confidence.threshold){
              action = "futility.significant"
            } else {action='fail'}
          } else {
            if (confidence$conf.benefit > parlist$confidence.bounds.efficacy[interims.per.arm[i+1]] ){
              action='efficacy.significant'
            } else if (confidence$conf.benefit < parlist$confidence.bounds.inferiority[interims.per.arm[i+1]] ){
              action='inferiority.significant'
            } else if (confidence$conf.lack.meaningful.benefit > parlist$lmb.confidence.threshold ){
              action='futility.significant'
            } else {action='fail'}
          }

        } else {
          # INTERIM ANALYSIS
          if (parlist$multiarm.mode=='MONITOR FUTILITY'){
            if (confidence$conf.lack.meaningful.benefit > parlist$lmb.confidence.threshold){
              action = "stop.futile"
            } else {action = "continue"}
          } else {
            if (confidence$conf.benefit > parlist$confidence.bounds.efficacy[interims.per.arm[i+1]]){
              action = "stop.efficacy"
            } else if (confidence$conf.benefit < parlist$confidence.bounds.inferiority[interims.per.arm[i+1]]){
              action = "stop.inferior"
            } else if (confidence$conf.lack.meaningful.benefit > parlist$lmb.confidence.threshold){
              action = "stop.futile"
            } else {action = "continue"}
          }
        }

        ## Now append this to the stagewise action vector and the overall confidence matrix
        action.all.arms[[i]] = action
        res = list(
          interim.total=j,
          mean=confidence$mean,
          standard.error=confidence$s.error,
          conf.benefit=confidence$conf.benefit,
          conf.lack.meaningful.benefit=confidence$conf.lack.meaningful.benefit,
          action=action,
          N.looks=length(parlist$looks),
          misc=parlist$misc,
          N.arm=n.per.arm[[i+1]],
          N.pair = pairwise.size,
          N.known=n.at.look,
          N=length(currdatlist.interim$dat))
        conf.all[[j,i]] = unlist(res)
      }

    } else if (parlist$outcome.type == 'CONTINUOUS'){
      ######################
      # CONTINUOUS OUTCOME #
      ######################
      # For continuous outcomes, group difference is measured using difference of means
      for (i in 1:num.treat.arms){

        # if we have dropped the arm at a previous stage, don't evaluate anymore
        if (drop.arms[[i]]==1){
          if (verbose){
            print(paste0("Skipping treatment ", i ,"."))
          }
          next
        }

        # unique figure tag
        tag.arm = paste0(tag,'-trmt',  i, '-', parlist$resprate.str[[i+1]])

        # get the boolean array
        current.arm = i + 1

        concurrent.looktime = looktimes[j + 1 - interims.per.arm[current.arm]]

        arm.bool = (currdatlist.interim$arm == 1 & currdatlist.interim$arrival.day > concurrent.looktime
                    & currdatlist.interim$known == TRUE)|(currdatlist.interim$arm == current.arm &
                                                            currdatlist.interim$known == TRUE)
        pairwise.size = sum(arm.bool) # size of this analysis

        # print out
        if (verbose){
          print(paste0("Treatment ", i, "/", num.treat.arms," vs control ( N=", pairwise.size, ")."))
        }

        data =  data.frame(id=currdatlist.interim$subjid[arm.bool],
                           arm=currdatlist.interim$arm[arm.bool],
                           Y_1=currdatlist.interim$dat[arm.bool],
                           Start_time=currdatlist.interim$arrival.day[arm.bool])

        # difference of means
        means <- tapply(currdatlist.interim$dat[arm.bool], currdatlist.interim$arm[arm.bool], mean)
        vars <- tapply(currdatlist.interim$dat[arm.bool], currdatlist.interim$arm[arm.bool], mean)
        ns <- table(currdatlist.interim$arm[arm.bool])

        difference.of.means <- means[[i + 1]] - means[[1]]

        standard.error <- sqrt((vars[[1]]/ns[[1]]) + (vars[[i + 1]]/ns[[i + 1]]))

        dir.beneift <- 1 # greater than 1 signifies benefit to treatment over control
        confidence = confidenceCurves::makeConfidenceCurves(
          theta.estimator = difference.of.means,
          standard.error = standard.error,
          sample.size = pairwise.size,
          show=show,
          pval=toupper(sided),
          directory=directory,
          tag=tag.arm ,
          return.plot=FALSE,
          save.plot = save.plot,
          min.effect = parlist$lmb.threshold,
          neutral.effect = 0,
          dir.benefit = dir.benefit)

        ## NOW CHECK CONFIDENCE AGAINST DECISION THRESHOLDS FOR EACH ARM
        # if the arm has the maximum number of looks OR accured more than the maximum per arm
        if (interims.per.arm[i+1] == length(parlist$looks) | n.per.arm[i+1] >= nmax.arm){
          # FINAL ANALYSIS
          if (parlist$multiarm.mode=='MONITOR FUTILITY'){
            # used fixed alpha
            if (confidence$conf.benefit > (1 - parlist$alpha) ){
              action='efficacy.significant'
            } else if (confidence$conf.lack.meaningful.benefit > parlist$lmb.confidence.threshold){
              action = "futility.significant"
            } else {action='fail'}
          } else {
            if (confidence$conf.benefit > parlist$confidence.bounds.efficacy[interims.per.arm[i+1]] ){
              action='efficacy.significant'
            } else if (confidence$conf.benefit < parlist$confidence.bounds.inferiority[interims.per.arm[i+1]] ){
              action='inferiority.significant'
            } else if (confidence$conf.lack.meaningful.benefit > parlist$lmb.confidence.threshold ){
              action='futility.significant'
            } else {action='fail'}
          }

        } else {
          # INTERIM ANALYSIS
          if (parlist$multiarm.mode=='MONITOR FUTILITY'){
            if (confidence$conf.lack.meaningful.benefit > parlist$lmb.confidence.threshold){
              action = "stop.futile"
            } else {action = "continue"}
          } else {
            if (confidence$conf.benefit > parlist$confidence.bounds.efficacy[interims.per.arm[i+1]]){
              action = "stop.efficacy"
            } else if (confidence$conf.benefit < parlist$confidence.bounds.inferiority[interims.per.arm[i+1]]){
              action = "stop.inferior"
            } else if (confidence$conf.lack.meaningful.benefit > parlist$lmb.confidence.threshold){
              action = "stop.futile"
            } else {action = "continue"}
          }
        }

        ## append
        action.all.arms[[i]] = action
        res = list(
          interim.total=j,
          mean=confidence$mean,
          standard.error=confidence$s.error,
          conf.benefit=confidence$conf.benefit,
          conf.lack.meaningful.benefit=confidence$conf.lack.meaningful.benefit,
          action=action,
          N.looks=length(parlist$looks),
          misc=parlist$misc,
          N.arm=n.per.arm[[i+1]],
          N.pair = pairwise.size,
          N.known=n.at.look,
          N=length(currdatlist.interim$dat))

        suffstats.all[[j]] = suffstats.interim$formattedrate
        conf.all[[j,i]] = unlist(res)
      }
    }

    ##########################################################
    # ALL ARMS ARE EVALUATED, NOW MAKE INTERIM/FINAL DECISION
    #########################################################

    # the action.all.arms vector dictates decisions

    # all remaining arms stop?
    # remaining arms = treatment arms - dropped arms
    if (sum(grepl('stop', action.all.arms, fixed=TRUE))==(num.treat.arms-sum(drop.arms))){
      if (verbose){print("All arms stopped. Stopping trial.")}
      break
    }

    # which arms have finished?
    stop.finish = as.numeric(grepl(paste(c('significant', 'fail'), collapse='|'), action.all.arms))
    # stop them
    drop.arms[which(stop.finish==1)] = 1


    # which arms are stopping for bad reasons?
    inf = as.numeric(grepl('stop.inferior', action.all.arms, fixed=TRUE))
    fut = as.numeric(grepl('stop.futile', action.all.arms, fixed=TRUE))
    stop.bad = inf == 1 | fut == 1


    # always stop for bad

    if (sum(stop.bad) > 0){

      drop.arms[which(do.call("rbind", list(stop.bad)) == 1)] = 1
      # print out
      if (verbose){
        print(paste0("Stopping treatment ",
                     paste(which(do.call("rbind", list(stop.bad)) == 1), collapse=", "),
                     "."))
      }
    }

    # which arms stopping for good reasons?
    stop.good = grepl('stop.efficacy', action.all.arms, fixed=TRUE)

    # Get a list of Conf(BENEFIT) for each arm
    arms = conf.all[j,]
    conf.arms = lapply(arms, function(x){
      as.numeric(x["conf.benefit"][[1]])})

    # determine actions based on the MULTIARM SETTING
    # SELECT BEST' 'DROP WORST' 'ALL PROMISING' and 'CONFIDENCE-BASED'

    if (parlist$multiarm.mode == 'SELECT BEST'){

      # if any are stop for good, end trial
      if (sum(stop.good)>0){
        if (verbose){print("SELECT BEST: Stop for efficacy." )}
        break
      }

      # find the treatment with highest confidence in benefit
      best = which.max(do.call("rbind", conf.arms))

      # print out
      if (verbose){print(paste0("SELECT BEST: Treament ", best, " continues."))}

      # drop the rest
      for (j in 1:length(drop.arms)){
        if (!j==best){
          drop.arms[[j]] = 1
        }
      }

    } else if (parlist$multiarm.mode == 'DROP WORST'){

      # if you haven't already dropped arms
      if (sum(stop.bad) == 0) {

        # Drop the arm with the lowest confidence in treatment benefit
        drop.arms[[which.min(do.call("rbind", conf.arms))]] = 1

        # print out
        if (verbose){print(paste0("DROP WORST: Dropping treatment ",
                                which.min(do.call("rbind", conf.arms)), "." ))}

      }

    } else if (parlist$multiarm.mode == 'CONFIDENCE-BASED'){

      # stop inferior, futile and efficacious
      if (sum(stop.good) > 0){

        # drop arms
        drop.arms[which(do.call("rbind", list(stop.good)) == 1)] = 1

        # print out

        if (verbose){print(
          paste0(
            "CONFIDENCE-BASED: Drop Treatments ",
            paste(
              sapply(which(do.call("rbind", list(stop.good)) == 1), paste, collapse=":"),
              collapse = ", "),
            "."
          )
        )}
      }
    }

    # check to see if we're dropping arms
    if (sum(drop.arms) > 0){
      reshuffle=TRUE
    }

    # which arms remain in the trial (including control)
    rem.arms =  c(1, which(drop.arms==0) + 1)
  }

  conf.all = do.call("rbind", data.frame(conf.all))

  conf.all<-do.call("rbind",
                    lapply(seq(num.treat.arms), function(x){
                      cbind(sim.no=sim.no, arm=x+1,
                            interim.arm=1:length(plyr::compact(conf.all[x,])),
                            do.call("rbind", conf.all[x,]))}))

  # save the output into a csv
  filename = paste0(parlist$outfilename, "-conf-output-line-by-line.csv")
  if (save.text){
    if (! is.null(directory)){
      if ((! endsWith(directory, "/")) & (! endsWith(directory, "\\"))){
        directory = paste0(directory, "/")
      }
      # if we are running N simulations
      if (
        (sim.no > 1) &
        (file.exists(paste0(directory, filename)))
      ){
        old.conf = read.csv(paste0(directory, filename))
        write.csv(rbind(old.conf, conf.all), paste0(directory, filename), row.names = F)
      } else{
        write.csv(conf.all, paste0(directory, filename), row.names = F)
      }
    }
  }
  return(conf.all)
}


