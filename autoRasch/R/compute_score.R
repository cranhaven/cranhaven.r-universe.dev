#' Compute the In-plus-out-of-questionnaire log likelihood (with DIF) (IPOQ-LL(-DIF))
#'
#' \code{compute_score} computes the the IPOQ-LL/IPOQ-LL-DIF score of an instrument (included set) of the given initial survey.
#' While \code{compute_scores} computes the IPOQ-LL/IPOQ-LL-DIF score of many (more than one) instruments (included sets) of
#' the given initial survey simultanously.
#'
#' @param X A matrix or data.frame of the observed responses (ordinal or binary response).
#' @param incl_set A vector of the items (columns) number in the data.frame X that are included in the included set.
#' @param type The type of the score. \code{ipoqll} if we ignore the presence of the DIF and \code{ipoqlldif} if we want to consider the DIF effect.
#' @param groups_map Matrix to map the respondents to the DIF groups.
#' @param init_par_iq Initial values of the parameters in the included set before the estimation begin.
#' @param init_par_oq Initial values of the parameters in the excluded set before the estimation begin.
#' @param setting_par_iq The coordinate descent optimisation setting of the included set. See \code{\link[autoRasch:autoRaschOptions]{autoRasch::autoRaschOptions()}} \code{cd_control} parameter.
#' @param setting_par_oq The coordinate descent optimisation setting of the excluded set. See \code{\link[autoRasch:autoRaschOptions]{autoRasch::autoRaschOptions()}} \code{cd_control} parameter.
#' @param method The implementation option of log likelihood function. \code{fast} using a \code{c++} implementation and \code{novel} using an \code{R} implementation.
#'
#' @return
#' \code{compute_score} will return a vector which contains in-questionnaire log likelihood (IQ-LL(-DIF)), out-of-questionnaire log likelihood(OQ-LL(-DIF)),
#' IPOQ-LL(-DIF), included set's items' number in the given initial survey, the estimated theta parameters, the estimated items' parameters in the included set,
#' and the estimated items' parameters in the excluded set, sequentially.
#'
#' @examples
#' ipoqll_score <- compute_score(shortDIF,incl_set = c(1:3),type = "ipoqll")
#' summary(ipoqll_score)
#'
#' \dontrun{
#' ipoqll_scores <- compute_scores(shortDIF,incl_set = rbind(c(1:3),c(2:4)),
#'                                 type = "ipoqll", cores = 2)
#' View(ipoqll_scores)
#' }
#'
#' @importFrom parallel detectCores makeCluster stopCluster
#'
#' @rdname compute_score
#' @export
compute_score <- function(X, incl_set, type = c("ipoqll","ipoqlldif"), groups_map = c(),
                          init_par_iq = c(), init_par_oq = c(),
                          setting_par_iq = c(), setting_par_oq = c(), method = c("fast","novel")){

  if(is.null(type)){
    type <- "ipoqll"
  }

  if(type[1] == "ipoqll"){
    fixed_par <- c("delta")
    isPenalized_delta <- FALSE
    groups_map <- NULL
    scoreName <- "IPOQ-LL"
  } else if(type[1] == "ipoqlldif"){
    fixed_par <- c()
    isPenalized_delta <- TRUE
    if(is.null(groups_map)){
      stop("autoRasch ERROR: to use the `ipoqlldif`, `groups_map` must be provided.")
    }
    groups_map <- as.matrix(groups_map)
    scoreName <- "IPOQ-LL-DIF"
  }

  dset <- as.matrix(X)
  incl_set <- incl_set[!is.na(incl_set)]
  incl_resp <- dset[,incl_set]

  if(length(incl_set) != ncol(dset)){
    excl_resp <- dset[,-c(incl_set)]
    excl_resp <- as.matrix(excl_resp)
  }

  if(is.null(setting_par_iq)){
    setting_iq <- autoRaschOptions()
    if(type[1] == "ipoqlldif"){
      setting_iq$optz_method <- "mixed"
    } else {
      setting_iq$optz_method <- "optim"
    }
  } else {
    if("aR_opt" %in% class(setting_par_iq)){
      setting_iq <- setting_par_iq
    } else {
      stop("autoRasch ERROR: The setting used should be a class of `aR_opt'!")
    }
  }

  setting_iq$isHessian <- FALSE
  setting_iq$fixed_par <- fixed_par
  setting_iq$isPenalized_delta <- isPenalized_delta
  setting_iq$groups_map <- groups_map
  setting_iq$randomized <- TRUE

  iqll <- pjmle(incl_resp, init_par = init_par_iq, setting = setting_iq, method = method)

  if(ncol(dset) == length(incl_set)){
    loglik_oqll <- NA
  } else {
    if(is.null(setting_par_oq)){
      setting_oq <- autoRaschOptions()
      if(type[1] == "ipoqlldif"){
        setting_oq$optz_method <- "mixed"
      } else {
        setting_oq$optz_method <- "optim"
      }
    } else {
      if("aR_opt" %in% class(setting_par_oq)){
        setting_oq <- setting_par_oq
      } else {
        stop("The setting used should be a class of aR_opt!")
      }
    }

    setting_oq$isHessian <- FALSE
    setting_oq$fixed_par <- c("theta",fixed_par)
    setting_oq$fixed_theta <- iqll$theta
    setting_oq$isPenalized_delta <- isPenalized_delta
    setting_oq$isPenalized_theta <- FALSE
    setting_oq$groups_map <- groups_map
    setting_oq$randomized <- TRUE

    oqll <- pjmle(excl_resp, init_par = init_par_oq, setting = setting_oq, method = method)

    loglik_oqll <- oqll$loglik
  }

  ipoqll <- sum(c(iqll$loglik,loglik_oqll),na.rm = TRUE)
  res <- c(iqll$loglik, loglik_oqll, ipoqll)

  ### Parse the parameters estimate
  if(type[1] == "ipoqlldif"){

    ### parse for iq-ll estimate
    n_par <- sum(nrow(dset),((1+ncol(groups_map)+(max(dset,na.rm = TRUE)-min(dset,na.rm = TRUE)))*ncol(dset)))

    if((minCat <- min(dset,na.rm = TRUE)) != 0){  ### makes sure the response is started at 0
      dset <- dset - minCat
    }

    if(method[1] == "novel"){

      mt_vek_ori <- apply(dset, 2L, max, na.rm = TRUE)
      mt_vek_ori <- max(mt_vek_ori)

      mt_vek_incl <- apply(matrix(dset[,c(incl_set)],ncol = length(seq_len(ncol(dset))[c(incl_set)])), 2L, max, na.rm = TRUE)

      mt_idx_incl <- rep(seq_along(mt_vek_incl),mt_vek_incl)
      betalist_incl <- as.vector(unlist(tapply(iqll$beta, mt_idx_incl, function(x){
        temp <- c(x,rep(0,(mt_vek_ori-length(x))))
        return(temp)
      })))

      betalength <- sum(apply(dset,2,function(x){
        temp <- max(x,na.rm = TRUE)-min(x,na.rm = TRUE)
        return(temp)
      }))


    } else {

      betalength <- ncol(dset)*max(iqll$mt_vek)
      betalist_incl <- iqll$beta

    }

    length(betalist_incl) <- betalength
    gamma.ret <- iqll$gamma
    length(gamma.ret) <- ncol(dset)
    delta.ret <- iqll$delta
    length(delta.ret) <- ncol(groups_map)*ncol(dset)

    iqll_params <- c(iqll$theta, betalist_incl, gamma.ret, delta.ret)

    ### parse for oq-ll estimate
    if(ncol(dset) == length(incl_set)){
      oqll_params <- NA
    } else {
      if(method[1] == "novel"){

        mt_vek_excl <- apply(matrix(dset[,-c(incl_set)],ncol = length(seq_len(ncol(dset))[-c(incl_set)])), 2L, max, na.rm = TRUE)

        mt_idx_excl <- rep(seq_along(mt_vek_excl), mt_vek_excl)

        betalist_excl <- as.vector(unlist(tapply(oqll$beta, mt_idx_excl, function(x){
          temp <- c(x,rep(0,(mt_vek_ori-length(x))))
          return(temp)
        })))

      } else {

        betalist_excl <- oqll$beta

      }

      length(betalist_excl) <- betalength
      gamma.ret <- oqll$gamma
      length(gamma.ret) <- ncol(dset)
      delta.ret <- oqll$delta
      length(delta.ret) <- ncol(groups_map)*ncol(dset)

      oqll_params <- c(betalist_excl, gamma.ret, delta.ret)

    }
    length(iqll_params) <- n_par
    length(oqll_params) <- n_par - nrow(dset)
  } else {
    n_par <- sum(nrow(dset),((1+(max(dset,na.rm = TRUE)-min(dset,na.rm = TRUE)))*ncol(dset)))

    if((minCat <- min(dset,na.rm = TRUE)) != 0){  ### makes sure the response is started at 0
      dset <- dset - minCat
    }

    if(method[1] == "novel"){

      mt_vek_ori <- apply(dset, 2L, max, na.rm = TRUE)
      mt_vek_ori <- max(mt_vek_ori)

      mt_vek_incl <- apply(matrix(dset[,c(incl_set)],ncol = length(seq_len(ncol(dset))[c(incl_set)])), 2L, max, na.rm = TRUE)

      mt_idx_incl <- rep(seq_along(mt_vek_incl), mt_vek_incl)

      betalist_incl <- as.vector(unlist(tapply(iqll$beta, mt_idx_incl, function(x){
        temp <- c(x,rep(0,(mt_vek_ori-length(x))))
        return(temp)
      })))

      betalength <- sum(apply(dset,2,function(x){
        temp <- max(x,na.rm = TRUE)-min(x,na.rm = TRUE)
        return(temp)
      }))

    } else {

      betalength <- ncol(dset)*max(iqll$mt_vek)
      betalist_incl <- iqll$beta

    }

    length(betalist_incl) <- betalength
    gamma.ret <- iqll$gamma
    length(gamma.ret) <- ncol(dset)

    iqll_params <- c(iqll$theta, betalist_incl, gamma.ret)


    if(ncol(dset) == length(incl_set)){
      oqll_params <- NA
    } else {

      if(method[1] == "novel"){

        mt_vek_excl <- apply(matrix(dset[,-c(incl_set)],ncol = length(seq_len(ncol(dset))[-c(incl_set)])), 2L, max, na.rm = TRUE)

        mt_idx_excl <- rep(seq_along(mt_vek_incl), mt_vek_incl)

        betalist_excl <- as.vector(unlist(tapply(oqll$beta, mt_idx_excl, function(x){
          temp <- c(x,rep(0,(mt_vek_ori-length(x))))
          return(temp)
        })))

      } else {

        betalist_excl <- oqll$beta

      }

      length(betalist_excl) <- betalength
      gamma.ret <- oqll$gamma
      length(gamma.ret) <- ncol(dset)

      oqll_params <- c(betalist_excl, gamma.ret)

    }
    length(iqll_params) <- n_par
    length(oqll_params) <- n_par - nrow(dset)

  }

  n_par <- sum(nrow(dset),((1+(max(dset,na.rm = TRUE)-min(dset,na.rm = TRUE)))*ncol(dset)))
  iqll_params <- c(iqll$theta, iqll$beta, iqll$gamma)
  if(ncol(dset) == length(incl_set)){
    oqll_params <- NA
  } else {
    oqll_params <- c(oqll$beta, oqll$gamma)
  }
  length(iqll_params) <- n_par
  length(oqll_params) <- n_par - nrow(dset)


  length(incl_set) <- ncol(dset)
  res <- c(res, incl_set, iqll_params, oqll_params)
  names(res) <- c("IQ-LL","OQ-LL",scoreName,rep("item no.",length(incl_set)),rep("iq-ll par.",length(iqll_params)),rep("oq-ll par.",length(oqll_params)))
  # res <- c(res, incl_set)
  # names(res) <- c("IQ-LL","OQ-LL",scoreName, rep("item no.",ncol(dset)))
  class(res) <- c("score",type[1],class(res))
  return(res)

}

compute_scores_unparalleled <- function(X, incl_sets, type = c("ipoqll","ipoqlldif"),
                                        step_direct = c("fixed","forward","backward"), groups_map = c(),
                                        init_par_iq = c(), init_par_oq = c(), setting_par_iq = c(),
                                        setting_par_oq = c(), method = c("fast","novel"), timeLimit = 3600){


  dset <- as.matrix(X)
  # incl_sets <- itemsets

  if(is.vector(incl_sets) & length(incl_sets) > ncol(dset)){
    stop("autoRasch ERROR: the number of items in the incl_set can not exceed the initial items.")
  }

  if(type[1] == "ipoqlldif"){
    if(is.null(groups_map)){
      stop("autoRasch ERROR: to use the `ipoqlldif`, `groups_map` must be provided.")
    }
  } else {
    type <- "ipoqll"
  }

  if(is.matrix(incl_sets) | is.null(step_direct)){
    step_direct <- "fixed"
  }

  if(step_direct[1] == "forward"){
    excl_set <- seq_len(ncol(dset))[-c(incl_sets)]
    add_items <- t(combn(excl_set,1))
    rep_itemsets <- matrix(rep.int(incl_sets,length(add_items)), nrow = length(add_items), byrow = TRUE)
    incl_sets <- cbind(rep_itemsets,add_items)
  } else if(step_direct[1] == "backward"){
    incl_sets <- t(combn(incl_sets,(length(incl_sets)-1)))
  } else if(step_direct[1] == "fixed"){
    incl_sets <- as.matrix(incl_sets)
    if((any(class(incl_sets) == "matrix")) & (dim(incl_sets)[2] == 1 | dim(incl_sets)[1] == 1)){
      incl_sets <- matrix(as.vector(incl_sets), ncol = 1)
    }
  }

  incl_sets <- as.matrix(incl_sets)

  i <- NULL

  scoreList <- foreach(i=seq_len(nrow(incl_sets)), .combine = rbind, .errorhandling = "stop") %dopar% {

    incl_set <- incl_sets[i,]
    incl_set <- incl_set[!is.na(incl_set)]
    incl_set <- sort(incl_set,decreasing = FALSE)

    # if(!is.null(init_par_iq) & !is.null(init_par_oq)){
    #
    #
    #   init_iq <- iqll_init(dset = dset, prev_incl_set = incl_sets, prev_par_iq = init_par_iq, prev_par_oq = init_par_oq,
    #                        incl_set = incl_set, direction = step_direct, type = type[1], groups_map = groups_map,
    #                        iq_noise = 1e-3)
    #
    #
    #
    #   init_oq <- oqll_init(dset = dset, prev_incl_set = incl_sets, prev_par_iq = init_par_iq, prev_par_oq = init_par_oq,
    #                        incl_set = incl_set, direction = step_direct, type = type[1], groups_map = groups_map,
    #                        oq_noise = 1e-3)
    # } else {
      init_iq <- c()
      init_oq <- c()
    # }
      # checkOS <- Sys.info()
      # if(checkOS['sysname'] == "Linux"){
      # log <- utils::capture.output(
      #   withCallingHandlers({
      #     setTimeLimit(elapsed = timeLimit)
      #     score_res <- compute_score(dset, incl_set = incl_set, type = type, groups_map = groups_map,
      #                           init_par_iq = init_iq, init_par_oq = init_oq,
      #                           optim_control_iq = optim_control_iq, optim_control_oq = optim_control_oq,
      #                           setting_par_iq = setting_par_iq, setting_par_oq = setting_par_oq,
      #                           method = method)
      #     res <- c(score_res)
      #   },
      #     error = function(e){
      #
      #   })
      # )
      # } else {
        withCallingHandlers({
          setTimeLimit(elapsed = timeLimit)
          score_res <- compute_score(dset, incl_set = incl_set, type = type, groups_map = groups_map,
                                     init_par_iq = init_iq, init_par_oq = init_oq,
                                     setting_par_iq = setting_par_iq, setting_par_oq = setting_par_oq,
                                     method = method)
          res <- c(score_res)
        },
        error = function(e){

        })
      # }


    # print(i)
    length(incl_set) <- ncol(dset)

    return(res)
  }

  res <- scoreList

  return(res)
}

#' @param incl_sets A matrix as a results of a \code{rbind} of \code{incl_set}.
#' @param cores Number of cores that is used in the paralellization.
#' @param step_direct How will you compute the criterion score. \code{fixed} for the given itemset,
#' \code{forward} computes all the scores of the possible combination of items if an item is added to the given set,
#' \code{backward}  computes all the scores of the possible combination of items if an item is removed to the given set.
#' @param timeLimit To limit the execution time of scores' computation.
#'
#' @return
#' \code{compute_scores} will return a matrix as a result of the \code{rbind} operation of the \code{compute_score}'s result.
#'
#' @import doParallel
#' @import foreach
#'
#' @rdname compute_score
#' @export
compute_scores <- function(X, incl_sets, type = c("ipoqll","ipoqlldif"),
                           step_direct = c("fixed","forward","backward"), groups_map = c(),
                           init_par_iq = c(), init_par_oq = c(),
                           setting_par_iq = c(), setting_par_oq = c(),
                           cores = NULL, method = c("fast","novel"), timeLimit = 3600){


  # incl_sets <- as.matrix(incl_sets)

  if(is.null(cores)){
    cores <- nrow(incl_sets)
    if(cores > 2){
      cores <- 2
    }
  } else {
    if(cores > detectCores()){
      cores <- detectCores()
    }
  }


  # checkOS <- Sys.info()
  # if(checkOS['sysname'] == "Linux"){
  #   doParallel::registerDoParallel(cores = cores)
  # } else {
    # oFuture::registerDoFuture()
    # future::plan(future::cluster, workers = cl)
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl=cl, cores = cores)
    # doParallel::registerDoParallel(cores = cores)
    parallel::clusterCall(cl, function(x) .libPaths(x), .libPaths())
  # }

  scoreList <- compute_scores_unparalleled(X = X, incl_sets = incl_sets, type = type,
                                           step_direct = step_direct, groups_map = groups_map,
                                          init_par_iq = init_par_iq, init_par_oq = init_par_oq,
                                          setting_par_iq = setting_par_iq, setting_par_oq = setting_par_oq, method = method, timeLimit = timeLimit)

  # checkOS <- Sys.info()
  # if(checkOS['sysname'] == "Linux"){
  #   doParallel::stopImplicitCluster()
  # } else {
    parallel::stopCluster(cl)
  # }
  foreach::registerDoSEQ()

  res <- scoreList

  return(res)
}


iqll_init <- function(dset, prev_incl_set, prev_par_iq, prev_par_oq, incl_set, direction, type, groups_map, iq_noise){

  old.set.iq <- prev_incl_set
  new.set.iq <- incl_set
  old.dataset.iq <- as.matrix(dset[,old.set.iq])
  nn.th <- max(dset,na.rm = TRUE)-min(dset,na.rm = TRUE)

  if(!is.null(groups_map)){
    n.resp.th <- ncol(as.matrix(groups_map))
  }
  nlmPar.old.iq <- prev_par_iq
  nlmPar.new.iq <- c()

  old.set.oq <- seq_len(ncol(dset))[-c(prev_incl_set)]
  new.set.oq <- seq_len(ncol(dset))[-c(incl_set)]
  old.dataset.oq <- as.matrix(dset[,old.set.oq])
  nlmPar.old.oq <- prev_par_oq

  if(direction == "backward"){
    item.no.iq <- which(!(old.set.iq %in% new.set.iq))
    idx.remv.iq.beta <- c((nrow(old.dataset.iq) + ((nn.th*item.no.iq)-(nn.th-1))):(nrow(old.dataset.iq)+(nn.th*item.no.iq)))
    idx.remv.iq.gamma <- c(nrow(old.dataset.iq)+(ncol(old.dataset.iq)*nn.th)+item.no.iq)
    if(type[1] == "ipoqlldif"){
      idx.remv.iq.delta <- c()
      for(i in 1: n.resp.th){
        idx.remv.iq.delta <- c(idx.remv.iq.delta,(nrow(old.dataset.iq)+((ncol(old.dataset.iq)*nn.th)+ncol(old.dataset.iq))+(ncol(old.dataset.iq)*(i-1))+item.no.iq))
      }
      idx.remv.iq <- c(idx.remv.iq.beta,idx.remv.iq.gamma,idx.remv.iq.delta)
    } else {
      idx.remv.iq <- c(idx.remv.iq.beta,idx.remv.iq.gamma)
    }
    nlmPar.new.iq <- nlmPar.old.iq[-c(idx.remv.iq)]
    nlmPar.new.iq <- nlmPar.new.iq + (runif(length(nlmPar.new.iq),-1,1)*iq_noise)
  } else if(direction == "forward"){
    item.no.iq <- which(!(new.set.iq %in% old.set.iq))
    idx.remv.iq.beta <- c((nrow(old.dataset.iq) + ((nn.th*item.no.iq)-(nn.th-1))):(nrow(old.dataset.iq)+(nn.th*item.no.iq)))
    idx.remv.iq.gamma <- c(length(idx.remv.iq.beta)+nrow(old.dataset.iq)+(ncol(old.dataset.iq)*nn.th)+item.no.iq)
    if(type[1] == "ipoqlldif"){
      idx.remv.iq.delta <- c()
      for(i in 1: n.resp.th){
        idx.remv.iq.delta <- c(idx.remv.iq.delta,(nrow(old.dataset.iq)+((ncol(old.dataset.iq)*nn.th)+ncol(old.dataset.iq))+(ncol(old.dataset.iq)*(i-1))+item.no.iq+length(idx.remv.iq.beta)+length(idx.remv.iq.gamma+(i-1))))
      }
      idx.remv.iq <- c(idx.remv.iq.beta,idx.remv.iq.gamma,idx.remv.iq.delta)
    } else {
      idx.remv.iq <- c(idx.remv.iq.beta,idx.remv.iq.gamma)
    }
    nlmPar.new.iq <- insert.at(nlmPar.old.iq,idx.remv.iq,(length(nlmPar.old.iq)+length(idx.remv.iq)))

    item.no.oq <- which(!(old.set.oq %in% new.set.oq))
    idx.remv.oq.beta <- c((((nn.th*item.no.oq)-(nn.th-1))):((nn.th*item.no.oq)))
    idx.remv.oq.gamma <- c((ncol(old.dataset.oq)*nn.th)+item.no.oq)
    if(type[1] == "ipoqlldif"){
      idx.remv.oq.delta <- c()
      for(i in 1: n.resp.th){
        idx.remv.oq.delta <- c(idx.remv.oq.delta,(((ncol(old.dataset.oq)*nn.th)+ncol(old.dataset.oq))+(ncol(old.dataset.oq)*(i-1))+item.no.oq))
      }
      idx.remv.oq <- c(idx.remv.oq.beta,idx.remv.oq.gamma,idx.remv.oq.delta)
    } else {
      idx.remv.oq <- c(idx.remv.oq.beta,idx.remv.oq.gamma)
    }
    nlmPar.new.iq[idx.remv.iq] <- nlmPar.old.oq[idx.remv.oq]
    nlmPar.new.iq <- nlmPar.new.iq + (runif(length(nlmPar.new.iq),-1,1)*iq_noise)
  }

  return(nlmPar.new.iq)
}

oqll_init <- function(dset, prev_incl_set, prev_par_iq, prev_par_oq, incl_set, direction, type, groups_map, oq_noise){
  old.set.iq <- prev_incl_set
  new.set.iq <- incl_set
  old.dataset.iq <- as.matrix(dset[,old.set.iq])
  nn.th <- max(dset,na.rm = TRUE)-min(dset,na.rm = TRUE)
  if(!is.null(groups_map)){
    n.resp.th <- ncol(as.matrix(groups_map))
  }
  nlmPar.old.iq <- prev_par_iq

  old.set.oq <- seq_len(ncol(dset))[-c(prev_incl_set)]
  new.set.oq <- seq_len(ncol(dset))[-c(incl_set)]
  old.dataset.oq <- as.matrix(dset[,old.set.oq])
  nlmPar.old.oq <- prev_par_oq
  nlmPar.new.oq <- c()

  if(direction == "forward"){
    item.no.oq <- which(!(old.set.oq %in% new.set.oq))
    idx.remv.oq.beta <- c((((nn.th*item.no.oq)-(nn.th-1))):((nn.th*item.no.oq)))
    idx.remv.oq.gamma <- c((ncol(old.dataset.oq)*nn.th)+item.no.oq)
    if(type[1] == "ipoqlldif"){
      idx.remv.oq.delta <- c()
      for(i in 1: n.resp.th){
        idx.remv.oq.delta <- c(idx.remv.oq.delta,(((ncol(old.dataset.oq)*nn.th)+ncol(old.dataset.oq))+(ncol(old.dataset.oq)*(i-1))+item.no.oq))
      }
      idx.remv.oq <- c(idx.remv.oq.beta,idx.remv.oq.gamma,idx.remv.oq.delta)
    } else {
      idx.remv.oq <- c(idx.remv.oq.beta,idx.remv.oq.gamma)
    }
    nlmPar.new.oq <- nlmPar.old.oq[-c(idx.remv.oq)]
    nlmPar.new.oq <- nlmPar.new.oq + (runif(length(nlmPar.new.oq),-1,1)*oq_noise)
  } else if(direction == "backward"){
    item.no.oq <- which(!(new.set.oq %in% old.set.oq))
    idx.remv.oq.beta <- c((((nn.th*item.no.oq)-(nn.th-1))):((nn.th*item.no.oq)))
    idx.remv.oq.gamma <- c(length(idx.remv.oq.beta)+(ncol(old.dataset.oq)*nn.th)+item.no.oq)
    if(type[1] == "ipoqlldif"){
      idx.remv.oq.delta <- c()
      for(i in 1: n.resp.th){
        idx.remv.oq.delta <- c(idx.remv.oq.delta,(((ncol(old.dataset.oq)*nn.th)+ncol(old.dataset.oq))+(ncol(old.dataset.oq)*(i-1))+item.no.oq+length(idx.remv.oq.beta)+length(idx.remv.oq.gamma)+(i-1)))
      }
      idx.remv.oq <- c(idx.remv.oq.beta,idx.remv.oq.gamma,idx.remv.oq.delta)
    } else {
      idx.remv.oq <- c(idx.remv.oq.beta,idx.remv.oq.gamma)
    }
    nlmPar.new.oq <- insert.at(nlmPar.old.oq,idx.remv.oq,(length(nlmPar.old.oq)+length(idx.remv.oq)))


    item.no.iq <- which(!(old.set.iq %in% new.set.iq))
    idx.remv.iq.beta <- c((nrow(old.dataset.iq) + ((nn.th*item.no.iq)-(nn.th-1))):(nrow(old.dataset.iq)+(nn.th*item.no.iq)))
    idx.remv.iq.gamma <- c(nrow(old.dataset.iq)+(ncol(old.dataset.iq)*nn.th)+item.no.iq)
    if(type[1] == "ipoqlldif"){
      idx.remv.iq.delta <- c()
      for(i in 1: n.resp.th){
        idx.remv.iq.delta <- c(idx.remv.iq.delta,(nrow(old.dataset.iq)+((ncol(old.dataset.iq)*nn.th)+ncol(old.dataset.iq))+(ncol(old.dataset.iq)*(i-1))+item.no.iq))
      }
      idx.remv.iq <- c(idx.remv.iq.beta,idx.remv.iq.gamma,idx.remv.iq.delta)
    } else {
      idx.remv.iq <- c(idx.remv.iq.beta,idx.remv.iq.gamma)
    }
    nlmPar.new.oq[idx.remv.oq] <- nlmPar.old.iq[c(idx.remv.iq)]
    nlmPar.new.oq <- nlmPar.new.oq + (runif(length(nlmPar.new.oq),-1,1)*oq_noise)
  }

  return(nlmPar.new.oq)
}

insert.at <- function(a, pos, max.nlmpar){
  addLast <- FALSE
  pos <- (c(pos)-c(seq_along(pos)))

  if(pos[length(pos)] == (max.nlmpar-length(pos))){
    pos <- pos[-c(length(pos))]
    a <- c(a,0)
    addLast <- TRUE
  }
  if(!identical(pos, integer(0))){
    if(pos[1] == 0){
      length.begin <- length(which(pos == 0))
      pos <- pos[-c(1:length.begin)]
      pos <- pos + length.begin
      a <- c(rep.int(0,length.begin),a)
    }
  }
  if(!identical(pos, integer(0)) & !identical(pos, numeric(0))){
    pos.idx <- split(pos,pos)
    dots <- rapply(pos.idx,function(x) x*0, how = "replace")
    pos <- unique(pos)
    stopifnot(length(dots)==length(pos))
    result <- vector("list",2*length(pos)+1)
    result[c(TRUE,FALSE)] <- split(a, cumsum(seq_along(a) %in% (pos+1)))
    result[c(FALSE,TRUE)] <- dots

    result <- unlist(result)
  } else {
    result <- a
  }
  return(result)
}

#' @param object The object from the class \code{score}. The result of the score computation.
#' @param ... further argument passed or from other method.
#'
#' @rdname compute_score
#' @export
summary.score <- function(object, ...){
  dotdotdot <- list(...)
  cat("\n")
  cat("Score of the itemsets: ")
  cat("\n\n")
  cat("IQ-LL: ", object[1])
  cat("\nOQ-LL: ", object[2])
  cat("\nIPOQ-LL: ", object[3])
  cat("\n\n")
}
