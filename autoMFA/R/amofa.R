#' Adaptive Mixture of Factor Analyzers (AMoFA)
#'
#'@description An implementation of the Adaptive Mixture of Factor Analyzers (AMoFA)
#' algorithm from \insertCite{kaya2015adaptive}{autoMFA}. This code is a R port of the MATLAB code which was
#' included with that paper.
#'
#' @param data An \emph{n} by \emph{p} data matrix, where \emph{n} is the number of
#' observations and \emph{p} is the number of dimensions of the data.
#' @param itmax The maximum number of EM iterations allowed for the estimation of each MFA model.
#' @param verbose Boolean indicating whether or not to print more
#' verbose output, including the number of EM-iterations used and the total
#' running time. Default is FALSE.
#' @param varimax Boolean indicating whether the output factor loading matrices should be constrained
#' using varimax rotation or not. 
#'@return A list containing the following elements:
#' \itemize{
#' \item{\code{model}:}{ A list specifying the final MFA model. This contains: \itemize{
#'            \item{\code{B}:}{ A list containing the factor loading matrices for each component.}
#'             \item{\code{D}:}{ A \emph{p} by \emph{p} by \emph{g} array of error variance matrices.}
#'            \item{\code{mu}:}{  A \emph{p} by \emph{g} array containing the mean of each cluster.}
#'           \item{\code{pivec}:}{ A 1 by g vector containing the mixing
#'             proportions for each FA in the mixture.}
#'           \item{\code{numFactors}:}{ A \emph{1} by \emph{g} vector containing the number of factors for each FA.}}
#'           }
#'\item{\code{clustering}:}{ A list specifying the clustering produced by the final model. This contains: \itemize{
#'             \item{\code{responsibilities}:}{ A \emph{n} by \emph{g} matrix containing the probability
#'               that each point belongs to each FA in the mixture.}
#'             \item{\code{allocations}:}{ A \emph{n} by 1 matrix containing which
#'               FA in the mixture each point is assigned to based on the responsibilities.}}}
#'\item{\code{diagnostics}:}{ A list containing various pieces of information related to the fitting process of the algorithm. This contains: \itemize{
#'             \item{\code{bic}:}{ The BIC of the final model.}
#'             \item{\code{logL}:}{ The log-likelihood of the final model.}
#'             \item{\code{totalEM}:}{ The total number of EM iterations used.}
#'             \item{\code{progress}:}{ A matrix containing information about the decisions
#'               made by the algorithm.}
#'             \item{\code{times}:}{ The time taken for each loop in the algorithm.}
#'             \item{\code{totalTime}:}{ The total time taken to fit the final model.}}}
#'}
#'
#'@references \insertRef{kaya2015adaptive}{autoMFA}
#'
#' @export
#'
#' @examples
#' RNGversion('4.0.3'); set.seed(3)
#' MFA.fit <- amofa(autoMFA::MFA_testdata)
#'
amofa <- function(data, itmax = 100, verbose  = FALSE, varimax = FALSE) {
  #
  start.time <- Sys.time()
  # [mixture,trainingHistory] =imofa_mml_ext(data,progressLimit)
  # mixture has fields Lambda, Psi, Mu, Pi, numFactors
  # numFactors indicates number of factors per component
  # trainingHistory has fields logsTra, bestdl, descriptionLength, progress,
  # totalEM, models (MoFA models after each adaptive step)

  # Original IMoFA-L is developed by Albert Ali Salah
  # Extension to AMoFA by Heysem Kaya
  # 1-factor analytic solution based on Z. Ghahramani's ffa code

  # -----------------------------------------------------------------------
  # Copyleft (2014): Heysem Kaya and Albert Ali Salah
  #

  # This software is distributed under the terms
  # of the GNU General Public License Version 3
  #
  # Permission to use, copy, and distribute this software for
  # any purpose without fee is hereby granted, provided that this entire
  # notice is included in all copies of any software which is or includes
  # a copy or modification of this software and in all copies of the
  # supporting documentation for such software.
  # This software is being provided "as is", without any express or
  # implied warranty.  In particular, the authors do not make any
  # representation or warranty of any kind concerning the merchantability
  # of this software or its fitness for any particular purpose."
  # ----------------------------------------------------------------------
 #
  stopifnot("verbose should be either TRUE or FALSE"={verbose %in% c(TRUE,FALSE)})
  stopifnot("varimax should be either TRUE or FALSE"={varimax %in% c(TRUE,FALSE)})
  stopifnot("data should be an n by p numeric matrix"={is.numeric(data)&is.matrix(data)})
  stopifnot("itmax should be numeric"={is.numeric(itmax)})
  stopifnot("itmax should be an integer greater than or equal to 1"={(itmax%%1==0) & (itmax >=1)})
  
  ModelSelCri <- 1
  maxIter <- itmax
  #max. EM iterations per run
  maxTrial <- 3
  #try cont_mfa 3 times before giving up
  #continue incremental model evolution till last 'progressLimit' steps fail
  # progressLimit = 1 (no bias), stop as soon as improvement stops
  progressLimit <- 1
  printDownsize <- 0

  noProgress <- 0

  tempdim <- dim(data)
  numSamples <-  tempdim[1]
  numDims <- tempdim[2]
  lambdaSmall <- 0.001

  dd <- numDims * (numDims + 2)
  #for Mardia kurtosis metric
  myepsilon <- 1e-5

  maxFactor <- numDims * 0.8

  downsizeOp <-
    1
  # option 1 is min Pi, option 2 is max DL to remove
  numFactors <- 1
  #initial number of factors per component
  numMeans <- 1

  totalEM <- 0
  #the total number of EM iterations
  logc = log2(2.865064)

  #Rissanen J., Information and Complexity in Statistical Modeling, Springer, 2007, p.15
  cat(sprintf("AMOFA: Fitting initial model\n"))
  #fit a 1-factor, 1-component (factor analysis) model
  ffaOut <- ffa(data, numFactors, maxIter)

  Lambda1 <- ffaOut$L
  Psi1 <- ffaOut$Ph
  logsTra <- ffaOut$LL
  Mu1 = matrix(colMeans(data))
  
  Pi = 1

  totalEM = totalEM + dim(logsTra)[2]
  #add to total number of iterations

  #means are stored as column vectors

  count = 1

  DataLogLike = loglike3(data, Lambda1, Psi1, Mu1, 1, numFactors, 1000)

  logsCv = DataLogLike
  #keeps track of validation loglike during the whole algorithm

  tLog = exp(loglike2(data, Lambda1, Psi1, Mu1))
  #the total weighted likelihoods for each sample
  #normalize tLog
  tLogN = ones(numSamples, 1)
  #normalized posteriors

  #create the initial batch variables
  Lambda = Lambda1

  Psi = Psi1

  Mu = Mu1


  model_0 <- vector(mode = "list", length = 1)
  model_0$numSamples = numSamples

  model_0$Mu = Mu

  model_0$Lambda = Lambda

  model_0$Pi = Pi

  model_0$Psi = Psi

  model_0$numFactors = numFactors

  tempdesclen = getDescLen(model_0, DataLogLike, ModelSelCri)

  dlength = tempdesclen$descLength

  dl <- vector()
  dl[1] = dlength



  modelNames <-
    c("Mu",
      "Lambda",
      "Pi",
      "Psi",
      "numFactors",
      "logsTra",
      "dl",
      "action",
      "tLog",
      "tLogN")
  models <- vector(mode = "list", length = 1)
  models[[1]] <-
    setNames(vector("list", length(modelNames)), modelNames)
  models[[1]]$Mu = Mu

  models[[1]]$Lambda = Lambda

  models[[1]]$Pi = Pi

  models[[1]]$Psi = Psi

  models[[1]]$numFactors = numFactors

  models[[1]]$dl = dlength

  models[[1]]$logTra = vector()

  models[[1]]$action = -1


  progress = zeros(1, 3)
  progress[1, 1] = dim(logsTra)[2]
  #1st column keeps track of iterations
  #progress(1,2) = 0; #which component
  #progress(1,3) = 0; #what is done
  progressCount = 2


  cont_EM = 1
  #flag to indicate EM continuation
  direction = 1
  #1 -> up, -1 -> down
  loop.times <- c()
  while (cont_EM) {
    #
    loop.start <- Sys.time()
    #CvLog > oldCvLog
    action = -1
    #No action
    factorCount = 0

    if (numMeans == 1) {
      componentToSplit = 1

      componentToAddFactor = 1

      splitThreshold = numDims * (numFactors[1] + 2) + 2

      #in case no split is possible don't attempt to do it!
      if (numSamples < splitThreshold) {
        componentToSplit = -1

      }
    } else {
      #
      factorCount2 = 0
      if (exists("splitMetric")) {rm(splitMetric)}
      if (exists("factorMetric")) {rm(factorMetric)}

      ll <- matrix(Rfast::rowMaxs(tLogN, value = TRUE))
      indic <- matrix(Rfast::rowMaxs(tLogN, value = FALSE))

      #
      if (direction < 0) {
        componentToSplit = -1

        componentToAddFactor = -1

        #kill the weakest component

        # or kill the component whose message cost is maximum
        factorCount3 = 0

        comp_DLs = zeros(numMeans, 1)

        for (comp in 1:numMeans) {
          cluster_k = (indic == comp)

          localdata = data[cluster_k, ]

          rngLambda = (factorCount3 + 1):(factorCount3 + numFactors[comp])

          factorCount3 = factorCount3 + numFactors[comp]

          comp_DLs[comp] = Pi[comp] * numSamples - (numDims * (numFactors[comp] +
                                                                 2)) / 2
        }

        if (downsizeOp == 1) {
          minPi = min(Pi)

          k = which.min(Pi)
        } else {
          maxDL = min(comp_DLs)
          k = which.min(comp_DLs)

        }

        startL = 1

        endL = numFactors[k]

        if (k > 1) {
          #
          startL = sum(matrix(numFactors,nrow = 1)[1, (1:(k - 1))]) + 1

          endL = sum(numFactors[1:k])

        }
        Mu <- matrix(Mu[, -k], ncol = dim(Mu)[2] - 1)
        Psi <- matrix(Psi[, -k], ncol = dim(Psi)[2] - 1)
        Lambda <- matrix(Lambda[, -(startL:endL)], ncol = dim(Lambda)[2] - length(startL:endL))
        Pi <- Pi[-k]
        Pi <- Pi / sum(Pi)
        tLogN <- matrix(tLogN[, -k],ncol = dim(tLogN)[2] - 1)
        if (dim(tLogN)[2] == 1){
          tLogN <- ones(numSamples,1)
        } else {
        tLogN <-
          tLogN / matrix(rep(rowSums(tLogN), dim(tLogN)[2]), ncol = dim(tLogN)[2]) }
        tLog <- tLog[, -k]

        numFactors <- numFactors[-k]

        numMeans <- numMeans - 1

        ll <- Rfast::rowMaxs(tLogN, value = TRUE)
        indic <- Rfast::rowMaxs(tLogN, value = FALSE)
        if(printDownsize){
        cat(sprintf('\nDownsizing: component annihilation begun\n')); printDownsize = 0}

        iMu = Mu
        iLambda = Lambda
        iPsi = Psi
        iPi = Pi
        inumFactors = numFactors

        # now run locally detoxified factor analyzers in the mixture
        tempModel = cont_mfa_fj(data, iMu, iLambda, iPsi, iPi, inumFactors, Inf, 10, verbose = verbose)

        Lambda <- tempModel$Lambda
        Psi <- tempModel$Psi
        Mu <- tempModel$Mu
        Pi <- tempModel$Pi
        numFactors <- tempModel$numFactors
        logs <- tempModel$logs
        minDL <- tempModel$minDL


        if (numMeans != dim(Mu)[2]) {
          numMeans = dim(Mu)[2]
        }

        #

        tLog = exp(loglike4(data, Lambda, Psi, Mu, Pi, numFactors))
        tLogN = tLog / matrix(rep(rowSums(tLog), dim(tLog)[2]), ncol = dim(tLog)[2]) ##### Check --> repmat(sum(tLog,2),1,size(tLog,2));
        models=logModel(models,Mu,Lambda,Pi,Psi,numFactors,dumLogs,minDL,action,tLog,tLogN);
        progress <- rbind(progress, c(dim(dumLogs)[2],0,0)) #Need to update the two other values later ...
        dl[progressCount]=minDL;
        progressCount = progressCount+1;
      } else {
        ll <- Rfast::rowMaxs(tLogN, value = TRUE)
        indic <- Rfast::rowMaxs(tLogN, value = FALSE)

        for (k in 1:numMeans) {
          ks = as.character(k)

          #find the data set that is associated with this component
          ll <- Rfast::rowMaxs(tLogN, value = TRUE)
          indic <- Rfast::rowMaxs(tLogN, value = FALSE)
          cluster_k = (indic == k)
          dat = data[cluster_k, ]
          kD = (factorCount2 + 1):(factorCount2 + numFactors[k])
          factorCount2 = factorCount2 + numFactors[k]
          tLambda = matrix(Lambda[, kD],ncol = length(kD))
          tempCov = tLambda %*% t(tLambda) + diag(as.vector(Psi[, k]))

          #Mardia multivariate kurtosis metric
          invCov = solve(tempCov)
          tMu = t(Mu[, k])
          b_sum = 0

          for (i in (1:numSamples)) {
            x = data[i, ] - tMu
            #centered data
            b_sum = b_sum + tLogN[i, k] * expm::`%^%`((x %*% invCov %*% t(x)),2)

          }
          b_sum = b_sum / sum(tLogN[, k])


          if (!exists("splitMetric")) {splitMetric <- vector()}
          #
          splitMetric[k] = abs((b_sum - dd) / sqrt(8 * dd / sum(tLogN[, k])))


          # factor metric: the sum of squares of the covariance differences
          #
          if (!exists("factorMetric")) {factorMetric <- vector()}
          factorMetric[k] = sum(sum(((
            tempCov - cov(dat)
          ) * (
            1 - diag(dim(tempCov)[1])
          )) ^ 2))


          #if the size of the component is too small, do not split it
          # here we check if the equal sized newborn components will
          # survive the next step

          # component params *2
          # for numFactors consider the new case case
          # (numfactors(k)+1+2)
          factorThreshold = (numDims * (numFactors[k] + 3) + getIntCodeLen(numFactors[k] +
                                                                             1) + logc) / 2

          # for a component to split it needs twice support of its
          # current annihilation threshold: C(k)=T(k)/2 so threshold
          # is C(k) plus the cost for mixture proportions
          splitThreshold = numDims * (numFactors[k] + 2) + 2


          if (Pi[k] * numSamples <= splitThreshold) {
            #
            if(verbose){
           message(paste(
              'Split not allowed due to insufficient soft support for component ',
              k
            ))}

            splitMetric[k] = -Inf

          }
          #if the number of factors are too much, do not consider adding new factors
          if ( (numFactors[k] > numDims*0.8) ||
              (Pi[k]*numSamples <= factorThreshold)) {
            factorMetric[k] = -Inf

          }
        }
        #we have looped for all components

        componentToSplit = which.max(splitMetric)
        #greatest total deviation from 3 in kurtosis or greatest deviation from zero
        componentToAddFactor = which.max(factorMetric)
        #greatest covariance difference
        #
        if (max(factorMetric) == -Inf) {
          #we don't want to add factors
          componentToAddFactor = -1

        }
        if (max(splitMetric) == -Inf) {
          #we don't want to add components
          componentToSplit = -1

        }
      }
    }

    if (componentToAddFactor > 0 &&
        numFactors[componentToAddFactor] >= maxFactor) {
      componentToAddFactor = -1

    }

    #initialize newLogs, to select which action pays off better
    newLogs = zeros(1, 2)


    #split the componentToSplit
    k = componentToSplit

    if (k > -1 & direction > 0) {
      ks = as.character(k)
      #first find the split
      ll <- Rfast::rowMaxs(tLogN, value = TRUE)
      indic <- Rfast::rowMaxs(tLogN, value = FALSE)
      cluster_k = (indic == k)

      localdata = data[cluster_k, ]

      if (prod(dim(localdata)) <= 1) {
        if(verbose){
        message('No local data for splitting')}

        componentToSplit = -1

        newLogs[1] = -Inf

        newDlength[1] = Inf

      } else {



        eval(parse(
          text = paste(
            'templocal = softsplit_fj(data=localdata,lambda = Lambda',
            ks,
            ',psi = Psi',
            ks,
            ',posteriors = matrix(tLogN[cluster_k,k],ncol=1),mu = Mu',
            ks,
            ',verbose = verbose)',
            sep = ""
          )
        ))
        Lambda_l = templocal$Lambda
        Psi_l = templocal$Psi
        Mu_l = templocal$Mu
        Pi_l = templocal$Pi
        NumFactors_l = templocal$numFactors
        logs_l = templocal$logs
        minDL_l = templocal$minDL
        #create a dummy set of batch variables
        oldFactorCount = sum(numFactors)

        if (k == 1){
          factorCount = 0
        } else {
          #
          factorCount = sum(matrix(numFactors,nrow = 1)[1, (1:(k - 1))])
        }

        found = 0

        reject = 0

        if (!is.infinite(minDL_l)) {
          if (dim(Mu_l)[2] == 2) {
            # update Lambda: the local dimensionality of new components
            # may vary

            if (factorCount < 1) { dumLa = vector() } else {dumLa = Lambda[, 1:factorCount]}
            if (factorCount + numFactors[k] + 1 > oldFactorCount) {dumLb = vector()} else {dumLb = Lambda[, (factorCount + numFactors[k] + 1):oldFactorCount]}


            dumLambda = unname(cbind(dumLa, Lambda_l[, 1:NumFactors_l[1]],
                             dumLb, Lambda_l[, (NumFactors_l[1] +1):sum(NumFactors_l)]))


            tmpNumFactors = numFactors


            tmpNumFactors[k] = NumFactors_l[1]
            #local split can adapt the number of factors :)
            dumNumFactorsSplit = unname(cbind(matrix(tmpNumFactors,nrow = 1), NumFactors_l[2]))

            #Init components for global EM sweep
            dumPsi = cbind(Psi, Psi_l[, 2])

            dumPsi[, k] = Psi_l[, 1]

            dumMu = cbind(Mu, Mu_l[, 2])

            dumMu[, k] = Mu_l[, 1]

            dumPi = matrix(Pi)

            prevPi = dumPi[k, 1]

            dumPi[k, 1] = prevPi * Pi_l[1]
            #the prior of the component is halved
            dumPi = rbind(dumPi, prevPi * Pi_l[2])


          } else {
            # if one of the components are annihilated during
            # initialization of split then use the new params
            reject = 1
            if(verbose){
            message(paste('Split is rejected during initialization for component ',ks))}
            dumMu[, k] = Mu_l[, 1]

            dumNumFactorsSplit[k] = NumFactors_l[1]
            if((factorCount + numFactors[k] + 1) <= oldFactorCount){
            dumLambda = cbind(Lambda[, 1:factorCount], matrix(Lambda_l[, 1:NumFactors_l[1]], ncol = NumFactors_l[1]),
                              Lambda[, (factorCount + numFactors[k] + 1):oldFactorCount])
            } else {
            dumLambda = cbind(Lambda[, 1:factorCount], matrix(Lambda_l[, 1:NumFactors_l[1]], ncol = NumFactors_l[1]))
            }

            if(k > ncol(dumPsi)){
            dumPsi = cbind(dumPsi , Psi_l[, 1])
            } else {
            dumPsi[, k] = Psi_l[, 1]}

            # no change in dumPi
            dumPi = Pi

          }

          if (reject == 0) {
            trials = 0


            while (trials < maxTrial) {


              #
              dummyTC <- tryCatch( cont_mfa_fj(data,
                          dumMu,
                          dumLambda,
                          dumPsi,
                          dumPi,
                          dumNumFactorsSplit,
                          Inf,
                          maxIter, verbose = verbose), error = function(e){
                            warning(paste('Warning: cont_mfa2 could not split component ',ks))
                            return( errorFlag = 1 )
                          })

              if (typeof(dummyTC) == "list"){

              dumLambdaSplit <- dummyTC$Lambda
              dumPsiSplit <- dummyTC$Psi
              dumMuSplit <- dummyTC$Mu
              dumPiSplit <- dummyTC$Pi
              dumNumFactorsSplit <- dummyTC$numFactors
              dumLogsSplit <- dummyTC$logs
              newDlength <- vector()
              newDlength[1] <- dummyTC$minDL

              #
              totalEM = totalEM + dim(dumLogsSplit)[2]

              #add to total number of iterations
              csMN <-
                c("numSamples",
                  "Mu",
                  "Lambda",
                  "Pi",
                  "Psi",
                  "numFactors",
                  "totalEM")
              model_cs <- vector(mode = "list")
              model_cs  <-
                setNames(vector("list", length(csMN)), csMN)
              model_cs$numSamples = numSamples
              model_cs$Mu = dumMuSplit
              model_cs$Lambda = dumLambdaSplit
              model_cs$Pi = dumPiSplit
              model_cs$Psi = dumPsiSplit
              model_cs$numFactors = dumNumFactorsSplit
              model_cs$totalEM = totalEM
              trials = maxTrial
              found = 1
              } else {
              #we had no problems
              #increment the trial counter
              #warning(paste('Warning: cont_mfa2 could not split component ',ks))
              trials = trials + 1 }
              }
            }
            else {
              newDlength[1] = Inf
              }

          }
          #check whether the trials succeeded
          if (!found) {
            #newLogs(1) = -Inf;
            newDlength[1] = Inf

          }
      }
      } else {
          #newLogs(1) = -Inf;
          newDlength[1] = Inf

      }

        #now try adding a factor to componentToAddFactor
        k = componentToAddFactor

        if (k > -1 & direction > 0) {
          ks = as.character(k)

          #find the data subset for which the posterior is greatest for this component, use tLog
          #newPsi is dummy...
          #
          eval(parse(
            text = paste(
              'templocal2 = softresidual(data,Lambda',
              ks,
              ',Psi',
              ks,
              ',tLogN[,k],Mu',
              ks,
              ');',
              sep = ""
            )
          ))

          resids <- templocal2$resids
          dumLambda <- templocal2$newLambda
          newPsi <- templocal2$newPsi

          newFactor = matrix(dumLambda[, dim(dumLambda)[2]])

          #prepare new dumLambda by adding the new factor to Lambda after Lambda_k

          t = sum(matrix(numFactors,nrow=1)[1, 1:k])

          if ((t + 1) > sum(numFactors)) {dumLc <- vector()} else {dumLc <- matrix(Lambda[, (t + 1):sum(numFactors)],ncol = sum(numFactors) -t  )}

          dumLambda = cbind(matrix(Lambda[, 1:t],ncol = t), newFactor, dumLc)

          dumNumFactorsFactor = numFactors

          dumNumFactorsFactor[k] = dumNumFactorsFactor[k] + 1

          #Psi correction
          dumPsi = Psi

          dumPsi[, k] = matrix(dumPsi[, k]) - newFactor^2

          #threshold it to eliminate negative values
          dumPsi = ((dumPsi >= 0.0001) * dumPsi) + ((dumPsi < 0.0001) *
                                                      0.0001)
          #let it converge
          trials = 0

          found = 0

          while (trials < maxTrial) {

            dummyTC2 <- tryCatch(cont_mfa_fj(data,
                                             Mu,
                                             dumLambda,
                                             dumPsi,
                                             Pi,
                                             dumNumFactorsFactor,
                                             Inf,
                                             maxIter, verbose=verbose), error = function(e){
                                               warning(paste('Warning: cont_mfa_fj could not split component ',ks))
                                               return( errorFlag = 1 )
                                             })
            if (typeof(dummyTC) == "list"){

              dumLambdaFactor <- dummyTC2$Lambda
              dumPsiFactor <- dummyTC2$Psi
              dumMuFactor <- dummyTC2$Mu
              dumPiFactor <- dummyTC2$Pi
              dumNumFactorsFactor <- dummyTC2$numFactors
              dumLogsFactor <- dummyTC2$logs
              newDlength[2] <- dummyTC2$minDL
              totalEM = totalEM + dim(dumLogsFactor)[2]

              #add to total number of iterations
              fMN <-
                c("numSamples",
                  "Mu",
                  "Lambda",
                  "Pi",
                  "Psi",
                  "numFactors",
                  "totalEM")
              model_f <- vector(mode = "list")
              model_f  <-
                setNames(vector("list", length(fMN)), fMN)
              model_f$numSamples = numSamples
              model_f$Mu = dumMuFactor
              model_f$Lambda = dumLambdaFactor
              model_f$Pi = dumPiFactor
              model_f$Psi = dumPsiFactor
              model_f$numFactors = dumNumFactorsFactor
              model_f$totalEM = totalEM
              trials = maxTrial
              found = 1
            } else {
              #we problems
              #increment the trial counter
              trials = trials + 1 }
          }

            #we had no problems
          #check whether the trials succeeded
          if (!found) {
            newDlength[2] = Inf

          }

        } else {
          newDlength[2] = Inf

        }

        #select the best likelihood
        minDlenght = min(newDlength)

        action = which.min(newDlength)
        if (action == 1) {
          chosenComponent = componentToSplit

        } else if (action == 2) {
          chosenComponent = componentToAddFactor
        }

        deltaProgress = abs((minDlenght - dl[progressCount - 1]) / dl[progressCount -
                                                                        1])


        if ((minDlenght < dl[progressCount - 1])
          & (deltaProgress > myepsilon)) {
            noProgress = 0

          } else {
            noProgress = noProgress + 1

            if ((direction > 0) &
                (noProgress >= progressLimit | is.infinite(minDlenght))) {
              direction = -1
              printDownsize = 1
              noProgress = 0

            } else if (direction < 0 & numMeans == 1) {
              cont_EM = 0

            }
          } #if cvlog>oldlog

        if (!is.infinite(minDlenght)) {
          #record what we are doing
          progress = rbind(progress, matrix(c(0,chosenComponent,action),nrow = 1)) #progressCount
          
          #execute the increment
          k = chosenComponent

          ks = as.character(chosenComponent)


          if (action == 1) {
            #split the chosen component
            if(verbose){
            message(paste('Splitting component ', ks))}

            Lambda = model_cs$Lambda
            #dumLambdaSplit;
            Psi = model_cs$Psi
            # dumPsiSplit;
            Mu = model_cs$Mu
            # dumMuSplit;
            Pi = model_cs$Pi
            # dumPiSplit;
            dumLogs = dumLogsSplit

            numFactors = model_cs$numFactors
            # dumNumFactorsSplit;
            nm = dim(Mu)[2]


            numMeans = nm

          } else if (action == 2) {
            #add a new factor to the new component
            if(verbose){
            message(paste('Adding a factor to component ', ks))}
            #
            Lambda = model_f$Lambda

            Psi = model_f$Psi

            Mu = model_f$Mu

            Pi = model_f$Pi

            numFactors = model_f$numFactors
            # dumNumFactorsSplit;
            dumLogs = dumLogsFactor

            nm = dim(Mu)[2]
            
            numMeans = nm

          } #if action...
          models = logModel(
            models,
            Mu,
            Lambda,
            Pi,
            Psi,
            numFactors,
            dumLogs,
            minDlenght,
            action,
            tLog,
            tLogN
          )

          progress[progressCount, 1] = dim(dumLogs)[2]

          dl[progressCount] = minDlenght

          #unravel parameters
          factorCount = 0

          #correct too small values of Lambda and Psi

          patch1 = (abs(Lambda) < lambdaSmall) * (Lambda < 0)
          #negative small values
          patch2 = (abs(Lambda) < lambdaSmall) * (Lambda >= 0)
          #positive small values
          Lambda = Lambda + patch1 * (-lambdaSmall) + patch2 * lambdaSmall

          Psi = Psi + (abs(Psi) < 0.0001) * 0.0001
          #and that's a patch...

          for (k in 1:numMeans) {
            ks = as.character(k)

            eval(parse(
              text = paste(
                'Lambda',
                ks,
                '=matrix(Lambda[,(factorCount+1):(factorCount+numFactors[k])], ncol = numFactors[k])',
                sep = ""
              )
            ))

            factorCount = factorCount + numFactors[k]

            eval(parse(text = paste('Mu', ks, '=matrix(Mu[,k])', sep = "")))

            eval(parse(text = paste('Psi', ks, '=matrix(Psi[,k])', sep = "")))


          }

         #

          tLog = exp(loglike4(data, Lambda, Psi, Mu, t(Pi), numFactors))

          #normalize tLog
          if ( dim(tLog)[2]  == 1) {
            #
            tLogN = ones(numSamples, 1)

          } else {
            #
            tLogN = tLog / matrix(rep(rowSums(tLog), dim(tLog)[2]), ncol = dim(tLog)[2])
            if (sum(sum(is.nan(tLogN))) > 0) {
              #somewhere, there was a division by zero...
              dummyProb = 1 / numMeans
              #uniform prob...
              for (i in (1:dim(tLogN)[1])) {
                if (is.nan(tLogN[i, 1])) {
                  # once one is NaN, the whole line is NaN
                  tLogN[i, ] = ones(1, numMeans) * dummyProb

                }
              }
            }
            if (min(colSums(t(tLogN))) < 0.9999) {
              #there is a too small log somewhere...
              smallLogIndices = find(colSums(t(tLogN)) < 0.1);
for (i in smallLogIndices) {
  maxEl = which.max(tLog[i,]);
  tLogN[i,maxEl]=1;
  tLogN[i,] = tLogN[i,] / sum(tLogN[i,]);
}
            }
          } #normalize tLog

          logsTra = cbind(logsTra, dumLogs)

          #logsDL = [logsCv CvLog];
          progressCount = progressCount + 1


        }
    #
      if(direction > 0 && verbose  == TRUE){
      message(paste("Used ", totalEM, " EM iterations.",sep = ""))} else if(direction > 0 && verbose  == FALSE){
        progress.amofa.vbmfa(totalEM)
      }
      loop.end <- Sys.time()
      if(direction > 0 && verbose  == TRUE){
      message(paste("Loop time: ", loop.end - loop.start, "s. Cumulative running time: ", sum(loop.times) , "s"))}
      loop.times <- c(loop.times, loop.end - loop.start)
      } #while

      count = 0

      bestDL = min(dl)
      bestind = which.min(dl)

      bestmodel = models[[bestind]]

      mixtureNames <-
        c("Lambda", "Mu",  "Psi", "numFactors", 'Pi')
      mixture <-
        setNames(vector("list", length(mixtureNames)), mixtureNames)

      loglikeVal <- loglike4(data,bestmodel$Lambda,bestmodel$Psi,bestmodel$Mu,bestmodel$Pi,bestmodel$numFactors)
      sumLoglik <- logsumexp(loglikeVal,2)
      ll <- sum(sumLoglik)
      bestmodel$numFactors <- matrix(bestmodel$numFactors, nrow = 1)
      g <- dim(bestmodel$numFactors)[2]; p <- ncol(data);
      d <- (g-1) + 2*g*p  + sum(p*as.vector(bestmodel$numFactors) -
                                   0.5*as.vector(bestmodel$numFactors)*(as.vector(bestmodel$numFactors)-1))
      bic <- log(numSamples)*d-2*ll;
      loglikeVal <- loglikeVal-matrix(rep(sumLoglik,g),ncol = g)
      responsibility <- exp(loglikeVal);
      clustering <- Rfast::rowMaxs(loglikeVal);

      Lambda <- vector(mode = 'list'); Mu <- vector(mode = 'list');
      Psi <- vector(mode = 'list');
        for (i in 1:g) {
          Lambda[[i]] <- matrix(bestmodel$Lambda[, (count + 1):(count + bestmodel$numFactors[i])], ncol = bestmodel$numFactors[i])
          Mu[[i]] <-  matrix(bestmodel$Mu[, i], ncol = 1)
          Psi[[i]] <- diag(bestmodel$Psi[, i])

          count = count +  bestmodel$numFactors[i]

        }

      #
      out <- vector(mode = 'list')
      #Model parameters
      out$model$pivec =  matrix(bestmodel$Pi,nrow = 1)
      out$model$mu = abind::abind(Mu,along = 3)
      out$model$B = Lambda
      out$model$D = abind::abind(Psi,along=3)
      out$model$numFactors <- bestmodel$numFactors
      #Diagnostic quantities
      out$diagnostics$bic = bic
      out$diagnostics$logL = ll
      out$diagnostics$totalEM = totalEM
      out$diagnostics$progress = progress

      #Clustering information
      out$clustering$responsibilities = responsibility; #Class conditional responsibilities
      out$clustering$allocations = clustering; #Clustering allocations

      end.time <- Sys.time()
      out$diagnostics$times = loop.times
      out$diagnostics$totalTime = difftime(end.time,start.time,units = "secs")

      cat(sprintf(paste('Training complete in', out$diagnostics$totalTime , 's, with', totalEM, 'EM iterations used.\n' )))

      
      if(varimax){
        no.comp <- length(out$model$B)
        for(i in 1:no.comp){
          out$model$B[,,i] <- matrix(out$model$B[,,i], nrow = p) %*% stats::varimax(matrix(out$model$B[,,i], nrow = p))$rotmat
        }
      }
      return(out)
    }


