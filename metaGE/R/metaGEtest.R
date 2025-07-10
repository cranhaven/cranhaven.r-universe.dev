#' @import utils
## quiets concerns of R CMD check re: the .'s that appear in pipelines
#if(getRversion() >= "2.15.1")  utils::globalVariables(c(".",">"))
if (getRversion() >= "2.15.1")
  utils::globalVariables(c("."))


#' Create the matrix of incidence.
#'
#' The function metaGE.incidence convert categorical variable describing the environments into a matrix of dummy variables with in rows the levels of the variables and in columns the environment.
#' @param VarName The name of the column containing the categorical variable in the Covariate dataset.
#' @param Covariate A dataset containing categorical variables (in columns) describing the environments (in rows).
#' @param EnvName The name of the column containing the names of the environment in the Covariate dataset.
#' @param Data A dataset containing the effects and pvalues of each marker (in rows) in each environment (in columns), as obtained from [metaGE.collect()].
#' @param AtLeast A numeric value indicating the minimum number of environments must belong to each level (equals \code{1} by default).
#' @return  A  binary matrix containing indicator variables with in rows the levels of the variables and in columns the environment.
#' @details The names of the environment must be the same as used in the Data dataset.
#' @export
#' @import dplyr stringr purrr
#' @examples
#' # Import the data
#' data("metaData")
#' data("envDesc")
#'
#' # Build the matrix of incidence
#' (Incidence.Temp <- metaGE.incidence(VarName = "Temp",Covariate = envDesc,
#'                                     EnvName = "ShortName", Data = metaData))


metaGE.incidence <-
  function(VarName,
           Covariate,
           EnvName,
           Data,
           AtLeast = 1) {
    ## Select the levels of interest
    Levels <- dplyr::select(Covariate, all_of(VarName)) %>%
      table %>%
      .[. >= AtLeast] %>%
      names %>%
      set_names(., .)

    if (length(Levels) <= 1) {
      stop(paste0(VarName, " variable should have at least two levels."))
    }

    ## Rename the environment column
    Covariate <- Covariate %>%  rename("FileName" = all_of(EnvName))
    Covariate <- Covariate %>% rename("Var"= all_of(VarName))

    ## Order environments as in the metaData dataset
    DT <- names(Data) %>%
      .[str_detect(., 'PVAL.')] %>%
      stringr::str_remove(pattern = 'PVAL.') %>%
      tibble(FileName = .) %>%
      left_join(., Covariate, by = "FileName") %>%
      select(.data$FileName,.data$Var)

    ## Build the different univariate contrasts
    Incidence <- purrr::map(Levels,  ~ {
      (DT %>% pull(.data$Var) == .x) %>% as.numeric
    })

    ## Convert into a matrix
    Incidence <- purrr::reduce(Incidence, rbind)
    rownames(Incidence) <- Levels
    colnames(Incidence) <- DT$FileName

    return(t(Incidence))
  }

#' Check and reformat the matrix of contrast
#'
#' The function CheckContrast check and reformat the matrix of  contrast.
#' @param Contrast A matrix of contrast.
#' @param ContrastName The name of the contrast.
#' @return  The matrix of contrast in the right format.


CheckContrast <- function(Contrast, ContrastName) {
  if (!is.null(Contrast)) {
    ## Check the class of the contrast, reshape into matrix
    if (!is.numeric(Contrast)) {
      stop(paste0("Contrast ", ContrastName, " should be numeric"))
    }
    # else if (is.vector(Contrast)) {
    #   Contrast <- t(Contrast)
    # }

    ## Check whether Contrast matrix can be simplified
    if (sum(abs(Contrast)) == 0) {
      stop(paste0(
        "Contrast",
        ContrastName,
        " has 0 values only. Please check it!"
      ))
    } else{
      if (qr(Contrast)$rank < nrow(Contrast)) {
        nb_non_independant_contrast <- nrow(Contrast) - qr(Contrast)$rank
        message(
          paste0(
            "Contrast ",
            ContrastName,
            " contained ",
            nb_non_independant_contrast,
            " colinear rows.
                         It has been resized."
          )
        )
        for (i in 1:nb_non_independant_contrast) {
          which_col <-
            lapply(1:nrow(Contrast), function(x) {
              qr(Contrast[-x, ])$rank
            }) %>%
            lapply(., function(x) {
              x == qr(Contrast)$rank
            }) %>%
            unlist %>% which(.)
          Contrast <- Contrast[-which_col[1], ]
        }
      }
    }
  }
  return(Contrast)
}

#' Check and reformat the matrix of incidence
#'
#' The function CheckIncidence check and reformat the matrix of incidence.
#' @param Incidence A matrix of incidence, as obtained from [metaGE.incidence()].
#' @param IncidenceName The name of the incidence.
#' @return  The matrix of incidence in the right format.


CheckIncidence <- function(Incidence, IncidenceName) {
  ## Check the class of the Incidence, reshape into matrix
  if (!is.numeric(Incidence)) {
    stop(paste0("Incidence ", IncidenceName, " should be numeric"))
  } else if (is.vector(Incidence)) {
    stop(paste0("Incidence ", IncidenceName, " should be a matrix"))
  }


  ## Check whether each environment is in one and only one group
  J <- ncol(Incidence)
  K <- nrow(Incidence)

  if (!identical(rowSums(Incidence) %>% unname, rep(1, K)) |
      !identical(Rfast::rowMaxs(Incidence, value = T), rep(1, K)) |
      !identical(Rfast::rowMins(Incidence, value = T), rep(0, K))) {
    stop(
      paste0(
        "Each environment should be in one and only one group in the incidence matrix ",
        IncidenceName,
        ". Please check it!"
      )
    )
  }
  if (sum(colSums(Incidence) == 0) != 0) {
    stop(
      paste0(
        "At least one environment should be in each group in the incidence matrix ",
        IncidenceName,
        " .Please check it!"
      )
    )
  }

  return(Incidence)
}


#' Compute the statistic of the contrast test.
#'
#' The function ContrastStatTest compute the statistic of the contrast test.
#' @param Incidence A matrix of incidence, as obtained from [metaGE.incidence()].
#' @param Contrast A matrix of contrast, if \code{NULL} the identity matrix is used. (\code{NULL} by default)
#' @param Zmat A matrix containing the Zscores of all markers (in rows) in each environment (in columns).
#' @param MatCorr The inter-environments correlation matrix. Can be computed using [metaGE.cor()].
#' @param IncidenceName The name of the incidence.
#' @return  A dataset of two columns containing the pvalue of the test of contrast and the minimum number of environment per group of all markers.
#' @importFrom stats setNames

ContrastStatTest <-
  function(Incidence,
           Contrast = NULL,
           Zmat,
           MatCorr,
           IncidenceName) {
    ##Check the shape of the matrix of incidence and contrast
    if (!is.null(Contrast)) {
      if (ncol(Incidence) != ncol(Contrast)) {
        stop(
          "The matrix of incidence and matrix of contrast should have the same number of columns."
        )
      }
    }

    if (is.null(Contrast)) {
      Contrast <- diag(rep(1, ncol(Incidence)))
    }

    ## Compute the contrast variance matrix
    D <- 1 / diag(crossprod(Incidence, Incidence))
    Temp <- Contrast %*% (D * t(Incidence))
    Vc <- tcrossprod(Temp %*% MatCorr, Temp)

    if (length(Vc) == 1) {
      InvSqrtVc <- 1 / sqrt(Vc)
    } else {
      InvSqrtVc <- Vc %>%
        as.matrix() %>%
        svd %>%
        {
          .$u %*% diag((1 / sqrt(.$d))) %*% t(.$u)
        }
    }

    ## Computing Chi2 test statistic
    TestStat <- Zmat %>%
      as.matrix %>%
      tcrossprod(., Temp) %>%
      tcrossprod(InvSqrtVc, .) %>%
      . ^ 2 %>%
      colSums

    ## Computing  p-values
    Pval <-
      pchisq(q = TestStat,
             df = nrow(Contrast),
             lower.tail = FALSE)
    NbMinEnvPerGroup <- rep(min(colSums(Incidence)), nrow(Zmat))
    DF <- rep(nrow(Contrast), nrow(Zmat))

    res <-
      stats::setNames(data.frame(Pval, NbMinEnvPerGroup,DF),
                      paste0(c('PVALUE.', 'NbMinEnvPerGroup.','DF.'), IncidenceName))
    return(res)
  }

#' Compute the statistic of the contrast test in presence of missing values
#'
#' The function ContrastStatTest compute the statistic of the contrast test.
#' @param Incidence A matrix of incidence, as obtained from metaGE.incidence.
#' @param Contrast A matrix of contrast, if NULL the identity matrix is used. (\code{NULL} by default)
#' @param Zmat A matrix containing the Zscores of all markers (in rows) in each environment (in columns).
#' @param MatCorr The inter-environments correlation matrix. Can be computed using [metaGE.cor()].
#' @param Data A dataset containing the effect, the pvalues and the na configuration for all marker
#' @param Configs.list A vector containing the \code{NA} configurations present in the dataset
#' @param IncidenceName The name of the incidence.
#' @return  A dataset of two columns containing the pvalue of the test of contrast and the minimum number of environment per group of all markers.
#' @importFrom stats setNames

ContrastStatTest.NA <-
  function(Incidence,
           Contrast = NULL,
           Zmat,
           MatCorr,
           Data,
           Configs.list,
           IncidenceName) {

    ##Check the shape of the matrix of incidence and contrast
    if (!is.null(Contrast)) {
      if (ncol(Incidence) != ncol(Contrast)) {
        stop(
          "The matrix of incidence and matrix of contrast should have the same number of columns."
        )
      }
    }

    if (is.null(Contrast)) {
      Contrast <- diag(rep(1, ncol(Incidence)))
    }

    ##Check if the contrast matrix is the identity matrix
    Contrast.id = FALSE
    if(identical(rowSums(abs(Contrast)),rep(1,nrow(Contrast)))){
      Contrast.id = TRUE
    }

    ##Initialization
    TestStat <- rep(NA, nrow(Zmat))
    NbMinEnvPerGroup <- rep(NA, nrow(Zmat))
    DF <- rep(nrow(Contrast), nrow(Zmat))

    map(Configs.list, ~ {
      ## Configuration NA info
      Marker.idx <- which(Data$NA.config == .x)
      Env.idx <- !is.na(Zmat[Marker.idx[1],]) %>% as.numeric
      K.config <- sum(Env.idx)
      M.config <- length(Marker.idx)

      ## Check if there are more than one env
      if (K.config == 1) {
        TestStat[Marker.idx] <<- NA
        NbMinEnvPerGroup[Marker.idx] <<- 1

      ## Check if some groups are empty
      } else if (min(colSums(Incidence[Env.idx, ])) <= 1 ) {
        ## Check if the contrast matrix is the identity matrix and there are more than one group
        if(!Contrast.id  | sum(colSums(Incidence[Env.idx, ]) > 1)<=1){
          TestStat[Marker.idx] <<- NA
          NbMinEnvPerGroup[Marker.idx] <<-
            min(colSums(Incidence[Env.idx, ]))
        }else{
          Grp.ind <- (colSums(Incidence[Env.idx, ]) > 1)

          ## Check the new contrast matrix
          Contrast.config <- CheckContrast(Contrast[Grp.ind,Grp.ind],IncidenceName)


          ## Compute the contrast variance matrix
          D <-
            1 / diag(crossprod(Incidence[Env.idx,Grp.ind], Incidence[Env.idx,Grp.ind]))
          Temp <- Contrast.config  %*% (D * t(Incidence[Env.idx,Grp.ind ]))
          Vc <- tcrossprod(Temp %*% MatCorr[Env.idx, Env.idx], Temp)

          if (length(Vc) == 1) {
            InvSqrtVc <- 1 / sqrt(Vc)
          } else{
            Vc.svd <- Vc %>%
              as.matrix() %>%
              svd
            if (sum(Vc.svd$d <= 0) != 0) {
              Vc.svd$d <- Vc.svd$d + 2 * min(Vc.svd$d)
            }
            InvSqrtVc <- Vc %>%
              as.matrix() %>%
              svd %>%
              {
                .$u %*% diag((1 / sqrt(.$d))) %*% t(.$u)
              }
          }

          ## Computing Chi2 test statistic
          TestStat[Marker.idx] <<- Zmat[Marker.idx, Env.idx] %>%
            as.matrix %>%
            tcrossprod(., Temp) %>%
            tcrossprod(InvSqrtVc, .) %>%
            . ^ 2 %>%
            colSums
          NbMinEnvPerGroup[Marker.idx] <<-
            min(colSums(Incidence[Env.idx, ]))
          DF[Marker.idx] <<- nrow(Contrast.config )
        }

      } else{
        ## Compute the contrast variance matrix
        D <-
          1 / diag(crossprod(Incidence[Env.idx, ], Incidence[Env.idx, ]))
        Temp <- Contrast %*% (D * t(Incidence[Env.idx, ]))
        Vc <- tcrossprod(Temp %*% MatCorr[Env.idx, Env.idx], Temp)

        if (length(Vc) == 1) {
          InvSqrtVc <- 1 / sqrt(Vc)
        } else{
          Vc.svd <- Vc %>%
            as.matrix() %>%
            svd
          if (sum(Vc.svd$d <= 0) != 0) {
            Vc.svd$d <- Vc.svd$d + 2 * min(Vc.svd$d)
          }
          InvSqrtVc <- Vc %>%
            as.matrix() %>%
            svd %>%
            {
              .$u %*% diag((1 / sqrt(.$d))) %*% t(.$u)
            }
        }

        ## Computing Chi2 test statistic
        TestStat[Marker.idx] <<- Zmat[Marker.idx, Env.idx] %>%
          as.matrix %>%
          tcrossprod(., Temp) %>%
          tcrossprod(InvSqrtVc, .) %>%
          . ^ 2 %>%
          colSums
        NbMinEnvPerGroup[Marker.idx] <<-
          min(colSums(Incidence[Env.idx, ]))
      }
    })

    ## Computing  p-values
    Pval <-
      pchisq(q = TestStat,
             df = DF,
             lower.tail = FALSE)

    res <-
      stats::setNames(data.frame(Pval, NbMinEnvPerGroup,DF),
                      paste0(c('PVALUE.', 'NbMinEnvPerGroup.','DF.'), IncidenceName))
    return(res)
  }


#' Compute the pvalue of the meta-regression test.
#'
#' The function RegressionStatTest compute the statistic and the pvalue of the regression test.
#' @param Covariate A dataset containing the values of one Covariate (in columns) in each environment (in rows).
#' @param CovName The name the Covariate.
#' @param Zmat A matrix containing the Zscores of all markers (in rows) in each environment (in columns).
#' @param MatCorr The inter-environments correlation matrix. Can be computed using [metaGE.cor()].
#' @return A dataset of two columns containing the pvalue of the meta-regression test and the number of environment used to perform the test of all markers.
#' @importFrom stats pnorm

RegressionStatTest <- function(Covariate, CovName, Zmat, MatCorr) {
  #Center the Covariate
  Covariate <- Covariate - mean(Covariate,na.rm=TRUE)

  #Check if there are NA values
  if(sum(is.na(Covariate))!=0){
    Zmat <- Zmat[,!is.na(Covariate)]
    MatCorr <- MatCorr[!is.na(Covariate),!is.na(Covariate)]
    Covariate <- Covariate[!is.na(Covariate)]
  }

  #Diagonalization of the correlation matrix
  EigenCor.list <- eigen(MatCorr)
  Delta <- EigenCor.list$values
  P <- EigenCor.list$vectors

  #Computing test statistic
  M <- nrow(Zmat)
  K <- ncol(Zmat)
  SdT <-
    rowSums(Zmat * matrix(rep(Covariate, M), M, K, byrow = TRUE)) / sqrt(sum(crossprod(Covariate, P) *
                                                                                Delta * t(crossprod(P,Covariate))))

  #Computing p-values
  Pvalue <- 2 * stats::pnorm(-abs(SdT))
  NbEnv <- rep(K, M)
  res <-
    stats::setNames(data.frame(Pvalue, NbEnv), paste0(c('PVALUE.', 'NbEnv.'), CovName))
  return(res)
}

#' Compute the pvalue of the regression test in presence of missing values.
#'
#' The function RegressionStatTest compute the statistic and the pvalue of the regression test.
#' @param Covariate A dataset containing the values of one covariate (in columns) in each environment (in rows).
#' @param CovName The name the covariate.
#' @param Zmat A matrix containing the Zscores of all markers (in rows) in each environment (in columns).
#' @param MatCorr The inter-environments correlation matrix. Can be computed using [metaGE.cor()].
#' @param Data A dataset containing the effect, the pvalues and the \code{NA} configuration for all marker
#' @param Configs.list A vector containing the NA configurations present in the dataset
#' @return A dataset of two columns containing the pvalue of the meta-regression test and the number of environment used to perform the test of all markers.
#' @importFrom stats pnorm
RegressionStatTestNA <-
  function(Covariate,
           CovName,
           Zmat,
           MatCorr,
           Data,
           Configs.list) {

    #Check if there are NA values
    if(sum(is.na(Covariate))!=0){
      Zmat <- Zmat[,!is.na(Covariate)]
      MatCorr <- MatCorr[!is.na(Covariate),!is.na(Covariate)]
      Covariate <- Covariate[!is.na(Covariate)]
    }

    #Initialisation
    TestStat <- rep(NA, nrow(Zmat))

    #Loop on the NA configs
    map(Configs.list, ~ {
      ## Configuration NA info
      Marker.idx <- which(Data$NA.config == .x)
      Env.idx <- !is.na(Zmat[Marker.idx[1],]) %>% as.numeric
      K.config <- sum(Env.idx)
      M.config <- length(Marker.idx)

      ## Check if there are more than one env
      if (K.config <= 1 | is.null(K.config)) {
        TestStat[Marker.idx] <<- NA
      } else{
        # Center covariate
        Covariate.config <-
          Covariate[Env.idx] - mean(Covariate[Env.idx])

        # Diagonalization of the correlation matrix
        EigenCor.list <- eigen(MatCorr[Env.idx, Env.idx])
        Delta <- EigenCor.list$values
        P <- EigenCor.list$vectors

        # Check if the eigen values are positives
        if (sum(Delta <= 0) != 0) {
          Delta <- Delta + 2 * min(Delta)
        }

        # Compute test statistic
        TestStat[Marker.idx] <<-
          rowSums(Zmat[Marker.idx, Env.idx] * matrix(
            rep(Covariate.config, M.config),
            M.config,
            K.config,
            byrow = TRUE
          )) / sqrt(sum(crossprod(Covariate.config, P) * Delta * t(crossprod(
            P, Covariate.config
          ))))
      }
    })

    #Computing p-values
    Pvalue <- 2 * stats::pnorm(-abs(TestStat))
    NbEnv <- rowSums(!is.na(Zmat))
    res <-
      stats::setNames(data.frame(Pvalue, NbEnv), paste0(c('PVALUE.', 'NbEnv.'), CovName))
    return(res)
  }


#' Meta-analysis test for Genotype x Environment interactions: Contrast or Regression.
#'
#' The function metaGE.test compute meta-analysis contrast or regression test.
#' @param Data A dataset containing the estimated marker effect and its associated pvalue of each marker (in rows) in each environment (in columns), as obtained from [metaGE.collect()].
#' @param MatCorr The inter-environment correlation matrix. It can be compute by the [metaGE.cor()] function.
#' @param Incidence A matrix of incidence, as obtained from [metaGE.incidence()] or a list of such matrix.
#' @param Contrast A matrix of contrast, or a list of such matrix.
#' @param Covariate  A dataset containing the values of one or more covariates (in columns) in each environment (in rows).
#' @param EnvName The name of the column containing the names of the environment in the \code{Covariate} dataset.
#' @param NA.omit A boolean specifying whether the markers with some \code{NA} values should be removed from the test procedure. (\code{TRUE} by default)
#' @param DropZScores A boolean specifying whether the Zscores should be dropped from the dataset or not. (\code{FALSE} by default)
#' @details If \code{Incidence} is provided, the function will perform all the corresponding tests of contrast. If \code{Covariate} is provided, the function will perform all the corresponding meta-regression tests.
#' The \code{Contrast} can be \code{NULL}, in this case the identity matrix is used.
#' @return The dataset \code{Data} with supplementary columns containing the PVALUE of each test performed.
#' @import dplyr stringr
#' @importFrom stats pnorm qnorm
#' @importFrom data.table setnames
#' @export
#' @examples
#' require(dplyr)
#'
#'  # Import the data
#' data("metaData")
#' data("envDesc")
#'
#' # Compute the inter-environment correlation matrix
#' matCorr <- metaGE.cor(metaData, Threshold = 0.8)
#'
#' #### Contrast test
#' # Build the matrix of incidence
#' Incidence.Water <- metaGE.incidence(VarName = "Water",Covariate = envDesc,
#'                                     EnvName = "ShortName", Data = metaData)
#'
#' # Perform the contrast test
#' ContrastDF <- metaGE.test(metaData, matCorr,Incidence = Incidence.Water, Contrast = NULL)
#' head(ContrastDF %>% select(CHR, POS, MARKER, PVALUE.Contrast1))
#'
#' #### Regression test
#' RegressionDF <- metaGE.test(metaData,matCorr, Covariate = envDesc[,c(1,5)],EnvName = "ShortName" )
#' head(RegressionDF %>% select(CHR, POS, MARKER, PVALUE.Tnight.mean))


metaGE.test <-
  function(Data,
           MatCorr,
           Incidence = NULL,
           Contrast = NULL,
           Covariate = NULL,
           EnvName=NULL,
           NA.omit = TRUE,
           DropZScores = FALSE) {
    ## Get SNPs with no missing values
    if (NA.omit) {
      NonMissing.idx <- Data %>%
        dplyr::select(contains('PVAL.')) %>%
        purrr::map(~ is.na(.x) %>% which) %>%
        reduce(union) %>%
        setdiff(1:nrow(Data), .)
      if (length(NonMissing.idx) < nrow(Data)) {
        message(paste0(
          nrow(Data) - length(NonMissing.idx),
          ' SNPs with NAs removed from the analysis'
        ))
        Data <- Data[NonMissing.idx, ]
      }
    }


    ## Compute the Z score per environment
    qnorm_function <- function(x) {
      -stats::qnorm(x / 2)
    }
    SignDF <- Data %>%
      select(contains('EFFECT.')) %>%
      map_df(sign)
    TransPvalDF <- Data %>%
      select(contains('PVAL.')) %>%
      map_df(qnorm_function)
    Zmat <- SignDF * TransPvalDF
    names(Zmat) <- names(SignDF) %>%
      stringr::str_replace(pattern = "EFFECT", replacement = "Z")
    if (!DropZScores) {
      Data <- bind_cols(Data, Zmat)
    }
    if (sum(is.na(Zmat)) == 0) {
      NA.omit <- TRUE
    }


    ## Compute the NA configurations
    if (!NA.omit) {
      Data$NA.config <- Zmat %>%
        is.na %>%
        {
          . + 0
        } %>%
        apply(., 1, paste, collapse = "")

      Configs.list <- table(Data$NA.config) %>%
        {
          .[order(., decreasing = T)]
        } %>% names
    }

    ## General checking
    if (is.null(Incidence) & is.null(Covariate)) {
      stop("Incidence and Covariate are NULL.")
    }

    ## Test of Contrast
    if (!is.null(Incidence)) {
      ### Checks on Incidence
      if (!is.matrix(Incidence) &
          !is.numeric(Incidence) & !is.list(Incidence)) {
        stop('Incidence should be a numeric vector, a matrix, or a list of such objects')
      }
      if (!is.list(Incidence)) {
        Incidence <- list(Incidence)
      }
      ### Assign names to incidence (if none provided)
      if (is.null(names(Incidence))) {
        names(Incidence) <- paste0('Contrast', 1:length(Incidence))
      }

      ### Checks on Contrast
      if (!is.null(Contrast) &
          !is.matrix(Contrast) & !is.numeric(Contrast) & !is.list(Contrast)) {
        stop('Contrast should be null, a numeric vector, a matrix, or a list of such objects')
      }
      if (!is.list(Contrast) & !is.null(Contrast)) {
        Contrast <- list(Contrast)
      }

      if ((!is.null(Contrast)) &
          (length(Contrast) != length(Incidence))) {
        stop('Each incidence must be associated with a contrast.')
      }

      ### Assign names to contrasts (if none provided)
      if (!is.null(Contrast) & is.null(names(Contrast))) {
        names(Contrast) <- paste0('Contrast', 1:length(Incidence))
      }

      ### Check each incidence and contrast separately
      Incidence <- purrr::map(Incidence, CheckIncidence)
      if (!is.null(Contrast)) {
        Contrast <- purrr::map2(Contrast, names(Contrast), CheckContrast)
      }

      ### Compute p-values

      if (!is.null(Contrast)) {
        if (NA.omit) {
          Pvalmat <-
            purrr::map_dfc(
              1:length(Incidence),
              ~ ContrastStatTest(
                Incidence = Incidence[[.x]],
                Contrast = Contrast[[.x]],
                Zmat = Zmat,
                MatCorr = MatCorr,
                IncidenceName = names(Incidence)[.x]
              )
            )
        } else{

          Pvalmat <-
            purrr::map_dfc(
              1:length(Incidence),
              ~ ContrastStatTest.NA(
                Incidence = Incidence[[.x]],
                Contrast = Contrast[[.x]],
                Zmat = Zmat,
                MatCorr = MatCorr,
                Data = Data,
                Configs.list = Configs.list,
                IncidenceName = names(Incidence)[.x]
              )
            )
        }

      }
      else{
        if (NA.omit) {
          Pvalmat <-
            purrr::map_dfc(
              1:length(Incidence),
              ~ ContrastStatTest(
                Incidence = Incidence[[.x]],
                Contrast = NULL,
                Zmat = Zmat,
                MatCorr = MatCorr,
                IncidenceName = names(Incidence)[.x]
              )
            )
        } else{
          Pvalmat <-
            purrr::map_dfc(
              1:length(Incidence),
              ~ ContrastStatTest.NA(
                Incidence = Incidence[[.x]],
                Contrast = NULL,
                Zmat = Zmat,
                MatCorr = MatCorr,
                Data = Data,
                Configs.list = Configs.list,
                IncidenceName = names(Incidence)[.x]
              )
            )
        }
      }
      ### merge with the initial dataset
      Data <- Data %>%
        bind_cols(., Pvalmat)
    }

    ## Regression
    if (!is.null(Covariate)) {
      ### Match the environment order
      Covariate <- Covariate %>%  rename("Experiment" = all_of(EnvName))
      EnvData <-
        data.frame(Experiment = str_remove(names(Zmat), pattern = 'Z.')) %>% left_join(Covariate, by =
                                                                                         'Experiment')

      ### Check no missing values

      ### Compute the pvalues for all Covariates
      if (NA.omit) {
        PvalRegDF <-
          map_dfc(which(names(EnvData)!="Experiment"),
                  ~ RegressionStatTest(EnvData[, .x], names(EnvData)[.x], Zmat, MatCorr))
      } else{
        PvalRegDF <-
          map_dfc(
            which(names(EnvData)!="Experiment"),
            ~ RegressionStatTestNA(
              EnvData[, .x],
              names(EnvData)[.x],
              Zmat,
              MatCorr,
              Data,
              Configs.list
            )
          )
      }

      ### Merge with the initial dataset
      Data <- Data %>%
        bind_cols(., PvalRegDF)
    }

    return(Data)
  }
