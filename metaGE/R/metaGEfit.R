#' @import utils
## quiets concerns of R CMD check re: the .'s that appear in pipelines
#if(getRversion() >= "2.15.1")  utils::globalVariables(c(".",">"))
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))



#' LLikelihoodT_vect
#'
#' This function compute the values of loglikelihood for all markers.
#' @param Zmat A matrix containing the Zscores of all markers (in rows) in each environment (in columns)
#' @param Delta A vector containing the diagonal coefficients of the diagonal matrix obtained by the diagonalization of the correlation matrix
#' @param P Matrix such that MatCorr = P Delta t(P), with Delta diagonal
#' @param Mu A vector containing the average effect of the markers
#' @param Tau A vector containing the heterogeneity between environments of the markers
#' @return  A vector containing the value of the Log-Likelihood of all markers
#'

LLikelihoodT_vect <- function(Zmat, Delta, P,Mu, Tau){
  K <- ncol(Zmat)
  M <- nrow(Zmat)
  res<- -0.5 * ( K*log(2*pi) + K*log(1+Tau) + sum(log(Delta)) + (t(rowSums(crossprod(t(Zmat)-matrix(rep(Mu,K),K,M, byrow = T),P) * matrix(rep(1/Delta,M),M,K, byrow=T) * t(crossprod(P,t(Zmat)-matrix(rep(Mu,K),K,M, byrow = T))))))/(1+Tau) )
  return(res %>% as.numeric())
}

#' Meta-analysis procedure: Fixed or Random effect.
#'
#' Quantitative trait loci detection via Fixed or Random effect meta-analysis GWAS procedure.
#' @param Data A dataset containing the estimated marker effect and its associated pvalue of each marker (in rows) in each environment (in columns), as obtained from [metaGE.collect()].
#' @param MatCorr The inter-environments correlation matrix. Can be computed using [metaGE.cor()].
#' @param Method A string specifying the method to be performed: either "\code{Fe}" or "\code{Re}".
#' @param NA.omit A boolean specifying whether the markers with some \code{NA} values should be removed. (\code{TRUE} by default)
#' @param DropZScores A boolean specifying whether the Zscores should be dropped from the dataset or not.(\code{FALSE} by default)
#' @return  The dataset Data with supplementary columns:
#' * PVALUE: The pvalue of the MA test,
#' * Mu: Estimate of the mean marker effect,
#' * Tau: Estimate of the variance of the marker effect, for the Random model only,
#' * the Zscores for each environment if \code{DropZScores = FALSE}.
#' @details Different tests may be performed:
#' * Fixed Effect (Fe), to identify markers with a stable effect across environments.
#' * Random Effect (Re), to identify markers whose effects may be unstable across environments.
#' @export
#' @import dplyr stringr
#' @importFrom stats qnorm pnorm pchisq
#' @importFrom emdbook pchibarsq
#' @examples
#' require(dplyr)
#' # Import the data
#' data("metaData")
#'
#' # Compute the inter-environment correlation matrix
#' matCorr <- metaGE.cor(metaData, Threshold = 0.8)
#'
#' # Fixed Effect
#' FeDF <- metaGE.fit(metaData, matCorr, Method = "Fe")
#' head(FeDF %>% select(CHR, POS, MARKER, Mu, Tau, PVALUE))
#'
#' # Random Effect
#' ReDF <- metaGE.fit(metaData, matCorr, Method = "Re")
#' head(ReDF %>% select(CHR, POS, MARKER, Mu, Tau, PVALUE))

metaGE.fit <-
  function(Data,
           MatCorr,
           Method,
           NA.omit = TRUE,
           DropZScores = FALSE) {

    ## Checkings
    if (Method != "Fe" & Method != "Re") {
      stop("Method must be Fe or Re.")
    }

    ## Get SNPs with no missing values
    if (NA.omit) {
      NonMissing.idx <- Data %>%
        dplyr::select(contains('PVAL.')) %>%
        purrr::map( ~ is.na(.x) %>% which) %>%
        reduce(union) %>%
        setdiff(1:nrow(Data), .)
      if (length(NonMissing.idx) < nrow(Data)) {
        message(paste0(
          nrow(Data) - length(NonMissing.idx),
          ' SNPs with NAs removed from the analysis'
        ))
        Data <- Data[NonMissing.idx,]
      }
    }

    if (sum(is.na(Data)) == 0) {
      NA.omit  <- TRUE
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

    ## Compute the NA configurations
    if(!NA.omit){
      Data$NA.config <- Zmat %>%
        is.na %>%
        {. + 0} %>%
        apply(., 1, paste, collapse = "")

      Configs.list <- table(Data$NA.config) %>%
        {.[order(., decreasing = T)]} %>% names

    }

    #### Perform Fixed effect test
    if (Method == "Fe") {
      ## Mu inference
      Data$Mu <- rowMeans(Zmat, na.rm = TRUE)
      ## Put the Tau to 1
      Data$Tau <- rep(1, nrow(Data))

      ## Test statistic computation
      if(NA.omit){

        SdT <- rowSums(Zmat) / sqrt(sum(MatCorr))

      }else{

        SdT <- rep(NA, nrow(Zmat))

        map(Configs.list,~{
          ## Configuration NA info
          Marker.idx <- which(Data$NA.config == .x)
          Env.idx <- !is.na(Zmat[Marker.idx[1], ]) %>% as.numeric

          if(sum(Env.idx)==1){
            SdT[Marker.idx] <<- NA
          }else{
            SdT[Marker.idx] <<- rowSums(Zmat[Marker.idx,Env.idx]) / sqrt(sum(MatCorr[Env.idx,Env.idx]))
          }
        })
      }

      ## Compute the pvalue
      Data$PVALUE <- 2 * pnorm(-abs(SdT))


      #### Perform Random effect test
    } else if (Method == "Re") {

      if(NA.omit){
        K <- ncol(Zmat)
        M <- nrow(Zmat)

        ## Diagonalization of the correlation matrix
        EigenCor.list <- eigen(MatCorr)
        Delta <- EigenCor.list$values
        P <- EigenCor.list$vectors

        ## Estimation of parameters mu and tau
        Mu_hat <-  colSums(colSums(P) * (1/Delta) * crossprod(P,t(Zmat))) / (sum(colSums(P) * (1/Delta) *rowSums(t(P))))
        Tau_hat <- (t(rowSums(crossprod(t(Zmat)-matrix(rep(Mu_hat,K),K,M, byrow = T),P) * matrix(rep(1/Delta,M),M,K, byrow=T) * t(crossprod(P,t(Zmat)-matrix(rep(Mu_hat,K),K,M, byrow = T)))))/K )- 1
        Tau_hat <- Tau_hat  %>% map_dbl(~max(.x,0) )

        ## Computation of loglikekihood values
        Data <- Data %>% dplyr::mutate(Mu = Mu_hat,
                                       Tau = Tau_hat + 1,
                                       LogLik = LLikelihoodT_vect(Zmat, Delta, P, Mu_hat, Tau_hat),
                                       LogLik0 = LLikelihoodT_vect(Zmat, Delta, P, rep(0,M),rep(0,M)),
                                       LogLikm = LLikelihoodT_vect(Zmat, Delta, P, Mu_hat, rep(0,M)),
                                       LogLikt = LLikelihoodT_vect(Zmat, Delta, P, rep(0,M), Tau_hat),
                                       LogLikMu = LLikelihoodT_vect(Zmat, Delta, P, Mu_hat,rep(0,M)))
      }else {
        ## Initialization
        M <- nrow(Zmat)
        Mu_hat <- rep(NA, M)
        Tau_hat <- rep(NA, M)
        LogLik <- rep(NA, M)
        LogLik0 <- rep(NA, M)
        LogLikm <- rep(NA, M)
        LogLikt <- rep(NA, M)

        map(Configs.list,  ~ {
          ## Configuration NA info
          Marker.idx <- which(Data$NA.config == .x)
          Env.idx <- !is.na(Zmat[Marker.idx[1], ]) %>% as.numeric
          K.config <- sum(Env.idx)
          M.config <- length(Marker.idx)

          ## Check if there are more than one env
          if(K.config==1){
            Mu_hat[Marker.idx] <<- NA
            Tau_hat[Marker.idx] <<- NA
            LogLik[Marker.idx] <<- NA
            LogLik0[Marker.idx] <<- NA
            LogLikm[Marker.idx] <<- NA
            LogLikt[Marker.idx] <<- NA

          }else {
            ## Diagonalization of the correlation matrix
            EigenCor.list <- eigen(MatCorr[Env.idx, Env.idx])
            Delta <- EigenCor.list$values
            P <- EigenCor.list$vectors

            ## Check if the eigen values are positives
            if(sum(Delta<=0)!=0){
              Delta <- Delta + 2*abs(min(Delta))
            }

            ## Estimation of parameters mu and tau
            Mu_hat[Marker.idx] <<-
              colSums(colSums(P) * (1 / Delta) * crossprod(P, t(Zmat[Marker.idx, Env.idx]))) / (sum(colSums(P) * (1 /
                                                                                                                    Delta) * rowSums(t(P))))
            Tau_temp <-
              (t(rowSums(
                crossprod(t(Zmat[Marker.idx, Env.idx]) - matrix(
                  rep(Mu_hat[Marker.idx], K.config), K.config, M.config, byrow = T
                ), P) * matrix(rep(1 / Delta, M.config), M.config, K.config, byrow = T) * t(crossprod(
                  P, t(Zmat[Marker.idx, Env.idx]) - matrix(rep(Mu_hat[Marker.idx], K.config), K.config, M.config, byrow = T)
                ))
              )) / K.config) - 1
            Tau_hat[Marker.idx] <<- Tau_temp  %>% map_dbl( ~ max(.x, 0))


            ## Computation of loglikekihood values
            LogLik[Marker.idx] <<-LLikelihoodT_vect(Zmat[Marker.idx, Env.idx], Delta, P, Mu_hat[Marker.idx], Tau_hat[Marker.idx])
            LogLik0[Marker.idx] <<- LLikelihoodT_vect(Zmat[Marker.idx, Env.idx], Delta, P, rep(0, M.config), rep(0, M.config))
            LogLikm[Marker.idx] <<- LLikelihoodT_vect(Zmat[Marker.idx, Env.idx], Delta, P, Mu_hat[Marker.idx], rep(0, M.config))
            LogLikt[Marker.idx] <<- LLikelihoodT_vect(Zmat[Marker.idx, Env.idx], Delta, P, rep(0, M.config), Tau_hat[Marker.idx])

          }


        })

        Data <- Data %>% dplyr::mutate(
          Mu = Mu_hat,
          Tau = Tau_hat + 1,
          LogLik = LogLik,
          LogLik0 = LogLik0,
          LogLikm = LogLikm,
          LogLikt = LogLikt)

      }

      ## Computation of p-values
      Data <- Data %>% dplyr::mutate(
        SdT = 2 * (.data$LogLik - .data$LogLik0),
        SdTm = 2 * (.data$LogLikm - .data$LogLik0),
        SdTt = 2 * (.data$LogLik - .data$LogLikt),
        SdTtau = 2 * (.data$LogLik - .data$LogLikm),
        # PVALUE = map_dbl(.data$SdT, ~ TcGSA::pchisqmix(.x, 1, 1, lower.tail = F)),
        PVALUE= map_dbl(.data$SdT, ~ emdbook::pchibarsq(.x, 2, 0.5, lower.tail = F)),
        PVALUEm = pchisq(.data$SdTm, 1, lower.tail = F),
        PVALUEt = pchisq(.data$SdTt, 1, lower.tail = F),
        PVALUEtau = pchisq(.data$SdTtau, 1, lower.tail = F)
      )

      ## Delete not required values
      Data <-
        Data %>% select(
          -contains("LogLik"),
          -contains("SdT"),
          -.data$PVALUEm,
          -.data$PVALUEt,
          -.data$PVALUEtau
        )

    }

    Data$NbEnv <- rowSums(!is.na(Zmat))
    if(!NA.omit){
      Data <- Data %>% select(-.data$NA.config)
    }

    return(Data)
  }

