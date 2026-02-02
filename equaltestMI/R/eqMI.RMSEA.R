#' Obtain the RMSEA cutoff values for equivalence testing
#'
#' Generate adjusted cutoff values of RMSEA for equivalence testing corresponding to conventional cutoff values .01, .05, .08, and .10.
#'
#' @param N Total sample size of all groups
#' @param m Number of groups
#' @param df Degree of freedom
#' @return The adjusted cutoff values corresponding to conventional cutoff values .01, .05, .08, and .10.
#' @details The adjusted cutoff values of RMSEA for equivalence testing can be obtained with \code{N, m, df} and transformed variables. Formulas are estimated using simulation studies and the coefficients are given in Table 11 of the reference.
#' @references Steiger, J. H. (1980). Statistically based tests for the number of common factors. In the annual meeting of the Psychometric Society. Iowa City, IA.
#' @references Yuan, K. H., & Chan, W. (2016). Measurement invariance via multigroup SEM: Issues and solutions with chi-square-difference tests. Psychological methods, 21(3), 405-426.
#' @export
#' @examples
#' alpha <- .05;
#' N <- 200;
#' m <- 1;
#' T_ml <- 28.446; #the statistic T_ml for group 1;
#' df <- 24;
#' eqMI.RMSEA(N = N, m = m, df = df);
#'
eqMI.RMSEA <- function(N, m, df){
  if(!is.null(df) & df<0){df=1}
  RMSEA_cvec <- c(.01,.05,.08,.10);
  RMSEA_avec <- matrix(0,nrow = 1,ncol = 4);
  for (j in 1:4){
    RMSEA_c <- RMSEA_cvec[j];
    if (RMSEA_c==.01){
      if (m==1){
        n <- N-m;
        y_e <- 1.34863-.51999*log(df)+.01925*log(df)*log(df)-.59811*log(n)+.00902*sqrt(n)+.01796*log(df)*log(n);
      }else if (m==2){
        n <- N-m;
        y_e <- 1.51409-.48929*log(df)+.01781*log(df)*log(df)-.53903*log(n)+.00005602*n+.01188*log(df)*log(n);
      }else if (m==3){
        n <- N-m;
        y_e <- 1.66291-.46885*log(df)+.01699*log(df)*log(df)-.52870*log(n)+.00004074*n+.00846*log(df)*log(n);
      }else if (m==4){
        n <- N-m;
        y_e <- 1.77502-.45735*log(df)+.01652*log(df)*log(df)-.52262*log(n)+.00003201*n+.00656*log(df)*log(n);
      }else if (m==5){
        n <- N-m;
        y_e <- 1.86574-.44997*log(df)+.01622*log(df)*log(df)-.51862*log(n)+.00002636*n+.00534*log(df)*log(n);
      }
    }
    if (RMSEA_c==.05){
      if (m==1){
        n <- N-m;
        y_e <- 2.06034-.62974*log(df)+.02512*log(df)*log(df)-.98388*log(n)+.05442*log(n)*log(n)-.00005188*n+.05260*log(df)*log(n);
      }else if (m==2){
        n <- N-m;
        y_e <- 2.61777-.67451*log(df)+.02596*log(df)*log(df)-.99310*log(n)+.04739*log(n)*log(n)+.05331*log(df)*log(n);
      }else if (m==3){
        n <- N-m;
        y_e <- 2.98724-.67419*log(df)+.02566*log(df)*log(df)-1.03326*log(n)+.04855*log(n)*log(n)+.05022*log(df)*log(n);
      }else if (m==4){
        n <- N-m;
        y_e <- 1.98287-.66535*log(df)+.02518*log(df)*log(df)-.95007*log(n)+.71711*(n**(1/5))+.04695*log(df)*log(n);
      }else if (m==5){
        n <- N-m;
        y_e <- 2.12615-.65419*log(df)+.02468*log(df)*log(df)-.94634*log(n)+.69787*(n**(1/5))+.04390*log(df)*log(n);
      }
    }
    if (RMSEA_c==.08){
      if (m==1){
        n <- N-m;
        y_e <- 2.84129-.54809*log(df)+.02296*log(df)*log(df)-.76005*log(n)+.10229*log(n)*log(n)-1.11167*(n**(1/5))+.04845*log(df)*log(n);
      }else if (m==2){
        n <- N-m;
        y_e <- 3.73383-.61834*log(df)+.02487*log(df)*log(df)-.93229*log(n)+.11073*log(n)*log(n)-1.09715*(n**(1/5))+.05296*log(df)*log(n);
      }else if (m==3){
        n <- N-m;
        y_e <- 2.91386-.65125*log(df)+.02563*log(df)*log(df)-1.09019*log(n)+.06789*log(n)*log(n)-.00948*sqrt(n)+.05430*log(df)*log(n);
      }else if (m==4){
        n <- N-m;
        y_e <- 3.04581-.66589*log(df)+.02588*log(df)*log(df)-1.05019*log(n)+.05572*log(n)*log(n)-.00003864*n+.05406*log(df)*log(n);
      }else if (m==5){
        n <- N-m;
        y_e <- 3.21990-.67182*log(df)+.02591*log(df)*log(df)-1.05668*log(n)+.05430*log(n)*log(n)-.00002718*n+.05314*log(df)*log(n);
      }
    }
    if (RMSEA_c==.10){
      if (m==1){
        n <- N-m;
        y_e <- 2.36352-.49440*log(df)+.02131*log(df)*log(df)-.64445*log(n)+.09043*log(n)*log(n)-1.01634*(n**(1/5))+.04422*log(df)*log(n);
      }else if (m==2){
        n <- N-m;
        y_e <- 3.34834-.56888*log(df)+.02354*log(df)*log(df)-.81593*log(n)+.10506*log(n)*log(n)-1.10591*(n**(1/5))+.04962*log(df)*log(n);
      }else if (m==3){
        n <- N-m;
        y_e <- 3.90933-.61431*log(df)+.02476*log(df)*log(df)-.92236*log(n)+.11054*log(n)*log(n)-1.10284*(n**(1/5))+.05273*log(df)*log(n);
      }else if (m==4){
        n <- N-m;
        y_e <- 2.96570-.63962*log(df)+.02537*log(df)*log(df)-1.07005*log(n)+.06814*log(n)*log(n)-.01029*sqrt(n)+.05396*log(df)*log(n);
      }else if (m==5){
        n <- N-m;
        y_e <- 3.18941-.65440*log(df)+.02568*log(df)*log(df)-1.09231*log(n)+.06714*log(n)*log(n)-.00894*sqrt(n)+.05424*log(df)*log(n);
      }
    }
    RMSEA_avec[j] <- exp(y_e);
  }

  result <- as.numeric(c(m, N, df, '', RMSEA_avec))
  names(result) <- c("m","N","df","cutoffs(", " .01  "," .05  "," .08  "," .10  )");
  return(result);
  #print(result, row.names = FALSE)
}
