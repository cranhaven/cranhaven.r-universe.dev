#' Combined Interaction Test
#'
#' This function reports the p-values of the tests for non-additivity developed by Boik (1993), Piepho (1994),
#' Kharrati-Kopaei and Sadooghi-Alvandi (2007), Franck et al. (2013), Malik et al. (2016)
#' and Kharrati-Kopaei and Miller (2016). In addition, it combines the p-values of these six tests (and some other available p-values) into a single p-value as a test statistic for testing interaction.
#' There are four combination methods:
#' Bonferroni, Sidak, Jacobi expansion, and Gaussian Copula. The results of these four combined tests are also reported. If there is a significant interaction, the type of interaction is also provided.
#'
#' @param x numeric matrix, \eqn{a \times b} data matrix where the number of row and column is corresponding to the number of factor levels.
#' @param nsim a numeric value, the number of Monte Carlo samples for computing an exact Monte Carlo p-value. The default value is 10000.
#' @param nc0 a numeric value, the number of Monte Carlo samples for computing the unbiasing constant \eqn{c_0} in \code{KKM.test}. The default value is 10000.
#' @param opvalue a numeric vector, other p-values (in addition to the six considered p-values) that are going to be combined.
#' @param alpha a numeric value, the level of the test. The default value is 0.05.
#' @param report logical: if \code{TRUE} the result of the test is reported at the \code{alpha} level.
#' @param Elapsed_time logical: if \code{TRUE} the progress will be printed in the console.
#'
#' @details The data matrix is divided based on the row of the data matrix for \code{KKSA_test} and \code{Franck_test}. Note that \code{KKSA_test} is not applicable when \eqn{a} is less than four. \code{Franck_test} and \code{Piepho_test} are not applicable when \eqn{a} is less than three. This function needs \code{mvtnorm} package.
#'
#' @return An object of the class \code{combtest}, which is a list inducing following components:
#' \item{nsim}{The number of Monte Carlo samples that are used to estimate p-value.}
#' \item{Piepho_pvalue}{The p-value of Piepho's (1994) test.}
#' \item{Piepho_Stat}{The value of Piepho's (1994) test statistic.}
#' \item{Boik_pvalue}{The p-value of Boik's (1993) test.}
#' \item{Boik_Stat}{The value of Boik's (1993) test statistic.}
#' \item{Malik_pvalue}{The p-value of Malik's (2016) et al. test.}
#' \item{Malik_Stat}{The value of Malik's (2016) et al. test statistic.}
#' \item{KKM_pvalue}{The p-value of Kharrati-Kopaei and Miller's (2016) test.}
#' \item{KKM_Stat}{The value of Kharrati-Kopaei and Miller's (2016) test statistic.}
#' \item{KKSA_pvalue}{The p-value of Kharrati-Kopaei and Sadooghi-Alvandi's (2007) test.}
#' \item{KKSA_Stat}{The value of Kharrati-Kopaei and Sadooghi-Alvandi's (2007) test statistic.}
#' \item{Franck_pvalue}{The p-value of Franck's (2013) et al. test.}
#' \item{Franck_Stat}{The value of Franck's (2013) et al. test statistic.}
#' \item{Bonferroni}{The combined p-value by using the Bonferroni method.}
#' \item{Sidak}{The combined p-value by using the Sidak method.}
#' \item{Jacobi}{The combined p-value by using the Jacobi method.}
#' \item{GC}{The combined p-value by using the Gaussian copula.}
#' \item{data_name}{The name of the input dataset.}
#' \item{test}{The name of the test.}
#' \item{Level}{The level of test.}
#' \item{Result}{The result of the combined test at the alpha level with some descriptions on the type of significant interaction.}
#'
#' @references Shenavari, Z., Kharrati-Kopaei, M. (2018). A Method for Testing Additivity in
#'  Unreplicated Two-Way Layouts Based on Combining Multiple Interaction Tests. International Statistical Review
#'  86(3): 469-487.
#'
#' @examples
#' data(CNV)
#' CI_test(CNV, nsim = 1000, Elapsed_time = FALSE)
#' 
#' @importFrom stats pchisq pf qnorm var
#'
#' @export
CI_test <- function(x, nsim = 10000, nc0 = 10000, opvalue = NULL, alpha = 0.05, report = TRUE, Elapsed_time = TRUE) {
  if (!is.matrix(x)) {
    stop("The input should be a matrix")
  } else {
    DNAME <- deparse1(substitute(x))
    y <- c(t(x))
    tr <- ncol(x)
    bl <- nrow(x)
    if (bl == 3) {
      warning("KKSA_test needs at least 4 levels for the row factor. For combining pvalues, the pvalue of the KKSA test is not considered.")
    }
    if (bl <= 2) {
      warning("Franck_test and Piepho_test need at least 3 levels for the row factor. KKSA_test also needs at least 4 levels for the row factor. For combining pvalues, the pvalues of the Franck, Piepho, and KKSA tests are not considered.")
    }
    n <- tr * bl
    block <- gl(bl, tr)
    treatment <- gl(tr, 1, bl * tr)
    p <- min(tr - 1, bl - 1)
    q <- max(tr - 1, bl - 1)
    cck <- 2^(bl - 1) - 1 - bl
    cch <- 2^(bl - 1) - 1
    kp <- kpr(bl, tr)
    c0 <- mean(replicate(nc0, {
      median(abs(kp %*% rnorm(n)))
    }))
    sta <- bmp_f(x)
    Bstat <- sta$Boik
    Mstat <- sta$Tc
    pistat <- sta$piepho
    pstat <- picf(y, kp, c0)
    if (bl == 3) {
      Hstat <- hh_f(x)
    }
    if (bl > 3) {
      Ksimu <- rep(0, 0)
      kh <- kh_f(x)
      Kstat <- kh$fmin
      Hstat <- kh$fmax
    }
    Bsimu <- Msimu <- psimu <- pisimu <- Hsimu <- rep(0, 0)
    if (Elapsed_time) {
      pb <- completed(nsim)
      for (i in 1:nsim) {
        yy <- rnorm(n)
        xx <- matrix(yy, nrow = bl, byrow = TRUE)
        sta <- bmp_f(xx)
        Bsimu[i] <- sta$Boik
        Msimu[i] <- sta$Tc
        pisimu[i] <- sta$piepho
        psimu[i] <- picf(yy, kp, c0)
        if (bl == 3) {
          Hsimu[i] <- hh_f(xx)
        }
        if (bl > 3) {
          kh <- kh_f(xx)
          Ksimu[i] <- kh$fmin
          Hsimu[i] <- kh$fmax
        }
        if (i == pb$pr[pb$j]) pb <- nextc(pb, i)
      }
    } else {
      for (i in 1:nsim) {
        yy <- rnorm(n)
        xx <- matrix(yy, nrow = bl, byrow = TRUE)
        sta <- bmp_f(xx)
        Bsimu[i] <- sta$Boik
        Msimu[i] <- sta$Tc
        pisimu[i] <- sta$piepho
        psimu[i] <- picf(yy, kp, c0)
        if (bl == 3) {
          Hsimu[i] <- hh_f(xx)
        }
        if (bl > 3) {
          kh <- kh_f(xx)
          Ksimu[i] <- kh$fmin
          Hsimu[i] <- kh$fmax
        }
      }
    }
    Tb <- (1 / Bstat - 1)
    if (p == 1) {
      Boik_pvalue <- 1
    }
    if (p == 2) {
      Boik_pvalue <- 1 - pbeta(Tb, 1, (q - 1) / 2)
    }
    if (p > 2) {
      Boik_pvalue <- mean(Bstat >= Bsimu)
    }
    PIC_pvalue <- mean(pstat < psimu)
    Malik_pvalue <- mean(Mstat < Msimu)
    if (bl <= 3) {
      KKSA_pvalue <- NA
    } else {
      KKSA_pvalue <- mean(Kstat > Ksimu)
    }
    if (bl <= 2) {
      hiddenf_pvalue <- NA
    } else {
      hiddenf_pvalue <- mean(Hstat < Hsimu)
    }
    if (bl <= 2 | is.nan(pistat) | any(is.nan(pisimu))) {
      piepho_pvalue <- NA
      warning("Piepho_test is not applicable since this data produce NaN.")
    } else {
      piepho_pvalue <- mean(pistat < pisimu)
    }
    
    pvalues <- c(Boik_pvalue, piepho_pvalue, hiddenf_pvalue, Malik_pvalue, PIC_pvalue, KKSA_pvalue, opvalue)
    if (is.null(opvalue)) {
      names(pvalues) <- c("Boik_test", "Piepho_test", "Franck_test", "Malik_test", "KKM_test", "KKSA_test")
    } else {
      names(pvalues) <- c("Boik_test", "Piepho_test", "Franck_test", "Malik_test", "KKM_test", "KKSA_test", paste0("added test", 1:length(opvalue)))
    }
    if (bl <= 3 | any(is.na(pvalues))) {
      pvalues1 <- pvalues[!is.na(pvalues)]
    } else {
      pvalues1 <- pvalues
    }
    spvalues <- sort(pvalues1, index.return = TRUE)
    sindex <- spvalues$ix
    spvalues <- spvalues$x

    cp <- comb(pvalues1)
    Bonferroni <- cp$Bon
    GC <- cp$GC
    Sidak <- cp$Sidak
    jacobi <- cp$jacobi
    if (report) {
      if (cp$Bon >= alpha & cp$GC >= alpha & cp$Sidak >= alpha & cp$jacobi >= alpha) {
        str <- paste0("No significant interaction was detected at the ", paste0(100 * alpha, "%"), " level.", "\n")
      }
      str1 <- Result_Boik(x, nsim = nsim, alpha = alpha, simu = Bsimu)
      str2 <- Result_Piepho(x, nsim = nsim, alpha = alpha, simu = pisimu)
      str3 <- Result_Franck(x, nsim = nsim, alpha = alpha, simu = Hsimu)$string
      str4 <- Result_Malik(x, simu = Msimu, nsim = nsim, alpha = alpha)
      str5 <- Result_KKM(x, nsim = nsim, simu = psimu, alpha = alpha, nc0 = nc0)
      str6 <- Result_KKSA(x, nsim = nsim, alpha = alpha, simu = Ksimu)$string
      if (!is.null(opvalue)) {
        str7 <- rep(0, length(opvalue))
        for (j in 1:length(opvalue)) {
          str7[j] <- paste0("Significant interactions may be due to the ", paste0("added test", j), " that its p-value is recently added.")
        }
        allstr <- c(str1, str2, str3, str4, str5, str6, str7)
      } else {
        allstr <- c(str1, str2, str3, str4, str5, str6)
      }
      allstr <- allstr[!is.na(pvalues)]
      sstr <- allstr[sindex]
      if ((cp$Bon < alpha | cp$Sidak < alpha | cp$jacobi < alpha)) {
        str <- sstr[1]
      }
      if (cp$Bon < alpha) {
        for (i in 2:length(spvalues)) {
          if (spvalues[i] < alpha / (length(spvalues) - i + 1)) {
            str <- paste0(str, "\n", "\n", paste0("Part ", i, " of the report:"), " In addition to the ", paste0(names(spvalues[i - 1]), ", the "), names(spvalues[i]), " is significant ", " by using the Holm-Bonferroni method.")
            str <- paste(str, sstr[i])
          }
        }
      }
    } else {
      str <- paste("A report has not been wanted! To have a report, change argument 'report' to TRUE.")
    }
    if (bl < 4) {
      KKSA_pvalue <- NA
      Kstat <- NA
    }
    if (bl < 3) {
      hiddenf_pvalue <- NA
      Hstat <- NA
      piepho_pvalue <- NA
      pistat <- NA
    }
    if (bl <= 2 | is.nan(pistat) | any(is.nan(pisimu))) {
      piepho_pvalue <- NA
      pistat <- NA
    }
      
    out <- list(
      nsim = nsim,
      Piepho_pvalue = piepho_pvalue,
      Piepho_Stat = pistat,
      Boik_pvalue = Boik_pvalue,
      Boik_Stat = Bstat,
      Malik_pvalue = Malik_pvalue,
      Malik_Stat = Mstat,
      KKM_pvalue = PIC_pvalue,
      KKM_Stat = pstat,
      KKSA_pvalue = KKSA_pvalue,
      KKSA_Stat = Kstat,
      Franck_pvalue = hiddenf_pvalue,
      Franck_Stat = Hstat,
      Bonferroni = Bonferroni,
      Sidak = Sidak,
      Jacobi = jacobi,
      GC = GC,
      data_name = DNAME,
      test = "Combined interaction Test",
      Level = alpha,
      Result = str
    )
    structure(out, class = "combtest")
  }
}
