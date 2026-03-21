#' Significance testing for the Proportional Allocation 2, Equal Allocation 3,
#' Equal Allocation 2 procedures
#'
#' Performs significance testing for the Proportional Allocation 2, Equal Allocation 3,
#' Equal Allocation 2 procedures.
#' Also reports the hazard ratios, 95\% confidence intervals, p-values,
#' nominal significance levels, and correlations for the overall and simple
#' test statistics.
#'
#' @param time follow-up times
#' @param event event indicators (0/1)
#' @param indA treatment A indicators (0/1)
#' @param indB treatment B indicators (0/1)
#' @param covmat covariate matrix, must be non-NULL.  Factor variables MUST use 0/1 dummy variables
#' @param alpha two-sided familywise significance level
#' @param dig number of decimal places to which we \code{\link{roundDown}} the critical value
#' @param niter number of interations passed to \code{crit2x2} function call
#'
#' @return \item{loghrAoverall }{overall A log hazard ratio}
#' @return \item{seAoverall }{standard error of the overall A log hazard ratio}
#' @return \item{ZstatAoverall }{Z-statistic for the overall A log hazard ratio}
#' @return \item{pvalAoverall }{two-sided p-value for the overall hazard ratio}
#' @return \item{hrAoverall }{overall A hazard ratio}
#' @return \item{ciAoverall }{95\% confidence interval for the overall A hazard ratio}
#' @return \item{loghrAsimple }{simple A log hazard ratio}
#' @return \item{seAsimple }{standard error of the simple A log hazard ratio}
#' @return \item{ZstatAsimple }{Z-statistic for the simple A log hazard ratio}
#' @return \item{pvalAsimple }{two-sided p-value for the simple A hazard ratio}
#' @return \item{hrAsimple }{simple A hazard ratio}
#' @return \item{ciAsimple }{95\% confidence interval for the simple A hazard ratio}
#' @return \item{loghrABsimple }{simple AB log hazard ratio}
#' @return \item{seABsimple}{standard error of the simple AB log hazard ratio}
#' @return \item{ZstatABsimple }{Z-statistic for the simple AB log hazard ratio}
#' @return \item{pvalABsimple }{two-sided p-value for the simple AB hazard ratio}
#' @return \item{hrABsimple }{simple AB hazard ratio}
#' @return \item{ciABsimple }{95\% confidence interval for the simple AB hazard ratio}
#' @return \item{critEA3_A }{Equal Allocation 3's critical value for the overall A simple A, and simple AB hypotheses}
#' @return \item{sigEA3_A }{Equal Allocation 3's p-value rejection criterion for the overall A, simple A, and simple AB hypotheses}
#' @return \item{resultEA3_A }{Equal Allocation 3's accept/reject decisions for the overall A, simple A, and simple AB hypotheses}
#' @return \item{critPA2overallA }{Proportional Allocation 2's critical value for the overall A statistic}
#' @return \item{sigPA2overallA }{Proportional Allocation 2's p-value rejection criterion for the overall A hypothesis}
#' @return \item{critPA2simpleAB}{Proportional Allocation 2's critical value for the simple AB hypothesis}
#' @return \item{sigPA2simpleAB }{Proportional Allocation 2 procedure's p-value rejection criterion for the simple AB hypothesis}
#' @return \item{resultPA2_A }{Proportional Allocation 2 procedure's accept/reject decisions for the overall A and simple A hypotheses}
#' @return \item{critEA2_A }{Equal Allocation 2 procedure's critical value for the simple A and simple AB hypotheses}
#' @return \item{sigEA2_A }{Equal Allocation 2 procedure's p-value rejection criterion for the simple A and simple AB hypotheses}
#' @return \item{resultEA2_A }{Equal Allocation 2 procedure's accept/reject decisions for the simple A and simple AB hypotheses }
#' @return \item{corAa }{correlation between the overall A and simple A logrank statistics}
#' @return \item{corAab }{correlation between the overall A and simple AB logrank statistics}
#' @return \item{coraab }{correlation between the simple A and simple AB logrank statistics}
#'
#' @details  For each of the three multiple testing procedures, the critical values for
#' the overall A (respectively, simple A) logrank statistics may be slightly different
#' from the overall B (respectively, simple B) logrank statistics.  This is
#' due to their slightly different correlations with each other (i.e., correlation
#' between overall A and simple A, respectively, overall B and simple B,
#' statistics) as well as with  the simple AB statistic.
#' @author Eric Leifer, James Troendle
#' @references Leifer, E.S., Troendle, J.F., Kolecki, A., Follmann, D.
#' Joint testing of overall and simple effect for the two-by-two factorial design. (2020). Submitted.
#' @references Lin, D-Y., Gong, J., Gallo, P., et al. Simultaneous inference on treatment effects
#' in survival studies with factorial designs. Biometrics. 2016; 72: 1078-1085.
#' @references Slud, E.V. Analysis of factorial survival experiments. Biometrics. 1994; 50: 25-38.
#' @export fac2x2analyze
#' @examples
#'  # First load the simulated data variables. The "simdataSub" file is
#'  # a 100-by-9 matrix which is loaded with the factorial2x2 package.
#'  time <- simdataSub[, "time"]
#'  event <- simdataSub[, "event"]
#'  indA <- simdataSub[, "indA"]
#'  indB <- simdataSub[, "indB"]
#'  covmat <- simdataSub[, 6:10]
#'  fac2x2analyze(time, event, indA, indB, covmat, alpha = 0.05, niter = 5)
#' #  $loghrA
#' # [1] 0.05613844
#'
#' # $seA
#' # [1] 0.4531521
#'
#' # $ZstatA
#' # [1] 0.1238843
#'
#' # $pvalA
#' # [1] 0.9014069
#'
#' # $hrA
#' # [1] 1.057744
#'
#' # $ciA
#' # [1] 0.4351608 2.5710556
#'
#' # $loghra
#' # [1] 0.1987329
#'
#' # $sea
#' # [1] 0.6805458
#'
#' # $Zstata
#' # [1] 0.2920198
#'
#' # $pvala
#' # [1] 0.7702714
#'
#' # $hra
#' # [1] 1.219856
#'
#' # $cia
#' # [1] 0.3213781 4.6302116
#'
#' # $loghrab
#' # [1] 0.2864932
#'
#' # $seab
#' # [1] 0.6762458
#'
#' # $Zstatab
#' # [1] 0.4236525
#'
#' # $pvalab
#' # [1] 0.6718193
#'
#' # $hrab
#' # [1] 1.331749
#'
#' # $ciab
#' # [1] 0.3538265 5.0125010
#'
#' # $critPA2A
#' # [1] -2.129
#'
#' # $sigPA2A
#' # [1] 0.03325426
#'
#' # $critPA2ab
#' # [1] -2.299
#'
#' # $sigPA2ab
#' # [1] 0.02150494
#'
#' # $result23
#' # [1] "accept overall A" "accept simple AB"
#'
#' # $critEA3
#' # [1] -2.338
#'
#' # $sigEA3
#' # [1] 0.01938725
#'
#' # $result13
#' # [1] "accept overall A" "accept simple A"  "accept simple AB"
#'
#' # $critEA2
#' # [1] -2.216
#'
#' # $sigEA2
#' # [1] 0.0266915
#'
#' # $result12
#' # [1] "accept simple A"  "accept simple AB"
#'
#' # $corAa
#' # [1] 0.6123399
#'
#' # $corAab
#' # [1] 0.5675396
#'
#' # $coraab
#' # [1] 0.4642737


fac2x2analyze <- function(time, event, indA, indB, covmat, alpha, dig = 2, niter = 5){
  # Performs the Wald significance tests for the Proportional Allocation 2,
  # Equal Allocation 3, and Equal Allocation 2 procedures.  NEED to consider the case
  # when covmat = NULL.  It calls crit2x2 which calls
  # cor2x2.
  # time =  follow-up time
  # event = event indicator: 0=censoring, 1=event
  # indA = treatment A indicator (0 = no, 1 = yes)
  # indB = treatment B indicator (0 = no, 1 = yes)
  # covmat = covariate matrix.  NOTE!! Factor variables have to
  #		use 0/1 dummy variables
  # niter = number of iterations in the crit2x2 call
  # It computes the overall A, simple A, and simple AB p-values for
  # the corresponding Cox model log hazard ratio parameter estimates
  # (where the overall A Cox model is stratified
  # on B) to determine the statistical significance of each effect for
  # each of the three procedures.

  # First we calculate the various quantities for the
  # overall A, simple A, and simple AB hypotheses.
  coraux <- cor2x2(time, event, indA, indB, covmat)
  corAa <- coraux$corAa
  corAab <- coraux$corAab
  coraab <- coraux$coraab
  loghrA <- coraux$loghrA
  seA <- coraux$seA
  hrA <- coraux$hrA
  ciA <- coraux$ciA
  pvalA <- coraux$pvalA
  loghra <- coraux$loghra
  sea <- coraux$sea
  hra <- coraux$hra
  cia <- coraux$cia
  pvala <- coraux$pvala
  loghrab <- coraux$loghrab
  seab <- coraux$seab
  hrab <- coraux$hrab
  ciab <- coraux$ciab
  pvalab <- coraux$pvalab

  critaux <- crit2x2(corAa, corAab, coraab, dig = dig,
                     alpha = alpha, niter = niter)
  critPA2A <- critaux$critPA2A
  sigPA2A <- critaux$sigPA2A
  critPA2ab <- critaux$critPA2ab
  sigPA2ab <- critaux$sigPA2ab
  critEA3 <- critaux$critEA3
  sigEA3 <- critaux$sigEA3
  critEA2 <- critaux$critEA2
  sigEA2 <- critaux$sigEA2

  # Make accept/reject statements.
  accA <- "accept overall A"
  rejA <- "reject overall A"
  acca <- "accept simple A"
  reja <- "reject simple A"
  accab <- "accept simple AB"
  rejab <- "reject simple AB"

  # Statement for Proportional Allocation 2 procedure
  if (pvalA <= sigPA2A) {
    res23A <- rejA
  } else {
    res23A <- accA
  }
  if (pvalab <= sigPA2ab) {
    resPA2ab <- rejab
  } else {
    resPA2ab <- accab
  }
  result23 <- c(res23A, resPA2ab)

  # Statement for Equal Allocation 3 procedure
  if (pvalA <= sigEA3) {
    res13A <- rejA
  } else {
    res13A <- accA
  }
  if (pvala <= sigEA3) {
    res13a <- reja
  } else {
    res13a <- acca
  }
  if (pvalab <= sigEA3) {
    res13ab <- rejab
  } else {
    res13ab <- accab
  }
  result13 <- c(res13A, res13a, res13ab)

  # Statement for Equal Allocation 2 procedure
  if (pvala <= sigEA2) {
    res12a <- reja
  } else {
    res12a <- acca
  }
  if (pvalab <= sigEA2) {
    res12ab <- rejab
  } else {
    res12ab <- accab
  }
  result12 <- c(res12a, res12ab)

  ###############################################
  # Next we calculate the various quantities for the
  # overall B and simple B hypotheses. We do not consider
  # the simple AB quantities since they have been
  # calculated above.

  # In the next line, we switch indA and indB from above.
  coraux <- cor2x2(time, event, indB, indA, covmat)
  corBb <- coraux$corAa
  corBab <- coraux$corAab
  corbab <- coraux$coraab
  loghrB <- coraux$loghrA
  seB <- coraux$seA
  hrB <- coraux$hrA
  ciB <- coraux$ciA
  pvalB <- coraux$pvalA
  loghrb <- coraux$loghra
  seb <- coraux$sea
  hrb <- coraux$hra
  cib <- coraux$cia
  pvalb <- coraux$pvala
  loghrab <- coraux$loghrab
  seab <- coraux$seab
  hrab <- coraux$hrab
  ciab <- coraux$ciab
  pvalab <- coraux$pvalab

  critaux <- crit2x2(corBb, corBab, corbab, dig = dig,
                     alpha = alpha, niter = niter)
  critPA2_B <- critaux$critPA2A
  sigPA2_B <- critaux$sigPA2A
#  critPA2ab <- critaux$critPA2ab
#  sigPA2ab <- critaux$sigPA2ab
  critEA3_B <- critaux$critEA3
  sigEA3_B <- critaux$sigEA3
  critEA2_B <- critaux$critEA2
  sigEA2_B <- critaux$sigEA2

  # Make accept/reject statements for B.
  accB <- "accept overall B"
  rejB <- "reject overall B"
  accb <- "accept simple B"
  rejb <- "reject simple B"


  # Statement for Proportional Allocation 2 procedure
  if (pvalB <= sigPA2_B) {
    res23B <- rejB
  } else {
    res23B <- accB
  }
# if (pvalab <= sigPA2ab) {
#   resPA2ab <- rejab
#  } else {
#    resPA2ab <- accab
#  }
  result23_B <- res23B

  # Statement for Equal Allocation 3 procedure
  if (pvalB <= sigEA3_B) {
    res13B <- rejB
  } else {
    res13B <- accB
  }
  if (pvalb <= sigEA3_B) {
    res13b <- rejb
  } else {
    res13b <- accb
  }
#  if (pvalab <= sigEA3) {
#    res13ab <- rejab
#  } else {
#    res13ab <- accab
#  }
  resultEA3_B <- c(res13B, res13b)

  # Statement for Equal Allocation 2 procedure
  if (pvalb <= sigEA2_B) {
    res12b <- rejb
  } else {
    res12b <- accb
  }
#  if (pvalab <= sigEA2) {
#    res12ab <- rejab
#  } else {
#    res12ab <- accab
#  }
  resultEA2_B <- res12b

  list(loghrAoverall = loghrA, seAoverall = seA, ZstatAoverall = loghrA/seA,
       pvalAoverall = pvalA, hrAoverall = hrA, ciAoverall = ciA,
       loghrAsimple = loghra, seAsimple = sea, ZstatAsimple = loghra/sea,
       pvalAsimple = pvala, hrAsimple = hra, ciAsimple = cia,
       loghrABsimple = loghrab, seABsimple = seab, ZstatABsimple = loghrab/seab,
       pvalABsimple = pvalab, hrABsimple = hrab, ciABsimple = ciab,
       critEA3_A = critEA3, sigEA3_A = sigEA3, resultEA3_A = result13,
       critPA2_A = critPA2A, sigPA2_A = sigPA2A, critPA2_ab = critPA2ab,
       sigPA2_ab = sigPA2ab, resultPA2_A = result23,
       critEA2_A = critEA2, sigEA2_A = sigEA2, resultEA2_A = result12,
       corAa = corAa, corAab = corAab, coraab = coraab,
       # Overall B and simple B results
       loghrBoverall = loghrB, seBoverall = seB, ZstatBoverall = loghrB/seB,
       pvalBoverall = pvalB, hrBoverall = hrB, ciBoverall = ciB,
       loghrBsimple = loghrb, seBsimple = seb, ZstatBsimple = loghrb/seb,
       pvalBsimple = pvalb, hrBsimple = hrb, ciBsimple = cib,
#       loghrABsimple = loghrab, seABsimple = seab, ZstatABsimple = loghrab/seab,
#       pvalABsimple = pvalab, hrABsimple = hrab, ciABsimple = ciab,
       critEA3_B = critEA3_B, sigEA3_B = sigEA3_B, resultEA3_B = resultEA3_B,
       critPA2_B = critPA2_B, sigPA2_B = sigPA2_B,
#      critPA2_ab = critPA2ab, sigPA2_ab = sigPA2ab,
       resultPA2_B = res23B,
       critEA2_B = critEA2_B, sigEA2_B = sigEA2_B, resultEA2_B = resultEA2_B,
       corBb = corBb, corBab = corBab, corbab = corbab
       )
}

