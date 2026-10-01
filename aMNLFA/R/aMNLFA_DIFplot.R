#' aMNLFA plotting function for aMNLFA.prune() results
#'
#' This function gives the user a plot corresponding to loading, intercept, or threshold DIF from the aMNLFA.prune() function
#' @param diflist The listing of results from aMNLFA.prune(), which contains the DIF tables (as well as impact tables, which aren't used here)
#' @param diftype The type of DIF the user wants plot for. Options include "loading" (for loading DIF), "intercept" (for intercept DIF when threshold DIF is not tested), "threshold.highest" (which uses only the largest test statistic across all categories when threshold DIF is tested), and "threshold.all" (which uses the test statistic for all categories when threshold DIF is tested)
#' @param log Logical. If TRUE, plot the y axis on a log scale. Defaults to FALSE.
#' @return No return value; generates a plot using base R.
#' @keywords MNLFA
#' @export
#' @examples
#'  \dontrun{
#'  wd <- tempdir()
#'  first<-paste0(system.file(package='aMNLFA'),"/extdata")
#'  the.list <- list.files(first,full.names=TRUE)
#'  file.copy(the.list,wd,overwrite=TRUE)
#'    
#'  ob <- aMNLFA::aMNLFA.object(dir = wd, 
#'  mrdata = xstudy, 
#'  indicators = paste0("BIN_", 1:12),
#'  catindicators = paste0("BIN_", 1:12), 
#'  meanimpact = c("AGE", "GENDER", "STUDY"), 
#'  varimpact = c("AGE", "GENDER", "STUDY"), 
#'  measinvar = c("AGE", "GENDER", "STUDY"),
#'  factors = c("GENDER", "STUDY"),
#'  ID = "ID",
#'  thresholds = FALSE)
#'  
#'  prune.object <- aMNLFA.prune(ob)
#'  aMNLFA.DIFplot(prune.object, "loading", log = FALSE)
#'  }

aMNLFA.DIFplot <- function(diflist, diftype, log = FALSE) {

  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
    
  myMeasInvar <- diflist$summary$measinvar
  myindicators <- diflist$summary$indicators
  
  
  if (diftype == "loading") {
    difobject <- diflist$summary$loadingDIF
    difstring <- "loading"}
  if (diftype == "intercept") {
    difobject <- diflist$summary$interceptDIF
    difstring <- "intercept"}
  if (diftype == "threshold.highest") {
    difobject <- diflist$summary$tDIF_highest
    difstring <- "threshold"}
  if (diftype == "threshold.all") {
    difobject <- diflist$summary$tDIF_all
    difstring = "threshold"}
  
  graphics::par(mar=c(10,4,4,2))
  the.series <- subset(difobject, difobject$pval < .05)
  the.series$pval[the.series$pval == 0] <- .00001
  if (nrow(the.series) > 0) {
    if (log == TRUE) {
    the.plot <- plot(1:nrow(the.series), the.series$pexact, main = paste0("All ", difstring, " effects with unadjusted p. values under .05"), xlab = " ", xaxt = "n", ylab = "p. value", ylim = c(min(the.series$pexact), .05), yaxt = "n", log = "y", pch = 3)
    marks <- c(.0001, .001, .01, .05)
    graphics::axis(2,at=marks,labels=format(marks,scientific=FALSE))
  } else {
    the.plot <- plot(1:nrow(the.series), the.series$pexact, main = paste0("All ", difstring, " effects with unadjusted p. values under .05"), xlab = " ", xaxt = "n", ylab = "p. value", ylim = c(min(the.series$pexact), .05), yaxt = "n", pch = 3)
    marks <- c(.01, .02, .03, .04, .05)
    graphics::axis(2,at=marks,labels=format(marks,scientific=FALSE))
  }

  
    if (nrow(the.series) > 1) {
      graphics::abline(h = .05, col = "#EE4035")
      graphics::lines(1:nrow(the.series), the.series$BH.actual, col = "#F37736")
      graphics::lines(1:nrow(the.series), the.series$BH.ibc, col = "#3D1E6D")
      graphics::lines(1:nrow(the.series), the.series$bon.actual, col = "#7BC043")
      graphics::lines(1:nrow(the.series), the.series$bon.ibc, col = "#0392CF")
    } else {
      graphics::abline(h = .05, col = "#EE4035")
      graphics::abline(h = the.series$BH.actual, col = "#F37736", lty = 1)
      graphics::abline(h = the.series$BH.ibc, col = "#3D1E6D", lty = 1)
      graphics::abline(h = the.series$bon.actual, col = "#7BC043", lty = 2)
      graphics::abline(h = the.series$bon.ibc, col = "#0392CF", lty = 2)
    }
    graphics::points(1:nrow(the.series), the.series$pexact, pch = 3)
    if ("category.label" %in% names(difobject)) {
      x.marks <- paste0(myindicators[difobject$item.label], " / ", difobject$category.label, " / ", myMeasInvar[difobject$covariate.label])
    } else {
      x.marks <- paste0(myindicators[difobject$item.label],  " / ", myMeasInvar[difobject$covariate.label])
    }
    graphics::axis(1, at = 1:nrow(difobject), labels = x.marks, las = 2)
    if (log == FALSE) {
      graphics::legend(1, .05, legend = c("Unadjusted", 
                               "B-H with m = effects tested in simultaneous model", 
                               "Bonferroni with m = effects tested in simultaneous model",
                               "B-H with m = all possible tests", 
                               "Bonferroni with m = all possible tests"),
             col = c("#EE4035", "#F37736", "#7BC043", "#3D1E6D", "#0392CF"), lty = 1, bty = "n")
    } else {
      graphics::legend("bottomright", legend = c("Unadjusted", 
                                "B-H with m = effects tested in simultaneous model", 
                                "Bonferroni with m = effects tested in simultaneous model",
                                "B-H with m = all possible tests", 
                                "Bonferroni with m = all possible tests"),
             col = c("#EE4035", "#F37736", "#7BC043", "#3D1E6D", "#0392CF"), lty = 1, bty = "n")
      }

  
  }
}
