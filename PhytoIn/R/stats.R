#' Representativeness and confidence statistics for a forest inventory
#'
#' Computes representativeness and confidence statistics for a forest inventory.
#' Summarizes inventory coverage, estimates absolute stand parameters per hectare
#' with uncertainty (density, volume, basal area), and extrapolates results to the
#' total habitat area, including sample-size requirements for target permissible errors.
#'
#' @param obj An object of class \code{param} created by \code{\link{phytoparam}}.
#'   Must contain the raw data (\code{obj$data}), variable mappings (\code{obj$vars}),
#'   and global inventory metadata (\code{obj$global}).
#' @param area.tot Total habitat area (in hectares) to which estimates will be
#'   extrapolated (e.g., the whole forest fragment). \strong{Required}.
#' @param prob Confidence level used to compute \emph{t}-based confidence limits
#'   (default \code{0.95}).
#' @param shape.factor Stem form correction factor used in the individual volume
#'   calculation (\eqn{V_i = abi \times h \times shape.factor}). Default \code{1}
#'   (cylindrical shape).
#' @param rm.dead Logical. If \code{TRUE}, dead trees are excluded from all
#'   calculations (rows whose taxon label matches the “dead” code stored in
#'   \code{obj$vars}).
#'
#' @details
#' Extracts the sample-unit (SU) identifier, taxon label, height/length variable,
#' and “dead” code from \code{obj$vars}, and the surveyed (inventoried) area from
#' \code{obj$global}. If \code{rm.dead = TRUE}, individuals flagged as dead are
#' removed before analysis.
#'
#' Individual volume is computed as \eqn{V_i = abi \times h \times shape.factor},
#' where \code{abi} and \code{h} are columns present in \code{obj$data}. Per-SU
#' totals of volume and basal area are converted to per-hectare values using the
#' SU area. The function then derives, for density (ADe), volume (AVol), and basal
#' area (ABA):
#' \itemize{
#'   \item mean, variance, standard deviation, standard error;
#'   \item coefficient of variation (CV);
#'   \item absolute and relative sampling error (based on the \emph{t} quantile with \code{|SU| - 1} df);
#'   \item lower and upper confidence limits per hectare.
#' }
#'
#' Inventory representativeness (total area, number/area of SUs, and percentage inventoried)
#' is reported, and population totals (for \code{area.tot}) are produced with corresponding
#' confidence limits. Required numbers of SUs to attain \code{10\%} and \code{20\%} permissible
#' relative errors are computed from standard finite-population sampling formulae.
#'
#' @return A list with three components:
#' \itemize{
#'   \item \code{inventory}: data frame summarizing inventory coverage
#'     (fields \code{Stat} and \code{Values}).
#'   \item \code{ha}: data frame of per-hectare statistics for stand parameters
#'     (fields \code{Stat}, \code{ADe}, \code{AVol}, \code{ABA}).
#'   \item \code{population}: data frame of population-level statistics for the
#'     total area (\code{area.tot}), including required numbers of SUs for
#'     \code{10\%} and \code{20\%} permissible errors.
#' }
#'
#' @note
#' \itemize{
#'   \item Representativeness is reported both as inventoried area (ha) and as
#'   percentage of the total area.
#'   \item The number of SUs required for a target permissible error depends on the
#'   observed variance, the SU area, and the chosen confidence level (\code{prob}).
#' }
#'
#' @author Rodrigo Augusto Santinelo Pereira \email{raspereira@usp.br}
#'
#' @references
#' FAO (1981). \emph{Manual of Forest Inventory—With Special Reference to Mixed Tropical Forests}.
#' Food and Agriculture Organization of the United Nations, Rome.
#'
#' @examples
#' ## Creating the param object containing the phytosociological parameters
#' quadrat.param <- phytoparam(x = quadrat.df, measure.label = "CBH",
#'                             taxon = "Species", dead = "Morta", family = "Family",
#'                             circumference = TRUE, su = "Plot", height = TRUE,
#'                             su.size = 25, rm.dead = FALSE)
#'
#' ## Calculating the statistics
#' stats(obj = quadrat.param, area.tot = 4)
#'
#' @export


stats <- function(obj, area.tot, prob = 0.95, shape.factor=1, rm.dead=FALSE)
{
  if(missing(area.tot)) stop("\n 'area.tot' must be indicated")
  if (!inherits(obj, "param")) stop("'obj' must be of class 'param'.")
    x <- obj$data  # extract the data matrix
  colnames(x) <- tolower(colnames(x)) # rename columns to low cases
  taxon <- obj$vars[[1]]
  h <- obj$vars[[2]]
  dead <- obj$vars[[3]]
  area <- obj$global[3,2]
  su <- obj$vars[[4]]

  # remove dead trees
  filter <- tolower(x[[taxon]]) == dead # filter dead individuals

  if (rm.dead)
  {
    x <- x[!filter, ] # remove lines with dead trees
    message (sum(filter), " dead individuals removed from the dataset.")
  }

  x$Vi <- x$abi * x[[h]] * shape.factor   # Volume individual

  # Statistics
  if(is.vector(obj$vars[[5]])) {
    su.size <- obj$vars[[5]]/10000 # quadrat area in ha
    De.su <- aggregate(x[[su]]!="" ~ x[[su]], FUN = sum)
    De.su <-De.su[,2]/su.size     # Density per sample unit
  }
  else {
    Ai.su <- obj$vars[[5]]
    point.area <- Ai.su$Ai
    su.size <- mean(point.area)/10000  # point area in ha
    De.su <- 10000/point.area
  }
  N.su <- length(unique(x[[su]])) # Number of sample units
  t.value <- qt((1 - prob)/2, (N.su-1), lower.tail = FALSE)
  N.su.area <- area.tot/su.size  # Number of sample units in the total area
  perc.invent<- area/area.tot*100 # Percentage of total area that is inventoried

  Vol.su <- aggregate(x$Vi ~ x[[su]], FUN = sum)
  AB.su <- aggregate(x$abi ~ x[[su]], FUN = sum)
  Vol.su <- Vol.su[,2]/su.size   # Volume per sample unit
  AB.su <- AB.su[,2]/su.size     # Basal area per sample unit
  De <- mean(De.su)    # Mean density per su
  Vol <- mean(Vol.su)  # Mean volume per su
  AB <- mean(AB.su)    # Mean basal area per su

  De.sd <- sd(De.su)    # SD density per su
  Vol.sd <- sd(Vol.su)  # SD volume per su
  AB.sd <- sd(AB.su)    # SD basal area per su
  De.se <- De.sd/sqrt(N.su)    # SD density per su
  Vol.se <- Vol.sd/sqrt(N.su)  # SD volume per su
  AB.se <- AB.sd/sqrt(N.su)    # SD basal area per su

  De.CV <- sd(De.su)/De*100     # VC density per su
  Vol.CV <- sd(Vol.su)/Vol*100  # VC volume per su
  AB.CV <- sd(AB.su)/AB*100     # VC basal area per su

  Abs.De.error <-  De.se * t.value   # Absolut sample error
  Abs.Vol.error <-  Vol.se * t.value
  Abs.AB.error <-  AB.se * t.value

  Rel.De.error <- Abs.De.error/De*100      # Relative sample error
  Rel.Vol.error <- Abs.Vol.error/Vol*100
  Rel.AB.error <- Abs.AB.error/AB*100

  De.lower.lim <- De - Abs.De.error   # Lower limit per ha
  Vol.lower.lim <- Vol - Abs.Vol.error
  AB.lower.lim <- AB - Abs.AB.error

  De.upper.lim <- De + Abs.De.error   # Upper limit per ha
  Vol.upper.lim <- Vol + Abs.Vol.error
  AB.upper.lim <- AB + Abs.AB.error

  # Population (area.tot)
  pop.De <- De * area.tot
  pop.Vol <- Vol * area.tot
  pop.AB <- AB * area.tot

  pop.De.error <- pop.De * Rel.De.error/100
  pop.Vol.error <- pop.Vol * Rel.Vol.error/100
  pop.AB.error <- pop.AB * Rel.AB.error/100

  pop.De.lower.lim <- pop.De - pop.De.error   # Pop lower limit
  pop.Vol.lower.lim <- pop.Vol - pop.Vol.error
  pop.AB.lower.lim <- pop.AB - pop.AB.error

  pop.De.upper.lim <- pop.De + pop.De.error   # Pop upper limit
  pop.Vol.upper.lim <- pop.Vol + pop.Vol.error
  pop.AB.upper.lim <- pop.AB + pop.AB.error

  De.error10 <- (N.su.area * De.sd^2 * t.value^2) / (N.su.area * (De*0.1)^2 + De.sd^2 * t.value^2)    # prob. accuracy, 10% error
  Vol.error10 <- (N.su.area * Vol.sd^2 * t.value^2) / (N.su.area * (Vol*0.1)^2 + Vol.sd^2 * t.value^2)
  AB.error10 <- (N.su.area * AB.sd^2 * t.value^2) / (N.su.area * (AB*0.1)^2 + AB.sd^2 * t.value^2)

  De.error20 <- (N.su.area * De.sd^2 * t.value^2) / (N.su.area * (De*0.2)^2 + De.sd^2 * t.value^2)    # prob. accuracy, 20% error
  Vol.error20 <- (N.su.area * Vol.sd^2 * t.value^2) / (N.su.area * (Vol*0.2)^2 + Vol.sd^2 * t.value^2)
  AB.error20 <- (N.su.area * AB.sd^2 * t.value^2) / (N.su.area * (AB*0.2)^2 + AB.sd^2 * t.value^2)

  inventory <- data.frame(Stat = c("Total area (ha)", "n. sample units", "N. su in total area",
                                   "su area (ha)", "Inventoried area", "% inventoried area"),
                          Values = format(round(c(area.tot, N.su, N.su.area, su.size, area, perc.invent), 4), scientifc=FALSE))

  ha <- data.frame(Stat = c("Mean", "Variance", "Standard deviation", "Standard error", "Coefficient of Variation",
                            "Absolut sample error", "Relative sample error", "Lower limit (ha)", "Upper limit (ha)"),
                   ADe = format(round(c(De, De.sd^2, De.sd, De.se, De.CV, Abs.De.error, Rel.De.error, De.lower.lim, De.upper.lim), 2), scientifc=FALSE),
                   AVol = format(round(c(Vol, Vol.sd^2, Vol.sd, Vol.se, Vol.CV, Abs.Vol.error, Rel.Vol.error, Vol.lower.lim, Vol.upper.lim), 2), scientifc=FALSE),
                   ABA = format(round(c(AB, AB.sd^2, AB.sd, AB.se, AB.CV, Abs.AB.error, Rel.AB.error, AB.lower.lim, AB.upper.lim), 2), scientifc=FALSE))

  population <- data.frame(Stat = c("Mean", "Lower limit", "Upper limit", "N. of su (10% permissible error)",
                                    "N. of su (20% permissible error)"),
                           ADe = format(round(c(pop.De, pop.De.lower.lim, pop.De.upper.lim, De.error10, De.error20), 2), scientifc=FALSE),
                           AVol = format(round(c(pop.Vol, pop.Vol.lower.lim, pop.Vol.upper.lim, Vol.error10, Vol.error20)), 2, scientifc=FALSE),
                           ABA = format(round(c(pop.AB, pop.AB.lower.lim, pop.AB.upper.lim, AB.error10, AB.error20), 2), scientifc=FALSE))

  res <- list(inventory = inventory, ha = ha, population = population)
  return(res)
}
