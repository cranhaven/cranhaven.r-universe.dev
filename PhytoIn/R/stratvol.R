#' Stratified wood volume by DBH classes
#'
#' \code{stratvol} computes wood volume (\eqn{m^3} \eqn{ha^{-1}}) stratified by diameter at breast height (DBH, in centimeters)
#' classes for each taxon in a forest inventory. Individual tree volume is calculated as
#' \eqn{V_i = ABi \times h \times shape.factor}, where \eqn{ABi} is the individual basal area at breast height
#' and \eqn{h} is tree height. Volumes are then summed within DBH classes and standardized per hectare using
#' the inventoried area stored in \code{obj}.
#'
#' @param obj An object of class \code{"param"} produced by \code{\link{phytoparam}},
#'   containing the inventory data, variable names, and global area statistics used for standardization.
#' @param classes Numeric vector of breakpoints (in centimeters) defining the DBH classes.
#'   If a single value is supplied, two classes are formed (\eqn{\leq} value; \eqn{>} value). Defaults to 20.
#' @param shape.factor Stem form correction factor used in the individual volume calculation
#'   (\eqn{V_i = ABi \times h \times shape.factor}). Default 1 (cylindrical shape).
#' @param rm.dead Logical. If \code{TRUE}, individuals labeled as dead (rows whose taxon string equals
#'   the \emph{dead} code stored in \code{obj$vars}) are excluded from all calculations. Default \code{FALSE}.
#'
#' @details
#' - DBH classes are defined from the numeric breakpoints provided in \code{classes}, using closed–open intervals internally
#'   and labeled for readability as: \code{<=a}, \code{]a-b]}, …, \code{>z} (all in centimeters).
#' - Individual volume is computed as \eqn{V_i = ABi \times h \times shape.factor}.
#'   Summed volumes per class are divided by the inventoried area (in hectares) retrieved from \code{obj$global}, yielding \eqn{m^3} \eqn{ha^{-1}}.
#'
#' @return A \code{data.frame} with one row per taxon and the following columns:
#' \itemize{
#'   \item \code{Taxon}: Taxon label.
#'   \item One column per DBH class (labeled in centimeters), containing the summed wood volume per hectare (\eqn{m^3} \eqn{ha^{-1}})
#'   for that taxon within the class. Missing combinations are returned as 0.
#' }
#'
#' @note
#' \itemize{
#'   \item \strong{Units:} Supply size classes in centimeters. The function assumes \code{ABi} and \code{h}
#'   yield volume in cubic meters before per-hectare standardization.
#'   \item \strong{Class limits:} Class breakpoints should not exceed the maximum observed DBH; otherwise the function stops with an error.
#' }
#'
#' @author Rodrigo Augusto Santinelo Pereira (\email{raspereira@usp.br})
#'
#' @references
#' FAO (1981). \emph{Manual of forest inventory—With special reference to mixed tropical forests.}
#' Food and Agriculture Organization of the United Nations.
#'
#' @seealso \code{\link{phytoparam}}
#'
#' @examples
#' # Creating the 'param' object with phytosociological parameters
#' point.param <- phytoparam(x = point.df, measure.label = "CBH",
#'                           taxon = "Species", dead = "Morta", family = "Family",
#'                           circumference = TRUE, su = "Point", height = TRUE,
#'                           quadrat = FALSE, d = "Distance", rm.dead = FALSE)
#'
#' # Stratified volumes with a single breakpoint (<= 20 cm; > 20 cm)
#' stratvol(point.param, classes = 20)
#'
#' # Stratified volumes with multiple classes (<= 5], ]5–10], > 10 cm)
#' stratvol(point.param, classes = c(5, 10))
#'
#' # Using a taper/form correction factor and excluding dead trees
#' stratvol(point.param, classes = c(10, 20, 30), shape.factor = 0.7, rm.dead = TRUE)
#'
#' @export

stratvol <- function(obj, classes=20, shape.factor=1, rm.dead=FALSE)
{
  if (!inherits(obj, "param")) stop("'obj' must be of class 'param'.")
    x <- obj$data  # extract the data matrix
  colnames(x) <- tolower(colnames(x)) # rename columns to low cases
  taxon <- obj$vars[[1]]
  h <- obj$vars[[2]]
  dead <- obj$vars[[3]]
  area <- obj$global[3,2]
  # remove dead trees
  filter <- tolower(x[[taxon]]) == dead # filter dead individuals

  if (rm.dead)
  {
    x <- x[!filter, ] # remove lines with dead trees
    message(sum(filter), " dead individuals removed from the dataset.")
  }

  x$Diam <- 2 * sqrt(x$abi/pi)  # Convert ABi in diameter
  x$Vi <- x$abi * x[[h]] * shape.factor   # Volume individual

  # Stratify by diameter
  if(length(classes)==1){
    lab <- c(paste0("<=",classes), paste0(">",classes))
  } else{
    lab <- paste0("<=",classes[1])
    for(i in 1:(length(classes)-1)){
      lab <- c(lab, paste0("]",classes[i],"-", classes[i+1], "]"))
    }
    lab <- c(lab, paste0(">", tail(classes, n=1)))
  }
  classes <- classes/100
  if(max(classes) > max(x$Diam)) stop("\n Class value higher than the maximum diameter")
  # Create vector of classes
  diam_classes <- cut(x$Diam, breaks = c(-Inf, classes, Inf), labels = lab)  # Create vector of classes
  data.split <- split(x, diam_classes)  # Split data frame according to classes

  # Volume per species per class
  species <- unique(x[[taxon]])
  table <- aggregate(I(Vi/area)~data.split[[1]][[taxon]], data = data.split[[1]], FUN=sum, na.rm = TRUE)
  volume <- table[, 2][match(species, table[, 1])]
  res <- data.frame(species, volume)
  for(j in 2:length(lab)){
    table <- aggregate(I(Vi/area)~data.split[[j]][[taxon]], data = data.split[[j]], FUN=sum, na.rm = TRUE)
    volume <- table[, 2][match(species, table[, 1])]
    res <- cbind(res, volume)
  }
  colnames(res) <- c("Taxon", lab)
  res[is.na(res)]<-0
  return(res)
  }
