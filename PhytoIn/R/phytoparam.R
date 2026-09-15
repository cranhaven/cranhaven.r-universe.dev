#' Estimate phytosociological parameters and diversity indices
#'
#' Estimate the phytosociological parameters and the Shannon--Wiener, Pielou, and
#' Simpson diversity indices, using the quadrat or the point-centered quarter methods.
#'
#' @param x A \code{data.frame} containing the community sample data. See 'Details'.
#' @param measure.label Name of the column representing the circumference/diameter at
#'   breast height. If omitted the function assumes the default names "cbh" or "dbh"
#'   for circumference or diameter at breast height, respectively (see \code{circumference}).
#' @param h Name of the column representing trunk height. Default is "h".
#' @param taxon Name of the column representing the sampled taxa. Default is "taxon".
#'   Use UTF-8 encoding; accents and special characters are not allowed.
#' @param family Name of the column representing the family names of the sampled taxa.
#'   Default is "family". Used to calculate the number of individuals and number of
#'   species per family. If you do not want these parameters, set \code{family = NA}.
#'   Use \code{UTF-8} encoding; accents and special characters are not allowed.
#' @param dead String used to identify dead individuals. Default is "dead".
#' @param circumference Logical. If \code{TRUE} (default) circumference at breast height
#'   was measured; otherwise "dbh" is assumed.
#' @param su Name of the column representing the sample-unit identifier.
#'   Default is "quadrat" for the quadrat method and "point" for the
#'   point-centered quarter method.
#' @param height Logical. If \code{FALSE} (default) trunk volume is not calculated.
#' @param quadrat Logical. If \code{TRUE} (default) data were sampled using the quadrat
#'   method; if \code{FALSE}, the point-centered quarter method is assumed.
#' @param su.size Numeric scalar giving the quadrat area (\eqn{m^2}); required only if
#'   \code{quadrat = TRUE}.
#' @param d Name of the column representing the point-to-tree distance; required only
#'   if \code{quadrat = FALSE}. Default is "distance".
#' @param shape.factor Numeric value between in 0 and 1, indicating the trunk shape.
#'   Value \code{1} assumes a perfect cylinder.
#' @param rm.dead Logical. If \code{FALSE} (default) phytosociological parameters for dead
#'   individuals are calculated.
#' @param check.spelling Logical. If \code{TRUE} (default) taxon names are checked for
#'   misspelling.
#'
#' @details
#' The function estimates phytosociological parameters for tree communities sampled
#' by quadrat or point-centered quarter methods (\code{quadrat = TRUE} or \code{FALSE},
#' respectively).
#'
#' For the quadrat method, \code{x} must contain columns for sample-unit labels,
#' taxon names, and "cbh" or "dbh" measurements for each sampled tree. Additionally,
#' trunk height and family can be included to estimate volume and family-level parameters.
#'
#' For the point-centered quarter method, \code{x} must contain (in addition to the
#' mandatory quadrat columns) a column for the distance from the point to each individual.
#'
#' The "cbh"/"dbh" column accepts multiple-stem notation, e.g. "17.1+8+5.7+6.8".
#' The plus sign delimits stems. Decimal delimiter may be period or comma; spaces
#' around "+" are ignored. Column names in \code{x} are coerced to lowercase at runtime,
#' making matching case-insensitive. If \code{x} contains the default column names, the
#' arguments \code{h}, \code{taxon}, \code{family}, \code{dead}, \code{su} and \code{d} can be omitted.
#'
#' Unbiased absolute density for the point-centered quarter method follows
#' Pollard (1971) and Seber (1982).
#'
#' \strong{Measurement units:} individual "cbh"/"dbh" in centimeters; trunk height and
#' point-to-individual distance in meters.
#'
#' @return An object of class \code{param} with two or four data frames:
#' \itemize{
#'   \item \code{data}: A \code{data.frame} containing the original community sample data,
#'   added with a column of the individual basal area (ABi).
#'   \item \code{global}: total parameters and diversity indices. Sampled area in hectares (ha),
#'   total density in individuals/ha, total dominance in \eqn{m^2} \eqn{ha^{-1}} (basal area) or \eqn{m^3} \eqn{ha^{-1}}
#'   (volume, when computed), and Shannon--Wiener H' in nats/individual (natural log).
#'   \item \code{param}: taxon-level table with observed number of individuals (N), absolute/relative density (ADe, RDe),
#'   absolute/relative frequency (AFr, RFr), absolute/relative dominance (ADo, RDo),
#'   absolute/relative volume (AVol, RVol), Importance Value Index (IV),
#'   and Cover Value Index (CV). Absolute parameters per hectare; relative parameters in percentage.
#'   \item \code{family}: If \code{family != NA}, a table listing the number of individuals and the number of species
#'   per family is presented.
#'   \item \code{vars}: A \code{list} containing objects used in the functions \code{\link{AGB}}, \code{\link{stats}} and \code{\link{stratvol}}.
#'   }
#'
#' @references
#' Pollard, J. H. (1971). On distance estimators of density in randomly distributed forests.
#' \emph{Biometrics}, 27, 991--1002.
#'
#' Seber, G. A. F. (1982). \emph{The Estimation of Animal Abundance and Related Parameters}.
#' New York: Macmillan, pp. 41--45.
#'
#' @seealso \code{\link{summary.param}}, \code{\link{plot.param}}
#'
#' @examples
#' ## Quadrat method
#' quadrat.param <- phytoparam(
#'   x = quadrat.df, measure.label = "CBH", taxon = "Species",
#'   dead = "Morta", family = "Family", circumference = TRUE, su = "Plot",
#'   height = TRUE, su.size = 25, rm.dead = FALSE
#' )
#' summary(quadrat.param)
#' head(quadrat.param$data)
#' quadrat.param$global
#' quadrat.param$family
#' quadrat.param$param
#'
#' ## Point-centered quarter method
#' point.param <- phytoparam(
#'   x = point.df, measure.label = "CBH", taxon = "Species",
#'   dead = "Morta", family = "Family", circumference = TRUE, su = "Point",
#'   height = TRUE, quadrat = FALSE, d = "Distance", rm.dead = FALSE
#' )
#' summary(point.param)
#' head(point.param$data)
#' point.param$global
#' point.param$family
#' point.param$param
#'
#' ## Using default column names
#' point.default <- point.df
#' colnames(point.default) <- c("point", "family", "taxon", "distance", "cbh", "h")
#' point.param.default <- phytoparam(
#'   x = point.default, dead = "morta",
#'   circumference = TRUE, height = TRUE, quadrat = FALSE
#' )
#' summary(point.param.default)
#' point.param.default$global
#'
#' ## Plotting
#' plot(quadrat.param)
#' plot(point.param)
#' plot(point.param, theme = "theme_light")
#' plot(point.param, theme = "theme_bw")
#' plot(point.param, theme = "theme_minimal")
#'
#' @export

phytoparam <- function(x, measure.label=NULL, h="h", taxon="taxon", family="family", dead="dead", circumference=TRUE, su="quadrat", height=TRUE,
                       quadrat=TRUE, su.size, d="distance", shape.factor=1, rm.dead=FALSE, check.spelling=TRUE)
{
  if(!missing(measure.label)) measure.label <- tolower(measure.label)
  taxon <- tolower(taxon)
  family <- tolower(family)
  h <- tolower(h)
  dead <- tolower(dead)
  su <- tolower(su)
  d <- tolower(d)
  colnames(x) <- tolower(colnames(x)) # rename data.frame column names to lowercase only

  # Define measure.label if missing
  if (is.null(measure.label) || !nzchar(measure.label)) {
    measure.label <- if (isTRUE(circumference)) "cbh" else "dbh"
  }
  measure.label <- tolower(measure.label)

  # Define su if quadrat=FALSE
  if (!quadrat && identical(su, "quadrat")) su <- "point"
  # Validations
  if (!family %in% names(x) & !is.na(family)) stop("family is missing or misspelled")
  if (!h %in% names(x) & height) stop("h is missing or misspelled")
  if (!measure.label %in% names(x)) stop("measure.label is missing or misspelled")
  if (!taxon %in% names(x)) stop("taxon is missing or misspelled")
  if (!su %in% names(x)) stop("su is missing or misspelled")
  if (!d %in% names(x) & !quadrat) stop("d is missing or misspelled")

  y<-is.na(x) | x == "" # find all empty cells or NAs
  x <- x[!apply(y == TRUE, 1, all), !apply(y == TRUE, 2, all)]  # remove all-empty/NA rows and columns

  filter <- tolower(x[[taxon]]) == tolower(dead) # flag which individuals are "dead"

  # Check taxon spelling
  if(check.spelling){
    initial.list<-unique(x[[taxon]])
    spp.list<-initial.list
    for(k in 1:length(spp.list)){
      close.strings<-agrep(initial.list[k], spp.list, value=TRUE)
      if(length(close.strings)>1)
      {
        opt<-menu(close.strings, title="Choose the number corresponding the correct name. Type 0 to skip.")
        correct.name<-close.strings[opt]
        incorrect<-close.strings[-opt]
        if(opt!=0) for(i in 1:length(incorrect)) {
          spp<-gsub(incorrect[i], correct.name, x[[taxon]])
          spp.list<-unique(gsub(incorrect[i], correct.name, spp.list))
        }
      }
    }
  }
  N.total <- aggregate(x[[taxon]]!="" ~ x[[taxon]], FUN = sum)  # compute N per species including dead

  if (rm.dead) # if the option is to remove dead plants from the data frame
  {
    x <- x[!filter, ] # remove rows with dead plants from the data frame
    message(sum(filter), " dead individuals removed from the dataset.") # print the number of "dead" removed from the data frame
  }
  x[[su]] <- factor(x[[su]]) # coerce the sampling-unit column to factor
  x[[taxon]] <- factor(x[[taxon]]) # coerce the taxon column to factor

  # *** QUADRAT METHOD ***

  if(quadrat)
  {
    area <- length(unique(x[[su]])) * su.size / 10000  # sampled area

    #  *** DENSITY QUADRAT ***

    N <- aggregate(x[[taxon]]!="" ~ x[[taxon]], FUN = sum)  # compute N per species
    ADe <- N[,2]/area                                       # compute absolute density
    RDe <- ADe/sum(ADe) * 100
    N.spp <- dim(N[tolower(N[, 1]) != tolower(dead), ])[1]

    #  *** FREQUENCY QUADRAT ***

    frequency.table <- table(x[[taxon]], x[[su]]) > 0
    AFr <- rowSums(frequency.table) / dim(frequency.table)[2] * 100
    RFr <- AFr/sum(AFr) * 100

    #  *** DOMINANCE QUADRAT ***

    measure <- x[[measure.label]]  # extract the measurement column
    measure <- gsub(",", ".", measure) # replace any commas with dots

    # Separa os fustes multiplos em colunas
    my_list<-strsplit(x=measure, split="+", fixed=TRUE)
    #print(my_list)
    n.obs <- sapply(my_list, length)
    #print(n.obs)
    seq.max <- seq_len(max(n.obs))
    #print(seq.max)
    mat <- t(sapply(my_list, "[", i = seq.max))
    #print(mat)
    measure.table<-matrix(as.numeric(mat), ncol = max(seq.max))    # convert to numeric matrix
    measure.table[is.na(measure.table)] <- 0
    #print(measure.table)
    if (circumference) AB <- (measure.table/100)^2/(4*pi) else AB <- (pi*(measure.table/100)^2)/4  # compute basal area (m²) of each stem from cbh or dbh
    #print(AB)

    x$ABi <- apply(AB, 1, sum) # compute individual basal area and append to the data
    #print(x$ABi)
    G <- aggregate(x$ABi ~ x[[taxon]], FUN = sum)[,2] # compute basal area per species
    ADo <- G/area         # absolute dominance
    tADo <- sum(ADo)      # total dominance
    RDo <- ADo/tADo*100   # relative dominance

    table <- data.frame(Taxon = N[,1], N = N[,2], ADe = round(ADe, 2), RDe = round(RDe, 2), AFr= round(AFr, 2), RFr= round(RFr, 2), ADo = round(ADo, 2), RDo = round(RDo, 2)) # build the parameter table

    #  *** VOLUME QUADRAT ***

    if(height)
    {
      Vi <- x$ABi * x[[h]] * shape.factor   # compute individual volume
      volume <- aggregate(Vi ~ x[[taxon]], FUN = sum)   # compute volume per species
      AVol <- volume[, 2]/area           # compute absolute volume in m³/ha
      RVol <- AVol/sum(AVol) * 100       # compute relative volume
      table <- cbind(table, AVol= round(AVol, 2), RVol= round(RVol, 2)) # append columns AVol and RVol
    }

    ############################################################################
    # close the quadrat block and start the point-quarter block
  } else {

    if(su=="quadrat") su="point"
    measure <- x[[measure.label]]  # extract the measurement column
    measure <- gsub(",", ".", measure) # replace any commas with dots

    # split multiple stems into columns
    my_list<-strsplit(x=measure, split="+", fixed=TRUE)
    n.obs <- sapply(my_list, length)
    seq.max <- seq_len(max(n.obs))
    mat <- t(sapply(my_list, "[", i = seq.max))
    measure.table<-matrix(as.numeric(mat), ncol = ncol(mat))    # convert to numeric matrix
    measure.table[is.na(measure.table)] <- 0

    if (circumference) AB <- (measure.table/100)^2/(4*pi) else AB <- (pi*(measure.table/100)^2)/4  # compute basal area (m²) of each stem from cbh or dbh

    x$ABi <- apply(AB, 1, sum) # compute individual basal area and append to the data

    N <- aggregate(x[[taxon]]!="" ~ x[[taxon]], FUN = sum)  # compute N per species

    N.spp <- dim(N[tolower(N[, 1]) != tolower(dead), ])[1]

    G <- aggregate(x$ABi ~ x[[taxon]], FUN = sum)[,2] # compute basal area per species

    DC <- x[[d]]  + sqrt(x$ABi/pi)  # corrected distance in meters

    Ai <- DC^2 * pi / 4             # area occupied by each individual

    Ai.su <- aggregate(Ai ~ x[[su]], FUN = sum)  # area occupied by a point

    AM <- sum(Ai)/(length(Ai) - 1)  # mean area occupied by an individual

    area <- sum(Ai)/10000  # total sampled area in ha


    #  *** DENSITY POINT ***

    DT <- 10000/AM   # total density per ha
    ADe <- N[, 2] * DT/sum(N[, 2])             # compute absolute density
    RDe <- ADe/sum(ADe) * 100

    #  *** FREQUENCY POINT ***

    frequency.table <- table(x[[taxon]], x[[su]]) > 0
    AFr <- rowSums(frequency.table) / dim(frequency.table)[2] * 100
    RFr <- AFr/sum(AFr) * 100

    #  *** DOMINANCE POINT ***

    ABM <- mean(x$ABi)
    tADo<- DT * ABM
    ADo <- G * tADo / sum(x$ABi)   # absolute dominance
    tADo <- sum(ADo)               # total dominance
    RDo <- ADo/tADo*100            # relative dominance

    table <- data.frame(Taxon = N[,1], N = N[,2], ADe = round(ADe, 2), RDe = round(RDe, 2), AFr = round(AFr, 2), RFr= round(RFr, 2), ADo = round(ADo, 2), RDo = round(RDo, 2)) # build the parameter table

    #  *** VOLUME POINT ***

    if(height)
    {
      Vi <- x$ABi * x[[h]] * shape.factor   # compute individual volume
      volume <- aggregate(Vi ~ x[[taxon]], FUN = sum)   # compute volume per species
      AVol <- volume[, 2]/area           # compute absolute volume in m³/ha
      RVol <- AVol/sum(AVol) * 100       # compute relative volume
      table <- cbind(table, AVol= round(AVol, 2), RVol= round(RVol, 2)) # append columns AVol and RVol
    }
  } # end of quadrat/point blocks

  if(quadrat) vars <- list(taxon, h, dead, su, su.size) else vars <- list(taxon, h, dead, su, Ai.su)

  # *** Family-level statistics ***

  if(!is.na(family)) {
    if(rm.dead == FALSE)     xf <- x[!filter, ] else xf <- x
    ind.fam<-table(xf[[family]])
    spp.fam <- table(xf[[family]], xf[[taxon]]) > 0
    spp.error <- colnames(spp.fam)[colSums(spp.fam)>1]
    if(sum(colSums(spp.fam)>1)) stop(paste("\n", spp.error, "is assigned to more than one family"))
    table.fam <- data.frame(Family=row.names(ind.fam), Indivuals=as.vector(ind.fam), Species=rowSums(spp.fam)) # Individuals per family
    row.names(table.fam) <- NULL
    N.fam<-dim(table.fam)[1]    # number of families
  }

  #  *** IV and CV ***

  IV <- RDe + RFr + RDo  # compute the Importance Value Index (IVI)
  CV <- RDe + RDo        # compute the Cover Value Index (IVC)

  table <- cbind(table, IV = round(IV, 2), CV = round(CV, 2)) # append IV and CV columns
  N.taxon<-dim(table)[1]  # number of species

  #  *** Shannon, Simpson and Pielou indices ***

  Pe.dead <- N.total[, 2]/sum(N.total[,2]) # relative density including dead
  H.dead <- -sum(Pe.dead*log(Pe.dead))     # H' including dead
  J.dead <- H.dead/log(dim(N.total)[1])    # J including dead
  C.dead <- 1 - (sum(N.total[, 2] * (N.total[, 2] - 1))/(sum(N.total[,2])*(sum(N.total[,2]) - 1)))  # Simpson index including dead

  if(!rm.dead){                                         # test branch for removing dead individuals
    #    filter.dead = tolower(x[[taxon]]) == tolower(dead)       # convert all "dead" words in the data object to lowercase
    x1 <- x[!filter, ]
    N1 <- aggregate(x1[[taxon]]!="" ~ x1[[taxon]], FUN = sum)  # compute N per species
  } else {
    x1 <- x  # remove rows containing dead plants
    N1 <- N
  }

  Pe <- N1[, 2]/sum(N1[,2]) # relative density excluding dead
  H <- -sum(Pe*log(Pe))     # H' excluding dead
  J <- H/log(dim(N1)[1])    # J excluding dead
  C <- 1 - (sum(N1[, 2] * (N1[, 2] - 1))/(sum(N1[,2])*(sum(N1[,2]) - 1)))  # Simpson index excluding dead

  if(quadrat){  # prepare the table with overall parameters
    if(!is.na(family)) {
      global.var <- c("N. of individuals", "N. of samples", "Sampled area", "Total density", "Total dominace", "N. of families", "N. of species",
                      "Shannon-Wiener with dead", "Shannon-Wiener excluding dead", "Pielou with dead", "Pielou excluding dead",
                      "Simpson with dead", "Simpson excluding dead")
      global.values <- round(c(sum(N[, 2]), dim(frequency.table)[2], area, sum(ADe), sum(ADo), N.fam, N.spp, H.dead, H,  J.dead, J, C.dead, C), 4)
      global <- data.frame(Parameter = global.var, Value = global.values)
    }
    else
    {
      global.var <- c("N. of individuals", "N. of samples", "Sampled area", "Total density", "Total dominace", "N. of species",
                      "Shannon-Wiener with dead", "Shannon-Wiener excluding dead", "Pielou with dead", "Pielou excluding dead",
                      "Simpson with dead", "Simpson excluding dead")
      global.values <- round(c(sum(N[, 2]), dim(frequency.table)[2], area, sum(ADe), sum(ADo), N.spp, H.dead, H,  J.dead, J, C.dead, C), 4)
      global <- data.frame(Parameter = global.var, Value = global.values)
    }

  } else
  {
    if(!is.na(family)) {
      global.var <- c("N. of individuals", "N. of samples", "Sampled area", "Unbiased total density", "Total dominace", "N. of families",
                      "N. of species", "Shannon-Wiener with dead", "Shannon-Wiener excluding dead", "Pielou with dead", "Pielou excluding dead",
                      "Simpson with dead", "Simpson excluding dead")
      global.values <- round(c(sum(N[, 2]), dim(frequency.table)[2], area, DT, tADo, N.fam, N.spp, H.dead, H,  J.dead, J, C.dead, C), 4)
      global <- data.frame(Parameter = global.var, Value = global.values)
    }
    else
    {
      global.var <- c("N. of individuals", "N. of samples", "Sampled area", "Unbiased total density", "Total dominace",
                      "N. of species", "Shannon-Wiener with dead", "Shannon-Wiener excluding dead", "Pielou with dead", "Pielou excluding dead",
                      "Simpson with dead", "Simpson excluding dead")
      global.values <- round(c(sum(N[, 2]), dim(frequency.table)[2], area, DT, tADo, N.spp, H.dead, H,  J.dead, J, C.dead, C), 4)
      global <- data.frame(Parameter = global.var, Value = global.values)
    }
  }
  table <- table[order(table$IV, decreasing=TRUE), ]
  rownames(table) <- seq(1, dim(table)[1])
  if(!is.na(family)){
    result <- list(vars = vars, data = x, global = global, family = table.fam, param = table)
    class(result) <- "param"
    invisible(result)
  }
  else
  {
    result <- list(vars = vars, data = x, global = global, param = table)
    class(result) <- "param"
    invisible(result)
  }
}


#' Summarize global phytosociological parameters
#'
#' Display a concise summary of the global parameters computed by \code{\link{phytoparam}}.
#' If family-level outputs are present (i.e., the string "N. of families" occurs
#' in the first column of \code{object$global}), the first seven rows are shown;
#' otherwise, the first six rows are shown.
#'
#'@encoding UTF-8
#'
#' @param object An object of class \code{param} returned by \code{\link{phytoparam}}.
#' @param ...    Ignored.
#'
#' @details Row names of \code{object$global} are removed before printing.
#' The function is intended for quick inspection of the main global metrics.
#'
#' @return Used mainly for its side effect of printing to the console.
#'   Invisibly returns the displayed \code{data.frame}.
#'
#' @seealso \pkg{PhytoIn} (\code{\link{phytoparam}}, \code{\link{plot.param}}.
#'
#' @examples
#' \donttest{
#' res <- phytoparam(x = quadrat.df, measure.label = "CBH",
#'                   taxon = "Species", family = "Family",
#'                   su = "Plot", su.size = 25)
#' summary(res)  # calls summary.param (S3)
#' }
#'
#' @export
#' @method summary param

summary.param <- function(object, ...) {
  if (!inherits(object, "param")) {
    stop("`object` must be of class 'param'.", call. = FALSE)
  }
  if (!is.data.frame(object$global) || ncol(object$global) < 1) {
    stop("`object$global` is missing or malformed.", call. = FALSE)
  }

  out <- object$global
  rownames(out) <- NULL

  n_to_show <- if ("N. of families" %in% out[, 1]) 7L else 6L
  n_to_show <- min(n_to_show, nrow(out))

  print(out[seq_len(n_to_show), , drop = FALSE])
  invisible(out[seq_len(n_to_show), , drop = FALSE])
}


#' Plot relative phytosociological parameters by taxon
#'
#' Produce a stacked bar chart of relative dominance (RDo), relative frequency (RFr),
#' and relative density (RDe) for each taxon contained in a \code{param} object returned
#' by \code{\link{phytoparam}}. Taxa are ordered by the Importance Value (IV).
#'
#' @param x An object of class \code{param} (output of \code{\link{phytoparam}}) whose
#'   \code{$param} data frame contains at least the columns \code{Taxon}, \code{RDe}, \code{RFr}, \code{RDo}, and \code{IV}.
#' @param theme A ggplot2 theme to apply. Either a character string naming a theme
#'   constructor in \strong{ggplot2} (e.g., \code{"theme_light"}, \code{"theme_bw"}, \code{"theme_minimal"}),
#'   or a ggplot2 theme object. Invalid inputs fall back to \code{ggplot2::theme_classic()}.
#' @param ... Ignored.
#'
#' @details The function reshapes the taxon-level table to long format and draws a
#'   horizontal stacked bar chart (RDo, RFr, RDe) with taxa ordered by increasing \code{IV}.
#'
#' @return A \code{ggplot} object.
#'
#' @seealso \code{\link{phytoparam}}, \code{\link{summary.param}}, and \pkg{ggplot2}.
#'
#' @examples
#' res <- phytoparam(x = quadrat.df, measure.label = "CBH", taxon = "Species",
#'                   dead = "Morta", family = "Family", circumference = TRUE,
#'                   su = "Plot", height = TRUE, su.size = 25)
#' plot(res)                        # default theme (theme_classic)
#' plot(res, theme = "theme_light") # theme by name
#' plot(res, theme = ggplot2::theme_minimal()) # theme object
#'
#' @method plot param
#' @importFrom ggplot2 ggplot geom_bar scale_fill_manual labs aes theme theme_classic
#' @importFrom ggplot2 element_text element_rect element_blank element_line
#' @importFrom grid unit
#' @importFrom scales alpha
#' @export

plot.param <- function(x, theme = "theme_classic", ...) {
  obj <- x
  param <- obj$param
  param <- param[order(param$IV, decreasing = FALSE), ]
  # Prepare data for stacked plot
  valores <- cbind(param$RDe, param$RFr, param$RDo)
  n_taxa <- nrow(param)
  df_plot <- data.frame(
    Taxon = rep(param$Taxon, 3),
    Variable = rep(c("Rel. Density", "Rel. Frequency", "Rel. Dominance"),
                   each = n_taxa),
    Value = c(param$RDe, param$RFr, param$RDo),
    IV = rep(param$IV, 3)
  )
  # Reorder Taxon factor based on IV
  df_plot$Taxon <- factor(df_plot$Taxon,
                          levels = param$Taxon[order(param$IV, decreasing = FALSE)])
  # Set variable order for stacking
  df_plot$Variable <- factor(df_plot$Variable,
                             levels = c("Rel. Dominance", "Rel. Frequency", "Rel. Density"))
  cores_pastel <- c(
    "Rel. Density" = "#FFB3BA",      # Pastel pink
    "Rel. Frequency" = "#BAFFC9",    # Pastel green
    "Rel. Dominance" = "#BAE1FF"     # Pastel blue
  )

  ## map theme from string
  resolve_theme <- function(th) {
    if (inherits(th, "theme")) return(th)
    if (is.character(th) && nzchar(th)) {
      if (exists(th, where=asNamespace("ggplot2"), inherits=FALSE)) {
        fun <- utils::getFromNamespace(th, "ggplot2")
        if (is.function(fun)) {
          obj <- try(fun(), silent=TRUE)
          if (inherits(obj, "theme")) return(obj)
        }
      }
    }
    theme_classic()  # fallback
  }
  theme_obj <- resolve_theme(theme)

  # Create the plot
  p <- ggplot2::ggplot(df_plot, aes(x = Value, y = Taxon, fill = Variable)) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    scale_fill_manual(values = cores_pastel,
                      name = NULL,
                      breaks = c("Rel. Density", "Rel. Frequency", "Rel. Dominance")) +
    labs(x = "IV", y = NULL) +
    theme_obj +
    theme(
      # Adjust left margin based on taxa name length
      axis.text.y = element_text(hjust = 1),
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      legend.position = c(0.85, 0.15),  # Position similar to "bottomright"
      legend.background = element_rect(fill = alpha("white", 0.7)),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      panel.grid.major.y = element_line(color = "gray80", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray80", linewidth = 0.5)
    )
  return(p)
}
