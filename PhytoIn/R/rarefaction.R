#' Rarefaction Analysis
#'
#' Performs a rarefaction analysis, a method widely used in ecology to estimate species richness
#' based on sample size. The function computes the expected number of species for increasing numbers
#' of individuals, along with confidence intervals, following classical approaches by Hurlbert (1971),
#' Heck et al. (1975), and related developments.
#'
#' @param formula An optional formula specifying the relationship between taxa and sample units
#'   (e.g., \code{Taxon ~ Sample}). If provided, the function extracts variables from \code{data}.
#'   A third variable may be included to remove dead individuals (e.g., \code{Taxon ~ Sample - Dead}).
#' @param data A data frame containing the variables specified in \code{formula} ('long format').
#'   It must contain one column representing the sample unit labels (e.g., quadrats or points)
#'   and one column representing the taxon names of the individual plants. This argument accepts
#'   the data frame used in the argument \code{x} in the function \code{phytoparam}.
#' @param x An optional contingency table of species (rows) by samples (columns). If not provided,
#'   it is calculated from \code{formula} and \code{data}. Alternatively, it can be a vector
#'   representing the number of individuals per species (see Examples).
#' @param step Step size for the sequence of sample sizes in the rarefaction curve. Default is 1.
#' @param points Optional vector of specific sample sizes (breakpoints) for which to calculate rarefaction.
#'   If \code{NULL}, a sequence from 1 to the total number of individuals is used.
#' @param prob The confidence level for the confidence intervals. Default is 0.95.
#' @param xlab Label for the x-axis of the plot (defaults to "Number of individuals").
#' @param ylab Label for the y-axis of the plot (defaults to "Number of species").
#' @param plot Logical; if \code{TRUE}, a rarefaction curve is plotted. Default is \code{TRUE}.
#' @param theme Character string with the name of a ggplot2 theme to be applied to the plot
#'   (e.g., \code{"theme_light"}, \code{"theme_bw"}, \code{"theme_minimal"}).
#'   Default is \code{"theme_classic"}.
#'
#' @details
#' Rarefaction analysis provides a standardized way to compare species richness among samples
#' of different sizes. It is based on probabilistic resampling without replacement and produces
#' an expected species accumulation curve. Confidence intervals are calculated following variance
#' estimators proposed by Heck et al. (1975) and Tipper (1979).
#'
#' The function accepts data in three formats:
#' \itemize{
#'   \item long format (formula + data arguments),
#'   \item contingency matrix,
#'   \item vector of individuals per species.
#' }
#' Dead individuals can be excluded by specifying an additional term in the formula.
#'
#' @return A data frame with the following components:
#' \itemize{
#'   \item \code{n}: number of sample units.
#'   \item \code{s}: mean number of species.
#'   \item \code{lower}: lower confidence interval bound.
#'   \item \code{upper}: upper confidence interval bound.
#'   }
#'
#' If \code{plot = TRUE}, a rarefaction curve with confidence ribbons is produced using \pkg{ggplot2}.
#'
#' @author Rodrigo Augusto Santinelo Pereira \email{raspereira@usp.br}
#'
#' @references
#' Colwell, R. K., Mao, C. X., & Chang, J. (2004).
#' Interpolating, extrapolating, and comparing incidence-based species accumulation curves.
#' \emph{Ecology}, 85(10), 2717--2727. \doi{10.1890/03-0557}
#'
#' Heck, K. L., Van Belle, G., & Simberloff, D. (1975).
#' Explicit calculation of the rarefaction diversity measurement and the determination of sufficient sample size.
#' \emph{Ecology}, 56(6), 1459--1461. \doi{10.2307/1934716}
#'
#' Hurlbert, S. H. (1971).
#' The nonconcept of species diversity: A critique and alternative parameters.
#' \emph{Ecology}, 52(4), 577--586. \doi{10.2307/1934145}
#'
#' Tipper, J. C. (1979).
#' Rarefaction and rarefiction---The use and abuse of a method in paleoecology.
#' \emph{Paleobiology}, 5(4), 423--434. \doi{10.1017/S0094837300016924}
#'
#' @examples
#' ## Using 'formula' (long format)
#' rarefaction(
#'   formula = Species ~ Plot - Morta,
#'   data = quadrat.df,
#'   plot = TRUE
#' )
#'
#' \donttest{
#' ## Using different plot themes
#' rarefaction(
#'   formula = Species ~ Plot - Morta,
#'   data = quadrat.df,
#'   plot = TRUE,
#'   theme = "theme_light"
#' )
#' rarefaction(
#'   formula = Species ~ Plot - Morta,
#'   data = quadrat.df,
#'   plot = TRUE,
#'   theme = "theme_bw"
#' )
#' rarefaction(
#'   formula = Species ~ Plot - Morta,
#'   data = quadrat.df,
#'   plot = TRUE,
#'   theme = "theme_minimal"
#' )
#'
#' ## Using a matrix (wide format)
#' data.matrix <- with(
#'   quadrat.df,
#'   table(Plot, Species, exclude = "Morta")
#' )
#' rarefaction(x = data.matrix, plot = TRUE)
#'
#' data.matrix <- as.matrix(
#'   xtabs(~ Plot + Species, data = quadrat.df, exclude = "Morta")
#' )
#' rarefaction(x = data.matrix, plot = TRUE)
#'
#' ## Using a vector
#' data.vector <- sort(
#'   as.vector(apply(data.matrix, 2, sum)),
#'   decreasing = TRUE
#' )
#' rarefaction(x = data.vector, plot = TRUE)
#'
#' ## Using breakpoints
#' pts <- c(1, 10, 30, 50, 80)
#' rarefaction(
#'   formula = Species ~ Plot - Morta,
#'   data = quadrat.df,
#'   points = pts,
#'   plot = TRUE
#' )
#' rarefaction(x = data.matrix, points = pts, plot = TRUE)
#' rarefaction(
#'   x = data.vector,
#'   points = pts,
#'   plot = TRUE,
#'   theme = "theme_light"
#' )
#' rarefaction(x = data.vector, points = 50, plot = FALSE)
#' }
#' @importFrom ggplot2 ggplot geom_line geom_ribbon xlab ylab aes theme_classic
#' @export

rarefaction <- function(formula, data, x, step = 1, points = NULL, prob = 0.95, xlab, ylab, plot=TRUE, theme = "theme_classic")
{
if(!missing(points)&length(points)==1) plot <- FALSE
if(!missing(formula)) {
  fm <- as.formula(formula)
  vars <- all.vars(fm)
  taxon<-vars[1]
  su<-vars[2]

 y<-is.na(data) | data == ""
 data <- data[!apply(y == TRUE, 1, all), !apply(y == TRUE, 2, all)]

 if (length(vars)==3) # if the option is to remove dead plants from the data frame
  {
  dead<-vars[3]
  filter = data[[taxon]] == dead
  data <- data[!filter, ]
  message(sum(filter), " dead individuals removed from the dataset.") # exibe na tela uma mensagem com o n mero de "mortas" removidas do data frame
  }
x<-table(data[[su]], data[[taxon]])
}
#	if(is.matrix(x)) x <- x > 0 # If data is entered as matrix
	if(missing(xlab)) xlab <- "Number of individuals"
	if(missing(ylab)) ylab <- "Number of species"

# Number of individuals/species
Ni <- if(is.vector(x)) Ni <- x else as.vector(apply(x, 2, sum))
N <- sum(Ni)
N.Ni <- N-Ni
if(!missing(points)) n <- points else n <- seq(from = 1, to = N, by = step)

# Hurlbert 1971
rare<-NULL
for(i in 1:length(Ni))
   rare<-rbind(rare, 1 - ifelse(N.Ni[i] < n, 0, choose(N.Ni[i], n)/choose(N, n)))
   s<-apply(rare, 2, sum)

Sum1 <- NULL
for(i in 1:length(Ni))
   Sum1 <- rbind(Sum1, ifelse(N.Ni[i] < n, 0, choose(N.Ni[i], n)/choose(N, n) * (1 - choose(N.Ni[i], n)/choose(N, n))))

Sum2 <-NULL
   for(j in 2:length(Ni)){
      for(i in 1:(j-1)){
          Sum2 <- rbind(Sum2, ifelse(N - Ni[i] - Ni[j] < n, 0, choose(N - Ni[i] - Ni[j], n)/choose(N, n) - (choose(N - Ni[i], n) * choose(N - Ni[j], n)) / (choose(N, n)*choose(N, n))  ))
          }
       }
# Heck et al. 1975; Tipper 1979
V <- apply(Sum1, 2, sum) + 2 * apply(Sum2, 2, sum)
      alpha <- prob + (1-prob)/2

      tol <- sqrt(.Machine$double.eps)  # ~1.49e-8
      V[V < 0 & abs(V) < tol] <- 0
      V <- pmax(V, 0)

lower<- s - qnorm(alpha) * sqrt(V)
upper<- s + qnorm(alpha) * sqrt(V)


result <- as.data.frame(cbind(n, s, lower, upper))

	if (plot==TRUE){
#        if(all((.packages())!= "ggplot2")) library(ggplot2)

	  ## map theme from string
	  resolve_theme <- function(th) {
	    if (inherits(th, "theme")) return(th)
	    if (is.character(th) && nzchar(th)) {
	      if (exists(th, where=asNamespace("ggplot2"), inherits=FALSE)) {
	        fun <- getFromNamespace(th, "ggplot2")
	        if (is.function(fun)) {
	          obj <- try(fun(), silent=TRUE)
	          if (inherits(obj, "theme")) return(obj)
	        }
	      }
	    }
	    theme_classic()  # fallback
	  }
	  theme_obj <- resolve_theme(theme)

        p<-ggplot(data=result, aes(x=n, y=s)) +
        geom_line() +
        geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.1) +
        xlab(xlab) +
        ylab(ylab) +
        theme_obj
        print(p)
		}
result
}
