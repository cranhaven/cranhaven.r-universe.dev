#' Plot basal areas on a map of quadrats
#'
#' Plot basal areas of trees on a map of quadrats. If individual tree coordinates
#' are not known, the coordinates inside the quadrats are randomly defined according
#' to the uniform distribution.
#'
#' @param formula A model formula indicating the trunk measure (circumference [default]
#' or diameter) in centimeters and the xy coordinates of each quadrat the tree belongs
#' (\code{ind.coord = FALSE}) or, the actual tree coordinates (\code{ind.coord = TRUE}).
#' Example: \code{measure ~ x + y}. See Details.
#' @param data A data frame containing the community sample data. See Details.
#' @param taxon Name of the column representing the sampled taxa. Default is \code{"taxon"}.
#' @param circumference Logical. If \code{TRUE} (the default), the function assumes that the
#' circumference at breast height was measured.
#' @param quadrat.size A vector indicating the side lengths (in meters) of the x and y quadrat
#' sides (e.g., \code{c(x, y)}). It can be given as a single value if the quadrat is a square.
#' @param dead String used to identify the dead individuals. Default is \code{"dead"}.
#' @param rm.dead Logical. If \code{FALSE} (the default) basal areas of dead individuals are plotted.
#' @param origin A numeric vector indicating the map origin coordinates. Default is \code{c(0, 0)}.
#' @param col Circle color of represented basal areas. This argument has value only if
#' \code{legend = FALSE}. Default is \code{"grey40"}.
#' @param alpha Value of transparency factor: zero (100\% transparent) -- 1 (no transparency).
#' Default is 1.
#' @param cex.radius A numerical value giving the amount by which the tree radius should be
#' magnified relative to the actual measure. Default is 1.
#' @param ind.coord Logical indicating whether the individual coordinates are given. If
#' \code{FALSE} (the default) the tree coordinates inside the quadrats are randomly defined.
#' If \code{TRUE}, the actual tree coordinates are plotted.
#' @param legend Logical. If \code{TRUE} (default), the taxon is used as the color legend;
#' otherwise, circle color will be defined by the argument \code{col}.
#' @param long Logical. If \code{FALSE} (default) the function does not return the result data frame,
#' which contains xy coordinates, radius and taxon name of each sampled tree.
#'
#' @details
#' \code{BAplot} uses the function \code{circleRepelLayout()} from the \pkg{packcircles} package
#' to rearrange circle coordinates to avoid overlapping. The minimum distance allowed among trees
#' is 1 meter. The packages \pkg{ggforce} and \pkg{ggplot2} are used to draw the map.
#'
#' The data frame passed to the \code{data} argument must include two columns indicating x and y
#' coordinates of each quadrat that the tree belongs or the actual tree coordinates. If actual
#' coordinates are supplied, the \code{ind.coord} argument must be set \code{TRUE}.
#'
#' Circumference/diameter measures accept the traditional notation for multiple trunks, e.g.,
#' \code{"17.1+8+5.7+6.8"}. The plus sign is the separator for each trunk measure. Decimal
#' separator can be point or comma and spaces after or before \code{"+"} are ignored by the function.
#'
#' @return A plot representing the quadrat map and tree basal areas. If \code{long = TRUE}, the
#' function returns a data frame containing taxon name, xy coordinates and radius of each sampled tree.
#'
#' @author Rodrigo A. S. Pereira (\email{raspereira@usp.br})
#'
#' @references
#' Collins, C. R., and Stephenson, K. (2003). A circle packing algorithm.
#'   \emph{Computational Geometry}, 25(3), 233--256. \doi{10.1016/S0925-7721(02)00099-8}
#'
#' Wang, W., Wang, H., Dai, G., and Wang, H. (2006). Visualization of large hierarchical data by circle packing.
#'   \emph{Proceedings of the SIGCHI Conference on Human Factors in Computing Systems}, 517--520.
#'   \doi{10.1145/1124772.1124851}
#'
#' @examples
#' # Using plot coordinates (random coordinates for individuals)
#' data1 <- quadrat2_plot.df
#' BAplot(formula = CBH ~ x + y, data = data1, taxon = "Species",
#'        circumference = TRUE, quadrat.size = 5, dead = "Morta",
#'        rm.dead = FALSE, alpha = 0.4, cex.radius = 2,
#'        legend = TRUE, long = FALSE, ind.coord = FALSE)
#'
#' # Using actual coordinates
#' data2 <- quadrat2_tree.df
#' BAplot(formula = CBH ~ x + y, data = data2, taxon = "Species",
#'        circumference = TRUE, quadrat.size = 5, dead = "Morta",
#'        rm.dead = FALSE, alpha = 0.4, cex.radius = 2,
#'        legend = TRUE, long = FALSE, ind.coord = TRUE)
#'
#' # Rectangular plots and plot coordinates
#' data3 <- quadrat3_rect.df
#' BAplot(formula = DBH ~ x + y, data = data3, taxon = "Species",
#'        circumference = FALSE, quadrat.size = c(20, 10),
#'        dead = "Morta", rm.dead = FALSE, col = "blue",
#'        alpha = 0.4, cex.radius = 2, legend = FALSE,
#'        long = FALSE, ind.coord = FALSE)
#'
#' @importFrom ggplot2 ggplot theme element_blank aes scale_x_continuous scale_y_continuous coord_fixed
#' @importFrom ggforce geom_circle
#' @importFrom packcircles circleRepelLayout
#' @export

BAplot <- function(formula, data, taxon="taxon", circumference=TRUE, quadrat.size, dead="dead", rm.dead=FALSE,
                    origin=c(0, 0), col="grey40", alpha=1, cex.radius=1, ind.coord=FALSE, legend=TRUE, long=FALSE)
{
  if (!taxon %in% names(data)) stop("taxon is missing or misspelled")
  if(missing(quadrat.size)) stop("Quadrat size is mandatory")
  y<-is.na(data) | data == "" # search empty and NAs cells
  data <- data[!apply(y == TRUE, 1, all), !apply(y == TRUE, 2, all)]
  #if(all((.packages())!= "ggplot2")) require(ggplot2)
  #if(all((.packages())!= "ggforce")) require(ggforce)
  #if(all((.packages())!= "packcircles")) require(packcircles)

  if (rm.dead) # remove dead trees
  {
    filter = tolower(data[[taxon]]) == tolower(dead)
    data <- data[!filter, ]
    message(sum(filter), " dead individuals removed from the dataset")
  }
  fm <- as.formula(formula) # convert 'formula' in a formula object
  vars <- all.vars(fm)
  vars <- c(vars[1], sort(vars[2:3]))

  # Individual radius
  measure <- data[[vars[1]]]  # extract the measure column
  measure <- gsub(",", ".", measure)

  # split multiple trunks
  my_list<-strsplit(x=measure, split="+", fixed=TRUE)
  n.obs <- sapply(my_list, length)
  seq.max <- seq_len(max(n.obs))
  mat <- t(sapply(my_list, "[", i = seq.max))
  measure.table<-matrix(as.numeric(mat), ncol = max(seq.max))   # Convert to numeric matrix
  measure.table[is.na(measure.table)] <- 0

  if (circumference) AB <- (measure.table/100)^2/(4*pi) else AB <- (pi*(measure.table/100)^2)/4
  ABi <- apply(AB, 1, sum)   # basal area of each tree
  r <- sqrt(ABi/pi)
  if(length (quadrat.size)==1) quadrat.size <- c(quadrat.size, quadrat.size)

  # Test whether xy is present in 'data'
  if(ind.coord) {
    # Grid
    grid.x <- seq(origin[1], ceiling(max(data[[vars[2]]])/quadrat.size[1]) * quadrat.size[1], by=quadrat.size[1])
    grid.y <- seq(origin[2], ceiling(max(data[[vars[3]]])/quadrat.size[2]) * quadrat.size[2], by=quadrat.size[2])
    res.plot <- data.frame(taxon=data[[taxon]], x=data[[vars[2]]], y=data[[vars[3]]], radius=r)    # Junta a coluna com o nome das especies
  }
  else {
    quadrat.label<-with(data, paste(x,y))
    quadrat.index<-unique(quadrat.label)
    data<-cbind(quadrat.label, data)

    r <- r + 0.5   # Individual radius + 0.5 to avoid circle overlap
    xi <- NULL
    yi <- NULL

    for(i in 1:dim(data)[1])
    {
      xi[i]<-runif(1, min = data[[vars[2]]] [i] + 0.3, max = data[[vars[2]]] [i] + quadrat.size[1] - 0.3) # sample a random position within the quadrat
      yi[i]<-runif(1, min = data[[vars[3]]] [i] + 0.3, max = data[[vars[3]]] [i] + quadrat.size[2] - 0.3)
    }
    # repel the coordinates to avoid circle overlap
    data <- cbind(xi, yi, r, data)
    res.plot <- NULL
    for(j in quadrat.index){
      data.sub <- subset(data, subset=quadrat.label == j)
      res <- packcircles::circleRepelLayout(x=data.sub, xlim = c(data.sub[[vars[2]]] [1], data.sub[[vars[2]]] [1] + quadrat.size[1]),
                               ylim = c(data.sub[[vars[3]]] [1], data.sub[[vars[3]]] [1] + quadrat.size[2]), xysizecols = 1:3, sizetype="radius")$layout
      res <- cbind(res, taxon=data.sub[[taxon]])      # bind the column with taxon names
      res.plot <- rbind(res.plot, res)                # add data of quadrat j
    }
    res.plot$radius <- res.plot$radius - 0.5        # Subtract 0.5 from the radius

    grid.x <- unique(data[[vars[2]]])
    grid.y <- unique(data[[vars[3]]])

    grid.x <- c(grid.x, tail(grid.x, n=1) + quadrat.size[1])
    grid.y <- c(grid.y, tail(grid.y, n=1) + quadrat.size[2])

  } # End of 'if()'

  # plot the map

  if(legend){
    map<-ggplot2::ggplot() +
      theme(panel.grid.minor = element_blank()) +
      geom_circle(aes(x0 = x, y0 = y, r = radius*cex.radius, fill = taxon, color=taxon), alpha=alpha, data = res.plot) +
      scale_x_continuous(breaks = grid.x) +
      scale_y_continuous(breaks = grid.y) +
      coord_fixed()
    print(map)
  } else {
    map<-ggplot2::ggplot() +
      theme(panel.grid.minor = element_blank()) +
      geom_circle(aes(x0 = x, y0 = y, r = radius*cex.radius), fill = col, color=NA, alpha=alpha, show.legend=FALSE, data = res.plot) +
      scale_x_continuous(breaks = grid.x) +
      scale_y_continuous(breaks = grid.y) +
      coord_fixed()
    print(map)
  }
  if(long) return(res.plot)
}
