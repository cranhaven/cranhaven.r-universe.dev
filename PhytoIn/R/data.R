#' Forest inventory dataset from 25 quadrats
#'
#' A dataset containing tree measurements from 25 quadrats (5 × 5 m each)
#' sampled in a forest fragment located at the campus of the University of Sao Paulo,
#' Ribeirao Preto, Brazil.
#'
#' @format A data frame with 171 rows and 5 variables:
#' \describe{
#'   \item{Plot}{Quadrat identifier (e.g., X2Y3).}
#'   \item{Family}{Botanical family of the tree.}
#'   \item{Species}{Scientific name of the species.}
#'   \item{CBH}{Circumference at breast height (cm).}
#'   \item{h}{Total height of the tree (m).}
#' }
#'
#' @usage data(quadrat.df)
#' @docType data
#' @keywords datasets
"quadrat.df"



#' Point-centered quarter dataset
#'
#' A dataset containing tree measurements collected using the
#' point-centered quarter method at 25 sampling points
#' in a forest fragment located at the campus of the University of Sao Paulo,
#' Ribeirao Preto, Brazil. At each point, the nearest tree in each of the
#' four quadrants was sampled, totaling 100 individuals.
#'
#' @format A data frame with 100 rows and 6 variables:
#' \describe{
#'   \item{Point}{Sampling point identifier (1–25).}
#'   \item{Family}{Botanical family of the tree.}
#'   \item{Species}{Scientific name of the species.}
#'   \item{Distance}{Distance from the point to the tree (m).}
#'   \item{CBH}{Circumference at breast height (cm).}
#'   \item{h}{Total height of the tree (m).}
#' }
#'
#' @usage data(point.df)
#' @docType data
#' @keywords datasets
"point.df"



#' Quadrat dataset with coordinates for basal area plotting
#'
#' A dataset containing tree measurements from 25 quadrats (5 × 5 m each)
#' sampled in a forest fragment located at the campus of the University of Sao Paulo,
#' Ribeirao Preto, Brazil. This dataset is organized to demonstrate the use of the
#' function \code{BAplot()} for visualizing basal areas. In addition to tree measurements,
#' it provides the spatial coordinates of the lower-left corner of each quadrat.
#'
#' @format A data frame with 219 rows and 6 variables:
#' \describe{
#'   \item{Plot}{Quadrat identifier (e.g., X1Y1).}
#'   \item{CBH}{Circumference at breast height (cm).}
#'   \item{h}{Total height of the tree (m).}
#'   \item{Species}{Scientific name of the species.}
#'   \item{x}{X coordinate (m) of the lower-left corner of the quadrat.}
#'   \item{y}{Y coordinate (m) of the lower-left corner of the quadrat.}
#' }
#'
#' @usage data(quadrat2_plot.df)
#' @docType data
#' @keywords datasets
"quadrat2_plot.df"



#' Quadrat dataset with simulated individual tree coordinates
#'
#' A dataset containing tree measurements from 25 quadrats (5 × 5 m each)
#' sampled in a forest fragment located at the campus of the University of Sao Paulo,
#' Ribeirao Preto, Brazil. This dataset corresponds to the same trees as in
#' \code{quadrat2_plot.df}, but includes simulated coordinates (in meters) for
#' each individual tree inside the quadrats. It is organized to demonstrate the
#' use of the function \code{BAplot()} with the argument \code{ind.coord = TRUE}.
#'
#' @format A data frame with 219 rows and 7 variables:
#' \describe{
#'   \item{Plot}{Quadrat identifier (e.g., X1Y1).}
#'   \item{CBH}{Circumference at breast height (cm).}
#'   \item{h}{Total height of the tree (m).}
#'   \item{Species}{Scientific name of the species.}
#'   \item{x}{Simulated X coordinate (m) of the tree inside the quadrat.}
#'   \item{y}{Simulated Y coordinate (m) of the tree inside the quadrat.}
#' }
#'
#' @usage data(quadrat2_tree.df)
#' @docType data
#' @keywords datasets
"quadrat2_tree.df"


#' Rectangular quadrat dataset
#'
#' A dataset containing tree measurements from 25 rectangular quadrats
#' (20 × 10 m each) sampled in a seasonal semideciduous forest fragment
#' in the State of Sao Paulo, Brazil. The dataset includes total height and
#' commercial bole height of trees, along with the spatial coordinates of the
#' lower-left corner of each quadrat. It is organized to demonstrate the use
#' of the function \code{BAplot()} with rectangular quadrats.
#'
#' @format A data frame with 497 rows and 8 variables:
#' \describe{
#'   \item{Plot}{Quadrat identifier.}
#'   \item{x}{X coordinate (m) of the lower-left corner of the quadrat.}
#'   \item{y}{Y coordinate (m) of the lower-left corner of the quadrat.}
#'   \item{Species}{Scientific name of the species.}
#'   \item{Family}{Botanical family of the tree.}
#'   \item{DBH}{Diameter at breast height (cm).}
#'   \item{h}{Total height of the tree (m).}
#'   \item{hcom}{Commercial bole height of the tree (m).}
#' }
#'
#' @usage data(quadrat3_rect.df)
#' @docType data
#' @keywords datasets
"quadrat3_rect.df"


