#' Auto MPG Data Set
#'
#' Data concerns city-cycle fuel consumption - revised from CMU StatLib library.
#'
#' @name auto_mpg
#' @docType data
#' @format A matrix containing 398 observations and 10 attributes.
#'    \describe{
#'       \item{\code{mpg}}{Miles per gallon of the engine. Predictor attribute}
#'       \item{\code{cylinders}}{Number of cylinders in the engine}
#'       \item{\code{displacement}}{Engine displacement}
#'       \item{\code{horsepower}}{Horsepower of the car}
#'       \item{\code{weight}}{Weight of the car (lbs)}
#'       \item{\code{acceleration}}{Acceleration of the car (seconds taken for 0-60mph)}
#'       \item{\code{model_year}}{Model year of the car in the 1900s}
#'       \item{\code{origin}}{Car origin}
#'       \item{\code{make}}{Car manufacturer}
#'       \item{\code{car_name}}{Name of the car}
#'    }
#'
#' @references Quinlan,R. (1993). Combining Instance-Based and Model-Based
#' Learning. In Proceedings on the Tenth International Conference of Machine
#' Learning, 236-243, University of Massachusetts, Amherst. Morgan Kaufmann.
#'
#' @source http://archive.ics.uci.edu/ml/datasets/Auto+MPG
#'
#' @examples
#' data(auto_mpg)    # Lazy loading
"auto_mpg"


#' Breakfast Cereal Data Set
#'
#' Nutritional information and manufacturer data for 70+ popular US breakfast cereals
#'
#' @name cereal
#' @docType data
#' @format A matrix containing 77 observations and 16 attributes.
#'    \describe{
#'       \item{\code{name}}{name of cereal}
#'       \item{\code{manuf}}{manufacturer of cereal, coded into seven categories: "A" for American Home Food Products, "G" for General Mills, "K" for Kelloggs, "N" for Nabisco, "P" for Post, "Q" for Quaker Oats, and "R" for Ralston Purina}
#'       \item{\code{type}}{cold or hot}
#'       \item{\code{calories}}{calories per serving}
#'       \item{\code{protein}}{grams of protein}
#'       \item{\code{fat}}{grams of fat}
#'       \item{\code{sodium}}{milligrams of sodium}
#'       \item{\code{fiber}}{grams of dietary fiber}
#'       \item{\code{carbo}}{grams of complex carbohydrates}
#'       \item{\code{sugars}}{grams of sugars}
#'       \item{\code{potass}}{milligrams of potassium}
#'       \item{\code{vitamins}}{vitamins and minerals - 0, 25, or 100, indicating the typical percentage of FDA recommended}
#'       \item{\code{shelf}}{display shelf (1, 2, or 3, counting from the floor)}
#'       \item{\code{weight}}{weight in ounces of one serving}
#'       \item{\code{cups}}{number of cups in one serving}
#'       \item{\code{rating}}{a rating of the cereals}
#'    }
#'
#' @references Reza Mohammadi (2025). Data Science Foundations and Machine
#' Learning with R: From Data to Decisions. \url{https://book-data-science-r.netlify.app}.
#'
#' @source https://lib.stat.cmu.edu/datasets/1993.expo/
#'
#' @examples
#' data(cereal)
#' str(cereal)
"cereal"



#' Wine Data Set
#'
#' Chemical analysis to determine the origin of wines.
#'
#' @name wine
#' @docType data
#' @format A matrix containing 178 observations and 14 attributes (including 1 classification attribute).
#'    \describe{
#'       \item{\code{Class}}{Class of cultivar}
#'       \item{\code{Alcohol}}{Alcohol}
#'       \item{\code{Malic acid}}{Malic acid}
#'       \item{\code{Ash}}{Ash}
#'       \item{\code{Alcalinity of ash}}{Alcalinity of ash}
#'       \item{\code{Magnesium}}{Magnesium}
#'       \item{\code{Total phenols}}{Total phenols}
#'       \item{\code{Flavanoids}}{Flavanoids}
#'       \item{\code{Nonflavanoid phenols}}{Nonflavanoid phenols}
#'       \item{\code{Proanthocyanins}}{Proanthocyanins}
#'       \item{\code{Color intensity}}{Color intensity}
#'       \item{\code{Hue}}{Hue}
#'       \item{\code{OD280/OD315 of diluted wines}}{OD280/OD315 of diluted wines}
#'       \item{\code{Proline}}{Proline}
#'    }
#'
#' @references Dua, D., Graff, C.: UCI Machine Learning Repository. University
#' of California, School of Information and Computer Science, Irvine, CA (2019)
#'
#' @source http://archive.ics.uci.edu/dataset/109/wine
#'
#' @examples
#' data(wine)
#' X <- wine[,-1]
#' class <- wine[,1]
"wine"


