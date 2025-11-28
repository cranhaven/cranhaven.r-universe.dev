#' Create a Probability Box (Pbox) Object
#'
#' Constructs a probability box (Pbox) object from a given dataset and a pre-defined copula model. This
#' auxiliary method facilitates the integration of data with a copula to form a comprehensive probabilistic
#' model known as a Pbox.
#'
#' @name make_pbox
#' @export
#' @param data A dataframe or data table; this data will be coerced to a `data.table` internally.
#' @param cop An object of class `mvdc` representing the multivariate dependency structure (copula).
#' @return An object of class `pbox` with slots:
#'         - `$data`: The data coerced into a `data.table`.
#'         - `$copula`: The provided copula object.
#' @examples
#'   library(copula)
#'   data("SEAex")
#'
#'   cop <- normalCopula(param = 0.5, dim = 4)
#'   distList <- c("RG", "SN1", "RG", "RG")
#'   allDistrs <- list(list(mu = 31.07, sigma = 0.28),
#'                     list(mu = 34.4, sigma = 0.98, nu = 1.7),
#'                     list(mu = 31.4, sigma = 0.34),
#'                     list(mu = 25.6, sigma = 0.24))
#'   copSEA <- mvdc(cop, distList, allDistrs)
#'   pbx <- make_pbox(data = SEAex, cop = copSEA)
#'   print(class(pbx))
#' @importFrom data.table setDT

setGeneric("make_pbox",
           def = function(data, cop) {
             standardGeneric("make_pbox")
           })


#' @rdname make_pbox
#' @description
#' Method for creating a `pbox` object using a specified copula and data. This method ensures
#' that the input data and copula are compatible in terms of dimensions and structurally fit
#' to form a Pbox.
#'
#'
#'
#'
setMethod("make_pbox",
          definition =  function(data,cop){
            if (!inherits(data, c("data.frame","data.table"))) {
              stop("Input must be a data frame or a data.table")
            }
            if (!inherits(cop, c("mvdc"))) {
              stop("Input must be an object of class mvdc genrated using the copula package")
            }

            # ADD checks for colnames, number of marginal distributions
            if (!ncol(data)==cop@copula@dimension) {
              stop("The number of columns in the datset and the dimension of the copula object do not match")
            }
            setDT(data)

            obj <- new("pbox", data =data, copula=cop,fit=list())

          })

