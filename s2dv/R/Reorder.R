#'Reorder the dimension of an array
#'
#'Reorder the dimensions of a multi-dimensional array. The order can be provided
#'either as indices or the dimension names. If the order is dimension name, 
#'the function looks for names(dim(x)). If it doesn't exist, the function checks
#' if attributes "dimensions" exists; this attribute is in the objects generated
#' by Load(). 
#'
#'@param data An array of which the dimensions to be reordered.
#'@param order A vector of indices or character strings indicating the new 
#'  order of the dimensions.
#'
#'@return An array which has the same values as parameter 'data' but with 
#'  different dimension order.
#'
#'@examples
#'  dat1 <- array(c(1:30), dim = c(dat = 1, sdate = 3, ftime = 2, lon = 5))
#'  print(dim(Reorder(dat1, c(2, 1, 4, 3))))
#'  print(dim(Reorder(dat1, c('sdate', 'dat', 'lon', 'ftime'))))
#'  dat2 <- array(c(1:10), dim = c(2, 1, 5))
#'  print(dim(Reorder(dat2, c(2, 1, 3))))
#'  attr(dat2, 'dimensions') <- c('sdate', 'time', 'region')
#'  dat2_reorder <- Reorder(dat2, c('time', 'sdate', 'region'))
#'  # A character array
#'  dat3 <- array(paste0('a', 1:24), dim = c(b = 2, c = 3, d = 4))
#'  dat3_reorder <- Reorder(dat3, c('d', 'c', 'b'))
#'@export
Reorder <- function(data, order) {

  # Check inputs 
  ## data
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (!is.array(data)) {
    stop("Parameter 'data' must be an array.")
  }

  ## If attribute "dimensions" exists
  attr.dim.reorder <- !is.null(attributes(data)$dimensions)

  ## order
  if (is.null(order)) {
    stop("Parameter 'order' cannot be NULL.")
  }
  if (!is.vector(order) | (is.vector(order) & !is.numeric(order) & !is.character(order))) {
    stop("Parameter 'order' must be a vector of numeric or character string.")
  }
  if (is.numeric(order)) {
    if (any(order < 1) | any(order %% 1 != 0)) {
      stop("Parameter 'order' must be positive integers.")
    } else if (any(order > length(dim(data)))) {
      stop("Parameter 'order' exceeds the dimension length of parameter 'data'.")
    }
  }
  if (is.character(order)) {
    if (is.null(names(dim(data)))) {
      if (attr.dim.reorder) {
        warning("Found dimension names in attributes. Use them to reorder.")
        dim_names <- attributes(data)$dimensions
      } else {
        stop("The array doesn't have dimension names.")
      }
    } else {
      dim_names <- names(dim(data))
      if (attr.dim.reorder && any(attributes(data)$dimensions != dim_names)) {
        warning("Found attribute 'dimensions' has different names from ",
                "names(dim(x)). Use the latter one to reorder.")
      }
    }
    if (!all(order %in% dim_names)) {
      stop("Parameter 'order' do not match the dimension names of parameter 'data'.")
    }
  }
  if (length(order) != length(dim(data))) {
    stop("The length of parameter 'order' should be the same with the ",
         "dimension length of parameter 'data'.")
  }


  ###############################
  # Reorder

  ## If order is character string, find the indices
  if (is.character(order)) {
    order <- match(order, dim_names)
  }

  ## reorder
  old_dims <- dim(data)
  attr_bk <- attributes(data)
  if ('dim' %in% names(attr_bk)) {
    attr_bk[['dim']] <- NULL
  }
  if (is.numeric(data)) {
    data <- aperm(data, order)
  } else {
    y <- array(seq_along(data), dim = dim(data))
    y <- aperm(y, order)
    data <- data[as.vector(y)]
    dim(data) <- old_dims[order]
  }
  if (attr.dim.reorder) {
    attr_bk$dimensions <- attr_bk$dimensions[order]
  }

  attributes(data) <- c(attributes(data), attr_bk)

  return(data)
}


