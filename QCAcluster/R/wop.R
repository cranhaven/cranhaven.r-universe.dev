#' Weight of partitions for pooled solution parameters
#' for conservative or parsimonious solution
#' 
#' \code{wop} calculates the contribution or weight of partitions 
#' for the pooled solution parameters of consistency and coverage
#' for the conservative or parsimonious solution. 
#'
#' @importFrom plyr ldply
#' @importFrom testit has_error
#' @importFrom purrr map
#' @importFrom stats na.omit
#' @import QCA
#' 
#' @param dataset Calibrated pooled dataset for partitioning and minimization
#' of pooled solution.
#' @param units Units that define the within-dimension of data (time series).
#' @param time Periods that define the between-dimension of data (cross sections).
#' @param cond Conditions used for the pooled analysis.
#' @param out Outcome used for the pooled analysis.
#' @param n_cut Frequency cut-off for designating truth table rows as observed
#' in the pooled analysis.
#' @param incl_cut Inclusion cut-off for designating truth table rows as
#' consistent in the pooled analysis.
#' @param solution A character specifying the type of solution that should
#' be derived. \code{C} produces the conservative (or complex) solution,
#' \code{P} the parsimonious solution. See \code{\link{wop_inter}} for deriving 
#' intermediate solution.
#' @param amb_selector Numerical value for selecting a single model in the
#' presence of model ambiguity. Models are numbered according to their 
#' order produced by \code{\link{minimize}} by the \code{QCA} package.
#' @return A dataframe with information about the weight of the partitions 
#' with the following columns:
#' 
#' * \code{type}: The type of the partition. \code{between} stands for 
#' cross-sections; \code{within} stands for time series. \code{pooled} stands  
#' information about the pooled data. 
#' * \code{partition}: Type of partition. For 
#' between-dimension, the unit identifiers are listed  (argument \code{units}).
#' For the within-dimension, the time identifiers are listed (argument \code{time}).
#' The entry is \code{-} for the pooled data.
#' * \code{denom_cons}: Denominator of the consistency formula. It is the sum
#' over the cases' membership in the solution.
#' * \code{num_cons}: Numerator of the consistency formula. It is the sum
#' over the minimum of the cases' membership in the solution and the outcome.
#' * \code{denom_cov}: Denominator of the coverage formula. It is the sum
#' over the cases' membership in the outcome.
#' * \code{num_cov}: Numerator of the coverage formula. It is the sum
#' over the minimum of the cases' membership in the solution and the outcome.
#' (identical to \code{num_cons})
#' @md
#' 
#' @examples
#' data(Thiem2011)
#' wop_pars <- wop(
#'   dataset = Thiem2011,
#'   units = "country", time = "year",
#'   cond = c("fedismfs", "homogtyfs", "powdifffs", "comptvnsfs", "pubsupfs", "ecodpcefs"),
#'   out = "memberfs",
#'   n_cut = 6, incl_cut = 0.8,
#'   solution = "P",
#'   amb_selector = 1)
#' wop_pars

#' @export
wop <- function(dataset, units, time, cond, out, n_cut, incl_cut, 
                solution, amb_selector) {
  
  # turning of warnings
  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
  if (missing(amb_selector)) {
    amb_selector <- 1
  }
  
  # splitting the data if time and unit values are available
  x <- dataset
  if (missing(units)) {
    colnames(x)[which(names(x) == time)] <- "time"
    xxx <- 1
  } else if (missing(time)) {
    colnames(x)[which(names(x) == units)] <- "units"
    xxx <- 2
  } else {
    xxx <- 3
    colnames(x)[which(names(x) == time)] <- "time"
    colnames(x)[which(names(x) == units)] <- "units"
  }
  
  PO_list <- list(x)
  
  paster <- function(x) {
    x <- paste(x, collapse = "+")
    x
  }
  
  #### Function for Between and Within Solutions ####
  pqmcc <- function(x) {
    
    check <- x[cond]
    check[check < 0.5] <- 0
    check[check > 0.5] <- 1
    check2 <- as.data.frame(colMeans(check))
    check2[check2 == 1] <- 0
    check3 <- as.numeric(colMeans(check2))
    
    if (check3 == 0) {
      
      stop("No variation in all coniditions")
      
    } else {
      
      s <- testit::has_error(susu <- try(suppressWarnings(QCA::truthTable(x, outcome = out, conditions = cond, incl.cut1 = incl_cut, n.cut = n_cut)), silent = TRUE))
      
      if (s == F) {
        x1 <- try(suppressWarnings(QCA::truthTable(x, outcome = out, conditions = cond, incl.cut1 = incl_cut, n.cut = n_cut)), silent = TRUE)
        
        
        x2 <- x1$tt$OUT
        x2[x2 == "?"] <- NA
        x2 <- as.numeric(x2)
        # x2[is.na(x2)] <- 0.5
        x2 <- na.omit(x2)
        x2 <- mean(x2)
        
        if (x2 == 0) {
          
          stop("All inconsistent")
          
        } else if (x2 == 1 & solution == "P") {
          
          stop("All consistent")
          
        } else {
          
          if (solution == "C") {
            
            x3 <- QCA::minimize(x1, explain = "1", include = "1", details = T, show.cases = T, all.sol = T, row.dom = F)
            
          } else if (solution == "P") {
            
            x3 <- QCA::minimize(x1, explain = "1", include = "?", details = T, show.cases = T, all.sol = T, row.dom = F)
            
          } else {
            
            x3 <- QCA::minimize(x1, explain = "1", include = "1", details = T, show.cases = T, all.sol = T, row.dom = F)
          }
          
          ambig <- length(x3$IC$individual)
          
          if (ambig == 0) {
            ambig <- 1}
          
          if (ambig < 2) {
            
            if (ambig < amb_selector) {
              stop("Selected model number larger than total number of models")
            }
            
            pim <- x3$pims
            pimlength <- as.numeric(ncol(pim))
            pim$max <- do.call(pmax, pim[1:pimlength])
            #denom <- sum(pim$max)
            pim$out <- unlist(x[out])
            pim$min <- with(pim, pmin(max, out))
            
            if (xxx == 1) {
              pim$time <- x$time
            } else if (xxx == 2) {
              pim$units <- x$units
            } else {
              pim$time <- x$time
              pim$units <- x$units
            }
            
            pim_df <- as.data.frame(pim)
            
          } else {
            
            if (ambig < amb_selector) {
              stop("Selected model number larger than total number of models")
            }
            
            pims <- x3$IC$individual[]
            allpims <- purrr::map(pims,3)
            
            getpims <- function(y) {
              pimlength <- as.numeric(ncol(y))
              y$max <- do.call(pmax, y[1:pimlength])
              y$out <- unlist(x[out])
              y$min <- with(y, pmin(max, out))
              if (xxx == 1) {
                y$time <- x$time
              } else if (xxx == 2) {
                y$units <- x$units
              } else {
                y$time <- x$time
                y$units <- x$units
              }
              list <-as.list(y)
            }
            pim <- quiet(lapply(allpims, getpims))
            pim_df <- as.data.frame(pim[amb_selector])
          }
          
        }
      } else {
        
        stop("no combinations at this frequency cutoff")
      }
    }
    return(pim_df)
  }
  
  #### Application of Function ####
  
  PO_list1 <- quiet(lapply(PO_list, pqmcc))
  PO_pims <- as.data.frame(PO_list1)
  
  if (xxx == 1) {
    BE_list <- split(PO_pims, PO_pims[, "time"])
  } else if (xxx == 2) {
    WI_list <- split(PO_pims, PO_pims[, "units"])
  } else {
    BE_list <- split(PO_pims, PO_pims[, "time"])
    WI_list <- split(PO_pims, PO_pims[, "units"])
  }
  
  pimfunc <- function(x) {
    
    if (xxx == 1) {
      part <- as.character(x$time[1])
      type <- "between"
    } else if (xxx == 2) {
      part <- as.character(x$units[1])
      type <- "within"
    } else {
      partition <- unlist(x$time)
      
      if (partition[1] == partition[2]) {
        part <- as.character(x$time[1])
        type <- "between"
      } else {
        part <- as.character(x$units[1])
        type <- "within"
      }
    }
    
    pim_data <- as.data.frame(type)
    pim_data$partition <- part
    pim_data$denom_cons <- sum(x$max)
    pim_data$num_cons <- sum(x$min)
    pim_data$denom_cov <- sum(x$out)
    pim_data$num_cov <- sum(x$min)
    
    pim_data
  }
  
  #### Application of Function ####
  
  if (xxx == 2) {
    WI_list1 <- quiet(lapply(WI_list, pimfunc))
    PO <- quiet(lapply(PO_list1, pimfunc))
    dff2 <- plyr::ldply(WI_list1)[, -1]
    dff3 <- plyr::ldply(PO_list1)[, ]
    dff3$type <- "pooled"
    dff3$partition <- "-"
    total <- rbind(dff3, dff2)
  } else if (xxx == 1) {
    BE_list1 <- quiet(lapply(BE_list, pimfunc))
    PO <- quiet(lapply(PO_list1, pimfunc))
    
    dff1 <- plyr::ldply(BE_list1)[, -1]
    dff3 <- plyr::ldply(PO_list1)[, ]
    dff3$type <- "pooled"
    dff3$partition <- "-"
    total <- rbind(dff3, dff1)
    
  } else {
    BE_list1 <- quiet(lapply(BE_list, pimfunc))
    WI_list1 <- quiet(lapply(WI_list, pimfunc))
    PO <- quiet(lapply(PO_list1, pimfunc))
    
    dff1 <- plyr::ldply(BE_list1)[, -1]
    dff2 <- plyr::ldply(WI_list1)[, -1]
    dff3 <- plyr::ldply(PO)[, ]
    dff3$type <- "pooled"
    dff3$partition <- "-"
    
    total <- rbind(dff3, dff1, dff2)
  }
  
  return(total)
}