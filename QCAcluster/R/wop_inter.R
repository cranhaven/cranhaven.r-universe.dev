#' Calculation of weight of partitions in pooled solution parameters
#' for intermediate solution
#' 
#' \code{wop_inter} calculates the weight of partitions in the pooled
#' solution parameters (consistency, coverage) for the intermediate solution. 
#' 
#' @importFrom plyr ldply
#' @importFrom testit has_error
#' @importFrom purrr map
#' @importFrom stats na.omit
#' @import QCA
#' 
#' @param dataset Calibrated pooled dataset for partitioning and minimization
#' @param units Units defining the within-dimension of data (time series)
#' @param time Periods defining the between-dimension of data (cross sections)
#' @param cond Conditions used for the pooled analysis
#' @param out Outcome used for the pooled analysis
#' @param n_cut Frequency cut-off for designating truth table rows as observed
#' @param incl_cut Inclusion cut-off for designating truth table rows as
#' consistent
#' @param intermediate A vector of directional expectations to derive the
#' intermediate solutions
#' @param amb_selector Numerical value for selecting a single model in the
#' presence of model ambiguity. Models are numbered according to their 
#' order produced by \code{\link{minimize}} by the \code{QCA} package.
#' @return A dataframe with information about the weight of the partitions 
#' for pooled consistency and coverage scores and the following columns:
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
#' data(Schwarz2016)
# Schwarz_wop_inter <- wop_inter(
#   Schwarz2016,
#   units = "country", time = "year",
#   cond = c("poltrans", "ecotrans", "reform", "conflict", "attention"),
#   out = "enlarge",
#   n_cut = 1, incl_cut = 0.8,
#   intermediate = c("1", "1", "1", "1", "1"))
#'
#' @export
wop_inter <- function(dataset, units, time, cond, out, n_cut, incl_cut, 
                      intermediate, amb_selector) {
  
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
          
        } else if (x2 == 1) {
          
          stop("All consistent")
          
        } else {
          t <- testit::has_error(susu <- try(QCA::minimize(x1, 
                                                           explain = "1",
                                                           dir.exp = intermediate, 
                                                           include = "?", 
                                                           details = T, 
                                                           show.cases = T, 
                                                           all.sol = T, row.dom = F), 
                                             silent = TRUE))
          
          if (t==T){
            
            stop("Values specified in the directional expectations do not appear in the data")}
          
          else {
            
            x3 <- QCA::minimize(x1, explain = "1", 
                                dir.exp = intermediate, 
                                include = "?", details = T, 
                                show.cases = T, all.sol = T, 
                                row.dom = F)
            x3
            
            getpims <- function(x){
              if(length(x$solution)<2){
                x <- x$pims
              }else{
                allpi <- x$IC$individual
                x <- purrr::map(allpi,3)
              }
              x
            }
            a <- x3$i.sol
            pimsisi <- lapply(a, getpims)
            new <- unique(pimsisi)
            
            ambig <- length(new)
            if (ambig < amb_selector){
              stop("Selected model number > total number of ambiguous models")
            }   
            
            if(ambig > 1){
              new <- new[amb_selector]
            }else{
              new <- new
            }
            
            pim <- as.data.frame(new)
            
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
            
          }}
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