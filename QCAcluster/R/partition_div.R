#' Diversity of cases belonging to the same partition of the pooled data
#' 
#' \code{partition_div} calculates the diversity of cases that belong to the same
#' partition of the clustered data (a time series; a cross section; etc.).
#' Diversity is measured by the number of truth table rows that the cases of
#' a partition cover. \code{partition_div} calculates the partition diversity 
#' for all truth table rows and for the subsets of consistent and 
#' inconsistent rows.
#'
#' @importFrom plyr ldply
#' @importFrom testit has_error
#' @import QCA
#' 
#' @param dataset Calibrated pooled dataset that is partitioned 
#' and minimized for deriving the pooled solution.
#' @param units Units defining the within-dimension of data (time series)
#' @param time Periods defining the between-dimension of data (cross sections)
#' @param cond Conditions used for the pooled analysis
#' @param out Outcome used for the pooled analysis
#' @param n_cut Frequency cut-off for designating truth table rows as 
#' observed in the pooled data
#' @param incl_cut Inclusion cut-off for designating truth table rows as
#' consistent in the pooled data
#' 
#' @return A dataframe presenting the diversity of cases belonging to the
#' same partition with the following columns:
#' 
#' * \code{type}: The type of the partition. \code{pooled} are 
#' rows with information on the pooled data; \code{between} is for 
#' cross-section partitions; \code{within} is for time-series partitions. 
#' * \code{partition}: Specific dimension of the partition at hand. For 
#' between-dimension, the unit identifiers are included here 
#' (argument \code{units}). For the within-dimension, the time identifier 
#' are listed (argument \code{time}). The entry is \code{-} for the 
#' pooled data without partitions. 
#' * \code{diversity}: Count of all truth table rows with at least one member
#' belonging to a partition.
#' * \code{diversity_1}: Count of consistent truth table rows with at least 
#' one member belonging to a partition.
#' * \code{diversity_0}: Count of inconsistent truth table rows with at least
#' one member belonging to a partition.
#' * \code{diversity_per}: Ratio of the value for \code{diversity} and the 
#' total number of truth table rows from pooled data
#' (\code{diversity} value for pooled data). 
#' * \code{diversity_per_1}: Ratio of the value for \code{diversity_1} and the 
#' total number of consistent truth table rows from pooled data 
#' (\code{diversity_1} value for pooled data).
#' * \code{diversity_per_0}: Ratio of the value for \code{diversity_0} and the 
#' total number of inconsistent truth table rows from pooled data 
#' (\code{diversity_0} value for pooled data).
#' @md
#'
#' @examples
#' data(Schwarz2016)
#' Schwarz_diversity <- partition_div(Schwarz2016, 
#' units = "country", time = "year", 
#' cond = c("poltrans", "ecotrans", "reform", "conflict", "attention"), 
#' out = "enlarge", 1, 0.8)
#' 
#' @export
partition_div <- function(dataset, 
                          units, time, 
                          cond, out, 
                          n_cut, incl_cut) {
  
  # turning of warnings
  quiet <- function(x) 
    { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
  # splitting the data if time and unit values are available
  x <- dataset
  if (missing(units)) {
    colnames(x)[which(names(x) == time)] <- "time"
    xB <- x
    
    xxx <- 1
    BE_list <- split(xB, xB[, "time"])
    
  } else if (missing(time)) {
    
    colnames(x)[which(names(x) == units)] <- "units"
    xW <- x
    
    xxx <- 2
    WI_list <- split(xW, xW[, "units"])
    
  } else {
    xxx <- 3
    colnames(x)[which(names(x) == time)] <- "time"
    colnames(x)[which(names(x) == units)] <- "units"
    
    xB <- x
    xW <- x
    
    BE_list <- split(xB, xB[, "time"])
    WI_list <- split(xW, xW[, "units"])
  }
  
  PO_list <- list(x)
  
  paster <- function(x) {
    x <- paste(x, collapse = "+")
    x
  }
  
  #### Function for Between and Within Solutions ####
  pqmcc <- function(x) {
    
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
    check <- x[cond]
    check[check < 0.5] <- 0
    check[check > 0.5] <- 1
    check2 <- as.data.frame(colMeans(check))
    check2[check2 == 1] <- 0
    check3 <- as.numeric(colMeans(check2))
    
    if (check3 == 0) {
      
      zz <- as.data.frame(part)
      zz$type <- type
      zz$diversity <- "No variation in all coniditions"
      zz$diversity_per <- "-"
      zz$diversity_1 <- "-"
      zz$diversity_0 <- "-"
      zz <- zz[!duplicated(zz), ]
      colnames(zz)[1] <- "partition"
      
    } else {
      
      # s <- has_error(truthTable(x, outcome = out, conditions = cond, incl.cut1 = x[,ncol(x)][1], n.cut = n_cut))
      s <- testit::has_error(susu <- try(suppressWarnings(QCA::truthTable(x, outcome = out, conditions = cond, incl.cut1 = incl_cut, n.cut = n_cut)), silent = TRUE))
      
      if (s == F) {
        x1 <- try(suppressWarnings(QCA::truthTable(x, outcome = out, conditions = cond, incl.cut1 = incl_cut, n.cut = n_cut)), silent = TRUE)
        
        zz <- as.data.frame(part)
        zz$type <- type
        zz$diversity <- as.numeric(length(x1$indexes))
        zz$diversity_1 <- as.numeric(sum(x1$tt$OUT == 1))
        zz$diversity_0 <- as.numeric(sum(x1$tt$OUT == 0))
        zz$diversity_per <- "???"
        zz <- zz[!duplicated(zz), ]
        colnames(zz)[1] <- "partition"
        
      } else {
        
        zz <- as.data.frame(part)
        zz$type <- type
        zz$diversity <- "no combinations at this frequency cutoff"
        zz$diversity_per <- "-"
        zz$diversity_1 <- "-"
        zz$diversity_0 <- "-"
        zz <- zz[!duplicated(zz), ]
        colnames(zz)[1] <- "partition"
      }
    }
    zz
  }
  
  #### Application of Function ####
  
  if (missing(time)) {
    WI_list1 <- quiet(lapply(WI_list, pqmcc))
    PO_list1 <- quiet(lapply(PO_list, pqmcc))
    dff2 <- plyr::ldply(WI_list1)[, -1]
    dff3 <- plyr::ldply(PO_list1)[, ]
    dff3$type <- "pooled"
    dff3$partition <- "-"
    
    total <- rbind(dff3, dff2)
  } else if (missing(units)) {
    BE_list1 <- quiet(lapply(BE_list, pqmcc))
    PO_list1 <- quiet(lapply(PO_list, pqmcc))
    
    dff1 <- plyr::ldply(BE_list1)[, -1]
    dff3 <- plyr::ldply(PO_list1)[, ]
    dff3$type <- "pooled"
    dff3$partition <- "-"
    
    total <- rbind(dff3, dff1)
    
  } else {
    BE_list1 <- quiet(lapply(BE_list, pqmcc))
    WI_list1 <- quiet(lapply(WI_list, pqmcc))
    PO_list1 <- quiet(lapply(PO_list, pqmcc))
    
    dff1 <- plyr::ldply(BE_list1)[, -1]
    dff2 <- plyr::ldply(WI_list1)[, -1]
    dff3 <- plyr::ldply(PO_list1)[, ]
    dff3$type <- "pooled"
    dff3$partition <- "-"
    
    total <- rbind(dff3, dff1, dff2)
  }
  
  total$diversity_old <- total$diversity
  total$diversity[total$diversity == "No variation in all coniditions"] <- NA
  total$diversity[total$diversity == "no combinations at this frequency cutoff"] <- NA
  total$diversity_1[total$diversity_1 == "-"] <- NA
  total$diversity_0[total$diversity_0 == "-"] <- NA
  total$diversity <- as.numeric(total$diversity)
  total$diversity_1 <- as.numeric(total$diversity_1)
  total$diversity_0 <- as.numeric(total$diversity_0)
  
  if(length(unique(total$diversity)) == 1){
    total$diversity_per <- "-"
    total$diversity_per_1 <- "-"
    total$diversity_per_0 <- "-"
    total$diversity <- "-"
    total$diversity <- total$diversity_old
    total$diversity_old <- NULL
  }else{
    
    y <- as.numeric(max(total$diversity, na.rm = T))
    
    total$diversity_per <- ifelse(is.na(total$diversity), NA, total$diversity/y)
    total$diversity_per_1 <- ifelse(is.na(total$diversity_1), NA, total$diversity_1/y)
    total$diversity_per_0 <- ifelse(is.na(total$diversity_0), NA, total$diversity_0/y)
    total$diversity <- total$diversity_old
    total$diversity_old <- NULL}
  
  
  total <- total[, c(2, 1, 3, 4, 5, 6, 7, 8)]
  
  return(total)}