#' @title Detailed description of a data frame
#' @description
#' \code{contents} provides a comprehensive description of a data
#' frame, including summary statistics for both quantitative and
#' categorical variables
#' @param data a data frame
#' @param digits number of decimal digits for statistics.
#' @param maxcat maximum number of levels of a character/factor
#' variable to print.
#' @param label_length maximum length of factor level label
#' to print. Longer labels will be truncated.
#' @return a list with 6 components:
#' \describe{
#' \item{dfname}{name of data frame}
#' \item{nrow}{number of rows}
#' \item{ncol}{number of columns}
#' \item{overall}{data frame of overall dataset characteristics}
#' \item{qvars}{data frame with summary statistics for quantitative variables}
#' \item{cvars}{data frame with summary statistics for categorical variables}
#' }
#'
#' @details Prints a comprehensive description of a data frame via
#' several tables, a general summary table and tables that provide
#' a breakdown of quantitative and categorical variables.
#' @examples
#' contents(cars74)
#'
#' @rdname contents
#' @export

contents <- function(data, digits = 2,
                     maxcat=10, label_length=20){

  if(!(is.data.frame(data))){
    stop("You need to input a data frame")
  }

  # bind global variables to keep check from warning
  numstats <- NULL

  dataname <- deparse(substitute(data))

  results <- list(dfname=dataname, nrow=nrow(data), ncol=ncol(data),
                  overall=NULL, qvars=NULL, cvars=NULL)

  # overall summary --------------------------
  varnames <- colnames(data)
  colnames <- c("pos", "variable", "type", "n_unique",
                "n_miss", "pct_miss")

  pos = seq_along(data)
  varname <- colnames(data)
  type = sapply(data, function(x)class(x)[1])
  n_unique = sapply(data, function(x)length(unique(x)))
  n_miss = sapply(data, function(x)sum(is.na(x)))
  pct_miss = paste0(round(n_miss/nrow(data), digits) * 100, "%")
  overall <- data.frame(
    pos, varname, type, n_unique, n_miss, pct_miss
  )

  results$overall <- overall

  # numeric variables-----------------------------

  # identify numeric variables
  nindex <- sapply(data, is.numeric)

  if(any(nindex)){
    # get statistics
    numstats <- function(x){
      # bind global variables to keep check from warning
      x = stats::na.omit(x)
      n=sum(!is.na((x)))
      mean=round(mean(x), digits=digits)
      sd=round(sd(x), digits=digits)
      skew=round(skewness(x), digits=digits)
      min=round(min(x), digits=digits)
      p25=round(stats::quantile(x, 0.25)[[1]], digits=digits)
      median=round(median(x), digits=digits)
      p75=round(stats::quantile(x, 0.75)[[1]], digits=digits)
      max=round(max(x), digits=digits)
      return(c(n=n, mean=mean, sd=sd, skew=skew,
               min=min, p25=p25, median=median,
               p75=p75, max=max))
    }

    qvars <- sapply(data[nindex], numstats)
    qvars <- as.data.frame(t(qvars))

    results$qvars <- qvars
  }


  # character variables-----------------------------
  # identify character variables
  cindex <- sapply(data, function(x)is.character(x)|is.factor(x))

  if(any(cindex)){
    cdata <- data[cindex]
    cnames <- names(cdata)
    cvars <- data.frame()

    # get table
    for(i in seq_along(cdata)){
      cname <- cnames[i]
      x <- table(cdata[[i]])
      n <- as.numeric(x)
      pct <- as.numeric(n/sum(n))
      level <- as.character(dimnames(x)[[1]])
      dfc <- data.frame(
        variable = cname,
        level = level,
        n = n,
        pct = pct
      )

      # long labels
      dfc$level <- substr(dfc$level, 1, label_length)

      # many levels
      if (nrow(dfc) > maxcat){
        row_final <- data.frame(
          variable = cname,
          level=paste0("(", nrow(dfc)-maxcat, " more levels)"),
          n = sum(dfc$n[(maxcat+1):nrow(dfc)]),
          pct = sum(dfc$pct[(maxcat+1):nrow(dfc)])
        )
        dfc <- dfc[1:maxcat,]
        dfc <- rbind(dfc, row_final)
      }
      cvars <- rbind(cvars, dfc)
      cvars$pct <- round(cvars$pct, digits)
      cvars$variable[duplicated(cvars$variable)] <- " "
    }

    results$cvars <- cvars

  }
  class(results) <- c("contents")
  return(results)
}
