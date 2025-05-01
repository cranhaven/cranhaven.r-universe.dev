#' Simple Rise or Fall Trend of Several Years
#'
#' When topic names and corresponding years are given, this function computes 
#' the rise and fall trend during the period by \code{lm}.
#' 
#' The detail of trend info in the result is as follows:
#'
#' \itemize{
#'   \item (1) trendIndex: a regression with function \code{lm} is done for every
#' topic with year as x and amount of topics as y. The value of trendIndex
#' is the slope k in y = kx+b. 
#'   \item (2) trendLevel: the p value of k.
#'   \item (3) totalTrend: if trendIndex is larger than 0, then "rise", otherwise "fall".
#' If trendLevel is smaller than 0.05, than "significant rise" or "significant fall". 
#'   \item (4) maxminYear: if totalTrend is "rise" or "significant rise", then this value
#' points out which year has the largest amount. If several years have the largest 
#' value, the most recent year is returned. If totalTrend is "fall" or "significant fall", 
#' the year has the smallest amount is returned.
#'   \item (5) detailTrend: if totalTrend is "rise" or "significant rise", then the function
#' will see whether the year has the largest amount is the last year, if it is, then 
#' "rise along", otherwise "rise and fall". If totalTrend is "fall" or "significant fall", 
#' the function will see whether the year has the smallest amount is the
#' last year, if it is, then "fall along", otherwise "fall and rise".
#'   \item (6) simpleTrend: it is simply whether the amount of the last year 
#' is larger than that of the first year. If yes, then "rise", if smaller, then "fall", if 
#' the same, then "equal". 
#' }
#'
#' When computing trend for a topic, if less than 3 years has valid value and value
#' in other years are all \code{NA}, then trendIndex, trendLevel and 
#' maxminYear will be -999, and other cells are "less than 3y". If the numbers of a topic do not
#' change through years, then trendIndex will be 0, trendLevel and maxminYear will be -999, totalTrend 
#' and detailTrend will be "almost same".
#'
#' @param year a numeric vector of years for corresponding topics, 
#' if it is not numeric, the function will try to 
#' coerce. The years should be written in full-digit, that is, if they are 1998 and 2013, 
#' do not simply write 98 and 13. No \code{NA} is allowed. And, the number of 
#' unique years is at least 3, otherwise an error will be raised.
#' @param topic a character vector of topics. If it is not character, the function will
#' try to coerce. The length of topic and year should be the same. No \code{NA}
#' is allowed.
#' @param relative if \code{FALSE} (default), the numbers of topics is used. If 
#' \code{TRUE}, the percentage of a topic in a year against the total number
#' of that year is used. Suppose this year we have 200 texts on art, and the total
#' number of texts in this year is 1000, then the relative value 
#' is 200/1000 = 0.2 rather than the absolute number 200. Note: if to use
#' relative value, \code{NA} of the amount of a topic will be 
#' automatically set to 0. 
#' @param zero this can only be 0 (default) or \code{NA}. Suppose we have
#' 0 text on a certain topic, then you will make sure whether the amount 
#' is really 0, or the data of this topic in that year is missing. Set this
#' argument to \code{NA} to make all 0 into \code{NA}. 
#'
#' @return a list. The 1st element is trend info. The 2nd is a summary of amount
#' of each topic in each year. If argument relative is \code{TRUE}, a 3rd
#' element is returned, which is the relative value (percentage) of each
#' topic in each year.
#'
#' @import stats
#' @export
#' @examples
#' set.seed(1)
#' topic <- sample(c("art", "economy", "law", "politics", "sociology"), 50, replace = TRUE)
#' set.seed(2)
#' year <- sample(2011: 2016, 50, replace = TRUE)
#' tr1 <- topic_trend(year, topic)
#' tr2 <- topic_trend(year, topic, zero = NA)
#' tr3 <- topic_trend(year, topic, relative=TRUE)
topic_trend <-
function(year, topic, relative = FALSE, zero = 0) {
    infolocale <- localestart2()
    on.exit(localeend2(infolocale))
    year <- as.numeric2(year)
    topic <- as.character2(topic)
    if (any(is.na(year)) | any(is.na(topic))) 
        stop("No NA is allowed.")
    stopifnot(length(year) == length(topic))
    if (length(unique(year)) < 3) 
        stop("To compute trend needs at least 3 years.")
    stopifnot(identical(zero, 0) | identical(zero, NA))
    stopifnot(relative %in% c(TRUE, FALSE))
    minyear <- min(year)
    if (minyear < 1000) 
        message("The starting year is ", minyear, ". If you are sure the input years are OK, it is OK.")
    m_table <- as.matrix(table(year, topic))
    orn <- order(rownames(m_table))
    ocn <- order(colnames(m_table))
    m_table <- m_table[orn, ]
    m_table <- m_table[, ocn]
    m_table[m_table == 0] <- zero
    if (relative) {
        m_table2 <- m_table
        if (any(is.na(m_table))) 
            message("NAs are set to 0 to compute relative trend.")
        m_table[is.na(m_table)] <- 0
        rElAtIvE_pEr <- function(xs) round(xs/sum(xs), 5)
        m_table <- t(apply(m_table, 1, rElAtIvE_pEr))
    }
    year <- as.numeric(row.names(m_table))
    all_info <- matrix(NA, nrow = ncol(m_table), ncol = 6)
    for (i in 1:ncol(m_table)) {
        topic_num <- m_table[, i]
        is_na <- is.na(topic_num)
        topic_num <- topic_num[!is_na]
		yy <- year[!is_na]
        if (length(topic_num) < 3) {
            info <- c(-999, -999)
        } else {
            mod <- stats::lm(topic_num ~ yy)
            info <- tryCatch(
				expr = {
					sm <- summary(mod)
					smc <- sm$coefficients
					k <- smc[2, 1]
					p <- smc[2, 4]
					round(c(k, p), 4)
				}, warning = function(w) {
					wchar <- as.character(w)
					sm <- suppressWarnings(expr = {summary(mod)})
					smc <- sm$coefficients
					k <- smc[2, 1]
					p <- smc[2, 4]
					if (grepl("essentially perfect fit", wchar)){
						if (abs(k) > 0.9){
							return(round(c(k, p), 4))
						} else {
							return(c(0, -999))
						}
					} else {
						warning(wchar)
						return(round(c(k, p), 4))
					}
				}
			)
        }
		if (info[1] == -999 & info[2] == -999) {
            total_trend <- "less than 3y"
            which_year_mm <- -999
            detail_trend <- "less than 3y"
            simple_trend <- "less than 3y"
        } else if (info[1] == 0 & info[2] == -999) {
            total_trend <- "almost same"
            which_year_mm <- -999
            detail_trend <- "almost same"
            ta <- utils::tail(topic_num, 1)
            he <- utils::head(topic_num, 1)
            simple_trend <- ifelse(ta > he, "rise", ifelse(ta < he, "fall", "equal"))
		} else if (info[1] > 0){
            total_trend <- ifelse(info[2] < 0.05, "significant rise", "rise")
            which_mm <- which(topic_num == max(topic_num))
            which_year_mm <- max(yy[which_mm])
            detail_trend <- ifelse(which_year_mm == tail(yy, 1), "rise along", "rise and fall")
            ta <- utils::tail(topic_num, 1)
            he <- utils::head(topic_num, 1)
            simple_trend <- ifelse(ta > he, "rise", ifelse(ta < he, "fall", "equal"))
        } else if (info[1] < 0) {
            total_trend <- ifelse(info[2] < 0.05, "significant fall", "fall")
            which_mm <- which(topic_num == min(topic_num))
            which_year_mm <- max(yy[which_mm])
            detail_trend <- ifelse(which_year_mm == tail(yy, 1), "fall along", "fall and rise")
            ta <- utils::tail(topic_num, 1)
            he <- utils::head(topic_num, 1)
            simple_trend <- ifelse(ta > he, "rise", ifelse(ta < he, "fall", "equal"))
        }
        info <- c(info, total_trend, which_year_mm, detail_trend, simple_trend)
        all_info[i, ] <- info
    }
    colnames(all_info) <- c("trendIndex", "trendLevel", "totalTrend", "maxminYear", "detailTrend", "simpleTrend")
    all_info <- data.frame(all_info, stringsAsFactors = FALSE)
    all_info[, 1] <- as.numeric(all_info[, 1])
    all_info[, 2] <- as.numeric(all_info[, 2])
    all_info[, 4] <- as.numeric(all_info[, 4])
    rownames(all_info) <- colnames(m_table)
    if (relative) {
        message("relative trend")
        to_return <- list(trend = all_info, absoluteFreq = m_table2, relativeFreq = m_table)
    } else {
        message("absolute trend")
        to_return <- list(trend = all_info, absoluteFreq = m_table)
    }
	return(to_return)
}
