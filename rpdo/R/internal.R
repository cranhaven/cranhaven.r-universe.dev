get_pdo <- function() {
  pdo <- tempfile()
  download.file("http://research.jisao.washington.edu/pdo/PDO.latest", pdo)
  readLines(pdo)
}

clean_pdo <- function(pdo) {
  pdo <- pdo[grepl("(^YEAR)|(^\\d{4,4}[*]{0,2})\\s", pdo)]
  pdo <- gsub("[*]+", "", pdo)
  pdo <- gsub(" [.]", "0.", pdo)
  pdo <- gsub("[ ]+$", "", pdo)
  pdo <- gsub("[ ]+", " ", pdo)
  pdo
}

read_pdo <- function(pdo) {
  pdo <- strsplit(pdo, " ")
  pdo <- lapply(pdo, function(x) c(x, rep(NA, 13 - length(x))))
  pdo <- vapply(pdo, function(x) paste(x, collapse = ","), "")
  pdo <- paste(pdo, collapse = "\n")
  pdo <- read.csv(text = pdo)
  pdo
}

tidy_pdo <- function(pdo) {
  months <- c(
    "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
    "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
  )
  pdo <- reshape(pdo,
    varying = months, timevar = "Month", v.names = "PDO",
    idvar = "YEAR", direction = "long"
  )
  pdo$Year <- as.integer(pdo$YEAR)
  pdo <- pdo[c("Year", "Month", "PDO")]
  pdo <- pdo[!is.na(pdo$PDO), ]
  pdo <- pdo[order(pdo$Year, pdo$Month), ]
  row.names(pdo) <- NULL
  pdo
}

check_pdo <- function(pdo) {
  old_pdo <- rpdo::pdo

  check_data(pdo,
    values = list(
      Year = c(1900L, as.integer(format(Sys.Date(), "%Y"))),
      Month = c(1L, 12L),
      PDO = c(-4.1, 4.1)
    ),
    exclusive = TRUE,
    nrow = c(1406L, 2147483647L),
    order = TRUE,
    key = c("Year", "Month")
  )

  old_pdo <- merge(old_pdo, pdo, by = c("Year", "Month"), all.x = TRUE)

  if (any(is.na(old_pdo$PDO.x))) err("missing PDO index data")
  if (any(old_pdo$PDO.x != old_pdo$PDO.y)) err("incorrect PDO index data")

  if (!any(diff(pdo$Month) %in% c(1, -11))) err("missing PDO index data")
  if (!any(diff(pdo$Year) %in% c(0, 1))) err("missing PDO index data")
  pdo
}
