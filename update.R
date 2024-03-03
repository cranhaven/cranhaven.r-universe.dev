## Find archived packages and when they were archived
bfr <- readLines("https://dirk.eddelbuettel.com/cranberries/cran/removed/index.rss")

pattern <- ".*<title>Package ([^ ]+) .*<[/]title>.*"
pkgs <- grep(pattern, bfr, value = TRUE)
pkgs <- sub(pattern, "\\1", pkgs)
pattern <- ".*<pubDate>([^<]+)<[/]pubDate>.*"
dates <- grep(pattern, bfr, value = TRUE)
dates <- sub(pattern, "\\1", dates)
stopifnot(length(pkgs) == length(dates))

tzs <- sub(".* ([[:alpha:]]+)$", "\\1", dates)
tz <- tzs[1]
stopifnot(all(tz == tzs))
timestamps <- strptime(dates, format = "%a, %d %b %Y %H:%M:%S", tz = tz)

## Packages archived within the last four weeks should be on CRANhaven
cranhaven <- data.frame(package = pkgs, archived_on = timestamps)
cranhaven <- subset(cranhaven, archived_on >= Sys.time() - 4*7*24*3600)

## Make sure they haven't been reintroduced on CRAN
cran_pkgs <- unname(available.packages(repos = "https://cloud.r-project.org")[, "Package"])
cranhaven <- subset(cranhaven, !package %in% cran_pkgs)
stopifnot(!any(duplicated(cranhaven$package)))

## Write packages.json for R-universe 
cranhaven$url <- with(cranhaven, paste0("https://github.com/cran/", package))
jsonlite::write_json(cranhaven, "packages.json", pretty = TRUE)
