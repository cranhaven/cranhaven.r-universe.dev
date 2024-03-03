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
cranhaven <- cranhaven[order(cranhaven$package), ]
message("Number of CRAN packages archived during the last four weeks: ", nrow(cranhaven))

## Make sure they haven't been reintroduced on CRAN
cran_pkgs <- unname(available.packages(repos = "https://cloud.r-project.org")[, "Package"])
message("Number of CRAN packages: ", length(cran_pkgs))
cranhaven <- subset(cranhaven, !package %in% cran_pkgs)
stopifnot(!any(duplicated(cranhaven$package)))
message("Number of CRANhaven packages: ", nrow(cranhaven))

## Update package subfolders
path <- "packages"
if (!utils::file_test("-d", path)) dir.create(path)
setwd(path)

## Drop package folders for packages no longer on CRANhaven
pkgs <- setdiff(dir(), cranhaven$package)
unlink(pkgs, recursive = TRUE, force = TRUE)
message("Number of CRANhaven packages to remove: ", length(pkgs))

## Clone to package subfolders, if not already done
pkgs <- setdiff(cranhaven$package, dir())
pkgs <- NULL  ## FIXME
message("Number of CRANhaven packages to add: ", length(pkgs))
failed <- c()
for (pkg in pkgs) {
  message("Cloning package ", sQuote(pkg))
  pp <- file.path(pkg, ".git")
  url <- paste0("https://github.com/cran/", pkg)
  res <- system2("git", args = c("clone", "--depth=1", url))
  if (res != 0) failed <- c(failed, pkg)
  unlink(file.path(pkg, ".git"), recursive = TRUE, force = TRUE)
  res <- system2("git", args = c("add", pkg))
  if (res != 0) {
    unlink(pkg, recursive = TRUE, force = TRUE)
    failed <- c(failed, pkg)
  }
}

pkgs <- cranhaven$package
pkgs <- NULL  ## FIXME
repo <- "https://cranhaven.r-universe.org"
for (pkg in pkgs) {
  field <- "Additional_repositories"
  file <- file.path(pkg, "DESCRIPTION")
  desc <- read.dcf(file)
  if (field %in% colnames(desc)) {
    repos <- desc[,field]
    if (!grepl(repo, repos)) {
      repos <- paste(c(repos, repo), collapse = ",\n")
      desc[,field] <- repos
    }
  } else {
    repos <- matrix(repo, ncol = 1L)
    colnames(repos) <- field
    desc <- cbind(desc, repos)
  }
  write.dcf(desc, file = file)
}

if (length(failed) > 0) {
  stop(sprintf("Failed to clone %d package(s): %s", length(failed), paste(sQuote(failed), collapse = ", ")))
}

## Assert that all packages where cloned
##stopifnot(identical(sort(cranhaven$package), sort(dir())))
setwd("..")

## Write packages.json for R-universe 
cranhaven$url <- with(cranhaven, file.path("https://github.com/cran", package))
#cranhaven$subdir <- with(cranhaven, file.path("packages", package))
jsonlite::write_json(cranhaven, "packages.json", pretty = TRUE)
message("packages.json written")
