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

## Get current CRAN packages
cran_pkgs <- unname(available.packages(repos = "https://cloud.r-project.org")[, "Package"])

github_repo <- "https://github.com/cranhaven/cranhaven.r-universe.dev"
runiverse_repo <- "https://cranhaven.r-universe.dev"

## Packages archived within the last five weeks should be on CRANhaven
cranhaven <- data.frame(package = pkgs, on_cran = (pkgs %in% cran_pkgs), archived_on = timestamps, url = github_repo, branch = file.path("package", pkgs), subdir = pkgs)
cranhaven <- subset(cranhaven, archived_on >= Sys.time() - 5*7*24*3600)
cranhaven <- cranhaven[order(cranhaven$package), ]
cranhaven_all <- cranhaven
message("Number of CRAN packages archived during the last five weeks: ", nrow(cranhaven_all))

cranhaven <- subset(cranhaven_all, !on_cran)
rownames(cranhaven) <- NULL
message("Number of CRAN packages archived during the last five weeks that are still not on CRAN: ", nrow(cranhaven))

## Returns status = 128 if no such branches exist, which is okay
message("Checkout main branch")
res <- system2("git", args = c("checkout", "main"))

message("Listing all package branches")
branches <- system2("git", args = c("branch", "--all", "--list", shQuote('*/package/*')), stdout = TRUE, stderr = TRUE)
branches <- sub("^[* ]+.*package/", "package/", branches)
message(sprintf("Branches: [n=%d] %s", length(branches), paste(branches, collapse = ", ")))

failed <- c()
for (kk in seq_len(nrow(cranhaven))) {
  entry <- cranhaven[kk, ]
  pkg <- entry$package
  message(sprintf("%d/%d Package %s", kk, nrow(cranhaven), sQuote(pkg)))
  
  branch <- entry$branch
  message(" - Branch: ", branch)

  ## Already done?
  if (branch %in% branches) next

  message(" - Checkout main branch")
  res <- system2("git", args = c("checkout", "main"))

  ## Create empty package branch?
  message(" - Create branch")
  output <- system2("git", args = c("checkout", "--orphan", branch), stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status")
  if (!is.null(status)) {
    failed <- c(failed, pkg)
    next
  }

  message(" - Erase branch")
  res <- system2("git", args = c("rm", "-rf", "."))
  if (res != 0) {
    failed <- c(failed, pkg)
    next
  }

  message(" - Clone package")
  url <- paste0("https://github.com/cran/", pkg)
  res <- system2("git", args = c("clone", "--depth=1", url, pkg))
  if (res != 0) {
    failed <- c(failed, pkg)
    next
  }
  
  message(" - Prune package")
  unlink(file.path(pkg, ".git"), recursive = TRUE, force = TRUE)
  res <- system2("git", args = c("add", pkg))
  if (res != 0) {
    failed <- c(failed, pkg)
    next
  }

  message(" - Update Additional_repositories")
  field <- "Additional_repositories"
  file <- file.path(pkg, "DESCRIPTION")
  desc <- desc0 <- read.dcf(file)
  if (field %in% colnames(desc)) {
    repos <- desc[,field]
    if (!grepl(runiverse_repo, repos)) {
      repos <- paste(c(repos, runiverse_repo), collapse = ",\n")
      desc[,field] <- repos
    }
  } else {
    repos <- matrix(runiverse_repo, ncol = 1L)
    colnames(repos) <- field
    desc <- cbind(desc, repos)
  }
  
  if (!identical(desc, desc0)) {
    write.dcf(desc, file = file)
  }
  
  message(" - Commit package")
  when <- entry$archived_on
  when <- format(when, format = "%F %T %z")
  env <- paste0(c("GIT_AUTHOR_DATE=", "GIT_COMMITTER_DATE="), shQuote(when))
  msg <- sprintf("Add %s to CRANhaven, because archived on %s", pkg, when)
  output <- system2("git", args = c("commit", "-a", "-m", shQuote(msg)), env = env, stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status")
  if (!is.null(status)) {
    print(status)
    print(output)
    failed <- c(failed, pkg)
    stop("Failed")
    next
  }

  message(" - Push branch")
  res <- system2("git", args = c("push", "--set-upstream", "origin", branch))
  if (res != 0) {
    failed <- c(failed, pkg)
    next
  }

  message("-- Checkout main branch")
  res <- system2("git", args = c("checkout", "main"))
} ## for (kk in ...) 

if (length(failed) > 0) {
  stop(sprintf("Failed to create branches for %d package(s): %s", length(failed), paste(sQuote(failed), collapse = ", ")))
}

## Assert that all packages where cloned
#stopifnot(identical(sort(cranhaven$package), sort(dir())))

## Identify diff
pkgs <- cranhaven$package
pkgs_prev <- vapply(jsonlite::read_json("packages.json"), FUN = function(x) x$package, FUN.VALUE = NA_character_)
diff <- list(add = setdiff(pkgs, pkgs_prev), remove = setdiff(pkgs_prev, pkgs))

if (sum(lengths(diff)) > 0) {
  msg <- "CRANhaven updates:"
  for (what in names(diff)) {
    if (length(diff[[what]]) > 0) {
      msg <- c(msg, sprintf("%s %s.", tools::toTitleCase(what), paste(sQuote(diff[[what]]), collapse = ", ")))
    }
  }
  msg <- paste(msg, collapse = " ")
  message(msg)

  ## Write packages.json for R-universe 
  jsonlite::write_json(cranhaven, "packages.json", pretty = TRUE)
  message("packages.json written")

  message("Commit packages.json updates")
  system2("git", args = c("commit", "-a", "-m", shQuote(msg)))

  message("Push updates")
  system2("git", args = c("push"))
} else {
  message("Nothing changed")
}
