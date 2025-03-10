dryrun <- FALSE

#' @export
capitalize <- function(x) {
  vapply(x, FUN.VALUE = NA_character_, FUN = function(s) {
    if (is.na(s) || nchar(s) == 0) return(s)
    first <- substr(s, start = 1L, stop = 1L)
    tail <- substr(s, start = 2L, stop = nchar(s))
    paste0(toupper(first), tail)
  })
} ## capitalize()

#' @export
read_cranberries_removed <- local({
  data <- NULL
  
  function(rss = "https://dirk.eddelbuettel.com/cranberries/cran/removed/index.rss") {
    if (!is.null(data)) return(data)
    
    ## Find archived packages and when they were archived
    bfr <- readLines(rss)
  
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
  
    db <- data.frame(package = pkgs, archived_on = timestamps)

    ## Drop old duplicates
    db <- db[order(db$archived_on, decreasing = TRUE), ]
    db <- subset(db, !duplicated(db$package))
    
    ## Sanity check
    stopifnot(!any(duplicated(db$package)))

    data <<- db
    
    data
  }
}) ## read_cranberries_removed()


#' @export
cran_events <- local({
  data <- NULL
  
  function(url = "https://cran.r-project.org/src/contrib/PACKAGES.in") {
    if (!is.null(data)) return(data)

    con <- url(url)
    db <- read.dcf(con)

    ## Check for typos and spelling errors in column names
    ## Drop unknown column names and drop missing-value package names
    ## that might follow from it
    known_names <- c("Package", "X-CRAN-Comment", "X-CRAN-History", "Replaced_by", "Additional_repositories", "Maintainer", "License_restricts_use", "SystemRequirements", "LazyDataCompression", "License_is_FOSS", "OS_type", "URL")
    unknown <- setdiff(colnames(db), known_names)
    str(unknown)
    if (length(unknown) > 0) {
      warning(sprintf("Detected unknown fields in <%s>: %s", url, paste(sQuote(unknown), collapse = ", ")), immediate. = TRUE)
      db <- db[, known_names, drop = FALSE]
      db <- db[!is.na(db[, "Package"]), ]
    }
    db <- as.data.frame(db)

    ## Assert assumption about one entry per package
    dups <- db$Package[duplicated(db$Package)]
    if (length(dups) > 0) {
      warning(sprintf("Detected duplicated package entries in %s: [n=%d] %s", url, length(dups), paste(sQuote(dups), collapse = ", ")), immediate. = TRUE)
      for (pkg in dups) {
        idxs <- which(db$Package == pkg)
        db_pkg <- lapply(db[idxs,], function(x) {
          x <- unique(na.omit(x))
          if (length(x) == 0) x <- NA_character_
          x
        })
        db[idxs[1], ] <- db_pkg
        db <- db[-idxs[-1], ]
      }
      stopifnot(!any(duplicated(db$Package)))
    }

    names <- colnames(db)
    names <- tolower(names)
    names <- gsub("-", "_", names)
    colnames(db) <- names

    db_active <- subset(db, !is.na(x_cran_comment))
    db_legacy <- subset(db,  is.na(x_cran_comment))

    ## Extract date and type of event
    ## Note: Be forgiving for typos like "2016-07-1"
    pattern <- ".*([[:digit:]]{4,4}-[[:digit:]]{1,2}-[[:digit:]]{1,2}).*"
    dates <- rep(as.Date(NA_integer_), times = nrow(db_active))
    idxs <- grep(pattern, db_active$x_cran_comment)
    values <- sub(pattern, "\\1", db_active$x_cran_comment[idxs])
    values <- as.Date(values)
    dates[idxs] <- values
    db_active$x_cran_comment_date <- dates

    pattern <- "^([Uu]narchived?|[Aa]rchived?|[Rr]enamed?|[Oo]rphaned?|[Rr]eplaced?|[Rr]emoved?)"
    events <- gsub("[[:blank:]:].*", "", db_active$x_cran_comment)
    events <- tolower(events)
    events[events == "archive"] <- "archived"
    events[!events %in% c("archived", "orphaned", "removed", "renamed", "replaced")] <- NA_character_
    db_active$x_cran_comment_event <- events

    reasons <- rep(NA_character_, times = nrow(db_active))
    idxs <- which(!is.na(events))
    values <- gsub("^.* on [[:digit:]-]+[[:blank:]]*(|,|;|:|as|at)([[:blank:]]+|[.])", "", db_active$x_cran_comment[idxs])
    ## Anonymize email addresses
    values <- sub("[[:blank:]]+<[^>@]+@[^>@]+>", "", values)
    values <- gsub("[[:blank:]]+", " ", values)
    values <- gsub("[\n]+", " ", values)
    values <- gsub("(^[[:blank:]]+|[[:blank:].]+$)", "", values)
    values <- gsub("[.] , ([A-Z])", ". \\1", values)
    values <- gsub(". , ", ", ", values)
    values <- gsub("^for ", "", values)
    values <- gsub("^the maintainer's ", "maintainer's ", values, fixed = TRUE)
    values <- capitalize(values)
    reasons[idxs] <- values
    db_active$x_cran_comment_reason <- reasons

    db_legacy$x_cran_comment_date <- as.Date(NA_integer_)
    db_legacy$x_cran_comment_event <- NA_character_
    db_legacy$x_cran_comment_reason <- NA_character_

    db <- rbind(db_legacy, db_active)

    ## Anonymize email addresses
    values <- db$x_cran_history
    values <- sub("[[:blank:]]+<[^>@]+@[^>@]+>", "", values)
    db$x_cran_history <- values

    cols <- c("package", "x_cran_comment_date", "x_cran_comment_event", "x_cran_comment_reason", "x_cran_comment", "x_cran_history", "replaced_by", "additional_repositories", "maintainer", "license_restricts_use", "systemrequirements", "lazydatacompression", "license_is_foss", "os_type", "url")
    db <- db[, cols]
    db <- db[order(db$x_cran_comment_date, decreasing = TRUE), ]
    
    data <<- db
  }
}) ## cran_events()


#' @export
cran_current <- local({
  data <- NULL
  function() {
    if (!is.null(data)) return(data)
    db <- tools:::CRAN_current_db()
    tarballs <- rownames(db)
    pkgs <- gsub("_.*", "", tarballs)
    data <<- cbind(data.frame(package = pkgs, tarball = tarball), db)
  }
}) ## cran_current()


#' @export
cran_archived <- local({
  data <- NULL
  function() {
    if (!is.null(data)) return(data)
    data <<- tools:::CRAN_archive_db()
  }
}) ## cran_archived()


#' @importFrom jsonlite read_json
#' @export
cranhaven_pkgs <- local({
  ## Manual imports for now
  read_json <- jsonlite::read_json
  
  data <- NULL
  
  function(url = "https://raw.githubusercontent.com/cranhaven/cranhaven.r-universe.dev/main/packages.json") {
    if (!is.null(data)) return(data)
    data <<- read_json(url)
    data
  }
}) ## cranhaven_pkgs()


cran_pkg_description <- local({
  db <- list()
  function(pkg) {
    desc <- db[[pkg]]
    if (!is.null(desc)) return(desc)
    url <- sprintf("https://raw.githubusercontent.com/cran/%s/master/DESCRIPTION", pkg)
    desc <- tryCatch(read.dcf(url(url)), error = function(ex) list())
    db[[pkg]] <<- desc
    desc
  }
}) ## cran_pkg_description()


cran_pkg_annotations <- function(pkgs) {
  desc <- lapply(pkgs, FUN = cran_pkg_description)
  desc <- lapply(desc, FUN = function(d) {
    res <- data.frame(Package = NA_character_, URL = NA_character_, BugReports = NA_character_, Maintainer = NA_character_)
    names <- intersect(colnames(d), names(res))    
    for (name in names) res[[name]] <- d[, name]
    res
  })
  desc <- do.call(rbind, desc)
#  desc$Maintainer <- sub("[[:space:]]*<.*", "", desc$Maintainer)
#  desc$Maintainer <- gsub("(^\"|\"$)", "", desc$Maintainer)
  colnames(desc) <- tolower(colnames(desc))
  colnames(desc)[2:3] <- paste0("package_", colnames(desc)[2:3])
  desc
} ## cran_pkg_annotations()


#' @export
call_git <- function(cmd = c("add", "branch", "checkout", "clone", "commit", "push", "rm"), ..., env = character(), stdout = FALSE, stderr = FALSE) {
  cmd <- match.arg(cmd)
  if (dryrun) return(0L)
  args <- c(cmd, ...)
  res <- system2("git", args = args, env = env, stdout = stdout, stderr = stderr)
  res
} ## call_git()


main <- function() {
  ## Find archived packages and when they were archived
  message("Querying CRANberries removed")
  removed <- read_cranberries_removed()
  
  ## Get current CRAN packages
  message("Querying CRAN for available packages")
  cran_pkgs <- unname(available.packages(repos = "https://cloud.r-project.org")[, "Package"])
  
  ## Get archived CRAN packages
  message("Querying CRAN for archived packages")
  cran_archived <- tools:::CRAN_archive_db()
  cran_archived_pkgs <- names(cran_archived)
  
  github_repo <- "https://github.com/cranhaven/cranhaven.r-universe.dev"
  runiverse_repo <- "https://cranhaven.r-universe.dev"
  
  message("Querying CRAN for package events")
  events <- cran_events()
  events <- subset(events, package %in% removed$package)
  events <- events[, c("package", "x_cran_comment_date", "x_cran_comment_event", "x_cran_comment_reason", "x_cran_history")]
  
  ## Packages archived within the last five weeks should be on CRANhaven
  cranhaven <- data.frame(package = removed$package, on_cran = (removed$package %in% cran_pkgs), archived_on = removed$archived_on, url = github_repo, branch = file.path("package", removed$package), subdir = removed$package)
  cranhaven <- subset(cranhaven, archived_on >= Sys.time() - 5*7*24*3600)
  cranhaven <- cranhaven[order(cranhaven$package), ]
  cranhaven <- merge(cranhaven, events, by = "package")
  cranhaven_all <- cranhaven
  message("Number of CRAN packages archived during the last five weeks: ", nrow(cranhaven_all))
  
  cranhaven <- subset(cranhaven_all, !on_cran)
  rownames(cranhaven) <- NULL
  message("Number of CRAN packages archived during the last five weeks that are still not on CRAN: ", nrow(cranhaven))
  stopifnot(!any(duplicated(cranhaven$package)))
  
  ## Returns status = 128 if no such branches exist, which is okay
  message("Checkout main branch")
  res <- call_git("checkout", args = "main")
  
  message("Listing all local or remote package branches")
  branches <- call_git("branch", c("--all", "--list", shQuote('*/package/*')), stdout = TRUE, stderr = TRUE)
  branches <- sub("^[* ]+.*package/", "package/", branches)
  message(sprintf("Branches: [n=%d] %s", length(branches), paste(branches, collapse = ", ")))

  message("Adding newly archived packages")
  pkgs <- setdiff(cranhaven$package, basename(branches))
  message(sprintf(" - packages to add: [n=%d] %s", length(pkgs), paste(pkgs, collapse = ", ")))
  failed <- c()
  for (kk in seq_along(pkgs)) {
    pkg <- pkgs[kk]
    message(sprintf("%d/%d Package %s", kk, length(pkgs), sQuote(pkg)))
    ee <- match(pkg, cranhaven$package)
    entry <- cranhaven[ee, ]
    stopifnot(entry$package == pkg)
    
    branch <- entry$branch
    message(" - Branch: ", branch)
  
    message(" - Checkout main branch")
    res <- call_git("checkout", "main")
  
    ## Create empty package branch?
    message(" - Create branch")
    output <- call_git("checkout", "--orphan", branch, stdout = TRUE, stderr = TRUE)
    status <- attr(output, "status")
    if (!is.null(status)) {
      failed <- c(failed, pkg)
      next
    }
  
    message(" - Erase branch")
    res <- call_git("rm", "-rf", ".")
    if (res != 0) {
      failed <- c(failed, pkg)
      next
    }
  
    message(" - Clone package")
    url <- paste0("https://github.com/cran/", pkg)
    res <- call_git("clone", "--depth=1", url, pkg)
    if (res != 0) {
      failed <- c(failed, pkg)
      next
    }
    
    message(" - Prune package")
    unlink(file.path(pkg, ".git"), recursive = TRUE, force = TRUE)
    res <- call_git("add", pkg)
    if (res != 0) {
      failed <- c(failed, pkg)
      next
    }
  
    message(" - Update Additional_repositories")
    if (!dryrun) {
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
    }
    
    message(" - Commit package")
    when <- entry$archived_on
    when <- format(when, format = "%F %T %z")
    env <- paste0(c("GIT_AUTHOR_DATE=", "GIT_COMMITTER_DATE="), shQuote(when))
    msg <- sprintf("Add %s to CRANhaven, because archived on %s", pkg, when)
    output <- call_git("commit", "-a", "-m", shQuote(msg), env = env, stdout = TRUE, stderr = TRUE)
    status <- attr(output, "status")
    if (!is.null(status)) {
      print(status)
      print(output)
      failed <- c(failed, pkg)
      stop("Failed")
      next
    }
  
    message(" - Push branch")
    res <- call_git("push", "--set-upstream", "origin", branch)
    if (res != 0) {
      failed <- c(failed, pkg)
      next
    }
  
    message("-- Checkout main branch")
    res <- call_git("checkout", "main")
  } ## for (kk in ...) 
  
  if (length(failed) > 0) {
    stop(sprintf("Failed to create branches for %d package(s): %s", length(failed), paste(sQuote(failed), collapse = ", ")))
  }


  message("Removing expired and unarchived packages")
  pkgs <- setdiff(basename(branches), cranhaven$package)
  message(sprintf(" - packages to remove: [n=%d] %s", length(pkgs), paste(pkgs, collapse = ", ")))
  message(" - Checkout main branch")
  res <- call_git("checkout", "main")
  failed <- c()
  for (kk in seq_along(pkgs)) {
    pkg <- pkgs[kk]
    message(sprintf("%d/%d Package %s", kk, length(pkgs), sQuote(pkg)))
    bb <- match(pkg, basename(branches))
    branch <- branches[bb]
    message(" - Branch: ", branch)
  
    message(" - Delete remote branch")
    res <- call_git("push", "origin", "--delete", branch)
    if (res != 0) {
      message(" - failed")
      failed <- c(failed, pkg)
    }
    
    message(" - Delete local branch")
    res <- call_git("branch", "-D", branch)
    if (res != 0) {
      message(" - failed")
      failed <- c(failed, pkg)
    }
  } ## for (kk in ...) 
    
  if (length(failed) > 0) {
    failed <- unique(failed)
    warning(sprintf("Failed to delete branches for %d package(s): %s", length(failed), paste(sQuote(failed), collapse = ", ")), immediate. = TRUE)
  }

  message("Querying CRAN package descriptions")
  annotations <- cran_pkg_annotations(cranhaven$package)
  cranhaven <- merge(cranhaven, annotations, by = "package")
  
  ## Identify diff
  pkgs <- cranhaven$package
  pkgs_prev <- vapply(cranhaven_pkgs(), FUN = function(x) x$package, FUN.VALUE = NA_character_)
  pkgs_removed <- setdiff(setdiff(pkgs, pkgs_prev), cran_archived_pkgs)
  diff <- list(
    removed    = pkgs_removed,
    archived   = setdiff(setdiff(pkgs, pkgs_prev), pkgs_removed),
    unarchived = intersect(setdiff(pkgs_prev, pkgs), cran_pkgs),
    expired    = setdiff(setdiff(pkgs_prev, pkgs), cran_pkgs)
  )
  str(diff)
  
  if (sum(lengths(diff)) > 0) {
    msg <- "CRAN updates:"
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
    call_git("commit", "-a", "-m", shQuote(msg))
  
    message("Push updates")
    call_git("push")
  } else {
    message("Nothing changed")
  }
} ## main()

main()
