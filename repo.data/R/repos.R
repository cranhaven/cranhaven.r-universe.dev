
#' Package dependencies to repositories
#'
#' Explore the relationships between packages and repositories available.
#' @inheritParams tools::package_dependencies
#' @param repos Repositories and their names are taken from `getOptions("repos")`.
#'
#' @returns A data.frame with one line per package and at least one column per
#' repository. It also has a column for Other repositories (Additional_repositories,
#' or missing repositories), and the total number of dependencies and total
#' number of repositories used.
#' `NA` if not able to collect the data from repositories.
#' @export
#' @family utilities
#' @examples
#' pr <- package_repos("experDesign")
#' head(pr)
package_repos <- function(packages = NULL, repos = getOption("repos"), which = "all") {
    stopifnot(is.character(repos) && length(repos))
    check_packages(packages, length = NA)

    which <- check_which(which)
    ap <- tryCatch(available.packages(repos = repos, filters = c("CRAN", "duplicates")),
                   warning = function(w){NA})
    if (is_not_data(ap)) {
        return(NA)
    }

    # Check packages
    repos_packages <- setdiff(packages, BASE)
    omit_pkg <- setdiff(packages, rownames(ap))
    if (length(omit_pkg)) {
        warning("Omitting packages ", toString(omit_pkg),
                ".\n Maybe they are currently not available?",
                immediate. = TRUE, call. = FALSE)
    }
    if (is.null(packages)) {
        packages <- rownames(ap)
    } else {
        packages <- intersect(packages, rownames(ap))
    }

    # Get the repo where each package comes from
    repositories <- gsub("/src/contrib", "", ap[, "Repository"], fixed = TRUE)
    names(repositories) <- rownames(ap)
    repositories[] <- names(repos)[match(repositories, repos)]

    # Get the direct dependencies for each package
    rd <- repos_dependencies(packages, which)

    pd2 <- rd[!rd$Name %in% c(BASE, "R"), c("Name", "Package")]

    pd2$Repo <- repositories[pd2$Name]
    pd2$Repo[is.na(pd2$Repo)] <- "Other"

    # Prefill matrix
    M <- matrix(0, ncol = length(repos) + 1L, nrow = NROW(ap))
    colnames(M) <- c(names(repos), "Other")
    rownames(M) <- rownames(ap)

    # Count repositories
    s <- split(pd2$Repo, pd2$Package)
    l <- lapply(s, function(pkg) {
        tab <- table(factor(pkg, levels = c(names(repos), "Other")))
        as.matrix(tab)
    })
    deps_m <- do.call(cbind, l)
    deps_m <- t(deps_m)
    rownames(deps_m) <- names(s)
    M[rownames(deps_m), ] <- deps_m[, colnames(M)]

    repos_n <- apply(M, 1L, function(x){sum(x > 0L)})
    deps_n <- rowSums(M)
    M2 <- cbind(M, Packages_deps = deps_n, Repos = repos_n)
    df2 <- as.data.frame(M2)
    # bioc_deps <- rowSums(M2[, 2:6])
    df3 <- cbind(Package = rownames(df2), Repository = repositories, df2)
    df3 <- df3[packages, , drop = FALSE]
    rownames(df3) <- NULL
    df3
}

