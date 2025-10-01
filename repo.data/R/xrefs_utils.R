xrefs2df <- function(x) {
    if (!length(x)) {
        return(NULL)
    }

    rdxrefsDF <- do.call(rbind, x)
    rdxrefsDF <- cbind(rdxrefsDF, Package = rep(names(x), vapply(x, NROW, numeric(1L))))
    rownames(rdxrefsDF) <- NULL
    rdxrefsDF[, c("Package", "Source", "Anchor", "Target"), drop = FALSE]
}

#' Resolve links
#'
#' Converts Anchors and targets so that it can be easily understood.
#' See [WRE](https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Cross_002dreferences)
#' for extensive explanations
#'
#' There are 4 different types of links:
#' - `{Target}`
#' - `[=Target]{name}`
#' - `[package]{Target}`
#' - `[package:target]{name}`
#' The first two can be to any package and led to disambiguation pages, the last
#' two are fully resolved (package and alias)
#' @param links A data.frame with Package, Source, Anchor and Target.
#' @param count A logical value if links should be counted.
#' @seealso [targets2files()]
#'
#' @returns A data.frame with Package, Source, to_pkg, to_target, n (number of times it happens)
#' @keywords internal
split_anchor <- function(links, count = TRUE) {
    links_targets <- strcapture("([[:alnum:].]*{2,})?[:=]?(.*)",
                                x = links[, "Anchor"],
                                proto = data.frame(to_pkg = character(),
                                                   to_target = character()))
    link_w_targets <- cbind(links, as.matrix(links_targets))

    # To [=Target]{name}: [=detect_as]{detect_as()}
    anchor <- link_w_targets[, "Anchor"]
    w_anchor <- nzchar(anchor)
    eq_anchor <- startsWith(anchor, "=")
    full_anchor <- grepl(":", anchor, fixed = TRUE)
    missing_target <-  w_anchor & !eq_anchor & !full_anchor
    link_w_targets[missing_target, "to_target"] <- link_w_targets[missing_target, "Target"]

    # To [package]{Target}: [rmarkdown]{render}
    pkg_anchor <- w_anchor & !full_anchor & !eq_anchor
    link_w_targets[pkg_anchor, "to_target"] <- link_w_targets[pkg_anchor, "Target"]

    # To [package:target]{name}: [stats:optim]{stats}
    # known_res <- w_anchor & full_anchor
    # Nothing to be done, xrefs2df have this covered

    # name is target.
    missing_target <- !nzchar(link_w_targets$to_target)
    link_w_targets[missing_target, "to_target"] <- link_w_targets[missing_target, "Target"]

    sort_order <- intersect(c("Package", "Source", "to_pkg", "to_target"), colnames(link_w_targets))
    l2t <- link_w_targets[, sort_order]
    l2t <- sort_by(l2t, l2t)
    if (NROW(l2t) && count) {
        uniq_count(l2t)
    } else {
        unique(l2t)
    }
}

self_refs <- function(refs) {
    same <- refs$Rd_origin == refs$Rd_destiny & refs$from_pkg == refs$to_pkg
    unique(refs[same, c("Rd_origin", "from_pkg", "Target")])
}

#' Resolves missing targets
#'
#' Resolves links that require to know available alias so solve them.
#' @param links The output of [split_anchor()].
#' @param alias The output of [alias2df()] as data.frame.
#'
#' @returns A data.frame with to_pkg, to_target, from_pkg, from_Rd, n, to_Rd.
#' @keywords internal
targets2files <- function(links, alias) {

    to_pkg <- links[, "to_pkg"]
    to_target <- links[, "to_target"]
    # Packages without resolved target
    w_pkg <- nzchar(to_pkg) | is.na(to_pkg)

    # No duplicated alias
    da <- dup_alias(alias)
    dalias <- unique(da$Target)
    targets_nodup <- !w_pkg & !to_target %in% dalias
    match_target <- match(to_target[targets_nodup], alias[, "Target"])
    links[targets_nodup, "to_pkg"] <- alias[match_target, "Package"]
    links[is.na(links[, "to_pkg"]), "to_pkg"] <- ""

    # Duplicated alias
    targets_dup <- !w_pkg & to_target %in% dalias
    # dtrMatrix-class-dense.Rd: ?Matrix::`dtrMatrix-class` has a link to Ops
    # which then open a disambiguation page for methods::Ops or base::Ops.
    # The resolution will depend on which packages are installed!!

    # Adding duplicated Targets/topics at other packages but present on the package
    # The present on the package takes precedence
    if (any(targets_dup)) {
        z_keep <- targets_dup & !nzchar(links[, "to_pkg"])
        to <- paste0(links[z_keep, "Package"], ":", links[z_keep, "to_target"])
        received <- paste0(da[, "Package"], ":", da[, "Target"])
        selfs <- to %in% received
        links[z_keep, "to_pkg"][selfs] <- links$Package[z_keep][selfs]
    }

    links$tmp <- seq_len(NROW(links))
    links_w_files <- merge(links, alias,
                           by.x = c("to_pkg", "to_target"),
                           by.y = c("Package", "Target"),
                           all.x = TRUE, sort = FALSE)
    # Dealing with links that are different per OS.
    path_x <- grep("/", links_w_files$Source.x, fixed = TRUE)
    path_y <- grep("/", links_w_files$Source.y, fixed = TRUE)

    # From one file to different OS paths
    diff_paths_y <- setdiff(path_y, path_x)
    diff_paths_x <- setdiff(path_x, path_y)

    table_x <- split(links_w_files$Source.x[diff_paths_x],
                     links_w_files$Source.y[diff_paths_x])
    removing_idx <- numeric()
    if (any(lengths(table_x) > 1L)) {
        source_y_dup <- names(table_x)[lengths(table_x) == 2]
        dup_sources <- links_w_files[diff_paths_x, "Source.y"] %in% source_y_dup
        links_w_files[diff_paths_x, "Source.x"][dup_sources] <- basename(links_w_files[diff_paths_x, "Source.x"][dup_sources])
        dup_x <- duplicated(links_w_files$Source.x[diff_paths_x[dup_sources]])
        removing_idx <- diff_paths_x[dup_sources][dup_x]
    }
    if (any(lengths(table_x) < 2L)) {
        warning("Some pages point to different places according to the OS.",
                call. = FALSE)
    }

    dup_y <- duplicated(links_w_files$tmp[diff_paths_y])
    removing_idy <- numeric()
    if (any(dup_y)) {
        dups <- links_w_files$tmp %in% links_w_files$tmp[diff_paths_y][dup_y]
        # Create duplicated
        links_w_files$Source.y[dups] <- basename(links_w_files$Source.y[dups])
        dup_y <- duplicated(links_w_files$Source.y[dups])
        removing_idy <- which(dups)[which(dup_y)]
    }
    if (length(diff_paths_y) && !all(dup_y))  {
        warning("Some links are distinct depending on the OS.",
                call. = FALSE)
    }

    # Links between different OS aren't possible
    # Remove combinations between OS: Keep only the one from the same OS.
    both_have_paths <- intersect(path_x, path_y)
    dir_x <- dirname(links_w_files$Source.x[both_have_paths])
    dir_y <- dirname(links_w_files$Source.y[both_have_paths])
    removing_ids <- both_have_paths[dir_x != dir_y]

    links_w_files$tmp <- NULL
    # Removing links due to different OS Targets or manual pages
    # This allows to keep duplicated links on the original help pages
    removing_all_issues <- unique(c(removing_idx, removing_idy, removing_ids))
    # In case there are no issues it would remove all data!
   if (length(removing_all_issues)) {
        links_w_files <- links_w_files[-removing_all_issues, ]
   }

    # Prepare for the output
    colnames(links_w_files) <- c("to_pkg", "to_target", "from_pkg", "from_Rd", "n", "to_Rd")
    links_w_files[is.na(links_w_files[, "to_Rd"]), "to_Rd"] <- ""
    links_w_files <- links_w_files[, c(3L, 4L, 1L, 2L, 6L, 5L)]
    links_w_files <- sort_by(links_w_files,
                             links_w_files[, c("from_pkg", "from_Rd", "to_target", "to_Rd")])
    rownames(links_w_files) <- NULL
    links_w_files
}

# pkg:topic but pkg is not on the dependencies.
xrefs_wo_deps <- function(links, ap, ref) {
    anchor <- links$Anchor
    # WRE: only applies to these conditions:
    eq_anchor <- startsWith(anchor, "=")
    full_anchor <- grepl(":", anchor, fixed = TRUE)

    links2 <- links[full_anchor & !eq_anchor, ]
    links3 <- split_anchor(links2)

    # Filter out those that are not on the recursive dependency tree
    links4 <- links2[!links3$to_pkg %in% c(rownames(ap), BASE, ref), ]
    rownames(links4) <- NULL
    links4
}

# Targets linked.
pkgs_linked_missing <- function(links, alias) {
   anchor <- links$Anchor
   eq_anchor <- startsWith(anchor, "=")

   links2 <- links[eq_anchor, ]
   topics <- substring(links2$Anchor, 2L)
   missing_topics <- setdiff(unique(topics), alias$Target)
   links2 <- links2[topics %in% missing_topics, ]
   rownames(links2) <- NULL
   links2
}

check_anchor <- function(targets) {
    target <- targets$Target
    anchor <- targets$Anchor
    using_anchor <- nzchar(anchor)
    whitespaces_target <- (grepl("^\\s+", target) | grepl("\\s+$", target))
    whitespaces_anchor <- (grepl("^\\s+", anchor) | grepl("\\s+$", anchor))
    error_anchor <- (whitespaces_target & !using_anchor) | (whitespaces_anchor & using_anchor)
    if (any(error_anchor)) {
        pkges <- unique(targets$Package[error_anchor])
        warning("Packages ", toString(sQuote(pkges)),
                " have a trailing whitespace on 'Anchors' that can create problems.\n",
                "Check section 2.5 section of Writing R Extensions. ")
        return(FALSE)
    }
    TRUE
}


packages_in_links <- function(links, packages) {
    to_pkg <- (!is.na(links$to_pkg) & links$to_pkg %in% packages)
    links <- links[links$from_pkg %in% packages | to_pkg, , drop = TRUE]
    rownames(links) <- NULL
    links
}
