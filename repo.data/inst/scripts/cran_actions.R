actions_s2 <- actions_s[actions_s$Action != "remove", ]
s_pkg <- split(actions_s2, actions_s2$Package)


around_archive <- function(e) {
    # Make sure data is sorted
    e <- sort_by(e, ~Package + as.POSIXct(paste(Date, Time), tz = "UTC"))
    pub <- e$Action == "publish"
    arc <- e$Action == "archive"
    pub_w <- which(pub)
    arc_w <- which(arc)
    # a <- rle(cumsum(arc))
    # p <- rle(cumsum(pub))

    dts <- e$Date
    first_rel <- if (length(pub_w)) dts[pub_w[1]] else NA
    first_arc <- if (length(arc_w)) dts[arc_w[1]] else NA
    last_rel <- if (length(pub_w)) dts[pub_w[length(pub_w)]] else NA
    last_arc <- if (length(arc_w)) dts[arc_w[length(arc_w)]] else NA

    pkg_info <- data.frame(
        First.Release = first_rel,
        First.Archival = first_arc,
        Last.Release = last_rel,
        Last.Archival = last_arc,
        Published = length(pub_w),
        Archived = length(arc_w),
        check.names = FALSE
    )
    cbind(pkg_info,
          before_after(arc_w, pub_w, e)
    )
}

before_after <- function(ref, abc, pkg_info) {
    a <- b <- vector(mode = "numeric", length(ref))
    # We compare by position (sorted by date & Time!!)
    # Not possible to have the same position both Archived and Published
    for (i in seq_along(ref)) {
        before <- abc[abc < ref[i]]
        b[i] <- if (length(before)) {
            max(before)
        } else {
            NA
        }
        after <- abc[abc > ref[i]]
        a[i] <- if (length(after)) {
            min(after)
        } else {
            NA
        }
    }
    df <- data.frame(Published.Before = pkg_info$Date[b],
                     Archived.Date = pkg_info$Date[ref],
                     Published.After = pkg_info$Date[a],
                     Archived.by = pkg_info$User[ref],
                     check.names = FALSE)
    if (!nrow(df)) {
        df[1L, , drop = FALSE]
    } else {
        df
    }
}

pkgs_dates <- lapply(s_pkg, around_archive)
table(lens <- vapply(pkgs_dates, NROW, 0L))

## Unarchivals:
udb <- cbind(Package = rep.int(names(pkgs_dates), lens),
             do.call(rbind, pkgs_dates))

udb$First.Release <- as.Date(udb$First.Release, origin = "1970-01-01")
udb$Last.Release <- as.Date(udb$Last.Release, origin = "1970-01-01")
udb_delta <- cbind(udb,
                   Delta.Before = as.numeric(difftime(udb$Archived.Date, udb$Published.Before, units = "days")),
                   Delta.After = as.numeric(difftime(udb$Published.After, udb$Archived.Date, units = "days")))

tab_a <- do.call(rbind,
               with(udb_delta[!is.na(udb_delta$Archived.Date), ],
                    tapply(Delta.After,
                           format(Published.Before, "%Y"),
                           function(e)
                               c(summary(e), n = length(e))
                           )))
tab_p <- do.call(rbind,
               with(udb_delta[!is.na(udb_delta$Archived.Date), ],
                    tapply(Delta.Before,
                           format(Published.Before, "%Y"),
                           function(e)
                               c(summary(e), n = length(e))
                           )))


who <- do.call(rbind,
               with(udb_delta[!is.na(udb_delta$User), ],
                    tapply(User,
                           format(Publish, "%Y"),
                           function(e){c(summary(e), total = length(e))})))


total_packages <- function(action_s) {
    new_p <- unique(udb[, c("Package", "First.Release", "Archived.Date", "Published.After")])
    rel_dates <- unique(c(new_p$First.Release, new_p$Published.Before, new_p$Archived.Date, new_p$Published.After))
    rel_dates <- sort(na.omit(rel_dates))
    dates <- sapply(rel_dates, function(d, new_p) {
        # arc_d <- new_p$Archived.Date == d
        new_packages <- sum(new_p$First.Release == d, na.rm = TRUE)
        archived <- sum(new_p$Archived.Date == d, na.rm = TRUE)
        resubmitted <- sum(new_p$Published.After == d, na.rm = TRUE)
        c(new_packages = new_packages,
          archived = archived,
          resubmitted = resubmitted,
          removed =  resubmitted)
    }, new_p = new_p)
    da <- as.data.frame(t(dates))
    da$date <- rel_dates
    da$total <- da$new_packages - da$archived + da$resubmitted
    da$ctotal <- cumsum(da$total)

}
new_pkg <- data.frame(New.Packages = new_p,
                      Date = unique(actions_s$Date), check.names = FALSE)
