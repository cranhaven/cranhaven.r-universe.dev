# Build a data dictionary from a dataframe
#
# This data dictionary can a) be exported by the user and used to relabel a dataframe,
# and b) contains pre-calculated values that are used by `sift()` to report on the
# dataframe's contents. Note that for use-case a), dataframe fields that are expected to
# have multiple values are stored as deparsed code, and `eval(parse(text = dict$col[rownum]))`
# can be used to actually run this code to get the vectors back.
build_dictionary <- function(DF, dictlist) {
    df_name <- DF
    DF <- eval(as.symbol(DF))

    cli::cli_alert_info(msg_sift("building", 1, df_name), wrap = TRUE)

    start_time <- Sys.time()

    # Elements to be searched, in lists where each element may be >1 long.
    raw_colnum   <- 1:length(DF)
    raw_varnames <- vapply(colnames(DF), function(name) { deparse(as.name(name), backtick = TRUE) }, character(1))
    raw_var_labs <- sapply(DF, attr, "label")
    raw_val_labs <- sapply(DF, function(col) { names(attr(col, "labels")) })  # The names are what I want.
    raw_fct_lvls <- sapply(DF, levels)

    # Extra details for the data dictionary
    dct_type_strs <- sapply(DF, coltype)
    dct_ordered   <- sapply(DF, is.ordered)
    dct_classes   <- sapply(DF, function(x) { paste(class(x), collapse = ", ") })
    dct_types     <- sapply(DF, typeof)
    dct_pct_miss  <- sapply(DF, function(col) { trunc((sum(is.na(col)) / length(col)) * 100) })
    dct_rand_uniq <- sapply(DF, some_uniques)
    dct_all_same  <- sapply(DF, invariant)

    # Getting labels into vectors of length 1.
    var_labs <- crunch(raw_var_labs)
    val_labs <- crunch(raw_val_labs)
    fct_lvls <- crunch(raw_fct_lvls)
    # Those labels and unique values joined together to make searchable strings.
    haystacks <- smash(raw_varnames, var_labs, val_labs, fct_lvls, dct_rand_uniq)

    dictionary <-
        data.frame(
            colnum      = raw_colnum,
            varname     = raw_varnames,
            var_lab     = var_labs,
            rand_unique = dct_rand_uniq,
            pct_miss    = dct_pct_miss,
            pct_nonmiss = 100 - dct_pct_miss,
            type_str    = dct_type_strs,
            all_same    = dct_all_same,
            val_lab     = codify(raw_val_labs),
            fct_lvl     = codify(raw_fct_lvls),
            fct_ordered = dct_ordered,
            class       = codify(dct_classes),
            type        = codify(dct_types),
            haystack    = haystacks,
            row.names   = NULL
        )

    dictlist[df_name] <- list(dictionary)

    end_time <- Sys.time()
    elapsed <- round(end_time - start_time, digits = 2)
    elapsed_str <- paste(elapsed, attr(elapsed, "units"))

    cli::cli_alert_success(msg_sift("built", 1, elapsed_str))
    cli::cat_line()

    return(invisible(dictlist))
}
