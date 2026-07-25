inztransform <- function(df, transform) {
    if (is.null(transform) || length(transform) == 0L) return(df)

    for (trans in names(transform)) {
        # cat(sprintf("Transform %s using %s\n", trans, transform[[trans]]))
        df$data[[trans]] <- switch(
            transform[[trans]],
            "log" = {
                if (all(is.na(df$data[[trans]]))) {
                    df$data[[trans]]
                    break
                }
                if (min(df$data[[trans]], na.rm = TRUE) < 0)
                    warning("Cannot log negative values; converting to NA.")
                suppressWarnings(log(df$data[[trans]]))
            },
            "log10" = {
                if (all(is.na(df$data[[trans]]))) {
                    df$data[[trans]]
                    break
                }
                if (min(df$data[[trans]], na.rm = TRUE) < 0)
                    warning("Cannot log negative values; converting to NA.")
                suppressWarnings(log10(df$data[[trans]]))
            },
            # the default (for e.g., time transform) is just the col
            df$data[[trans]]
        )
    }

    if (!is.null(df$design))
        df$design$variables <- df$data
    
    df
}
