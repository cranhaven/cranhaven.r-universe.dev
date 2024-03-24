
#' Save a dataframe's dictionary to a file
#'
#' @description
#' Saves a data dictionary in a form that is usable by my dataframe
#' labelling package, [`tsv2label`](https://github.com/DesiQuintans/tsv2label).
#'
#' This function creates a folder called `"{df name}_dictionary"` that contains
#' a file called `index.tsv` and other files that contain factor level
#' definitions. _The folder itself_ is the data dictionary, and `tsv2label`
#' looks inside it to do its labelling.
#'
#' Under normal circumstances you would simply save the dataframe using
#' [saveRDS()] or [save()], both of which save the dataframe exactly as it is,
#' including all of its labels and types.
#' However, there are times when you need to keep the dataframe in a format that
#' won't save its metadata. For example:
#' 1. You want to export the dataframe to a CSV to share it with others.
#' 2. You need to keep the data in a particular format to take advantage of
#'    another R package, and that package does not save metadata. For example,
#'    maybe you're working with very large data and you want to keep it in CSV
#'    to take advantage of `vroom`, or in FST to take advantage of `fst`.
#' 3. You are accessing the data remotely (e.g. building it from API calls) and
#'    you need to relabel it every time.
#'
#'
#' @param df (Dataframe) The dataframe whose dictionary you want to save.
#' @param path (Character) The path to save the dictionary to. The dictionary's
#'      files will be put into a new subfolder called `"{df}_dictionary"`.
#' @param ... (Dots) Other named arguments that will be passed to [utils::write.table()].
#'
#' @return Returns nothing, but prints a message announcing where the dictionary
#'      was saved. In an interactive session, it also opens that location in
#'      your file explorer.
#' @export
#'
#' @examples
#' \donttest{
#' save_dictionary(CO2, path = tempdir())
#' }
#'
#' @md
save_dictionary <- function(df, path = stop("'path' must be specified."), ...) {
    df_symb <- substitute(df)
    df_char <- deparse(df_symb)


    # 1. Set up save location -------------------------------------------------

    # NOTE I have decided not to add a zip option to this function because it
    # requires a `zip` exe to be in the user's PATH, which may confuse people.
    # I leave it to the user to zip the file if they want.
    save_path <- file.path(path, paste0(df_char, "_dictionary"))

    if (dir.exists(save_path) == FALSE) {
        dir.create(save_path)
    }


    # 2. Force a rebuild of the dictionary ------------------------------------

    # This rebuilds the dictionary via sift() so that the updated dictionary is
    # immediately available inside sift()'s closure.
    # https://stackoverflow.com/a/75849101/5578429
    dict <- eval(bquote(sift(.(df_symb), .rebuild = TRUE)))
    rownames(dict) <- NULL


    # 3. Generate a factor file for each unique factor variable ----------------

    # 1. Get and name unique factor levels in this dataframe
    each_fct <- dict[c("varname", "fct_lvl", "fct_ordered")]   # Keep these cols
    each_fct <- each_fct[each_fct$fct_lvl != "NULL", ]         # Only keep factors
    each_fct <- each_fct[!duplicated(each_fct$fct_lvl), ]      # Only keep first mentions
    each_fct$fname <- sprintf('fct_%s.tsv', each_fct$varname)  # Generate names

    # Create and write these dataframes
    for (i in seq_len(nrow(each_fct))) {
        levels_vec <- eval(str2lang(each_fct[i, ]$fct_lvl))

        utils::write.table(
            data.frame(levels  = levels_vec,
                       labels  = levels_vec,
                       ordered = c(each_fct[i, ]$fct_ordered, rep("", length(levels_vec) - 1)),
                       exclude = "",
                       stringsAsFactors = FALSE
            ),
            file = file.path(save_path, each_fct[i, ]$fname), quote = FALSE,
            sep = "\t", row.names = FALSE, qmethod = "double", ...
        )

        # Replace entries matching these levels in the dictionary with the filename.
        dict$fct_lvl[dict$fct_lvl == each_fct[i, ]$fct_lvl] <- each_fct[i, ]$fname
    }


    # 4. Clean up final output for index.tsv -----------------------------------

    # Add a column for renames
    dict$rename <- ""

    # Only some columns are useful in a data dictionary
    dict <- dict[, c("colnum", "varname", "rename", "var_lab", "fct_lvl", "type_str", "pct_miss", "pct_nonmiss")]

    # Some columns need to be renamed for tsv2label
    colnames(dict) <- c("colnum", "name", "rename", "description", "factor_file", "type", "pct_missing", "pct_nonmissing")

    # Remove UTF-8 'x' in type_str; Excel doesn't open UTF-8 by default.
    dict$type <- gsub(cli::symbol$times, "x", dict$type)

    # Remove NULLs from factor_file
    dict$factor_file[dict$factor_file == "NULL"] <- ""

    # Write it to a tab-delimited file.
    # Encoding uses the platform's native encoding by default. See `iconvlist`
    # for a list of accepted encodings.
    utils::write.table(dict, file = file.path(save_path, "index.tsv"), quote = FALSE,
                       sep = "\t", row.names = FALSE, qmethod = "double",
                       ...)

    message("Dictionary saved to '", gsub("\\\\", "/", save_path), "'.")

    if (interactive()) {
        utils::browseURL(save_path)
    }

    return(invisible())
}
