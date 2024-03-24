
# User-facing messages are collected here for easy editing/tone consistency.
#
# @param entry (String) The name of one of the entries below.
# @param i (Integer) The index/line to return from the entry.
# @param ... (Dots) Arguments that will be passed to [sprintf()] to fill the placeholders
#      in the messages.
#
# @return A named list of character vectors.
# @examples
# # msg_sift("not a df", 1, "month.abb")
# @md
msg_sift <- function(entry, i = 1, ...) {
    text <- list(
        `not a df`    = c("'%s' is not a dataframe.",
                          "sift() only searches through dataframes."),

        `report dims` = c("'%s' has %i columns: %s."),

        `no matches`  = c("No matches found for query '%s' with .dist = %.2f.",
                          "If you're using a regular expression, pass it as a string.",
                          "Try increasing '.dist = %.2f' to allow more distant matches."),

        `building`    = c("Building dictionary for '%s'. This only happens when it changes."),

        `built`       = c("Dictionary was built in %s."),

        `n results`   = c("There %s %i result%s for query `%s`."),

        `over limit`  = c("Only %1$s of them %2$s printed, set by options_sift(\"sift_limit\", %1$s)"),

        `not option`  = c("'%s' is not one of sift's options.",
                          "Did you mean %s?"),

        `dist_ignore` = c("An orderless search was performed, so '.dist = %.2f' was ignored.",
                          "To remove this warning, either remove '.dist' or provide your query as a single character string.")
        )
    return(sprintf(text[[entry]][i], ...))
}
