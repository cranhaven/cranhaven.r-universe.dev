
# Print a row of the data dictionary to the terminal
#
# This function controls how results from the data dictionary
#
# @param dr (Character) A named Character vector which is one extracted row
#      of the data dictionary (as sent to this function by apply() in sift()).
#
# @return Side-effect of printing to the terminal.
display_row <- function(dr) {
    # This should check whether the summary stat is NA/NULL, and if it's not, then print it.
    # This means that control of what is displayed from the dictionary is held here, and
    # control over what is calculated at a variable level is rightly held
    # inside the build_dictionary() function and what it decided to compute.

    # Controls what gets printed.
    #
    # This function
    print_stat <- function(col, fmt, unit = "") {
        if (!is.na(col) & col != "NULL" & col != "" & col != cli::symbol["warning"]) {
            cli::col_grey(fmt, col, unit)
        } else {
            if (grepl(cli::symbol["warning"], col)) {
                cli::col_grey(fmt, paste(cli::symbol["warning"], "--"), unit)
            } else {
                cli::col_grey(fmt, "--", unit)
            }
        }
    }

    cli::cli({
        # Line 1: Column number and column name.
        cli::cli_text(cli::col_grey(dr["colnum"]), " ", cli::style_bold(dr["varname"]))

        # Pad all other output on the left and right
        cli::cli_div(class = "tmp",
                     theme = list(.tmp = list("margin-left"  = 4)))

            # Line 2: If the variable has a label, show it.
            if (!any(dr["var_lab"] == c("NULL", "", character(0)))) {
                cli::cli_text(cli::col_grey(dr["var_lab"]))
            }

            # Pad all other output more rightward
            cli::cli_div(class = "tmp2",
                         theme = list(.tmp2 = list("margin-left"  = 4)))

            cli::cli_verbatim(
                cli::ansi_columns(
                    c(print_stat(dr["type_str"],    "Type: "),
                      print_stat(dr["pct_miss"],    "Missing: ", " %"),
                      print_stat(dr["all_same"],    "All same? ")
                      ),
                    width = cli::console_width() - 4*2,  # This is already double-padded
                    fill = "rows",
                    max_cols = 4,
                    align = "left",
                    sep = "    "
                )
            )

            cli::cli_text(
                cli::col_grey(
                cli::ansi_strtrim(print_stat(dr["rand_unique"], "Peek: "),
                                  width = cli::console_width() - 17)
                                  # -17 for indenting, "Peek: " label, and approx glyph
                ))

            cli::cat_line()
    })
}
