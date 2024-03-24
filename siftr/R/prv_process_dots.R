
# Processing 'dots' into queries ------------------------------------------
# Copied from DesiQuintans/librarian ======================================

# Convert dots to Character strings
nse_dots <- function(...) {
    if (dots_length(...) <= 0) {
        return(character(0))
    }

    if (dots1_is_char(...)) {
        # If dots only contains one Character vector (of any length), then
        # accept it as-is.
        dots <- ..1
    } else {
        # Otherwise, process dots into a Character vector.
        dots <- as.character(eval(substitute(alist(...))))
        dots <- gsub("\\s", "", dots)  # Closes https://github.com/DesiQuintans/librarian/issues/13
    }

    dots <- unique(dots)
    dots <- dots[nchar(dots) > 0]

    return(dots)
}


# How many items are in dots?
dots_length <- function(...) {
    length(eval(substitute(alist(...))))
}


# Is the 1st 'dots' arg a character vector with length > 1?
dots1_is_char <- function(...) {
    result <- tryCatch(eval(..1),
                       error   = function(e) return(FALSE),
                       warning = function(w) return(FALSE))

    any(
        # A character vector with more than one element.
        (is.vector(result) & is.character(result) & length(result) > 1),
        # A character vector and no other items are provided in dots.
        (is.vector(result) & is.character(result) & dots_length(...) == 1)
    )
}
