# Fast approximate summary functions

# Randomly choose and show some unique values from a variable.
#
# This does some clever stuff depending on the class of the variable.
#
# @param x (Vector) A vector.
#
# @return A Character string.
# @md
some_uniques <- function(x) {
    # ---- Special cases -------------------------------------------------------

    # Sampling unique values can be expensive for big data. Many variable types
    # can be specially handled for efficiency.

    # ---- For Factors, use their levels ----

    # If it's a haven_labelled vector, I want to show it as a factor that
    # has both the values and the labels together.
    if (has_class(x, "haven_labelled")) {
        x <- haven_to_factor(x)
    }

    # If the thing is a factor, then its unique values are its levels. All levels
    # should be returned.
    if (is.factor(x)) {
        return(trimws(cli::ansi_collapse(levels(x), width = Inf, style = "head",
                                         sep = " | ", last = " | ")))
    }

    # ---- For Logicals, use primitive functions ----

    if (is.logical(x)) {
        # Fast unique for logical vectors using primitive functions.
        # This does not create any copies.
        #
        # l <- sample(c(T, F, NA), size = 6e6, replace = TRUE)
        #   expression             min median `itr/sec` mem_alloc `gc/sec` n_itr
        #   <bch:expr>         <bch:t> <bch:>     <dbl> <bch:byt>    <dbl> <int>
        # 1 unique(l)           30.1ms 32.4ms      30.6    22.9MB     5.56    11
        # 2 code below           1.9µs  2.4µs  366043.         0B     0    10000

        # all(l) * 1  returns 1 if everything is TRUE (0 if there are FALSEs).
        # anyNA() * 2 returns 2 if there are any NAs  (0 if no NAs).

        return(
            switch (as.character(sum(all(x, na.rm = TRUE) * 1, anyNA(x) * 2)),
                    `0` = "TRUE | FALSE",       # 0 + 0 = 0 = TRUE FALSE
                    `1` = "TRUE",               # 1 + 0 = 1 = TRUE
                    `2` = "TRUE | FALSE | NA",  # 0 + 2 = 2 = TRUE FALSE NA
                    `3` = "TRUE | NA"           # 1 + 2 = 3 = TRUE       NA
            )
        )
    }

    # ---- For Numerics, use the head ----

    if (is.numeric(x)) {
        # Having numeric values in the haystack is not often helpful for
        # searching. They're more useful in the peek, as a preview of what the
        # column contains.
        # unique(head(x)) meets this need AND doesn't create copies.

        # test <- rnorm(1e6)
        # bench::mark(a = unique(head(test)), b = sample(test, 6), c = test[sample.int(length(test), 6, useHash = TRUE)], check = FALSE)
        #
        #   expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc
        #   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>
        # 1 a             4.7µs    5.7µs   164957.        0B      0   10000     0
        # 2 b           518.4µs  601.9µs     1621.    3.82MB     27.6   588    10
        # 3 c             3.9µs    4.9µs   188942.    2.49KB      0   10000     0

        pool <- should_approx(x)
        uniques <- unique(x[1:min(length(x), options_sift("sift_guessmax"))])
        uniques <- uniques[!is.na(uniques)]  # cli::ansi_collapse() cannot handle NAs.

        return(trimws(paste(pool$marker,
                        cli::ansi_collapse(uniques, width = Inf, style = "head",
                                           sep = " | ", last = " | "))))
    }

    # Other vectors need to have their unique values worked out.

    # Sample elements from the variable.
    pool <- should_approx(x)

    uniques <- as.character(sample(unique(x[pool$indices])))
    uniques <- uniques[!is.na(uniques)]  # cli::ansi_collapse() cannot handle NAs.

    if (length(uniques) == 0) {
        # Length can be zero if the column was entirely NAs.
        return(trimws(paste(pool$marker, "NA")))
    }

    # Avoiding an issue with Excel where cells are limited to 32k characters. Probably
    # smart to limit the length of this output anyway so that the haystack being searched
    # is not too long.
    charlen <- cumsum(nchar(uniques))   # Running count of character length
    uniques <- uniques[charlen <= options_sift("sift_peeklength")]  # All elements until the nth character.

    return(trimws(paste(pool$marker,
                     cli::ansi_collapse(uniques, width = Inf, style = "head",
                                        sep = " | ", last = " | "))))
}


# Are all elements of the vector identical?
#
# Up to 30x faster than `length(unique())`, with 1/5th memory allocation.
# <https://gist.github.com/DesiQuintans/0607047ba767469c870a3149b1f78cfe>
# Although it's very fast (6e6 doubles takes 50 ms, 6e6 strings takes 200 ms),
# it still has to run on each column, and there may be hundreds of those. It
# therefore randomly samples the vector once it reaches `sift_guessmax`'s limit.
#
# @param x (Vector) A vector.
# @param na.rm (Logical) If `TRUE`, remove `NA`s.
#
# @return A logical.
# @md
invariant <- function(x) {
    # Source: https://stackoverflow.com/a/59067398/5578429

    if (typeof(x) %in% c("list")) {
        # I don't want to go into the rabbit hole of comparing list cols.
        return(NA_character_)
    }

    pool <- should_approx(x)

    y <- x[pool$indices]
    y <- y[!is.na(y)]

    if (length(y) == 0 || all(y[1] == y)) {
        # Length can be zero if the column was entirely NAs.
        return(trimws(paste(pool$marker, "Yes")))
    } else {
        return(trimws(paste(pool$marker, "No")))
    }
}



# Choose the most informative variable type to show
#
# Almost all variable types can be named from their [typeof()], except
# for factor variables which are given the [class()] of 'factor'. Likewise,
# objects with class `haven_labelled` have an underlying type, and both the
# class and type are important to show.
#
# @param x (Vector) A vector.
#
# @return Character.
# @md
coltype <- function(x) {
    #   class               typeof
    #
    #   * factor            integer
    #   * haven_labelled    * integer
    #   numeric             * double
    #   integer             * integer
    #   logical             * logical
    #   character           * character

    if (has_class(x, "factor")) {
        ordered <- ""
        if (is.ordered(x)) {
            ordered <- " (ord.)"
        }

        return(sprintf("factor %s%i%s", cli::symbol$times, length(levels(x)), ordered))
    }

    if (has_class(x, "haven_labelled")) {
        return(sprintf("int (hvn_lbl) %s%i", cli::symbol$times, length(attr(x, "labels"))))
    }

    return(typeof(x))
}
