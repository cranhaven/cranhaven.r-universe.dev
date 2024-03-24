# Hash an object with fastdigest and update a list with its name and hash
#
# @param df_name (String) The name of a dataframe, as a Character string.
# @param hashlist (List) The list/vector that will hold named hashes.
#
# @return A named vector/list. The name is the variable name of the object that was
#     hashed, and the value is the hash itself.
#
# @examples
# # Example data
# myiris <- iris
#
# # Initialise the empty hash list
# x <- list()
#
# # Add a hash
# x <- hash_obj("myiris", x)
# x
#
# #> $myiris
# #> [1] "0b3ea20866c7f9624af51089a7fe1b90"
#
# # Add another hash
# x <- hash_obj("mtcars", x)
# x
#
# #> $myiris
# #> [1] "0b3ea20866c7f9624af51089a7fe1b90"
# #>
# #> $mtcars
# #> [1] "8cf6009c35431a30d621433ae7e63905"
#
# # Newly calculated hashes can overwrite/update an existing hash in the list.
# # To demonstrate, let's change myiris somehow.
# myiris <- head("myiris")
#
# x <- hash_obj("myiris", x)
# x
#
# #> $myiris
# #> [1] "6e5ee1af3da0e3141cb521323c8fd52c"
# #>
# #> $mtcars
# #> [1] "8cf6009c35431a30d621433ae7e63905"
# }
# @md
hash_obj <- function(df_name, hashlist) {
    # Early dev versions of sift hashed the entire object, but this became prohibitive
    # with even modest datasets (1 GB, ~ 300 cols and 2 million rows). But the entire
    # dataframe doesn't really need to be hashed, we can get lots of info from hashing
    # only part of it.
    #
    # Which parts do we hash? What actually counts as a dataframe changing, i.e. a
    # transformational event that justifies rebuilding the dictionary?
    #
    # 1. Change in dimensions (rows and cols)
    # 2. Change in order of columns
    # 3. Change in data types
    # 4. Change in labels
    # 5. Change in number of NAs in a column (implies transformation of some kind).

    df <- eval(as.symbol(df_name))

    fingerprint <- list(
        # Col names, order, types, levels, labels.
        df[0,],
        # Number of rows and columns.
        dim(df),
        # NA count. https://gist.github.com/DesiQuintans/a01d5b91d50954d3b66c9e1fc3109d8f
        vapply(df, function(col) { sum(is.na(col)) }, integer(1))
    )

    hashlist[df_name] <- fastdigest::fastdigest(fingerprint)

    return(hashlist)
}
