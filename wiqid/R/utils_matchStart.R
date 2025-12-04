
# Performs partial matching, finding the elements in 'big'
#   where the start of 'big' matches 'little'.
# eg. matchStart(c("Ju", "Nov", "J"), month.name) returns 6, 7, 11, 1.
#  June and July match both 'Ju' and 'J', but only the first match is returned.
# The order of the output depends on 'little'.
# Does not use 'grep', and brackets "[ ]" are treated as normal characters.
# Returns integer(0) if no matches.

# 'little' may also be a vector of numerical indices, which will be checked and returned.

matchStart <- function(little, big) {
  if(is.numeric(little)) {
    little <- round(little)
    if(all(little < 0))
      return(seq_along(big)[little])
    out <- little[little > 0 & little <= length(big)]
  } else {
    little <- as.character(little)
    out <- NULL
    for(i in seq_along(little)) {
      nc <- nchar(little[i])
      big1 <- substr(big, 1, nc)
      out <- c(out, which(big1 == little[i]))
    }
  }
  unique(out)
}
