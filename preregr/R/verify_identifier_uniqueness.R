verify_identifier_uniqueness <- function(x, dfName, colName) {

  if (any(duplicated(x))) {
    stop("When sanitizing and verifying the identifiers in data frame / ",
         "worksheet '", dfName, "', I found that not ",
         "all identifiers (i.e., values in column '",
         colName, "') were unique. Specifically, ",
         vecTxtQ(x[duplicated(x)]), " occurred twice or more.");
  }

  return(x);

}
