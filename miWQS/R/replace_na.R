# Copy from makeJournalTables
#
# tidyr::replace_na() cannot replace a missing value with a list of values, i.e. executing
#  tidyr::replace_na(c(NA, 2, NA, 3), list(-23, -43))
#  will return Error: Replacement for `alpha-chlordane` is length 11, not length 1
#
# replace_na  <- function(data, replace) tidyr::replace_na(data, replace)


replace_na <- function(data, replace) {
  if (is.vector(data) | is.factor(data)) {
    data[is.na(data)] <- replace

  } else {         # Way to avoid a for loop???
    for (j in 1:ncol(data)) {
      data[is.na(data[, j]), j] <- replace[[j]]
    }
  } # end class if statement
  return(data)
}
#
# Proof:
# #   ## > log.X <- as.matrix(
#   +   makeJournalTables::replace_na(as.data.frame(X.bdl), replace = as.list(DL/sqrt(2)))
#   + )
# > log.X2 <- as.matrix(
#   +   tidyr::replace_na(as.data.frame(X.bdl), replace = as.list(DL/sqrt(2)))
#   + )
# > summary(log.X)
# alpha-chlordane        dieldrin        gamma-chlordane        lindane
# Min.   :    0.572   Min.   :    4.59   Min.   :    0.593   Min.   :    3.63
# 1st Qu.:    3.023   1st Qu.:   25.75   1st Qu.:    3.083   1st Qu.:   14.22
# Median :   12.983   Median :  109.51   Median :   15.543   Median :   50.32
# Mean   :  175.944   Mean   :  995.06   Mean   :  168.920   Mean   :  441.49
# 3rd Qu.:   48.727   3rd Qu.:  466.67   3rd Qu.:   74.503   3rd Qu.:  182.86
# Max.   :13844.725   Max.   :74786.25   Max.   :12492.984   Max.   :36505.39
# > summary(log.X2)
# alpha-chlordane        dieldrin        gamma-chlordane        lindane
# Min.   :    0.572   Min.   :    4.59   Min.   :    0.593   Min.   :    3.63
# 1st Qu.:    3.023   1st Qu.:   25.75   1st Qu.:    3.083   1st Qu.:   14.22
# Median :   12.983   Median :  109.51   Median :   15.543   Median :   50.32
# Mean   :  175.944   Mean   :  995.06   Mean   :  168.920   Mean   :  441.49
# 3rd Qu.:   48.727   3rd Qu.:  466.67   3rd Qu.:   74.503   3rd Qu.:  182.86
# Max.   :13844.725   Max.   :74786.25   Max.   :12492.984   Max.   :36505.39
# > summary(log.X2 - log.X)
# alpha-chlordane    dieldrin gamma-chlordane    lindane
# Min.   :0       Min.   :0   Min.   :0       Min.   :0
# 1st Qu.:0       1st Qu.:0   1st Qu.:0       1st Qu.:0
# Median :0       Median :0   Median :0       Median :0
# Mean   :0       Mean   :0   Mean   :0       Mean   :0
# 3rd Qu.:0       3rd Qu.:0   3rd Qu.:0       3rd Qu.:0
# Max.   :0       Max.   :0   Max.   :0       Max.   :0
# >
