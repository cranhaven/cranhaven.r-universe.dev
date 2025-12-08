library(testthat)
library(tgver)

# disable browserURL
Sys.setenv(R_BROWSER = FALSE)
test_check("tgver")
