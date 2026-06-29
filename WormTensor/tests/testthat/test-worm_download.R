.dist2mat <- function(x) {
    as.matrix(x)
}

############ Euclid ############
## dist object
Ds_Euclid <- worm_download("Euclid")$Ds
expect_true(object.size(Ds_Euclid) != 0)
expect_equal(length(Ds_Euclid), 24)
expect_true(all(lapply(Ds_Euclid, function(x) {is(x, "dist")})))
## matrix object
Ms_Euclid <- lapply(Ds_Euclid, .dist2mat)
expect_true(all(unlist(lapply(Ms_Euclid, isSymmetric))))

############# mSBD #############
## dist object
Ds_mSBD <- worm_download("mSBD")$Ds
expect_true(object.size(Ds_mSBD) != 0)
expect_equal(length(Ds_mSBD), 24)
expect_true(all(lapply(Ds_mSBD, function(x) {is(x, "dist")})))

## matrix object
Ms_mSBD <- lapply(Ds_mSBD, .dist2mat)
expect_true(all(unlist(lapply(Ms_mSBD, isSymmetric))))

############ WARN ############
Ds_Euclid_warn <- worm_download("Euclid", qc = "WARN")$Ds
expect_true(object.size(Ds_Euclid_warn) != 0)
expect_equal(length(Ds_Euclid_warn), 27)
expect_true(all(lapply(Ds_Euclid_warn, function(x) {is(x, "dist")})))
############ FAIL ############
Ds_Euclid_fail <- worm_download("Euclid", qc = "FAIL")$Ds
expect_true(object.size(Ds_Euclid_fail) != 0)
expect_equal(length(Ds_Euclid_fail), 28)
expect_true(all(lapply(Ds_Euclid_fail, function(x) {is(x, "dist")})))
