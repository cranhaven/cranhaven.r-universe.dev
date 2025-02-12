library(data.table)
library(acs)
library(synthACS)
library(retry)
library(testthat)


# load(system.file("testdata", 'dat-acsdata.Rda', package= "synthACS"))
# ca_dat <- readRDS(file= system.file("tests", "testdata", 'dat-cadat.rds'
#                                     , package= "synthACS"))
# towork <- readRDS(file= system.file("tests", "testdata", 'dat-towork.rds'
#                                     , package= "synthACS"))

# test_check("synthACS")

# LOAD TEST DATA FOR LOCAL TESTS
#------------------------------------------------
# below file is ~69 mb; saved on dropbox. Must port to local install
# load("./tests/testdata/dat-par_sim_anneal.Rdata")


# MAKE TEST DATA
#------------------------------------------------
# library(synthACS)
# ca_geo <- geo.make(state= 'CA', county= '*')
# ca_dat <- pull_synth_data(2012, 5, ca_geo)
# save(ca_dat, file= system.file("tests", "testdata", 'dat-cadat.rds'
#                                     , package= "synthACS"), version= 3)

# ca_transit <- pull_transit_work(2014, 5, ca_geo)
# towork <- gen_attr_vectors(ca_transit, "mode_transit_by_age")
# save(ca_dat, file= system.file("tests", "testdata", 'dat-towork.rds'
#                                , package= "synthACS"), version= 3)

# split_ca_dat <- split(ca_dat, n_splits = 7)
# syn <- derive_synth_datasets(split_ca_dat[[1]], leave_cores = 0)
# syn <- syn[c(2,4,6,8)]
# class(syn) <- c("synthACS", "list")