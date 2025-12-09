### * Setup

new_networkModel <- function() {
    isotracer::new_networkModel(quiet = TRUE)
}

### * priors()

test_that("priors() works as expected", {
    exp <- tibble::tribble(
  ~time.day,  ~species, ~biomass, ~prop15N, ~treatment,
          0,     "NH4",    0.205,    0.739,    "light",
          2,     "NH4",    0.232,    0.403,    "light",
          4,     "NH4",       NA,    0.199,    "light",
          6,     "NH4",       NA,    0.136,    "light",
          8,     "NH4",    0.306,       NA,    "light",
         10,     "NH4",    0.323,   0.0506,    "light",
          0,   "algae",    0.869,  0.00305,    "light",
          2,   "algae",       NA,   0.0875,    "light",
          4,   "algae",     0.83,    0.131,    "light",
          6,   "algae",    0.706,       NA,    "light",
         10,   "algae",    0.666,   0.0991,    "light",
          0, "daphnia",     2.13,  0.00415,    "light",
          2, "daphnia",     1.99,       NA,    "light",
          4, "daphnia",     1.97,   0.0122,    "light",
          6, "daphnia",       NA,   0.0284,    "light",
          8, "daphnia",       NA,   0.0439,    "light",
         10, "daphnia",      1.9,   0.0368,    "light",
          0,     "NH4",    0.474,     0.98,     "dark",
          2,     "NH4",    0.455,     0.67,     "dark",
          4,     "NH4",    0.595,    0.405,     "dark",
          6,     "NH4",       NA,    0.422,     "dark",
         10,     "NH4",    0.682,    0.252,     "dark",
          0,   "algae",     1.06,  0.00455,     "dark",
          2,   "algae",        1,   0.0637,     "dark",
          4,   "algae",    0.862,   0.0964,     "dark",
          6,   "algae",       NA,    0.222,     "dark",
          8,   "algae",       NA,    0.171,     "dark",
         10,   "algae",    0.705,    0.182,     "dark",
          0, "daphnia",     1.19,  0.00315,     "dark",
          4, "daphnia",     1.73,   0.0204,     "dark",
          6, "daphnia",     1.75,       NA,     "dark",
          8, "daphnia",     1.54,   0.0598,     "dark",
         10, "daphnia",     1.65,   0.0824,     "dark"
  )
    inits <- exp %>% dplyr::filter(time.day == 0)
    obs <- exp %>% dplyr::filter(time.day > 0)
    m <- new_networkModel() %>%
        set_topo("NH4 -> algae -> daphnia -> NH4") %>%
        set_init(inits, comp = "species", size = "biomass", prop = "prop15N",
                 group_by = "treatment") %>%
        set_obs(obs, comp = "species", size = "biomass", prop = "prop15N",
                time = "time.day", group_by = "treatment")
    p <- priors(m)
    expect_is(p, c("tbl_df", "tbl", "data.frame"))
    expect_equal(dim(p), c(8, 2))
    expect_setequal(p$in_model, c("eta", "lambda_algae", "lambda_daphnia",
                                  "lambda_NH4", "upsilon_algae_to_daphnia",
                                  "upsilon_daphnia_to_NH4",
                                  "upsilon_NH4_to_algae", "zeta"))
})
