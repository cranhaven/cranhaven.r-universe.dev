test_that("Define a sampler object and test all options", {
  tmp_sampler <- sampler.iglm(
    init_empty = TRUE, n_burn_in = 10, n_simulation = 20,
    seed = 42,
    sampler_x = sampler.net.attr(n_proposals = 10),
    sampler_y = sampler.net.attr(n_proposals = 20),
    sampler_z = sampler.net.attr(n_proposals = 30)
  )
  expect_equal(tmp_sampler$n_burn_in, 10)
  expect_equal(tmp_sampler$n_simulation, 20)
  expect_equal(tmp_sampler$seed, 42)
  expect_equal(inherits(tmp_sampler$sampler_x, "sampler.net.attr"), TRUE)
  expect_equal(inherits(tmp_sampler$sampler_y, "sampler.net.attr"), TRUE)
  expect_equal(inherits(tmp_sampler$sampler_z, "sampler.net.attr"), TRUE)
  expect_equal(tmp_sampler$sampler_x$n_proposals, 10)
  expect_equal(tmp_sampler$sampler_y$n_proposals, 20)
  expect_equal(tmp_sampler$sampler_z$n_proposals, 30)
  tmp_name <- paste(tempfile(), ".RDS")
  tmp_sampler$save(file = tmp_name)
  rm(tmp_sampler)

  loaded_sampler <- sampler.iglm(file = tmp_name)
  expect_equal(loaded_sampler$n_burn_in, 10)
  expect_equal(loaded_sampler$n_simulation, 20)
  expect_equal(loaded_sampler$seed, 42)
  expect_equal(inherits(loaded_sampler$sampler_x, "sampler.net.attr"), TRUE)
  expect_equal(inherits(loaded_sampler$sampler_y, "sampler.net.attr"), TRUE)
  expect_equal(inherits(loaded_sampler$sampler_z, "sampler.net.attr"), TRUE)
  expect_equal(loaded_sampler$sampler_x$n_proposals, 10)
  expect_equal(loaded_sampler$sampler_y$n_proposals, 20)
  expect_equal(loaded_sampler$sampler_z$n_proposals, 30)
  file.remove(tmp_name)
})

test_that("sampler.iglm defaults init_empty to FALSE and uses fixed default seed when seed = NA", {
  s1 <- sampler.iglm()
  s2 <- sampler.iglm()
  expect_false(s1$init_empty)
  expect_false(s2$init_empty)
  expect_equal(s1$seed, 123456789)
  expect_equal(s2$seed, 123456789)

  gen1 <- sampler.iglm.generator$new()
  gen2 <- sampler.iglm.generator$new()
  expect_false(gen1$init_empty)
  expect_false(gen2$init_empty)
  expect_equal(gen1$seed, 123456789)
  expect_equal(gen2$seed, 123456789)
})

