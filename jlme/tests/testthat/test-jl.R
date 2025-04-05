skip_conditionally()

system.time({
  stop_julia()
  print(JuliaConnectoR::startJuliaServer())
})

test_that("Basic interop functions work", {

  # `jl()` eval string
  expect_true(is_jl(jl("1")))
  expect_identical(jl("1 .+ [1, 3]", .R = TRUE), 1L + c(1L, 3L))
  expect_identical(jl("1 .+ [1, 3]", .R = TRUE), jl_get(jl("1 .+ [1, 3]")))
  expect_identical(jl("(1, )", .R = TRUE), list(1L))
  expect_true(is_jl(jl(jl("(1, )"), .passthrough = TRUE)))

  # round-trip
  expect_identical(1L, jl_get(jl_put(1L)))

  # `jl()` interpolation
  expect_identical(jl("%i", 1, .R = TRUE), jl("x", x = 1L, .R = TRUE))

  # Auto protect vectors unless `I()`
  dict <- jl_get(jl_dict(a = 1, b = I(2)))
  expect_identical(sapply(dict$values, attr, "JLDIM"), list(1L, NULL))

  # Auto splice
  expect_identical(dict, jl_get(jl_dict(list(a = 1, b = I(2)))))

})

stop_julia()
