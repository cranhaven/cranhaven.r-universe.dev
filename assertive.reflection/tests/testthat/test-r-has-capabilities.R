test_that(
  "test.r_has_capabilities.returns_true_if_r_has_capabilities",
  {
    caps <- capabilities()
    fns <- paste(
      "r_has", 
      sub("[/.]", "_", tolower(names(caps))), 
      "capability",
      sep = "_"
    )
    for(i in seq_along(caps))
    {
      actual <- get(fns[i], envir = as.environment("package:assertive.reflection"))()
      
      if(caps[i])
      {
        expect_true(actual, info = fns[i])
      } else
      {
        expect_false(actual)
        expect_equal(
          cause(actual), 
          noquote(
            paste(
              "R does not have",
              names(caps)[i],
              "capability."
            )
          )
        )
      }      
    }
  }
)

if(as.package_version(version) < "4.1.0") {
  test_that(
    "test.r_has_capabilities.returns_na_for_undeclared_capability", 
    {
      actual <- r_has_rprof_capability()
      expect_equal(strip_attributes(actual), NA)
      expect_equal(
        cause(actual), 
        noquote("Rprof capability is not declared for versions of R before 4.1.0.")
      )
    }
  )
}