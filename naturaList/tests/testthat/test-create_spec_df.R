data(spec_names_ex)

test_that("Create the collums correctly", {
  expect_equal(
    create_spec_df("Arthur Rodrigues"),
    data.frame(LastName = "Rodrigues", Name1 = "Arthur", Abbrev1 = "A")
    )

  expect_equal(
    create_spec_df("Arthur V Rodrigues"),
    data.frame(
      LastName = "Rodrigues",
      Name1 = "Arthur",
      Name2 = "",
      Abbrev1 = "A",
      Abbrev2 = "V")
  )

  expect_equal(
    create_spec_df("Arthur Vinicius Rodrigues"),
    data.frame(
      LastName = "Rodrigues",
      Name1 = "Arthur",
      Name2 = "Vinicius",
      Abbrev1 = "A",
      Abbrev2 = "V")
  )

})

test_that("Create the rows correctly when there is some accent mark", {
  expect_equal(
    create_spec_df("Arthur Vinícius Rodrigues"),
    data.frame(
      LastName = rep("Rodrigues", 2),
      Name1 = rep("Arthur", 2),
      Name2 = c("Vinícius", "Vinicius"),
      Abbrev1 = rep("A", 2),
      Abbrev2 = rep("V", 2)
      )
  )
})

test_that("Create the cols correctely when have some abbreviated name",{
  expect_equal(
    create_spec_df(c("Arthur V Rodrigues", "Gabriel Nakamura")),
    data.frame(
      LastName = c("Nakamura", "Rodrigues"),
      Name1 = c("Gabriel", "Arthur"),
      Name2 = c("", ""),
      Abbrev1 = c("G", "A"),
      Abbrev2 = c("", "V")
    )
  )
})

test_that("Keep the upper and lower case as specified by the user and ignore lower case in 'Abbrev' columns", {
  expect_equal(
    create_spec_df("Arthur de Rodrigues"),
    data.frame(
      LastName = "Rodrigues",
      Name1 = "Arthur",
      Name2 = "de",
      Abbrev1 = "A"
      )
    )
})


