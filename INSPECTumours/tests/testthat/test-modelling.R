context("Modelling")

test_that(
  "get_responder() returns 'Non-responder' if all statuses are 'Non-responder'", {
    one_animal_data <- rep("Non-responder", 5)
    expect_equal(get_responder(one_animal_data, 5), "Non-responder")
  }
)

test_that(
  "get_responder() returns Responder if number of consecutive Responders >= n", {
    one_animal_data <-
      c(
        "Responder",
        "Responder",
        "Non-responder",
        "Non-responder",
        "Non-responder",
        "Responder"
      )
    expect_equal(get_responder(one_animal_data, n = 3), "Non-responder")
    expect_equal(get_responder(one_animal_data, n = 2), "Responder")
  }
)
