context("Exclude data")

test_that(
  "below_min_points() returns rows with one animal_id that occurs
  >= min_points but belongs to different studies", {
    df <- data.frame(animal_id = rep(4, 8),
                     study = as.character(1:8))

    expect_equal(below_min_points(df, min_points = 2),
                 df %>% mutate(reason = "few_data_points"))

    expect_equal(below_min_points(df, min_points = 8),
                 df %>% mutate(reason = "few_data_points"))
  })

test_that(
  "below_min_points returns empty df if there is >= min_points
  for each animal_id per study", {
    df <- data.frame(animal_id = c(1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1),
                     study = c(rep("study 1", 8), rep("study 2", 4)))

    expect_equal(nrow(below_min_points(df, min_points = 4)), 0)
  }
)
