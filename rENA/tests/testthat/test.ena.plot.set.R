suppressMessages(library(rENA, quietly = F, verbose = F))
context("Test plotting sets")

data(RS.data)
codenames <- c("Data", "Technical.Constraints", "Performance.Parameters",
  "Client.and.Consultant.Requests", "Design.Reasoning", "Collaboration");

accum <- rENA:::ena.accumulate.data.file(
  RS.data, units.by = c("UserName", "Condition"),
  conversations.by = c("ActivityNumber", "GroupName"),
  codes = codenames
);
test_that("Test for top-level plot object", {
  set <- ena.make.set(accum)

  testthat::expect_null(set$model$plots)
  testthat::expect_is(set$plots, "list")
})
test_that("Create a plot object", {
  set <- ena.make.set(accum)

  newplot <- plot(set)

  testthat::expect_is(newplot, "ena.set")
  testthat::expect_is(newplot$plots[[1]], "ENAplot")
})

test_that("Plot all points", {
  newset <- ena.make.set(accum)

  newplot <- plot(newset) %>% add_points()

  testthat::expect_equal(nrow(newplot$plots[[1]]$plotted$points[[1]]$data), nrow(newset$points))
})

test_that("Plot some points", {
  accum <- rENA:::ena.accumulate.data.file(
    RS.data, units.by = c("UserName", "Condition"),
    conversations.by = c("ActivityNumber", "GroupName"),
    codes = codenames
  );
  newset <- ena.make.set(accum)

  newplot <- plot(newset) %>%
     add_points(Condition$FirstGame, colors = "blue")

  expected <- nrow(newset$points$Condition$FirstGame)
  observed <- nrow(newplot$plots[[1]]$plotted$points[[1]]$data)
  testthat::expect_equal(observed, expected)

  n_to_plot = 5
  newplot2 <- plot(newset) %>%
                add_points(as.matrix(
                  newset$points$Condition$FirstGame)[1:n_to_plot, ]
                )

  observed <- nrow(newplot2$plots[[1]]$plotted$points[[1]]$data)
  testthat::expect_equal(observed, 5)
})

test_that("Plot some points with mean from list", {
  accum <- rENA:::ena.accumulate.data.file(
    RS.data, units.by = c("UserName", "Condition"),
    conversations.by = c("ActivityNumber", "GroupName"),
    codes = codenames
  );
  newset <- ena.make.set(accum)

  newplot <- plot(newset) %>%
     add_points(Condition$FirstGame, colors = "blue", mean = list(colors = "red"))

  testthat::expect_equal(
    nrow(newplot$plots[[1]]$plotted$points[[1]]$data),
    nrow(newset$points$Condition$FirstGame)
  )
})

test_that("Plot a group", {
  accum <- rENA:::ena.accumulate.data.file(
    RS.data, units.by = c("UserName", "Condition"),
    conversations.by = c("ActivityNumber", "GroupName"),
    codes = codenames
  );
  newset <- ena.make.set(accum)

  newplot <- plot(newset) %>%
     add_group(Condition$FirstGame, colors = "blue")

  testthat::expect_equal(length(newplot$plots[[1]]$plotted$means[[1]]$data), 15)

  noplot = testthat::expect_warning(plot(newset) %>%
                          add_group(Condition$NoGame))
  noplot = testthat::expect_warning(plot(newset) %>%
                          add_group(Condition2$FirstGame))
})

test_that("Plot a network", {
  accum <- rENA:::ena.accumulate.data.file(
    RS.data, units.by = c("UserName", "Condition"),
    conversations.by = c("ActivityNumber", "GroupName"),
    codes = codenames
  );
  newset <- ena.make.set(accum)

  newplot <- plot(newset) %>% add_network(Condition$FirstGame)
  testthat::expect_equal(
    length(newplot$plots[[1]]$plotted$networks[[1]]),
    ncol(newset$rotation$adjacency.key)
  )

  newplot2 <- plot(newset) %>% add_network(with.mean = TRUE)
  testthat::expect_equal(
    length(newplot2$plots[[1]]$plotted$networks[[1]]),
    ncol(newset$rotation$adjacency.key)
  )
  testthat::expect_equal(length(newplot2$plots[[1]]$plotted$means), 1)

  newplot3 <- plot(newset) %>%
                add_network(Condition$FirstGame, with.mean = TRUE)
  testthat::expect_equal(
    length(newplot3$plots[[1]]$plotted$networks[[1]]),
    ncol(newset$rotation$adjacency.key)
  )

  wgts <- as.matrix(newset$line.weights$Condition$FirstGame)
  expect_equal(nrow(wgts), 26)
  newplot4 <- plot(newset) %>% add_network(wgts)
  testthat::expect_equal(
    length(newplot4$plots[[1]]$plotted$networks[[1]]),
    ncol(newset$rotation$adjacency.key)
  )


  newplot5 <- plot(newset) %>%
              add_network(
                Condition$FirstGame - Condition$SecondGame, with.mean = TRUE
              )
  testthat::expect_equal(length(newplot5$plots[[1]]$plotted$means), 2)
  testthat::expect_equal(
    length(newplot5$plots[[1]]$plotted$networks[[1]]),
    ncol(newset$rotation$adjacency.key)
  )
})

test_that("Plot a Trajectory", {
  accum <- rENA:::ena.accumulate.data.file(
    RS.data, units.by = c("UserName", "Condition"),
    conversations.by = c("ActivityNumber", "GroupName"),
    codes = codenames,
    model = "A"
  );
  newset <- ena.make.set(accum)

  newplot <- plot(newset) %>% add_trajectory("ENA_UNIT")
  testthat::expect_equal(
    nrow(newplot$plots[[1]]$plotted$trajectories[[1]]),
    length(unique(newset$points$ENA_UNIT))
  )

  newplot2 <- plot(newset) %>% add_trajectory()
  testthat::expect_equal(
    nrow(newplot2$plots[[1]]$plotted$trajectories[[1]]),
    length(unique(newset$points$ENA_UNIT))
  )

  newplot3 <- plot(newset) %>% add_trajectory(Condition$FirstGame)
  testthat::expect_equal(
    nrow(newplot3$plots[[1]]$plotted$trajectories[[1]]),
    length(unique(newset$points$Condition$FirstGame$ENA_UNIT))
  )
})

test_that("Test old plot object", {
  accum <- suppressWarnings({
    rENA:::ena.accumulate.data.file(
      RS.data, units.by = c("UserName", "Condition"),
      conversations.by = c("ActivityNumber", "GroupName"),
      codes = codenames, as.list = F
    )
  })
  set <- suppressWarnings({
    ena.make.set(accum, as.list = F)
  })

  testthat::expect_warning(ena.plot(set))

  plot <- suppressWarnings({ ena.plot(set) })
  plot <- plot %>% ena.plot.points()

  testthat::expect_is(plot, "ENAplot")
  testthat::expect_equal(
    length(plot$plot$x$attrs) - 1,
    length(set$enadata$unit.names)
  )
})
