test_that("is_combined correctly identifies combined nodes", {
  data(klass_131_graph)
  graph <- klass_131_graph

  ## Not using `compare_node`

  # Two parent nodes: 0103 Fredrikstad and 0113 Borge

  expect_true(is_combined(
    graph = graph,
    node = klass_node(graph, "0106", "1994-01-01")
  ))

  # One parent node through two edges: 0113

  expect_false(is_combined(
    graph = graph,
    klass_node(graph, "0113", "1838-01-01")
  ))

  ## Using `compare_node`

  # 0106 Fredrikstad

  expect_true(is_combined(
    graph = graph,
    node = klass_node(graph, "0106", "1994-01-01"),
    compare_node = klass_node(graph, "0103", "1964-01-01")
  ))

  expect_false(is_combined(
    graph = graph,
    node = klass_node(graph, "3004", "2020-01-01"),
    compare_node = klass_node(graph, "0106", "1994-01-01")
  ))

  # 1036 Fjotland

  expect_true(is_combined(
    graph = graph,
    node = klass_node(graph, "4227", "2020-01-01"),
    compare_node = klass_node(graph, "1036", "1840-01-01")
  ))
})
