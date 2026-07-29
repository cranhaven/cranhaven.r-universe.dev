test_that(".tiddler_json", {
    expect_error(tiddler_json(title = c("A", "B"), text = "A"))
    expect_error(tiddler_json(title = c("A"), text = "A", type = "A"))

    expect_error(tiddler_json(title = c("A"), text = "A",
                               type = "text/vnd.tiddlywiki",
                               fields = c("A")))

    expect_error(tiddler_json(title = c("A"), text = "A",
                               type = "text/vnd.tiddlywiki",
                               fields = c("N1" = "A", "B")))

    tj <- tiddler_json(title = c("A"), text = c("t1", "t2"),
                  type = "text/vnd.tiddlywiki",
                  tags = c("T1", "T2"),
                  fields = list("N1" = "A", "N2" = "B"))

    expect_false(grepl('\\[\\"', tj))

    tj2 <- jsonlite::fromJSON(tj)
    expect_equal(tj2$title, "A")
    expect_equal(grepl("t1", tj2$text), TRUE)
    expect_equal(tj2$tags, "[[T1]] [[T2]]")
    expect_equal(tj2$fields$N1, "A")
    expect_equal(tj2$fields$N2, "B")
})
