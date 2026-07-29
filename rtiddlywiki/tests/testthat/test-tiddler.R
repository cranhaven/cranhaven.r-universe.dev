test_that("tiddler", {
    skip_if(!is_test_tw())
    expect_null(get_tiddler("test-missing"))
    expect_no_error(delete_tiddler("test1"))

    # test in text
    expect_error(put_tiddler("test00",
                                type = "text/x-tiddlywiki",
                                tags = c("TAG 1", "TAG2"),
                                fields = list("f1" = "f1", "f2" = "f 2")))

    expect_no_error(put_tiddler("test0", text = c("", ""),
                                type = "text/x-tiddlywiki",
                                tags = c("TAG 1", "TAG2"),
                                fields = list("f1" = "f1", "f2" = "f 2")))
    expect_no_error(put_tiddler("test0",
                                type = "text/x-tiddlywiki",
                                tags = c("TAG 1", "TAG2"),
                                fields = list("f1" = "f1", "f2" = "f 2")))


    expect_no_error(put_tiddler("test1", "This is a test tiddler",
                                type = "text/x-tiddlywiki",
                                tags = c("TAG 1", "TAG2"),
                                fields = list("f1" = "f1", "f2" = "f 2")))
    new_tiddler <- get_tiddler("test1")
    expect_equal(new_tiddler$title, "test1")
    expect_equal(new_tiddler$text, "This is a test tiddler")
    expect_equal(new_tiddler$type, "text/x-tiddlywiki")
    expect_equal(new_tiddler$tags, c("TAG 1", "TAG2"))
    expect_equal(new_tiddler$fields$f1, "f1")
    expect_equal(new_tiddler$fields$f2, "f 2")

    title = "test1"
    text = "This is a test tiddler"
    type = "text/x-tiddlywiki"
    tags = c("TAG 3", "TAG2")
    fields =  list("f2" = "f 2 again", "f3" = "f 3")
    expect_no_error(put_tiddler(title = "test1"))

    expect_no_error(put_tiddler(title = "test1",
                                text = text,
                                tags = c("TAG 3", "TAG2"),
                                fields = list("f2" = "f 2 again", "f3" = "f 3")))
    new_tiddler <- get_tiddler("test1")
    expect_equal(new_tiddler$title, "test1")
    expect_equal(new_tiddler$text, "This is a test tiddler")
    expect_equal(new_tiddler$type, "text/vnd.tiddlywiki")
    expect_equal(new_tiddler$tags, c("TAG 3", "TAG2"))
    expect_equal(new_tiddler$fields$f1, "f1")
    expect_equal(new_tiddler$fields$f2, "f 2 again")
    expect_equal(new_tiddler$fields$f3, "f 3")
    tiddlers <- get_tiddlers("[all[tiddlers]!is[system]sort[title]]")
    expect_true(length(tiddlers) > 0)

    expect_no_error(put_tiddler(title = "test1",
                                text = text,
                                fields = list("f2" = "f 2")))

    new_tiddler <- get_tiddler("test1")
    expect_equal(new_tiddler$title, "test1")
    expect_equal(new_tiddler$text, "This is a test tiddler")
    expect_equal(new_tiddler$type, "text/vnd.tiddlywiki")
    expect_equal(new_tiddler$tags, c("TAG 3", "TAG2"))
    expect_equal(new_tiddler$fields$f1, "f1")
    expect_equal(new_tiddler$fields$f2, "f 2")


    # Remove an existing field
    remove_fields("test1", "f1")
    expect_error(remove_fields("test1", "f1"))
    expect_error(remove_fields("test13333", "f33"))
    tiddler <- get_tiddler("test1")
    expect_equal(tiddler$fields$f1, NULL)
    expect_equal(tiddler$fields$f2, "f 2")
    expect_equal(tiddler$fields$f3, "f 3")


    # Test more field choices
    expect_no_error(put_tiddler("test3", "",
                                type = "text/x-tiddlywiki",
                                tags = c("TAG 1", "TAG2"),
                                fields = list("f1" = "", "f2" = "f 2")))
    new_tiddler <- get_tiddler("test3")
    expect_equal(new_tiddler$title, "test3")
    expect_equal(new_tiddler$fields$f1, "")

    expect_no_error(put_tiddler("test4", "",
                                type = "application/json",
                                tags = c("TAG 1", "TAG2"),
                                fields = list("f1" = "", "f2" = "f 2")))
    new_tiddler <- get_tiddler("test4")
    expect_equal(new_tiddler$title, "test4")
    expect_equal(new_tiddler$type, "application/json")
    expect_equal(new_tiddler$fields$f1, "")


    expect_no_error(put_tiddler("test4", "",
                                type = "application/json",
                                tags = c("TAG 1", "TAG2"),
                                fields = list("f1" = "", "f2" = c("V1", "V2", "V 4"))))
    new_tiddler <- get_tiddler("test4")
    expect_equal(new_tiddler$title, "test4")
    expect_equal(new_tiddler$type, "application/json")
    expect_equal(new_tiddler$fields$f1, "")
    expect_equal(new_tiddler$fields$f2, "V1 V2 [[V 4]]")


    expect_error(put_tiddler("test5", "",
                                type = "application/json",
                                tags = c("TAG 1", "TAG2"),
                                fields = list("f1" = "", "f2" = list("V1", "V2", "V 4"))))

    expect_error(put_tiddler("test5", "",
                             type = "application/json",
                             tags = c("TAG 1", "TAG2"),
                             fields = list("f1" = "", "f2" = data.frame(a=c(1,2)))))


    expect_no_error(delete_tiddler("test1"))
    expect_no_error(delete_tiddler("test3"))
    expect_no_error(delete_tiddler("test4"))

    # check title

    titles <- c("tiddler with space", "tiddler with space :",
                "/tiddler with space /")

    for (i in seq(along = titles)) {
        expect_no_error(put_tiddler(titles[i], "",
                                type = "text/vnd.tiddlywiki"))

        expect_no_error(delete_tiddler(titles[i]))
    }

    titles <- c("tiddler with space", "tiddler with space :",
                "/tiddler with space /")
    for (i in seq(along = titles)) {
        expect_error(put_tiddler(titles[i], "",
                                 type = "application/json",
                                 tags = c("TAG 1", "TAG2"),
                                 fields = list("f1" = "", "f2" = list("V1", "V2", "V 4"))))

        expect_no_error(delete_tiddler(titles[i]))
    }


    # Test tiddler title with space 
    expect_no_error(put_tiddler("test tiddler", "This is a test tiddler"))
    expect_no_error(new_tiddler <- get_tiddler("test tiddler"))
    expect_equal(new_tiddler$title, "test tiddler")
    expect_no_error(delete_tiddler("test tiddler"))
})


