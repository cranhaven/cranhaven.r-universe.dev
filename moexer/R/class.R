append_class <- function(x, class_name) {
    assert_that(is.string(class_name), !is.na(class_name))
    class(x) <- c(class_name, class(x))
    x
}
