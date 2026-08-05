data <- data.frame(
    ID = c("example1", "example2"),
    C0 = c(302, 160),
    C1 = c(28, 55),
    C2 = c(22, 19),
    C3 = c(8, 17),
    C4 = c(1, 9),
    C5 = c(0, 4)
)


calculate_aberr_table(data,
                      type = "case",
                      aberr_module = "dicentrics",
                      assessment_u = 1)
