# Pipe Operation
worm_download()$Ds |>
    as_worm_tensor() |>
        worm_membership(k = 6) |>
            worm_clustering() -> object

######### Internal Validity Indices (without Labels) #########
worm_evaluate(object) -> object_internal

expect_equal(length(object_internal@eval$internal), 3)
expect_true(is.numeric(object_internal@eval$internal$PseudoF))
expect_true(is.numeric(object_internal@eval$internal$Connectivity))
expect_true(is.numeric(object_internal@eval$internal$silhouette))

######### External Validity Indices (with Labels) #########
labels <- list(
    label1 = sample(3, length(object@clustering), replace = TRUE),
    label2 = sample(4, length(object@clustering), replace = TRUE),
    label3 = sample(5, length(object@clustering), replace = TRUE)
)

worm_evaluate(object, labels) -> object_external

expect_true(length(object_external@eval$external) == 3)
expect_true(
    all(names(object_external@eval$external$label1) %in%
        c("Fmeasure", "Entropy", "Purity", "ARI"))
)
expect_true(
    all(names(object_external@eval$external$label2) %in%
        c("Fmeasure", "Entropy", "Purity", "ARI"))
)
expect_true(
    all(names(object_external@eval$external$label3) %in%
        c("Fmeasure", "Entropy", "Purity", "ARI"))
)
expect_true(
    all(is.numeric(unlist(object_external@eval$external$label1)))
)
expect_true(
    all(is.numeric(unlist(object_external@eval$external$label2)))
)
expect_true(
    all(is.numeric(unlist(object_external@eval$external$label3)))
)
# Data for each cell
expect_true(length(object_external@eval$cellwise) == 3)
# Data for weight_ARI_no.png
expect_true(length(object_external@eval$each_animal) == 4)
