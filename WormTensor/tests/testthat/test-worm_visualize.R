# Temporary directory to save figures
out.dir <- tempdir()

######### without Labels #########
worm_download()$Ds |>
    as_worm_tensor() |>
        worm_membership(k = 6) |>
            worm_clustering() |>
                worm_evaluate() |>
                    worm_visualize(out.dir) -> object

filename1 <- paste0(out.dir, "/figures/Silhouette.png")
expect_true(file.exists(filename1))

filename2 <- paste0(out.dir, "/figures/Cluster.png")
expect_true(file.exists(filename2))

filename3 <- paste0(out.dir, "/figures/no_identified.png")
expect_true(file.exists(filename3))

filename4 <- paste0(out.dir, "/figures/weight_ARI_no.png")
expect_true(file.exists(filename4))

######### with Labels #########
labels <- list(
    label1 = sample(3, length(object@clustering), replace = TRUE),
    label2 = sample(4, length(object@clustering), replace = TRUE),
    label3 = sample(5, length(object@clustering), replace = TRUE)
)

worm_download()$Ds |>
    as_worm_tensor() |>
        worm_membership(k = 6) |>
            worm_clustering() |>
                worm_evaluate(labels) |>
                    worm_visualize(out.dir) -> object_labels

filename1 <- paste0(out.dir, "/figures/Silhouette.png")
expect_true(file.exists(filename1))

filename2 <- paste0(out.dir, "/figures/Cluster.png")
expect_true(file.exists(filename2))

filename3 <- paste0(out.dir, "/figures/no_identified.png")
expect_true(file.exists(filename3))

filename4 <- paste0(out.dir, "/figures/weight_ARI_no.png")
expect_true(file.exists(filename4))

filename5 <- paste0(out.dir, "/figures/consistency_label1.png")
expect_true(file.exists(filename5))

filename6 <- paste0(out.dir, "/figures/consistency_label2.png")
expect_true(file.exists(filename6))

filename7 <- paste0(out.dir, "/figures/consistency_label3.png")
expect_true(file.exists(filename7))

filename8 <- paste0(out.dir, "/figures/Class_label1.png")
expect_true(file.exists(filename8))

filename9 <- paste0(out.dir, "/figures/Class_label2.png")
expect_true(file.exists(filename9))

filename10 <- paste0(out.dir, "/figures/Class_label3.png")
expect_true(file.exists(filename10))

######### CSPA (without Labels) #########
worm_download()$Ds |>
    as_worm_tensor() |>
        worm_membership(k = 6) |>
            worm_clustering("CSPA") |>
                worm_evaluate() |>
                    worm_visualize(out.dir) -> object

filename1 <- paste0(out.dir, "/figures/Silhouette.png")
expect_true(file.exists(filename1))

filename2 <- paste0(out.dir, "/figures/Cluster.png")
expect_true(file.exists(filename2))

filename3 <- paste0(out.dir, "/figures/no_identified.png")
expect_true(file.exists(filename3))

######### OINDSCAL (without Labels) #########
worm_download()$Ds |>
    as_worm_tensor() |>
        worm_membership(k = 6) |>
            worm_clustering("OINDSCAL") |>
                worm_evaluate() |>
                    worm_visualize(out.dir) -> object

filename1 <- paste0(out.dir, "/figures/Silhouette.png")
expect_true(file.exists(filename1))

filename2 <- paste0(out.dir, "/figures/Cluster.png")
expect_true(file.exists(filename2))

filename3 <- paste0(out.dir, "/figures/no_identified.png")
expect_true(file.exists(filename3))
