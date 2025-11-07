library(miRetrieve)
library(testthat)

own_keywords <- c("test", "test2", "test3")

generate_score_abstracts <- function(animal, patient, biomarker, own) {

    abstract <- c(sample(miRetrieve::animal_keywords, size = animal, replace = TRUE),
                  sample(miRetrieve::patients_keywords, size = patient, replace = TRUE),
                  sample(miRetrieve::biomarker_keywords, size = biomarker, replace = TRUE),
                  sample(own_keywords, size = own, replace = TRUE))

    return(paste0(abstract, collapse = " "))
}

toy_df <- data.frame("Abstract" = c(generate_score_abstracts(3,6,3, 2),
                                    generate_score_abstracts(4,0,4, 1),
                                    generate_score_abstracts(2,10,7, 5),
                                    generate_score_abstracts(6,8,9, 3),
                                    generate_score_abstracts(2,3,3, 1)),
                     "PMID" = seq(1:5))

plot_animal <- plot_score_animals(toy_df,
                                  title = "Test_plot",
                                  colour = "red")

plot_patients <- plot_score_patients(toy_df,
                                     title = "Test_plot",
                                     colour = "red")

plot_biomarker <- plot_score_biomarker(toy_df,
                                       title = "Test_plot",
                                       colour = "red")

plot_own <- plot_score_topic(toy_df,
                             keywords = own_keywords,
                             title = "Test_plot",
                             colour = "red")

test_that("Tests plotting scores", {
    expect_equal(typeof(plot_animal), "list")
    expect_equal(typeof(plot_patients), "list")
    expect_equal(typeof(plot_biomarker), "list")
    expect_equal(typeof(plot_own), "list")
    expect_equal(plot_animal$labels$title, "Test_plot")
    expect_equal(plot_patients$labels$title, "Test_plot")
    expect_equal(plot_biomarker$labels$title, "Test_plot")
    expect_equal(plot_own$labels$title, "Test_plot")
})
