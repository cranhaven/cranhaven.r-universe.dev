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

df_animal <- calculate_score_animals(toy_df)
df_animal_indicate <- calculate_score_animals(toy_df,
                                              threshold = 4,
                                              indicate = TRUE)
df_animal_discard <- calculate_score_animals(toy_df,
                                              threshold = 4,
                                              discard = TRUE)

test_that("Tests calculating scores for animals", {
    expect_gte(ncol(df_animal), ncol(toy_df))
    expect_gte(ncol(df_animal_indicate), ncol(df_animal))
    expect_lte(nrow(df_animal_discard), nrow(df_animal))
})

df_pat <- calculate_score_patients(toy_df)
df_pat_indicate <- calculate_score_patients(toy_df,
                                            threshold = 4,
                                            indicate = TRUE)
df_pat_discard <- calculate_score_patients(toy_df,
                                           threshold = 4,
                                           discard = TRUE)

test_that("Tests calculating scores for patients", {
    expect_gte(ncol(df_pat), ncol(toy_df))
    expect_gte(ncol(df_pat_indicate), ncol(df_pat))
    expect_lte(nrow(df_pat_discard), nrow(df_pat))
})

df_bio <- calculate_score_biomarker(toy_df)
df_bio_indicate <- calculate_score_biomarker(toy_df,
                                             threshold = 10,
                                             indicate = TRUE)
df_bio_discard <- calculate_score_biomarker(toy_df,
                                            threshold = 10,
                                            discard = TRUE)

test_that("Tests calculating scores for biomarkers", {
    expect_gte(ncol(df_bio), ncol(toy_df))
    expect_gte(ncol(df_bio_indicate), ncol(df_bio))
    expect_lte(nrow(df_bio_discard), nrow(df_bio))
})


df_own <- calculate_score_topic(toy_df, keywords = own_keywords)
df_own_indicate <- calculate_score_topic(toy_df,
                                         keywords = own_keywords,
                                         col.indicate = "Present",
                                         threshold = 4)
df_own_discard <- calculate_score_biomarker(toy_df,
                                            keywords = own_keywords,
                                            threshold = 4,
                                            discard = TRUE)

test_that("Tests calculating scores for own topics", {
    expect_gte(ncol(df_own), ncol(toy_df))
    expect_gte(ncol(df_own_indicate), ncol(df_own))
    expect_lte(nrow(df_own_discard), nrow(df_own))
})
