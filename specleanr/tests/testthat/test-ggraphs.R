
#all data combined for all species
soutl <- multidetect(data = iris, var = 'Sepal.Width',
                 multiple = FALSE, exclude = "Species",
                 methods = c('mixediqr', 'logboxplot','lof',
                             'distboxplot','iqr', 'semiqr','adjbox',
                             'zscore', 'hampel'), silence_true_errors = TRUE)

#multiple species per species
moutl <- multidetect(data = iris, var = 'Sepal.Width',
                      multiple = TRUE, var_col  = "Species",
                      methods = c('mixediqr', 'logboxplot','lof'), silence_true_errors = TRUE)

test_that(desc = "count the classes produced",
          code = {
            expect_equal(length(class(ggoutliers(soutl))), 5)

          })
