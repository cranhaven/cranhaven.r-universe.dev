
#allowed methods in the package

am <- c('reference','adjbox', 'zscore','kmeans', 'iforest', 'distboxplot','optimal',
                    'mixediqr', 'seqfences', 'mahal', 'medianrule', 'iqr','hampel',
                    'logboxplot', 'onesvm', 'jknife', 'semiqr', 'lof','glosh', 'knn')

test_that(desc = "check if methods are printed",
          code = {
            #output is a list of 7 categories

            expect_type(extractMethods(), 'list')
            #7 categories
            expect_equal(length(extractMethods()), 7)

            #check if allowed methods are same as extracted methods

            ex <- extractMethods() #a list of allowed methods and expect true

            expect_true(all(am %in% do.call(c, ex)))
          })
