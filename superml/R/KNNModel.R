#
# KNNModel <- R6Class(classname = 'KNNModel', public = list(
#
#     algorithm = NA,
#     epsilon = NA,
#     input_model = NA,
#     k = NA,
#     leaf_size = NA,
#     query = NA,
#     random_basis = FALSE,
#     reference = NA,
#     rho = NA,
#     seed = NA,
#     tau = NA,
#     tree_type = NA,
#     true_distances = NA,
#     true_neighbors = NA,
#     verbose = FALSE
#
#
#     initialize = function(){
#
#     },
#     fit = function(){
#         mlpack::knn()
#     },
#     predict = function(){
#
#     }
# ))
#
#
# data <- datasets::attitude
# library(mlpack)
# library(data.table)
# model = krann(k=3,)
#
#
# df <- fread("https://www.mlpack.org/datasets/covertype-small.csv.gz")
# # Split the labels.
# labels <- df[, .(label)]
# dataset <- df[, label:=NULL]
# # Split the dataset using mlpack.
# prepdata <- preprocess_split(input = dataset,
#                              input_labels = labels,
#                              test_ratio = 0.3,
#                              verbose = TRUE)
# # Train a random forest.
# model <- random_forest(training = prepdata$training,
#                         labels = prepdata$training_labels,
#                         print_training_accuracy = TRUE,
#                         num_trees = 10,
#                         minimum_leaf_size = 3,
#                         verbose = TRUE)
#
# model <- decision_tree(training = prepdata$training,
#                         labels = prepdata$training_labels,
#                         print_training_accuracy = TRUE,
#                         verbose = TRUE)
# pred = decision_tree(input_model = model$output_model,
#                      test = prepdata$test,
#                      test_labels = prepdata$test_labels)
# length(pred)
# lapply(pred, length)
#
#
# model <- softmax_regression(training=prepdata$training,
#                             labels=prepdata$training_labels,
#                             number_of_classes = 7)
#
# pred = softmax_regression(input_model = model$output_model,
#                      test = prepdata$test,
#                      test_labels = prepdata$test_labels)
# lapply(pred, length)
#
#
# model <- bayesian_linear_regression(input = prepdata$training,
#                                  responses = prepdata$training_labels,center=1, scale=0)
#
# pred = bayesian_linear_regression(input_model = model$output_model,
#                           test = prepdata$test)
# lapply(pred, length)
#
#
# output <- kmeans(input=prepdata$training, clusters=4)
# lapply(output, length)
#
#
#
#
# # keras word2vec
# install_tensorflow()
# library(keras)
# library(tensorflow)
#
# keras::text_one_hot("how are you")
# sentences = c("how are you", "wanna go with me")
#
# keras::skipgrams()
# tok = keras::text_tokenizer(num_words = )
# tok$
# tok$texts_to_matrix(texts = c("how are you", "jam here you"),)
#
# cnt = CountVectorizer$new(min_df = 0.01)
# cnt$fit(c("how are you", "jam here you"))
# cnt$model
#
