
#' Constructs a nearest neighbours-based classifier and returns the predictions
#' for a test set
#'
#' \code{knn_classifier} returns the predictions for a test set concerning a
#' nearest neighbours-based classifier.
#'
#' @param dataset A list of MTS (numerical matrices).
#' @param classes A vector containing the class labels associated with the
#' elements in \code{dataset}.
#' @param index_test The indexes associated with the test elements in \code{dataset},
#' i.e., the elements for which predictions will be computed.
#' @param distance The corresponding distance measure to compute the
#' nearest neighbours-based classifier (must be one the functions implemented
#' in \pkg{mlmts}, as a string).
#' @param k The number of neighbours.
#' @param ... Additional parameters for the function with respect to the considered
#' distance.
#' @return The class labels for the elements in the test set.
#' @examples
#' predictions_1_nn <- knn_classifier(BasicMotions$data[1 : 10], BasicMotions$classes[1 : 10],
#' index_test = 6 : 10, distance = 'dis_modwt', k = 1) # Computing the
#' # predictions for the test elements in dataset BasicMotions according to
#' # a 1-nearest neighbour classifier based on dis_modtw.
#' predictions_1_nn
#' @details
#' Given a collection of MTS containing the training and test set,
#' the function constructs a nearest neighbours-based classifier
#' based on a given dissimilarity measure. The corresponding predictions
#' for the elements in the test set are returned.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @export


knn_classifier <- function(dataset, classes, index_test, distance, k,...) {


  # Getting the distance function

  distance_function <- get(distance) # The distance function


  # Training and testing sets

  training_set <- dataset[-index_test]
  test_set <- dataset[index_test]
  l_training <- length(training_set)
  l_test <- length(test_set)
  classes_training <- classes[-index_test]


  # Distance matrix between training and testing sets

  distance_matrix <- matrix(0, nrow = l_test, ncol = l_training)

  for (i in 1 : l_test) {
    for(j in 1 : l_training) {

      list_dataset <- list(test_set[[i]], training_set[[j]])
      distance_matrix[i, j] <- as.numeric(distance_function(list_dataset,...))

    }

  }


  # kNN classifier

  test_set_predictions <- numeric()

  for (i in 1 : l_test) {

    nearest_neighbours_indexes <- order(distance_matrix[i,])[1 : k]
    nearest_neighbours_classes <- classes_training[nearest_neighbours_indexes]

    if (length(as.numeric(DescTools::Mode(nearest_neighbours_classes))) > 1) {

      test_set_predictions[i] <- sample(as.numeric(DescTools::Mode(nearest_neighbours_classes)), size = 1)

    } else {

    test_set_predictions[i] <- as.numeric(DescTools::Mode(nearest_neighbours_classes))

    }

  }

  return(test_set_predictions)




}
