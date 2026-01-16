#' @title Distance-Based Common Spatial Patterns
#'
#' @description dbcsp is a package which offers a way to apply Distance-Based Common Spatial Patterns (DB-CSP) techniques in different fields, both classical Common Spatial Patterns (CSP) as well as DB-CSP.
#'
#' @seealso \code{\link[=dbcsp-class]{dbcsp}}, \code{\link[=print.dbcsp]{print}}, \code{\link[=summary.dbcsp]{summary}}, \code{\link[=train.dbcsp]{train}}, \code{\link{selectQ}}, \code{\link[=predict.dbcsp]{predict}}, \code{\link[=plot.dbcsp]{plot}}, \code{\link[=boxplot.dbcsp]{boxplot}}
#' @name dbcsp-package
#' @rdname dbcsp-package
#' @aliases dbcsp-package
#' @docType package
#' @author Itsaso Rodriguez Moreno \email{itsaso.rodriguez@@ehu.eus}
#' @author Itziar Irigoien \email{itziar.irigoien@@ehu.eus}
#' @author Concepción Arenas \email{carenas@@ub.edu}
#' @examples
#' # There is an example dataset called AR.data shipped with the package.
#'
#' # It contains the skeleton data extracted from videos of people performing six different actions,
#' # recorded by a humanoid robot. So, it has 6 different classes.
#'
#' # As the whole process is performed pairwise, first two classes are selected (some instances are
#' # saved to use later as test)
#'
#' handshake <- AR.data$handshake[1:15]
#' ignore <- AR.data$ignore[1:15]
#'
#' # Now, the dbcsp object can be created, where q represents the number of vectors used in
#' # the projection, the obtained filter will have 2*q dimension.
#'
#' # By default, euclidean distance is used. To change it, just select another distance type.
#' # A mixture with euclidean distance and another one can be performed too, changing the mixture
#' # parameter value.
#'
#' # mydbcsp <- new('dbcsp', handshake, ignore, type="minkowski", p=0.2)
#'
#' # A user-defined custom distance can be used too.
#'
#' fn <- function(x, y) mean(1 - cos(x - y))
#' mydbcsp <- new("dbcsp", X1 = handshake, X2 = ignore, type="fn")
#'
#' # Regarding the classification, train, predict and selectQ functions can be used.
#' # An LDA model can be train with the features extracted after performing the CSP, both with the
#' # train function or using the training=TRUE parameter when creating the dbcsp object
#'
#' mydbcsp <- new('dbcsp', handshake, ignore, q=10, training=TRUE, fold = 1)
#'
#' # Once the object is created, print and summary functions can be used to see some details
#'
#' print(mydbcsp)
#' summary(mydbcsp)
#'
#' # The predict function returns the predicted labels for the selected test data.
#' # And if true_labels are indicated, the obtained accuracy is also printed
#'
#' handshake_test <- AR.data$handshake[41:45]
#' ignore_test <- AR.data$ignore[41:45]
#' test_data <- c(handshake_test, ignore_test)
#' true_labels <- c(rep('handshake',length(handshake_test)),rep('ignore',length(ignore_test)))
#' predictions <- predict(mydbcsp, test_data,true_labels)
#'
#' # To help us deciding which is the best dimension to use when performing the CSP algorithm, the
#' # selectQ function can be used. Instead of using train_size to validate with train/test split,
#' # cross validation can be performed too.
#'
#' bestQ <- selectQ(mydbcsp, Q=c(2,3,5), train_size=0.8)
#'
#' # A plot can also be obtained, which displays the signals before and after the CSP projection
#' # With the vectors parameter it can be decided which dimensions to show and if we just want to
#' # plot the transformed signals, the before parameter must be set to FALSE
#'
#' plot(mydbcsp,class=2,index=1,before=FALSE,vectors = 1:5, legend=TRUE)
#'
#' # A boxplot can also be obtained to show the features achieved after the CSP (the variances of
#' # the transformed signals) which are used to perform the classification
#' # In the CSP algorithm the vectors work in pairs (the first q vectors maximize the variance of
#' # one class and minimize the variance of the other, while the last q vectors do the opposite),
#' # by default the vectors are showed in pairs, but this can be changed.
#' # The logarithm of the variances are plotted, but the value of the variances are shown
#' # when changing show_log parameter to FALSE
#'
#' boxplot(mydbcsp, vectors=c(2,4,8))
#'
NULL
