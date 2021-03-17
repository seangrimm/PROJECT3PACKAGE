#' k-nearest neighbors with cross validation
#'
#' Builds a classification model for data \code{train} using the k-nearest
#' neighbors algorithm with k = \code{k_nn} and applies k-fold
#' cross-validation with k = \code{k_cv}.
#'
#' @param train Data frame used for training the model.
#' @param cl Vector of true classifications for entries in \code{train}.
#' @param k_nn Integer indicating how many neighbors should be considered in the
#'   k-nearest neighbors algorithm.
#' @param k_cv Integer indicating how many folds \code{train} will be divided
#'   into for k-fold cross-validation. Must have \code{k_cv} >= 2.
#'
#' @keywords prediction
#'
#' @return A list containing a vector of classifications predicted by the model
#'   using \code{train} as both the training data and the testing data and a
#'   numeric representing the cross-validation error from the k-fold
#'   cross-validation.
#'
#' @examples
#' train <- na.omit(my_penguins)[,3:6]
#' cl <- na.omit(my_penguins)[1]
#' my_knn_cv(train, cl, 1, 5)
#' my_knn_cv(train, cl, 5, 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # Randomly split data into k_cv groups
  fold <- sample(rep(1:k_cv, length = nrow(cl)))

  split_data <- train
  split_data$fold <- fold

  split_class <- cl
  split_class$fold <- fold

  cv_errs <- rep(0, k_cv)

  for (i in 1:k_cv) {
    # separate into train and test
    data_train <- split_data[which(split_data$fold != i), ]
    data_train <- subset.data.frame(data_train, select = -ncol(data_train))

    data_test <- split_data[which(split_data$fold == i), ]
    data_test <-subset.data.frame(data_test, select = -ncol(data_test))

    class_train <- split_class[which(split_class$fold != i), ]
    class_train <- subset.data.frame(class_train, select = -ncol(class_train))

    class_test <- split_class[which(split_class$fold == i), ]
    class_test <- subset.data.frame(class_test, select = -ncol(class_test))

    # determine model results and error
    result <- class::knn(data_train, data_test, unlist(class_train), k_nn)
    cv_errs[i] <- mean(result != unlist(class_test))
  }

  class <- class::knn(train, train, cl[[1]], k_nn)
  cv_err <- mean(cv_errs)
  return(list(class, cv_err))
}
