test_that("wrappers work", {
	set.seed(123)
	n <- 50
	p <- 2
	X <- data.frame(matrix(rnorm(n*p), nrow = n, ncol = p))
	Y <- rbinom(n, 1, plogis(X[,1]))
	test <- train <- list(X = X, Y = Y)
	library(SuperLearner)

	# define a function that 
	check_wrapper <- function(wrapper, test, train){
		test <- do.call(wrapper, 
		                args = list(train = train, test = test))
		expect_true(all(c("test_pred","train_pred","model","train_y","test_y") %in% names(test)))
		expect_true(is.numeric(test$test_pred))
		expect_true(is.numeric(test$train_pred))
		expect_true(length(test$train_pred) == length(test$train_y))
		expect_true(length(test$test_pred) == length(test$test_y))
	}

	pkg_wrappers <- c(
		"randomforest", 
		"ranger",
		"glm",
		"stepglm",
		"xgboost",
		"superlearner",
		"glmnet"
	)
	for(wrap in pkg_wrappers){
		check_wrapper(paste0(wrap,"_wrapper"), test = test, train = train)
	}

})