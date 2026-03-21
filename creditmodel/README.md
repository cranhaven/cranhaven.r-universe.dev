# creditmodel

`creditmodel` is a free and open source automated modeling R package designed to help model developers improve model development efficiency and enable many people with no background in data science to complete the modeling work in a short time.Let them focus more on the problem itself and allocate more time to decision-making.

`creditmodel` covers various tools such as data preprocessing, variable processing/derivation, variable screening/dimensionality reduction, modeling, data analysis, data visualization, model evaluation, strategy analysis, etc. It is a set of customized "core" tool kit for model developers.

`creditmodel` is suitable for machine learning automated modeling of classification targets, and is more suitable for the risk and marketing data of financial credit, e-commerce, and insurance with relatively high noise and low information content.

# Installation
```
# install.packages("creditmodel")
```
# Example
```
 # Automated Model Development Process


 if (!dir.exists("c:/test_model")) dir.create("c:/test_model")
 setwd("c:/test_model")
 library(creditmodel)
 sub = cv_split(UCICreditCard, k = 3)[[1]]
 dat = UCICreditCard[sub,]
 dat = re_name(dat, "default.payment.next.month", "target")
 dat = data_cleansing(dat, target = "target", obs_id = "ID", occur_time = "apply_date", miss_values = list("", -1, -2))
 train_test =train_test_split(dat, split_type = "OOT", prop = 0.7, occur_time = "apply_date")
 dat_train = train_test$train
 dat_test = train_test$test
 
 B_model = training_model(dat = dat_train,
                         model_name = "UCICreditCard", target = "target", x_list = NULL,
                         occur_time = "apply_date", obs_id = "ID", dat_test = dat_test,
                         preproc = FALSE,
                         feature_filter = NULL,
                         algorithm = list("RF","LR","XGB","GBM"),
                         LR.params = lr_params(lasso = TRUE,
                                               step_wise = FALSE, vars_plot = FALSE),
                         XGB.params = xgb_params(),
                         breaks_list = NULL,
                         parallel = FALSE, cores_num = NULL,
                         save_pmml = FALSE, plot_show = FALSE,
                         model_path = getwd(),
                         seed = 46)
```
