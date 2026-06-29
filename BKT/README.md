# BKT: Bayesian Knowledge Tracing in R

We introduce an **R** implementation of the Bayesian Knowledge Tracing algorithm and its variants, which estimate student cognitive mastery from problem-solving sequences.

The R package `BKT` is publicly available on [GitHub](https://github.com/Feng-Ji-Lab/bkt). It can be installed and loaded using the following R commands:

```r
devtools::install_github("Feng-Ji-Lab/bkt")
library(BKT)
```

This package is based on the work of pyBKT in Python [pyBKT](https://github.com/CAHLR/pyBKT) developed by [CAHLR](https://github.com/CAHLR) lab at UC Berkeley.

## Preparing Data and Running the Model

The following is a mini-tutorial on how to get started with `BKT`.

### Input and Output Data

The accepted input formats are data files of type CSV (comma-separated) or TSV (tab-separated). `BKT` will automatically infer which delimiter to use when a data file is provided. Since the column names that map each field in the data (e.g., skill name, correct/incorrect) vary per data source, you may need to specify a mapping from your data file's column names to `BKT`'s expected column names. In many cases with Cognitive Tutor and **ASSISTments** datasets, `BKT` will be able to automatically infer column name mappings. If it is unable to do so, it will raise an error. Note that correctness is indicated by -1 (no response), 0 (incorrect), or 1 (correct).

### Creating and Training Models

The process of creating and training models in `BKT` resembles that of scikit-learn. `BKT` provides easy methods for fetching online datasets and fitting models on a combination of skills or all skills available in a particular dataset.

```r
library(BKT)

# Initialize the model with an optional seed
model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)

# Fetch example data
fetch_dataset(model, "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/as.csv", ".")
fetch_dataset(model, "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv", ".")

# Train a simple BKT model on all skills in the CT dataset
result_all <- fit(model, data_path = "ct.csv")

# Train a simple BKT model on one skill in the CT dataset
result_single <- fit(model, data_path = "ct.csv", skills = "Plot imperfect radical")

# Train a simple BKT model on multiple skills in the CT dataset
result_double <- fit(model, data_path = "ct.csv", skills = c("Plot imperfect radical", "Plot pi"))

# View the trained parameters
print(params(result_all))
```

The results will be shown as below: 

```
> print(params(result_all))
                                          skill   param   class    value
1        Plot non-terminating improper fraction  learns default 0.222808
2        Plot non-terminating improper fraction forgets default 0.000000
3        Plot non-terminating improper fraction guesses default 0.003382
4        Plot non-terminating improper fraction   slips default 0.322177
5        Plot non-terminating improper fraction   prior default 0.737443
6                        Plot imperfect radical  learns default 0.134145
7                        Plot imperfect radical forgets default 0.000000
8                        Plot imperfect radical guesses default 0.077672
9                        Plot imperfect radical   slips default 0.298567
10                       Plot imperfect radical   prior default 0.286065
11             Plot terminating proper fraction  learns default 0.039221
12             Plot terminating proper fraction forgets default 0.000000
13             Plot terminating proper fraction guesses default 0.310272
14             Plot terminating proper fraction   slips default 0.329641
15             Plot terminating proper fraction   prior default 0.572755
16                                      Plot pi  learns default 0.966132
17                                      Plot pi forgets default 0.000000
18                                      Plot pi guesses default 0.000000
19                                      Plot pi   slips default 0.391130
20                                      Plot pi   prior default 0.996233
21                            Plot whole number  learns default 0.332330
22                            Plot whole number forgets default 0.000000
23                            Plot whole number guesses default 0.790995
24                            Plot whole number   slips default 0.032996
25                            Plot whole number   prior default 0.574087
26                   Plot decimal - thousandths  learns default 0.959413
27                   Plot decimal - thousandths forgets default 0.000000
28                   Plot decimal - thousandths guesses default 0.000000
29                   Plot decimal - thousandths   slips default 0.617645
30                   Plot decimal - thousandths   prior default 0.708424
31                          Calculate unit rate  learns default 0.066416
32                          Calculate unit rate forgets default 0.000000
33                          Calculate unit rate guesses default 0.501578
34                          Calculate unit rate   slips default 0.113609
35                          Calculate unit rate   prior default 0.000048
36  Calculate part in proportion with fractions  learns default 0.136166
37  Calculate part in proportion with fractions forgets default 0.000000
38  Calculate part in proportion with fractions guesses default 0.394147
39  Calculate part in proportion with fractions   slips default 0.133619
40  Calculate part in proportion with fractions   prior default 0.557565
41 Calculate total in proportion with fractions  learns default 0.269612
42 Calculate total in proportion with fractions forgets default 0.000000
43 Calculate total in proportion with fractions guesses default 0.300150
44 Calculate total in proportion with fractions   slips default 0.125299
45 Calculate total in proportion with fractions   prior default 0.438129
46              Finding the intersection, Mixed  learns default 0.084853
47              Finding the intersection, Mixed forgets default 0.000000
48              Finding the intersection, Mixed guesses default 0.295654
49              Finding the intersection, Mixed   slips default 0.357887
50              Finding the intersection, Mixed   prior default 0.509688
51                Finding the intersection, GLF  learns default 0.060822
52                Finding the intersection, GLF forgets default 0.000000
53                Finding the intersection, GLF guesses default 0.424778
54                Finding the intersection, GLF   slips default 0.078708
55                Finding the intersection, GLF   prior default 0.225888
56                Finding the intersection, SIF  learns default 0.121315
57                Finding the intersection, SIF forgets default 0.000000
58                Finding the intersection, SIF guesses default 0.406944
59                Finding the intersection, SIF   slips default 0.009698
60                Finding the intersection, SIF   prior default 0.004261
```

In the result, the first column is the name of the skills, the second column indicates the type of parameters, including `learns`, `forgets`, `guesses`, `slips`, and `prior`. The laset column shows the estimated parameter value. 

Note that if you train on a dataset that has unfamiliar columns to `BKT`, you will be required to specify a mapping of column names in that dataset to the expected `BKT` columns. This is referred to as the model defaults (i.e., it specifies the default column names to look up in the dataset). An example usage is provided below for an unknown dataset that has column names "row", "skill_t", "answer", and "gs_classes".

```r
# Unfamiliar dataset file
file <- 'mystery.csv'

# For other non-ASSISTments/Cognitive Tutor style datasets, we will need to specify the
# columns corresponding to each required column (i.e., the user ID, correct/incorrect).
# For that, we use a defaults list.
# In this case, the order ID that BKT expects is specified by the column 'row' in the
# dataset, the 'skill_name' is specified by a column 'skill_t', and the correctness is specified
# by the 'answer' column in the dataset.
defaults <- list(order_id = 'row', skill_name = 'skill_t', correct = 'answer')

# Fit using the defaults (column mappings) specified in the list
fit(model, data_path = file, defaults = defaults)
```

### Model Prediction and Evaluation

Prediction and evaluation behave similarly to scikit-learn. `BKT` offers a variety of features for prediction and evaluation.

```r
# Load the BKT library (Bayesian Knowledge Tracing)
library(BKT)

# Initialize the BKT model with a specific seed for reproducibility and disable parallel processing
model <- bkt(seed = 42, parallel = FALSE)

# Fetch the dataset from the given URL and store it in the current directory
fetch_dataset(model, "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/as.csv", ".")

# Fit the BKT model using the provided dataset, disabling the 'forgets' option,
# and focusing on the skill labeled as "Box and Whisker"
result <- fit(model, data_path = "as.csv", forgets = TRUE, skills = "Box and Whisker")

print(params(result))

# Make predictions using the trained BKT model on the same dataset
preds_df <- predict_bkt(result, data_path = "as.csv")

# Filter predictions for the specific skill "Box and Whisker" and select relevant columns:
# 'user_id', 'correct' (actual performance), 'correct_predictions', and 'state_predictions' (predicted states)
box_and_whisker_preds <- subset(preds_df, skill_name == "Box and Whisker",
    select = c("user_id", "correct", "correct_predictions", "state_predictions")
)

# Print the filtered predictions
print(head(box_and_whisker_preds))
```

The sample result is shown below.
```
> print(box_and_whisker_preds)
  user_id correct correct_predictions state_predictions
1   64525       1           0.6923705         0.3048028
2   64525       1           0.8003175         0.1188919
3   70363       0           0.6923705         0.3048028
4   70363       1           0.5550472         0.5413068
5   70363       0           0.7347318         0.2318463
6   70363       1           0.5903234         0.4805526
...
```

The table box_and_whisker_preds contains the following columns:

`user_id`: This is the ID of each individual user. Each row represents an attempt by the user at a question.  
`correct`: This column represents whether the user's response was correct (1 for correct, 0 for incorrect) for that particular question attempt.  
`correct_predictions`: This column shows the model's predicted probbaility that the user's response would be correct. It is a value between 0 and 1, indicating the likelihood of the correct answer.  
`state_predictions`: This column reflects the predicted probability that the user is in a "learned" state or has mastered the skill. It's a measure of how likely it is that the user has learned the skill in question.

```R
library(BKT)

# Firstly, fit the model.
model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
fetch_dataset(model, "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv", ".")
result <- fit(model, data_path = "ct.csv", skills = "Plot non-terminating improper fraction")

# Evaluate the RMSE of the model on the training data.
# Note that the default evaluate metric is RMSE.
training_rmse <- evaluate(result, data_path = "ct.csv")
print(training_rmse)

# We can define a custom metric as well
mae <- function(true_vals, pred_vals) {
    return(mean(abs(true_vals - pred_vals)))
}
training_mae <- evaluate(result, data_path = "ct.csv", metric = mae)
print(training_mae)

```

We can see the following results:
```
> training_rmse <- evaluate(result, data_path = "ct.csv")
> print(training_rmse)
[1] 0.480762
```

The RMSE (Root Mean Squared Error) result of 0.480762 indicates that, on average, the difference between the predicted values and the actual values for the skill "Plot non-terminating improper fraction" in the dataset is approximately 0.48. This provides an overall measure of how well the model predictions fit the data, with lower values indicating better performance.

```
> training_mae <- evaluate(result, data_path = "ct.csv", metric = mae)
> print(training_mae)
[1] 0.4632682
```

The MAE (Mean Absolute Error) result of 0.4632682 shows that, on average, the absolute difference between the predicted and actual values is around 0.46. This metric focuses on the average magnitude of errors, without considering their direction, and it also suggests a reasonably good fit between the model's predictions and the actual data.

### Cross-Validation

Cross-validation is offered as a black-box function similar to a combination of `fit` and `evaluate` that accepts a particular number of folds, a seed, and a metric (either one of the provided 'rmse' or a custom R function taking two arguments). Similar arguments for the model types, data path/data, and skill names are accepted as with the `fit` function.

```r
library(BKT)

# Initialize the model with an optional seed
model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)

# Cross-validate with 5 folds on all skills in the CT dataset
crossvalidated_errors <- crossvalidate(model, data_path = 'ct.csv', skills = "Plot non-terminating improper fraction", folds = 5)
print(crossvalidated_errors)

# Cross-validate on a particular set of skills with a given
# seed, folds, and metric
mae <- function(true_vals, pred_vals) {
  # Calculates the mean absolute error
  mean(abs(true_vals - pred_vals))
}

# Note that the 'skills' argument accepts a REGEX pattern. In this case, this matches and
# cross-validates on all skills containing the word 'fraction'.
crossvalidated_mae_errs <- crossvalidate(model, data_path = 'ct.csv', skills = ".*fraction.*",
                                         folds = 5, metric = mae)
print(crossvalidated_mae_errs)
```

```
> print(crossvalidated_errors)
                                   skill        dummy
1 Plot non-terminating improper fraction 0.483330....
```

 The result for Plot non-terminating improper fraction with an RMSE of approximately 0.483 indicates that the model's predicted values differ from the actual values by an average of about 0.483 across the 5 cross-validation folds.

```
> print(crossvalidated_mae_errs)
                                         skill        dummy
1       Plot non-terminating improper fraction 0.465849....
2             Plot terminating proper fraction 0.490182....
3  Calculate part in proportion with fractions 0.366942....
4 Calculate total in proportion with fractions 0.361044....
```

The result for multiple skills (such as Plot non-terminating improper fraction with an MAE of 0.466, Plot terminating proper fraction with an MAE of 0.490, etc.) represents the average absolute difference between the predicted and actual values for each skill across the 5 cross-validation folds.

### Parameter Fixing

Another advanced feature supported by `BKT` is parameter fixing, where we can fix one or more parameters and train the model conditioned on those fixed parameters. This can be useful if you already know the ground truth value of some parameters beforehand or to avoid degenerate model creation by fixing parameters at reasonable values. To specify which parameters and values we want fixed for any skill, we can pass in a list to `set_coef`, and then specify `fixed = TRUE` in the `fit` call:

```r
library(BKT)

model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
fetch_dataset(model, "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv", ".")

# Fixes the prior rate to 0.5 for the 'Plot non-terminating improper fraction' skill, and trains the model given those fixed parameters
model <- set_coef(model, list("Plot non-terminating improper fraction" = list("prior" = 0.5)))
result <- fit(model,
    forgets = TRUE,
    data_path = "ct.csv", skills = "Plot non-terminating improper fraction",
    fixed = list("Plot non-terminating improper fraction" = list("prior" = TRUE))
)
print(params(result))
```

The results are shown below:

```
> print(params(result))
                                   skill   param   class    value
1 Plot non-terminating improper fraction  learns default 0.236883
2 Plot non-terminating improper fraction forgets default 0.001726
3 Plot non-terminating improper fraction guesses default 0.135079
4 Plot non-terminating improper fraction   slips default 0.227050
5 Plot non-terminating improper fraction   prior default 0.500000
```

In this example, the model was trained with the prior probability fixed at 0.5 for the "Plot non-terminating improper fraction" skill, and it estimated the other parameters (learns, forgets, guesses, slips) based on the data. The model predicts a moderate probability of learning (23.7%), a very low chance of forgetting (0.17%), a moderate guess rate (13.5%), and a significant slip rate (22.7%). The fixed prior indicates that the model started with the assumption that the student had a 50% chance of already knowing the skill before any questions were answered.

Within the `set_coef` list, the 'prior' parameter takes a scalar, while 'learns', 'forgets', 'guesses', and 'slips' take a vector, in order to provide support for parameter fixing in model extensions with multiple learn or guess classes. An example of such is shown below:

```r
library(BKT)

model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
fetch_dataset(model, "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv", ".")

model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
model <- set_coef(model, list("Plot non-terminating improper fraction" = list("learns" = c(0.25), "forgets" = c(0.25))))
print(params(model))
result <- fit(model,
    data_path = "ct.csv", forgets = TRUE, skills = "Plot non-terminating improper fraction",
    fixed = list("Plot non-terminating improper fraction" = list("learns" = TRUE, "forgets" = TRUE))
)
print(params(result))

```

The result are shown below:
```
> print(params(result))
                                   skill   param   class    value
1 Plot non-terminating improper fraction  learns default 0.250000
2 Plot non-terminating improper fraction forgets default 0.250000
3 Plot non-terminating improper fraction guesses default 0.182944
4 Plot non-terminating improper fraction   slips default 0.112771
5 Plot non-terminating improper fraction   prior default 0.483079
```

In this example, the learns and forgets parameters were manually fixed at 0.25 using the `set_coef` function, meaning the model was not allowed to adjust these values during training. The model then estimated the other parameters (guesses, slips, and prior) based on the data. The fixed learns and forgets parameters indicate a constant 25% chance of learning or forgetting the skill, while the model predicted an 18.3% chance of guessing correctly and an 11.3% chance of slipping. The prior probability indicates that, initially, there was a 48.3% chance the student already knew the skill.

### BKT Variants


#### Forget

Train a forget BKT model in the CT dataset. Switch parameter `forgets = TRUE` in the fit function.

Forgets refers to learning and forgetting behavior (LFB) Model.

```r

library(BKT)

model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
fetch_dataset(model, "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv", ".")

model = Model(seed = 42, num_fits = 1, parallel = False)
model.fit(data_path = 'ct.csv', forgets = True, skills = 'Plot terminating proper fraction')
print(model.params())

```
The result are shown below:
```
                             skill   param   class    value
1 Plot terminating proper fraction  learns default 0.087497
2 Plot terminating proper fraction forgets default 0.054892
3 Plot terminating proper fraction guesses default 0.285785
4 Plot terminating proper fraction   slips default 0.329848
5 Plot terminating proper fraction   prior default 0.622707
```

#### Multiple Priors


Train a multiple prior BKT model on multiple students in the CT dataset. Switch parameter `multiprior = TRUE` in fit function.

Multiple Prior refers to the Prior Per Student (PPS) Model.

```r
library(BKT)
model <- bkt(seed = 42, num_fits = 10, parallel = FALSE)

fetch_dataset(model, "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv", ".")
result <- fit(model, data_path = "ct.csv", skills = c("Finding the intersection, Mixed"), multiprior = TRUE)

print(params(result))
```

The result are shown below:

```
                             skill   param        class    value
1  Finding the intersection, Mixed  learns      default 0.016332
2  Finding the intersection, Mixed guesses      default 0.292058
3  Finding the intersection, Mixed   slips      default 0.240662
4  Finding the intersection, Mixed   prior     171eKI1Z 1.000000
5  Finding the intersection, Mixed   prior     171nKnHW 1.000000
6  Finding the intersection, Mixed   prior     171RUh1Q 1.000000
7  Finding the intersection, Mixed   prior      1T4w47X 0.000000
8  Finding the intersection, Mixed   prior 2302sApyRu_a 1.000000
9  Finding the intersection, Mixed   prior 2304U8pLyy_a 0.000000
10 Finding the intersection, Mixed   prior 2304YbHCok_a 1.000000
11 Finding the intersection, Mixed   prior 2304YbHCok_b 0.000000
12 Finding the intersection, Mixed   prior 2305Ra0wn7_a 0.000000
13 Finding the intersection, Mixed   prior 2309tLkCW6_a 0.000000
14 Finding the intersection, Mixed   prior 230amNTt8e_a 0.001792
15 Finding the intersection, Mixed   prior 230amNTt8e_b 0.000000
16 Finding the intersection, Mixed   prior 230cbpmx2H_a 0.000000
17 Finding the intersection, Mixed   prior 230cQtHpL1_a 0.000000
18 Finding the intersection, Mixed   prior 230duwYN4p_a 0.000000
19 Finding the intersection, Mixed   prior 230EGiaN2S_a 0.000099
20 Finding the intersection, Mixed   prior 230HLorplQ_a 0.000000
21 Finding the intersection, Mixed   prior 230hzMTB8t_a 0.000000
22 Finding the intersection, Mixed   prior 230hzMTB8t_c 1.000000
23 Finding the intersection, Mixed   prior 230J1b5ZjX_a 0.000000
24 Finding the intersection, Mixed   prior 230jc0wqMP_a 0.000000
25 Finding the intersection, Mixed   prior 230JX4ja8w_a 1.000000
26 Finding the intersection, Mixed   prior 230JX4ja8w_b 0.000000
27 Finding the intersection, Mixed   prior 230kbSDgQ3_a 0.000000
28 Finding the intersection, Mixed   prior 230KZfQk0y_a 0.000000
29 Finding the intersection, Mixed   prior 230NIBA0Dq_a 0.000000
30 Finding the intersection, Mixed   prior 230NIBA0Dq_b 1.000000
31 Finding the intersection, Mixed   prior 230P2rW3yn_a 0.000000
32 Finding the intersection, Mixed   prior 230rlAi5Zw_a 0.000000
33 Finding the intersection, Mixed   prior 230tBgtvsW_a 0.000000
34 Finding the intersection, Mixed   prior 230tBgtvsW_b 0.000000
35 Finding the intersection, Mixed   prior 230XGzqSto_a 0.001056
36 Finding the intersection, Mixed   prior 230XGzqSto_b 1.000000
37 Finding the intersection, Mixed   prior 230Y6CWb3d_a 0.000000
38 Finding the intersection, Mixed   prior    236wm0wnm 1.000000
39 Finding the intersection, Mixed   prior    236z60ytg 1.000000
40 Finding the intersection, Mixed   prior    2480hxxcx 1.000000
41 Finding the intersection, Mixed   prior    2480wql4f 1.000000
42 Finding the intersection, Mixed   prior    2483hkq13 0.018457
43 Finding the intersection, Mixed   prior    2483mf0as 1.000000
44 Finding the intersection, Mixed   prior    2484vyh4g 1.000000
45 Finding the intersection, Mixed   prior    24864dslr 0.000000
46 Finding the intersection, Mixed   prior    2487lpei2 1.000000
47 Finding the intersection, Mixed   prior    2487o1gku 0.000016
48 Finding the intersection, Mixed   prior    2487p61kc 0.000000
49 Finding the intersection, Mixed   prior    2488721d5 1.000000
50 Finding the intersection, Mixed   prior    2488t72oe 1.000000
51 Finding the intersection, Mixed   prior    24899b8f6 1.000000
52 Finding the intersection, Mixed   prior    2489c9lbg 1.000000
53 Finding the intersection, Mixed   prior    248abpghv 1.000000
54 Finding the intersection, Mixed   prior    248agzhky 1.000000
55 Finding the intersection, Mixed   prior    248clu9mj 1.000000
56 Finding the intersection, Mixed   prior    248g1paph 0.000000
57 Finding the intersection, Mixed   prior    248g1wh6o 0.000000
58 Finding the intersection, Mixed   prior    248hh7azg 1.000000
59 Finding the intersection, Mixed   prior    248iwnoa3 1.000000
60 Finding the intersection, Mixed   prior    248jqsua5 0.000000
61 Finding the intersection, Mixed   prior    248k03ldd 1.000000
62 Finding the intersection, Mixed   prior    248llcp7q 0.000000
63 Finding the intersection, Mixed   prior    248m27z11 1.000000
64 Finding the intersection, Mixed   prior    248mbp1cf 1.000000
65 Finding the intersection, Mixed   prior    248oylglm 0.000000
66 Finding the intersection, Mixed   prior    248pthl4g 0.000000
67 Finding the intersection, Mixed   prior    248qhy2yo 0.031083
68 Finding the intersection, Mixed   prior    248s9v653 1.000000
69 Finding the intersection, Mixed   prior    248t7n0my 1.000000
70 Finding the intersection, Mixed   prior    248tviw5h 0.000000
71 Finding the intersection, Mixed   prior    248vthf5i 0.000000
72 Finding the intersection, Mixed   prior    248zczb3i 0.000000
73 Finding the intersection, Mixed   prior 271g7beuc4s1 0.000000
74 Finding the intersection, Mixed   prior 271k966j74lv 0.000000
75 Finding the intersection, Mixed   prior 271np4zc8vd1 1.000000
76 Finding the intersection, Mixed   prior      3cjD21W 1.000000
77 Finding the intersection, Mixed   prior      Vd9xmeb 1.000000
```
Note column `class` refers to different students.

#### Multiple Guess

Train a multiple guess and slip BKT model on multiple skills in the CT dataset. Switch parameter `multigs = TRUE` in fit function.

Multiple Guess refers to Item Difficulty Effect (IDE) Model.

```r
library(BKT)

model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
fetch_dataset(model, "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv", ".")

result <- fit(model, data_path = "ct.csv", skills = c("Plot pi"), multigs = TRUE)
print(params(result))
```

The result are shown below:
```
     skill   param         class    value
1  Plot pi  learns       default 0.986592
2  Plot pi guesses RATIONAL1-045 0.000000
3  Plot pi guesses RATIONAL1-063 0.000000
4  Plot pi guesses RATIONAL1-122 0.000000
5  Plot pi guesses RATIONAL1-156 0.000000
6  Plot pi guesses RATIONAL1-190 0.000000
7  Plot pi guesses RATIONAL1-207 0.000000
8  Plot pi guesses RATIONAL1-237 0.000000
9  Plot pi guesses RATIONAL1-242 0.000000
10 Plot pi guesses RATIONAL1-254 0.000000
11 Plot pi guesses RATIONAL1-284 0.000000
12 Plot pi   slips RATIONAL1-045 0.575424
13 Plot pi   slips RATIONAL1-063 0.218712
14 Plot pi   slips RATIONAL1-122 0.243997
15 Plot pi   slips RATIONAL1-156 0.425278
16 Plot pi   slips RATIONAL1-190 0.235049
17 Plot pi   slips RATIONAL1-207 0.559538
18 Plot pi   slips RATIONAL1-237 0.378160
19 Plot pi   slips RATIONAL1-242 0.474867
20 Plot pi   slips RATIONAL1-254 0.231365
21 Plot pi   slips RATIONAL1-284 0.469949
22 Plot pi   prior       default 0.987410
```
Note column `class` refers to different items.

#### Multiple Learn

Train a multiple learn BKT model on multiple skills in the CT dataset. Switch parameter `multilearn = TRUE` in fit function.

multilearn refers to Item Learning Effect (ILE) Model.

```r
library(BKT)

model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
fetch_dataset(model, "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv", ".")

result <- fit(model, data_path = "ct.csv", skills = c("Plot pi"), multilearn = TRUE)
print(params(result))
```

The result are shown below:

```
     skill   param         class    value
1  Plot pi  learns RATIONAL1-045 0.968846
2  Plot pi  learns RATIONAL1-063 1.000000
3  Plot pi  learns RATIONAL1-122 1.000000
4  Plot pi  learns RATIONAL1-156 1.000000
5  Plot pi  learns RATIONAL1-190 1.000000
6  Plot pi  learns RATIONAL1-207 0.334115
7  Plot pi  learns RATIONAL1-237 0.775121
8  Plot pi  learns RATIONAL1-242 0.742444
9  Plot pi  learns RATIONAL1-254 0.000000
10 Plot pi  learns RATIONAL1-284 0.000372
11 Plot pi guesses       default 0.000000
12 Plot pi   slips       default 0.352701
13 Plot pi   prior       default 0.936924
```

Note column `class` refers to different items.


#### Multiple Pair

Train a multiple pair BKT model on multiple skills in the CT dataset. Switch parameter `multipair = TRUE` in fit function.

Multiple Pair refers to Item Order Effect (IOE) Model.

```r
library(BKT)

model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
fetch_dataset(model, "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv", ".")

result <- fit(model, data_path = "ct.csv", skills = c("Plot pi"), multipair = TRUE)
print(params(result))
```

The result are shown below:

```
     skill   param                       class    value
1  Plot pi  learns                     Default 0.363054
2  Plot pi  learns RATIONAL1-063 RATIONAL1-237 1.000000
3  Plot pi  learns RATIONAL1-254 RATIONAL1-063 1.000000
4  Plot pi  learns RATIONAL1-284 RATIONAL1-254 1.000000
5  Plot pi  learns RATIONAL1-063 RATIONAL1-122 1.000000
6  Plot pi  learns RATIONAL1-122 RATIONAL1-284 1.000000
7  Plot pi  learns RATIONAL1-237 RATIONAL1-254 1.000000
8  Plot pi  learns RATIONAL1-284 RATIONAL1-045 1.000000
9  Plot pi  learns RATIONAL1-284 RATIONAL1-190 1.000000
10 Plot pi  learns RATIONAL1-122 RATIONAL1-045 1.000000
11 Plot pi  learns RATIONAL1-045 RATIONAL1-254 0.000000
12 Plot pi  learns RATIONAL1-242 RATIONAL1-045 1.000000
13 Plot pi  learns RATIONAL1-122 RATIONAL1-237 1.000000
14 Plot pi  learns RATIONAL1-237 RATIONAL1-122 1.000000
15 Plot pi  learns RATIONAL1-254 RATIONAL1-190 1.000000
16 Plot pi  learns RATIONAL1-254 RATIONAL1-284 1.000000
17 Plot pi  learns RATIONAL1-242 RATIONAL1-156 1.000000
18 Plot pi  learns RATIONAL1-156 RATIONAL1-242 1.000000
19 Plot pi  learns RATIONAL1-122 RATIONAL1-242 1.000000
20 Plot pi  learns RATIONAL1-207 RATIONAL1-190 1.000000
21 Plot pi  learns RATIONAL1-190 RATIONAL1-045 1.000000
22 Plot pi  learns RATIONAL1-045 RATIONAL1-063 1.000000
23 Plot pi  learns RATIONAL1-254 RATIONAL1-207 1.000000
24 Plot pi  learns RATIONAL1-063 RATIONAL1-156 1.000000
25 Plot pi  learns RATIONAL1-045 RATIONAL1-284 0.000000
26 Plot pi  learns RATIONAL1-156 RATIONAL1-045 1.000000
27 Plot pi  learns RATIONAL1-063 RATIONAL1-284 1.000000
28 Plot pi  learns RATIONAL1-122 RATIONAL1-156 1.000000
29 Plot pi  learns RATIONAL1-207 RATIONAL1-242 1.000000
30 Plot pi  learns RATIONAL1-237 RATIONAL1-242 1.000000
31 Plot pi  learns RATIONAL1-207 RATIONAL1-254 0.000000
32 Plot pi  learns RATIONAL1-242 RATIONAL1-207 1.000000
33 Plot pi  learns RATIONAL1-063 RATIONAL1-242 1.000000
34 Plot pi  learns RATIONAL1-156 RATIONAL1-284 1.000000
35 Plot pi  learns RATIONAL1-045 RATIONAL1-156 1.000000
36 Plot pi  learns RATIONAL1-190 RATIONAL1-242 1.000000
37 Plot pi  learns RATIONAL1-207 RATIONAL1-156 1.000000
38 Plot pi  learns RATIONAL1-242 RATIONAL1-237 1.000000
39 Plot pi  learns RATIONAL1-063 RATIONAL1-190 1.000000
40 Plot pi  learns RATIONAL1-190 RATIONAL1-237 1.000000
41 Plot pi  learns RATIONAL1-242 RATIONAL1-190 1.000000
42 Plot pi  learns RATIONAL1-237 RATIONAL1-063 1.000000
43 Plot pi  learns RATIONAL1-207 RATIONAL1-045 0.000053
44 Plot pi  learns RATIONAL1-284 RATIONAL1-207 1.000000
45 Plot pi  learns RATIONAL1-190 RATIONAL1-156 1.000000
46 Plot pi  learns RATIONAL1-190 RATIONAL1-207 1.000000
47 Plot pi  learns RATIONAL1-237 RATIONAL1-190 1.000000
48 Plot pi  learns RATIONAL1-156 RATIONAL1-122 1.000000
49 Plot pi  learns RATIONAL1-237 RATIONAL1-284 1.000000
50 Plot pi  learns RATIONAL1-190 RATIONAL1-122 1.000000
51 Plot pi  learns RATIONAL1-063 RATIONAL1-045 1.000000
52 Plot pi  learns RATIONAL1-237 RATIONAL1-207 1.000000
53 Plot pi  learns RATIONAL1-207 RATIONAL1-063 1.000000
54 Plot pi  learns RATIONAL1-063 RATIONAL1-207 1.000000
55 Plot pi  learns RATIONAL1-122 RATIONAL1-190 1.000000
56 Plot pi  learns RATIONAL1-284 RATIONAL1-122 1.000000
57 Plot pi  learns RATIONAL1-242 RATIONAL1-284 1.000000
58 Plot pi  learns RATIONAL1-045 RATIONAL1-190 1.000000
59 Plot pi  learns RATIONAL1-190 RATIONAL1-284 1.000000
60 Plot pi  learns RATIONAL1-063 RATIONAL1-254 1.000000
61 Plot pi  learns RATIONAL1-207 RATIONAL1-237 1.000000
62 Plot pi  learns RATIONAL1-237 RATIONAL1-045 1.000000
63 Plot pi  learns RATIONAL1-122 RATIONAL1-207 1.000000
64 Plot pi  learns RATIONAL1-207 RATIONAL1-284 1.000000
65 Plot pi  learns RATIONAL1-284 RATIONAL1-156 1.000000
66 Plot pi  learns RATIONAL1-254 RATIONAL1-156 1.000000
67 Plot pi  learns RATIONAL1-122 RATIONAL1-063 1.000000
68 Plot pi  learns RATIONAL1-254 RATIONAL1-237 1.000000
69 Plot pi  learns RATIONAL1-242 RATIONAL1-254 1.000000
70 Plot pi  learns RATIONAL1-254 RATIONAL1-242 1.000000
71 Plot pi  learns RATIONAL1-207 RATIONAL1-122 1.000000
72 Plot pi  learns RATIONAL1-242 RATIONAL1-122 1.000000
73 Plot pi guesses                     default 0.000000
74 Plot pi   slips                     default 0.378540
75 Plot pi   prior                     default 0.980586
```

Note column `class` refers to different items orders.

### Simulation

In the `simulation` directory, there are several files used for data simulation.

Specifically:
- The function `simulate_bkt_data` in `generator.R` is used to generate data for the basic BKT model.
- `run.R` is used to perform a simple data simulation and testing.

## Internal Data Format

`BKT` models student mastery of a skill as they progress through a series of learning resources and checks for understanding. Mastery is modeled as a latent variable with two states: "knowing" and "not knowing." At each checkpoint, students may be given a learning resource (e.g., watch a video) and/or question(s) to check for understanding. The model finds the probability of learning, forgetting, slipping, and guessing that maximizes the likelihood of observed student responses to questions.

To run the `BKT` model, define the following variables:

- `num_subparts`: The number of unique questions used to check understanding. Each subpart has a unique set of emission probabilities.
- `num_resources`: The number of unique learning resources available to students.
- `num_fit_initialization`: The number of iterations in the EM step.

Next, create an input object `Data`, containing the following attributes:

- `data`: a matrix containing sequential checkpoints for all students, with their responses. Each row represents a different subpart, and each column represents a checkpoint for a student. There are three potential values: 0 (no response or no question asked), 1 (wrong response), or 2 (correct response). If at a checkpoint a resource was given but no question was asked, the associated column would have `0` values in all rows. For example, to set up data containing 5 subparts given to two students over 2-3 checkpoints, the matrix would look as follows:

    ```
    | 0  0  0  0  2 |
    | 0  1  0  0  0 |
    | 0  0  0  0  0 |
    | 0  0  0  0  0 |
    | 0  0  2  0  0 |
    ```

  In the above example, the first student starts out with just a learning resource and no checks for understanding. In subsequent checkpoints, this student responds to subparts 2 and 5, getting the first wrong and the second correct.

- `starts`: defines each student's starting column in the `data` matrix. For the above matrix, `starts` would be defined as:

    ```
    | 1  4 |
    ```

- `lengths`: defines the number of checkpoints for each student. For the above matrix, `lengths` would be defined as:

    ```
    | 3  2 |
    ```

- `resources`: defines the sequential ID of the resources at each checkpoint. Each position in the vector corresponds to the column in the `data` matrix. For the above matrix, the learning `resources` at each checkpoint would be structured as:

    ```
    | 1  2  1  1  3 |
    ```

- `stateseqs`: this attribute represents the true knowledge state for the above data and should be left undefined before running the `BKT` model.

The output of the model will be stored in a `fitmodel` object, containing the following probabilities as attributes:

- `As`: the transition probabilities between the "knowing" and "not knowing" states. This includes both the `learns` and `forgets` probabilities and their inverses. `As` creates a separate transition probability for each resource.
- `learns`: the probability of transitioning to the "knowing" state given "not known."
- `forgets`: the probability of transitioning to the "not knowing" state given "known."
- `prior`: the prior probability of "knowing."

The `fitmodel` also includes the following emission probabilities:

- `guesses`: the probability of guessing correctly, given the "not knowing" state.
- `slips`: the probability of making an incorrect response, given the "knowing" state.
