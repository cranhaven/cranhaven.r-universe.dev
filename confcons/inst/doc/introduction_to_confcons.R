## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
#  install.packages("confcons", dependencies = TRUE)

## -----------------------------------------------------------------------------
suppressWarnings(library(terra))
suppressWarnings(library(sf))
suppressWarnings(library(blockCV))
suppressWarnings(library(ranger))
suppressWarnings(library(ROCR))
suppressWarnings(library(confcons))

## -----------------------------------------------------------------------------
environment <- terra::rast(list.files(system.file("extdata/au/", package = "blockCV"), full.names = TRUE))
terra::nlyr(environment)
(predictors <- names(environment))
terra::crs(x = environment, describe = TRUE)$name
terra::res(environment)

## -----------------------------------------------------------------------------
occurrences <- read.csv(system.file("extdata/", "species.csv", package = "blockCV"))
occurrences <- sf::st_as_sf(x = occurrences,
                            coords = c("x", "y"),
                            crs = terra::crs(environment))

## ----eval = FALSE-------------------------------------------------------------
#  vignette("tutorial_1")

## -----------------------------------------------------------------------------
blocks <- blockCV::cv_spatial(x = occurrences,
                              column = "occ",
                              r = environment,
                              size = 350000,
                              k = 2,
                              selection = "random",
                              iteration = 50,
                              seed = 12345,
                              progress = FALSE,
                              report = FALSE,
                              plot = TRUE)
blocks_sf <- sf::st_as_sf(x = blocks$blocks)

## ----fig.height=10, fig.width=10----------------------------------------------
plot(x = environment[["bio_5"]], axes = FALSE, col = colorRampPalette(c("lightskyblue2", "lightyellow1", "rosybrown2"))(255), colNA = "gray95")
plot(x = occurrences[occurrences$occ == 1, ], pch = "+", col = "darkgreen", add = TRUE)
plot(x = occurrences[occurrences$occ == 0, ], pch = "+", col = "orange", add = TRUE)
plot(x = sf::st_geometry(blocks_sf[blocks_sf$folds == 1, ]), col = "transparent", border = "royalblue1", lwd = 2, add = TRUE)
plot(x = sf::st_geometry(blocks_sf[blocks_sf$folds == 2, ]), col = "transparent", border = "palevioletred1", lwd = 2, add = TRUE)
legend(x = -2100000,
       y = -1300000,
       legend = c("presence", "absence", "training", "evaluation"),
       col = c("darkgreen", "orange", NA, NA),
       pch = c("+", "+", NA, NA),
       border = c(NA, NA, "royalblue1", "palevioletred1"),
       fill = c(NA, NA, "transparent", "transparent"))

## -----------------------------------------------------------------------------
dataset <- as.data.frame(terra::extract(x = environment,
                                        y = occurrences,
                                        ID = FALSE))
dataset$occurrences <- occurrences$occ
dataset$training_mask <- (1:nrow(occurrences)) %in% blocks$folds_list[[1]][[1]]
str(dataset)

## -----------------------------------------------------------------------------
linear_formula <- as.formula(paste0("occurrences ~ ", paste(predictors, collapse = " + ")))
model_glm <- step(trace = 0,
                  object = glm(formula = linear_formula,
                               family = binomial(link = "logit"),
                               data = dataset[dataset$training_mask, ]))
dataset$predictions_glm <- predict(object = model_glm,
                                   newdata = dataset,
                                   type = "response")

## -----------------------------------------------------------------------------
model_rf <- ranger::ranger(formula = linear_formula,
                           data = dataset[dataset$training_mask, ],
                           num.trees = 10000,
                           min.node.size = 10,
                           max.depth = 8,
                           seed = 12345,
                           verbose = FALSE,
                           classification = FALSE)
dataset$predictions_rf <- predict(object = model_rf,
                                  data = dataset,
                                  type = "response",
                                  verbose = FALSE)$predictions
str(dataset[, c("occurrences", "training_mask", "predictions_glm", "predictions_rf")])

## -----------------------------------------------------------------------------
(thresholds_glm <- thresholds(observations = dataset$occurrences,
                              predictions = dataset$predictions_glm))
(thresholds_rf <- thresholds(observations = dataset$occurrences,
                             predictions = dataset$predictions_rf))

## -----------------------------------------------------------------------------
conf_P_eval <- confidence(observations = dataset$occurrences[!dataset$training_mask],
                          predictions = dataset$predictions_glm[!dataset$training_mask],
                          thresholds = thresholds_glm,
                          type = "positive")
conf_P_eval
conf_N_eval <- confidence(observations = dataset$occurrences[!dataset$training_mask],
                          predictions = dataset$predictions_glm[!dataset$training_mask],
                          thresholds = thresholds_glm,
                          type = "neutral")
conf_N_eval

## -----------------------------------------------------------------------------
conf_P_train <- confidence(observations = dataset$occurrences[dataset$training_mask],
                           predictions = dataset$predictions_glm[dataset$training_mask],
                           thresholds = thresholds_glm,
                           type = "positive")
conf_P_train
conf_P_eval < conf_P_train

## -----------------------------------------------------------------------------
consistency(conf_train = conf_P_train, conf_eval = conf_P_eval)

## -----------------------------------------------------------------------------
measures(observations = dataset$occurrences,
         predictions = dataset$predictions_glm,
         evaluation_mask = !dataset$training_mask)
measures(observations = dataset$occurrences,
         predictions = dataset$predictions_rf,
         evaluation_mask = !dataset$training_mask)

## -----------------------------------------------------------------------------
measures(observations = dataset$occurrences,
         predictions = dataset$predictions_glm,
         evaluation_mask = !dataset$training_mask,
         goodness = TRUE)

## -----------------------------------------------------------------------------
measures(observations = dataset$occurrences,
         predictions = dataset$predictions_rf,
         evaluation_mask = !dataset$training_mask,
         goodness = TRUE,
         df = TRUE)


## -----------------------------------------------------------------------------
model_IDs <- c("glm", "rf")
for (model_ID in model_IDs) {
  column_name <- paste0("predictions_", model_ID)
  conf_and_cons <- measures(observations = dataset$occurrences,
                            predictions = dataset[, column_name, drop = TRUE],
                            evaluation_mask = !dataset$training_mask,
                            df = TRUE)
  if (model_ID == model_IDs[1]) {
    conf_and_cons_df <- conf_and_cons
  } else {
    conf_and_cons_df <- rbind(conf_and_cons_df, conf_and_cons)
  }
}
rownames(conf_and_cons_df) <- model_IDs
conf_and_cons_df

## -----------------------------------------------------------------------------
conf_and_cons_list <- lapply(X = model_IDs,
                             FUN = function(model_ID) {
                               column_name <- paste0("predictions_", model_ID)
                               measures(observations = dataset$occurrences,
                                        predictions = dataset[, column_name, drop = TRUE],
                                        evaluation_mask = !dataset$training_mask,
                                        df = TRUE)
                             })
conf_and_cons_df <- do.call(what = rbind,
                            args = conf_and_cons_list)
rownames(conf_and_cons_df) <- model_IDs
conf_and_cons_df

