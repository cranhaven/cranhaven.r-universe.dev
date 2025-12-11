## ----include=FALSE------------------------------------------------------------
# knitr::opts_chunk$set(
#   collapse = TRUE,
#   comment = "#>"
# )

## ----load data, echo=TRUE, message=FALSE, warning=FALSE-----------------------
library(dplyr)
library(prettyglm)
data('titanic')
head(titanic) %>%
  select(-c(PassengerId, Name, Ticket)) %>% 
  knitr::kable(table.attr = "style='width:10%;'" ) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

## ----preprocessing 1, echo=TRUE, message=FALSE, warning=FALSE-----------------
# Easy way to convert multiple columns to a factor.
columns_to_factor <- c('Pclass',
                       'Sex',
                       'Cabin', 
                       'Embarked',
                       'Cabintype')
meanage <- base::mean(titanic$Age, na.rm=T)
titanic  <- titanic  %>%
  dplyr::mutate_at(columns_to_factor, list(~factor(.))) %>%
  dplyr::mutate(Age =base::ifelse(is.na(Age)==T,meanage,Age)) 


## ----build model, echo=TRUE---------------------------------------------------
survival_model <- stats::glm(Survived ~ Pclass + 
                                        Sex + 
                                        Fare +
                                        Age +
                                        Embarked + 
                                        SibSp + 
                                        Parch, 
                             data = titanic, 
                             family = binomial(link = 'logit'))

## ----visualise coefficients, echo=TRUE, eval=FALSE, include=TRUE--------------
#  pretty_coefficients(model_object = survival_model)

## ----visualise coefficients type iii, echo=TRUE, eval=FALSE, include=TRUE-----
#  pretty_coefficients(survival_model, type_iii = 'Wald', significance_level = 0.1)

## ----visualise coefficients vi,echo=TRUE, eval=FALSE, include=TRUE------------
#  pretty_coefficients(model_object = survival_model,
#                      type_iii = 'Wald',
#                      significance_level = 0.1,
#                      vimethod = 'permute',
#                      target = 'Survived',
#                      metric = 'auc',
#                      pred_wrapper = predict.glm,
#                      reference_class = 0)

## ----visualise rels, echo=TRUE, eval=FALSE, include=TRUE----------------------
#  pretty_relativities(feature_to_plot= 'Embarked',
#                      model_object = survival_model,
#                      relativity_label = 'Liklihood of Survival'
#                      )

## ----visualise rels 2, echo=TRUE, eval=FALSE, include=TRUE--------------------
#  pretty_relativities(feature_to_plot= 'Fare',
#                      model_object = survival_model,
#                      relativity_label = 'Liklihood of Survival',
#                      upper_percentile_to_cut = 0.1)

## ----build model 2, echo=TRUE-------------------------------------------------
survival_model2 <- stats::glm(Survived ~ Pclass:Fare +
                                         Age +
                                         Embarked:Sex +
                                         SibSp +
                                         Parch,
                              data = titanic,
                              family = binomial(link = 'logit'))

## ----visualise ff F, echo=TRUE, eval=FALSE, include=TRUE----------------------
#  pretty_relativities(feature_to_plot= 'Embarked:Sex',
#                      model_object = survival_model2,
#                      relativity_label = 'Liklihood of Survival',
#                      iteractionplottype = 'facet',
#                      facetorcolourby = 'Sex'
#                      )

## ----visualise ff C, echo=TRUE, eval=FALSE, include=TRUE----------------------
#  pretty_relativities(feature_to_plot= 'Embarked:Sex',
#                      model_object = survival_model2,
#                      relativity_label = 'Liklihood of Survival',
#                      iteractionplottype = 'colour',
#                      facetorcolourby = 'Embarked'
#                      )

## ----visualise ff N, echo=TRUE, eval=FALSE, include=TRUE----------------------
#  pretty_relativities(feature_to_plot= 'Embarked:Sex',
#                      model_object = survival_model2,
#                      relativity_label = 'Liklihood of Survival'
#                      )

## ----visualise cf C, echo=TRUE, eval=FALSE, include=TRUE----------------------
#  pretty_relativities(feature_to_plot= 'Pclass:Fare',
#                      model_object = survival_model2,
#                      relativity_label = 'Liklihood of Survival',
#                      upper_percentile_to_cut = 0.03
#                      )

## ----visualise cf F, echo=TRUE, eval=FALSE, include=TRUE----------------------
#  pretty_relativities(feature_to_plot= 'Pclass:Fare',
#                      model_object = survival_model2,
#                      relativity_label = 'Liklihood of Survival',
#                      iteractionplottype = 'facet',
#                      upper_percentile_to_cut = 0.03,
#                      height = 800
#                      )

## ----fit the splines----------------------------------------------------------
titanic  <- titanic  %>%
  dplyr::mutate(Age_0_18 = prettyglm::splineit(Age,0,18),
                Age_18_35 = prettyglm::splineit(Age,18,35),
                Age_35_120 = prettyglm::splineit(Age,35,120)) %>%
  dplyr::mutate(Fare_0_55 = prettyglm::splineit(Fare,0,55),
                Fare_55_600 = prettyglm::splineit(Fare,55,600))

## ----build model 4, echo=TRUE-------------------------------------------------
survival_model4 <- stats::glm(Survived ~ Pclass +
                                         Sex:Fare_0_55 +
                                         Sex:Fare_55_600 +
                                         Age_0_18 +
                                         Age_18_35 +
                                         Age_35_120 +
                                         Embarked +
                                         SibSp +
                                         Parch,
                              data = titanic,
                              family = binomial(link = 'logit'))

## ----visualise coefficients type spline, echo=TRUE, eval=FALSE, include=TRUE----
#  pretty_coefficients(survival_model4, significance_level = 0.1, spline_seperator = '_')

## ----visualise age spine, echo=TRUE, eval=FALSE, include=TRUE-----------------
#  pretty_relativities(feature_to_plot= 'Age',
#                      model_object = survival_model4,
#                      relativity_label = 'Liklihood of Survival',
#                      spline_seperator = '_'
#                      )

## ----visualise fare spine, echo=TRUE, eval=FALSE, include=TRUE----------------
#  pretty_relativities(feature_to_plot= 'Sex:Fare',
#                      model_object = survival_model4,
#                      relativity_label = 'Liklihood of Survival',
#                      spline_seperator = '_',
#                      upper_percentile_to_cut = 0.03
#                      )

## ----visualise fare Facet, echo=TRUE, eval=FALSE, include=TRUE----------------
#  pretty_relativities(feature_to_plot= 'Sex:Fare',
#                      model_object = survival_model4,
#                      relativity_label = 'Liklihood of Survival',
#                      spline_seperator = '_',
#                      upper_percentile_to_cut = 0.03,
#                      iteractionplottype = 'facet'
#                      )

## ----oneway cts, echo=TRUE, eval=FALSE, include=TRUE--------------------------
#  one_way_ave(feature_to_plot = 'Age',
#              model_object = survival_model4,
#              target_variable = 'Survived',
#              data_set = titanic,
#              upper_percentile_to_cut = 0.1,
#              lower_percentile_to_cut = 0.1)

## ----oneway discrete, echo=TRUE, eval=FALSE, include=TRUE---------------------
#  one_way_ave(feature_to_plot = 'Cabintype',
#              model_object = survival_model4,
#              target_variable = 'Survived',
#              data_set = titanic)

## ----oneway cts facet, echo=TRUE, eval=FALSE, include=TRUE--------------------
#  one_way_ave(feature_to_plot = 'Age',
#              model_object = survival_model4,
#              target_variable = 'Survived',
#              facetby = 'Sex',
#              data_set = titanic,
#              upper_percentile_to_cut = 0.1,
#              lower_percentile_to_cut = 0.1)

## ----custom predict, echo=TRUE, eval=FALSE, include=TRUE----------------------
#  # Custom Predict Function and facet
#  a_custom_predict_function <- function(target, model_object, dataset){
#    dataset <- base::as.data.frame(dataset)
#    Actual_Values <- dplyr::pull(dplyr::select(dataset, tidyselect::all_of(c(target))))
#    if(class(Actual_Values) == 'factor'){
#      Actual_Values <- base::as.numeric(as.character(Actual_Values))
#    }
#    Predicted_Values <- base::as.numeric(stats::predict(model_object, dataset, type='response'))
#  
#    to_return <-  base::data.frame(Actual_Values = Actual_Values,
#                                   Predicted_Values = Predicted_Values)
#  
#    to_return <- to_return %>%
#      dplyr::mutate(Predicted_Values = base::ifelse(Predicted_Values > 0.4,0.4,Predicted_Values))
#    return(to_return)
#  }
#  
#  one_way_ave(feature_to_plot = 'Age',
#              model_object = survival_model4,
#              target_variable = 'Survived',
#              data_set = titanic,
#              upper_percentile_to_cut = 0.1,
#              lower_percentile_to_cut = 0.1,
#              predict_function = a_custom_predict_function)

## ----bucketed aves, echo=TRUE, eval=FALSE, include=TRUE-----------------------
#  actual_expected_bucketed(target_variable = 'Survived',
#                           model_object = survival_model4,
#                           data_set = titanic)

## ----bucketed aves facet, echo=TRUE, eval=FALSE, include=TRUE-----------------
#  actual_expected_bucketed(target_variable = 'Survived',
#                           model_object = survival_model4,
#                           data_set = titanic,
#                           facetby = 'Sex')

