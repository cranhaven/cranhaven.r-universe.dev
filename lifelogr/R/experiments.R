#' Merge a list of lists into one list
#' 
#' @description merges list of lists specifying source and variables from each
#'              source into one list
#' 
#' @param list_of_lists list of lists, each with structure 
#'                      list(source1 = c(var1, var2), source2 = c(var3, var4))
#'                      where source is a source of data as defined in a Person
#'                      object, and var1 and var2 are variables from source1, 
#'                      while var3 and var4 are variables from source2
#' @return one list, with structure list(source1 = c(var1, var2), 
#'                                       source2 = c(var3, var4)),
#'         where variables from the same source have been grouped in that 
#'         source's sublist
#' @export
#' @examples
#' variables = list("fitbit_intraday" = c("steps"), 
#'                 "fitbit_daily" = c("sleepDuration"),
#'                 "util" = c("day_of_week", "day_type", "month"))
#' measures = list("fitbit_daily" = c("distance", "restingHeartRate"))
#' all_variables <- merge_lists(list(variables, measures))
#' 
merge_lists <- function(list_of_lists){

  merged_list = list()
  existing_sources = list()
  
  for (l in list_of_lists){

    sources <- names(l)
    for (source in sources){
      if(source %in% existing_sources){
        merged_list[[source]] <- c(merged_list[[source]], l[[source]])
      }
      else {
        merged_list[[source]] <- l[[source]]
        existing_sources <- c(existing_sources, source)
      }
    }
  }
  return(merged_list)
}
  
  
#' Do the specified analysis of the impact of the variables on the measure
#' 
#' @description Performs the analysis specified on the variables (X) and
#' measures (Y).
#' 
#' @param person an instantiated Person object
#' @param variables list of variables in person of interest, with structure 
#'                      list(source1 = c(var1, var2), source2 = c(var3, var4))
#'                      where source is a source of data as defined in a Person
#'                      object, and var1 and var2 are variables from source1, 
#'                      while var3 and var4 are variables from source2
#' @param measures list of measures in person of interest, with structure 
#'                      list(source1 = c(var1, var2), source2 = c(var3, var4))
#'                      where source is a source of data as defined in a Person
#'                      object, and var1 and var2 are variables from source1, 
#'                      while var3 and var4 are variables from source2
#' @param analysis list of ways in which to analyze the relationship between 
#'                 each variable and each measure - options are "plot",
#'                 "correlation", "anova", "compare_groups", "regression"
#' @param time_var the time variable that variables and measures are 
#'                 observed in (time, date, or datetime)
#' @return NULL - results of analysis chosen are printed
#' @export
#' @examples
#' data(EX)
#' experiment(person = EX, variables = list("fitbit_daily" = c("sleepDuration"),
#'                                          "util" = c("day_of_week")),
#'                         measures = list("fitbit_daily" = 
#'                                           c("restingHeartRate")),
#'                         analysis = c("plot"), time_var = c("date"))
#'
experiment <- function(person, variables, measures,
                       analysis = c("plot", "correlation", 
                                    "anova", "compare_groups", "regression"),
                       time_var) {
  
  # create dataset
  dataset <- create_dataset(person, all_variables = merge_lists(list(variables,
                                                                     measures)),
                            time_var = time_var)

  # call the type of analysis requested
  for (type in analysis){
    switch(type,
           "plot" = l_plot(dataset, person, variables, measures, time_var), 
           "correlation" = correlation(dataset, person, variables, measures, 
                                       time_var),
           "anova" = l_anova(dataset, person, variables, measures, time_var),
           "compare_groups" = compare_groups(dataset, person, variables, 
                                             measures),
           "regression" = l_regression(dataset, person, variables, measures, 
                                       time_var),
           stop("your type of analysis did not match an available options")
          )
  }
}


#' Creates a dataset across data sources in a Person object
#' 
#' @description Joins all variables (across sources) by time_var into one 
#'              dataframe, which is returned
#' 
#' @param person an instantiated Person object
#' @param all_variables list of variables in person to join, with structure 
#'                      list(source1 = c(var1, var2), source2 = c(var3, var4))
#'                      where source is a source of data as defined in a Person
#'                      object, and var1 and var2 are variables from source1, 
#'                      while var3 and var4 are variables from source2
#' @param time_var the time variable to join the datasets across (time, date, 
#'                                                                or datetime)
#'                 as a character
#' @return one dataframe with all variables in all_variables, joined by time_var
#' @export
#' @examples
#' data(EX)
#' dataset <- create_dataset(person = EX,
#'                           all_variables = list("util" = c("month"),
#'                                                "fitbit_daily" = c("steps")),
#'                           time_var = c("date"))
#'
create_dataset <- function(person, all_variables, time_var){
  if(is.na(time_var)){
    stop("time_var must be 'time', 'date', or 'datetime'")
  }
  if(!(time_var %in% c("time", "date", "datetime"))){
    stop("time_var must be 'time', 'date', or 'datetime'")
  }
  
  all_dfs <- list()
  
  # for each source (name of a df), grab columns from that source + time_var
  
  for (source in names(all_variables)){
    all_dfs[[source]] <- 
      person[[source]][, c(time_var, all_variables[[source]])]
  }
  
  dataset <- Reduce(function(x, y) merge(x, y, all = TRUE, by = time_var), 
                    all_dfs)
  return(dataset)
}


#' Plot each variable vs each measure of interest
#' 
#' @description Plots each variable vs each measure listed. Can pass in a 
#' dataset from create_dataset, or function calls create_dataset itself.
#' 
#' @param dataset dataset from create_dataset that contains all variables
#'                and measures of interest
#' @param person an instantiated Person object
#' @param variables list of variables in person of interest, with structure 
#'                      list(source1 = c(var1, var2), source2 = c(var3, var4))
#'                      where source is a source of data as defined in a Person
#'                      object, and var1 and var2 are variables from source1, 
#'                      while var3 and var4 are variables from source2
#' @param measures list of measures in person of interest, with structure 
#'                      list(source1 = c(var1, var2), source2 = c(var3, var4))
#'                      where source is a source of data as defined in a Person
#'                      object, and var1 and var2 are variables from source1, 
#'                      while var3 and var4 are variables from source2
#' @param time_var the time variable that variables and measures are 
#'                 observed in (time, date, or datetime) - only needed if 
#'                 dataset is not passed in
#' @return NULL - plots for each variable vs each measure are printed
#' @export
#' @examples
#' data(EX)
#' 
#' l_plot(person = EX, variables = list("fitbit_daily" = c("sleepDuration",
#'                                                         "steps", "distance"),
#'                                      "util" = c("day_of_week", "day_type")), 
#'        measures = list("fitbit_daily" = c("restingHeartRate")),
#'        time_var = c("date"))
#' 
#' dataset <- create_dataset(person = EX, all_variables = list(
#'                                                "util" = c("month"),
#'                                                "fitbit_daily" = c("steps")),
#'                                        time_var = c("date"))
#'                                        
#' l_plot(dataset, person = EX, variables = list("util" = c("month")),
#'        measures = list("fitbit_daily" = c("steps")))
#'        
#'        
l_plot <- function(dataset = NA, person, variables, measures, time_var = NA) {
  
  # plot each variable against each measure
  if (!is.data.frame(dataset)){
    dataset <- create_dataset(person, all_variables = merge_lists(
                                list(variables, measures)),
                              time_var = time_var)
  }

  for (meas.source in names(measures)) {
    for (measure in measures[[meas.source]]) {
      for (var.source in names(variables)) {
        for (variable in variables[[var.source]]) {

          print(ggplot2::ggplot(dataset) +
                  ggplot2::aes_string(x = variable, y = measure) +
                  ggplot2::geom_point() + 
                  ggplot2::ggtitle(paste(variable, "vs", measure)))
          
        }
      }
    }
  }
}


#' Correlation between each variable vs each measure
#' 
#' @description Prints and returns Pearson's correlation between each variable
#' and each measure listed. Can pass in a 
#' dataset from create_dataset, or function calls create_dataset itself.
#' 
#' @param dataset dataset from create_dataset that contains all variables
#'                and measures of interest
#' @param person an instantiated Person object
#' @param variables list of variables in person of interest, with structure 
#'                      list(source1 = c(var1, var2), source2 = c(var3, var4))
#'                      where source is a source of data as defined in a Person
#'                      object, and var1 and var2 are variables from source1, 
#'                      while var3 and var4 are variables from source2
#' @param measures list of measures in person of interest, with structure 
#'                      list(source1 = c(var1, var2), source2 = c(var3, var4))
#'                      where source is a source of data as defined in a Person
#'                      object, and var1 and var2 are variables from source1, 
#'                      while var3 and var4 are variables from source2
#' @param time_var the time variable that variables and measures are 
#'                 observed in (time, date, or datetime) - only needed if 
#'                 dataset is not passed in
#' @return Pearson's correlation between each variable and each measure
#' @section Note:
#' `correlation` uses "pairwise.complete.obs", which only computes the 
#' correlation between all complete pairs of observations.
#' @export
#' @examples
#' data(EX)
#' 
#' dataset <- create_dataset(person = EX, 
#'             all_variables = list("fitbit_daily" = c("sleepDuration", 
#'                                                     "steps")),
#'             time_var = c("date"))
#'                       
#' correlation_df <- correlation(dataset, person = EX, 
#'                             variables = list("fitbit_daily" = 
#'                                                  c("sleepDuration")),
#'                             measures = list("fitbit_daily" = c("steps")),
#'                            time_var = "date")
#'        
#'       
correlation <- function(dataset = NA, person, variables, measures, 
                        time_var = NA){
  if (!is.data.frame(dataset)){
    dataset <- create_dataset(person, all_variables = merge_lists(
                                list(variables, measures)),
                              time_var = time_var)
    }
  
  pearson_corr <- cor(dataset[, unlist(variables)], dataset[, unlist(measures)],
                      method = "pearson",
                      use = "pairwise.complete.obs")
  print(pearson_corr)
  return(pearson_corr)
}


#' ANOVA test to assess impact of all variables (together) upon each measure
#' 
#' @description Prints and returns ANOVA test on all variables and
#' interactions for each measure. Can pass in a 
#' dataset from create_dataset, or function calls create_dataset itself.
#' 
#' @param dataset dataset from create_dataset that contains all variables
#'                and measures of interest
#' @param person an instantiated Person object
#' @param variables list of variables in person of interest, with structure 
#'                      list(source1 = c(var1, var2), source2 = c(var3, var4))
#'                      where source is a source of data as defined in a Person
#'                      object, and var1 and var2 are variables from source1, 
#'                      while var3 and var4 are variables from source2
#' @param measures list of measures in person of interest, with structure 
#'                      list(source1 = c(var1, var2), source2 = c(var3, var4))
#'                      where source is a source of data as defined in a Person
#'                      object, and var1 and var2 are variables from source1, 
#'                      while var3 and var4 are variables from source2
#' @param time_var the time variable that variables and measures are 
#'                 observed in (time, date, or datetime) - only needed if 
#'                 dataset is not passed in
#' @return list of ANOVAs for each measure
#' @export
#' @examples
#' data(EX)
#' 
#' dataset <- create_dataset(person = EX,
#'                           all_variables = list("util" = c("day_of_week"), 
#'                      "fitbit_daily" = c("sleepDuration", 
#'                                         "steps",
#'                                         "restingHeartRate")),
#'                                          time_var = c("date"))

#' all_anovas <- l_anova(dataset, person = EX, variables = list("util" = c("day_of_week"), 
#'                                                "fitbit_daily" = c("sleepDuration",
#'                                                "steps")),
#'                       measures = list("fitbit_daily" = c("restingHeartRate")))
#'        
l_anova <- function(dataset = NA, person, variables, measures, time_var = NA){
  anovas <- list()
  
  if (!is.data.frame(dataset)){
    dataset <- create_dataset(person, all_variables = merge_lists(list(variables,
                                                                       measures)),
                              time_var = time_var)
  }
  measures_flat <- unlist(measures)
  variables_flat <- unlist(variables)
  # for each measure, fit linear model with interactions, run anova
  for (i in 1:length(measures_flat)) {
    f <- paste0(measures_flat[[i]], " ~ ", 
               "(", paste(variables_flat, collapse = " + "), ")^2")
    lin_model <- do.call("lm", list(as.formula(f), data = as.name("dataset")))
    lin_anova <- anova(lin_model)
    print(f)
    print(lin_anova)
    anovas <- c(anovas, lin_anova)
  }
  return(anovas)
}

#' Prints statistics on dataset, grouped by group assignments
#' 
#' @description Groups the dataset by each group assignment named in 
#' names_of_groupings (must be found in person$groupings, 
#' or passed in as a dataframe
#' in the list of addl_grouping_assignments). Prints statistics by group.
#' 
#' @param dataset dataset from create_dataset that contains all variables
#'                and measures of interest
#' @param person an instantiated Person object
#' @param names_of_groupings names of groupings to test (default is groupings
#'                           in person$groupings)
#' @param addl_grouping_assignments list of named dataframes, where each data 
#'                                  frame provides a mapping from a value of a 
#'                                  specified variable to group on to the group 
#'                                  assignment for observations with that value 
#'                                  for that variable
#' @param variables_to_compare variables to print grouped statistics on
#' @return NULL - prints statistics
#' @export
#' @examples
#' data(EX)
#' 
#' dataset <- create_dataset(person = EX, all_variables = list("util" = c("month"), 
#'                                   "fitbit_daily" = c("sleepDuration", "steps",
#'                                   "restingHeartRate")), time_var = c("date"))
#'                                   
#' indiv_months <- data.frame("month"= c("Jan", "Feb", "Mar", "Apr", "May",
#'                                       "Jun", "Jul", "Aug", "Sep", "Oct", 
#'                                       "Nov", "Dec"),
#'                            "group" = c(1:12))
#'compare_groups(dataset, person = EX, 
#'             addl_grouping_assignments = list("indiv_months" = indiv_months), 
#'             names_of_groupings = c("indiv_months"),
#'             variables_to_compare = c("steps", "restingHeartRate"))
#' 
#'        
compare_groups <- function(dataset, person, names_of_groupings = NA, 
                  addl_grouping_assignments = NA, variables_to_compare){

  if (all(is.na(names_of_groupings))) {
    names_of_groupings <- names(person$groupings)
  }
  
  # append addl_grouping_assignments to person's grouping assignments
  all_group_maps <- c(person$groupings, addl_grouping_assignments)

  # From the dataset, keep the variables to compare, join on each groupings 
  # assignment with the column labeled by the groupings name
  for (grouping in names_of_groupings){
    
    # join this grouping onto the dataset
    merge_var <- names(all_group_maps[[grouping]])[names(all_group_maps[[grouping]]) != "group"]
    print(merge_var)
    g_dataset <- merge(dataset, all_group_maps[[grouping]], by = merge_var)

    # for each variable in variables to compare
    group_stats <- function(variable){
      print(variable)
      print(dplyr::summarise_(dplyr::group_by(g_dataset, group),
                mean = lazyeval::interp(~mean(v), v = as.name(variable)),
                sd = lazyeval::interp(~sd(v), v = as.name(variable))))
    }
    
    lapply(variables_to_compare, group_stats)
  }
}


#' Performs linear regression with all variables and interactions upon each measure
#' 
#' @description Prints and returns linear regression on all variables and
#' interactions for each measure. Can pass in a 
#' dataset from create_dataset, or function calls create_dataset itself.
#' 
#' @param dataset dataset from create_dataset that contains all variables
#'                and measures of interest
#' @param person an instantiated Person object
#' @param variables list of variables in person of interest, with structure 
#'                      list(source1 = c(var1, var2), source2 = c(var3, var4))
#'                      where source is a source of data as defined in a Person
#'                      object, and var1 and var2 are variables from source1, 
#'                      while var3 and var4 are variables from source2
#' @param measures list of measures in person of interest, with structure 
#'                      list(source1 = c(var1, var2), source2 = c(var3, var4))
#'                      where source is a source of data as defined in a Person
#'                      object, and var1 and var2 are variables from source1, 
#'                      while var3 and var4 are variables from source2
#' @param time_var the time variable that variables and measures are 
#'                 observed in (time, date, or datetime) - only needed if dataset
#'                 is not passed in
#' @return list of linear models for each measure
#' @export
#' @examples
#' data(EX)
#' 
#' dataset <- create_dataset(person = EX,
#'                           all_variables = list("util" = c("day_of_week"), 
#'                      "fitbit_daily" = c("sleepDuration", 
#'                                         "steps",
#'                                         "restingHeartRate")),
#'                                          time_var = c("date"))

#' all_models <- l_regression(dataset, person = EX, variables = list("util" = c("day_of_week"), 
#'                                                "fitbit_daily" = c("sleepDuration",
#'                                                "steps")),
#'                       measures = list("fitbit_daily" = c("restingHeartRate")))
#'        
l_regression <- function(dataset = NA, person, variables, measures,
                         time_var = NA){
  if (!is.data.frame(dataset)){
    dataset <- create_dataset(person, all_variables = merge_lists(list(variables,
                                                                       measures)),
                              time_var = time_var)
  }
  models <- list()
  measures_flat <- unlist(measures)
  variables_flat <- unlist(variables)
  
  # for each measure, fit linear model with interactions, run anova
  for (i in 1:length(measures_flat)) {
    f <- paste0(measures_flat[[i]], " ~ ", 
               "(", paste(variables_flat, collapse=" + "), ")^2", sep="")
    print(f)
    lin_model <- do.call("lm", list(as.formula(f), data = as.name("dataset")))
    print(summary(lin_model))
    models <- c(models, lin_model)
  }
}