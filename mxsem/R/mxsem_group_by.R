#' mxsem_group_by
#'
#' creates a multi-group model from an OpenMx model.
#'
#' mxsem_group_by creates a multi-group model by splitting the data found
#' in an mxModel object using dplyr's group_by function. The general idea is
#' as follows:
#'
#' 1. The function extracts the data from mxModel
#' 2. The data is split using the group_by function of dplyr with the variables
#' in grouping_variables
#' 3. a separate model is set up for each group. All parameters that match
#' those specified in the parameters argument are group specific
#'
#' **Warning**: The multi-group model may differ from **lavaan**! For instance,
#' **lavaan** will automatically set the latent variances for all but the first
#' group free if the loadings are fixed to equality. Such automatic procedures
#' are not yet implemented in **mxsem**.
#'
#'
#' @param mxModel mxModel with the entire data
#' @param grouping_variables Variables used to split the data in groups
#' @param parameters the parameters that should be group specific. By default
#' all parameters are group specific.
#' @param use_grepl if set to TRUE, grepl is used to check which parameters are
#' group specific. For instance, if parameters = "a" and use_grepl = TRUE, all parameters
#' whose label contains the letter "a" will be group specific. If use_grep = FALSE
#' only the parameter that has the label "a" is group specific.
#' @return mxModel with multiple groups. Use get_groups to extract the groups
#' @export
#' @examples
#' # THE FOLLOWING EXAMPLE IS ADAPTED FROM
#' # https://openmx.ssri.psu.edu/docs/OpenMx/latest/_static/Rdoc/mxModel.html
#' library(mxsem)
#'
#' model <- 'spatial =~ visual + cubes + paper
#'           verbal  =~ general + paragrap + sentence
#'           math    =~ numeric + series + arithmet'
#'
#' mg_model <- mxsem(model = model,
#'                   data  = OpenMx::HS.ability.data) |>
#'   # we want separate models for all combinations of grades and schools:
#'   mxsem_group_by(grouping_variables = "school") |>
#'   mxTryHard()
#'
#' # let's summarize the results:
#' summarize_multi_group_model(mg_model)
mxsem_group_by <- function(mxModel,
                           grouping_variables,
                           parameters = c(".*"),
                           use_grepl = TRUE){
  warning("This function is very experimental and may not yet work properly. Use with caution.")

  if(!is(mxModel, "MxRAMModel"))
    stop("mxModel must be of class MxRAMModel.")
  if(mxModel$data$type != "raw")
    stop("The data of mxModel must be of type 'raw'")

  splitted_data <- mxModel$data$observed |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_variables))) |>
    dplyr::group_split()

  n_groups <- length(splitted_data)

  names(splitted_data) <- paste0("group_", seq_len(n_groups))

  group_specific_parameters <- c()
  parameter_labels <- names(OpenMx::omxGetParameters(mxModel))
  for(p in parameters){
    if(use_grepl){
      group_specific_parameters <- c(group_specific_parameters,
                                     parameter_labels[grepl(p, parameter_labels)])
    }else{
      group_specific_parameters <- c(group_specific_parameters,
                                     parameter_labels[parameter_labels == p]
      )
    }
  }
  common_parameters <- parameter_labels[!parameter_labels %in% group_specific_parameters]

  message(paste0("The following parameters will be the same across groups: ",
                 paste0(common_parameters, collapse = ", ")))
  message(paste0("The following parameters will be group specific: ",
                 paste0(group_specific_parameters, collapse = ", ")))

  mg_model <- OpenMx::mxModel(name = mxModel$name)

  fitfunction <- c()

  for(gr in seq_len(n_groups)){

    fitfunction <- c(fitfunction,
                     paste0(mxModel$name, "_group_", gr, ".fitfunction"))

    current_model <- OpenMx::mxModel(
      name = paste0(mxModel$name, "_group_", gr),
      mxModel,
      OpenMx::mxData(splitted_data[[gr]], type = "raw")
    )

    current_parameter_labels <- names(OpenMx::omxGetParameters(mxModel))

    for(p in parameters){
      if(use_grepl){
        current_parameter_labels[grepl(p, current_parameter_labels)] <- paste0(
          current_parameter_labels[grepl(p, current_parameter_labels)],
          "_group_", gr
        )
      }else{
        current_parameter_labels[current_parameter_labels == p] <- paste0(
          current_parameter_labels[current_parameter_labels == p],
          "_group_", gr
        )
      }
    }

    current_model <- OpenMx::omxSetParameters(model = current_model,
                                              labels = names(OpenMx::omxGetParameters(mxModel)),
                                              values = OpenMx::omxGetParameters(mxModel),
                                              newlabels = current_parameter_labels)
    mg_model <- OpenMx::mxModel(
      mg_model,
      current_model
    )
  }

  mg_model <- OpenMx::mxModel(
    mg_model,
    # define fitfunction as the sum of the fitfunctions of the group models
    OpenMx::mxAlgebraFromString(paste0(fitfunction, collapse = " + "),
                                name = "mg_fitfunction"),
    OpenMx::mxFitFunctionAlgebra(algebra = "mg_fitfunction")
  )

  attr(mg_model, which = "groups") <- splitted_data
  attr(mg_model, which = "grouping_variables") <- grouping_variables
  attr(mg_model, which = "common_parameters") <- common_parameters
  attr(mg_model, which = "group_specific_parameters") <- group_specific_parameters

  return(mg_model)
}

#' get_groups
#'
#' returns a list of groups for a multi group model
#' @param multi_group_model multi group model created with mxsem_group_by
#' @return list with data for each group
#' @export
#' @examples
#' # THE FOLLOWING EXAMPLE IS ADAPTED FROM
#' # https://openmx.ssri.psu.edu/docs/OpenMx/latest/_static/Rdoc/mxModel.html
#' library(mxsem)
#'
#' model <- 'spatial =~ visual + cubes + paper
#'           verbal  =~ general + paragrap + sentence
#'           math    =~ numeric + series + arithmet'
#'
#' mg_model <- mxsem(model = model,
#'                   data  = OpenMx::HS.ability.data) |>
#'   # we want separate models for all combinations of grades and schools:
#'   mxsem_group_by(grouping_variables = "school") |>
#'   mxTryHard()
#'
#' # let's summarize the results:
#' summarize_multi_group_model(mg_model)
#'
#' # let's get the groups:
#' get_groups(mg_model)
get_groups <- function(multi_group_model){
  return(attr(multi_group_model, "groups"))
}


#' summarize_multi_group_model
#'
#' summarize the results of a multi group model created with mxsem_group_by
#' @param multi_group_model multi group model created with mxsem_group_by
#' @return list with goup specific parameters and common parameters
#' @export
#' @examples
#' # THE FOLLOWING EXAMPLE IS ADAPTED FROM
#' # https://openmx.ssri.psu.edu/docs/OpenMx/latest/_static/Rdoc/mxModel.html
#' library(mxsem)
#'
#' model <- 'spatial =~ visual + cubes + paper
#'           verbal  =~ general + paragrap + sentence
#'           math    =~ numeric + series + arithmet'
#'
#' mg_model <- mxsem(model = model,
#'                   data  = OpenMx::HS.ability.data) |>
#'   # we want separate models for all combinations of grades and schools:
#'   mxsem_group_by(grouping_variables = "school") |>
#'   mxTryHard()
#'
#' # let's summarize the results:
#' summarize_multi_group_model(mg_model)
summarize_multi_group_model <- function(multi_group_model){

  groups <- attr(multi_group_model, which = "groups")
  grouping_variables <- attr(multi_group_model, which = "grouping_variables")
  common_parameters <- attr(multi_group_model, "common_parameters")
  parameters <- OpenMx::omxGetParameters(model = multi_group_model)
  summmarized <- summary(multi_group_model)

  parameter_table <- vector("list", length(groups) + 1)
  names(parameter_table) <- c("common parameters",
                              names(groups))

  parameter_table[["common parameters"]] <- summmarized$parameters[summmarized$parameters$name %in% common_parameters,]

  for(gr in seq_len(length(groups))){

    group_name <- names(groups)[gr]
    parameter_table[[group_name]] <- list(
      parameters = summmarized$parameters[grepl(pattern = paste0(group_name, "$"),
                                                                  x = names(parameters)),],
      group_setting = groups[[gr]][,grouping_variables] |>
        unique()
    )
  }

  class(parameter_table) <- "multi_group_parameters"
  return(parameter_table)
}


#' print the multi_group_parameters
#' @param x object from summarize_multi_group_model
#' @param ... not used
#' @method print multi_group_parameters
#' @export
print.multi_group_parameters <- function(x, ...){
  console_width <- getOption("width")
  cat("\n")
  cat("\n")
  cat(paste0(rep("-", console_width), collapse = ""))
  cat("\n")
  cat("Common Parameters:")
  cat("\n")
  print(x$`common parameters`)

  for(gr in 2:length(x)){
    cat(paste0(rep("-", console_width), collapse = ""))
    cat("\n")
    cat(paste0(names(x[gr])[1], ":"))
    cat("\n")
    print(x[[gr]][["group_setting"]])
    cat("\n")
    print(x[[gr]][["parameters"]])
  }
  cat("\n")
  cat(paste0(rep("-", console_width), collapse = ""))
}
