#' @name rave-ui-preset
#' @title Preset reusable front-end components for 'RAVE' modules
#' @description For examples and use cases, please check
#' \code{\link{new_rave_shiny_component_container}}.
#' @param id input or output ID of the element; this ID will be prepended with
#' module namespace
#' @param varname variable name(s) in the module's settings file
#' @param label readable label(s) of the element
#' @param height height of the element
#' @param loader_project_id the ID of \code{presets_loader_project} if
#' different to the default
#' @param loader_subject_id the ID of \code{presets_loader_subject} if
#' different to the default
#' @param loader_reference_id the ID of \code{presets_loader_reference} if
#' different to the default
#' @param loader_electrodes_id the ID of \code{presets_loader_electrodes} if
#' different to the default
#' @param import_setup_id the ID of \code{presets_import_setup_native} if
#' different to the default
#' @param import_blocks_id the ID of \code{presets_import_setup_blocks} if
#' different to the default
#' @param pipeline_repository the pipeline name that represents the 'RAVE'
#' repository from functions such as \code{\link[raveio]{prepare_subject_bare}},
#' \code{\link[raveio]{prepare_subject_with_epoch}}, and
#' \code{\link[raveio]{prepare_subject_power}}
#' @param max_components maximum number of components for compound inputs
#' @param baseline_choices the possible approaches to calculate baseline
#' @param baseline_along_choices the units of baseline
#' @param settings_entries used when importing pipelines, pipeline variable
#' names to be included or excluded, depending on \code{fork_mode}
#' @param fork_mode \code{'exclude'} (default) or \code{'include'}; in
#' \code{'exclude'} mode, \code{settings_entries} will be excluded from the
#' pipeline settings; in \code{'include'} mode, only \code{settings_entries}
#' can be imported.
#' @param from_module which module to extract input settings
#' @param project_varname,subject_varname variable names that should be
#' extracted from the settings file
#' @param mode whether to create new reference, or simply to choose from
#' existing references
#' @param checks whether to check if subject has been applied with 'Notch'
#' filters or 'Wavelet'; default is both.
#' @param gadgets gadget types to include; see \code{type} argument in function
#' \code{\link{output_gadget}}
#' @param start_simple whether to start in simple view and hide optional inputs
#' @param multiple whether to allow multiple inputs
#' @returns A \code{'RAVEShinyComponent'} instance.
#' @seealso \code{\link{new_rave_shiny_component_container}}
NULL

