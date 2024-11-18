# linear interpolation of counts based on BD
lin_interp = function(df, BD_min, BD_max, n=20){
  stopifnot(!is.null(df$Buoyant_density))
  stopifnot(!is.null(df$Count))

  # linear interpolation function
  BDs = as.numeric(as.character(df$Buoyant_density))
  lin_fun = stats::approxfun(x=BDs, y=as.numeric(as.character(df$Count)))

  # BDs (x) for interplation of abundances (y)
  BD_x = seq(BD_min, BD_max, length.out=n)
  Count_y = lin_fun(BD_x)
  Count_y = ifelse(is.na(Count_y), 0, Count_y)

  df_int = data.frame(
    Buoyant_density = BD_x,
    Count_interp = Count_y
  )
  return(df_int)
}

#' delta_BD calculation
#'
#' Calculate delta_BD as described in
#' \href{http://www.ncbi.nlm.nih.gov/pmc/articles/PMC4867679/}{Pepe-Ranney et al., 2016}.
#'
#' Basically, the abundance of each OTU is interpolated at specific BD values in order to
#' have abundance values at consistent points across gradients (gradient fraction BDs
#' normally vary from gradient to gradient). The center of mass (CM) is calculcated from these
#' interpolated values, which is the weighted mean BD with interpolated OTU abundances
#' used as weights (ie., where in the density gradient contains the 'center' of the OTU
#' abundance distribution). Delta_BD is then calculated by substracting the CM for the
#' unlabeled control gradient from the labeled treatment gradient.
#'
#' The delta_BD calculation will be a comparison between unlabled control and labeled
#' treatment samples. These samples are distinguished from each other with the
#' 'control_expr' parameter. NOTE: if multiple gradients fall into the control or
#' treatment category, they will be treated as one gradient (which may be OK if
#' you want to combine replicate gradients).
#'
#' NaN values may occur due low abundances.
#'
#' The BD range used for interpolation is set by the min/max of all buoyant density
#' values in the phyloseq object (standardize across).
#'
#' @param physeq  Phyloseq object
#' @param control_expr  An expression for identifying unlabeled control
#' samples in the phyloseq object (eg., "Substrate=='12C-Con'")
#' @param n  How many evenly-spaced buoyant density values to use for linear
#' interpolation of abundances.
#' @param BD_min  The minimum BD value of the BD range used for OTU abundance interpolation.
#' If NULL, then BD_min will be the minimum of all BD values in the phyloseq object.
#' @param BD_max  The maximum BD value of the BD range used for OTU abundance interpolation.
#' If NULL, then BD_max will be the maximum of all BD values in the phyloseq object.
#'
#' @return data.frame with delta_BD values for each OTU. 'CM' stands for 'center of mass'.
#'
#' @export
#'
#' @examples
#' data(physeq_S2D2_l)
#' # just selecting 1 treatment-control comparison
#' physeq = physeq_S2D2_l[[1]]
#'
#' \dontrun{
#' # calculating delta_BD
#' df = delta_BD(physeq, control_expr='Substrate=="12C-Con"')
#' head(df)
#'
#' # In this example, the replicate gradients will be combined for treatments/controls
#' data(physeq_rep3)
#' df = delta_BD(physeq_rep3, control_expr='Treatment=="12C-Con"')
#' head(df)
#' }
#'
delta_BD = function(physeq, control_expr, n=20, BD_min=NULL, BD_max=NULL){
  # atom excess
  df_OTU = qSIP_atom_excess_format(physeq, control_expr, treatment_rep=NULL)
  if(nrow(df_OTU) == 0){
    stop('No rows in OTU table after qSIP_atom_excess_format(). Check control_exp & treatment_rep')
  }

  # total sum scaling
  df_OTU = df_OTU %>%
    dplyr::group_by_("SAMPLE_JOIN") %>%
    dplyr::mutate_(Count = "as.numeric(as.character(Count))",
                   Count = "Count / sum(Count)",
                   Count = "ifelse(is.na(Count), 0, Count)") %>%
    dplyr::ungroup()

  # BD min/max
  df_OTU$Buoyant_density = as.numeric(as.character(df_OTU$Buoyant_density))
  if(is.null(BD_min)){
    BD_min = df_OTU$Buoyant_density %>% min
  }
  if(is.null(BD_max)){
    BD_max = df_OTU$Buoyant_density %>% max
  }

  # calculating BD shift
  ## params for standard-eval
  nest_cols = c('SAMPLE_JOIN', 'Count', 'Buoyant_density')
  dots = list(~lapply(data, lin_interp, n=n, BD_min=BD_min, BD_max=BD_max))
  dots = stats::setNames(dots, "data")
  ## calculation
  df_OTU = df_OTU %>%
    # linear interpolation for each OTU in each gradient
    dplyr::group_by_("IS_CONTROL", "OTU") %>%
    tidyr::nest(nest_cols) %>%
    dplyr::mutate_(.dots=dots) %>%
    tidyr::unnest() %>%
    # center of mass
    dplyr::group_by_("IS_CONTROL", "OTU") %>%
    dplyr::summarize_(center_of_mass = "stats::weighted.mean(x=Buoyant_density,
                                                             w=Count_interp)") %>%
    # delta BD
    dplyr::group_by_("OTU") %>%
    dplyr::mutate_(IS_CONTROL="ifelse(IS_CONTROL==TRUE, 'CM_control', 'CM_treatment')") %>%
    tidyr::spread("IS_CONTROL", "center_of_mass") %>%
    dplyr::mutate_(delta_BD="CM_treatment - CM_control") %>%
    dplyr::ungroup()

  return(df_OTU)
}

