utils::globalVariables(c("S", "varZ", "mZ", "sum_r", "cur_fid", "k_temp"))

#' Title Helper function for Kendler's FGRS
#'
#' @param tbl tibble with columns cip, lower, upper, and pid (the personal identifier column).
#' @param cov Kinship matrix with proband as first row and column
#' @param pid column name of personal identifier
#' @param cur_dad_id ID of father (not column name, but the actual ID)
#' @param cur_mom_id ID of mother (not column name, but the actual ID)
#' @param env_cor_sib Cohabitation effect, i.e. Factor by which the siblings are weighted. Defaults to 1.
#' @param env_cor_f  Cohabitation effect, i.e. Factor by which the father is weighted. Defaults to 1.
#' @param env_cor_m  Cohabitation effect, i.e. Factor by which the mother is weighted. Defaults to 1.
#'
#' @returns A tibble with family specific values required for Kendler's FGRS calculation.
#'
#' @importFrom stats var pnorm
#' @importFrom dplyr pull
#'
#' @export
#'
#' @examples
#' # See Vignettes.
kendler_family_calculations = function(tbl,
                                       cov,
                                       pid,
                                       cur_dad_id,
                                       cur_mom_id,
                                       env_cor_sib = 1,
                                       env_cor_f = 1,
                                       env_cor_m = 1) {
  #### this function is kept mostly as-is from Morten's code (PAFGRS package) ####
  # assumes proband's full liab is first row in cov and tbl
  k_temp = cov[-1,1]
  dad_ind = names(k_temp) == cur_dad_id
  mom_ind = names(k_temp) == cur_mom_id
  if (is.na(cur_dad_id)) dad_ind = rep(0,length(k_temp))
  if (is.na(cur_mom_id)) mom_ind = rep(0,length(k_temp))
  # we only use kinship matrix, i.e. h2 = 1
  sib_ind = k_temp == 0.5 & !dad_ind & !mom_ind

  sum_r = sum(k_temp)
  k_temp = k_temp -
    (k_temp * sib_ind * rep(env_cor_sib,length(k_temp))) -
    (k_temp * dad_ind * rep(env_cor_f,length(k_temp))) -
    (k_temp * mom_ind * rep(env_cor_m,length(k_temp)))
  w = pull(tbl, cip)[-1]/pnorm(-pull(tbl, upper)[-1])# assuming proband is always first
  w[!is.finite(w)] = 1
  lower = pull(tbl, lower)[-1] # assuming proband is always first
  upper = pull(tbl, upper)[-1] # assuming proband is always first
  m_above = function(t) sapply(t, function(t) f = tnorm_mean(mu = 0, sigma = 1, lower =    t, upper = Inf))
  m_below = function(t) sapply(t, function(t) f = tnorm_mean(mu = 0, sigma = 1, lower = -Inf, upper = t))
  z = ifelse(is.finite(lower),
             m_above(lower),
             m_below(upper))

  S <- sum(k_temp * z * w)
  S <- S/sum(k_temp > 0)
  vZ = var(z, na.rm = TRUE)
  tibble(
    !!as.symbol(pid) := tbl[[pid]][[1]],
    S = S,
    varZ = vZ,
    mZ = mean(z),
    sum_r = sum_r)
}


#' Title Kendler's FGRS
#'
#' @param .tbl A matrix, list or data frame that can be converted into a tibble.
#' Must have at least five columns that hold the family identifier, the personal
#' identifier, the role and the lower and upper thresholds. Note that the
#' role must be one of the following abbreviations
#' - \code{g} (Genetic component of full liability)
#' - \code{o} (Full liability)
#' - \code{m} (Mother)
#' - \code{f} (Father)
#' - \code{c[0-9]*.[0-9]*} (Children)
#' - \code{mgm} (Maternal grandmother)
#' - \code{mgf} (Maternal grandfather)
#' - \code{pgm} (Paternal grandmother)
#' - \code{pgf} (Paternal grandfather)
#' - \code{s[0-9]*} (Full siblings)
#' - \code{mhs[0-9]*} (Half-siblings - maternal side)
#' - \code{phs[0-9]*} (Half-siblings - paternal side)
#' - \code{mau[0-9]*} (Aunts/Uncles - maternal side)
#' - \code{pau[0-9]*} (Aunts/Uncles - paternal side).
#' Defaults to \code{NULL}.
#' @param family_graphs A tibble with columns pid and family_graph_col.
#' See prepare_graph for construction of the graphs. The family graphs Defaults to NULL.
#' @param  pid A string holding the name of the column in \code{.tbl} (or \code{family} and
#' \code{threshs}) that hold the personal identifier(s). Defaults to "PID".
#' @param fid A string holding the name of the column in \code{.tbl} or \code{family} that
#' holds the family identifier. Defaults to "fid".
#' @param role A string holding the name of the column in \code{.tbl} that
#' holds the role. Each role must be chosen from the following list of abbreviations
#' - \code{g} (Genetic component of full liability)
#' - \code{o} (Full liability)
#' - \code{m} (Mother)
#' - \code{f} (Father)
#' - \code{c[0-9]*.[0-9]*} (Children)
#' - \code{mgm} (Maternal grandmother)
#' - \code{mgf} (Maternal grandfather)
#' - \code{pgm} (Paternal grandmother)
#' - \code{pgf} (Paternal grandfather)
#' - \code{s[0-9]*} (Full siblings)
#' - \code{mhs[0-9]*} (Half-siblings - maternal side)
#' - \code{phs[0-9]*} (Half-siblings - paternal side)
#' - \code{mau[0-9]*} (Aunts/Uncles - maternal side)
#' - \code{pau[0-9]*} (Aunts/Uncles - paternal side).
#' Defaults to "role".
#' @param family_graphs_col Name of column with family graphs in family_graphs. Defaults to "fam_graph".
#' @param dadcol column name of father in family_graphs or .tbl.
#' @param momcol column name of mother in family_graphs or .tbl.
#' @param env_cor_sib Cohabitation effect, i.e. Factor by which the siblings are weighted. Defaults to 1.
#' @param env_cor_f  Cohabitation effect, i.e. Factor by which the father is weighted. Defaults to 1.
#' @param env_cor_m  Cohabitation effect, i.e. Factor by which the mother is weighted. Defaults to 1.
#'
#' @returns A tibble with summary values used to calculate the kendler FGRS and the FGRS itself.
#'
#' @export
#'
#' @examples
#' # See Vignettes.
kendler = function(.tbl = NULL,
                   family_graphs = NULL,
                   family_graphs_col = "fam_graph",
                   pid = "pid",
                   fid = "fid",
                   role = NULL,
                   dadcol,
                   momcol,
                   env_cor_sib = 1, env_cor_f = 1, env_cor_m = 1) {

  # Validating input specific variables -------------------------------------

  if ( !is.null(.tbl) ) { #### .tbl input ####
    # validating .tbl input
    .tbl <- validating_tbl_input(.tbl = .tbl, pid = pid, fid = fid, role = role)

    # Extracting the (unique) family identifiers
    fam_list <- unique(pull(.tbl, !!as.symbol(fid)))

  } else if ( !is.null(family_graphs) ) { #### Graph input ####

    # validating graph input (nothing is returned)
    return_catch <- validating_graph_input(family_graphs = family_graphs, pid = pid, family_graphs_col = family_graphs_col)

    # Extracting the (unique) family identifiers
    fam_list <- unique(pull(family_graphs, !!as.symbol(pid)))


  } else ( stop("no valid input used.") )


  # Performing Kendler's calculations per family ----------------------------

  res = lapply(1:nrow(family_graphs), function(i){

    if ( !is.null(family_graphs) ) { # family_graph based covariance construction
      info = extract_estimation_info_graph(
        # no need to pass family_Graphs, family_graphs_Col, or 'i' this way:
        cur_fam_graph = family_graphs[[family_graphs_col]][[i]],
        cur_fid = cur_fid,
        h2 = 1,
        pid = pid,
        add_ind = FALSE)

    } else { # role based covariance construction
      # saving to place holder object
      info = extract_estimation_info_tbl(
        .tbl = .tbl,
        pid = pid,
        fid = fid,
        cur_fid = cur_fid,
        role = role,
        h2 = 1,
        add_ind = FALSE)
    }


    # prepare and pass input
    kendler_family_calculations(tbl = info$tbl,
                                cov = info$cov,
                                cur_dad_id = family_graphs[[dadcol]][i],
                                cur_mom_id = family_graphs[[momcol]][i],
                                env_cor_sib = env_cor_sib,
                                env_cor_f = env_cor_f,
                                env_cor_m = env_cor_m,
                                pid = pid)
  }) %>% do.call("bind_rows", .)

  # finalising calculations that require full population
  vs = var(res$S)
  total_varZ = mean(res$varZ) + var(res$mZ)

  # FGRS calculation and return
  res %>% mutate(FGRS = S * vs/(vs + total_varZ/sum_r))
}
