
#' Read in the pedigree, census, and sampling information from the spip run
#'
#' This function is run after `run_spip()`.  It assumes that `run_spip()`
#' has left the files: `spip_pedigree.tsv`, `spip_prekill_census.tsv`, and
#' `spip_samples.tsv`, `spip_postkill_census.tsv`, `spip_deaths.tsv`,
#' `spip_genotypes.tsv`, and `spip_migrants.tsv`
#'  inside the directory where `run_spip()` was run.
#'
#' For an example of its use, see the Vignette:
#' `vignette("species_1_simulation", package = "CKMRpop")`.
#' @param dir  the path to the directory where spip was run.  This is
#' returned by `run_spip()`.
#' @param num_generations how many generations back do you wish to consider
#' for find relatives of
#' each sampled individual.  0 means just the individual themselves (so, not very
#' interesting, and you likely wouldn't ever use it. 1 means up to
#' and including the parents; 2 means up to and including the grandparents; 3 means up to
#' and including the great grandparents; and so forth.
#' @return A list of tibbles.  Each tibble is a named component of
#' the return list.  The names are as follows:
#' - `pedigree`
#' - `census_prekill`,
#' - `census_postkill`,
#' - `samples`,
#' - `deaths`,
#' - `genotypes`,
#' - `migrants`
#'
#' You can inspect some example output in
#' the package data object `three_pops_with_mig_slurped_results`
#' @export
#' @examples
#' # see Vignette: vignette("species_1_simulation", package = "CKMRpop")
slurp_spip <- function(
  dir,
  num_generations
) {

  ped_file <- file.path(dir, "spip_pedigree.tsv")
  census_file <- file.path(dir, "spip_prekill_census.tsv")
  sample_file <- file.path(dir, "spip_samples.tsv")
  post_census_file <- file.path(dir, "spip_postkill_census.tsv")
  deaths_file <- file.path(dir, "spip_deaths.tsv")
  genos_file <- file.path(dir, "spip_genotypes.tsv")
  migrants_file <- file.path(dir, "spip_migrants.tsv")


  # read in the files, and do any necessary processing
  ped <- vroom::vroom(
    file = ped_file,
    delim = "\t",
    col_types = "iiccc"
  )
  census <- vroom::vroom(
    file = census_file,
    delim = "\t",
    col_types = "iiiii"
  )
  post_census <- vroom::vroom(
    file = post_census_file,
    delim = "\t",
    col_types = "iiiii"
  )
  death_reports <- vroom::vroom(
    file = deaths_file,
    delim = "\t",
    col_types = "cii"
  )
  migrants <- vroom::vroom(
    file = migrants_file,
    delim = "\t",
    col_types = "iic"
  ) %>%
    separate(
      event,
      into = c("ID", "from_pop", "arrow", "to_pop"),
      sep = " +",
      convert = TRUE
    ) %>%
    select(ID, everything()) %>%
    select(-arrow)

  samples <- vroom::vroom(
    file = sample_file,
    delim = "\t",
    col_types = "ccccccc"
  ) %>%
    mutate(
      samp_years_list_pre = str_split(syears_pre, "  *"),
      samp_years_list_pre = map(.x = samp_years_list_pre, .f = function(x) as.integer(x)),
      samp_years_list = str_split(syears_post, "  *"),
      samp_years_list = map(.x = samp_years_list, .f = function(x) as.integer(x)),
      samp_years_list_dur = str_split(syears_dur, "  *"),
      samp_years_list_dur = map(.x = samp_years_list_dur, .f = function(x) as.integer(x))
    ) %>%
    select(-syears_pre, -syears_post, -syears_dur) %>%
    extract(
      ID,
      into = c("sex", "born_year", "born_pop"),
      regex = "^([MF])([0-9]+)_([0-9]+)",
      remove = FALSE,
      convert = TRUE
    ) %>%
    mutate(samp_years_list_post = samp_years_list)  # this is a weird thing I am doing here.  I realized that
                                                    # gtyp-ppn-{male,fem}-post should be what people usually use,
                                                    # so I make that the samp_years_list that gets used downstream.


  #%>%  # now, here we add a list column with the ancestors over num_generations
    #  mutate(
    #    ancestor_list = ancestor_vectors(
    #      indivs = ID,
    #      ped = ped,
    #      num_generations = num_generations + 1
    #    )
    #  )
    # The above implementation is too slow for millions of individuals


  # Now, get a data frame of the samples' ancestor and relatives and
  # join it to samples.
  SAR <- find_ancestors_and_relatives_of_samples(P = ped, S = samples$ID, num_generations)


  # finally, read in the genotypes
  genos <- readr::read_tsv(
    file = genos_file,
    col_names = FALSE
  )
  genos <- genos[, -length(genos)]  # this removes that empty column at the end
  names(genos) <- c("ID", paste("Locus", 1:(length(genos) - 1), sep = "_"))

  list(
    pedigree = ped,
    census_prekill = census,
    census_postkill = post_census,
    samples = left_join(samples, SAR, by = c("ID" = "sample_id")),
    deaths = death_reports,
    genotypes = genos,
    migrants = migrants
  )

}
