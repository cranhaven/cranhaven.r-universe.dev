
#' Reference data
#'
#' Reference data on various aspects of parliament. This data is useful for
#' providing parameters for other function calls. The functions do not accept
#' any arguments aside from the `tidy` and `tidy_style` parameters,
#' which default to `TRUE` and `'snake_case'`, respectively. To
#' return all a list with tibbles of all reference data,
#' see [mnis_all_reference()].
#' @inheritParams mnis_basic_details
#'
#' @section `mnis_reference` functions:
#' * `ref_address_types` The types of addresses available in
#' member's contact details. Includes websites and social media, as well
#' as physical addresses
#' * `ref_answering_bodies` The bodies that members' can address
#' questions to
#' * `ref_areas` Geographic areas
#' * `ref_area_types` Identifiers for grouping areas
#' (e.g. borough constituencies)
#' * `ref_biography_categories` Member biography categories
#' * `ref_cabinets` Connections that a member has to the cabinet or
#' shadow cabinet
#' * `ref_committees` Identifier for parliamentary committees
#' * `ref_committee_types` Types of parliamentary committees
#' * `ref_constituencies()` All constituencies
#' * `ref_constituency_areas()` The links between constituencies and
#' constituency areas
#' * `ref_constituency_types()` Constituency categories
#' * `ref_countries()` List of countries that could be listed as
#' members' birthplace
#' * `ref_departments()` Government and opposition departments
#' * `ref_disqualification_types()` Types of ways members can be
#' disqualified from sitting in the House
#' * `ref_elections()` Codes of general and by-elections
#' * `ref_election_types()` Election categories
#' * `ref_end_reasons()` Reasons a member may leave the House of
#' Lords or the House of Commons
#' * `ref_experience_types()` Types of non-parliamentary experience
#' members can list
#' * `ref_government_post_departments()` All deparments that can
#' contain government posts
#' * `ref_government_posts()` All government posts
#' * `ref_government_ranks()` All government post ranks
#' * `ref_honourary_prefixes()` The types of honourary prefixes
#' for members
#' * `ref_honour_lists()` The types of honour lists that a member
#' may be honoured in
#' * `ref_honours()` The different honours available to members
#' * `ref_interest_categories()` The categories available for
#' reporting financial interests
#' * `ref_lords_membership_types()` Different types of membership
#' of the House of Lords
#' * `ref_lords_ranks()` Ranks that peers may hold
#' * `ref_opposition_post_departments()` The link between opposition
#' posts and the government department they shadow
#' * `ref_opposition_posts()` Opposition posts
#' * `ref_opposition_ranks()` How opposition posts are ranked
#' * `ref_other_parliaments()` Other parliaments that a member
#' may have sat in
#' * `ref_parliamentary_posts()` The different parliamentary posts
#' available
#' * `ref_parliamentary_ranks()` How those parliamentary posts are
#' ranked
#' * `ref_parliament_types()` Types of parliaments that
#' parliamentary data may link to
#' * `ref_parties()` All parties that members can be affiliated with
#' * `ref_party_sub_types()` Sub-types of parties
#' * `ref_photo_outputs()` Outputs that a photo of a member
#' may be linked to
#' * `ref_statuses()` A member's possible current status in the
#' House
#' * `ref_titles()` Salutory titles
#'
#' @export
#' @seealso [mnis_all_reference()]
#' @rdname mnis_reference
#' @export
ref_address_types <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "AddressTypes"

  mnis_reference_utils(type, tidy, tidy_style)
}

#' @export
#' @rdname mnis_reference
ref_answering_bodies <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "AnsweringBodies"

  mnis_reference_utils(type, tidy, tidy_style)
}
#' @export
#' @rdname mnis_reference
ref_areas <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "Areas"

  mnis_reference_utils(type, tidy, tidy_style)
}
#' @export
#' @rdname mnis_reference
ref_area_types <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "AreaTypes"

  mnis_reference_utils(type, tidy, tidy_style)
}
#' @export
#' @rdname mnis_reference
ref_biography_categories <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "BiographyCategories"

  mnis_reference_utils(type, tidy, tidy_style)
}

#' @export
#' @rdname mnis_reference
ref_cabinets <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "Cabinets"

  mnis_reference_utils(type, tidy, tidy_style)
}
#' @export
#' @rdname mnis_reference
ref_committees <- function(tidy = TRUE, tidy_style = "snake_case") {
  type = "Committees"

  mnis_reference_utils(type, tidy, tidy_style)
}

#' @export
#' @rdname mnis_reference
ref_committee_types <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "CommitteeTypes"

  mnis_reference_utils(type, tidy, tidy_style)
}

#' @export
#' @rdname mnis_reference
ref_constituencies <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "Constituencies"

  mnis_reference_utils(type, tidy, tidy_style)
}

#' @export
#' @rdname mnis_reference
ref_constituency_areas <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "ConstituencyAreas"

  mnis_reference_utils(type, tidy, tidy_style)
}

#' @export
#' @rdname mnis_reference
ref_constituency_types <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "ConstituencyTypes"

  mnis_reference_utils(type, tidy, tidy_style)
}
#' @export
#' @rdname mnis_reference
ref_countries <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "Countries"

  mnis_reference_utils(type, tidy, tidy_style)
}

#' @export
#' @rdname mnis_reference
ref_departments <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "Departments"

  mnis_reference_utils(type, tidy, tidy_style)
}

#' @export
#' @rdname mnis_reference
ref_disqualification_types <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "DisqualificationTypes"

  mnis_reference_utils(type, tidy, tidy_style)
}
#' @export
#' @rdname mnis_reference
ref_elections <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "Elections"

  mnis_reference_utils(type, tidy, tidy_style)
}

#' @export
#' @rdname mnis_reference
ref_election_types <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "ElectionTypes"

  mnis_reference_utils(type, tidy, tidy_style)
}

#' @export
#' @rdname mnis_reference
ref_end_reasons <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "EndReasons"

  mnis_reference_utils(type, tidy, tidy_style)
}

#' @export
#' @rdname mnis_reference
ref_experience_types <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "ExperienceTypes"

  mnis_reference_utils(type, tidy, tidy_style)
}

#' @export
#' @rdname mnis_reference
ref_government_post_departments <- function(tidy = TRUE,
                                            tidy_style = "snake_case") {
  type <- "GovernmentPostDepartments"

  mnis_reference_utils(type, tidy, tidy_style)
}

#' @export
#' @rdname mnis_reference
ref_government_posts <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "GovernmentPosts"

  mnis_reference_utils(type, tidy, tidy_style)
}

#' @export
#' @rdname mnis_reference
ref_government_ranks <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "GovernmentRanks"

  mnis_reference_utils(type, tidy, tidy_style)
}

#' @export
#' @rdname mnis_reference
ref_honourary_prefixes <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "HonouraryPrefixes"

  mnis_reference_utils(type, tidy, tidy_style)
}

#' @export
#' @rdname mnis_reference
ref_honour_lists <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "HonourLists"

  mnis_reference_utils(type, tidy, tidy_style)
}

#' @export
#' @rdname mnis_reference
ref_honours <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "Honours"

  mnis_reference_utils(type, tidy, tidy_style)
}

#' @export
#' @rdname mnis_reference
ref_interest_categories <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "InterestCategories"

  mnis_reference_utils(type, tidy, tidy_style)
}

#' @export
#' @rdname mnis_reference
ref_lords_membership_types <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "LordsMembershipTypes"

  mnis_reference_utils(type, tidy, tidy_style)
}
#' @export
#' @rdname mnis_reference
ref_lords_ranks <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "LordsRanks"

  mnis_reference_utils(type, tidy, tidy_style)
}

#' @export
#' @rdname mnis_reference
ref_opposition_post_departments <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "OppositionPostDepartments"

  mnis_reference_utils(type, tidy, tidy_style)
}

#' @export
#' @rdname mnis_reference
ref_opposition_posts <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "OppositionPosts"

  mnis_reference_utils(type, tidy, tidy_style)
}
#' @export
#' @rdname mnis_reference
ref_opposition_ranks <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "OppositionRanks"

  mnis_reference_utils(type, tidy, tidy_style)
}

#' @export
#' @rdname mnis_reference
ref_other_parliaments <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "OtherParliaments"

  mnis_reference_utils(type, tidy, tidy_style)
}
#' @export
#' @rdname mnis_reference
ref_parliamentary_posts <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "ParliamentaryPosts"

  mnis_reference_utils(type, tidy, tidy_style)
}
#' @export
#' @rdname mnis_reference
ref_parliamentary_ranks <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "ParliamentaryRanks"

  mnis_reference_utils(type, tidy, tidy_style)
}

#' @export
#' @rdname mnis_reference
ref_parliament_types <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "ParliamentTypes"

  mnis_reference_utils(type, tidy, tidy_style)
}

#' @export
#' @rdname mnis_reference
ref_parties <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "Parties"

  mnis_reference_utils(type, tidy, tidy_style)
}

#' @export
#' @rdname mnis_reference
ref_party_sub_types <- function(tidy = TRUE, tidy_style = "snake_case") {

  got <- mnis_query(query = paste0(base_url, "ReferenceData/PartySubTypes/"))

  x <- as.list(got$PartySubTypes$PartySubType)

  x <- unlist(x)

  x <- t(x)

  x <- tibble::as_tibble(as.data.frame(x))

  if (tidy == TRUE) {
    x <- mnis::ref_tidy(x, tidy_style)
  }
  x
}

#' @export
#' @rdname mnis_reference
ref_photo_outputs <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "PhotoOutputs"

  mnis_reference_utils(type, tidy, tidy_style)
}

#' @export
#' @rdname mnis_reference
ref_statuses <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "Statuses"

  mnis_reference_utils(type, tidy, tidy_style)
}
#' @export
#' @rdname mnis_reference
ref_titles <- function(tidy = TRUE, tidy_style = "snake_case") {
  type <- "Titles"

  mnis_reference_utils(type, tidy, tidy_style)
}
