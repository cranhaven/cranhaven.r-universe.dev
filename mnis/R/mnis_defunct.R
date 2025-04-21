

# Extra function
#
# It combines the various options of mnis_additional into one dataframe,
# and the default is similar to [mnis_full_biog()]. Variable
# descriptions are taken from the mnis website:
# [http://data.parliament.uk/membersdataplatform/memberquery.aspx](http://data.parliament.uk/membersdataplatform/memberquery.aspx).
#
# @param ID The ID number of the member. If `NULL`, does not return any
# data. Defaults to `NULL`.
# @param ref_dods Request based on the DODS membership ID scheme.
# Defaults to `FALSE`. If `FALSE`, requests data based on the
# default membership ID scheme. Defaults to `TRUE`.
# @param addresses Member address information (e.g. website, twitter,
# consituency address etc...). If `TRUE`, address details are included
# in the tibble. Defaults to `TRUE`.
# @param biography_entries Member biographical information (e.g. countries
# of interest, policy expertise etc...) If `TRUE`, biographical
# details are included in the tibble. Defaults to `TRUE`.
# @param committees Committees a Member sits or has sat on as well
# details on committee chairing. If `TRUE`, committee details are
# included in the tibble. Defaults to `TRUE`.
# @param constituencies Constituencies a Member has represented.
# If `TRUE`, constituency details are included in the tibble.
# Defaults to `TRUE`.
# @param elections_contested Elections a Member has contested but not won.
# If `TRUE`, details of unsuccessful election contests are included
# in the tibble. Defaults to `TRUE`.
# @param experiences Non-parliamentary experience of a Member. If
# `TRUE`, extra-parliamentary experience details are included in the
# tibble. Defaults to `TRUE`.
# @param government_posts Government posts a member has held. If
# `TRUE`, government posts details are included in the tibble.
# Defaults to `TRUE`.
# @param honours Honours (e.g. MBE, OBE etc...) held by a Member.
# If `TRUE`, honours details are included in the tibble.
# Defaults to `TRUE`.
# @param house_memberships House membership list of a Member.
# If `TRUE`, house membership details are included in the tibble.
# Defaults to `TRUE`.
# @param interests Registered interests (financial) of a Member.
# If `TRUE`, interest details are included in the tibble.
# Defaults to `TRUE`.
# @param known_as Details of names a Member has chosen to be known as instead
# of their full title (House of Lords members only). If `TRUE`, known as
# details are included in the tibble. Defaults to `TRUE`.
# @param maiden_speeches Maiden speech dates for a Member. If `TRUE`,
# maiden speech details are included in the tibble. Defaults to `TRUE`.
# @param opposition_posts Opposition posts a Member has held. If `TRUE`,
# opposition post details are included in the tibble. Defaults to `TRUE`.
# @param other_parliaments Other Parliaments that a Member has held a
# membership of. If `TRUE`, details of other parliaments are included
# in the tibble. Defaults to `TRUE`.
# @param parliamentary_posts Parliamentary posts a Member has held. If
# `TRUE`, parliamentary posts details are included in the tibble.
# Defaults to `TRUE`.
# @param parties Party affiliations of a Member. If `TRUE`, party
# details are included in the tibble. Defaults to `TRUE`.
# @param preferred_names Full set of data about a Members' name (e.g.
# surname, forename, Honorary prefixes, full details of HoL title and
# rank etc...). If `TRUE`, preferred names details are included in the
# tibble. Defaults to `TRUE`.
# @param staff The staff employed by a Member. If `TRUE`, staff details
# are included in the tibble. Defaults to `TRUE`.
# @param statuses Status history (e.g. suspensions and disqualifications)
# for a Member. If `TRUE`, status details are included in the tibble.
# Defaults to `TRUE`.
# @inheritParams mnis_additional
# @return A list with named elements for all the requested data on a given MP.
# @export
# @rdname mnis_extra
# @seealso [mnis_full_biog()]
# @seealso [mnis_basic_details()]
# @seealso [mnis_additional()]
# @examples
# \dontrun{
# x <- mnis_extra(172)
# }
#
mnis_extra <- function(ID, ref_dods = FALSE, addresses = TRUE,
                       biography_entries = TRUE, committees = TRUE,
                       constituencies = TRUE, elections_contested = TRUE,
                       experiences = TRUE, government_posts = TRUE,
                       honours = TRUE, house_memberships = TRUE,
                       interests = TRUE, known_as = TRUE,
                       maiden_speeches = TRUE, opposition_posts = TRUE,
                       other_parliaments = TRUE, parliamentary_posts = TRUE,
                       parties = TRUE, preferred_names = TRUE,
                       staff = TRUE, statuses = TRUE,
                       tidy = TRUE, tidy_style = "snake_case") {

  .Defunct("mnis_additional", msg = "mnis_extra is defunct")

  ID <- as.character(ID)

  if (is.null(ID) == TRUE) {
    stop("ID cannot be null", call. = FALSE)
  }

  mnis_df <- tibble::tibble(member_id = ID)

  if (addresses == TRUE) {
    addresses_df <- mnis_addresses(ID = ID, ref_dods = ref_dods,
                                   tidy = TRUE, tidy_style = "snake_case")
    suppressMessages(
      suppressWarnings(mnis_df <- dplyr::inner_join(mnis_df, addresses_df))
      )
  }

  if (biography_entries == TRUE) {
    biography_entries_df <- mnis_biography_entries(ID = ID, ref_dods = ref_dods,
                                                   tidy = TRUE, tidy_style = "snake_case")
    suppressMessages(suppressWarnings(mnis_df <- dplyr::inner_join(mnis_df, biography_entries_df)))
  }

  if (committees == TRUE) {
    committees_df <- mnis_committees(ID = ID, ref_dods = ref_dods, tidy = TRUE, tidy_style = "snake_case")
    suppressMessages(suppressWarnings(mnis_df <- dplyr::inner_join(mnis_df, committees_df)))
  }

  if (constituencies == TRUE) {
    constituencies_df <- mnis_constituencies(ID = ID, ref_dods = ref_dods, tidy = TRUE, tidy_style = "snake_case")
    suppressMessages(suppressWarnings(mnis_df <- dplyr::inner_join(mnis_df, constituencies_df)))
  }

  if (elections_contested == TRUE) {
    elections_contested_df <- mnis_elections_contested(ID = ID, ref_dods = ref_dods, tidy = TRUE, tidy_style = "snake_case")
    suppressMessages(suppressWarnings(mnis_df <- dplyr::inner_join(mnis_df, elections_contested_df)))

    addresses_df <- NULL
    biography_entries_df <- NULL
    committees_df <- NULL
    constituencies_df <- NULL
    elections_contested_df <- NULL
    experiences_df <- NULL
    government_posts_df <- NULL
    honours_df <- NULL
    house_memberships_df <- NULL
    interests_df <- NULL
    known_as_df <- NULL
    maiden_speeches_df <- NULL
    opposition_posts_df <- NULL
    other_parliaments_df <- NULL
    parliamentary_posts_df <- NULL
    parties_df <- NULL
    preferred_names_df <- NULL
    staff_df <- NULL
    statuses_df <- NULL

    if (addresses == TRUE) {
      addresses_df <- mnis_addresses(ID, ref_dods, tidy, tidy_style)
    }

    if (biography_entries == TRUE) {
      biography_entries_df <- mnis_biography_entries(
        ID, ref_dods,
        tidy, tidy_style
      )
    }

    if (committees == TRUE) {
      committees_df <- mnis_committees(ID, ref_dods, tidy, tidy_style)
    }

    if (constituencies == TRUE) {
      constituencies_df <- mnis_constituencies(ID, ref_dods, tidy, tidy_style)
    }

    if (elections_contested == TRUE) {
      elections_contested_df <- mnis_elections_contested(
        ID, ref_dods,
        tidy, tidy_style
      )
    }

    if (experiences == TRUE) {
      experiences_df <- mnis_experiences(ID = ID, ref_dods = ref_dods, tidy = TRUE, tidy_style = "snake_case")
      suppressMessages(suppressWarnings(mnis_df <- dplyr::inner_join(mnis_df, experiences_df)))

      experiences_df <- mnis_experiences(ID, ref_dods, tidy, tidy_style)
    }

    if (government_posts == TRUE) {
      government_posts_df <- mnis_government_posts(ID = ID, ref_dods = ref_dods, tidy = TRUE, tidy_style = "snake_case")
      suppressMessages(suppressWarnings(mnis_df <- dplyr::inner_join(mnis_df, government_posts_df)))
    }

    if (honours == TRUE) {
      honours_df <- mnis_honours(ID = ID, ref_dods = ref_dods, tidy = TRUE, tidy_style = "snake_case")
      suppressMessages(suppressWarnings(mnis_df <- dplyr::inner_join(mnis_df, honours_df)))
    }

    if (house_memberships == TRUE) {
      house_memberships_df <- mnis_house_memberships(ID = ID, ref_dods = ref_dods, tidy = TRUE, tidy_style = "snake_case")
      suppressMessages(suppressWarnings(mnis_df <- dplyr::inner_join(mnis_df, house_memberships_df)))
    }

    if (interests == TRUE) {
      interests_df <- mnis_interests(ID = ID, ref_dods = ref_dods, tidy = TRUE, tidy_style = "snake_case")
      suppressMessages(suppressWarnings(mnis_df <- dplyr::inner_join(mnis_df, interests_df)))

      government_posts_df <- mnis_government_posts(ID, ref_dods, tidy, tidy_style)
    }

    if (honours == TRUE) {
      honours_df <- mnis_honours(ID, ref_dods, tidy, tidy_style)
    }

    if (house_memberships == TRUE) {
      house_memberships_df <- mnis_house_memberships(
        ID, ref_dods,
        tidy, tidy_style
      )
    }

    if (interests == TRUE) {
      interests_df <- mnis_interests(ID, ref_dods, tidy, tidy_style)
    }

    if (known_as == TRUE) {
      known_as_df <- mnis_known_as(ID = ID, ref_dods = ref_dods, tidy = TRUE, tidy_style = "snake_case")
      suppressMessages(suppressWarnings(mnis_df <- dplyr::inner_join(mnis_df, known_as_df)))

      known_as_df <- mnis_known_as(ID, ref_dods, tidy, tidy_style)
    }

    if (maiden_speeches == TRUE) {
      maiden_speeches_df <- mnis_maiden_speeches(ID = ID, ref_dods = ref_dods, tidy = TRUE, tidy_style = "snake_case")
      suppressMessages(suppressWarnings(mnis_df <- dplyr::inner_join(mnis_df, maiden_speeches_df)))

      maiden_speeches_df <- mnis_maiden_speeches(ID, ref_dods, tidy, tidy_style)
    }

    if (opposition_posts == TRUE) {
      opposition_posts_df <- mnis_opposition_posts(ID = ID, ref_dods = ref_dods, tidy = TRUE, tidy_style = "snake_case")
      suppressMessages(suppressWarnings(mnis_df <- dplyr::inner_join(mnis_df, opposition_posts_df)))

      opposition_posts_df <- mnis_opposition_posts(ID, ref_dods, tidy, tidy_style)
    }

    if (other_parliaments == TRUE) {
      other_parliaments_df <- mnis_other_parliaments(ID = ID, ref_dods = ref_dods, tidy = TRUE, tidy_style = "snake_case")
      suppressMessages(suppressWarnings(mnis_df <- dplyr::inner_join(mnis_df, other_parliaments_df)))
    }

    if (parliamentary_posts == TRUE) {
      parliamentary_posts_df <- mnis_parliamentary_posts(ID = ID, ref_dods = ref_dods, tidy = TRUE, tidy_style = "snake_case")
      suppressMessages(suppressWarnings(mnis_df <- dplyr::inner_join(mnis_df, parliamentary_posts_df)))
    }

    if (parties == TRUE) {
      parties_df <- mnis_parties(ID = ID, ref_dods = ref_dods, tidy = TRUE, tidy_style = "snake_case")
      suppressMessages(suppressWarnings(mnis_df <- dplyr::inner_join(mnis_df, parties_df)))

      other_parliaments_df <- mnis_other_parliaments(
        ID, ref_dods,
        tidy, tidy_style
      )
    }

    if (parliamentary_posts == TRUE) {
      parliamentary_posts_df <- mnis_parliamentary_posts(
        ID, ref_dods,
        tidy, tidy_style
      )
    }

    if (parties == TRUE) {
      parties_df <- mnis_parties(ID, ref_dods, tidy, tidy_style)
    }

    if (preferred_names == TRUE) {
      preferred_names_df <- mnis_preferred_names(ID = ID, ref_dods = ref_dods, tidy = TRUE, tidy_style = "snake_case")
      suppressMessages(suppressWarnings(mnis_df <- dplyr::inner_join(mnis_df, preferred_names_df)))

      preferred_names_df <- mnis_preferred_names(ID, ref_dods, tidy, tidy_style)
    }

    if (staff == TRUE) {
      staff_df <- mnis_staff(ID = ID, ref_dods = ref_dods, tidy = TRUE, tidy_style = "snake_case")
      suppressMessages(suppressWarnings(mnis_df <- dplyr::inner_join(mnis_df, staff_df)))

      staff_df <- mnis_staff(ID, ref_dods, tidy, tidy_style)
    }

    if (statuses == TRUE) {
      statuses_df <- mnis_statuses(ID = ID, ref_dods = ref_dods, tidy = TRUE, tidy_style = "snake_case")
      suppressMessages(suppressWarnings(mnis_df <- dplyr::inner_join(mnis_df, statuses_df)))
    }

    if (tidy == TRUE) {
      mnis_df <- mnis::mnis_tidy(mnis_df, tidy_style)

      statuses_df <- mnis_statuses(ID, ref_dods, tidy, tidy_style)
    }


    mnis_df <- list(
      addresses_df,
      biography_entries_df,
      committees_df,
      constituencies_df,
      elections_contested_df,
      experiences_df,
      government_posts_df,
      honours_df,
      house_memberships_df,
      interests_df,
      known_as_df,
      maiden_speeches_df,
      opposition_posts_df,
      other_parliaments_df,
      parliamentary_posts_df,
      parties_df,
      preferred_names_df,
      staff_df,
      statuses_df
    )

    names(mnis_df) <- c(
      "addresses",
      "biography_entries",
      "committees",
      "constituencies",
      "elections_contested",
      "experiences",
      "government_posts",
      "honours",
      "house_memberships",
      "interests",
      "known_as",
      "maiden_speeches",
      "opposition_posts",
      "other_parliaments",
      "parliamentary_posts",
      "parties",
      "preferred_names",
      "staff",
      "statuses"
    )

    mnis_df
  } else {
    mnis_df
  }
}
