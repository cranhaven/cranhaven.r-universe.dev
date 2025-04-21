## Deprecated functions

mnis_additional <- function() {
  .Deprecated("mnis_additional", msg = "mnis_additional is defunct")

  x <- c("mnis_full_biog()", "mnis_basic_details()", "mnis_biography_entries()",
         "mnis_committees()", "mnis_addresses()", "mnis_constituencies()",
         "mnis_elections_contested()", "mnis_experiences()",
         "mnis_government_posts()", "mnis_honours()",
         "mnis_house_memberships()", "mnis_statuses()", "mnis_staff()",
         "mnis_interests()", "mnis_known_as()", "mnis_maiden_speeches()",
         "mnis_opposition_posts()", "mnis_other_parliaments()",
         "mnis_parliamentary_posts()", "mnis_parties()",
         "mnis_preferred_names()")
  message("All Available Additional Information Functions:")

  print(x)
}



mnis_reference <- function() {
  .Deprecated("mnis_reference", msg = "mnis_reference is defunct")

  message("All Available Reference Functions:")

  print(c("ref_address_types()", "ref_answering_bodies()", "ref_areas()", "ref_area_types()", "ref_biography_categories()", "ref_cabinets()", "ref_committees()", "ref_committee_types()", "ref_constituencies()", "ref_constituency_areas()", "ref_constituency_types()", "ref_countries()", "ref_departments()", "ref_disqualification_types()", "ref_elections()", "ref_election_types()", "ref_end_reasons()", "ref_experience_types()", "ref_government_post_departments()", "ref_government_posts()", "ref_government_ranks()", "ref_honourary_prefixes()", "ref_honour_lists()", "ref_honours()", "ref_interest_categories()", "ref_lords_membership_types()", "ref_lords_ranks()", "ref_opposition_post_departments()", "ref_opposition_posts()", "ref_opposition_ranks()", "ref_other_parliaments()", "ref_parliamentary_posts()", "ref_parliamentary_ranks()", "ref_parliament_types()", "ref_parties()", "ref_party_sub_types()", "ref_photo_outputs()", "ref_statuses()", "ref_titles()"))
}
