# Provides correct error messages for invalid `med_name`

    Code
      search_meds(med_name = NULL)
    Condition
      Error in `search_meds()`:
      ! `med_name` must be specified to use `search_meds()`

---

    Code
      search_meds(med_name = NULL)
    Condition
      Error in `search_meds()`:
      ! `med_name` must be specified to use `search_meds()`

---

    Code
      search_meds(med_name = 1)
    Condition
      Error in `search_meds()`:
      ! `med_name` must be a <character> string
      x You've supplied a <numeric> input

---

    Code
      search_meds(med_name = c("fent", "tram"))
    Condition
      Error in `search_meds()`:
      ! `med_name` only accepts a single string at a time

