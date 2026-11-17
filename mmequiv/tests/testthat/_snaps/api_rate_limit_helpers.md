# API rate limit warning is displayed correctly

    Code
      check_unique_pat(n_patients = 60, use_api = TRUE)
    Message
      ! You are using the API with 60 unique patient IDs
      x The NIH HEAL MME Online Calculator API has a rate limit of 50 patient-level
      requests per 15 minutes, so expect this to take some time!
      i Consider setting `use_api = FALSE` to perform calculations locally instead
      i Local calculations have no rate limit and don't require internet access

---

    Code
      check_unique_pat(n_patients = 40, use_api = TRUE)

---

    Code
      check_unique_pat(n_patients = 60, use_api = FALSE)

# API usage advisory is displayed correctly for <50 patients

    Code
      advise_api_usage(n_patients = 30, use_api = TRUE)
    Message
      i You are using the API with 30 unique patient IDs
      i The NIH HEAL MME Online Calculator API has a rate limit of 50 patient-level
      requests per 15 minutes
      i If you've recently made other API requests, you may still hit the rate limit
      i Consider setting `use_api = FALSE` if you experience rate limit issues

---

    Code
      advise_api_usage(n_patients = 30, use_api = FALSE)

