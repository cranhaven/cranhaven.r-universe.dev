# calculate_mme_local() is deprecated

    Code
      calculate_mme_local(10, 5, meds_list)
    Condition
      Warning:
      `calculate_mme_local()` was deprecated in mmequiv 1.0.0.
      i Please use `calculate_mme(use_api = FALSE)` instead
    Output
      $message
      [1] "Processed medications (local calculation)"
      
      $therapy_obs_window_with
      $therapy_obs_window_with$therapy_days
      [1] 10
      
      $therapy_obs_window_with$observation_window_days
      [1] 5
      
      
      $therapy_obs_window_without
      $therapy_obs_window_without$therapy_days
      [1] 10
      
      $therapy_obs_window_without$observation_window_days
      [1] 5
      
      
      $medications
                               medication_name dose doses_per_24_hours
      1 Buprenorphine buccal film (mcg) buccal   50                  2
      2                       Hydrocodone (mg)   75                  3
        days_of_medication factor    mme single_day_mme
      1                  5  0.039   19.5            3.9
      2                 10  1.000 2250.0          225.0
      
      $mme_definitions
      $mme_definitions$with_buprenorphine
        total_mme total_days  mme1   mme2  mme3  mme4
      1    2269.5         15 151.3 226.95 453.9 228.9
      
      $mme_definitions$without_buprenorphine
        total_mme total_days mme1 mme2 mme3 mme4
      1      2250         10  225  225  450  225
      
      

# Provides error messages for invalid therapy_days and observation_window_days arguments

    Code
      calculate_mme_local("invalid", 5, meds_list)
    Condition
      Error in `calculate_mme_local()`:
      ! The `therapy_days` argument must be either a single positive number or a vector of 2 positive numbers
      i Use a vector of 2 numbers to set different values for MME calculations 'with buprenorphine' and 'without buprenorphine'
      i Use a single number to apply the same value to both scenarios

---

    Code
      calculate_mme_local(0, 5, meds_list)
    Condition
      Error in `calculate_mme_local()`:
      ! The `therapy_days` argument must be a positive number

---

    Code
      calculate_mme_local(-10, 5, meds_list)
    Condition
      Error in `calculate_mme_local()`:
      ! The `therapy_days` argument must be a positive number

---

    Code
      calculate_mme_local(10, "invalid", meds_list)
    Condition
      Error in `calculate_mme_local()`:
      ! The `observation_window_days` argument must be either a single positive number or a vector of 2 positive numbers
      i Use a vector of 2 numbers to set different values for MME calculations 'with buprenorphine' and 'without buprenorphine'
      i Use a single number to apply the same value to both scenarios

---

    Code
      calculate_mme_local(10, 0, meds_list)
    Condition
      Error in `calculate_mme_local()`:
      ! The `observation_window_days` argument must be a positive number

---

    Code
      calculate_mme_local(10, -5, meds_list)
    Condition
      Error in `calculate_mme_local()`:
      ! The `observation_window_days` argument must be a positive number

---

    Code
      calculate_mme_local(c(10, 0), 5, meds_list)
    Condition
      Error in `calculate_mme_local()`:
      ! All values in `therapy_days` must be positive numbers

---

    Code
      calculate_mme_local(c(10, -5), 5, meds_list)
    Condition
      Error in `calculate_mme_local()`:
      ! All values in `therapy_days` must be positive numbers

---

    Code
      calculate_mme_local(c(0, 10), 5, meds_list)
    Condition
      Error in `calculate_mme_local()`:
      ! All values in `therapy_days` must be positive numbers

---

    Code
      calculate_mme_local(c(-1, 10), 5, meds_list)
    Condition
      Error in `calculate_mme_local()`:
      ! All values in `therapy_days` must be positive numbers

---

    Code
      calculate_mme_local(10, c(5, 0), meds_list)
    Condition
      Error in `calculate_mme_local()`:
      ! All values in `observation_window_days` must be positive numbers

---

    Code
      calculate_mme_local(10, c(0, 5), meds_list)
    Condition
      Error in `calculate_mme_local()`:
      ! All values in `observation_window_days` must be positive numbers

---

    Code
      calculate_mme_local(10, c(-5, 5), meds_list)
    Condition
      Error in `calculate_mme_local()`:
      ! All values in `observation_window_days` must be positive numbers

---

    Code
      calculate_mme_local(c(10, 20, 30), 5, meds_list)
    Condition
      Error in `calculate_mme_local()`:
      ! The `therapy_days` argument must be either a single positive number or a vector of 2 positive numbers
      i Use a vector of 2 numbers to set different values for MME calculations 'with buprenorphine' and 'without buprenorphine'
      i Use a single number to apply the same value to both scenarios

---

    Code
      calculate_mme_local(10, c(5, 10, 15), meds_list)
    Condition
      Error in `calculate_mme_local()`:
      ! The `observation_window_days` argument must be either a single positive number or a vector of 2 positive numbers
      i Use a vector of 2 numbers to set different values for MME calculations 'with buprenorphine' and 'without buprenorphine'
      i Use a single number to apply the same value to both scenarios

# Provides error messages for invalid medications argument

    Code
      calculate_mme_local(10, 5, "not_a_list")
    Condition
      Error in `calculate_mme_local()`:
      ! The `medications` argument must be a <list>
      x You've supplied a <character>

---

    Code
      calculate_mme_local(10, 5, list())
    Condition
      Error in `calculate_mme_local()`:
      ! The `medications` argument must be a non-empty <list>

---

    Code
      calculate_mme_local(10, 5, list("not_a_medication_list"))
    Condition
      Error in `calculate_mme_local()`:
      ! Element 1 in `medications` must be a <list>
      x You've supplied a <character>

---

    Code
      calculate_mme_local(10, 5, bad_meds_missing_field)
    Condition
      Error in `calculate_mme_local()`:
      ! Element 1 in `medications` is missing required fields: medication_name

---

    Code
      calculate_mme_local(10, 5, bad_meds_name)
    Condition
      Error in `calculate_mme_local()`:
      ! Element 1 in `medications`: medication_name must be a single <character> string

---

    Code
      calculate_mme_local(10, 5, bad_meds_dose)
    Condition
      Error in `calculate_mme_local()`:
      ! Element 1 in `medications`: medication_name "Test Med" is not a medication name accepted by the API
      i Run `get_med_list()` to see the list of medication_names accepted by the API

---

    Code
      calculate_mme_local(10, 5, bad_meds_dose_negative)
    Condition
      Error in `calculate_mme_local()`:
      ! Element 1 in `medications`: medication_name "Test Med" is not a medication name accepted by the API
      i Run `get_med_list()` to see the list of medication_names accepted by the API

---

    Code
      calculate_mme_local(10, 5, bad_meds_doses_per_day)
    Condition
      Error in `calculate_mme_local()`:
      ! Element 1 in `medications`: medication_name "Test Med" is not a medication name accepted by the API
      i Run `get_med_list()` to see the list of medication_names accepted by the API

---

    Code
      calculate_mme_local(10, 5, bad_meds_days)
    Condition
      Error in `calculate_mme_local()`:
      ! Element 1 in `medications`: medication_name "Test Med" is not a medication name accepted by the API
      i Run `get_med_list()` to see the list of medication_names accepted by the API

# Validates medication names against known list

    Code
      calculate_mme_local(10, 5, invalid_medication)
    Condition
      Error in `calculate_mme_local()`:
      ! Element 1 in `medications`: medication_name "Not A Real Medication Name" is not a medication name accepted by the API
      i Run `get_med_list()` to see the list of medication_names accepted by the API

