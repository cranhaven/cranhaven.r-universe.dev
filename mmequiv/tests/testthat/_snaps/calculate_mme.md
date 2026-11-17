# Provides error messages for invalid therapy_days and observation_window_days arguments

    Code
      calculate_mme(meds_list, "invalid", 5)
    Condition
      Error in `calculate_mme()`:
      ! The `therapy_days` argument must be either a single positive number or a vector of 2 positive numbers
      i Use a vector of 2 numbers to set different values for MME calculations 'with buprenorphine' and 'without buprenorphine'
      i Use a single number to apply the same value to both scenarios

---

    Code
      calculate_mme(meds_list, 0, 5)
    Condition
      Error in `calculate_mme()`:
      ! The `therapy_days` argument must be a positive number

---

    Code
      calculate_mme(meds_list, -10, 5)
    Condition
      Error in `calculate_mme()`:
      ! The `therapy_days` argument must be a positive number

---

    Code
      calculate_mme(meds_list, 10, "invalid")
    Condition
      Error in `calculate_mme()`:
      ! The `observation_window_days` argument must be either a single positive number or a vector of 2 positive numbers
      i Use a vector of 2 numbers to set different values for MME calculations 'with buprenorphine' and 'without buprenorphine'
      i Use a single number to apply the same value to both scenarios

---

    Code
      calculate_mme(meds_list, 10, 0)
    Condition
      Error in `calculate_mme()`:
      ! The `observation_window_days` argument must be a positive number

---

    Code
      calculate_mme(meds_list, 10, -5)
    Condition
      Error in `calculate_mme()`:
      ! The `observation_window_days` argument must be a positive number

---

    Code
      calculate_mme(meds_list, c(10, 0), 5)
    Condition
      Error in `calculate_mme()`:
      ! All values in `therapy_days` must be positive numbers

---

    Code
      calculate_mme(meds_list, c(10, -5), 5)
    Condition
      Error in `calculate_mme()`:
      ! All values in `therapy_days` must be positive numbers

---

    Code
      calculate_mme(meds_list, c(0, 10), 5)
    Condition
      Error in `calculate_mme()`:
      ! All values in `therapy_days` must be positive numbers

---

    Code
      calculate_mme(meds_list, c(-1, 10), 5)
    Condition
      Error in `calculate_mme()`:
      ! All values in `therapy_days` must be positive numbers

---

    Code
      calculate_mme(meds_list, 10, c(5, 0))
    Condition
      Error in `calculate_mme()`:
      ! All values in `observation_window_days` must be positive numbers

---

    Code
      calculate_mme(meds_list, 10, c(0, 5))
    Condition
      Error in `calculate_mme()`:
      ! All values in `observation_window_days` must be positive numbers

---

    Code
      calculate_mme(meds_list, 10, c(-5, 5))
    Condition
      Error in `calculate_mme()`:
      ! All values in `observation_window_days` must be positive numbers

---

    Code
      calculate_mme(meds_list, c(10, 20, 30), 5)
    Condition
      Error in `calculate_mme()`:
      ! The `therapy_days` argument must be either a single positive number or a vector of 2 positive numbers
      i Use a vector of 2 numbers to set different values for MME calculations 'with buprenorphine' and 'without buprenorphine'
      i Use a single number to apply the same value to both scenarios

---

    Code
      calculate_mme(meds_list, 10, c(5, 10, 15))
    Condition
      Error in `calculate_mme()`:
      ! The `observation_window_days` argument must be either a single positive number or a vector of 2 positive numbers
      i Use a vector of 2 numbers to set different values for MME calculations 'with buprenorphine' and 'without buprenorphine'
      i Use a single number to apply the same value to both scenarios

# Provides error messages for invalid medications argument

    Code
      calculate_mme("not_a_list", 10, 5)
    Condition
      Error in `calculate_mme()`:
      ! The `x` argument must be a <list>, <data.frame>, or <tbl_df>
      x You've specified a <character>

---

    Code
      calculate_mme(list(), 10, 5)
    Condition
      Error in `calculate_mme()`:
      ! When the `x` argument is a <list>, it must have at least one nested <list> element
      i Each nested <list> element must have the following fields: medication_name, dose, doses_per_24_hours, and days_of_medication

---

    Code
      calculate_mme(list("not_a_medication_list"), 10, 5)
    Condition
      Error in `calculate_mme()`:
      ! Element 1 in `x` must be a <list>
      x You've supplied a <character>

---

    Code
      calculate_mme(bad_meds_missing_field, 10, 5)
    Condition
      Error in `calculate_mme()`:
      ! Element 1 in `x` is missing required fields: medication_name

---

    Code
      calculate_mme(bad_meds_name, 10, 5)
    Condition
      Error in `calculate_mme()`:
      ! Element 1 in `x`: medication_name must be a single <character> string

---

    Code
      calculate_mme(bad_meds_name2, 10, 5)
    Condition
      Error in `calculate_mme()`:
      ! Element 1 in `x`: medication_name "Test Med" is not a medication name accepted by the API
      i Run `get_med_list()` to see the list of medication_names accepted by the API

---

    Code
      calculate_mme(bad_meds_dose, 10, 5)
    Condition
      Error in `calculate_mme()`:
      ! Element 1 in `x`: dose must be a positive number

---

    Code
      calculate_mme(bad_meds_dose_negative, 10, 5)
    Condition
      Error in `calculate_mme()`:
      ! Element 1 in `x`: dose must be a positive number

---

    Code
      calculate_mme(bad_meds_doses_per_day, 10, 5)
    Condition
      Error in `calculate_mme()`:
      ! Element 1 in `x`: doses_per_24_hours must be a positive number

---

    Code
      calculate_mme(bad_meds_days, 10, 5)
    Condition
      Error in `calculate_mme()`:
      ! Element 1 in `x`: days_of_medication must be a positive number

# Validates medication names against known list

    Code
      calculate_mme(invalid_medication, 10, 5)
    Condition
      Error in `calculate_mme()`:
      ! Element 1 in `x`: medication_name "Not A Real Medication Name" is not a medication name accepted by the API
      i Run `get_med_list()` to see the list of medication_names accepted by the API

