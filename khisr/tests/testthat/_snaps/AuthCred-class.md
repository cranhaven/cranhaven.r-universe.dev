# inputs are checked when creating AuthCred

    Code
      init_AuthCred(username = c("home", "weed"), password = NULL, base_url = NULL)
    Condition
      Error in `initialize()`:
      ! is.null(username) || is_scalar_character(username) is not TRUE

---

    Code
      init_AuthCred(username = 123, password = NULL, base_url = NULL)
    Condition
      Error in `initialize()`:
      ! is.null(username) || is_scalar_character(username) is not TRUE

---

    Code
      init_AuthCred(username = NULL, password = c("123", "abc"), base_url = NULL)
    Condition
      Error in `initialize()`:
      ! is.null(password) || is_scalar_character(password) is not TRUE

---

    Code
      init_AuthCred(username = NULL, password = 123, base_url = NULL)
    Condition
      Error in `initialize()`:
      ! is.null(password) || is_scalar_character(password) is not TRUE

---

    Code
      init_AuthCred(username = NULL, password = NULL, base_url = c("home", "weed"))
    Condition
      Error in `initialize()`:
      ! is.null(base_url) || is_scalar_character(base_url) is not TRUE

---

    Code
      init_AuthCred(username = NULL, password = NULL, base_url = 123)
    Condition
      Error in `initialize()`:
      ! is.null(base_url) || is_scalar_character(base_url) is not TRUE

