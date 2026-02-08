.Dictionary__initialize <- function(self, private, x, types) { # nolint
  if (length(x)) {
    private$.items <- list2env(
       assert_named_list(x, types = types),
       parent = emptyenv(),
       hash = TRUE
    )
  }

  private$.types <- types
  invisible(self)
}

.Dictionary__add <- function(self, private, x, keys, values) { # nolint
   if (!length(x)) {
      if (is.null(keys) || is.null(values)) {
         stop("Either a named list or 'keys' and 'values' must be provided.") # nolint
      }
      if (!is.R6Object(values) && length(values) > 1) {
         values <- as.list(values)
      } else if (!is.list(values)) {
         values <- list(values)
      }
      x <- setNames(as.list(values), keys)
   }

   # signif quicker than first concatenating and then checking type
   if (any(self$has(ls(x)))) {
      stop("Some or all keys of new values already exist in the dictionary")
   }

   assert_named_list(x, types = private$.types)

   if (is.list(x)) {
      private$.items <- list2env(x, parent = private$.items, hash = TRUE)
   } else {
      parent.env(x) <- private$.items
      private$.items <- x
   }

   invisible(self)
}

.Dictionary__remove <- function(self, private, x) { # nolint
   lapply(x, rm_env, private$.items)
   invisible(self)
}

.Dictionary__assert_contains <- function(self, private, keys) { # nolint
   if (all(self$has(keys))) {
      invisible(keys)
   } else {
      stop("Not all keys in self$keys")
   }
}

.Dictionary__get <- function(self, private, x, clone = TRUE) { # nolint
   if (!(length(private$.types) == 1 || length(x) == 1)) {
      stop("'get' can only be used if length of 'x' is '1' or if Dictionary has one type.") # nolint
   }

   x <- unlist(lapply(
      mget(x, private$.items, inherits = TRUE),
      function(.x) {
         if (is.R6Object(.x) && clone) {
            .x$clone(deep = TRUE)
         } else {
            .x
         }
      }
   ))

   ## catch not unlisting R6
   if (length(x) == 1) {
      x <- x[[1]]
   }

   if (!is.list(x)) {
      x <- unname(x)
   }

   x
}

.Dictionary__get_list <- function(self, private, x, clone = TRUE) { # nolint
   lapply(
      mget(x, private$.items, inherits = TRUE),
      function(.x) {
         if (is.R6Object(.x) && clone) {
            .x$clone(deep = TRUE)
         } else {
            .x
         }
      })
}

.Dictionary__has <- function(self, private, x) { # nolint
   !is.na(mget(x, private$.items, ifnotfound = NA, inherits = TRUE))
}

.Dictionary__has_value <- function(self, private, x) { # nolint
   x %in% self$values
}

.Dictionary__print <- function(self, private, n) { # nolint
   cat(as.character(self, n = n), "\n")
}

.Dictionary__summary <- function(self, private, n) { # nolint
   if (self$typed) {
      cat(sprintf("Typed dictionary of %d items and types: %s.\n",
                  self$length, string_as_set(self$types)))
   } else {
      cat(sprintf("Untyped dictionary of %d items.\n", self$length))
   }
   cat(as.character(self, n = n), "\n")
}

.Dictionary__rekey <- function(self, private, key, new_key) { # nolint
   stopifnot(length(key) == 1)
   self$assert_contains(key)

   if (self$has(new_key)) {
     stop("'new_key' already exists in self$keys")
   }

   rename_env_item(key, new_key, private$.items)
   invisible(self)
}

.Dictionary__revalue <- function(self, private, key, new_value) { # nolint
   stopifnot(length(key) == 1)
   self$assert_contains(key)
   if (self$typed) {
      stopifnot(inherits(key, self$types))
   }

   update_env_value(key, new_value, private$.items)
   invisible(self)
}

.Dictionary__merge <- function(self, private, x) { # nolint

   if (inherits(x, "Dictionary")) {
      self$add(private(x)$.items)
   } else if (is.list(x)) {
      lapply(x, function(.x) self$add(private(.x)$.items))
   } else {
      stop("'x' should either be a Dictionary or list of Dictionaries.")
   }
   invisible(self)
}

.Dictionary__values <- function(self, private) { # nolint
   if (length(private$.types == 1)) {
      unname(unlist(self$items))
   } else {
      unname(self$items)
   }
}

.Dictionary__items <- function(self, private, x) { # nolint
   if (missing(x)) {
      as_list_env(private$.items)
   } else {
      if (length(x)) {
         private$.items <- list2env(
            assert_named_list(x, private$.types),
            parent = emptyenv(),
            hash = TRUE
         )
      } else {
         private$.items <- emptyenv()
      }
     invisible(self)
   }
}
