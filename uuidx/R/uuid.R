.uuid_namespace_aliases = c(
    dns = "6ba7b810-9dad-11d1-80b4-00c04fd430c8",
    url = "6ba7b811-9dad-11d1-80b4-00c04fd430c8",
    oid = "6ba7b812-9dad-11d1-80b4-00c04fd430c8",
    x500 = "6ba7b814-9dad-11d1-80b4-00c04fd430c8"
)

.uuid_check_n = function(
    n
) {
    if (length(n) != 1L || is.na(n) || !is.numeric(n) || !is.finite(n)) {
        stop("`n` must be a single non-negative integer.", call. = FALSE)
    }

    if (n < 0 || n != as.integer(n)) {
        stop("`n` must be a single non-negative integer.", call. = FALSE)
    }

    as.integer(n)
}

.uuid_empty_fields = function(
) {
    structure(
        data.frame(
            canonical = character(),
            version = integer(),
            variant = character(),
            time_low = character(),
            time_mid = character(),
            time_hi_and_version = character(),
            clock_seq_hi_and_reserved = character(),
            clock_seq_low = character(),
            node = character(),
            stringsAsFactors = FALSE
        ),
        class = c("uuid_fields", "data.frame")
    )
}

.uuid_as_fields = function(
    x
) {
    out = as.data.frame(x, stringsAsFactors = FALSE, optional = TRUE)
    structure(out, class = c("uuid_fields", "data.frame"))
}

.uuid_normalize_name = function(
    name
) {
    name = as.character(name)

    if (!length(name)) {
        stop("`name` must contain at least one value.", call. = FALSE)
    }

    if (anyNA(name)) {
        stop("`name` must not contain missing values.", call. = FALSE)
    }

    name
}

.uuid_normalize_namespace = function(
    namespace
) {
    namespace = as.character(namespace)

    if (!length(namespace)) {
        stop("`namespace` must contain at least one value.", call. = FALSE)
    }

    if (anyNA(namespace)) {
        stop("`namespace` must not contain missing values.", call. = FALSE)
    }

    lower = tolower(namespace)
    is_alias = lower %in% names(.uuid_namespace_aliases)

    if (any(is_alias)) {
        namespace = replace(
            namespace,
            is_alias,
            unname(.uuid_namespace_aliases[lower[is_alias]])
        )
    }

    if (!all(uuid_validate(namespace))) {
        stop(
            "`namespace` must be a UUID string or one of \"dns\", \"url\", \"oid\", or \"x500\".",
            call. = FALSE
        )
    }

    namespace
}

.uuid_recycle_pair = function(
    namespace,
    name
) {
    size = max(length(namespace), length(name))

    if (!length(namespace) %in% c(1L, size) || !length(name) %in% c(1L, size)) {
        stop("`namespace` and `name` must have length 1 or a common length.", call. = FALSE)
    }

    list(
        namespace = rep_len(namespace, size),
        name = rep_len(name, size)
    )
}

#' Generate UUIDs
#'
#' `uuid_generate()` is the main entry point for new UUID creation. It defaults
#' to version 7, which is typically a practical default for new identifiers
#' because it keeps the randomness users expect while sorting naturally by time.
#'
#' Version 5 is name-based and deterministic, so it uses `namespace` and
#' `name` instead of `n`.
#'
#' @param n Number of UUIDs to generate. Must be a single non-negative integer.
#' @param version UUID version to generate. Defaults to `"v7"`.
#' @param namespace Namespace UUID used for version 5 generation. Standard
#'   aliases `"dns"`, `"url"`, `"oid"`, and `"x500"` are accepted.
#' @param name Name value or values used for version 5 generation.
#' @param output Output format. `"string"` returns canonical UUID strings and
#'   `"raw"` returns a list of raw vectors, each of length 16.
#'
#' @return A character vector for `output = "string"` or a list of raw vectors
#'   for `output = "raw"`.
#' @export
#'
#' @examples
#' uuid_generate()
#' uuid_generate(3, version = "v4")
#' uuid_generate(version = "v5", namespace = "dns", name = c("a", "b"))
uuid_generate = function(
    n = 1L,
    version = c("v7", "v4", "v5", "v6", "v1"),
    namespace = NULL,
    name = NULL,
    output = c("string", "raw")
) {
    version = match.arg(version)
    output = match.arg(output)

    if (version == "v5") {
        if (missing(namespace) || is.null(namespace) || missing(name) || is.null(name)) {
            stop(
                "`namespace` and `name` must both be supplied when `version = \"v5\"`.",
                call. = FALSE
            )
        }

        if (.uuid_check_n(n) != 1L) {
            stop("`n` is only used for versions 1, 4, 6, and 7.", call. = FALSE)
        }

        return(uuid_v5(namespace = namespace, name = name, output = output))
    }

    if (!is.null(namespace) || !is.null(name)) {
        stop("`namespace` and `name` are only used when `version = \"v5\"`.", call. = FALSE)
    }

    switch(
        version,
        v7 = uuid_v7(n = n, output = output),
        v4 = uuid_v4(n = n, output = output),
        v6 = uuid_v6(n = n, output = output),
        v1 = uuid_v1(n = n, output = output)
    )
}

#' Generate Version 7 UUIDs
#'
#' Version 7 UUIDs are time-ordered and are the default in `uuidx`.
#'
#' @inheritParams uuid_generate
#' @return A character vector or a list of 16-byte raw vectors.
#' @export
uuid_v7 = function(
    n = 1L,
    output = c("string", "raw")
) {
    n = .uuid_check_n(n)
    output = match.arg(output)
    uuid_rust_v7(n, identical(output, "raw"))
}

#' Generate Version 4 UUIDs
#'
#' Version 4 UUIDs are fully random identifiers.
#'
#' @inheritParams uuid_generate
#' @return A character vector or a list of 16-byte raw vectors.
#' @export
uuid_v4 = function(
    n = 1L,
    output = c("string", "raw")
) {
    n = .uuid_check_n(n)
    output = match.arg(output)
    uuid_rust_v4(n, identical(output, "raw"))
}

#' Generate Version 6 UUIDs
#'
#' Version 6 UUIDs retain the timestamp-based lineage of version 1 while being
#' more naturally sortable.
#'
#' @inheritParams uuid_generate
#' @return A character vector or a list of 16-byte raw vectors.
#' @export
uuid_v6 = function(
    n = 1L,
    output = c("string", "raw")
) {
    n = .uuid_check_n(n)
    output = match.arg(output)
    uuid_rust_v6(n, identical(output, "raw"))
}

#' Generate Legacy Version 1 UUIDs
#'
#' Version 1 remains available for compatibility work, but it is treated as a
#' legacy option in this package.
#'
#' @inheritParams uuid_generate
#' @return A character vector or a list of 16-byte raw vectors.
#' @export
uuid_v1 = function(
    n = 1L,
    output = c("string", "raw")
) {
    n = .uuid_check_n(n)
    output = match.arg(output)
    uuid_rust_v1(n, identical(output, "raw"))
}

#' Generate Deterministic Version 5 UUIDs
#'
#' Version 5 UUIDs are derived from a namespace UUID and a name using SHA-1.
#'
#' @param namespace Namespace UUID or one of `"dns"`, `"url"`, `"oid"`, or
#'   `"x500"`.
#' @param name One or more name strings.
#' @param output Output format. `"string"` returns canonical UUID strings and
#'   `"raw"` returns a list of raw vectors, each of length 16.
#'
#' @return A character vector or a list of 16-byte raw vectors.
#' @export
#'
#' @examples
#' uuid_v5("dns", "example.com")
#' uuid_v5("dns", c("alpha", "beta"))
uuid_v5 = function(
    namespace,
    name,
    output = c("string", "raw")
) {
    output = match.arg(output)
    namespace = .uuid_normalize_namespace(namespace)
    name = .uuid_normalize_name(name)
    recycled = .uuid_recycle_pair(namespace, name)

    uuid_rust_v5(
        recycled$namespace,
        recycled$name,
        identical(output, "raw")
    )
}

#' Parse UUIDs
#'
#' `uuid_parse()` canonicalizes UUID strings and can optionally expose their raw
#' bytes or field-level structure.
#'
#' For `output = "fields"`, the returned data frame exposes structural UUID
#' fields. These field names follow the conventional UUID layout, although the
#' semantics of those fields vary by UUID version.
#'
#' @param x Character vector of UUID strings.
#' @param output Output format. `"string"` returns canonical strings, `"raw"`
#'   returns a list of 16-byte raw vectors, and `"fields"` returns a data frame
#'   with class `"uuid_fields"`.
#'
#' @return A character vector, a list of raw vectors, or a `"uuid_fields"`
#'   data frame depending on `output`.
#' @export
#'
#' @examples
#' x = uuid_v7(2)
#' uuid_parse(x)
#' uuid_parse(x, output = "raw")
#' uuid_parse(x, output = "fields")
uuid_parse = function(
    x,
    output = c("string", "raw", "fields")
) {
    output = match.arg(output)
    x = as.character(x)

    if (!length(x)) {
        return(
            switch(
                output,
                string = character(),
                raw = list(),
                fields = .uuid_empty_fields()
            )
        )
    }

    if (anyNA(x)) {
        stop("`x` must not contain missing values.", call. = FALSE)
    }

    invalid = !uuid_rust_validate(x)

    if (any(invalid)) {
        invalid_count = sum(invalid)
        positions = paste(which(invalid), collapse = ", ")
        value_suffix = if (invalid_count == 1L) "" else "s"
        position_suffix = if (invalid_count == 1L) "" else "s"

        stop(
            sprintf(
                "`x` contains invalid UUID value%s at position%s %s.",
                value_suffix,
                position_suffix,
                positions
            ),
            call. = FALSE
        )
    }

    switch(
        output,
        string = uuid_rust_parse_string(x),
        raw = uuid_rust_parse_raw(x),
        fields = .uuid_as_fields(uuid_rust_parse_fields(x))
    )
}

#' Validate UUID Strings
#'
#' @param x Vector to validate.
#' @return A logical vector where valid UUIDs are `TRUE`.
#' @export
#'
#' @examples
#' uuid_validate(c(uuid_v7(), "not-a-uuid"))
uuid_validate = function(
    x
) {
    x = as.character(x)
    out = rep(FALSE, length(x))
    ok = !is.na(x)

    if (any(ok)) {
        out = replace(out, ok, uuid_rust_validate(x[ok]))
    }

    out
}

#' Detect UUID Versions
#'
#' @param x Vector of UUID strings.
#' @return An integer vector containing UUID version numbers, with `NA` for
#'   invalid inputs.
#' @export
#'
#' @examples
#' x = c(uuid_v7(), uuid_v4(), "not-a-uuid")
#' uuid_version(x)
uuid_version = function(
    x
) {
    x = as.character(x)
    out = rep(NA_integer_, length(x))
    ok = !is.na(x)

    if (any(ok)) {
        out = replace(out, ok, uuid_rust_version(x[ok]))
    }

    out
}

#' Return the Nil UUID
#'
#' @param output Output format. `"string"` returns the canonical UUID string and
#'   `"raw"` returns a raw vector of length 16.
#' @return A length-one character vector or a raw vector.
#' @export
#'
#' @examples
#' uuid_nil()
#' uuid_nil(output = "raw")
uuid_nil = function(
    output = c("string", "raw")
) {
    output = match.arg(output)
    uuid_rust_nil(identical(output, "raw"))
}

#' Return the Max UUID
#'
#' @param output Output format. `"string"` returns the canonical UUID string and
#'   `"raw"` returns a raw vector of length 16.
#' @return A length-one character vector or a raw vector.
#' @export
#'
#' @examples
#' uuid_max()
#' uuid_max(output = "raw")
uuid_max = function(
    output = c("string", "raw")
) {
    output = match.arg(output)
    uuid_rust_max(identical(output, "raw"))
}
