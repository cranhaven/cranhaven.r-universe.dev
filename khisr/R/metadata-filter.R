#' Metadata Filter
#'
#' Formats a metadata filter to DHIS 2 comparison operators.
#'
#' @details
#' To filter the metadata there are several filter operations that can be applied
#' to the returned list of metadata. The format of the filter itself is
#' straight-forward and follows the pattern property:operator:value, where
#' property is the property on the metadata you want to filter on, operator is
#' the comparison operator you want to perform and value is the value to check
#' against (not all operators require value). To view the operator see
#' [DHIS 2 Operator](https://docs.dhis2.org/en/develop/using-the-api/dhis-core-version-master/metadata.html#webapi_metadata_object_filter)
#'
#' * `%.eq%`        - Equality
#' * `%.ieq%`       - Case insensitive string, match exact
#' * `%.~eq%`       - Inequality
#' * `%.ne%`        - Inequality
#' * `%.Like%`      - Case sensitive string, match anywhere
#' * `%.~Like%`     - Case sensitive string, not match anywhere
#' * `%.^Like%`     - Case sensitive string, match start
#' * `%.~^Like%`    - Case sensitive string, not match start
#' * `%.Like$%`     - Case sensitive string, match end
#' * `%.~Like$%`    - Case sensitive string, not match end
#' * `%.like%`      - Case insensitive string, match anywhere
#' * `%.~like%`     - Case insensitive string, not match anywhere
#' * ` %.^like%`    - Case insensitive string, match start
#' * `%.~^like%`    - Case insensitive string, not match start
#' * `%.like$%`     - Case insensitive string, match end
#' * `%.~like$%`    - Case insensitive string, not match end
#' * `%.gt%`        - Greater than
#' * `%.ge%`        - Greater than or equal
#' * `%.lt%`        - Less than
#' * `%.le%`        - Less than or equal
#' * `%.token%`     - Match on multiple tokens in search property
#' * `%.~token%`    - Not match on multiple tokens in search property
#' * `%.in%`        - Find objects matching 1 or more values
#' * `%.~in%`       - Find objects not matching 1 or more values
#'
#' @param property The property on the metadata you want to filter on
#' @param operator The comparison operator you want to perform
#' @param values The value to check against
#' @param call description
#'
#'
#' @return A spliced list with filter in the format property:operator:value
#'
#' @examples
#'
#' # Generate an equality filter
#' id %.eq% 'element_id'
#'
#' # Finding multiple ids
#' 'id' %.in% c('id1', 'id2', 'id3')
#'
#' # Get all data elements which have a data set with id ID1
#' 'dataSetElements.dataSet.id' %.eq% 'ID1'
#'
#' # get data elements which are members of the ANC data element group
#' 'dataElementGroups.id' %.eq% 'qfxEYY9xAl6'
#'
#' # Get data elements which have any option set
#' metadata_filter('optionSet', '!null', NULL)
#'
#' @name metadata-filter
#'
#' @export

metadata_filter <- function(property,
                            operator,
                            values,
                            call = caller_env()) {

    check_scalar_character(property, arg = caller_arg(property), call = call)
    check_scalar_character(operator, arg = caller_arg(operator), call = call)
    check_supported_operator(operator, arg = caller_arg(operator), call = call)
    check_required(values, arg = caller_arg(values), call = call)

    if (length(values) > 1 &&
        !(operator %in% c('in', '!in'))) {
        khis_abort(
            c('x' = 'A vector of values is only supported for in and !in operators')
        )
    }

    if (is_empty(values) &&
        !(operator %in% c('null', '!null', 'empty'))) {
        khis_abort(
            c(
                'x' = '{.arg {values_arg}} cannot be null',
                '!' = 'NULL values are only supported for null, !null and empty operators'
            )
        )
    }

    if (!is_empty(values) &&
        (operator %in% c('null', '!null', 'empty'))) {
        khis_abort(
            c(
                'x' = '{.arg {values_arg}} must be NULL',
                '!' = 'NULL values required for null, !null and empty operators'
            )
        )
    }

    values = unique(values)

    if (operator %in% c('in', '!in')) {
        filters <- str_c(
            property, ':', operator, ':[',
            str_c(values, collapse = ','), ']'
        )
    } else {
        filters <- if (is_empty(values) || is.na(values)) {
            str_c(property, ':', operator)
        } else {
            str_c(property, ':', operator, ':', values)
        }
    }

    return(splice(list2(filter = filters)))
}

#' @rdname metadata-filter
#' @name %.eq%
#' @export

'%.eq%' <- function(property, values) {
    property <- as.character(ensym(property))
    metadata_filter(property, 'eq', values)
}

#' @rdname metadata-filter
#' @name %.ieq%
#' @export

'%.ieq%' <- function(property, values) {
    property <- as.character(ensym(property))
    metadata_filter(property, 'ieq', values)
}

#' @rdname metadata-filter
#' @name %.~eq%
#' @export

'%.~eq%' <- function(property, values) {
    property <- as.character(ensym(property))
    metadata_filter(property, '!eq', values)
}

#' @rdname metadata-filter
#' @name %.ne%
#' @export

'%.ne%' <- function(property, values) {
    property <- as.character(ensym(property))
    metadata_filter(property, 'ne', values)
}

#' @rdname metadata-filter
#' @name %.Like%
#' @export

'%.Like%' <- function(property, values) {
    property <- as.character(ensym(property))
    metadata_filter(property, 'like', values)
}

#' @rdname metadata-filter
#' @name %.~Like%
#' @export

'%.~Like%' <- function(property, values) {
    property <- as.character(ensym(property))
    metadata_filter(property, '!like', values)
}

#' @rdname metadata-filter
#' @name %.^Like%
#' @export

'%.^Like%' <- function(property, values) {
    property <- as.character(ensym(property))
    metadata_filter(property, '$like', values)
}

#' @rdname metadata-filter
#' @name %.~^Like%
#' @export

'%.~^Like%' <- function(property, values) {
    property <- as.character(ensym(property))
    metadata_filter(property, '!$like', values)
}

#' @rdname metadata-filter
#' @name %.Like$%
#' @export

'%.Like$%' <- function(property, values) {
    property <- as.character(ensym(property))
    metadata_filter(property, 'like$', values)
}

#' @rdname metadata-filter
#' @name %.~Like$%
#' @export

'%.~Like$%' <- function(property, values) {
    property <- as.character(ensym(property))
    metadata_filter(property, '!like$', values)
}

#' @rdname metadata-filter
#' @name %.like%
#' @export

'%.like%' <- function(property, values) {
    property <- as.character(ensym(property))
    metadata_filter(property, 'ilike', values)
}

#' @rdname metadata-filter
#' @name %.~like%
#' @export

'%.~like%' <- function(property, values) {
    property <- as.character(ensym(property))
    metadata_filter(property, '!ilike', values)
}

#' @rdname metadata-filter
#' @name %.^like%
#' @export

'%.^like%' <- function(property, values) {
    property <- as.character(ensym(property))
    metadata_filter(property, '$ilike', values)
}

#' @rdname metadata-filter
#' @name %.~^like%
#' @export

'%.~^like%' <- function(property, values) {
    property <- as.character(ensym(property))
    metadata_filter(property, '!$ilike', values)
}

#' @rdname metadata-filter
#' @name %.like$%
#' @export

'%.like$%' <- function(property, values) {
    property <- as.character(ensym(property))
    metadata_filter(property, 'ilike$', values)
}

#' @rdname metadata-filter
#' @name %.~like$%
#' @export

'%.~like$%' <- function(property, values) {
    property <- as.character(ensym(property))
    metadata_filter(property, '!ilike$', values)
}

#' @rdname metadata-filter
#' @name %.gt%
#' @export

'%.gt%' <- function(property, values) {
    property <- as.character(ensym(property))
    metadata_filter(property, 'gt', values)
}

#' @rdname metadata-filter
#' @name %.ge%
#' @export

'%.ge%' <- function(property, values) {
    property <- as.character(ensym(property))
    metadata_filter(property, 'ge', values)
}

#' @rdname metadata-filter
#' @name %.lt%
#' @export

'%.lt%' <- function(property, values) {
    property <- as.character(ensym(property))
    metadata_filter(property, 'lt', values)
}

#' @rdname metadata-filter
#' @name %.le%
#' @export

'%.le%' <- function(property, values) {
    property <- as.character(ensym(property))
    metadata_filter(property, 'le', values)
}

#' @rdname metadata-filter
#' @name %.token%
#' @export

'%.token%' <- function(property, values) {
    property <- as.character(ensym(property))
    metadata_filter(property, 'token', values)
}

#' @rdname metadata-filter
#' @name %.~token%
#' @export

'%.~token%' <- function(property, values) {
    property <- as.character(ensym(property))
    metadata_filter(property, '!token', values)
}

#' @rdname metadata-filter
#' @name %.in%
#' @export

'%.in%' <- function(property, values) {
    property <- as.character(ensym(property))
    metadata_filter(property, 'in', values)
}

#' @rdname metadata-filter
#' @name %.~in%
#' @export

'%.~in%' <- function(property, values) {
    property <- as.character(ensym(property))
    metadata_filter(property, '!in', values)
}
