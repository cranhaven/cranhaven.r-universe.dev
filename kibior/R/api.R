#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom stringr str_split
#' @importFrom purrr is_null is_list is_character is_logical
#' @importFrom jsonlite fromJSON
#' @importFrom rio export import
#' @importFrom tibble as_tibble
#' @importFrom dplyr select filter
#' @importFrom tidyr replace_na
#' @importFrom magrittr %>%
#' @importFrom data.table rbindlist
#' @importFrom elastic mapping_create index_forcemerge index_optimize index_flush index_clear_cache index_open index_close connect index_recreate index_create index_delete cluster_stats index_get docs_bulk_update docs_bulk_index reindex Search docs_mget scroll scroll_clear
#'
#' @export
#'
#' @keywords data integration dataset
#'
#' @name kibior
#' 
#' @title KibioR, an Kibio and Elasticsearch data manipulation package.
#' 
#' @description KibioR is a lightweight package for data manipulation 
#'  with Elasticsearch. Its main features allow easy data import, export,
#'  download, upload, searching and sharing to any Elasticsearch-based open 
#'  architecture, scaling to billions of data and TB capability.  
#'
#' @details A client to send, retrieve, search, join data in Elasticsearch.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @author Régis Ongaro-Carcy,
#'  \email{regis.ongaro-carcy2@crchudequebec.ulaval.ca}
#'
#' @references Kibio.science: \url{http://kibio.science}, \cr
#'  Elasticsearch documentation: 
#'  \url{https://www.elastic.co/guide/en/elasticsearch/reference/current/index.html}
#'
#' @seealso \code{\link{kibior}}
#'
#' @section Constructor Arguments:
#'  \tabular{llll}{
#'  \strong{Argument} \tab \strong{Type} \tab \strong{Details} \tab \strong{Default} \cr
#'  \code{host} \tab character \tab address or name of Elasticsearch server \tab "localhost" \cr
#'  \code{port} \tab numeric \tab port of Elasticsearch server \tab 9200 \cr
#'  \code{user} \tab character \tab if required by the server, the username for authentication \tab NULL \cr
#'  \code{pwd} \tab character \tab if required by the server, the password for authentication \tab NULL \cr
#'  \code{verbose} \tab logical \tab verbose mode \tab FALSE \cr
#'  }
#'
Kibior <- R6Class(
    c("Kibior", "KibiorOperators"),
    lock_objects = FALSE,
    lock_class = FALSE,

    # ============================================================================
    # PRIVATE
    # ============================================================================

    private = list(

        # --------------------------------------------------------------------------
        # attributes
        # --------------------------------------------------------------------------

        # Private .host Elasticsearch connection host
        .host = NULL,

        # Private .port Elasticsearch connection port
        .port = NULL,

        # Private .user Elasticsearch auth user
        .user = NULL,

        # Private .pwd Elasticsearch auth password
        .pwd = NULL,

        # Private .connection Elstic connection, computed automatically when host or port are 
        #   changed
        .connection = NULL,

        # Private .es_version Elasticsearch version, computed automatically when the connection is 
        #   set
        .es_version = NULL,

        # Private .head_search_size Size of search when head mode is activated, `$search(head = 
        #   TRUE)`
        .head_search_size = 5,

        # Private .es_wait Time in second to wait when an update command is sent to ES
        .es_wait = 2,

        # Private .es_primaries Default allocated number of primary shards when a new ES index is 
        #'  created
        .es_primary_shards = 1,

        # Private .es_replica_shards Default allocated number of replica shards when a new ES index 
        #   is created
        .es_replica_shards = 1,

        # Private .default_id_col Default name given to unique field when pushing data.
        .default_id_col = "kid",


        # --------------------------------------------------------------------------
        # constants
        # --------------------------------------------------------------------------

        # Private .MAX_BUCKET_SIZE Threshold for returning data from ES
        .MAX_BUCKET_SIZE = 10000L,

        # Private .VALID_JOINS List of valid joins, used to test
        .VALID_JOINS = c("inner", "full", "left", "right", "semi", "anti"),

        # Private .VALID_COUNT_TYPES List of count valid types, used to test
        .VALID_COUNT_TYPES = c("observations", "variables"),

        # Private .VALID_FEATURES List of Elasticsearch valid features in metadata, used to test
        .VALID_FEATURES = c("settings", "mappings", "aliases"),

        # Private .VALID_PUSH_MODES List of valid push data modes, used to test
        .VALID_PUSH_MODES = c("check", "recreate", "update"),

        # Private .RESERVED_QUERY_STRING_CHAR reserved characters for query string syntax
        .RESERVED_QUERY_STRING_CHAR = stringr::str_split("+ - = && || > < ! ( ) { } [ ] ^ \" ~ * ? : \\ /", " ")[[1]],

        # Private .AUTO_IMPORT_EXTENSION_MAPPING List of file extension to try automatic import
        .AUTO_IMPORT_EXTENSION_MAPPING = list(
            # tabular
            "csv"   = "tabular",
            "tsv"   = "tabular",
            "txt"   = "tabular",
            "tab"   = "tabular",
            # features
            "wig"   = "features",
            "bigwig"= "features",
            "gtf"   = "features",
            "gff"   = "features",
            "gff3"  = "features",
            "bed"   = "features",
            # "bb" = stop("Not supported. See https://support.bioconductor.org/p/116736/"),
            # alignment
            "bam"   = "alignments",
            "bai"   = "alignments",
            # sequence
            "fasta" = "sequences",
            "fa"    = "sequences",
            "fna"   = "sequences",
            "ffn"   = "sequences",
            "faa"   = "sequences",
            "frn"   = "sequences",
            # reads
            "fastq" = "reads", 
            "fq"    = "reads",
            # json
            "json"  = "json"
        ),

        # Private kibior indices metadata
        .KIBIOR_METADATA_INDICES = ".kibior_indices",


        # --------------------------------------------------------------------------
        # methods - inherits
        # --------------------------------------------------------------------------

        finalize = function(){
            "Destructor: finalize this object, wipe state"

            private$.connection <- NULL
            private$.es_version <- NULL
            private$.host <- NULL
            private$.port <- NULL
            private$.user <- NULL
            private$.pwd <- NULL
        },


        # --------------------------------------------------------------------------
        # methods - abstract & utils
        # --------------------------------------------------------------------------

        single_value_metric_aggregation = function(aggregation_type, index_name, columns, query = NULL){
            "[Abstract method] Execute a single value metric aggregation."
            "Mainly for avg, min, max, sum."
            ""
            "@param aggregation_type the aggregation to execute."
            "@param index_name the indices to target."
            "@param columns the columns to target."
            "@param query the query to target some data. (default: NULL)"
            "@return the list of tibble with the results."
            if(!purrr::is_character(aggregation_type)) stop(private$err_param_type_character("aggregation_type"))
            if(length(aggregation_type) > 1) stop(private$err_one_value("aggregation_type"))
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            if(!purrr::is_character(columns)) stop(private$err_param_type_character("columns"))
            if(!purrr::is_null(query)){
                if(!purrr::is_character(query)) stop(private$err_param_type_character("query"))
                if(length(query) > 1) stop(private$err_one_value("query"))
            }
            #
            involved_indices <- self$match(index_name)
            # 
            res <- list()
            if(!purrr::is_null(involved_indices)){
                # searched cols
                if(self$verbose){
                    columns %>% 
                        private$vector_to_str() %>% 
                        message(" -> Searching column names: ", .)
                    message(" -> Getting ", aggregation_type)
                }
                # 
                get_value <- function(v){
                    if(self$version$major > 6) v$value else v
                }
                # 
                for(ind in involved_indices){
                    res[[ind]] <- list()
                    # get all columns from an index
                    real_columns <- suppressMessages({ self$columns(ind)[[ind]] })
                    #
                    for(col in columns){
                        # req
                        res[[ind]][[col]] <- tryCatch(
                            expr = {
                                # raise an error if the col is not known
                                # should not happen but sum() actually let the
                                # unknown column with 0 as default value
                                if(!(col %in% real_columns)) stop("ABSENT_COLUMN")
                                # define agg
                                agg_body <- paste0('{"size":0,"aggs":{"kaggs":{"',
                                    aggregation_type, '":{"field":"', col, '"}}}}')
                                #req
                                stmp <- elastic::Search(
                                        self$connection, 
                                        index = ind, 
                                        size = 0, 
                                        q = query,
                                        body = agg_body
                                    ) %>% 
                                        .$aggregations %>% 
                                        .$kaggs %>% 
                                        get_value()
                                if(purrr::is_null(stmp)) stop("ABSENT_COLUMN")
                                # add index and col names
                                tmp <- list()
                                tmp[[aggregation_type]] <- stmp
                                tmp <- as.data.frame(tmp, stringsAsFactors = FALSE)
                                tmp <- cbind(column = col, tmp)
                                tmp
                            },
                            error = function(e){
                                if("ABSENT_COLUMN" == e$message){
                                    if(self$verbose){
                                        message("   - Skipping absent column '", col, "' of '", ind, "'.")
                                    }
                                } else {
                                    if(grepl("all shards failed", e$message, ignore.case = TRUE)){
                                        if(self$verbose){
                                            message("   - Skipping non numeric column '", col, "' of '", ind, "'.")
                                        }
                                    } else {
                                        # reraise
                                        stop(e$message)
                                    }
                                }
                            }
                        )
                    }
                    #
                    if(!private$is_list_empty(res[[ind]])){
                        res[[ind]] <- res[[ind]] %>% 
                            data.table::rbindlist() %>% 
                            tibble::as_tibble()
                        # change dot to underscore in names 
                        names(res[[ind]]) <- gsub("\\.", "_", names(res[[ind]]))
                    }
                }
            }
            if(length(res) == 0){
                res <- NULL
            }
            if(self$quiet_results) invisible(res) else res
        },

        multi_value_metric_aggregation = function(aggregation_type, index_name, columns, function_test_null_result = NULL, aggregation_supplementary_args = NULL, query = NULL){
            "[Abstract method] Execute a multi value metric aggregation."
            "Mainly for percentile, stats, summary, q1, q2, q3, median."
            ""
            "@param aggregation_type the aggregation to execute."
            "@param index_name the indices to target."
            "@param columns the columns to target."
            "@param function_test_null_result the function to test the mull ES result (default: NULL)."
            "@param aggregation_supplementary_args a string to pass to the request to add some args (default: NULL)."
            "@param query the query to target some data (default: NULL)."
            "@return the list of tibble with the results."

            if(!purrr::is_character(aggregation_type)) stop(private$err_param_type_character("aggregation_type"))
            if(length(aggregation_type) > 1) stop(private$err_one_value("aggregation_type"))
            if(!purrr::is_null(aggregation_supplementary_args)){
                if(!purrr::is_character(aggregation_supplementary_args)) stop(private$err_param_type_character("aggregation_supplementary_args"))
                if(length(aggregation_supplementary_args) > 1) stop(private$err_one_value("aggregation_supplementary_args"))
            }
            if(purrr::is_null(function_test_null_result)) stop("need a function to test the metrics.")
            if(!("closure" %in% typeof(function_test_null_result))) stop("not a function, need a funciton to test the metrics.")
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            if(!purrr::is_character(columns)) stop(private$err_param_type_character("columns"))
            if(!purrr::is_null(aggregation_supplementary_args) && !purrr::is_character(aggregation_supplementary_args)) stop("aggregation args must be a string.")
            if(length(aggregation_supplementary_args) > 1) stop("aggregation body error.")
            if(!purrr::is_null(query)){
                if(!purrr::is_character(query)) stop(private$err_param_type_character("query"))
                if(length(query) > 1) stop(private$err_one_value("query"))
            }
            #
            involved_indices <- self$match(index_name)
            # 
            res <- list()
            if(!purrr::is_null(involved_indices)){
                # supplementary args for req body
                s <- if(purrr::is_null(aggregation_supplementary_args)) "" else aggregation_supplementary_args
                # searched cols
                if(self$verbose){
                    columns %>% 
                        private$vector_to_str() %>% 
                        message(" -> Searching column names: ", .)
                    message(" -> Getting ", aggregation_type)
                }
                #
                for(ind in involved_indices){
                    res[[ind]] <- list()
                    # get all columns from an index
                    real_columns <- suppressMessages({ self$columns(ind)[[ind]] })
                    for(col in columns){
                        # define agg
                        agg_body <- paste0('{"size":0,"aggs":{"kaggs":{"', 
                            aggregation_type, '":{"field": "', col, '"', s, '}}}}')
                        # req
                        res[[ind]][[col]] <- tryCatch(
                            expr = {
                                # raise an error if the col is not known
                                # should not happen but sum() actually let the
                                # unknown column with 0 as default value
                                if(!(col %in% real_columns)) stop("ABSENT_COLUMN")
                                # req
                                tmp <- elastic::Search(
                                        self$connection, 
                                        index = ind, 
                                        size = 0, 
                                        q = query,
                                        body = agg_body
                                    ) %>% 
                                        .$aggregations %>% 
                                        .$kaggs
                                if(function_test_null_result(tmp)) stop("ABSENT_COLUMN")
                                # add index and col names
                                tmp <- as.data.frame(tmp, stringsAsFactors = FALSE)
                                tmp <- cbind(column = col, tmp)
                                tmp
                            },
                            error = function(e){
                                if( e$message == "ABSENT_COLUMN"){
                                    if(self$verbose){
                                        message("   - Skipping absent column '", col, "' of '", ind, "'.")
                                    }
                                } else {
                                    if(grepl("all shards failed", e$message, ignore.case = TRUE)){
                                        if(self$verbose){
                                            message("   - Skipping non numeric column '", col, "' of '", ind, "'.")
                                        }
                                    } else {
                                        # reraise
                                        stop(e$message)
                                    }
                                }
                            }
                        )
                    }
                    #
                    if(!private$is_list_empty(res[[ind]])){
                        res[[ind]] <- res[[ind]] %>% 
                            data.table::rbindlist() %>% 
                            tibble::as_tibble()
                        # change dot to underscore in names 
                        names(res[[ind]]) <- gsub("\\.", "_", names(res[[ind]]))
                    }
                }
            }
            if(length(res) == 0){
                res <- NULL
            }
            if(self$quiet_results) invisible(res) else res
        },

        join = function(join_type = NULL, left_index, right_index, left_columns = NULL, left_query = NULL, left_bulk_size = 500, left_max_size = NULL, right_columns = NULL, right_query = NULL, right_bulk_size = 500, right_max_size = NULL, by = NULL, keep_metadata = FALSE) {
            "[Abstract method] Execute a join between two datasets using `dplyr` joins."
            "The datasets can be in-memory (variable name) or the name of an currently stored Elasticsearch index."
            "This should not be call directly. "
            "Use one of the (`$inner_join`, `$full_join`, `$left_join`, `$right_join`, `$anti_join` or `$semi_join`) public methods instead."
            ""
            "@param join_type the join type, defined by `private$.VALID_JOINS`. (default: NULL)"
            "@param left_index the left index name or dataset."
            "@param right_index the right index name or dataset."
            "@param left_columns the left index columns to select. (default: NULL)"
            "@param left_query the left index query. (default: NULL)"
            "@param left_bulk_size the left index bulk size when downloading from Elasticsearch. (default: 500)"
            "@param left_max_size the left index max size (hard threshold). (default: NULL)"
            "@param right_columns the right index columns to select. (default: NULL)"
            "@param right_query the right index query. (default: NULL)"
            "@param right_bulk_size the right index bulk size when downloading from Elasticsearch. (default: 500)"
            "@param right_max_size the right index max size (hard threshold). (default: NULL)"
            "@param by the field names used to join the two datasets. Should be a named vector or list of this format c('field_left' = 'field_right') (default: NULL)"
            "@param keep_metadata Keep Elasticsearch metadata? (default: FALSE)"
            "@return the result of the called join"

            # TODO: add a "from_instance" param to join between instances
            # TODO: manage query on in-memory data through `enquo() + !!` ?
            
            # missing ds/index names
            if(missing(left_index)) stop("Missing left dataset/index name.")
            if(missing(right_index)) stop("Missing right dataset/index name.")
            # join type
            if(!purrr::is_character(join_type)) stop(private$err_param_type_character("join_type"))
            if(!(join_type %in% private$.VALID_JOINS)) stop(private$err_not_in_vector("Join type", private$.VALID_JOINS))
            # join
            if(!purrr::is_null(by) && !purrr::is_character(by)) stop(private$err_param_type_character("by", can_be_null = TRUE))
            if(purrr::is_character(by) && private$is_search_pattern(by)) stop(private$err_search_pattern_forbidden("by"))
            # metadata
            if(!purrr::is_logical(keep_metadata)) stop(private$err_param_type_logical("keep_metadata"))
            if(is.na(keep_metadata)) stop(private$err_logical_na("keep_metadata"))
            # add columns from "by" to the left and right columns if not present
            if(!purrr::is_null(left_columns)){
                # get "by" left index column names
                by_left_columns <- by %>% as.list() %>% names()
                left_columns <- c(left_columns, by_left_columns) %>% unique()
            }
            if(!purrr::is_null(right_columns)){
                # get "by" right index column names
                by_right_columns <- by %>% as.list() %>% unlist(use.names = FALSE)
                right_columns <- c(right_columns, by_right_columns) %>% unique()
            }
            # side args check (left and right)
            check_side_args <- function(side){
                #
                name_arg <- function(stype){ paste0(side, "_", stype) }
                call_arg <- function(stype){ name_arg(stype) %>% parse(text = .) %>% eval() }
                # names
                n_index <- name_arg("index")
                n_query <- name_arg("query")
                n_columns <- name_arg("columns")
                n_bulk_size <- name_arg("bulk_size")
                n_max_size <- name_arg("max_size")
                # values
                v_index <- call_arg("index")
                v_query <- call_arg("query")
                v_columns <- call_arg("columns")
                v_bulk_size <- call_arg("bulk_size")
                v_max_size <- call_arg("max_size")
                # index
                if(purrr::is_character(v_index)) {
                    # data from ES index
                    if(private$is_search_pattern(v_index)) stop(private$err_search_pattern_forbidden(n_index))
                    if(!purrr::is_null(v_query)){
                        if(!purrr::is_character(v_query)) stop(private$err_param_type_character(n_query, can_be_null = TRUE))
                        if(length(v_query) > 1) stop(private$err_one_value(n_query))
                    } 
                    if(!purrr::is_null(v_columns) && !purrr::is_character(v_columns)) {
                        stop(private$err_param_type_character(n_columns, can_be_null = TRUE))
                    }
                } else {
                    # data from in-memory 
                    if(purrr::is_null(v_index)) stop(private$err_null_forbidden(n_index))
                    if(!("data.frame" %in% class(v_index))) stop("Joining cannot be executed on '", n_index, "', need an index name, a data.frame or derivative.")
                    if(!purrr::is_null(v_query) && self$verbose) message("`", n_query, "` will be ignored.")
                }
                # bulk size
                if(!is.numeric(v_bulk_size)) stop(private$err_param_type_numeric(n_bulk_size))
                if(length(v_bulk_size) > 1) stop(private$err_one_value(n_bulk_size))
                if(v_bulk_size < 1) stop(private$err_param_positive(n_bulk_size, can_be_null = FALSE))
                # max size
                if(!purrr::is_null(v_max_size)) {
                    if(!is.numeric(v_max_size)) stop(private$err_param_type_numeric(n_max_size))
                    if(length(v_max_size) > 1) stop(private$err_one_value(n_max_size))
                    if(v_max_size < 1) stop(private$err_param_positive(n_max_size, can_be_null = FALSE))
                }
            }
            # check left side
            check_side_args("left")
            # check right side
            check_side_args("right")

            #
            get_data <- function(index, columns, query, bulk_size, max_size){
                "Function: get data from ES index or memory"
                index_data <- NULL
                if(purrr::is_character(index)){
                    # if char, it is the index name needed to be pulled out
                    if(self$verbose) message("from index '", index, "'")
                    index_data <- self$pull(
                            index_name = index,
                            columns = columns,
                            query = query,
                            bulk_size = bulk_size,
                            max_size = max_size,
                            keep_metadata = keep_metadata
                        )[[index]]
                    # if keep metadata, remove "_source" prefix in colnames
                    if(keep_metadata){
                        index_data <- index_data %>% 
                            dplyr::rename_at(
                                dplyr::vars(dplyr::starts_with('_source.')),
                                .funs = function(x){ 
                                    x %>% lapply(function(y){ 
                                        stringr::str_split(y, "_source.")[[1]][2] 
                                    })
                                }
                            )
                    }
                } else {
                    # if data.frame derivative, just load it as tibble
                    if(self$verbose) message("from memory")
                    index_data <- index %>%
                        dplyr::select( if(purrr::is_null(columns)) dplyr::everything() else columns ) %>%
                        (function(x){ if(!purrr::is_null(max_size)) head(x, max_size) else x }) %>%
                        tibble::as_tibble(.name_repair = "unique")
                }
                # ret
                index_data
            }
            # get left
            if(self$verbose) message("Getting left ", appendLF = FALSE)
            left <- get_data(
                    index = left_index, 
                    columns = left_columns, 
                    query = left_query, 
                    bulk_size = left_bulk_size, 
                    max_size = left_max_size
                ) 
            # get right
            if(self$verbose) message("Getting right ", appendLF = FALSE)
            right <- get_data(
                    index = right_index, 
                    columns = right_columns, 
                    query = right_query, 
                    bulk_size = right_bulk_size, 
                    max_size = right_max_size
                )
            # prepare args for join
            fargs <- list(
                left, 
                right, 
                by = by, 
                suffix = c("_left", "_right")
            )
            # call to dplyr joins
            join_name <- paste0(join_type, "_join")
            if(self$verbose) message(" -> Executing ", join_name, "...")
            join_name %>%
                list("dplyr") %>%
                do.call(what = "getFromNamespace", args = .) %>%
                do.call(args = fargs)
        },

        symmetric_difference = function(x, y){
            "Compute a symmetric difference between two sets"
            ""
            "@param x the first set."
            "@param y the second set."
            "@return the symmetric difference"
            sd <- dplyr::setdiff(dplyr::union(x, y), dplyr::intersect(x, y))
            if(length(sd) == 0) sd <- NULL
            sd
        },

        round = function(nb, nb_decimal = 1L){
            "Round to a number to a given decimal number"
            ""
            "@param nb the number to round."
            "@param nb_decimal the number of decimal to get."
            "@return a rounded number"
            if(!is.numeric(nb)) stop(private$err_param_type_numeric("nb"))
            if(!is.numeric(nb_decimal)) stop(private$err_param_type_numeric("nb_decimal"))
            if(nb_decimal < 0) stop(private$err_param_positive("nb_decimal"))
            nb_decimal <- as.integer(nb_decimal)
            #
            round(nb, nb_decimal) %>%
                format(nsmall = nb_decimal) %>%
                trimws() %>% 
                as.double()
        },

        humanize_time = function(time){
            "Format a second-based time to a easily readable string"
            ""
            "@param time the time in seconds"
            "@return a list composed of `$time` and `$unit`"

            # take a time number (in seconds) and returns a more readable version with unit
            if(!is.numeric(time)) stop(private$err_param_type_numeric("time"))
            if(time < 0) stop(private$err_param_positive("time"))
            # time in seconds
            res <- list(unit = "s", time = time)
            # milliseconds
            if(res$time < 1){
                # update to ms
                res$time <- res$time * 1000
                res$unit <- "ms"
            } else {
                # minutes
                if(res$time > 60){
                    # update to min
                    res$time <- res$time / 60
                    res$unit <- "m"
                    if(res$time > 60){
                        # update to hour
                        res$time <- res$time / 60
                        res$unit <- "h"
                        if(res$time > 24){
                            # update to day
                            res$time <- res$time / 24
                            res$unit <- "d"
                        }
                    }
                }
            }
            # round it 
            res$time <- as.character(private$round(res$time, nb_decimal = 3))
            res
        },

        is_list_empty = function(my_list){
            "Shortcut to test if a variable is a list and is empty"
            ""
            "@examples"
            "'aaa' %>% is_list_empty()            # ERROR"
            "c('aaa', 'bbb') %>% is_list_empty()  # ERROR"
            "list('aaa') %>% is_list_empty()      # [1] FALSE"
            "list() %>% is_list_empty()           # [1] TRUE"
            ""
            "@param my_list an object to test"
            "return ERROR if my_list is not a list, TRUE if my_list is empty, else FALSE"

            if(!purrr::is_list(my_list)) stop(private$err_param_type_list("my_list"))
            (length(my_list) == 0)
        },

        vector_to_str = function(vect){
            "Transform a vector to a listed string"
            vect %>% 
                paste0(collapse = "', '") %>% 
                paste0("'", ., "'")
        },

        df_to_line = function(df){
            "Transform a dataframe-like structure to a list of its lines"
            split(df, seq(nrow(df)))
        },


        # --------------------------------------------------------------------------
        # methods - Elastic utils
        # --------------------------------------------------------------------------

        connect = function(){
            "Connect to Elasticsearch API with the given attributes by updating the `$connection`."
            "Called automatically when changing `$host` or `$port`."
            "Should not be called otherwise."
            "Also update the Elasticsearch version available through `$version` when connected."

            private$.connection <- elastic::connect(host = self$host, 
                                                    port = self$port, 
                                                    user = self$user, 
                                                    pwd = self$pwd)
            # test connection
            p <- self$ping()
            if(purrr::is_null(p)) stop("Connection to '", self$host, ":", self$port, "' cannot be established.")
            # if connection is good
            msg <- paste0("Connected to '", p$cluster_name, "' at '", self$host, ":", self$port, "'")
            if(self$verbose) msg <- paste0(msg, " [Elasticsearch ", p$version$number, "]")
            message(msg)
            # Catch ES version
            private$.es_version <- stringr::str_split(p$version$number, "\\.")[[1]] %>% as.integer() %>% as.list()
            names(private$.es_version) <- c("major", "minor", "patch")
        },

        metadata_type = function(mtype, index_name){
            if(!purrr::is_character(mtype)) stop(private$err_param_type_character("mtype"))
            if(length(mtype) > 1) stop(private$err_one_value("mtype"))
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            #
            res <- NULL
            m <- suppressWarnings({
                elastic::index_get(
                    self$connection, 
                    index = index_name,
                    features = mtype,
                    include_type_name = FALSE, 
                    verbose = self$verbose
                )
            })
            if(length(m) > 0){
                res <- m %>%
                    lapply(function(x){ x[[mtype]] }) %>%
                    (function(x){ if(private$is_list_empty(x)) NULL else x })
            }
            if(self$quiet_results) invisible(res) else res
        },

        define_mappings = function(data){
            "Define the Elasticsearch mapping of a dataset (list subclass)."
            ""
            "@examples"
            "private$define_mappings(starwars) # private method, cannot be called from outside"
            ""
            "@param data a dataset"
            "@return the mapping of the dataset"

            if(!purrr::is_list(data)) stop("Need `data.frame` or derivative data structure")
            if(purrr::is_null(data) || length(data) == 0) stop(private$err_empty_data("data"))
            # types
            text_type <- list(
                type = "text",
                fields = list(
                    keyword = list(
                        type = "keyword",
                        ignore_above = 256
                    )
                )
            )
            numeric_type <- list(type = "float")
            # map
            map_types <- function(x,y){
                xclass <- class(x)
                if(length(xclass) > 1 && "factor" %in% xclass){
                    xclass <- "factor"
                }
                switch(xclass,
                    "NULL" = {
                        stop("The column '", y, "' contains only NULL values. Remove it before pushing.")
                    },
                    # text types
                    "factor"     = { text_type },
                    "character"  = { text_type },
                    "AsIs"       = { text_type },
                    "logical"    = { text_type },
                    # numerical types
                    "numeric"    = { numeric_type },
                    "integer"    = { numeric_type },
                    "double"     = { numeric_type },
                    # in case of list, get the type of the first non null value 
                    "list"       = { map_types(unlist(x, use.names = FALSE)[[1]], y) },
                    # data.frame
                    "data.frame" = {
                        stop("Data contains multiple levels of dataframes, which are not handled by Kibior") 
                    },
                    # all others
                    stop("Unknown type '", xclass, "' when creating Elasticsearch mapping")
                )
            }
            # return mapping body
            purrr::imap(data, map_types) %>%
                list(properties = .)
        },

        create_mappings = function(index_name, mapping){
            "Add a mapping on an index based on given data."
            ""
            "@param index_name a vector of index names."
            "@param mapping mapping to apply."
            "@return a list of indices, each containing their number of observations and variables."

            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            if(length(index_name) > 1) stop(private$err_one_value("index_name"))
            # req
            if(self$verbose) message(" -> Applying mapping to '", index_name, "'")
            res <- tryCatch(
                expr = {
                    if(self$verbose) message("   - Trying without type insertion (ES > v7)... ", appendLF = FALSE)
                    # do not insert mapping type
                    r <- elastic::mapping_create(
                        conn = self$connection,
                        index = index_name,
                        body = mapping
                    )
                    if(self$verbose) message("ok")
                    r
                },
                error = function(e){
                    res <- NULL
                    if(self$verbose) message("nok")
                    if(grepl("mapping type is missing", e$message, ignore.case = TRUE)){
                        # older ES version < 7, try another time with mapping types
                        if(self$verbose) message("   - Trying with type insertion (ES < v7)... ", appendLF = FALSE)
                        # with type
                        res <- tryCatch(
                            expr = {
                                res <- elastic::mapping_create(
                                    conn = self$connection,
                                    index = index_name,
                                    type = "_doc",
                                    include_type_name = TRUE,
                                    body = mapping
                                )
                                if(self$verbose) message("ok")
                                res
                            }, 
                            error = function(ee){
                                if(self$verbose) message("nok")
                                stop(ee$message)
                            }
                        )
                    } else {
                        stop(e$message)
                    }
                    res
                }
            )
            #
            if(self$verbose) message(" -> Waiting a bit for Elasticsearch")
            Sys.sleep(self$elastic_wait)
            res
        },

        force_merge = function(index_name, max_num_segments = NULL, only_expunge_deletes = FALSE, flush = TRUE){
            "TODO desc"

            if(missing(index_name)) index_name <- "*"
            elastic::index_forcemerge(self$connection, 
                                      index = index_name, 
                                      max_num_segments = max_num_segments, 
                                      only_expunge_deletes = only_expunge_deletes, 
                                      flush = flush)
        },

        optimize = function(index_name, max_num_segments = NULL, only_expunge_deletes = FALSE, flush = TRUE, wait_for_merge = TRUE){
            "TODO desc"

            if(self$version$major < 5){
                elastic::index_optimize(self$connection, 
                                        index = index_name, 
                                        max_num_segments = max_num_segments, 
                                        only_expunge_deletes = FALSE, 
                                        flush = flush, 
                                        wait_for_merge = wait_for_merge)
            } 
        },

        is_search_pattern = function(pattern){
            "Tell if a given string vector contains an Elasticsearch pattern string"
            ""
            "@param s the string to test"
            "@return TRUE if the string is a pattern, else FALSE"

            if(!purrr::is_character(pattern)) stop(private$err_param_type_character("pattern"))
            grepl("*", pattern, fixed = TRUE) %>% any()
        },


        # --------------------------------------------------------------------------
        # Format error messages
        # --------------------------------------------------------------------------

        err_null_forbidden = function(arg_name){
            "Argument cannot be null"
            paste0("`", arg_name, "` must not be NULL.")
        },

        err_active_is_read_only = function(param_name){
            "For Active bindings - argument is read-only"
            paste0("`", param_name, "` is read-only.")
        },

        err_param_type = function(param_name, expected_type, can_be_null = FALSE){
            "Wrong param type"
            s <- if(can_be_null) " or NULL" else ""
            paste0("`", param_name, "` must be ", expected_type, s, ".")
        },

        err_param_type_numeric = function(param_name, can_be_null = FALSE){
            "Wrong param type, expected numeric"
            private$err_param_type(param_name, "numeric", can_be_null = can_be_null)
        },

        err_param_type_logical = function(param_name, can_be_null = FALSE){
            "Wrong param type, expected logical"
            private$err_param_type(param_name, "logical", can_be_null = can_be_null)
        },

        err_param_type_character = function(param_name, can_be_null = FALSE){
            "Wrong param type, expected character"
            private$err_param_type(param_name, "character", can_be_null = can_be_null)
        },

        err_param_type_list = function(param_name, can_be_null = FALSE){
            "Wrong param type, expected list"
            private$err_param_type(param_name, "list", can_be_null = can_be_null)
        },

        err_not_kibior_instance = function(param_name, can_be_null = FALSE){
            "Wrong type"
            private$err_param_type(param_name, "a Kibior instance.", can_be_null = can_be_null)
        },

        err_param_positive = function(param_name, zero_valid = FALSE){
            "Wrong param type"
            s <- if(zero_valid) "" else " and non-NULL"
            paste0("`", param_name, "` must be positive", s, ".")
        },

        err_one_value = function(param_name){
            "Wrong number of value in the variable, expected only one"
            paste0("`", param_name, "` must have a length of 1.")
        },

        err_logical_na = function(param_name){
            "Wrong logical NA"
            paste0("`", param_name, "` must TRUE or FALSE, not NA.")
        },

        err_search_pattern_forbidden = function(param_name){
            "Not search pattern here"
            paste0("`", param_name, "` must not contain search pattern.")
        },

        err_index_unknown = function(index_name){
            "Index unknown"
            index_name %>%
                private$vector_to_str() %>%
                paste0("One or more indices in [", ., "] are unknown.")
        },

        err_index_already_exists = function(index_name){
            "Index already there"
            paste0("Index '", index_name, "' already exists.")
        },

        err_not_in_vector = function(s, vect){
            "Value not in vector"
            private$vector_to_str(vect) %>%
                paste0(s, " must be one of [", ., "].")
        },

        err_empty_data = function(param_name){
            "Dataset is empty"
            paste0("Data in `", param_name, "` is empty.") 
        },

        err_field_unknown = function(index_name, field_name){
            "Field is unknown or not present"
            paste0("Field `", field_name, "` is not present in index `", index_name, "`.") 
        },

        err_pkg_required = function(method_name, pkg_name, repo_name){
            paste0("Method `", method_name, "` requires `", pkg_name, "` (", repo_name,") package.") %>%
                paste0("\nPlease, install this package and retry.")
        },
       
        # Sad error is sad.
        ERR_WTF = "Well. This is sad. Report with an issue please."
    ),


    # ============================================================================
    # ACTIVE BINDING
    # ============================================================================

    active = list(

        #' @field host Access and change the Elasticsearch host
        host = function(value) {
            "Access and change the Elasticsearch host"
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$host"
            "kc$host <- 'elasticsearch' # 'elasticsearch' needs to be online, known and accessible"
            "kc$host"
            ""
            "@param value new host"
            "@return A string representing the host used in connection"

            if(missing(value)) {
                private$.host
            } else {
                if(!purrr::is_character(value)) stop(private$err_param_type_character("$host"), call. = FALSE)
                # change host and reconnect
                private$.host <- value
                private$connect()
            }
        },

        #' @field port Access and change the Elasticsearch port
        port = function(value) {
            "Access and change the Elasticsearch port"
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$port"
            "kc$port <- 15000 # the port 15000 needs to be online and accessible"
            "kc$port"
            ""
            "@param value new port"
            "@return A number representing the port used in connection"

            if(missing(value)) {
                private$.port
            } else {
                if(!is.numeric(value)) stop(private$err_param_type_numeric("$port"), call. = FALSE)
                # change port and reconnect
                private$.port <- as.integer(value)
                private$connect()
            }
        },

        #' @field endpoint Access the Elasticsearch main endpoint
        endpoint = function(value) {
            "Access the Elasticsearch endpoint address."
            "Not mutable, initialize another client"
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$endpoint"
            "kc$endpoint <- 'foobar' # Error, read only"
            ""
            "@param value not used"
            "@return A string representing the Elasticsearch endpoint."

            if(missing(value)) {
                paste0("http://", self$host, ":", self$port)
            } else {
                stop(private$err_active_is_read_only("$endpoint"), call. = FALSE)
            }
        },

        #' @field user Access the Elasticsearch user.
        user = function(value) {
            "Access the Elasticsearch user."
            "Not mutable, initialize another client"
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$user"
            "kc$user <- 'foobar' # Error, read only"
            ""
            "@param value new user, not used"
            "@return A string representing the user used in connection"

            if(missing(value)) {
                private$.user
            } else {
                stop(private$err_active_is_read_only("$user"), call. = FALSE)
            }
        },

        #' @field pwd Access the Elasticsearch password.
        pwd = function(value) {
            "Access the Elasticsearch password."
            "Not mutable, initialize another client"
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$pwd"
            "kc$pwd <- 'foobar' # Error, read only"
            ""
            "@param value new pwd, not used"
            "@return A string representing the password used in connection"

            if(missing(value)) {
                private$.pwd
            } else {
                stop(private$err_active_is_read_only("$pwd"), call. = FALSE)
            }
        },

        #' @field connection Access the Elasticsearch connection object.
        connection = function(value) {
            "Access the Elasticsearch connection object. "
            "Not mutable directly, change hsot or port to modify"
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$connection"
            "kc$connection <- 'whatever' # Error, read only"
            ""
            "@param value new connection, not used"
            "@return A `elastic::connect()` object"

            if(missing(value)) {
                private$.connection
            } else {
                stop(private$err_active_is_read_only("$connection"), call. = FALSE)
            }
        },

        #' @field head_search_size Access and change the head size default value.
        head_search_size = function(value){
            "Access and change the head size default value."
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$head_search_size"
            "kc$head_search_size <- 10"
            "kc$head_search_size"
            ""
            "@param value new head size"
            "@return A number representing the number of records the `$search()` method will get as sample"

            if(missing(value)) {
                private$.head_search_size
            } else {
                if(!is.numeric(value)) stop(private$err_param_type_numeric("$head_search_size"), call. = FALSE)
                private$.head_search_size <- value
            }
        },

        #' @field cluster_name Access the cluster name if and only if already connected.
        cluster_name = function(value){
            "Access the cluster name if and only if already connected."
            "Shortcut to `$infos()$cluster_name`"
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$cluster_name"
            "kc$cluster_name <- 'whatever' # Error, read only"
            ""
            "@param value not used"
            "@return A string representing the cluster name"

            if(missing(value)) {
                self$infos()$cluster_name
            } else {
                stop(private$err_active_is_read_only("$cluster_name"), call. = FALSE)
            }
        },

        #' @field cluster_status Access the cluster status if and only if already connected.
        cluster_status = function(value){
            "Access the cluster status if and only if already connected."
            "Shortcut to `$infos()$status`"
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$cluster_status"
            "kc$cluster_status <- 'whatever' # Error, read only"
            ""
            "@param value not used"
            "@return A string representing the cluster status ('green', 'yellow', or 'red')"

            if(missing(value)) {
                self$infos()$status
            } else {
                stop(private$err_active_is_read_only("$cluster_status"), call. = FALSE)
            }
        },

        #' @field nb_documents Access the current cluster total number of documents if and only if 
        #'  already connected.
        nb_documents = function(value){
            "Access the current cluster total number of documents if and only if already connected."
            "Shortcut to `$infos()$indices$docs$count`"
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$nb_documents"
            "kc$nb_documents <- 'whatever' # Error, read only"
            ""
            "@param value not used"
            "@return A string representing the total number of recorded documents"

            if(missing(value)) {
                self$infos()$indices$docs$count
            } else {
                stop(private$err_active_is_read_only("$nb_documents"), call. = FALSE)
            }
        },

        #' @field version Access the Elasticsearch version if and only if already connected.
        version = function(value){
            "Access the Elasticsearch version if and only if already connected."
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$version"
            "kc$version <- 'whatever' # Error, read only"
            ""
            "@param value not used"
            "@return A list with a semantic versioning (see `$major`, `$minor`, and `$patch`) of Elasticsearch"

            if(missing(value)) {
                private$.es_version
            } else {
                stop(private$err_active_is_read_only("$version"), call. = FALSE)
            }
        },

        #' @field elastic_wait Access and change the Elasticsearch wait time for update commands if 
        #'  and only if already connected.
        elastic_wait = function(value){
            "Access and change the Elasticsearch wait time for update commands if and only if already connected."
            "It is sometimes needed when Elasticsearch server configuration is not optimized for data ingestion and need extra delay."
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$elastic_wait"
            "kc$elastic_wait <- 'whatever' # Error, read only"
            ""
            "@param value the new time to wait in second"
            "@return A list with a semantic versioning (see `$major`, `$minor`, and `$patch`) of Elasticsearch"

            if(missing(value)) {
                private$.es_wait
            } else {
                if(!is.numeric(value)) stop("`$elastic_wait` must be numeric", call. = FALSE)
                if(value <= 0) stop(private$err_param_positive("$elastic_wait", zero_valid = FALSE), call. = FALSE)
                private$.es_wait <- value
            }
        },

        #' @field valid_joins Access the valid joins available in Kibior.
        valid_joins = function(value){
            "Access the valid joins available in Kibior."
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$valid_joins"
            "kc$valid_joins <- 'whatever' # Error, read only"
            ""
            "@param value not used"
            "@return A vector representing possible valid joins used internally by Kibior"

            if(missing(value)) {
                private$.VALID_JOINS
            } else {
                stop(private$err_active_is_read_only("$valid_joins"), call. = FALSE)
            }
        },

        #' @field valid_count_types Access the valid count types available (mainly observations = 
        #'  rows, variables = columns)
        valid_count_types = function(value){
            "Access the valid count types available (mainly observations = rows, variables = columns)"
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$valid_count_types"
            "kc$valid_count_types <- 'whatever' # Error, read only"
            ""
            "@param value not used"
            "@return A vector representing possible valid count types used internally by Kibior"

            if(missing(value)) {
                private$.VALID_COUNT_TYPES
            } else {
                stop(private$err_active_is_read_only("$valid_count_types"), call. = FALSE)
            }
        },

        #' @field valid_elastic_metadata_types Access the valid Elasticsearch metadata types 
        #'  available.
        valid_elastic_metadata_types = function(value){
            "Access the valid Elasticsearch metadata types available."
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$valid_elastic_metadata_types"
            "kc$valid_elastic_metadata_types <- 'whatever' # Error, read only"
            ""
            "@param value not used"
            "@return A vector representing possible valid push modes"

            if(missing(value)) {
                private$.VALID_FEATURES
            } else {
                stop(private$err_active_is_read_only("$valid_elastic_metadata_types"), call. = FALSE)
            }
        },

        #' @field valid_push_modes Access the valid push modes available.
        valid_push_modes = function(value){
            "Access the valid push modes available."
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$valid_push_modes"
            "kc$valid_push_modes <- 'whatever' # Error, read only"
            ""
            "@param value not used"
            "@return A vector representing possible valid Elasticsearch metadata types"

            if(missing(value)) {
                private$.VALID_PUSH_MODES
            } else {
                stop(private$err_active_is_read_only("$valid_push_modes"), call. = FALSE)
            }
        },

        #' @field shard_number Access and modify the number of allocated primary shards when 
        #'  creating an Elasticsearch index.
        shard_number = function(value){
            "Access and modify the number of allocated primary shards when creating an Elasticsearch index."
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$shard_number"
            "kc$shard_number <- 2"
            ""
            "@param value number of primary shards to create"
            "@return The number of primary shards defined for each index at their creation"

            if(missing(value)) {
                private$.es_primary_shards
            } else {
                if(!is.numeric(value)) stop(private$err_param_type_numeric("$shard_number"), call. = FALSE)
                if(value <= 0) stop(private$err_param_positive("$shard_number", zero_valid = FALSE), call. = FALSE)
                private$.es_primary_shards <- value
            }
        },

        #' @field shard_replicas_number Access and modify the number of allocated replicas in an 
        #'  Elasticsearch index.
        shard_replicas_number = function(value){
            "Access and modify the number of allocated replicas in an Elasticsearch index."
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$shard_replicas_number"
            "kc$shard_replicas_number <- 2"
            ""
            "@param value number of replica shards to create"
            "@return The number of replicas defined for each index "

            if(missing(value)) {
                private$.es_replica_shards
            } else {
                if(!is.numeric(value)) stop(private$err_param_type_numeric("$shard_replicas_number"), call. = FALSE)
                if(value < 0) stop(private$err_param_positive("$shard_replicas_number", zero_valid = TRUE), call. = FALSE)
                private$.es_replica_shards <- value
            }
        },

        #' @field default_id_col Access and modify the default ID column/field created when pushing 
        #'  data to Elasticsearch.
        default_id_col = function(value){
            "Access and modify the default ID field created when pushing data to Elasticsearch."
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$default_id_col"
            "kc$default_id_col <- 'kibior_unique_id'"
            ""
            "@param value new name of the unique IDs field"
            "@return the name of the current unique IDs field"

            if(missing(value)) {
                private$.default_id_col
            } else {
                if(!purrr::is_character(value)) stop(private$err_param_type_character("$default_id_col"), call. = FALSE)
                if(length(value) > 1) stop(private$err_one_value("$default_id_col"), call. = FALSE)
                private$.default_id_col <- value
            }
        }

    ),


    # ============================================================================
    # PUBLIC
    # ============================================================================

    public = list(

        # --------------------------------------------------------------------------
        # attributes
        # --------------------------------------------------------------------------

        #' @field verbose verbose mode, prints out more informations during execution
        verbose = FALSE,

        #' @field quiet_progress progressbar quiet mode, toggles progress bar
        quiet_progress = FALSE,

        #' @field quiet_results results quiet mode, toggles results printing
        quiet_results = FALSE,


        # --------------------------------------------------------------------------
        # methods - Class inherits
        # --------------------------------------------------------------------------


        #' @details
        #' Initialize a new object, automatically called when calling `Kibior$new()`
        #'
        #' @param host The target host to connect to Elasticsearch REST API (default: "localhost").
        #' @param port The target port (default: 9200).
        #' @param user If the server needs authentication, your username (default: NULL).
        #' @param pwd If the server needs authentication, your password (default: NULL).
        #' @param verbose The verbose mode (default: FALSE).
        #'
        #' @return a new instance/object of Kibior
        #'
        #' @examples
        #' \dontrun{
        #' # default initiatlization, connect to "localhost:9200"
        #' kc <- Kibior$new()
        #' # connect to "192.168.2.145:9200"
        #' kc <- Kibior$new("192.168.2.145")
        #' # connect to "es:15005", verbose mode activated
        #' kc <- Kibior$new(host = "elasticsearch", port = 15005, verbose = TRUE)
        #' # connect to "192.168.2.145:9450" with credentials "foo:bar"
        #' kc <- Kibior$new(host = "192.168.2.145", port = 9450, user = "foo", pwd = "bar")
        #' # connect to "elasticsearch:9200"
        #' kc <- Kibior$new("elasticsearch")
        #' 
        #' # get kibior var from env (".Renviron" file or local env) 
        #' dd <- system.file("doc_env", "kibior_build.R", package = "kibior")
        #' source(dd, local = TRUE)
        #' kc <- .kibior_get_instance_from_env()
        #' kc$quiet_progress <- TRUE
        #'
        #' # preparing all examples (do not mind this for this method)
        #' delete_if_exists <- function(index_names){
        #'     tryCatch(
        #'         expr = { kc$delete(index_names) },
        #'         error = function(e){  }
        #'     )
        #' }
        #' delete_if_exists(c(
        #'     "aaa", 
        #'     "bbb", 
        #'     "ccc", 
        #'     "ddd", 
        #'     "sw", 
        #'     "sw_naboo", 
        #'     "sw_tatooine", 
        #'     "sw_alderaan", 
        #'     "sw_from_file", 
        #'     "storms",
        #'     "starwars"
        #' ))
        #' }
        #'
        initialize = function(host = "localhost", port = 9200, user = NULL, pwd = NULL, verbose = getOption("verbose")){
            if(purrr::is_null(host)) host <- "localhost"
            if(!is.numeric(port)) stop(private$err_param_type_numeric("port"))
            if(!purrr::is_character(host)) stop(private$err_param_type_character("host"))
            if(!purrr::is_null(user)){
                if(!purrr::is_character(user)) stop(private$err_param_type_character("user"))
                if(length(user) != 1) stop(private$err_one_value("user"))
            }
            if(!purrr::is_null(pwd)){
                if(!purrr::is_character(pwd)) stop(private$err_param_type_character("pwd"))
                if(length(pwd) != 1) stop(private$err_one_value("pwd"))
            }
            if(!purrr::is_logical(verbose)) stop(private$err_param_type_logical("verbose"))
            if(!(verbose %in% c(FALSE, TRUE))) stop(private$err_logical_na("verbose"))
            # cast
            port <- as.integer(port)
            # build attr
            self$verbose <- verbose
            private$.host <- host
            private$.port <- port
            private$.user <- user
            private$.pwd <- pwd
            # try to connect
            private$connect()
        },

        #' @details
        #' Print simple informations of the current object.
        #'
        #' @examples
        #' \dontrun{
        #' print(kc)
        #' }
        #'
        print = function(){
            f <- function(x) if(x) "yes" else "no"
            cat("KibioR client: \n")
            cat("  - host:", private$.host, "\n")
            cat("  - port:", private$.port, "\n")
            if(!purrr::is_null(private$.user)){
                cat("  - username:", private$.user, "\n")
            }
            cat("  - verbose:", f(self$verbose), "\n")
            cat("  - print result:", f(!self$quiet_results), "\n")
            cat("  - print progressbar:", f(!self$quiet_progress), "\n")
        },

        #'
        #' @details
        #' Tells if another instance of Kibior has the same `host:port` couple.
        #'
        #' @param other Another instance/object of Kibior (default: NULL).
        #'
        #' @return TRUE if hosts and ports are identical, else FALSE
        #'
        #' @examples
        #' \dontrun{
        #' kc$eq(kc)
        #' }
        #'
        eq = function(other = NULL){
            if(!Kibior$is_instance(other)) stop(private$err_not_kibior_instance("other"))
            r <- (self$host == other$host && self$port == other$port)
            if(self$quiet_results) invisible(r) else r
        },

        #'
        #' @details
        #' Tells if another instance of Kibior has a different `host:port` couple.
        #'
        #' @param other Another instance/object of Kibior (default: NULL).
        #'
        #' @return TRUE if hosts and ports are differents, else FALSE
        #'
        #' @examples
        #' \dontrun{
        #' kc$ne(kc)
        #' }
        #' 
        ne = function(other = NULL){
            r <- !self$eq(other = other)
            if(self$quiet_results) invisible(r) else r
        },


        # --------------------------------------------------------------------------
        # methods - CRUD index
        # --------------------------------------------------------------------------


        #' @details
        #' Create one or several indices in Elasticsearch.
        #'
        #' @family crud-index
        #'
        #' @param index_name a vector of index names to create (default: NULL).
        #' @param force Erase already existing identical index names? (default: FALSE).
        #'
        #' @return a list containing results of creation per index
        #'
        #' @examples
        #' \dontrun{
        #' kc$create("aaa")
        #' kc$create(c("bbb", "ccc"))
        #' }
        #' 
        create = function(index_name, force = FALSE){
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            if(private$is_search_pattern(index_name)) stop(private$err_search_pattern_forbidden("index_name"))
            if(!purrr::is_logical(force)) stop(private$err_param_type_logical("force"))
            if(is.na(force)) stop(private$err_logical_na("force"))
            #
            res <- list()
            complete <- list()
            f <- if(force) elastic::index_recreate else elastic::index_create
            fnote <- if(force) "Recreating" else "Creating"
            for(i in index_name){
                if(self$verbose) message(" -> ", fnote, " index '", i, "'")
                # build with settings but mapping is defined with data to better match fields type
                body <- list(
                    "settings" = list(
                        "index" = list(
                            "number_of_shards" = private$.es_primary_shards,
                            "number_of_replicas" = private$.es_replica_shards
                        )
                    )
                )
                # execute
                complete[[i]] <- tryCatch(
                    expr = {
                        f(self$connection, index = i, body = body, verbose = FALSE)
                    },
                    error = function(e){
                        if(grepl("already exists", e$message, ignore.case = TRUE)){
                            list(acknowledged = FALSE)
                        } else {
                            stop(e$message)
                        }
                    }
                )
                # control
                if(complete[[i]]$acknowledged){
                    private$force_merge("*")
                    private$optimize(i)
                    res[[i]] <- TRUE
                } else {
                    res[[i]] <- FALSE
                }
            }
            # wait if at least one created
            if(any(unlist(res, use.names = FALSE))){
                if(self$verbose) message(" -> Waiting a bit for Elasticsearch")
                Sys.sleep(self$elastic_wait)
            }
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' List indices in Elasticsearch.
        #'
        #' @family crud-index
        #'
        #' @examples
        #' \dontrun{
        #' kc$list()
        #' kc$list(get_specials = TRUE)
        #' }
        #'
        #' @param get_specials a boolean to get special indices (default: FALSE).
        #'
        #' @return a list of index names, NULL if no index found
        #'
        list = function(get_specials = FALSE){
            r <- names(self$mappings())
            if(!purrr::is_null(r) && !get_specials){
                # remove special indices
                r <- r[!startsWith(r, ".")]
                r <- r[!startsWith(r, "watcher_")]
            }
            if(self$quiet_results) invisible(r) else r
        },

        #' @details
        #' Does Elasticsearch has one or several indices?
        #'
        #' @family crud-index
        #'
        #' @examples
        #' \dontrun{
        #' kc$has("aaa")
        #' kc$has(c("bbb", "ccc"))
        #' }
        #'
        #' @param index_name a vector of index names to check.
        #'
        #' @return a list with TRUE for found index, else FALSE
        #'
        has = function(index_name){
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            if(private$is_search_pattern(index_name)) stop(private$err_search_pattern_forbidden("index_name"))
            # result
            r <- self$list()
            res <- vapply(index_name,
                        FUN = function(x){ x %in% r },
                        FUN.VALUE = logical(1)) %>% 
                    as.list()
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Delete one or several indices in Elasticsearch.
        #'
        #' @family crud-index
        #'
        #' @param index_name a vector of index names to delete.
        #'
        #' @return a list containing results of deletion per index, or NULL if no index name match
        #'
        #' @examples
        #' \dontrun{
        #' kc$delete("aaa")
        #' kc$delete(c("bbb", "ccc"))
        #' }
        #' 
        delete = function(index_name){
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            if("_all" %in% index_name) stop("Cannot delete all indices at once.")
            if("*" %in% index_name) stop("Cannot delete all indices at once.")
            # match index names
            indices <- self$match(index_name)
            if(purrr::is_null(indices)){
                if(self$verbose) message(" -> Deleting no index... well, ok")
                res <- NULL
            } else {
                #
                res <- list()
                complete <- list()
                if(self$verbose) message(" -> Deleting")
                for(i in indices){
                    # if(self$verbose) message(" -> Deleting index '", i, "'... ", appendLF = FALSE)
                    if(self$verbose) message("   - Index '", i, "'... ", appendLF = FALSE)
                    
                    # delete
                    complete[[i]] <- tryCatch(
                        expr = {
                            r <- elastic::index_delete(self$connection, index = i, verbose = FALSE)
                            if(self$verbose) message("ok")
                            r
                        },
                        error = function(e){
                            if(self$verbose) message("nok")
                            if(grepl("no such index", e$message, ignore.case = TRUE)){
                                if(self$verbose) message(" -> Index '", i, "' does not exists")
                                r <- list(acknowledged = FALSE)
                            } else {
                                stop(e$message)
                            }
                            r
                        }
                    )
                    # 
                    if(complete[[i]]$acknowledged){
                        private$force_merge("*")
                        private$optimize(i)
                        res[[i]] <- TRUE
                    } else {
                        res[[i]] <- FALSE
                    }
                }
                # wait if at least one deleted
                if(any(unlist(res, use.names = FALSE))){
                    if(self$verbose) message(" -> Waiting a bit for Elasticsearch")
                    Sys.sleep(self$elastic_wait)
                }
            }
            #
            if(self$quiet_results) invisible(res) else res
        },


        # --------------------------------------------------------------------------
        # methods - indices metadata CRUD
        # --------------------------------------------------------------------------

        #' @details
        #' Add a description of a pushed dataset.
        #'
        #' @family kibior-metadata
        #'
        #' @param index_name the index name to describe
        #' @param dataset_name the full length dataset name
        #' @param source_name the source/website/entity full length name
        #' @param index_description the index description, should be explicit
        #' @param version the version of the source dataset
        #' @param change_log what have been done from the last version
        #' @param website the website to the source dataset website
        #' @param direct_download the direct download url of the dataset source
        #' @param version_date the version or build date
        #' @param license the license attached to this dataset, could be a url
        #' @param contact a mailto/contact
        #' @param references some paper and other references (e.g. doi, url)
        #' @param columns a list of (column_name = column_description) to register (default: list())
        #' @param force if FALSE, raise an error if the description already exists, else TRUE to 
        #'  overwrite (default: FALSE)
        #'
        #' @return the index name if complete, else an error
        #'
        #' @examples
        #' \dontrun{
        #' kc$add_description(
        #'     index_name = "sw", 
        #'     dataset_name = "starwars", 
        #'     source_name = "Package dplyr", 
        #'     index_description = "Description of starwars characters, the data comes from the Star 
        #'      Wars API.", 
        #'     version = "dplyr (1.0.0)", 
        #'     link = "http://swapi.dev/", 
        #'     direct_download_link = "http://swapi.dev/", 
        #'     version_date = "2020-05-28", 
        #'     license_link = "MIT", 
        #'     columns = list(
        #'         "name" = "Name of the character",
        #'         "height" = "Height (cm)",
        #'         "mass" = "Weight (kg)",
        #'         "hair_color" = "Hair colors",
        #'         "skin_color" = "Skin colors",
        #'         "eye_color" = "Eye colors",
        #'         "birth_year" = "Year born (BBY = Before Battle of Yavin)",
        #'         "sex" = "The biological sex of the character, namely male, female, 
        #'              hermaphroditic, or none (as in the case for Droids).",
        #'         "gender" = "The gender role or gender identity of the character as determined by 
        #'              their personality or the way they were progammed (as in the case for Droids
        #'              ).",
        #'         "homeworld" = "Name of homeworld",
        #'         "species" = "Name of species",
        #'         "films" = "List of films the character appeared in",
        #'         "vehicles" = "List of vehicles the character has piloted",
        #'         "starships" = "List of starships the character has piloted"
        #'     )
        #' )
        #' }
        #' 
        add_description = function(index_name, dataset_name, source_name, index_description, version, change_log, website, direct_download, version_date, license, contact, references, columns = list(), force = FALSE){
            # check
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            if(private$is_search_pattern(index_name)) stop(private$err_search_pattern_forbidden("index_name"))
            if(length(index_name) > 1) stop(private$err_one_value("index_name"))
            if(suppressMessages({ purrr::is_null(self$match(index_name)) })) stop("push an index before describing it please")
            if(!purrr::is_character(dataset_name)) stop(private$err_param_type_character("dataset_name"))
            if(length(dataset_name) > 1) stop(private$err_one_value("dataset_name"))
            if(!purrr::is_character(source_name)) stop(private$err_param_type_character("source_name"))
            if(length(source_name) > 1) stop(private$err_one_value("source_name"))
            if(!purrr::is_character(index_description)) stop(private$err_param_type_character("index_description"))
            if(length(index_description) > 1) stop(private$err_one_value("index_description"))
            if(!purrr::is_character(version)) stop(private$err_param_type_character("version"))
            if(length(version) > 1) stop(private$err_one_value("version"))
            if(!purrr::is_character(change_log)) stop(private$err_param_type_character("change_log"))
            if(length(change_log) > 1) stop(private$err_one_value("change_log"))
            if(!purrr::is_character(website)) stop(private$err_param_type_character("website"))
            if(length(website) > 1) stop(private$err_one_value("website"))
            if(!purrr::is_character(direct_download)) stop(private$err_param_type_character("direct_download"))
            if(length(direct_download) > 1) stop(private$err_one_value("direct_download"))
            if(!purrr::is_character(version_date)) stop(private$err_param_type_character("version_date"))
            if(length(version_date) > 1) stop(private$err_one_value("version_date"))
            if(!purrr::is_character(license)) stop(private$err_param_type_character("license"))
            if(length(license) > 1) stop(private$err_one_value("license"))
            if(!purrr::is_character(contact)) stop(private$err_param_type_character("contact"))
            if(!purrr::is_character(references)) stop(private$err_param_type_character("references"))
            if(!purrr::is_logical(force)) stop(private$err_param_type_logical("force"))
            if(is.na(force)) stop(private$err_logical_na("force"))
            # check cols
            if(!purrr::is_list(columns) || private$is_list_empty(columns)){
                stop("argument 'columns' must be a list, with the column name as key and column description as value")
            }
            actual_cols <- suppressMessages({ self$columns(index_name)[[index_name]] }) %>% 
                (function(x) x[x != self$default_id_col])
            if(length(actual_cols) != length(columns)){
                stop("argument 'columns' must have the same number of columns as in its index. It does not take '", self$default_id_col, "' into account.")
            }
            sd <- private$symmetric_difference(actual_cols, names(columns))
            if(!purrr::is_null(sd)){
                sd %>% 
                    private$vector_to_str() %>% 
                    stop("argument 'columns' has some undocumented or unknown column names ", .)
            }
            if(length(unlist(columns, use.names = FALSE)) != length(columns)) stop("argument 'columns' must not accept list type in values")
            has_non_str_type <- columns %>% 
                lapply(typeof) %>% 
                unlist(use.names = FALSE) %>% 
                {. != "character"} %>% 
                any()
            if(has_non_str_type) stop("argument 'columns' must only have character type in values")
            # get actual data
            if(self$verbose) message(" -> Getting actual descriptions")
            data <- suppressMessages({ 
                self$pull(private$.KIBIOR_METADATA_INDICES)[[private$.KIBIOR_METADATA_INDICES]]
            }) 
            if(index_name %in% data$index_name){
                if(!force){
                    stop("index '", index_name ,"' description is already registered. Use 'force' to overwrite.")
                } else {
                    if(self$verbose) message(" -> Cleaning old description of '", index_name, "'")
                    data <- dplyr::filter(data, index_name != index_name)
                }
            }
            #
            if(self$verbose) message(" -> Formatting new description for '", index_name, "'")
            meta_index <- list(
                index_name = index_name, 
                dataset_name = dataset_name, 
                source_name = source_name, 
                index_description = index_description, 
                version = version,
                change_log = change_log,
                website = website, 
                direct_download = direct_download, 
                version_date = version_date, 
                license = license,
                contact = contact,
                references = references
            ) %>% as.data.frame(stringsAsFactors = FALSE)
            #
            new_data <- columns %>% 
                purrr::imap(function(col_desc, col_name){ 
                    list(
                        index_name = index_name,
                        column_name = col_name,
                        column_description = col_desc
                    )
                }) %>% 
                data.table::rbindlist() %>%
                as.data.frame(stringsAsFactors = FALSE) %>% 
                dplyr::right_join(meta_index, by = "index_name")
            # 
            if(self$verbose) message(" -> Pushing new description for '", index_name, "'")
            suppressMessages({
                data %>% 
                    (function(d){
                        if(purrr::is_null(d)){
                            new_data 
                        } else {
                            d %>% 
                                dplyr::select(-c(self$default_id_col)) %>% 
                                rbind(new_data)
                        }
                    }) %>% 
                    self$push(private$.KIBIOR_METADATA_INDICES, mode = "recreate")
            })
            # 
            if(self$quiet_results) invisible(index_name) else index_name
        },

        #' @details
        #' Does the description exists?
        #'
        #' @family kibior-metadata
        #'
        #' @param index_name the index name to describe
        #'
        #' @return a list splitted by index, with TRUE if the description is 
        #'  found, else FALSE. Removes unknown index names.
        #'
        #' @examples
        #' \dontrun{
        #' kc$has_description("s*")
        #' kc$has_description(c("sw", "asdf"))
        #' }
        #' 
        has_description = function(index_name){
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            if(private$is_search_pattern(index_name)) stop(private$err_search_pattern_forbidden("index_name"))
            if(length(index_name) > 1) stop(private$err_one_value("index_name"))
            #
            i <- suppressMessages({ self$match(index_name) })
            if(purrr::is_null(i)) stop("No index found with these names or pattern.")
            n <- private$.KIBIOR_METADATA_INDICES
            tmp <- suppressMessages({ self$keys(n, "index_name") })
            res <- i %>% lapply(function(d){ d %in% tmp })
            names(res) <- i
            if(self$quiet_results) invisible(index_name) else index_name
        },

        #' @details
        #' List indices that do no have descriptions.
        #'
        #' @family kibior-metadata
        #'
        #' @return a vector of indices not present in description.
        #'
        #' @examples
        #' \dontrun{
        #' kc$missing_descriptions()
        #' }
        #' 
        missing_descriptions = function(){
            tmp <- suppressMessages({ self$list() })
            indices <- suppressMessages({ self$keys(private$.KIBIOR_METADATA_INDICES, "index_name") })
            res <- tmp[!tmp %in% indices]
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Remove a description.
        #'
        #' @family kibior-metadata
        #'
        #' @param index_name the index name to describe
        #'
        #' @return a vector of indices not present in description.
        #'
        #' @examples
        #' \dontrun{
        #' # remove the description of 'test' index
        #' kc$remove_description("test")
        #' }
        #' 
        remove_description = function(index_name){
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            if(private$is_search_pattern(index_name)) stop(private$err_search_pattern_forbidden("index_name"))
            if(length(index_name) > 1) stop(private$err_one_value("index_name"))
            #
            n <- private$.KIBIOR_METADATA_INDICES
            is_describe <- suppressMessages({ self$keys(n, "index_name") }) %>% { index_name %in% . }
            if(!is_describe){
                stop("No description found for index '", index_name, "'.")
            }
            tmp <- suppressMessages({ self$pull(n)[[n]] }) %>% 
                dplyr::filter(index_name != !!index_name)
            res <- tryCatch(
                expr = {
                    tmp %>% dplyr::select(-c(self$default_id_col))
                }, 
                error = function(e){
                    message(e)
                    tmp
                }
            )
            suppressMessages({ self$push(res, n, mode = "recreate") })
            if(self$quiet_results) invisible(index_name) else index_name
        },


        #' @details
        #' Remove all descriptions that do not have a index associated.
        #'
        #' @family kibior-metadata
        #'
        #' @return a list of index names which have been removed from descriptions.
        #'
        #' @examples
        #' \dontrun{
        #' # remove the description of 'test' index
        #' kc$clean_descriptions()
        #' }
        #' 
        clean_descriptions = function(){
            # get all description entries that do not have index associated
            tmp <- suppressMessages({ self$keys(private$.KIBIOR_METADATA_INDICES, "index_name") })
            description_names <- suppressMessages({ tmp[!tmp %in% self$list()] })
            if(length(description_names) == 0){
                if(self$verbose) message("No obsolete description found.")
                res <- NULL
            } else {
                if(self$verbose) message("Removing obsolete index descriptions: ", private$vector_to_str(description_names))
                res <- description_names %>% 
                    lapply(self$remove_description) %>% 
                    unlist(use.names = FALSE)
            }
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Get the description of indices and their columns.
        #'
        #' @family kibior-metadata
        #'
        #' @param index_name the index name to describe
        #' @param columns a vector of column names to describe (default: NULL)
        #' @param pretty pretty-print the result (default: FALSE)
        #'
        #' @return all description, grouped by indices
        #'
        #' @examples
        #' \dontrun{
        #' kc$describe("s*")
        #' kc$describe("sw", columns = c("name", "height"))
        #' }
        #' 
        describe = function(index_name, columns = NULL, pretty = FALSE){
            # check
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            if(!purrr::is_null(columns) && !purrr::is_character(columns)){
                stop(private$err_param_type_character("columns", can_be_null = TRUE))
            }
            if(!purrr::is_logical(pretty)) stop(private$err_param_type_logical("pretty"))
            if(is.na(pretty)) stop(private$err_logical_na("pretty"))
            # check if there is any description
            m <- suppressMessages({ self$match(index_name) })
            if(purrr::is_null(m)){
                if(self$verbose) message(" -> No description found for '", index_name,"'")
                res <- NULL
            } else {
                # query
                n <- private$.KIBIOR_METADATA_INDICES
                q <- paste0("index_name:", index_name)
                if(!purrr::is_null(columns)){
                    q <- columns %>% 
                        paste0(collapse = " || ") %>%
                        paste0(q, " && column_name:(", ., ")")
                }
                if(self$verbose) message(" -> Getting actual description for '", index_name, "'")
                tmp <- suppressMessages({ self$pull(n, query = q)[[n]] })
                if(purrr::is_null(tmp)){
                    res <- NULL
                } else {
                    tmp <- tryCatch(
                        expr = { 
                            tmp %>% dplyr::select(-c(self$default_id_col))
                        }, 
                        error = function(e){
                            if(grepl("doesn't handle lists", e$message, ignore.case = TRUE)){
                                if(self$verbose) message(" -> Skipping. No column name matching.")
                                NULL
                            } else {
                                stop(e$message)
                            }
                        }
                    )
                    if(purrr::is_null(tmp)){
                        res <- NULL
                    } else {
                        indices <- unique(tmp$index_name)
                        res <- indices %>% 
                            lapply(function(n){
                                tmp %>%
                                    dplyr::filter(index_name == n) %>%
                                    dplyr::select(-c("index_name"))
                            })
                        names(res) <- indices
                    }
                }
            }
            # pretty print
            if(pretty && !purrr::is_null(res)){
                pretty_format <- function(d,i){
                    res <- paste0("\nIndex '", i, "':\n") %>%
                        paste0("  - dataset name:       ", unique(d$dataset_name), "\n") %>% 
                        paste0("  - source name:        ", unique(d$source_name), "\n") %>% 
                        paste0("  - version:            ", unique(d$version), "\n") %>% 
                        paste0("  - website:            ", unique(d$website), "\n") %>% 
                        paste0("  - direct download:    ", unique(d$direct_download), "\n") %>% 
                        paste0("  - version date:       ", unique(d$version_date), "\n") %>% 
                        paste0("  - license:            ", unique(d$license), "\n") %>% 
                        paste0("  - contact:            ", unique(d$contact), "\n") %>% 
                        paste0("  - index description:  ", unique(d$index_description), "\n") %>% 
                        paste0("  - change log:         ", unique(d$change_log), "\n")
                    # ref
                    references <- d$references %>% 
                        unique() %>%
                        lapply(function(x) paste0("    - ", x)) %>% 
                        paste0(collapse = "\n") %>% 
                        paste0("  - references:\n", ., "\n")
                    # cols
                    columns <- d %>% 
                        dplyr::select(c("column_name", "column_description")) %>% 
                        unique() %>% 
                        apply(1, function(x) paste0("    - ", x[1], ": ", x[2])) %>% 
                        paste0(collapse = "\n") %>% 
                        paste0("  - columns:\n", ., "\n")
                    # combine
                    res %>% 
                        paste0(references) %>% 
                        paste0(columns)
                }
                # print  
                res %>% 
                    purrr::imap(pretty_format) %>% 
                    paste0(collapse = "\n\n") %>% 
                    cat()
                res <- NULL
            }
            res
        },

        #' @details
        #' Get the description text of indices.
        #'
        #' @family kibior-metadata
        #'
        #' @param index_name the index name to describe
        #'
        #' @return a list of description text, grouped by indices
        #'
        #' @examples
        #' \dontrun{
        #' kc$describe_index("s*")
        #' }
        #' 
        describe_index = function(index_name){
            res <- self$describe(index_name)
            if(!purrr::is_null(res)){
                res <- res %>% 
                    purrr::imap(function(d,i) d$index_description) %>%
                    purrr::imap(function(d,i) unique(d))
            }
            res
        },

        #' @details
        #' Get the description text of index columns.
        #'
        #' @family kibior-metadata
        #'
        #' @param index_name the index name to describe
        #' @param columns a vector of column names to describe
        #'
        #' @return a list of description text, grouped by indices
        #'
        #' @examples
        #' \dontrun{
        #' kc$describe_columns("s*", c("name", "height"))
        #' }
        #' 
        describe_columns = function(index_name, columns){
            res <- self$describe(index_name, columns = columns)
            if(!purrr::is_null(res)){
                res <- res %>% 
                    purrr::imap(function(d,i) dplyr::select(d, c("column_name", "column_description"))) %>% 
                    purrr::imap(function(d,i) unique(d))
            }
            res
        },


        # --------------------------------------------------------------------------
        # methods - cluster wealth
        # --------------------------------------------------------------------------


        #' @details
        #' Get informations about Elasticsearch cluster
        #'
        #' @family cluster-wealth
        #'
        #' @examples
        #' \dontrun{
        #' kc$infos()
        #' }
        #'
        #' @return a list of statistics about the cluster
        #'
        infos = function(){
            suppressMessages({ 
                r <- elastic::cluster_stats(self$connection) 
            })
            if(self$quiet_results) invisible(r) else r
        },

        #' @details
        #' Ping cluster connection
        #'
        #' @family cluster-wealth
        #'
        #' @examples
        #' \dontrun{
        #' kc$ping()
        #' }
        #'
        #' @return the ping result with some basic infos
        #'
        ping = function(){
            r <- self$connection$ping()
            if(self$quiet_results) invisible(r) else r
        },

        # --------------------------------------------------------------------------
        # methods - CRUD metadata
        # --------------------------------------------------------------------------

        #' @details
        #' Get mappings of indices
        #'
        #' @family crud-metadata
        #'
        #' @examples
        #' \dontrun{
        #' kc$mappings()
        #' kc$mappings("sw")
        #' kc$mappings(c("sw", "sw_naboo"))
        #' }
        #'
        #' @param index_name a vector of index names to get mappings.
        #'
        #' @return the list of indices, containing their mapping
        #'
        mappings = function(index_name){
            if(missing(index_name)) index_name <- "*"
            res <- private$metadata_type("mappings", index_name)
            if(!purrr::is_null(res)){
                # remove type call
                if(self$version$major < 7){
                    res <- res %>% lapply(function(x){ 
                        if(purrr::is_null(x) || length(x) == 0) x else x[[1]] 
                    })
                }
            }
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Get settings of indices
        #'
        #' @family crud-metadata
        #'
        #' @examples
        #' \dontrun{
        #' kc$settings()
        #' kc$settings("sw")
        #' kc$settings(c("sw", "sw_tatooine"))
        #' }
        #'
        #' @param index_name a vector of index names to get settings.
        #'
        #' @return the list of indices, containing their settings
        #'
        settings = function(index_name){
            if(missing(index_name)) index_name <- NULL
            res <- private$metadata_type("settings", index_name)
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Get aliases of indices
        #'
        #' @family crud-metadata
        #'
        #' @examples
        #' \dontrun{
        #' kc$aliases()
        #' kc$aliases("sw")
        #' kc$aliases(c("sw", "sw_alderaan"))
        #' }
        #'
        #' @param index_name a vector of index names to get aliases.
        #'
        #' @return the list of indices, containing their aliases
        #'
        aliases = function(index_name){ 
            if(missing(index_name)) index_name <- NULL
            res <- private$metadata_type("aliases", index_name)
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Shortcut to `$count()` to match the classical `dim()` function pattern `[line col]`
        #'
        #' @family crud-metadata
        #'
        #' @examples
        #' \dontrun{
        #' # Couple [<nb obs> <nb var>] in "sw"
        #' kc$dim("sw")
        #' # Couple [<nb obs> <nb var>] in indices "sw_naboo" and "sw_alderaan"
        #' kc$dim(c("sw_naboo", "sw_alderaan"))
        #' }
        #'
        #' @param index_name a vector of index names to get aliases.
        #'
        #' @return the list of indices, containing their number of observations and variables.
        #'
        dim = function(index_name){
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            # 
            involved_indices <- self$match(index_name)
            #
            res <- involved_indices %>%
                lapply(function(x){ 
                    c(
                        suppressMessages(self$count(x, type = "observations")[[x]]),
                        suppressMessages(self$count(x, type = "variables")[[x]])
                    )
                }) %>% 
                setNames(involved_indices)
            #
            if(length(res) == 0) res <- NULL
            # 
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Get fields/columns of indices.
        #'
        #' @family crud-metadata
        #'
        #' @examples
        #' \dontrun{
        #' kc$columns("sw")          # direct search
        #' kc$columns("sw_*")        # pattern search
        #' }
        #'
        #' @param index_name a vector of index names, can be a pattern.
        #'
        #' @return a list of indices, each containing their fields/columns.
        #'
        columns = function(index_name){
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            # 
            involved_indices <- self$match(index_name)
            #
            res <- NULL
            if(!purrr::is_null(involved_indices)){
                selected_mapping <- self$mappings(index_name)
                # 
                get_property_names <- function(index_name){
                    # try index name
                    r <- selected_mapping[[index_name]]$properties
                    # if main selected_mapping is empty, maybe in "_doc"
                    if(purrr::is_null(r)){ 
                        r <- selected_mapping[["_doc"]]$properties
                    }
                    names(r)
                }
                # 
                res <- involved_indices %>% 
                    lapply(get_property_names) %>% 
                    setNames(involved_indices)
            }
            #
            if(self$quiet_results) invisible(res) else res
        },


        # --------------------------------------------------------------------------
        # methods - Stats
        # --------------------------------------------------------------------------


        #' @details
        #' Count observations or variables in Elasticsearch data
        #'
        #' @family stats
        #'
        #' @examples
        #' \dontrun{
        #' # Number of observations (nb of records) in "sw"
        #' kc$count("sw")
        #' # Number of observations in indices "sw_naboo" and "sw_tatooine"
        #' kc$count(c("sw_naboo", "sw_tatooine"))
        #' # Number of variables (nb of columns) in index "sw_naboo"
        #' kc$count("sw_naboo", type = "variables")
        #' }
        #'
        #' @param index_name a vector of index names to get aliases.
        #' @param type a string representing the type to count: "observations" (lines) or 
        #'  "variables" (columns) (default: "observations").
        #' @param query a string as a query string syntax (default: NULL).
        #'
        #' @return the list of indices, containing their number of observations or variables. 
        #'  Use `$dim()` for both
        #'
        count = function(index_name, type = "observations", query = NULL){
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            if(length(type) > 1) stop(private$err_one_value("type"))
            if(!(type %in% private$.VALID_COUNT_TYPES)) stop(private$err_not_in_vector("Count type", private$.VALID_COUNT_TYPES))
            if(!purrr::is_null(query)){
                if(!purrr::is_character(query)) stop(private$err_param_type_character("query"))
                if(length(query) > 1) stop(private$err_one_value("query"))
            }
            #
            involved_indices <- self$match(index_name)
            # 
            res <- list()
            if(!purrr::is_null(involved_indices)){
                for(i in involved_indices){
                    res[[i]] <- switch(type,
                        "observations" = {
                            elastic::count(self$connection, index = i, q = query)
                        },
                        "variables" = {
                            suppressMessages(self$columns(i)[[i]]) %>% length()
                        },
                        stop(private$ERR_WTF, " Found type: ", type)
                    )
                }
            }
            if(length(res) == 0){
                res <- NULL
            }
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Get the average of numeric columns.
        #' 
        #' @family stats
        #'
        #' @examples
        #' \dontrun{
        #' # Avg of "sw" column "height"
        #' kc$avg("sw", "height")
        #' # if pattern
        #' kc$avg("s*", "height")
        #' # multiple indices, multiple columns
        #' kc$avg(c("sw", "sw2"), c("height", "mass"), query = "homeworld:naboo")
        #' }
        #'
        #' @param index_name a vector of index names.
        #' @param columns a vector of column names.
        #' @param query a string as a query string syntax (default: NULL).
        #'
        #' @return a tibble with avg, one line by matching index and column.
        #'
        avg = function(index_name, columns, query = NULL){
            private$single_value_metric_aggregation("avg", index_name, columns, query = query)
        },

        #' @details
        #' Get the mean of numeric columns.
        #' 
        #' @family stats
        #'
        #' @examples
        #' \dontrun{
        #' # mean of "sw" column "height"
        #' kc$mean("sw", "height")
        #' # if pattern
        #' kc$mean("s*", "height")
        #' # multiple indices, multiple columns
        #' kc$mean(c("sw", "sw2"), c("height", "mass"), query = "homeworld:naboo")
        #' }
        #'
        #' @param index_name a vector of index names.
        #' @param columns a vector of column names.
        #' @param query a string as a query string syntax (default: NULL).
        #'
        #' @return a tibble with mean, one line by matching index and column.
        #'
        mean = function(index_name, columns, query = NULL){
            args <- list(index_name, columns, query = query)
            mini <- do.call(self$min, args)
            maxi <- do.call(self$max, args)
            # for each index
            purrr::imap(mini, function(x,y){
                # add maxi to mini
                tmp <- dplyr::inner_join(x, maxi[[y]], by = c("column"))
                # compute mean
                tmp$mean <- ((tmp$min + tmp$max)/2)
                tmp[c("column", "mean")]
            })
        },

        #' @details
        #' Get the minimum of numeric columns.
        #' 
        #' @family stats
        #'
        #' @examples
        #' \dontrun{
        #' # min of "sw" column "height"
        #' kc$min("sw", "height")
        #' # if pattern
        #' kc$min("s*", "height")
        #' # multiple indices, multiple columns
        #' kc$min(c("sw", "sw2"), c("height", "mass"), query = "homeworld:naboo")
        #' }
        #'
        #' @param index_name a vector of index names.
        #' @param columns a vector of column names.
        #' @param query a string as a query string syntax (default: NULL).
        #'
        #' @return a tibble with min, one line by matching index and column.
        #'
        min = function(index_name, columns, query = NULL){
            private$single_value_metric_aggregation("min", index_name, columns, query = query)
        },

        #' @details
        #' Get the maximum of numeric columns.
        #'
        #' @family stats
        #'
        #' @examples
        #' \dontrun{
        #' # max of "sw" column "height"
        #' kc$max("sw", "height")
        #' # if pattern
        #' kc$max("s*", "height")
        #' # multiple indices, multiple columns
        #' kc$max(c("sw", "sw2"), c("height", "mass"), query = "homeworld:naboo")
        #' }
        #'
        #' @param index_name a vector of index names.
        #' @param columns a vector of column names.
        #' @param query a string as a query string syntax (default: NULL).
        #'
        #' @return a tibble with max, one line by matching index and column.
        #'
        max = function(index_name, columns, query = NULL){
            private$single_value_metric_aggregation("max", index_name, columns, query = query)
        },

        #' @details
        #' Get the sum of numeric columns.
        #'
        #' @family stats
        #'
        #' @examples
        #' \dontrun{
        #' # sum of "sw" column "height"
        #' kc$sum("sw", "height")
        #' # if pattern
        #' kc$sum("s*", "height")
        #' # multiple indices, multiple columns
        #' kc$sum(c("sw", "sw2"), c("height", "mass"), query = "homeworld:naboo")
        #' }
        #'
        #' @param index_name a vector of index names.
        #' @param columns a vector of column names.
        #' @param query a string as a query string syntax (default: NULL).
        #'
        #' @return a tibble with sum, one line by matching index and column.
        #'
        sum = function(index_name, columns, query = NULL){
            private$single_value_metric_aggregation("sum", index_name, columns, query = query)
        },

        #' @details
        #' Produces descriptive statistics of a column.
        #' Returns a tibble composed of: count, min, max, avg, sum, 
        #'  sum_of_squares, variance, std_deviation (+ upper and lower bounds).
        #' Multiple warnings here. One for the count and one for the std_dev.
        #' 1/ Counts: they are approximate, see vignette.
        #' 2/ Std dev: as stated in ES documentation: "The standard deviation 
        #' and its bounds are displayed by default, but they are not always 
        #' applicable to all data-sets. Your data must be normally distributed 
        #' for the metrics to make sense. The statistics behind standard 
        #' deviations assumes normally distributed data, so if your data is 
        #' skewed heavily left or right, the value returned will be misleading."
        #'
        #' @family stats
        #'
        #' @examples
        #' \dontrun{
        #' # Stats of "sw" column "height"
        #' kc$stats("sw", "height")
        #' # if pattern
        #' kc$stats("s*", "height")
        #' # multiple indices and sigma definition
        #' kc$stats(c("sw", "sw2"), "height", sigma = 2.5)
        #' # multiple indices, multiple columns
        #' kc$stats(c("sw", "sw2"), c("height", "mass"), query = "homeworld:naboo")
        #' }
        #'
        #' @param index_name a vector of index names.
        #' @param columns a vector of column names.
        #' @param sigma  (default: NULL).
        #' @param query a string as a query string syntax (default: NULL).
        #'
        #' @return a tibble with descriptive stats, one line by matching index.
        #'
        #' @seealso you should use \code{\link{count}} for more accurate count.
        #'
        stats = function(index_name, columns, sigma = NULL, query = NULL){
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            if(!purrr::is_character(columns)) stop(private$err_param_type_character("columns"))
            if(!purrr::is_null(sigma)){
                if(length(sigma) > 1) stop(private$err_one_value("sigma"))
                if(!is.numeric(sigma)) stop(private$err_param_type_numeric("sigma"))
                if(sigma <= 0) stop(private$err_param_positive("sigma", zero_valid = FALSE))
                if(self$verbose) message(" -> Using sigma: ", sigma)
            }
            if(!purrr::is_null(query)){
                if(!purrr::is_character(query)) stop(private$err_param_type_character("query"))
                if(length(query) > 1) stop(private$err_one_value("query"))
            }
            #
            test_function <- function(e){ e$count == 0 }
            supp_args <- if(purrr::is_null(sigma)) "" else paste0(',"sigma":', sigma)
            #
            private$multi_value_metric_aggregation("extended_stats", index_name, columns, 
                                                    function_test_null_result = test_function, 
                                                    aggregation_supplementary_args = supp_args, 
                                                    query = query)
        },

        #' @details
        #' Get percentiles of numeric columns.
        #'
        #' @family stats
        #'
        #' @examples
        #' \dontrun{
        #' # percentiles of "sw" column "height", default is with q1, q2 and q3
        #' kc$percentiles("sw", "height")
        #' # if pattern
        #' kc$percentiles("s*", "height")
        #' # defining percents to get
        #' kc$percentiles("s*", "height", percents = c(20, 25))
        #' # multiple indices, multiple columns
        #' kc$percentiles(c("sw", "sw2"), c("height", "mass"), query = "homeworld:naboo")
        #' }
        #'
        #' @param index_name a vector of index names.
        #' @param columns a vector of column names.
        #' @param percents a numeric vector of pecents to use (default: NULL).
        #' @param query a string as a query string syntax (default: NULL).
        #'
        #' @return a list of tibble, splitted by indices with percentiles inside tibble columns.
        #'
        # https://www.elastic.co/guide/en/elasticsearch/reference/6.8/search-aggregations-metrics-percentile-aggregation.html#search-aggregations-metrics-percentile-aggregation-approximation
        percentiles = function(index_name, columns, percents = NULL, query = NULL){
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            if(!purrr::is_character(columns)) stop(private$err_param_type_character("columns"))
            if(!purrr::is_null(percents)){
                if(!is.numeric(percents)) stop(private$err_param_type_numeric("percents"))
                if(any(percents <= 0)) stop(private$err_param_positive("percents", zero_valid = FALSE))
                if(any(percents > 100)) stop("percents cannot be > 100")
            } else {
                percents <- c(25, 50, 75)
            }
            if(self$verbose){ 
                percents %>% 
                    private$vector_to_str() %>% 
                    message(" -> Using percentiles: ", .)
            }
            if(!purrr::is_null(query)){
                if(!purrr::is_character(query)) stop(private$err_param_type_character("query"))
                if(length(query) > 1) stop(private$err_one_value("query"))
            }
            #
            test_function <- function(e){
                e %>% 
                    lapply(function(m) purrr::is_null(m)) %>% 
                    unlist(use.names = FALSE) %>% 
                    any()
            }
            supp_args <- percents %>% paste0(collapse = ",") %>% paste0(',"percents":[', ., ']')
            #
            private$multi_value_metric_aggregation("percentiles", index_name, columns, 
                                                    function_test_null_result = test_function, 
                                                    aggregation_supplementary_args = supp_args, 
                                                    query = query)
        },

        #' @details
        #' Get Q1 percentiles from numeric columns.
        #'
        #' @family stats
        #'
        #' @examples
        #' \dontrun{
        #' # Q1 of "sw" column "height"
        #' kc$q1("sw", "height")
        #' # if pattern
        #' kc$q1("s*", "height")
        #' # multiple indices, multiple columns
        #' kc$q1(c("sw", "sw2"), c("height", "mass"), query = "homeworld:naboo")
        #' }
        #'
        #' @param index_name a vector of index names.
        #' @param columns a vector of column names.
        #' @param query a string as a query string syntax (default: NULL).
        #'
        #' @return a list of tibble, splitted by indices with Q1 inside tibble columns.
        #'
        q1 = function(index_name, columns, query = NULL){
            self$percentiles(index_name, columns, percents = 25, query = query)
        },

        #' @details
        #' Get Q2 percentiles from numeric columns.
        #'
        #' @family stats
        #'
        #' @examples
        #' \dontrun{
        #' # Q2 of "sw" column "height"
        #' kc$q2("sw", "height")
        #' # if pattern
        #' kc$q2("s*", "height")
        #' # multiple indices, multiple columns
        #' kc$q2(c("sw", "sw2"), c("height", "mass"), query = "homeworld:naboo")
        #' }
        #'
        #' @param index_name a vector of index names.
        #' @param columns a vector of column names.
        #' @param query a string as a query string syntax (default: NULL).
        #'
        #' @return a list of tibble, splitted by indices with Q2 inside tibble columns.
        #'
        q2 = function(index_name, columns, query = NULL){
            self$percentiles(index_name, columns, percents = 50, query = query)
        },

        #' @details
        #' Get median from numeric columns.
        #' Basically a wrapper around `$q2()`.
        #'
        #' @family stats
        #'
        #' @examples
        #' \dontrun{
        #' # median of "sw" column "height"
        #' kc$median("sw", "height")
        #' # if pattern
        #' kc$median("s*", "height")
        #' # multiple indices, multiple columns
        #' kc$median(c("sw", "sw2"), c("height", "mass"), query = "homeworld:naboo")
        #' }
        #'
        #' @param index_name a vector of index names.
        #' @param columns a vector of column names.
        #' @param query a string as a query string syntax (default: NULL).
        #'
        #' @return a list of tibble, splitted by indices with median inside tibble columns.
        #'
        median = function(index_name, columns, query = NULL){
            self$q2(index_name, columns, query = query)
        },

        #' @details
        #' Get Q3 percentiles from numeric columns.
        #'
        #' @family stats
        #'
        #' @examples
        #' \dontrun{
        #' # Q3 of "sw" column "height"
        #' kc$q3("sw", "height")
        #' # if pattern
        #' kc$q3("s*", "height")
        #' # multiple indices, multiple columns
        #' kc$q3(c("sw", "sw2"), c("height", "mass"), query = "homeworld:naboo")
        #' }
        #'
        #' @param index_name a vector of index names.
        #' @param columns a vector of column names.
        #' @param query a string as a query string syntax (default: NULL).
        #'
        #' @return a list of tibble, splitted by indices with Q3 inside tibble columns.
        #'
        q3 = function(index_name, columns, query = NULL){
            self$percentiles(index_name, columns, percents = 75, query = query)
        },

        #' @details
        #' Summary for numeric columns.
        #' Cumulates `$min()`, `$max()`, `$q1()`, `$q2()`, `$q3()`.
        #'
        #' @family stats
        #'
        #' @examples
        #' \dontrun{
        #' # summary of "sw" column "height"
        #' kc$summary("sw", "height")
        #' # if pattern
        #' kc$summary("s*", "height")
        #' # multiple indices, multiple columns
        #' kc$summary(c("sw", "sw2"), c("height", "mass"), query = "homeworld:naboo")
        #' }
        #'
        #' @param index_name a vector of index names.
        #' @param columns a vector of column names.
        #' @param query a string as a query string syntax (default: NULL).
        #'
        #' @return a list of tibble, splitted by indices.
        #'
        summary = function(index_name, columns, query = NULL){
            if(!purrr::is_null(query)){
                if(!purrr::is_character(query)) stop(private$err_param_type_character("query"))
                if(length(query) > 1) stop(private$err_one_value("query"))
            }
            if(all(self$version$major >= 7, self$version$minor >= 8)){
                if(self$verbose) message(" -> ES >= 7.8.0, boxplot endpoint available")
                # use boxplot endpoint
                # https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-metrics-boxplot-aggregation.html#search-aggregations-metrics-boxplot-aggregation-approximation
                #
                test_function <- function(e){
                    e %>% 
                        (function(m) purrr::is_null(m$min)) %>% 
                        unlist(use.names = FALSE) %>% 
                        any()
                }
                #
                private$multi_value_metric_aggregation("boxplot", index_name, columns, 
                                                        function_test_null_result = test_function,
                                                        query = query)
            } else {
                # classical way
                if(self$verbose) message(" -> ES < 7.8.0, boxplot endpoint not available")
                args <- list(index_name, columns, query = query)
                chain <- function(ldf, func){
                    fres <- suppressMessages({ do.call(func, args) })
                    purrr::imap(ldf, function(x,y){
                        dplyr::inner_join(x, fres[[y]], by = c("column"))
                    })
                }
                do.call(self$min, args) %>% 
                    chain(self$max) %>%
                    chain(self$q1) %>%
                    chain(self$q2) %>%
                    chain(self$q3)
            }
        },


        # --------------------------------------------------------------------------
        # methods - Data manipulation
        # --------------------------------------------------------------------------


        #' @details
        #' Get distinct keys elements of a specific column.
        #'
        #' @family data-manipulation
        #'
        #' @examples
        #' \dontrun{
        #' kc$keys("sw", "name")
        #' kc$keys("sw", "eye_color")
        #' }
        #'
        #' @param index_name an index name.
        #' @param column a field name of this index (default: NULL).
        #' @param max_size the maximum result to return (default: 1000).
        #'
        #' @return a vector of keys values from this field/column
        #'
        keys = function(index_name, column, max_size = 1000){
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            if(length(index_name) > 1) stop(private$err_one_value("index_name"))
            if(private$is_search_pattern(index_name)) stop(private$err_search_pattern_forbidden("index_name"))
            if(!purrr::is_character(column)) stop(private$err_param_type_character("column"))
            if(length(column) > 1) stop(private$err_one_value("column"))
            if(!(column %in% self$columns(index_name)[[index_name]])) stop(private$err_field_unknown(index_name, column))
            if(!is.numeric(max_size)) stop(private$err_param_type_numeric("max_size"))
            if(max_size < 0) stop(private$err_param_positive("max_size"))
            # 
            get_body <- function(field){
                paste0('{', 
                    '"size": 0,',
                    '"aggs":{',
                        '"kaggs":{',
                            '"composite":{',
                                '"size": ', max_size, ',',
                                '"sources":[{',
                                    '"kaggs":{',
                                        '"terms":{',
                                            '"field": "', field, '"',
                                        '}',
                                    '}',
                                '}]',
                            '}',
                        '}',
                    '}',
                '}')
            }
            # warn
            if(self$verbose) message(" -> Maximum size asked: ", max_size)
            # get
            buckets <- tryCatch(
                expr = {
                    if(self$verbose) message(" -> Looking for '", column, "' column keys... ", appendLF = FALSE)
                    r <- elastic::Search(
                        self$connection, 
                        index = index_name, 
                        size = 0, 
                        body = get_body(column)
                    )
                    if(self$verbose) message("ok")
                    r
                },
                error = function(e){ 
                    if(self$verbose) message("nok")
                    if(e$message == "400 - all shards failed"){
                        tryCatch(
                            expr = {
                                new_col <- paste0(column, ".keyword")
                                if(self$verbose) message(" -> Looking for '", new_col, "' column keys... ", appendLF = FALSE)
                                r <- elastic::Search(
                                    self$connection, 
                                    index = index_name, 
                                    size = 0, 
                                    body = get_body(new_col)
                                )
                                if(self$verbose) message("ok")
                                r
                            }, error = function(ee){
                                stop("Elasticsearch - ", ee)
                            }
                        )
                    } else {
                        stop("Elasticsearch - ", e)
                    }
                }
            )
            # get columns
            res <- buckets$aggregations$kaggs$buckets %>% 
                lapply(function(x){ x$key }) %>% 
                unlist(use.names = FALSE)
            # 
            if(self$quiet_results) invisible(res) else res
        },


        # --------------------------------------------------------------------------
        # methods - Data transformation
        # --------------------------------------------------------------------------


        #' @details
        #' Transformation function for collapsing the BAM list of lists format 
        #'  into a single list as per the Rsamtools vignette
        #'
        #' @family data-manipulation
        #'
        #' @examples
        #' \dontrun{
        #' dd_bai <- system.file("extdata", "test.bam.bai", package = "kibior")
        #' bam_param <- Rsamtools::ScanBamParam(what = c("pos", "qwidth"))
        #' bam_data <- Rsamtools::scanBam(dd_bai, param = bam_param)
        #' kc$bam_to_tibble(bam_data)
        #' }
        #'
        #' @param bam_data data from a BAM file (default: NULL).
        #'
        #' @return a tibble of BAM data
        #'
        bam_to_tibble = function(bam_data = NULL){
            .unlist <- function(x){
                # do.call(c, ...) coerces factor to integer, which is undesired
                x1 <- x[[1L]]
                if (is.factor(x1)){
                    structure(unlist(x), class = "factor", levels = levels(x1))
                } else {
                    do.call(c, x)
                }
            }
            # store names of BAM fields
            bam_field <- names(bam_data[[1]])
            # go through each BAM field and unlist and store as data frame
            bam_df <- lapply(bam_field, function(y){ 
                    .unlist(lapply(bam_data, "[[", y))
                }) %>% 
                do.call(data.frame, .)
            names(bam_df) <- bam_field
            tibble::as_tibble(bam_df, .name_repair = "unique")
        },

        #' @details
        #' Casting function that tries to cast a transformation closure. Uses 
        #'  tibble::as_tibble() by default.
        #'
        #' @family data-manipulation
        #'
        #' @examples
        #' \dontrun{
        #' kc$soft_cast(datasets::iris)
        #' }
        #'
        #' @param data data to cast.
        #' @param caster the caster closure/function (default: tibble::as_tibble)
        #' @param caster_args others caster args (default: list(.name_repair = "unique"))
        #' @param warn do print warning if error? (default: TRUE)
        #'
        #' @return a cast or the unchanged data.
        #'
        soft_cast = function(data, caster = getFromNamespace("as_tibble", "tibble"), caster_args = list(.name_repair = "unique"), warn = TRUE){
            if(!("closure" %in% typeof(caster))){
                stop("Transformation function need to be a closure/function")
            }
            # 
            tryCatch(
                expr = {
                    do.call(caster, list(x = data, caster_args))
                }, 
                error = function(e){
                    if(warn) message("Cannot cast, no changes applied")
                    if(self$verbose) message(e, "\n")
                    data
                }
            )
        },

        #' @details
        #' Get a local filepath or an URL data through a tempfile. If the file
        #'  exists locally, the filepath will be returned, if not, it will tries
        #'  to download the data and return the temp filepath.
        #'
        #' @family data-manipulation
        #'
        #' @examples
        #' \dontrun{
        #' kc$get_resource(system.file("R", "kibior.R", package = "kibior"))
        #' kc$get_resource("https://ftp.ncbi.nlm.nih.gov/entrez/README")
        #' }
        #'
        #' @param url_or_filepath a filepath or an URL.
        #' @param fileext the file extension (default: NULL).
        #'
        #' @return a filepath.
        #'
        get_resource = function(url_or_filepath, fileext = NULL){
            if(!purrr::is_null(fileext)){
                if(!purrr::is_character(fileext)) stop(private$err_param_type_character("fileext"))
                if(length(fileext) > 1) stop(private$err_one_value("fileext"))
            } 
            if(file.exists(url_or_filepath)){
                # file exists
                url_or_filepath
            } else {
                # not local
                if(self$verbose) message("Filepath does not exist locally, trying download...")
                tryCatch(
                    expr = {
                        f <- tempfile(fileext = fileext)
                        download.file(url_or_filepath, f)
                        f
                    },
                    error = function(e){
                        message(e)
                    },
                    warning = function(w){
                        message(w)
                    }
                )
            }
        },


        # --------------------------------------------------------------------------
        # methods - Move data
        # --------------------------------------------------------------------------


        #' @details
        #' Export data to a file.
        #' Needs 'rio' package from CRAN.
        #' Some data formats are not installed by default.
        #' Use `rio::install_formats()` to be able to parse them.
        #'
        #' @family move-data
        #'
        #' @examples
        #' \dontrun{
        #' f <- tempfile(fileext=".csv")
        #' # export and overwrite last file with the same data from Elasticsearch
        #' kc$export(data = "sw", filepath = f)
        #' # export from in-memory data to a file
        #' kc$export(data = dplyr::starwars, filepath = f, force = TRUE)
        #' }
        #'
        #' @param data an index name or in-memory data to be extracted to a file.
        #' @param filepath the filepath to use as export, must contain the file extention.
        #' @param format the file format to use (default: "csv").
        #' @param force overwrite the file? (default: FALSE).
        #'
        #' @return the filepath if correctly exported, else an error
        #'
        export = function(data, filepath, format = "csv", force = FALSE){
            if(purrr::is_null(data)) stop(private$err_empty_data("data"))
            if(purrr::is_null(filepath)) stop(private$err_null_forbidden("filepath"))
            if(!purrr::is_logical(force)) stop(private$err_param_type_logical("force"))
            if(is.na(force)) stop(private$err_logical_na("force"))
            if(purrr::is_character(data)){
                if(private$is_search_pattern(data)) stop(private$err_search_pattern_forbidden("data"))
                has_indices <- self$has(data)
                absent_indices <- names(has_indices[has_indices == FALSE])
                if(length(absent_indices) != 0){
                    stop(private$err_index_unknown(absent_indices))
                }
            }
            # check package
            if(!Kibior$.is_installed("rio")[[1]]){
                stop(err_pkg_required("export", "rio", "CRAN"))
            }
            #
            if(!force && file.exists(filepath)) stop("File already exists. Use `force = TRUE` to overwrite")
            # 
            # data can be a in-memory dataset, or a name of an index in ES
            dataset <- if(purrr::is_character(data)) self$pull(index_name = data)[[data]] else data
            # write to file
            res <- rio::export(x = dataset, file = filepath, format = format)
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Import method for tabular data.
        #' Needs 'rio' package from CRAN.
        #' Works mainly with CSV, TSV, TAB, TXT and ZIPped formats.
        #'
        #' @family move-data
        #'
        #' @examples
        #' \dontrun{
        #' f <- tempfile(fileext = ".csv")
        #' rio::export(ggplot2::diamonds, f)
        #' # import to in-memory variable
        #' kc$import_tabular(filepath = f)
        #' # import raw data
        #' kc$import_tabular(filepath = f, to_tibble = FALSE)
        #' }
        #'
        #' @param filepath the filepath to use as import, must contain the file extention.
        #' @param to_tibble returns the result as tibble? If FALSE, the raw default rio::import() 
        #'  format will be used (default: TRUE).
        #' @param fileext the file extension (default: ".csv").
        #'
        #' @return data contained in the file as a tibble, or NULL.
        #'
        import_tabular = function(filepath, to_tibble = TRUE, fileext = ".csv"){
            # check package
            if(!Kibior$.is_installed("rio")[[1]]){
                stop(err_pkg_required("import_tabular", "rio", "CRAN"))
            }
            # do import
            filepath %>% 
                self$get_resource(fileext = fileext) %>%
                rio::import() %>%
                (function(x){
                    if(to_tibble) self$soft_cast(x) else x
                })
        },

        #' @details
        #' Import method for features data.
        #' Needs 'rtracklayer' package from Bioconductor.
        #' Works with BED, GTF, GFFx, and GZIPped formats.
        #'
        #' @family move-data
        #'
        #' @examples
        #' \dontrun{
        #' # get sample files
        #' f_gff <- system.file("extdata", "chr_y.gff3.gz", package = "kibior")
        #' f_bed <- system.file("extdata", "cpg.bed", package = "kibior")
        #' # import to in-memory variable
        #' kc$import_features(filepath = f_bed)
        #' kc$import_features(filepath = f_gff)
        #' # import raw data
        #' kc$import_features(filepath = f_bed, to_tibble = FALSE)
        #' kc$import_features(filepath = f_gff, to_tibble = FALSE)
        #' }
        #'
        #' @param filepath the filepath to use as import, must contain the file extention.
        #' @param to_tibble returns the result as tibble? If FALSE, the raw default 
        #'  rtracklayer::import() format will be used (default: TRUE).
        #' @param fileext the file extension (default: ".gtf").
        #'
        #' @return data contained in the file as a tibble, or NULL.
        #'
        import_features = function(filepath, to_tibble = TRUE, fileext = ".gtf"){
            # check package
            if(!Kibior$.is_installed("rtracklayer")[[1]]){
                stop(err_pkg_required("import_features", "rtracklayer", "Bioconductor"))
            }
            #
            filepath %>% 
                self$get_resource(fileext = fileext) %>%
                rtracklayer::import() %>%
                (function(x){
                    if(to_tibble) self$soft_cast(x) else x
                })
        },

        #' @details
        #' Import method for alignments data.
        #' Needs 'Rsamtools' packages from Bioconductor. 
        #' Works with BAM format.
        #'
        #' @family move-data
        #'
        #' @examples
        #' \dontrun{
        #' # get sample file
        #' f_bai <- system.file("extdata", "test.bam.bai", package = "kibior")
        #' # import to in-memory variable
        #' kc$import_alignments(filepath = f_bai)
        #' # import raw data
        #' kc$import_alignments(filepath = f_bai, to_tibble = FALSE)
        #' }
        #'
        #' @param filepath the filepath to use as import, should contain the file extention.
        #' @param to_tibble returns the result as tibble? If FALSE, the raw default 
        #'  Rsamtools::scanBam() format will be used (default: TRUE).
        #' @param fileext the file extension (default: ".bam").
        #'
        #' @return data contained in the file as a tibble, or NULL.
        #'
        import_alignments = function(filepath, to_tibble = TRUE, fileext = ".bam"){
            # check package
            if(!Kibior$.is_installed("Rsamtools")[[1]]){
                stop(err_pkg_required("import_alignments", "Rsamtools", "Bioconductor"))
            }
            #
            filepath %>%
                self$get_resource(fileext = fileext) %>% 
                Rsamtools::scanBam() %>%
                (function(x){
                    if(to_tibble) self$soft_cast(x, caster = self$bam_to_tibble) else x
                })
        },

        #' @details
        #' Import method for JSON format.
        #' Needs 'jsonlite' packages from CRAN.
        #'
        #' @family move-data
        #'
        #' @examples
        #' \dontrun{
        #' # get sample file
        #' f_json <- system.file("extdata", "storms100.json", package = "kibior")
        #' # import to in-memory variable
        #' kc$import_json(f_json)
        #' # import raw data
        #' kc$import_json(f_json, to_tibble = FALSE)
        #' }
        #'
        #' @param filepath the filepath to use as import, should contain the file extention.
        #' @param to_tibble returns the result as tibble? If FALSE, the raw dataframe format 
        #'  will be used (default: TRUE).
        #' @param fileext the file extension (default: ".json").
        #'
        #' @return data contained in the file as a tibble, dataframe or NULL.
        #'
        import_json = function(filepath, to_tibble = TRUE, fileext = ".json"){
            # check package
            if(!Kibior$.is_installed("jsonlite")[[1]]){
                stop(err_pkg_required("import_json", "jsonlite", "CRAN"))
            }
            #
            filepath %>% 
                self$get_resource(fileext = fileext) %>% 
                jsonlite::fromJSON() %>% 
                (function(x){
                    if(to_tibble) self$soft_cast(x) else x
                })
        },

        #' @details
        #' Import method for sequences data.
        #' Needs 'Biostrings' package from Bioconductor.
        #' Works with FASTA formats.
        #'
        #' @family move-data
        #'
        #' @examples
        #' \dontrun{
        #' # get sample file
        #' f_dna <- system.file("extdata", "dna_human_y.fa.gz", package = "kibior")
        #' f_rna <- system.file("extdata", "ncrna_mus_musculus.fa.gz", package = "kibior")
        #' f_aa <- system.file("extdata", "pep_mus_spretus.fa.gz", package = "kibior")
        #' # import to in-memory variable
        #' kc$import_sequences(filepath = f_dna, fasta_type = "dna")
        #' # import raw data
        #' kc$import_sequences(filepath = f_rna, to_tibble = FALSE, fasta_type = "rna")
        #' # import auto
        #' kc$import_sequences(filepath = f_aa)
        #' }
        #'
        #' @param filepath the filepath to use as import, should contain the file extention.
        #' @param to_tibble returns the result as tibble? If FALSE, the raw default 
        #'  Rsamtools::scanBam() format will be used (default: TRUE).
        #' @param fasta_type type of parsing. It can be "dna", "rna", "aa" ou "auto" (default: 
        #'  "auto")
        #'
        #' @return data contained in the file as a tibble, or NULL.
        #'
        import_sequences = function(filepath, to_tibble = TRUE, fasta_type = "auto"){
            if(!(fasta_type %in% c("auto", "dna", "rna", "aa"))){
                stop("Needed fasta_type to be one of 'dna', 'rna','aa' ou 'auto'")
            }
            # check package
            if(!Kibior$.is_installed("Biostrings")[[1]]){
                stop(err_pkg_required("import_sequence", "Biostrings", "Bioconductor"))
            }
            # add parameter (, with.qualities=FALSE) for readXStringSet function
            filepath %>% 
                # choose type
                (function(x){
                    switch(fasta_type,
                        "dna"   = { Biostrings::readDNAStringSet(x) },
                        "rna"   = { Biostrings::readRNAStringSet(x) },
                        "aa"    = { Biostrings::readAAStringSet(x) },
                        "auto"  = { Biostrings::readBStringSet(x) },
                        stop("Unknown fasta option '", fasta_type, "'.")
                    )
                }) %>%
                # cast if needed
                (function(stringset){
                    if(to_tibble){
                        data.frame(
                            width = Biostrings::width(stringset), 
                            seq = as.character(stringset), 
                            names = names(stringset)
                        ) %>% 
                            self$soft_cast()
                    } else {
                        stringset
                    }
                })
        },

        #' @details
        #' Import method that will try to guess importation method.
        #' Will also try to read from compressed data if they are.
        #' This method will call other import_* methods when trying.
        #' Some data formats are not installed by default.
        #' Use `rio::install_formats()` to be able to parse them.
        #'
        #' @family move-data
        #'
        #' @examples
        #' \dontrun{
        #' # get sample file
        #' f_dna <- system.file("extdata", "dna_human_y.fa.gz", package = "kibior")
        #' f_rna <- system.file("extdata", "ncrna_mus_musculus.fa.gz", package = "kibior")
        #' f_aa <- system.file("extdata", "pep_mus_spretus.fa.gz", package = "kibior")
        #' f_bai <- system.file("extdata", "test.bam.bai", package = "kibior")
        #' f_gff <- system.file("extdata", "chr_y.gff3.gz", package = "kibior")
        #' f_bed <- system.file("extdata", "cpg.bed", package = "kibior")
        #' # import 
        #' kc$guess_import(f_dna)
        #' kc$guess_import(f_rna)
        #' kc$guess_import(f_aa)
        #' kc$guess_import(f_bai)
        #' kc$guess_import(f_gff)
        #' kc$guess_import(f_bed)
        #' }
        #'
        #' @param filepath the filepath to use as import, must contain the file extention.
        #' @param to_tibble returns the result as tibble? (default: TRUE).
        #'
        #' @return data contained in the file, or NULL.
        #'
        guess_import = function(filepath, to_tibble = TRUE){
            # 
            guess_import_method <- function(f, rm_compression_extension = FALSE){
                "f can be an url or a fs path."
                "rm_compression_extension = TRUE will try to remove the first layer"
                "of extension (e.g. whatever.csv.gz -> whatever.csv) before trying"
                "to open the original file to identify the non-compressed file extension."

                # try to resolve relative path
                f <- tryCatch(
                    expr = { tools::file_path_as_absolute(f) },
                    error = function(e){ f }
                )
                f %>%
                    (function(x){
                        if(rm_compression_extension) tools::file_path_sans_ext(x) else x
                    }) %>%
                    tools::file_ext() %>%
                    (function(x){ 
                        if(!(x %in% names(private$.AUTO_IMPORT_EXTENSION_MAPPING))) stop("Unknown extension: '", x, "'.")
                        x
                    }) %>%
                    private$.AUTO_IMPORT_EXTENSION_MAPPING[[.]] %>%
                    (function(x){
                        if(self$verbose){
                            message("Try loading [", x, "] data...")
                            message("  file: ", f)
                        }
                        x
                    }) %>%
                    (function(type){
                        args <- list(
                            filepath = f, 
                            to_tibble = to_tibble
                        )
                        m <- switch(type,
                            "tabular"   = { self$import_tabular },
                            "features"  = { self$import_features },
                            "alignments"= { self$import_alignments },
                            "sequences" = { self$import_sequences },
                            "json"      = { self$import_json },
                            stop(paste0(private$ERR_WTF, " Uknown type: ", type))
                        )
                        do.call(what = m, args = args)
                    })
            }
            # try calling the right method
            tryCatch(
                expr = {
                    guess_import_method(filepath)
                }, 
                error = function(e){
                    # try removing potential compression extension else raise an error
                    tryCatch(
                        expr = {
                            guess_import_method(filepath, rm_compression_extension = TRUE)   
                        },
                        error = function(ee){
                            if(self$verbose) message(ee$message)
                            stop("Cannot auto-import. Try with specific methods.")
                        }
                    )
                }
            )
        },

        #' @details
        #' Generic import method.
        #' This method will call other import_* methods when trying.
        #' Some data formats are not installed by default.
        #'
        #' @family move-data
        #'
        #' @examples
        #' \dontrun{
        #' # get sample file
        #' f_aa <- system.file("extdata", "pep_mus_spretus.fa.gz", package = "kibior")
        #' f_gff <- system.file("extdata", "chr_y.gff3.gz", package = "kibior")
        #' f_bai <- system.file("extdata", "test.bam.bai", package = "kibior")
        #' # import 
        #' kc$import(filepath = f_aa)
        #' # import to Elasticsearch index ("sw_from_file") if not exists
        #' kc$import(filepath = f_bai, push_index = "sw_from_file")
        #' # import to index by recreating it, then pull indexed data
        #' kc$import(filepath = f_gff, push_index = "sw_from_file",
        #'  push_mode = "recreate")
        #' }
        #'
        #' @param filepath the filepath to use as import, must contain the file extention.
        #' @param import_type can be one of "auto", "tabular", "features", "alignments", "sequences" 
        #'  (default: "auto").
        #' @param push_index the name of the index where to push data (default: NULL).
        #' @param push_mode the push mode (default: "check").
        #' @param id_col the column name of unique IDs (default: NULL).
        #' @param to_tibble returns the result as tibble? (default: TRUE).
        #'
        #' @return data contained in the file, or NULL.
        #'
        import = function(filepath, import_type = "auto", push_index = NULL, push_mode = "check", id_col = NULL, to_tibble = TRUE){
            if(!purrr::is_character(filepath)) stop(private$err_param_type_character("filepath"))
            if(length(filepath) > 1) stop(private$err_one_value("filepath"))
            if(!purrr::is_null(push_index) && !purrr::is_character(push_index)) stop(private$err_param_type_character("push_index", can_be_null = TRUE))
            if(!(push_mode %in% private$.VALID_PUSH_MODES)) stop(private$err_not_in_vector("push_mode", private$.VALID_PUSH_MODES))
            if(!purrr::is_null(id_col)){
                if(!purrr::is_character(id_col)) stop(private$err_param_type_character("id_col"))
                if(length(id_col) > 1) stop(private$err_one_value("id_col"))
            }
            if(!purrr::is_logical(to_tibble)) stop(private$err_param_type_logical("to_tibble"))
            if(is.na(to_tibble)) stop(private$err_logical_na("to_tibble"))
            # perform import function
            m <- switch(import_type,
                "auto"      = { self$guess_import },
                "tabular"   = { self$import_tabular },
                "features"  = { self$import_features },
                "alignments"= { self$import_alignments },
                "sequences" = { self$import_sequences },
                "json"      = { self$import_json },
                stop(private$ERR_WTF, " Unknown import type '", import_type, "'")
            )
            data <- m(filepath = filepath, to_tibble = to_tibble)
            # check/push
            no_data <- { dim(data) == c(0, 0) } %>% all()
            res <- NULL
            #
            if(no_data) {
                if(self$verbose) message("No data found in '", filepath, "'")
            } else {
                res <- data %>% tibble::as_tibble()
                if(!purrr::is_null(push_index)){
                    res %>% 
                        self$push(push_index, mode = push_mode, id_col = id_col)
                }
            }
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Push data from in-memory to Elasticsearch.
        #' Everything is done by bulk.
        #'
        #' @family move-data
        #'
        #' @examples
        #' \dontrun{
        #' # erase the last push data by recreating the index and re-pushing data
        #' kc$push(dplyr::starwars, index_name = "sw", mode = "recreate")
        #' # characters names are unique, can be used as ID
        #' kc$push(dplyr::starwars, index_name = "sw", mode = "recreate", id_col = "name")
        #' # a bit more complicated: update some data of the dataset "starwars"
        #' # 38 records on 87 filtered
        #' some_new_data <- dplyr::filter(dplyr::starwars, height > 180)
        #' # make them all "gender <- female"
        #' some_new_data["gender"] <- "female"
        #' # update that apply, based on cahracter names to match the right record
        #' kc$push(some_new_data, "sw", mode = "update", id_col = "name")
        #' # view result by querying
        #' kc$pull("sw", query = "height:>180", columns = c("name", "gender"))
        #' }
        #'
        #' @param data the data to push.
        #' @param index_name the index name to use in Elasticsearch.
        #' @param bulk_size the number of record to send to Elasticsearch in a row (default: 1000).
        #' @param mode the push mode, could be "check", "recreate" or "update" (default: "check").
        #' @param id_col an column anme to use as ID, must be composed of unique elements (default: 
        #'  NULL).
        #'
        #' @return the index_name given if the push ended well, else an error.
        #'
        push = function(data, index_name, bulk_size = 1000, mode = "check", id_col = NULL){
            if(missing(data) || purrr::is_null(data)) stop(private$err_null_forbidden("data"))
            if(nrow(data) == 0) stop(private$err_null_forbidden("data"))
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            if(length(index_name) > 1) stop(private$err_one_value("index_name"))
            if(!is.numeric(bulk_size)) stop(private$err_param_type_numeric("bulk_size"))
            if(length(bulk_size) > 1) stop(private$err_one_value("bulk_size"))
            if(bulk_size < 1) stop(private$err_param_positive("bulk_size",))
            if(!(mode %in% private$.VALID_PUSH_MODES)) stop(private$err_not_in_vector("mode", private$.VALID_PUSH_MODES))
            if(length(mode) > 1) stop(private$err_one_value("mode"))
            # check index absence/presence
            has_index <- self$match(index_name)
            if(mode == "check"){
                if(index_name %in% has_index) stop(private$err_index_already_exists(index_name))
            }
            if(mode == "update"){
                if(purrr::is_null(id_col)) stop("Update mode needs a unique IDs column name.")
                if(!(index_name %in% has_index)) stop(private$err_index_unknown(absent_indices))
            }
            # -----------------------------------------
            # PREPARE DATA 

            # ------------------
            # data names to lowercase
            names(data) <- tolower(names(data))
            if(self$verbose) message(" -> Forcing all columns names to lowercase")

            # ------------------
            # handle id_col
            ids <- NULL
            if(!purrr::is_null(id_col)){
                if(!purrr::is_character(id_col)) stop(private$err_param_type_character("id_col"))
                if(length(id_col) > 1) stop(private$err_one_value("id_col"))
                id_col <- tolower(id_col)
                if(!(id_col %in% names(data))) stop("IDs column name not found.")
                # extracting ids from data
                ids <- dplyr::pull(data, id_col)
                if(length(unique(ids)) != length(ids)) stop("Column used as ID does not have unique elements.")
            } else {
                private$err_msg <- paste0("Kibior tried to add a unique column IDs '", 
                    self$default_id_col, "', but the column name already exists. ", 
                    "Try to define a column with unique IDs, change the column name or define another default name with '$default_id_col'.")
                if(self$default_id_col %in% names(data)) stop(private$err_msg)
                # force add a column with unique id (single sequence 1:nrow)
                ids <- seq_len(nrow(data))
                data <- within(data, assign(self$default_id_col, ids))
                if(self$verbose) message(" -> Adding unique '", self$default_id_col, "' column to enforce uniqueness")
            }

            # ------------------
            # try to flatten if data.frame class is found in one of the column types
            has_df_type <- data %>% 
                lapply(class) %>% 
                unlist(use.names = FALSE) %>% 
                {"data.frame" %in% .} %>% 
                any()
            if(has_df_type){
                if(self$verbose) message(" -> Flattening columns having sub-dataframes")
                data <- jsonlite::flatten(data)
            }

            # ------------------
            # transform col: field names cannot contain dots, transform and warn user
            has_dot <- grepl(".", names(data), fixed = TRUE)
            if(any(has_dot)){
                # replace names
                old_names <- names(data)[has_dot == TRUE]
                new_names <- old_names %>% gsub("\\.", "_", .)
                changed_names <- gsub("\\.", "_", names(data))
                # check
                if(length(changed_names) != length(unique(changed_names))){
                    zip <- changed_names
                    names(zip) <- names(data)
                    zip <- as.list(zip)
                    for(i in names(zip)){
                        if(i != zip[[i]] && zip[[i]] %in% names(zip)){
                            stop("Column name [", i, "] should be changed to [", zip[[i]], "], but it already exists")
                        }
                    }
                }
                # apply
                names(data) <- changed_names
                # warn
                if(self$verbose){ 
                    message(" -> Changing column names: ")
                    old_names %>% 
                        paste0(collapse = ", ") %>% 
                        message("      from: ", .)
                    new_names %>% 
                        paste0(collapse = ", ") %>% 
                        message("        to: ", .)
                }
            }

            # ------------------
            # remove empty columns
            is_column_empty_char <- function(x){
                # test if a list contains only empty strings ""  values
                x %>%
                    unlist(use.names = FALSE) %>%
                    {. == ""} %>%
                    all()
            }
            is_column_empty_null <- function(x){
                # test if a list contains only NULL values
                x %>%
                    unlist(use.names = FALSE) %>%
                    purrr::is_null()
            }
            is_column_empty_na <- function(x){
                # test if a list contains only NA values
                x %>%
                    unlist(use.names = FALSE) %>%
                    is.na() %>%
                    all()
            }
            is_column_empty <- function(x){
                # test if a list contains only NA, NULL or empty strings values
                is_column_empty_char(x) || 
                    is_column_empty_null(x) || 
                    is_column_empty_na(x)
            }
            # get empty columns
            cols_empty <- sapply(data, is_column_empty)
            if(any(cols_empty)){
                if(self$verbose){
                    cols_empty %>% 
                        which(.) %>% 
                        names() %>% 
                        paste0(collapse = ", ") %>%
                        message(" -> Removing empty columns: ", .)
                }
                # remove empty columns from data
                data <- data[-which(cols_empty)]
            }
            # ------------------
            # Listed columns containing NA/NULL changed to "" (empty string)
            list_col_names <- data %>% 
                sapply(purrr::is_list) %>% 
                data[.] %>% 
                names
            if(length(list_col_names) > 0){
                if(self$verbose){
                    list_col_names %>% 
                        paste0(collapse = ", ") %>%
                        message(" -> Adapting NULL/NA to empty strings values from columns: ", .)
                }
                data <- data %>% 
                    dplyr::mutate_at(dplyr::all_of(dplyr::vars(list_col_names)), tidyr::replace_na, "") %>% 
                    tibble::as_tibble()
            }
            # -----------------------------------------
            # DEFINE AND APPLY MAPPING
            # process: create index and mapping
            if(mode != "update"){
                res <- self$create(index_name = index_name, force = (mode == "recreate"))
                if(!res[[index_name]]) stop("Index '", index_name,"' not created")
                # define mapping based on data type
                if(self$verbose) message(" -> Defining mapping of '", index_name, "'")
                mapping <- private$define_mappings(data)
                #
                err_msg <- paste0("Cannot apply mapping to '", index_name, "'")
                mapping_res <- tryCatch(
                    expr = {
                        suppressWarnings({ 
                            private$create_mappings(index_name, mapping)
                        })
                    }, 
                    error = function(e){
                        if(self$verbose){
                            err_msg <- paste0(err_msg, "\n  ", e$message)
                        }
                        stop(err_msg)
                    }
                )
                # control result
                if(purrr::is_null(mapping_res) || !mapping_res$acknowledged) stop(err_msg)
            }
            # -----------------------------------------
            # SEND DATA
            # prepare
            if(self$verbose) message(" -> Sending data to '", index_name, "'")
            args <- list(conn = self$connection,
                        x = data,
                        index = index_name,
                        chunk_size = bulk_size,
                        raw = FALSE,
                        doc_ids = ids,
                        quiet = self$quiet_progress,
                        query = list(refresh = "wait_for"))
            # before v7, need arg type
            if(self$version$major < 7){
                args[["type"]] = "_doc"
            }
            # choose bulk method
            bulk_method <- NULL
            if(mode != "update"){
                # if id_col not defined by the user, Elasticsearch will give ids automatically
                args[["es_ids"]] = FALSE
                bulk_method <- elastic::docs_bulk_index
            } else {
                bulk_method <- elastic::docs_bulk_update
            }
            # Time the bulk send
            clock_start <- proc.time()
            bulks_res <- suppressWarnings({ 
                # suppress warnings caused by type removal deprecation in ES 6 and 7
                rr <- do.call(bulk_method, args)
                rr[[1]]
            })
            elapsed <- proc.time() - clock_start
            # some errors during data transfer
            if(bulks_res[["errors"]]){
                msg_errors <- bulks_res$items %>%
                    lapply(function(x){ x[["index"]][["error"]][["reason"]] }) %>%
                    unlist(use.names = FALSE) %>% 
                    paste0(collapse = ", ") %>%
                    warning("Some errors happenned during data transfer: ", .)
            }
            # verbose
            if(self$verbose){
                es_took <- bulks_res[["took"]] %>% 
                    unlist(use.names = FALSE) %>%
                    sum() %>%
                    as.double() %>%
                    {. / 1000} %>%
                    private$humanize_time()
                # user info
                user_took <- elapsed[["elapsed"]] %>% 
                    private$humanize_time()
                # execution
                message(" -> Execution time: ")
                message("   - data sent: ", es_took$time, es_took$unit)
                message("   - user wait: ", user_took$time, user_took$unit)

            }
            # get some time so Elasticsearch can make them available
            if(self$verbose) message(" -> Waiting a bit for Elasticsearch")
            Sys.sleep(self$elastic_wait)
            if(self$quiet_results) invisible(index_name) else index_name
        },

        #' @details
        #' Pull data from Elasticsearch.
        #' Everything is done by bulk.
        #' This method is essentially a wrapper around `$search()` with parameter `head = FALSE`
        #'
        #' @family move-data
        #'
        #' @examples
        #' \dontrun{
        #' # push some data sample
        #' kc$push(dplyr::storms, "storms")
        #' # get the whole "sw" index
        #' kc$pull("sw")
        #' # get the whole "sw" index with all metadata
        #' kc$pull("sw", keep_metadata = TRUE)
        #' # get only "name" and "status" columns of indices starting with "s"
        #' # columns not found will be ignored
        #' kc$pull("s*", columns = c("name", "status"))
        #' # limit the size of the result to 10
        #' kc$pull("storms", max_size = 10, bulk_size = 10)
        #' # use Elasticsearch query syntax to select and filter on all indices, for all data
        #' # Here, we want to search for all records taht match the conditions:
        #' # field "height" is strictly more than 180 AND field homeworld is "Tatooine" OR "Naboo"
        #' r <- kc$pull("sw", query = "height:>180 && homeworld:(Tatooine || Naboo)")
        #' # it can be used in conjunction with `columns` to select only columns that matter
        #' r <- kc$pull("sw", query = "height:>180 && homeworld:(Tatooine || Naboo)", columns = 
        #'  c("name", "hair_color", "homeworld"))
        #' }
        #'
        #' @param index_name the index name to use in Elasticsearch.
        #' @param bulk_size the number of record to send to Elasticsearch in a row (default: 500).
        #' @param max_size the number of record Elasticsearch will send (default: NULL (all data)).
        #' @param scroll_timer the time the scroll API will let the request alive to scroll on the 
        #'  result (default: "3m" (3 minute)).
        #' @param keep_metadata does Elasticsearch needs to sent metadata? Data columns will be 
        #'  prefixed by "_source." (default: FALSE).
        #' @param columns a vector of columns to select (default: NULL (all columns)).
        #' @param query a string formatted to Elasticsearch query syntax, see links for the syntax 
        #'  details (default: NULL)
        #'
        #' # Simple syntax details:
        #'
        #' @seealso \url{https://www.elastic.co/guide/en/elasticsearch/reference/current/common-options.html#time-units} 
        #'  for time-units and \url{https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-query-string-query.html#query-string-syntax}
        #'  for the Elasticsearch query string syntax.
        #'
        #' @return a list of datasets corresponding to the pull request, else an error. Keys of the 
        #'  list are index names matching the request, value are the associated tibbles
        #'
        pull = function(index_name, bulk_size = 500, max_size = NULL, scroll_timer = "3m", keep_metadata = FALSE, columns = NULL, query = NULL) {
            args <- list(
                index_name = index_name, 
                keep_metadata = keep_metadata, 
                columns = columns, 
                query = query, 
                bulk_size = bulk_size, 
                max_size = max_size, 
                scroll_timer = scroll_timer, 
                head = FALSE
            )
            if(self$quiet_results) invisible(do.call(self$search, args)) else do.call(self$search, args)
        },

        #'
        #' @details
        #' Move data from one index to another.
        #' It needs to be configured in the `config/elasticsearch.yml` file to actually work.
        #'
        #' @family move-data
        #'
        #' @examples
        #' \dontrun{
        #' kc$push(dplyr::starwars, "sw", mode = "recreate")
        #' # move data from an index to another (change name, same instance)
        #' r <- kc$move(from_index = "sw", to_index = "sw_new")
        #' kc$pull("sw_new")
        #' kc$list()
        #' } 
        #'
        #' @param from_instance If not NULL, the Kibior object of another instance. if NULL 
        #'  (default), this instance will be used. (default: NULL).
        #' @param from_index The source index name (default: NULL).
        #' @param to_index The destination index name (default: NULL).
        #' @param force Does the destination index need to be erase? (default: FALSE)
        #' @param copy Does the destination have to be a copy of the source? FALSE (default) will 
        #'  delete source index, TRUE will keep it. (default: FALSE).
        #'
        #' @seealso: \url{https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-reindex.html}
        #'  Elasticsearch reindex feature for more information.
        #'
        #' @return the reindex result
        #'
        move = function(from_index, to_index, from_instance = NULL, force = FALSE, copy = FALSE){
            if(!purrr::is_null(from_instance) && !Kibior$is_instance(from_instance)) stop("Need a Kibior instance type or NULL.")
            is_local <- purrr::is_null(from_instance)
            # select source instance
            source_instance <- if(is_local) self else from_instance
            if(!purrr::is_character(from_index)) stop(private$err_param_type_character("from_index"))
            if(private$is_search_pattern(from_index)) stop(private$err_search_pattern_forbidden("from_index"))
            if(length(from_index) > 1) stop(private$err_one_value("from_index"))
            if(!purrr::is_character(to_index)) stop(private$err_param_type_character("to_index"))
            if(private$is_search_pattern(to_index)) stop(private$err_search_pattern_forbidden("to_index"))
            if(length(to_index) > 1) stop(private$err_one_value("to_index"))
            if(!purrr::is_logical(force)) stop(private$err_param_type_logical("force"))
            if(is.na(force)) stop(private$err_logical_na("force"))
            if(is_local && from_index == to_index) stop("Source and destination indices are the same.")
            has_indices <- source_instance$has(from_index)
            absent_indices <- names(has_indices[has_indices == FALSE])
            if(length(absent_indices) != 0) stop(private$err_index_unknown(absent_indices))
            if(!force && !purrr::is_null(self$match(to_index))){
                stop(private$err_index_already_exists(already_there_indices))
            }
            if(!purrr::is_logical(copy)) stop(private$err_param_type_logical("copy"))
            if(is.na(copy)) stop(private$err_logical_na("copy"))
            # msg
            if(self$verbose){
                action <- if(copy) "Copying" else "Moving" 
                source_str <- paste0("'" , source_instance$host, ":", source_instance$port, "/", from_index, "'")
                dest_str <- paste0("'", self$host, ":", self$port, "/", to_index, "'")
                message(" -> ", action, " " , source_str, " to ", dest_str)
            }
            # TODO: add "_source" = columns to select only some columns
            # TODO: add "query" = query to execute a query before moving, need to parse query string to struct
            # building args - source subpart
            source_param <- list(index = from_index)
            if(!is_local){
                source_param[["remote"]] <- list(host = from_instance$endpoint)
                if(!purrr::is_null(from_instance$username)){
                    source_param[["remote"]][["username"]] <- from_instance$username
                }
                if(!purrr::is_null(from_instance$password)){
                    source_param[["remote"]][["password"]] <- from_instance$password   
                }
            }
            # compose args - source + dest
            reindex_args <- list(
                conn = self$connection,
                body = list(
                    source = source_param,
                    dest = list(index = to_index)
                ),
                wait_for_completion = "false"
            )
            # function to manage ES errors
            handle_error <- function(e){
                message(" -> Moving: errors during data transfer")
                self$delete(to_index)
                stop(e)
            }
            # reindex
            self$create(to_index, force = force)
            res <- tryCatch(
                expr = {
                    do.call(elastic::reindex, reindex_args)
                }, 
                error = function(e){
                    handle_error(e)
                }
            )
            # if there are some errors during reindex
            if(length(res$failure) != 0) handle_error(res$failure)
            # removing source index if action is "moving"
            if(!copy) source_instance$delete(from_index)
            # msg
            if(self$verbose){
                t <- {res$took / 1000} %>% private$humanize_time()
                message(" -> Documents transfered: ", res$total, ", took: ", t$time, t$unit)
            }
            #
            if(self$verbose) message(" -> Waiting a bit for Elasticsearch")
            Sys.sleep(self$elastic_wait)
            if(self$quiet_results) invisible(to_index) else to_index
        },

        #'
        #' @details
        #' Copy data from one index to another.
        #' It needs to be configured in the `config/elasticsearch.yml` file to actually work.
        #' This method is a wrapper around `$move(copy = TRUE)`.
        #'
        #' @family move-data
        #'
        #' @examples
        #' \dontrun{
        #' # copy data from one index to another (same instance)
        #' r <- kc$copy(from_index = "sw_new", to_index = "sw")
        #' kc$pull(c("sw", "sw_new"))
        #' kc$list()
        #' } 
        #'
        #' @param from_instance If not NULL, the Kibior object of another instance. if NULL 
        #'  (default), this instance will be used. (default: NULL).
        #' @param from_index The source index name (default: NULL).
        #' @param to_index The destination index name (default: NULL).
        #' @param force Does the destination index need to be erase? (default: FALSE)
        #'
        #' @seealso: \url{https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-reindex.html}
        #'  Elasticsearch reindex feature for more information.
        #'
        #' @return the reindex result
        #'
        copy = function(from_index, to_index, from_instance = NULL, force = FALSE){
            args <- list(
                from_instance = from_instance, 
                from_index = from_index, 
                to_index = to_index, 
                force = force, 
                copy = TRUE
            )
            if(self$quiet_results) invisible(do.call(self$move, args)) else do.call(self$move, args)
        },


        # --------------------------------------------------------------------------
        # methods - Search
        # --------------------------------------------------------------------------

        #' @details
        #' Match requested index names against Elasticsearch indices list.
        #'
        #' @family search-data
        #'
        #' @examples
        #' \dontrun{
        #' # search "sw" index name
        #' kc$match("sw")
        #' # search all starting with an "s"
        #' kc$match("s*")
        #' # get all index name, identical to `$list()`
        #' kc$match("*")
        #' # search multiple names 
        #' kc$match(c("sw", "sw_new", "nope"))
        #' # search multiple names with pattern
        #' kc$match(c("s*", "nope"))
        #' }
        #'
        #' @param index_name the index name to use in Elasticsearch, can be a pattern with '*'.
        #'
        #' @return a vector of matching index names, NULL if nothing matches.
        #'
        match = function(index_name){
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            # first requests can be long, warn user
            if(self$verbose){
                index_name %>% 
                    private$vector_to_str() %>% 
                    message(" -> Requesting index names: ", .)
            }
            # get indices
            res <- elastic::search_shards(
                    conn = self$connection,
                    index = index_name
                )[["indices"]] %>% names()
            # 
            if(typeof(res) != "character") stop(private$ERR_WTF, " expected string")
            if(length(res) == 0) res <- NULL
            # 
            if(self$verbose){
                to_msg <- if(purrr::is_null(res)) "none" else private$vector_to_str(res)
                message(" -> Matching index names: ", to_msg)
            }
            # 
            res
        },

        #' @details
        #' Search data from Elasticsearch.
        #' The goal of this method is to discover quickly what data are interesting, thus 
        #'  `head = TRUE` by default.
        #' If you want to get all data, use `head = FALSE` or `$pull()`.
        #' Everything is done by bulk.
        #'
        #' @family search-data
        #'
        #' @examples
        #' \dontrun{
        #' # search "sw" index, head mode on
        #' kc$search("sw")
        #' # search "sw" index with all metadata, head mode on
        #' kc$search("sw", keep_metadata = TRUE)
        #' # get only "name" field of the head of indices starting with "s"
        #' # if an index does not have the "name" field, it will be empty
        #' kc$search("s*", columns = "name")
        #' # limit the size of the result to 50 to the whole index
        #' kc$search("storms", max_size = 50, bulk_size = 50, head = FALSE)
        #' # use Elasticsearch query syntax to select and filter on all indices, for all data
        #' # Here, we want to search for all records taht match the conditions:
        #' # field "height" is strictly more than 180 AND field homeworld is "Tatooine" OR "Naboo"
        #' kc$search("*", query = "height:>180 && homeworld:(Tatooine || Naboo)")
        #' # it can be used in conjunction with `columns` to select only columns that matter
        #' kc$search("*", query = "height:>180 && homeworld:(Tatooine || Naboo)", columns = 
        #'  c("name", "hair_color", "homeworld"))
        #' }
        #'
        #' @param index_name the index name to use in Elasticsearch (default: NULL).
        #' @param bulk_size the number of record to send to Elasticsearch in a row (default: 500).
        #' @param max_size the number of record Elasticsearch will send (default: NULL (all data)).
        #' @param scroll_timer the time the scroll API will let the request alive to scroll on the 
        #'  result (default: "3m" (3 minutes)).
        #' @param keep_metadata does Elasticsearch needs to sent metadata? Data columns will be 
        #'  prefixed by "_source." (default: FALSE).
        #' @param columns a vector of columns to select (default: NULL (all columns)).
        #' @param head a boolean limiting the search result and time (default: TRUE)
        #' @param query a string formatted to Elasticsearch query syntax, see links for the syntax 
        #'  details (default: NULL)
        #'
        #' @seealso \url{https://www.elastic.co/guide/en/elasticsearch/reference/current/common-options.html#time-units} 
        #'  for time-units and \url{https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-query-string-query.html#query-string-syntax}
        #'  for the Elasticsearch query string syntax.
        #'
        #' @return a list of datasets corresponding to the pull request, else an error. Keys of the 
        #'  list are index names matching the request, value are the associated tibbles
        #'
        search = function(index_name = "_all", keep_metadata = FALSE, columns = NULL,  bulk_size = 500, max_size = NULL, scroll_timer = "3m", head = TRUE, query = NULL){
            if(purrr::is_null(index_name)) index_name <- "_all"
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            if(!purrr::is_logical(head)) stop(private$err_param_type_logical("head"))
            if(is.na(head)) stop(private$err_logical_na("head"))
            if(("_all" %in% index_name) && !head) message("Retriving ALL data... you should target some indices instead.")
            # define when to stop the search with head mode
            if(self$verbose) {
                message(" -> Using head mode: ", if(head) "yes" else "no")
                if(head) message(" -> Head size: ", self$head_search_size)
            }
            if(!is.numeric(bulk_size)) stop(private$err_param_type_numeric("bulk_size"))
            if(length(bulk_size) > 1) stop(private$err_one_value("bulk_size"))
            if(bulk_size < 1) stop(private$err_param_positive("bulk_size", can_be_null = FALSE))
            bulk_size <- as.integer(bulk_size)
            if(bulk_size > private$.MAX_BUCKET_SIZE){
                bulk_size <- private$.MAX_BUCKET_SIZE
                if(self$verbose) message(" -> Restricting 'bulk_size' to 10.000")
            }
            if(self$verbose) message(" -> Using 'bulk_size': ", bulk_size)
            if(!purrr::is_null(max_size)){
                if(!is.numeric(max_size)) stop(private$err_param_type_numeric("max_size"))
                if(length(max_size) > 1) stop(private$err_one_value("max_size"))
                if(max_size < 1) stop(private$err_param_positive("max_size", can_be_null = FALSE))
                max_size <- as.integer(max_size)
                if(self$verbose) message(" -> Using 'max_size': ", max_size)
                if(bulk_size > max_size){
                    if(self$verbose) message(" -> Reducing 'bulk_size' to match 'max_size': ", max_size)
                    bulk_size <- max_size
                }
            }
            if(!purrr::is_character(scroll_timer)) stop(private$err_param_type_character("scroll_timer"))
            if(length(scroll_timer) > 1) stop(private$err_one_value("scroll_timer"))
            if(!purrr::is_logical(keep_metadata)) stop(private$err_param_type_logical("keep_metadata"))
            if(is.na(keep_metadata)) stop(private$err_logical_na("keep_metadata"))
            if(!purrr::is_null(columns) && !purrr::is_character(columns)) stop(private$err_param_type_character("columns"))
            if(!purrr::is_null(query)){
                if(!purrr::is_character(query)) stop(private$err_param_type_character("query"))
                if(length(query) > 1) stop(private$err_one_value("query"))
            }
            # return result
            final_df <- NULL
            # init
            selected_fields <- if(purrr::is_null(columns)) NULL else paste0(columns, collapse = ",")
            end_search <- FALSE
            # total according to ES version
            get_total_records <- function(hits_total){
                if(self$version$major > 6) hits_total$value else hits_total
            }
            # function: get clean bulk
            get_clean_bulk <- function(bulk){
                "Clean bulks when they arrive"
                # stable
                workable_hits <- list()
                for(i in seq(nrow(bulk))) {
                    # get each line (df) as list
                    t <- bulk[i,] %>% as.list()
                    # extract _source if needed, else flatten the results
                    # Must be only one level, error else
                    # TODO: handle x levels: recusive = T -> prb with rbindlist
                    if(keep_metadata){
                        workable_hits[[i]] <- unlist(t, recursive = FALSE)
                    } else {
                        workable_hits[[i]] <- t[["_source"]]
                    }
                }
                # combine to raw_hits
                workable_hits %>%
                    data.table::rbindlist(use.names = TRUE, fill = FALSE) %>%
                    tibble::as_tibble(.name_repair = "unique")
            }
            # function: change column type
            change_column_type <- function(raw){
                "Take a tibble with listed columns and tries to 
                take the inner type of the listed type
                "
                data <- raw
                for(i in names(raw)){
                    n <- data[[i]][[1]]
                    if(length(n) == 1){
                        f <- switch(typeof(n),
                            "character" = { as.character },
                            "integer" = { as.integer },
                            "double" = { as.double },
                            "logical" = { as.logical },
                            stop(private$ERR_WTF, "Unknown type '", n, "'.")
                        )
                        data[[i]] <- f(data[[i]])
                    }
                }
                data
            }
            # extract ids
            extract_ids <- function(hits){
                hits %>% 
                    lapply(function(h){ 
                        h[["_id"]] 
                    }) %>% 
                    unlist(use.names = FALSE)
            }
            # involved_indices
            involved_indices <- self$match(index_name)
            if(!purrr::is_null(involved_indices)){
                final_df <- involved_indices %>% 
                    lapply(function(x){ list() }) %>% 
                    setNames(involved_indices)
                # various infos per index
                run_infos <- final_df
                for(i in names(run_infos)){
                    run_infos[[i]][["end_reached"]] <- FALSE
                    run_infos[[i]][["total_hits"]] <- NA
                    run_infos[[i]][["threshold"]] <- NA
                }
                # per index, first search config
                if(self$verbose) message(" -> Getting search results:")
                for(current_index in names(run_infos)){

                    # first search, init scroll and manage error
                    search_res <- tryCatch({
                            elastic::Search(
                                self$connection, 
                                index = current_index, 
                                size = bulk_size, 
                                source = "__",  # nullify _source
                                time_scroll = scroll_timer, 
                                q = query
                            )
                        },
                        error = function(e){
                            # pattern when limit of request size is reached
                            r <- e$message %>% 
                                trimws() %>%
                                stringr::str_match("400 - An HTTP line is larger than ([0-9]+) bytes.") %>%
                                .[1,2]
                            if(!is.na(r)){
                                msg <- paste0("Server '", self$cluster_name, "' (", self$host, ")")
                                msg <- paste0(msg, " cannot take requests that big (max ", r," bytes).")
                                msg <- paste0(msg, " Try cutting down your query into several pieces.")
                            } else {
                                msg <- e$message
                            }
                            stop(msg)
                        }
                    )
                    # check total hits
                    run_infos[[current_index]][["total_hits"]] <- get_total_records(search_res$hits$total)
                    # no results
                    if(length(search_res$hits$hits) == 0){
                        run_infos[[current_index]][["end_reached"]] <- FALSE
                    }
                    # max threshold
                    tmp_threshold <- if(purrr::is_null(max_size)) run_infos[[current_index]][["total_hits"]] else max_size
                    if(head && tmp_threshold > self$head_search_size){
                        tmp_threshold <- self$head_search_size
                    } 
                    run_infos[[current_index]][["threshold"]] <- tmp_threshold
                    # scroll id
                    run_infos[[current_index]][["scroll_id"]] <- search_res[["_scroll_id"]]
                    # timer 
                    run_infos[[current_index]][["timer"]] <- 0
                    # last hits
                    run_infos[[current_index]][["hits"]] <- extract_ids(search_res$hits$hits)

                    # msg
                    if(self$verbose){
                        index_hits_asked <- run_infos[[current_index]][["threshold"]]
                        index_hits_total <- run_infos[[current_index]][["total_hits"]]
                        paste0("   - ", current_index, ": ") %>%
                            paste0(if(index_hits_asked > index_hits_total) index_hits_total else index_hits_asked) %>%
                            paste0("/", index_hits_total) %>%
                            message()
                    }
                }

                # base args for inc loops
                base_args = list(
                    conn = self$connection, 
                    source = TRUE, 
                    raw = TRUE, 
                    verbose = FALSE, 
                    callopts=list(verbose=FALSE)
                )

                # progress bar init
                pb <- NULL
                if(!head && !self$quiet_progress){
                    cumul_threshold <- run_infos %>% 
                        lapply(function(x){ 
                            if(x[["threshold"]] < x[["total_hits"]]) x[["threshold"]] else x[["total_hits"]]
                        }) %>% 
                        unlist(use.names = FALSE) %>%
                        sum()
                    if(cumul_threshold > 0){
                        # init pbar
                        pb_sum <- 0
                        pb <- txtProgressBar(min = 0, max = cumul_threshold, initial = 0, style = 3)
                    }
                }

                # start timers
                for(current_index in names(final_df)){
                    run_infos[[current_index]][["timer"]] <- proc.time()
                }

                # loop until nothing is returned or threshold is reached
                all_end_search <- FALSE
                while(!all_end_search){

                    # each index
                    for(current_index in names(final_df)){

                        # first search check
                        if(length(run_infos[[current_index]][["hits"]]) == 0) {
                            run_infos[[current_index]][["end_reached"]] <- TRUE
                            # Stop the clock
                            run_infos[[current_index]][["timer"]] <- proc.time() - run_infos[[current_index]][["timer"]]
                        }

                        # if end not reached for this index, explore
                        if(!run_infos[[current_index]][["end_reached"]]){    

                            # search list of ids
                            if(length(run_infos[[current_index]][["hits"]]) == 1) {

                                # complete args
                                args <- c(base_args, list(
                                    index = current_index,
                                    id = run_infos[[current_index]][["hits"]],
                                    source_includes = selected_fields
                                ))
                                # get data from ES (simple get)
                                raw <- do.call(elastic::docs_get, args) %>%
                                    jsonlite::fromJSON(simplifyDataFrame = TRUE) %>%
                                    (function(x){ 
                                        if(keep_metadata) unlist(x, recursive = FALSE) else x[["_source"]] 
                                    }) %>%
                                    rbind() %>% 
                                    as.data.frame(stringsAsFactors = FALSE) %>% 
                                    tibble::as_tibble() %>%
                                    change_column_type()
                                
                                # combne results in one df
                                final_df[[ current_index ]] <- raw
                                # end
                                run_infos[[current_index]][["end_reached"]] <- TRUE
                                # Stop the clock
                                run_infos[[current_index]][["timer"]] <- proc.time() - run_infos[[current_index]][["timer"]]
                                #
                                nb_hits <- 1

                            } else {
                                # ids to req
                                if(head && length(run_infos[[current_index]][["hits"]]) > self$head_search_size){
                                    req_ids <- head(run_infos[[current_index]][["hits"]], self$head_search_size) 
                                } else {
                                    req_ids <- run_infos[[current_index]][["hits"]]
                                }
                                # complete args
                                args <- c(base_args, list(
                                    index = current_index,
                                    ids = req_ids,
                                    "_source_includes" = selected_fields
                                ))
                                # get data from ES
                                res <- do.call(elastic::docs_mget, args) %>% 
                                    jsonlite::fromJSON(simplifyDataFrame = TRUE) 
                                # if the result is not empty
                                nb_hits <- nrow(res$docs)
                                if(nb_hits > 0){
                                    # tidy data
                                    raw <- get_clean_bulk(res$docs)
                                    # combne results in one df
                                    if(length(final_df[[ current_index ]]) == 0){
                                        final_df[[ current_index ]] <- raw
                                    } else {
                                        final_df[[ current_index ]] <- tryCatch(
                                            expr = {
                                                # error raised with listed columns
                                                dplyr::bind_rows(raw, final_df[[ current_index ]])
                                            }, error = function(e){
                                                # better management of listed columns
                                                rbind(raw, final_df[[ current_index ]])
                                            }
                                        )
                                    }
                                }
                            }
                            # update pbar
                            if(!purrr::is_null(pb)){
                                pb_sum <- pb_sum + nb_hits
                                setTxtProgressBar(pb, pb_sum)
                            }

                            # test end
                            if(nrow(final_df[[current_index]]) >= run_infos[[current_index]][["threshold"]]){
                                run_infos[[current_index]][["end_reached"]] <- TRUE
                                # Stop the clock
                                run_infos[[current_index]][["timer"]] <- proc.time() - run_infos[[current_index]][["timer"]]

                            } else {
                                # continue search scroll
                                search_res <- elastic::scroll(self$connection, 
                                                            run_infos[[current_index]][["scroll_id"]], 
                                                            time_scroll = scroll_timer)
                                tmp_ids <- extract_ids(search_res$hits$hits)
                                run_infos[[current_index]][["hits"]] <- tmp_ids
                                run_infos[[current_index]][["end_reached"]] <- (length(tmp_ids) == 0)
                            }

                        }
                    }

                    # test if all ends are set
                    all_end_search <- run_infos %>% 
                        lapply(function(x){ x[["end_reached"]] }) %>%
                        unlist(use.names = FALSE)  %>%
                        all()
                }

                # verbose time
                if(self$verbose){
                    # CR after progress bar
                    message()
                    # has scroll pending?
                    message(" -> Closing scrolls: ")
                    for(current_index in names(final_df)){
                        # close scroll
                        run_infos[[current_index]][["scroll_id"]] %>%
                            elastic::scroll_clear(self$connection, x = ., all = FALSE) %>% 
                            (function(x){ if(x) "closed" else "implicit closing pending" }) %>%
                            message("   - ", current_index, ": ", .)
                    }
                    # execution
                    message(" -> Execution time: ")
                    execution_total <- 0
                    for(current_index in names(final_df)){
                        # calculate
                        user_took <- run_infos[[current_index]][["timer"]][["elapsed"]] %>%
                            private$humanize_time()
                        message("   - ", current_index, ": ", user_took$time, user_took$unit)
                        # total
                        execution_total <- execution_total + run_infos[[current_index]][["timer"]][["elapsed"]]
                    }
                    total_took <- private$humanize_time(execution_total)
                    message("   - total: ", total_took$time, total_took$unit)
                }

                # close progress bar
                if(!purrr::is_null(pb)) close(pb)
            }
            # return 
            if(self$quiet_results) invisible(final_df) else final_df
        },


        # --------------------------------------------------------------------------
        # methods - Joins
        # --------------------------------------------------------------------------

        #' @details
        #' Execute a inner join between two datasets using `dplyr` joins.
        #' The datasets can be in-memory (variable name) or the name of an currently stored 
        #'  Elasticsearch index. Joins cannot be done on column of type "list" ("by" 
        #'  argument).
        #'
        #' @param ... see `join()` params.
        #'
        #' @family joins
        #'
        #' @examples
        #' \dontrun{
        #' # some data for joins examples
        #' kc$push(ggplot2::diamonds, "diamonds")
        #' # prepare join datasets, only big the biggest diamonds are selected (9)
        #' sup_carat <- dplyr::filter(ggplot2::diamonds, carat > 3.5)
        #' r <- kc$push(sup_carat, "diamonds_superior")
        #' # execute a inner_join with one index and one in-memory dataset
        #' kc$inner_join(ggplot2::diamonds, "diamonds_superior")
        #' # execute a inner_join with one index queried, and one in-memory dataset
        #' kc$inner_join(ggplot2::diamonds, "diamonds", right_query 
        #'  = "carat:>3.5")
        #' }
        #'
        #' @return a tibble
        #'
        inner_join = function(...) {
            res <- private$join(join_type = "inner", ...)
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Execute a full join between two datasets using `dplyr` joins.
        #' The datasets can be in-memory (variable name) or the name of an currently stored 
        #'  Elasticsearch index. Joins cannot be done on column of type "list" ("by" 
        #'  argument).
        #'
        #' @param ... see `join()` params.
        #'
        #' @family joins
        #'
        #' @examples
        #' \dontrun{
        #' # prepare join datasets, fair cuts 
        #' fair_cut <- dplyr::filter(ggplot2::diamonds, cut == "Fair")  # 1605 lines
        #' sup_carat <- kc$pull("diamonds_superior")$diamonds_superior
        #' # execute a full_join with one index and one in-memory dataset
        #' kc$full_join(fair_cut, "diamonds_superior")
        #' # execute a full_join with one index queried, and one in-memory dataset
        #' kc$full_join(sup_carat, "diamonds", right_query = "cut:fair")
        #' }
        #'
        #' @return a tibble
        #'
        full_join = function(...) {
            res <- private$join(join_type = "full", ...)
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Execute a left join between two datasets using `dplyr` joins.
        #' The datasets can be in-memory (variable name) or the name of an currently stored 
        #'  Elasticsearch index. Joins cannot be done on column of type "list" ("by" 
        #'  argument).
        #'
        #' @param ... see `join()` params.
        #'
        #' @family joins
        #'
        #' @examples
        #' \dontrun{
        #' # prepare join datasets, fair cuts 
        #' fair_cut <- dplyr::filter(ggplot2::diamonds, cut == "Fair")  # 1605 lines
        #' sup_carat <- kc$pull("diamonds_superior")$diamonds_superior
        #' # execute a left_join with one index and one in-memory dataset
        #' kc$left_join(fair_cut, "diamonds_superior")
        #' # execute a left_join with one index queried, and one in-memory dataset
        #' kc$left_join(sup_carat, "diamonds", right_query 
        #'  = "cut:fair")
        #' }
        #'
        #' @return a tibble
        #'
        left_join = function(...) {
            res <- private$join(join_type = "left", ...)
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Execute a right join between two datasets using `dplyr` joins.
        #' The datasets can be in-memory (variable name) or the name of an currently stored 
        #'  Elasticsearch index. Joins cannot be done on column of type "list" ("by" 
        #'  argument).
        #'
        #' @param ... see `join()` params.
        #'
        #' @family joins
        #'
        #' @examples
        #' \dontrun{
        #' # prepare join datasets, fair cuts 
        #' fair_cut <- dplyr::filter(ggplot2::diamonds, cut == "Fair")  # 1605 lines
        #' sup_carat <- kc$pull("diamonds_superior")$diamonds_superior
        #' # execute a right_join with one index and one in-memory dataset
        #' kc$right_join(fair_cut, "diamonds_superior")
        #' # execute a right_join with one index queried, and one in-memory dataset
        #' kc$right_join(sup_carat, "diamonds", right_query 
        #'  = "cut:fair")
        #' }
        #'
        #' @return a tibble
        #'
        right_join = function(...) {
            res <- private$join(join_type = "right", ...)
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Execute a semi join between two datasets using `dplyr` joins.
        #' The datasets can be in-memory (variable name) or the name of an currently stored 
        #'  Elasticsearch index. Joins cannot be done on column of type "list" ("by" 
        #'  argument).
        #'
        #' @param ... see `join()` params.
        #'
        #' @family joins
        #'
        #' @examples
        #' \dontrun{
        #' # prepare join datasets, fair cuts 
        #' fair_cut <- dplyr::filter(ggplot2::diamonds, cut == "Fair")  # 1605 lines
        #' sup_carat <- kc$pull("diamonds_superior")$diamonds_superior
        #' # execute a semi_join with one index and one in-memory dataset
        #' kc$semi_join(fair_cut, "diamonds_superior")
        #' # execute a semi_join with one index queried, and one in-memory dataset
        #' kc$semi_join(sup_carat, "diamonds", right_query 
        #'  = "cut:fair")
        #' }
        #'
        #' @return a tibble
        #'
        semi_join = function(...) {
            res <- private$join(join_type = "semi", ...)
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Execute a anti join between two datasets using `dplyr` joins.
        #' The datasets can be in-memory (variable name) or the name of an currently stored 
        #'  Elasticsearch index. Joins cannot be done on column of type "list" ("by" 
        #'  argument).
        #'
        #' @param ... see `join()` params.
        #'
        #' @family joins
        #'
        #' @examples
        #' \dontrun{
        #' # prepare join datasets, fair cuts 
        #' fair_cut <- dplyr::filter(ggplot2::diamonds, cut == "Fair")  # 1605 lines
        #' sup_carat <- kc$pull("diamonds_superior")$diamonds_superior
        #' # execute a anti_join with one index and one in-memory dataset
        #' kc$anti_join(fair_cut, "diamonds_superior")
        #' # execute a anti_join with one index queried, and one in-memory dataset
        #' kc$anti_join(sup_carat, "diamonds", right_query 
        #'  = "cut:fair")
        #' # 
        #' # Do not mind this, removing example indices
        #' elastic::index_delete(kc$connection, "*")
        #' kc <- NULL
        #' }
        #'
        #' @return a tibble
        #'
        anti_join = function(...) {
            res <- private$join(join_type = "anti", ...)
            if(self$quiet_results) invisible(res) else res
        }
    )
)


# -------------------------------
# Kibior static methods
#

#'
#' @title Static - initiate a direct instance to Kibio public repository
#' @name Static - initiate a direct instance to Kibio public repository
#'
#' @details
#' Initiate a instance of Kibior connected to the Kibio public repository.
#'
#' @param verbose verbosity activation (default: FALSE)
#'
#' @family initiate
#'
#' @return a new instance of Kibior conencted to Kibio service
#'
Kibior$get_kibio_instance <- function(verbose = FALSE){
    kibio_endpoint <- "kibio.compbio.ulaval.ca"
    kibio_port <- 80L
    message("Trying to connect to Kibio servers on '", kibio_endpoint, ":", kibio_port, "'")
    tryCatch(expr = {
            k <- Kibior$new(kibio_endpoint, kibio_port, verbose = verbose)
            message("This instance grants you anonymous connection with read-only priviledges")
            k
        }, 
        error = function(e){
            e$message %>% 
                paste0("\nKibio servers seems not accessible.") %>% 
                paste0("\nPlease, try updating the KibioR package.") %>%
                stop()
        }
    )
}

#' 
#' @title Static - Kibior is instance
#' @name Static - Kibior is instance
#' 
#' @details
#' Tests if a given object is a Kibior instance.
#' Basically compute symmetric difference between two sets of class.
#'
#' @param obj an object
#'
#' @family comparison
#'
#' @return TRUE if the given object is an instance of Kibior, else FALSE
#' 
Kibior$is_instance <- function(obj){
    is(obj, Kibior$classname)
}

#' 
#' @title Static - Tests if packages are installed
#' @name Static - Tests if packages are installed
#' 
#' @details
#' Get the installation status of some package names (installed TRUE/FALSE).
#'
#' @param pkg_names a vector of some package names
#'
#' @family install
#'
#' @return a named vector of packages with installation status
#' 
Kibior$.is_installed <- function(pkg_names){
    if(!purrr::is_character(pkg_names)) stop("Need package names.")
    is_installed <- pkg_names %in% installed.packages()[,"Package"]
    names(is_installed) <- pkg_names
    is_installed
}


# -------------------------------
# Kibior operators
#

# allow to attach in the current env the "==" and "!=" operators for "KibiorOperators" derived 
#   classes

#' 
#' @title Kibior equals operator
#' @name Kibior equals operator
#' 
#' @details
#' Call kibioR `$eq()` for a comparison of the two instances.
#'
#' @param x the first Kibior instance
#' @param y the second Kibior instance
#'
#' @family comparison
#'
#' @return TRUE if the two instances are equals, else FALSE
#' 
`==.KibiorOperators` = function(x, y){ x$eq(y) }

#' 
#' @title Kibior not-equals operator
#' @name Kibior not-equals operator
#' 
#' @details
#' Call kibioR `$ne()` for a comparison of the two instances.
#'
#' @param x the first Kibior instance
#' @param y the second Kibior instance
#'
#' @family comparison
#'
#' @return TRUE if the two instances are differents, else FALSE
#' 
`!=.KibiorOperators` = function(x, y){ x$ne(y) }
