#' @keywords internal
#' @importFrom XML free

# xml class based on file_document class
setClass(
  "xml_document",
  contains = c("file_document"),
  validity = valid_doc
)


# object validation
setMethod(
  "valid_doc", signature(object = "xml_document"),
  function(object) {

    ext <- get_ext(object)

    if (length(object@name) == 0 || object@name == "") {
      return("file name is empty !")
    }
    if (!length(ext) || !ext == "xml") {
      return("Error: no xml extension")
    }
    if (!exist(object)) {
      return(paste("Error: file doesn't exist:", object@name, "!"))
    }
    if (exist(object) && isempty(object)) {
      return(paste("Error: file is empty:", object@name, "!"))
    }
    TRUE
  })


# constructor
setMethod(
  "xmldocument", signature(file = "character"),
  function(file = character(length = 0)) {
    suppressMessages(methods::new("xml_document", file = file))
  }
)

setMethod(
  "initialize", "xml_document",
  function(.Object, file = character(length = 0), warn = FALSE) {
    .Object <- methods::callNextMethod(.Object, file)
    methods::validObject(.Object)
    .Object <- load_content(.Object)
    return(.Object)
  }
)


setMethod("show", "xml_document", function(object) {
  methods::callNextMethod()
  if (is_loaded(object)) {
    print(paste0("   content : ", class(object@content)[[1]]))
  }
})

# setter methods
setReplaceMethod(
  "set_content", signature(object = "xml_document"),
  function(object, value) {
    if (!methods::is(value, "XMLInternalDocument")) {
      stop("Input value is not a XMLInternalDocument class object")
    }
    object@content <- value
    return(object)
  }
)

# getter methods
setMethod(
  "get_content", signature(object = "xml_document"),
  function(object) {
    return(object@content)
  }
)


setMethod(
  "get_nodes", signature(object = "xml_document"),
  function(object, path = NULL) {
    node_set <- NULL
    if (!is_loaded(object)) {
      stop("xml file is not loaded in xml_document object")
    }

    if (base::is.null(path)) {
      # getting root node name to get corresponding node set
      node_set <- XML::getNodeSet(
        object@content,
        paste0("/", XML::xmlName(XML::xmlRoot(object@content)))
      )
    } else {
      node_set <- XML::getNodeSet(object@content, path[1])
      # For a path vector, a loop is necessary
      # to keep results according to the path order !
      path_nb <- length(path)
      if (path_nb > 1) {
        for (i in 2:path_nb) {
          node_set <- merge_nodesets(
            node_set,
            XML::getNodeSet(object@content, path[i])
          )
        }
      }
    }

    if (!length(node_set)) {
      if (object@warn) warning("Node set is empty, check xml input path !")
      node_set <- NULL
    }
    return(node_set)
  }
)

setMethod(
  "get_attrs", signature(object = "xml_document"),
  function(object, path) {
    attr_list <- NULL
    node_set <- get_nodes(object, path)
    #

    if (base::is.null(node_set)) {
      return(attr_list)
    }

    # tranforming attributes to matrix (if only one attribute, a character
    # vector is returned)
    attr_list <- sapply(node_set, function(x) XML::xmlAttrs(x))
    # not any attributes in nodeset
    if (base::is.null(unlist(attr_list))) {
      return()
    }

    if (is.character(attr_list) & !is.matrix(attr_list)) {
      new_list <- vector(mode = "list", length(attr_list))
      for (i in seq_along(attr_list)) {
        new_list[[i]] <- attr_list[i]
      }
      attr_list <- new_list
    }

    if (is.list(attr_list)) {
      attr_list <- attributes_list2matrix(attr_list)
    } else {
      attr_list <- t(attr_list)
    }


    # testing if any node has not any attribute
    any_null <- any(sapply(attr_list, function(x) base::is.null(x)))
    if (any_null && object@warn) {
      warning(paste("Existing nodes without any attributes on xpath", path))
    }


    # testing if all nodes have the same attributes !!
    if (!is.matrix(attr_list) && !is.matrix(attr_list[, ])) {
      if (object@warn) {
        message(class(attr_list))
        warning(paste(
          "Existing nodes with different attributes comparing to others,",
          "missing attributes ?",
          path
        ))
      }
    }

    return(attr_list)
  }
)

# get_attrs_names
setMethod(
  "get_attrs_names", signature(object = "xml_document"),
  function(object, path) {
    attr_names <- NULL
    attr_list <- get_attrs(object, path)

    dim_names <- dimnames(attr_list)
    if (!base::is.null(dim_names[[1]])) {
      attr_names <- dim_names[[1]]
    } else {
      attr_names <- dim_names[[2]]
    }

    return(attr_names)
  }
)


# get_attrs_values
setMethod(
  "get_attrs_values", signature(object = "xml_document"),
  function(object, path, attr_list = character(length = 0),
           nodes_ids = NULL) {
    sel_values <- NULL

    # getting attributes values from doc
    attr_values <- get_attrs(object, path)

    # no attributes for the query
    if (base::is.null(attr_values)) {
      return(sel_values)
    }

    # selecting outputs
    # empty attr_list
    if (length(attr_list) == 0) {
      return(attr_values)
    }


    # finding existing attr names in path
    sel <- is.element(colnames(attr_values), attr_list)

    if (!any(sel)) {
      # not any given attr_list names exist in path
      if (object@warn) {
        warning(paste(
          "Not any given attribute name exist in ", path,
          "aborting !"
        ))
        return()
      }
    }

    # getting existing names from attr_list in attr_values
    # and getting the original order in initial attr_list
    found_list <- intersect(colnames(attr_values)[sel], attr_list)
    sel_list <- attr_list[is.element(attr_list, found_list)]

    # selecting wanted attributes columns
    # by the col names
    sel_values <- attr_values[, sel_list]

    # If only one column selected, restoring matrix format
    if (length(sel_list) == 1) {
      sel_values <- as.matrix(sel_values)
      colnames(sel_values) <- sel_list
    }

    # keeping only lines specified by nodes_ids
    if (!base::is.null(nodes_ids)) {
      if (max(nodes_ids) <= dim(sel_values)[1]) {
        sel_values <- sel_values[nodes_ids, ]
      } else {
        stop("Subscript out of range, check ids !")
      }
    }

    return(sel_values)
  }
)


# factoriser avec get_attrs!! + getNode(object,path,kind)
setMethod(
  "get_values", signature(object = "xml_document"),
  function(object, path, nodes_ids = NULL) {
    node_set <- get_nodes(object, path)

    # getting nodes number
    nodes_nb <- length(node_set)

    if (nodes_nb == 0) {
      return(invisible())
    }


    if (!base::is.null(nodes_ids)) {
      if (max(nodes_ids) <= nodes_nb) {
        node_set <- node_set[nodes_ids]
      } else {
        stop("Subscript out of range, check ids !")
      }
    }

    # Getting values from the node_set
    val_list <- unlist(lapply(node_set, function(x) XML::xmlValue(x)))

    return(val_list)
  }
)


# add_attrs
setMethod(
  "add_attrs", signature(object = "xml_document"),
  function(object, path, named_vector) {
    # add not is nullnull node_set !!!!
    if (!base::is.null(names(named_vector))) {
      node_set <- get_nodes(object, path)
      invisible(sapply(node_set, function(x) XML::xmlAttrs(x) <- named_vector))
    }
  }
)


# delete attributes
# delAttrs
# TODO: to remove all attrs !!!!!!!!!
setMethod(
  "remove_attrs", signature(object = "xml_document"),
  function(object, path, attr_names) {
    # add not base::is.null node_set !!!!
    if (!base::is.null(attr_names)) {
      node_set <- get_nodes(object, path)
      attr_nb <- length(attr_names)
      for (i in 1:attr_nb) {
        sapply(node_set, function(x) XML::removeAttributes(x, attr_names[i]))
      }
    }
  }
)


# Setters
#
# TODO : same code as set_values,
setMethod(
  "set_attrs_values", signature(object = "xml_document"),
  function(object, path, attr_name, values_list, nodes_ids = NULL) {
    node_set <- get_nodes(object, path)
    if (base::is.null(node_set)) {
      return(invisible())
    }

    if (!base::is.null(nodes_ids)) {
      node_set <- node_set[nodes_ids]
    }

    nodes_nb <- length(node_set)

    if (length(values_list) == 1) {
      values_list <- rep(values_list, nodes_nb)
    }
    values_nb <- length(values_list)
    if (values_nb != nodes_nb) {
      stop("Values number is not consistent with nodes number !")
    }
    for (i in 1:nodes_nb) {
      value <- values_list[[i]]
      XML::xmlAttrs(node_set[[i]])[[attr_name]] <- value
    }
  }
)


# set_values
setMethod(
  "set_values", signature(object = "xml_document"),
  function(object, path, values_list, nodes_ids = NULL) {
    node_set <- get_nodes(object, path)

    if (base::is.null(node_set)) {
      return(invisible())
    }

    if (!base::is.null(nodes_ids)) {
      node_set <- node_set[nodes_ids]
    }

    nodes_nb <- length(node_set)


    if (length(values_list) == 1) {
      values_list <- rep(values_list, nodes_nb)
    }
    values_nb <- length(values_list)

    if (values_nb != nodes_nb) {
      stop("Values number is not consistent with nodes number to be modified !")
    }

    for (i in 1:nodes_nb) {
      XML::xmlValue(node_set[[i]]) <- values_list[[i]]
    }
  }
)

#

# insert after ?????

# addNodes
setMethod(
  "add_nodes", signature(object = "xml_document"),
  function(object, nodes_to_add, parent_path = NULL) {
    # parent node is root node
    if (base::is.null(parent_path)) {
      pnode <- XML::xmlRoot(object@content)
      # getting parent node from given parent_path
    } else {
      node_set <- get_nodes(object, parent_path)

      if (base::is.null(node_set)) {
        return()
      }
      pnode <- node_set[[1]]
    }
    # for a node set
    if (class(nodes_to_add)[[1]] == "XMLNodeSet") {
      for (n in seq_along(nodes_to_add)) {
        XML::addChildren(pnode, nodes_to_add[[n]])
      }
      # for a single node
    } else {
      XML::addChildren(pnode, nodes_to_add)
    }
  }
)


# removeNodes
setMethod(
  "del_nodes", signature(object = "xml_document"),
  function(object, path) {
    node_set <- get_nodes(object, path)

    if (base::is.null(node_set)) {
      return()
    }

    XML::removeNodes(node_set)
  }
)


# other methods
setMethod(
  "load_content", signature(object = "xml_document"),
  function(object) {

    set_content(object) <- XML::xmlParse(get_path(object))
    return(object)
  }
)


setMethod(
  "is_loaded", signature(object = "xml_document"),
  function(object) {
    return(methods::is(object@content, "XMLInternalDocument"))
  }
)

setMethod(
  "is.xml_document", signature(object = "ANY"),
  function(object) {
    if (methods::is(object, "xml_document")) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
)


# save method
setMethod(
  "save_xml_doc", signature(object = "xml_document"),
  function(object, xml_path) {
    if (is_loaded(object)) {
      write(XML::saveXML(object@content), xml_path)
    }
  }
)

# clone method
setMethod(
  "clone_xml_doc", signature(object = "xml_document"),
  function(object) {
    if (!is_loaded(object)) {
      return(NULL)
    }

    set_content(object) <- XML::xmlClone(get_content(object))

    return(object)
  }
)

# delete object method
setMethod(
  "delete", signature(object = "xml_document"),
  function(object) {
    # freeing object @content slot (external pointer)
    # and deleting xml_document object
    free(object@content)

    rm(object)
    invisible(gc(verbose = FALSE))

  }
)
