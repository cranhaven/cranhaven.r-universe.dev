# hdf5_utils.R
# Internal HDF5 abstraction layer.
# Supports hdf5r (CRAN, preferred) and rhdf5 (Bioconductor) backends.

# Package-level cache for the detected HDF5 backend.
# Avoids repeated requireNamespace() calls on every HDF5 operation.
.hdf5_cache <- new.env(parent = emptyenv())

#' Detect available HDF5 backend (cached)
#' @return Character string: "hdf5r" or "rhdf5"
#' @noRd
.hdf5_backend <- function() {
  if (!is.null(.hdf5_cache$backend)) {
    return(.hdf5_cache$backend)
  }
  if (requireNamespace("hdf5r", quietly = TRUE)) {
    .hdf5_cache$backend <- "hdf5r"
  } else if (requireNamespace("rhdf5", quietly = TRUE)) {
    .hdf5_cache$backend <- "rhdf5"
  } else {
    stop("An HDF5 package is required. Install one with:\n",
         "  install.packages('hdf5r')       # recommended (CRAN)\n",
         "  BiocManager::install('rhdf5')    # alternative (Bioconductor)")
  }
  .hdf5_cache$backend
}

#' Create a new HDF5 file
#' @param path File path.
#' @return File handle.
#' @noRd
h5_create_file <- function(path) {
  # Remove existing file so we always start fresh (matches hdf5r "w" mode)
  if (file.exists(path)) {
    file.remove(path)
  }
  backend <- .hdf5_backend()
  if (backend == "hdf5r") {
    hdf5r::H5File$new(path, "w")
  } else {
    rhdf5::h5createFile(path)
    rhdf5::H5Fopen(path)
  }
}

#' Open an existing HDF5 file
#' @param path File path.
#' @return File handle.
#' @noRd
h5_open <- function(path) {
  backend <- .hdf5_backend()
  if (backend == "hdf5r") {
    hdf5r::H5File$new(path, "r+")
  } else {
    rhdf5::H5Fopen(path)
  }
}

#' Close an HDF5 file and all associated handles
#' @param handle File handle.
#' @noRd
h5_close <- function(handle) {
  backend <- .hdf5_backend()
  if (backend == "hdf5r") {
    handle$close_all()
  } else {
    rhdf5::h5closeAll()
  }
}

#' Create a group within an HDF5 file or group
#' @param parent File or group handle.
#' @param name Group path (relative to parent).
#' @return Group handle (invisibly).
#' @noRd
h5_create_group <- function(parent, name) {
  backend <- .hdf5_backend()
  if (backend == "hdf5r") {
    parent$create_group(name)
  } else {
    rhdf5::H5Gcreate(parent, name)
  }
}

#' Open an existing group
#' @param parent File or group handle.
#' @param name Group path (relative to parent).
#' @return Group handle.
#' @noRd
h5_open_group <- function(parent, name) {
  backend <- .hdf5_backend()
  if (backend == "hdf5r") {
    parent[[name]]
  } else {
    rhdf5::H5Gopen(parent, name)
  }
}

#' Close a group handle
#' @param handle Group handle.
#' @noRd
h5_close_group <- function(handle) {
  backend <- .hdf5_backend()
  if (backend == "hdf5r") {
    handle$close()
  } else {
    rhdf5::H5Gclose(handle)
  }
}

#' Read all attributes from a group in an HDF5 file
#' @param file_path Path to HDF5 file.
#' @param group_path Group path (without leading /).
#' @return Named list of attributes.
#' @noRd
h5_read_attrs <- function(file_path, group_path) {
  backend <- .hdf5_backend()
  if (backend == "hdf5r") {
    fid <- hdf5r::H5File$new(file_path, "r")
    on.exit(fid$close())
    grp <- fid[[group_path]]
    attr_names <- hdf5r::h5attr_names(grp)
    attrs <- lapply(attr_names, function(nm) hdf5r::h5attr(grp, nm))
    names(attrs) <- attr_names
    attrs
  } else {
    rhdf5::h5readAttributes(file_path, paste0("/", group_path))
  }
}

#' Write an attribute to an HDF5 object
#' @param handle File or group handle.
#' @param name Attribute name.
#' @param value Attribute value.
#' @noRd
h5_write_attr <- function(handle, name, value) {
  backend <- .hdf5_backend()
  if (backend == "hdf5r") {
    hdf5r::h5attr(handle, name) <- value
  } else {
    rhdf5::h5writeAttribute(h5obj = handle, attr = value, name = name)
  }
}

#' Write a dataset to an HDF5 group
#' @param parent Group handle.
#' @param name Dataset name.
#' @param data Data to write (data.frame, vector, matrix, etc.).
#' @noRd
h5_write_dataset <- function(parent, name, data) {
  backend <- .hdf5_backend()
  if (backend == "hdf5r") {
    parent[[name]] <- data
  } else {
    rhdf5::h5writeDataset(obj = data, h5loc = parent, name = name,
                          DataFrameAsCompound = TRUE)
  }
}

#' List top-level groups in an HDF5 file
#' @param file_path Path to HDF5 file.
#' @return Data frame with at least a \code{name} column.
#' @noRd
h5_ls <- function(file_path) {
  backend <- .hdf5_backend()
  if (backend == "hdf5r") {
    fid <- hdf5r::H5File$new(file_path, "r")
    on.exit(fid$close())
    fid$ls()
  } else {
    tmp <- rhdf5::h5ls(file_path, recursive = 1)
    tmp[tmp$group == "/", "name", drop = FALSE]
  }
}

#' List children of a specific group in an HDF5 file
#' @param file_path Path to HDF5 file.
#' @param group_path Group path (without leading /).
#' @return Character vector of child names.
#' @noRd
h5_ls_group <- function(file_path, group_path) {
  backend <- .hdf5_backend()
  if (backend == "hdf5r") {
    fid <- hdf5r::H5File$new(file_path, "r")
    on.exit(fid$close())
    grp <- fid[[group_path]]
    grp$ls()$name
  } else {
    tmp <- rhdf5::h5ls(file_path, recursive = TRUE)
    tmp[tmp$group == paste0("/", group_path), "name"]
  }
}
