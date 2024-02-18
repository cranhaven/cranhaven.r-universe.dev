Quaternion <- R6::R6Class(
  classname = "Quaternion",
  portable = TRUE,
  cloneable = FALSE,
  lock_objects = FALSE,
  private = list(
    .extern_ptr = NULL
  ),
  active = list(
    is_quaternion = function() { TRUE },
    pointer = function() { private$.extern_ptr },
    x = function( v ) {
      if(!missing(v)) {
        v <- as.double(v)[[1]]
        Quaternion__setX(private$.extern_ptr, v)
      }
      Quaternion__getX(private$.extern_ptr)
    },
    y = function( v ) {
      if(!missing(v)) {
        v <- as.double(v)[[1]]
        Quaternion__setY(private$.extern_ptr, v)
      }
      Quaternion__getY(private$.extern_ptr)
    },
    z = function( v ) {
      if(!missing(v)) {
        v <- as.double(v)[[1]]
        Quaternion__setZ(private$.extern_ptr, v)
      }
      Quaternion__getZ(private$.extern_ptr)
    },
    w = function( v ) {
      if(!missing(v)) {
        v <- as.double(v)[[1]]
        Quaternion__setW(private$.extern_ptr, v)
      }
      Quaternion__getW(private$.extern_ptr)
    }
  ),
  public = list(
    initialize = function() {
      private$.extern_ptr <- Quaternion__new()
    },
    format = function(...) {
      q <- format(self$to_array())
      sprintf(
        "<Quaternion>\nx = %s, y = %s, z = %s, w = %s",
        q[[1]], q[[2]], q[[3]], q[[4]]
      )
    },
    clone2 = function(...) {
      Quaternion$new()$copy(self)
    },
    set = function(x, y, z, w) {
      data <- as.numeric(c(x, y, z, w))[1:4]
      Quaternion__set(private$.extern_ptr, data[[1]], data[[2]], data[[3]], data[[4]])
      self
    },
    to_array = function() {
      Quaternion__to_array(private$.extern_ptr)
    },
    copy = function(m) {
      stopifnot(R6::is.R6(m) && isTRUE(m$is_quaternion))
      Quaternion__copy(private$.extern_ptr, m$pointer)
      self
    },
    normalize = function() {
      Quaternion__normalize(private$.extern_ptr)
      self
    },
    set_from_axis_angle = function(axis, angle) {
      axis <- as_vector3(axis)
      angle <- as.numeric(angle)[[1]]
      Quaternion__set_from_axis_angle(private$.extern_ptr, axis$pointer, angle)
      self
    },
    set_from_rotation_matrix = function(m) {
      m <- as_matrix4(m)
      Quaternion__set_from_rotation_matrix(private$.extern_ptr, m$pointer)
      self
    },
    set_from_unit_vectors = function(v_from, v_to) {
      v_from <- as_vector3(v_from)
      v_to <- as_vector3(v_to)
      Quaternion__set_from_unit_vectors(private$.extern_ptr, v_from$pointer, v_to$pointer)
      self
    },
    angle_to = function(q) {
      q <- as_quaternion(q)
      Quaternion__angle_to(private$.extern_ptr, q$pointer)
    },
    rotate_towards = function(q, step) {
      q <- as_quaternion(q)
      step <- as.double(step)[[1]]
      Quaternion__rotate_towards(private$.extern_ptr, q$pointer, step)
      self
    },
    slerp = function(qb, t) {
      qb <- as_quaternion(qb)
      t <- as.double(t)[[1]]
      Quaternion__slerp(private$.extern_ptr, qb$pointer, t)
      self
    },
    identity = function() {
      Quaternion__identity(private$.extern_ptr)
      self
    },
    invert = function() {
      Quaternion__invert(private$.extern_ptr)
      self
    },
    conjugate = function() {
      Quaternion__conjugate(private$.extern_ptr)
      self
    },
    dot = function(q) {
      q <- as_quaternion(q)
      Quaternion__dot(private$.extern_ptr, q$pointer)
    },
    length_squared = function() {
      Quaternion__length_squared(private$.extern_ptr)
    },
    length = function() {
      Quaternion__length(private$.extern_ptr)
    },
    multiply = function(q) {
      q <- as_quaternion(q)
      Quaternion__multiply_quaternions(private$.extern_ptr, private$.extern_ptr, q$pointer)
      self
    },
    premultiply = function(q) {
      q <- as_quaternion(q)
      Quaternion__multiply_quaternions(private$.extern_ptr, q$pointer, private$.extern_ptr)
      self
    },
    multiply_quaternions = function(qa, qb) {
      qa <- as_quaternion(qa)
      qb <- as_quaternion(qb)
      Quaternion__multiply_quaternions(private$.extern_ptr, qa$pointer, qb$pointer)
      self
    }
  )
)

#' Create a \code{Quaternion} instance to store '3D' rotation
#' @description
#' Create instances that mimic the \code{'three.js'} syntax.
#' @param x,y,z,w numeric of length one
#' @param q R object to be converted to \code{Quaternion}
#' @returns A \code{Quaternion} instance
#' @seealso \code{\link{new_vector3}}, \code{\link{new_matrix4}}
#' @export
new_quaternion <- function(x = 0, y = 0, z = 0, w = 1) {
  Quaternion$new()$set(x, y, z, w)
}

#' @export
`[.Quaternion` <- function(x, i, ..., drop = TRUE) {
  if(missing(i)) {
    x$to_array()
  } else {
    x$to_array()[i]
  }
}


#' @export
`==.Quaternion` <- function(e1, e2) {
  e2 == e1$to_array()
}

#' @rdname new_quaternion
#' @export
as_quaternion <- function(q) {
  if(R6::is.R6(q) && isTRUE(q$is_quaternion)) { return(q) }
  if(length(q) != 4) {
    stop("`as_quaternion`: length of `q` must be a vector of 4 numbers.")
  }
  new_quaternion(q[[1]], q[[2]], q[[3]], q[[4]])
}
