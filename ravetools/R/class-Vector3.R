Vector3 <- R6::R6Class(
  classname = "Vector3",
  portable = TRUE,
  cloneable = FALSE,
  lock_objects = FALSE,
  private = list(
    .extern_ptr = NULL
  ),
  active = list(
    is_vector3 = function() { TRUE },
    pointer = function() { private$.extern_ptr }
  ),
  public = list(
    initialize = function() {
      private$.extern_ptr <- Vector3__new()
    },
    format = function(...) {
      l <- self$get_size()
      s <- sprintf("<Vector3: len=%d>", l)
      if(l == 0) { return(s) }
      arr <- self$to_array(max_n_elems = 5)
      s <- c(
        s,
        utils::capture.output({
          print(data.frame(
            x = arr[1, ],
            y = arr[2, ],
            z = arr[3, ]
          ), ...)
        })
      )

      if( l > 5 ) {
        s <- c(s, "...")
      }
      s
    },
    clone2 = function(...) {
      Vector3$new()$copy(self)
    },
    set = function(x, y, z) {
      data <- as.numeric(rbind(x, y, z))
      Vector3__from_array(private$.extern_ptr, data)
      self
    },
    from_array = function(data, offset = 0L, n_elems = -1L) {
      Vector3__from_array(private$.extern_ptr,
                          as.numeric(data),
                          offset = as.integer(offset),
                          n_elems = as.integer(n_elems))
      self
    },
    resize = function(n_elems) {
      Vector3__resize(private$.extern_ptr, n_elems = as.integer(n_elems))
      self
    },
    get_size = function() {
      Vector3__get_size(private$.extern_ptr)
    },
    to_array = function(n_skip = 0L, max_n_elems = -1L) {
      Vector3__to_array(private$.extern_ptr, n_skip = as.integer(n_skip), max_n_elems = as.integer(max_n_elems))
    },
    set_scalar = function(value) {
      Vector3__set_scalar(private$.extern_ptr, as.numeric(value))
      self
    },
    set_x = function(value) {
      Vector3__set_x(private$.extern_ptr, as.numeric(value))
      self
    },
    set_y = function(value) {
      Vector3__set_y(private$.extern_ptr, as.numeric(value))
      self
    },
    set_z = function(value) {
      Vector3__set_z(private$.extern_ptr, as.numeric(value))
      self
    },
    get_x = function(i) {
      Vector3__get_x(private$.extern_ptr, as.integer(i))
    },
    get_y = function(i) {
      Vector3__get_y(private$.extern_ptr, as.integer(i))
    },
    get_z = function(i) {
      Vector3__get_z(private$.extern_ptr, as.integer(i))
    },
    get_item = function(i) {
      Vector3__get_item(private$.extern_ptr, as.integer(i))
    },
    copy = function(v) {
      stopifnot(R6::is.R6(v) && isTRUE(v$is_vector3))
      Vector3__copy(private$.extern_ptr, v$pointer)
      self
    },
    add = function(v) {
      v <- as_vector3(v)
      Vector3__add(private$.extern_ptr, v$pointer)
      self
    },
    add_scalar = function(s) {
      Vector3__add_scalar(private$.extern_ptr, as.numeric(s))
      self
    },
    add_vectors = function(a, b) {
      a <- as_vector3(a)
      b <- as_vector3(b)
      Vector3__add_vectors(private$.extern_ptr, a$pointer, b$pointer)
      self
    },
    add_scaled = function(v, s) {
      v <- as_vector3(v)
      Vector3__add_scaled(private$.extern_ptr, v$pointer, as.numeric(s))
      self
    },
    sub = function(v) {
      v <- as_vector3(v)
      Vector3__sub(private$.extern_ptr, v$pointer)
      self
    },
    sub_scalar = function(s) {
      Vector3__sub_scalar(private$.extern_ptr, as.numeric(s))
      self
    },
    sub_vectors = function(a, b) {
      a <- as_vector3(a)
      b <- as_vector3(b)
      Vector3__sub_vectors(private$.extern_ptr, a$pointer, b$pointer)
      self
    },
    multiply = function(v) {
      v <- as_vector3(v)
      Vector3__multiply(private$.extern_ptr, v$pointer)
      self
    },
    multiply_scalar = function(s) {
      Vector3__multiply_scalar(private$.extern_ptr, as.numeric(s))
      self
    },
    multiply_vectors = function(a, b) {
      a <- as_vector3(a)
      b <- as_vector3(b)
      Vector3__multiply_vectors(private$.extern_ptr, a$pointer, b$pointer)
      self
    },
    divide = function(v) {
      v <- as_vector3(v)
      Vector3__divide(private$.extern_ptr, v$pointer)
      self
    },
    divide_scalar = function(s) {
      Vector3__divide_scalar(private$.extern_ptr, as.numeric(s))
      self
    },
    min = function(v) {
      v <- as_vector3(v)
      Vector3__min(private$.extern_ptr, v$pointer)
      self
    },
    max = function(v) {
      v <- as_vector3(v)
      Vector3__max(private$.extern_ptr, v$pointer)
      self
    },
    clamp = function(min_v, max_v) {
      min_v <- as_vector3(min_v)
      max_v <- as_vector3(max_v)
      Vector3__clamp(private$.extern_ptr, min_v$pointer, max_v$pointer)
      self
    },
    floor = function() {
      Vector3__floor(private$.extern_ptr)
      self
    },
    ceil = function() {
      Vector3__ceil(private$.extern_ptr)
      self
    },
    round = function() {
      Vector3__round(private$.extern_ptr)
      self
    },
    round_to_zero = function() {
      Vector3__round_to_zero(private$.extern_ptr)
      self
    },
    negate = function() {
      Vector3__negate(private$.extern_ptr)
      self
    },
    dot = function(v) {
      v <- as_vector3(v)
      Vector3__dot(private$.extern_ptr, v$pointer)
    },
    length_squared = function() {
      Vector3__length_squared(private$.extern_ptr)
    },
    length = function() {
      Vector3__length(private$.extern_ptr)
    },
    length_manhattan = function() {
      Vector3__length_manhattan(private$.extern_ptr)
    },
    normalize = function() {
      Vector3__normalize(private$.extern_ptr)
      self
    },
    set_length = function(len) {
      Vector3__set_length(private$.extern_ptr, as.numeric(len))
      self
    },
    lerp = function(v, alpha) {
      v <- as_vector3(v)
      Vector3__lerp(private$.extern_ptr, v$pointer, as.numeric(alpha))
      self
    },
    lerp_vectors = function(v1, v2, alpha) {
      v1 <- as_vector3(v1)
      v2 <- as_vector3(v2)
      Vector3__lerp_vectors(private$.extern_ptr, v1$pointer, v2$pointer, as.numeric(alpha))
      self
    },
    cross = function(v) {
      v <- as_vector3(v)
      Vector3__cross(private$.extern_ptr, v$pointer)
      self
    },
    cross_vectors = function(a, b) {
      a <- as_vector3(a)
      b <- as_vector3(b)
      Vector3__cross_vectors(private$.extern_ptr, a$pointer, b$pointer)
      self
    },

    apply_matrix3 = function(m) {
      stopifnot(length(m) == 9)
      if(!is.numeric(m)) { m <- as.numeric(m) }
      Vector3__apply_matrix3(private$.extern_ptr, m)
      self
    },
    apply_matrix4 = function(m) {
      m <- as_matrix4(m)
      Vector3__apply_matrix4(private$.extern_ptr, m$pointer)
      self
    },
    apply_quaternion = function(q) {
      q <- as_quaternion(q)
      Vector3__apply_quaternion(private$.extern_ptr, q$pointer)
      self
    },
    apply_axis_angle = function(axis, angle) {
      axis <- as_vector3(axis)
      angle <- as.double(angle)[[1]]
      Vector3__apply_axis_angle(axis, angle)
      self
    },
    transform_direction = function(m) {
      m <- as_matrix4(m)
      Vector3__transform_direction(private$.extern_ptr, m$pointer)
      self
    },
    project_on_vector = function(v) {
      v <- as_vector3(v)
      Vector3__project_on_vector(private$.extern_ptr, v$pointer)
      self
    },
    project_on_plane = function(normal) {
      normal <- as_vector3(normal)
      Vector3__project_on_plane(private$.extern_ptr, normal$pointer)
      self
    },
    reflect = function(normal) {
      normal <- as_vector3(normal)
      Vector3__reflect(private$.extern_ptr, normal$pointer)
      self
    },
    angle_to = function(v) {
      v <- as_vector3(v)
      return ( Vector3__angle_to(private$.extern_ptr, v$pointer) )
    },
    distance_to = function(v) {
      v <- as_vector3(v)
      return ( Vector3__distance_to(private$.extern_ptr, v$pointer) )
    },
    distance_to_squared = function(v) {
      v <- as_vector3(v)
      return ( Vector3__distance_to_squared(private$.extern_ptr, v$pointer) )
    },
    distance_to_manhattan = function(v) {
      v <- as_vector3(v)
      return ( Vector3__distance_to_manhattan(private$.extern_ptr, v$pointer) )
    },
    set_from_matrix_position = function(m) {
      m <- as_matrix4(m)
      Vector3__set_from_matrix_position(private$.extern_ptr, m$pointer)
      self
    },
    set_from_matrix_scale = function(m) {
      m <- as_matrix4(m)
      Vector3__set_from_matrix_scale(private$.extern_ptr, m$pointer)
      self
    },
    set_from_spherical_coords = function(radius, phi, theta) {
      Vector3__set_from_spherical_coords(private$.extern_ptr,
                                         as.numeric(radius)[[1]],
                                         as.numeric(phi)[[1]],
                                         as.numeric(theta)[[1]])
      self
    }
  )
)

#' Create a \code{Vector3} instance to store '3D' points
#' @description
#' Create instances that mimic the \code{'three.js'} syntax.
#'
#' @param x,y,z numeric, must have the same length, \code{'xyz'} positions
#' @param v R object to be converted to \code{Vector3} instance
#' @returns A \code{Vector3} instance
#'
#' @examples
#'
#' vec3 <- new_vector3(
#'   x = 1:9,
#'   y = 9:1,
#'   z = rep(c(1,2,3), 3)
#' )
#'
#' vec3[]
#'
#' # transform
#' m <- new_matrix4()
#'
#' # rotation xy plane by 30 degrees
#' m$make_rotation_z(pi / 6)
#'
#' vec3$apply_matrix4(m)
#'
#' vec3[]
#'
#' as_vector3(c(1,2,3))
#'
#' @seealso \code{\link{new_matrix4}}, \code{\link{new_quaternion}}
#' @export
new_vector3 <- function(x = 0.0, y = 0.0, z = 0.0) {
  Vector3$new()$set(x = x, y = y, z = z)
}

#' @export
as.matrix.Vector3 <- function(x, ...) {
  if(R6::is.R6(x) && isTRUE(x$is_vector3)) {
    return(x$to_array(...))
  }
  NextMethod("as.matrix")
}

#' @export
`[.Vector3` <- function(x, i, ..., drop = TRUE) {
  if(missing(i)) {
    x$to_array()[, ..., drop = drop]
  } else {
    x$to_array()[i, ..., drop = drop]
  }
}

#' @export
dim.Vector3 <- function(x) {
  if(R6::is.R6(x) && isTRUE(x$is_vector3)) {
    return(c(3L, x$get_size()))
  }
  NextMethod("dim")
}

#' @export
`==.Vector3` <- function(e1, e2) {
  e2 == e1$to_array()
}

#' @rdname new_vector3
#' @export
as_vector3 <- function(v) {
  if( R6::is.R6(v) && isTRUE(v$is_vector3) ) { return(v) }
  x_ <- as.double(v)
  if(length(x_) == 1) {
    return(new_vector3(x_, x_, x_))
  }
  if(length(x_) < 3) {
    stop("Input cannot be converted to a Vector3 instance. Please make sure input is a numeric vector of which the length is greater equal to 3.")
  }
  new_vector3()$from_array(x_)
}
