Matrix4 <- R6::R6Class(
  classname = "Matrix4",
  portable = TRUE,
  cloneable = FALSE,
  lock_objects = FALSE,
  private = list(
    .extern_ptr = NULL
  ),
  active = list(
    is_matrix4 = function() { TRUE },
    pointer = function() { private$.extern_ptr },
    elements = function() {
      self$to_array()
    }
  ),
  public = list(
    initialize = function() {
      private$.extern_ptr <- Matrix4__new()
    },
    format = function(...) {
      m <- self$to_array()
      c(
        "<Matrix4>",
        utils::capture.output({
          print(m)
        })
      )
    },
    clone2 = function(...) {
      Matrix4$new()$copy(self)
    },
    set = function(n11, ..., byrow = TRUE) {
      if(is.matrix(n11)) {
        data <- n11
      } else {
        data <- matrix(c(n11, ...), nrow = 4, byrow = byrow)
      }
      if(!is.numeric(data)) {
        data <- as.numeric(data)
      }
      if(length(data) != 16) {
        stop("Matrix4$set() requires 16 elements")
      }
      Matrix4__from_array(private$.extern_ptr, data)
      self
    },
    to_array = function() {
      Matrix4__to_array(private$.extern_ptr)
    },
    identity = function() {
      Matrix4__identity(private$.extern_ptr)
      self
    },
    copy = function(m) {
      stopifnot(R6::is.R6(m) && isTRUE(m$is_matrix4))
      Matrix4__copy(private$.extern_ptr, m$pointer)
      self
    },
    copy_position = function(m) {
      m <- as_matrix4(m)
      Matrix4__copy_position(private$.extern_ptr, m$pointer)
      self
    },
    extract_basis = function(x_axis = NULL, y_axis = NULL, z_axis = NULL) {
      if(is.null(x_axis)) {
        x_axis <- new_vector3()
      } else {
        x_axis <- as_vector3(x_axis)
      }
      if(is.null(y_axis)) {
        y_axis <- new_vector3()
      } else {
        y_axis <- as_vector3(y_axis)
      }
      if(is.null(z_axis)) {
        z_axis <- new_vector3()
      } else {
        z_axis <- as_vector3(z_axis)
      }
      Matrix4__extract_basis(private$.extern_ptr, x_axis$pointer, y_axis$pointer, z_axis$pointer)
      self
    },
    make_basis = function(x_axis = NULL, y_axis = NULL, z_axis = NULL) {
      if(is.null(x_axis)) {
        x_axis <- new_vector3()
      } else {
        x_axis <- as_vector3(x_axis)
      }
      if(is.null(y_axis)) {
        y_axis <- new_vector3()
      } else {
        y_axis <- as_vector3(y_axis)
      }
      if(is.null(z_axis)) {
        z_axis <- new_vector3()
      } else {
        z_axis <- as_vector3(z_axis)
      }
      Matrix4__make_basis(private$.extern_ptr, x_axis$pointer, y_axis$pointer, z_axis$pointer)
      self
    },
    extract_rotation = function(m) {
      m <- as_matrix4(m)
      Matrix4__extract_rotation(private$.extern_ptr, m$pointer)
      self
    },
    look_at = function(eye, target, up) {
      eye <- as_vector3(eye)
      target <- as_vector3(target)
      up <- as_vector3(up)
      Matrix4__look_at(private$.extern_ptr, eye$pointer, target$pointer, up$pointer)
      self
    },
    multiply_matrices = function(a, b) {
      a <- as_matrix4(a)
      b <- as_matrix4(b)
      Matrix4__multiply_matrices(private$.extern_ptr, a$pointer, b$pointer)
      self
    },
    multiply = function(m) {
      m <- as_matrix4(m)
      Matrix4__multiply_matrices(private$.extern_ptr, private$.extern_ptr, m$pointer)
      self
    },
    premultiply = function(m) {
      m <- as_matrix4(m)
      Matrix4__multiply_matrices(private$.extern_ptr, m$pointer, private$.extern_ptr)
      self
    },
    multiply_scalar = function(scalar) {
      if(!is.numeric(scalar)) {
        scalar <- as.numeric(scalar[[1]])
      } else {
        scalar <- scalar[[1]]
      }
      Matrix4__multiply_scalar(private$.extern_ptr, scalar)
      self
    },
    determinant = function() {
      Matrix4__determinant(private$.extern_ptr)
    },
    transpose = function() {
      Matrix4__transpose(private$.extern_ptr)
      self
    },
    set_position = function(v, ...) {
      if(R6::is.R6(v) && isTRUE(v$is_vector3)) {
        v <- as.numeric(v[,1])
      } else {
        v <- as.numeric(c(v, ...))[1:3]
      }
      Matrix4__set_position(private$.extern_ptr, v[[1]], v[[2]], v[[3]])
      self
    },
    invert = function() {
      Matrix4__invert(private$.extern_ptr)
      self
    },
    scale = function(v, ...) {
      if(!R6::is.R6(v) || !isTRUE(v$is_vector3)) {
        v <- as_vector3(c(v, ...))
      }
      Matrix4__scale(private$.extern_ptr, v$pointer)
      self
    },
    get_max_scale_on_axis = function() {
      Matrix4__get_max_scale_on_axis(private$.extern_ptr)
    },
    make_translation = function(x, y = NULL, z = NULL) {
      if(R6::is.R6(x) && isTRUE(x$is_vector3)) {
        x <- as.numeric(x[,1])
      } else {
        x <- as.numeric(c(x, y, z))[1:3]
      }
      Matrix4__make_translation(private$.extern_ptr, x[[1]], x[[2]], x[[3]])
      self
    },
    make_rotation_x = function(theta) {
      theta <- as.double(theta)[[1]]
      Matrix4__make_rotation_x(private$.extern_ptr, theta)
      self
    },
    make_rotation_y = function(theta) {
      theta <- as.double(theta)[[1]]
      Matrix4__make_rotation_y(private$.extern_ptr, theta)
      self
    },
    make_rotation_z = function(theta) {
      theta <- as.double(theta)[[1]]
      Matrix4__make_rotation_z(private$.extern_ptr, theta)
      self
    },
    make_rotation_axis = function(axis, angle) {
      axis <- as_vector3(axis)
      angle <- as.double(angle)[[1]]
      Matrix4__make_rotation_axis(private$.extern_ptr, axis$pointer, angle)
      self
    },
    make_scale = function(x, y = NULL, z = NULL) {
      if(R6::is.R6(x) && isTRUE(x$is_vector3)) {
        x <- as.numeric(x[,1])
      } else {
        x <- as.numeric(c(x, y, z))[1:3]
      }
      Matrix4__make_scale(private$.extern_ptr, x[[1]], x[[2]], x[[3]])
      self
    },
    make_shear = function(xy, xz, yx, yz, zx, zy) {
      xy <- as.double(xy)[[1]]
      xz <- as.double(xz)[[1]]
      yx <- as.double(yx)[[1]]
      yz <- as.double(yz)[[1]]
      zx <- as.double(zx)[[1]]
      zy <- as.double(zy)[[1]]
      Matrix4__make_shear(private$.extern_ptr, xy, xz, yx, yz, zx, zy)
      self
    },
    make_perpective = function(left, right, top, bottom, near, far) {
      left <- as.double(left)[[1]]
      right <- as.double(right)[[1]]
      top <- as.double(top)[[1]]
      bottom <- as.double(bottom)[[1]]
      near <- as.double(near)[[1]]
      far <- as.double(far)[[1]]
      Matrix4__make_perspective(private$.extern_ptr, left, right, top, bottom, near, far)
      self
    },
    make_orthographic = function(left, right, top, bottom, near, far) {
      left <- as.double(left)[[1]]
      right <- as.double(right)[[1]]
      top <- as.double(top)[[1]]
      bottom <- as.double(bottom)[[1]]
      near <- as.double(near)[[1]]
      far <- as.double(far)[[1]]
      Matrix4__make_orthographic(private$.extern_ptr, left, right, top, bottom, near, far)
      self
    }
  )
)

#' Create a \code{Matrix4} instance for \code{'Affine'} transform
#' @param m a matrix or a vector to be converted to the \code{Matrix4} instance;
#' \code{m} must be one of the followings: for matrices, the dimension must be
#' \code{4x4}, \code{3x4} (the last row will be \code{0 0 0 1}), or
#' \code{3x3} (linear transform); for vectors, the length must be
#' \code{16}, \code{12} (will append \code{0 0 0 1} internally),
#' \code{3} (translation), or \code{1} (scale).
#' @returns A \code{Matrix4} instance
#' @seealso \code{\link{new_vector3}}, \code{\link{new_quaternion}}
#' @export
new_matrix4 <- function() {
  Matrix4$new()
}

#' @export
as.matrix.Matrix4 <- function(x, ...) {
  if(R6::is.R6(x) && isTRUE(x$is_matrix4)) {
    return(x$to_array())
  }
  NextMethod("as.matrix")
}

#' @export
`[.Matrix4` <- function(x, i, ..., drop = TRUE) {
  if(missing(i)) {
    x$to_array()[, ..., drop = drop]
  } else {
    x$to_array()[i, ..., drop = drop]
  }
}

#' @export
dim.Matrix4 <- function(x) {
  if(R6::is.R6(x) && isTRUE(x$is_matrix4)) {
    return(c(4L, 4L))
  }
  NextMethod("dim")
}

#' @export
`==.Matrix4` <- function(e1, e2) {
  e2 == e1$to_array()
}

#' @rdname new_matrix4
#' @export
as_matrix4 <- function(m) {
  if(R6::is.R6(m) && isTRUE(m$is_matrix4)) { return(m) }
  if(is.matrix(m)) {
    nc <- ncol(m)
    nr <- nrow(m)
    if(!nc %in% c(3, 4)) {
      stop("`as_matrix4`: when input `m` is a matrix, its column numbers must be 3 or 4")
    }
    if(!nr %in% c(3, 4)) {
      stop("`as_matrix4`: when input `m` is a matrix, its row numbers must be 3 or 4")
    }
    if( nc == 3 && nr == 3 ) {
      m <- cbind(rbind(m, c(0, 0, 0)), c(0, 0, 0, 1))
    } else if ( nc == 3 ) {
      m <- cbind(m, c(0, 0, 0, 1))
    } else if ( nr == 3 ){
      m <- rbind(m, c(0, 0, 0, 1))
    }
    return(new_matrix4()$set(m))
  }
  m <- as.numeric(m)
  if(length(m) == 12) {
    m <- c(m, 0, 0, 0, 1)
  } else if(length(m) == 3) {
    m <- matrix(c(
      1,0,0,m[[1]],
      0,1,0,m[[2]],
      0,0,1,m[[3]],
      0,0,0,1
    ), nrow = 4, byrow = TRUE)
    return(new_matrix4()$set(m))
  }
  new_matrix4()$set(m)
}
