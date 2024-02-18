test_that("Vector3", {

  library(testthat)
  x <- array(1:30, c(3, 10)) + 0.3
  v0 <- matrix(x[3:29], nrow = 3)
  v <- new_vector3()
  v$from_array(x, offset = 2)

  expect_equal(v$to_array(), v0)
  expect_equal(v$to_array(3, 3), v0[, 4:6])

  expect_equal(v$get_size(), ncol(v0))

  expect_error(v$resize(0))

  v$resize(6)
  v0 <- v0[,1:6]
  expect_equal(v$to_array(0,-1), v0)
  expect_equal(v$to_array(3, 3), v0[, 4:6])
  expect_equal(v$get_size(), ncol(v0))

  expect_equal(range(v$set_scalar(0.1)$to_array()), c(0.1, 0.1))

  v1 <- v$set_x(0.2)
  expect_equal(as.vector(v1$to_array(n_skip = 0, max_n_elems = 1)), c(0.2, 0.1, 0.1))
  v$set_y(0.3)
  v$set_z(0.4)
  expect_equal(as.vector(v1$to_array(n_skip = 3, max_n_elems = 1)), c(0.2, 0.3, 0.4))

  v$from_array(v0)
  expect_equal(as.vector(v1$to_array(n_skip = 3, max_n_elems = 1)),
               v$get_item(i = 4))

  v1 <- new_vector3()
  v1$copy(v)

  expect_equal(v1$to_array(), v0)

  v1$copy(v1)
  expect_equal(v1$to_array(), v0)

  expect_equal(v$get_x(4), v0[1,4])
  expect_equal(v$get_y(5), v0[2,5])
  expect_equal(v$get_z(6), v0[3,6])
  expect_equal(v$get_item(3), v0[,3])

  expect_equal(v1$add(v)[], v0*2)
  expect_equal(v1$add(v1)[], v0*4)

  v1$copy(v)
  expect_equal(v1$add_scalar(0.1)[], v0 + 0.1)
  expect_error(v1$add_scalar(c(1,2)))

  s <- seq_len(ncol(v)) / 10
  ss <- t(replicate(3, seq_len(ncol(v)) / 10))
  expect_equal(v1$add_scalar(s)[], v0 + 0.1 + ss)

  expect_equal(v1$add_vectors(v, v)[], v0*2)
  v2 <- new_vector3()$set(10.2,3.8,4.6)
  expect_error(v1$add_vectors(v, v2))
  v2$copy(v)$add_scalar(0.1)
  expect_equal(v1$copy(v)$add_vectors(v2, v1)[], v0*2 + 0.1)

  expect_equal(v1$copy(v)$add_scaled(v, 0.4)[], v0 * 1.4)

  expect_equal(v1$sub(v)[], v0 * 0.4)
  expect_equal(v1$copy(v)$sub_scalar(0.1)[], v0 - 0.1)
  expect_error(v1$sub_scalar(c(1,2)))
  expect_equal(v1$sub_scalar(s)[], v0 - 0.1 - ss)

  expect_equal(v1$copy(v)$multiply_scalar(1.5)$sub_vectors(v1, v)[], v0 * 0.5)
  expect_equal(v1$copy(v)$multiply_scalar(1.5)$sub_vectors(v, v1)[], v0 * -0.5)
  expect_equal(v1$copy(v)$multiply(v)[], v0^2)
  expect_equal(v1$copy(v)$multiply_scalar(s)[],
               v0 * ss)
  expect_error(v1$multiply_scalar(c(1,2)))
  expect_equal(v1$multiply_vectors(v, v)[], v0^2)
  expect_equal(v1$copy(v)$add_scalar(0.1)$multiply_vectors(v, v1)[], v0 * (v0+0.1))
  expect_equal(v1$copy(v)$add_scalar(0.1)$multiply_vectors(v1, v)[], v0 * (v0+0.1))

  expect_equal(v1$copy(v)$multiply_scalar(s)$divide(v)[],
               ss)
  expect_equal(v1$copy(v)$divide_scalar(s)[], v0 / ss)

  v2 <- new_vector3()$set(5.2,3.8,4.6)
  v00 <- v0
  v00[1, v00[1,] > v2$get_x(1)] <- v2$get_x(1)
  v00[2, v00[2,] > v2$get_y(1)] <- v2$get_y(1)
  v00[3, v00[3,] > v2$get_z(1)] <- v2$get_z(1)
  expect_equal(v1$copy(v)$min(v2)[], v00)
  expect_equal(v1$max(v)[], v0)

  v00 <- v0
  v00[1, v00[1,] < v2$get_x(1)] <- v2$get_x(1)
  v00[2, v00[2,] < v2$get_y(1)] <- v2$get_y(1)
  v00[3, v00[3,] < v2$get_z(1)] <- v2$get_z(1)
  expect_equal(v1$max(v2)[], v00)
  expect_equal(v1$min(v)[], v0)

  v2$set(10.2,3.8,4.6)
  v3 <- new_vector3()$set(5.2,13.8,14.6)
  v00 <- v0
  v00[1, v00[1,] > v2$get_x(1)] <- v2$get_x(1)
  v00[2, v00[2,] < v2$get_y(1)] <- v2$get_y(1)
  v00[3, v00[3,] < v2$get_z(1)] <- v2$get_z(1)
  v00[1, v00[1,] < v3$get_x(1)] <- v3$get_x(1)
  v00[2, v00[2,] > v3$get_y(1)] <- v3$get_y(1)
  v00[3, v00[3,] > v3$get_z(1)] <- v3$get_z(1)
  expect_equal(v1$copy(v)$clamp(v2, v3)[], v00)

  v2$from_array(c(0.1,-0.1, 0.4999,-0.4999, 0.5, -0.5, 0.999, -0.999, 1.0001, -1.0001, 0, 0))
  expect_equal(v2$floor()[], floor(v2[]))
  v2$from_array(c(0.1,-0.1, 0.4999,-0.4999, 0.5, -0.5, 0.999, -0.999, 1.0001, -1.0001, 0, 0))
  expect_equal(v2$ceil()[], ceiling(v2[]))
  v2$from_array(c(0.1,-0.1, 0.4999,-0.4999, 0.5, -0.5, 0.999, -0.999, 1.0001, -1.0001, 0, 0))
  expect_equal(v2$round()[], round(v2[]))
  v2$from_array(c(0.1,-0.1, 0.4999,-0.4999, 0.5, -0.5, 0.999, -0.999, 1.0001, -1.0001, 0, 0))
  expect_equal(abs(v2$round_to_zero()[]), floor(abs(v2[])))
  expect_equal(v1$copy(v)$negate()[], -v0)

  expect_equal(v1$copy(v)$dot(v), colSums(v0^2))
  expect_error(v1$dot(v2))
  expect_equal(v1$length_squared(), colSums(v0^2))
  expect_equal(v1$length(), sqrt(colSums(v0^2)))
  expect_equal(v1$length_manhattan(), (colSums(abs(v0))))
  expect_equal(v1$normalize()$length(), rep(1, ncol(v)))
  expect_equal(v1$set_length(s)$length(), s)

  expect_equal(v1$copy(v)$add_scalar(0.5)$negate()$lerp(v, alpha = 0.6)[], v0*0.2 - 0.2)
  v1$copy(v)$add_scalar(0.5)$negate()
  v00 <- v0 * 0.4 + v1[] * 0.6
  v1$lerp_vectors(v, v1, 0.6)
  expect_equal(v1[], v00)

  cross_prod <- function(x, y) {
    x <- x[seq_len(3)]
    y <- y[seq_len(3)]
    return(c(
      x[2] * y[3] - x[3] * y[2],
      x[3] * y[1] - x[1] * y[3],
      x[1] * y[2] - x[2] * y[1]
    ))
  }

  v2$from_array(c(10.2,3.8,4.6,-10.5,3.6,-4.2))
  v3$from_array(c(5.2,13.8,14.6, -5.3, 5.67, 10.55))
  v00 <- cbind(
    cross_prod(v2[,1], v3[,1]),
    cross_prod(v2[,2], v3[,2])
  )
  expect_equal(v1$cross_vectors(v2, v3)[], v00)
  expect_equal(v1$copy(v2)$cross(v3)[], v00)
  expect_equal(v1$copy(v3)$cross(v2)[], -v00)
  v2$resize(1)
  v3$resize(1)
  expect_equal(v1$copy(v3)$cross(v2)[], -v00[,1,drop = FALSE])
  expect_equal(v1$copy(v2)$cross_vectors(v3, v1)[], -v00[,1,drop = FALSE])

  v2 <- v1$copy(v)$clone2()$add(v)
  expect_equal(v2[], v0 * 2)
  expect_equal(v1[], v0)

  # // Vector3& applyMatrix3(const Matrix3& m);
  m <- matrix(c(1:9), nrow = 3, byrow = TRUE)
  expect_equal(v1$copy(v)$apply_matrix3(m)[], m %*% v0)

  # // Vector3& applyMatrix4(const Matrix4& m);
  m <- matrix(c(1:12, 0, 0, 0, 1), nrow = 4, byrow = TRUE)
  expect_equal(v1$copy(v)$apply_matrix4(m)[], (m %*% rbind(v0, 1))[1:3,])

  # // Vector3& applyQuaternion(const Quaternion& q);
  q <- c(0.1,0.2,0.3,0.4)
  expect_equal(v1$from_array(c(1,2,3,6,5,4))$apply_quaternion(q)[], cbind(c(0.3, 0.6, 0.9), c(0.12, 2.34, 1.2)))

  # // Vector3& transformDirection(const Matrix4& m);
  # JS Code:
  # new THREE.Vector3().set(1,2,3).transformDirection(new THREE.Matrix4().makeRotationX(Math.PI/2)).toArray()
  expect_equal(
    v1$from_array(c(1,2,3,6,5,4))$transform_direction(new_matrix4()$make_rotation_x(pi/2))[],
    cbind(
      c(0.2672612419124244, -0.8017837257372732, 0.5345224838248488),
      c(0.6837634587578276, -0.45584230583855173, 0.5698028822981898)
    )
  )

  expect_equal(
    v1$from_array(c(1,2,3,6,5,4, 0,0,0))$project_on_vector(new_vector3(1,1,1))[],
    cbind(
      rep(1, 3) * (sum(c(1,2,3)) / sum(c(1,1,1))) ,
      rep(1, 3) * (sum(c(6,5,4)) / sum(c(1,1,1))),
      c(0,0,0)
    )
  )
  expect_equal(
    v1$from_array(c(1,2,3,6,5,4, 0,0,0))$project_on_vector(new_vector3(0,0,0))[],
    cbind(
      c(0,0,0),
      c(0,0,0),
      c(0,0,0)
    )
  )

  expect_equal(
    v1$from_array(c(1,2,3,6,5,4, 0,0,0))$project_on_plane(new_vector3(1,1,1))[],
    cbind(
      c(1,2,3) - rep(1, 3) * (sum(c(1,2,3)) / sum(c(1,1,1))) ,
      c(6,5,4) - rep(1, 3) * (sum(c(6,5,4)) / sum(c(1,1,1))),
      c(0,0,0)
    )
  )
  expect_equal(
    v1$from_array(c(1,2,3,6,5,4, 0,0,0))$project_on_plane(new_vector3(0,0,0))[],
    cbind(
      c(1,2,3),
      c(6,5,4),
      c(0,0,0)
    )
  )

  # // Vector3& reflect(const Vector3& normal);
  ## JS Code:
  # new THREE.Vector3().set(1,2,3).reflect(new THREE.Vector3().set(1,1,1).normalize()).toArray()
  expect_equal(
    v1$from_array(c(1,2,3,6,5,4, 0,0,0))$reflect(new_vector3(1,1,1))[],
    cbind(
      c(-3,-2,-1),
      c(-4,-5,-6),
      c(0,0,0)
    )
  )

  expect_equal(
    v1$from_array(c(1,2,3,6,5,4, 0,0,0))$angle_to(new_vector3(1,1,1))[],
    acos(v2$from_array(c(1,2,3,6,5,4, 0,0,0))$normalize()$dot(new_vector3(1,1,1)$normalize()))
  )
  expect_error(
    v1$from_array(c(1,2,3,6,5,4, 0,0,0))$angle_to(new_vector3(1:2,1:2,1:2))
  )
  expect_equal(
    v1$from_array(c(1,2,3,6,5,4, 0,0,0))$angle_to(new_vector3(1:3,1:3,1:3))[],
    acos(v2$from_array(c(1,2,3,6,5,4, 0,0,0))$normalize()$dot(new_vector3(1,1,1)$normalize()))
  )

  expect_equal(
    v1$from_array(c(1,2,3,6,5,4, 0,0,0))$distance_to(new_vector3(1,1,1)),
    c(
      sqrt(sum((c(1,2,3) - c(1,1,1))^2)),
      sqrt(sum((c(6,5,4) - c(1,1,1))^2)),
      sqrt(sum((c(0,0,0) - c(1,1,1))^2))
    )
  )
  expect_equal(
    v1$from_array(c(1,2,3,6,5,4, 0,0,0))$distance_to_squared(new_vector3(1,1,1)),
    c(
      (sum((c(1,2,3) - c(1,1,1))^2)),
      (sum((c(6,5,4) - c(1,1,1))^2)),
      (sum((c(0,0,0) - c(1,1,1))^2))
    )
  )

  expect_equal(
    v1$from_array(c(1,2,3,6,5,4, 0,0,0))$distance_to_manhattan(new_vector3(1,1,1)),
    c(
      (sum(abs(c(1,2,3) - c(1,1,1)))),
      (sum(abs(c(6,5,4) - c(1,1,1)))),
      (sum(abs(c(0,0,0) - c(1,1,1))))
    )
  )

  m1 <- new_matrix4()$set(1:16)

  expect_equal(
    v2$set_from_matrix_position(m1)[],
    m1[1:3, 4, drop = FALSE]
  )
  expect_equal(
    v2$set_from_matrix_scale(m1)[,1],
    apply(m1[1:3, 1:3], 2, norm, '2')
  )

  # JS Code
  # new THREE.Vector3().setFromSphericalCoords(1,2,3).toArray()
  expect_equal(
    v2$set_from_spherical_coords(1,2,3)[],
    cbind(c(0.12832006020245673, -0.4161468365471424, -0.9001976297355174))
  )

  # // Vector3& applyAxisAngle(const Vector3& axis, double angle);
  # // Vector3& applyEuler(const Euler& euler);
  # // Vector3& applyNormalMatrix(const Matrix3& m);
  # // Vector3& project(const Camera& camera);
  # // Vector3& unproject(const Camera& camera);
  # // Vector3& setFromSpherical(const Spherical& s);
  # // Vector3& setFromMatrix3Column(const Matrix3& m, unsigned int index);

})
