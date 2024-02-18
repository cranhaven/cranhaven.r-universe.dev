test_that("Matrix4", {

  library(testthat)
  m <- ravetools:::new_matrix4()
  expect_true(m$is_matrix4)

  expect_equal(m$to_array(), diag(1, 4))
  expect_equal(m$elements, diag(1, 4))

  m$set(matrix(1:16, 4, 4))
  expect_equal(m$to_array(), matrix(1:16, 4, 4))
  expect_equal(m[], matrix(1:16, 4, 4))

  m$set(1:16)
  m0 <- matrix(1:16, 4, 4, byrow = TRUE)
  expect_equal(m[], m0)

  m2 <- m$clone2()
  expect_equal(m2$identity()[], diag(1, 4))
  expect_equal(m[], m0)

  m2$copy(m)
  expect_equal(m2[], m0)

  expect_equal(m2$identity()$copy_position(m)[], matrix(c(1,0,0,4,
                                                          0,1,0,8,
                                                          0,0,1,12,
                                                          0,0,0,1), 4, byrow = TRUE))
  x <- ravetools:::new_vector3()
  y <- ravetools:::new_vector3()
  z <- ravetools:::new_vector3()
  m2$copy(m)$extract_basis(x,y,z)
  expect_equal(x[], m0[1:3,1,drop = FALSE])
  expect_equal(y[], m0[1:3,2,drop = FALSE])
  expect_equal(z[], m0[1:3,3,drop = FALSE])


  m00 <- diag(1, 4)
  m00[1:3,1] <- x[]
  m00[1:3,2] <- y[]
  m00[1:3,3] <- 0
  expect_equal(m2$identity()$make_basis(x, y)[], m00)
  m00[1:3,3] <- z[]
  expect_equal(m2$identity()$make_basis(x, y, z)[], m00)

  m00[1:3,1] <- y[]
  m00[1:3,2] <- z[]
  m00[1:3,3] <- x[]
  expect_equal(m2$identity()$make_basis(y[], z[], x[])[], m00)

  m00 <- m2$extract_rotation(m)[]
  expect_equal(m0[1:3,1] / m00[1:3,1], rep(norm(m0[1:3,1], "2"), 3))
  expect_equal(m0[1:3,2] / m00[1:3,2], rep(norm(m0[1:3,2], "2"), 3))
  expect_equal(m0[1:3,3] / m00[1:3,3], rep(norm(m0[1:3,3], "2"), 3))

  ## JS code:
  ## new THREE.Matrix4().lookAt(new THREE.Vector3(1,2,3), new THREE.Vector3(4,5,6), new THREE.Vector3(9,8,6)).elements.join(",")
  ex <- matrix(
    c(-0.5345224838248486,0.8017837257372731,-0.2672612419124245,0,
      0.6172133998483676,0.15430334996209175,-0.7715167498104594,0,
      -0.5773502691896257,-0.5773502691896257,-0.5773502691896257,0,
      0,0,0,1), 4
  )
  expect_equal(m2$identity()$look_at(new_vector3(1,2,3), new_vector3(4,5,6), new_vector3(9,8,6))[], ex)


  expect_equal(m2$set(t(m0))$multiply(m)[], t(m0) %*% m0)
  expect_equal(m2$set(t(m0))$premultiply(m)[], m0 %*% t(m0))
  expect_equal(m2$set(m0)$multiply_matrices(m, m2)[], m0 %*% m0)
  expect_equal(m2$set(m0)$multiply_scalar(1.4)[], m0 * 1.4)

  m00 <- matrix(c(
    1, 0, 0, 128,
    -1,1, 0, 128,
    0,-1, 1, 128,
    0, 0, 4, 1
  ), 4, 4, byrow = TRUE)
  expect_equal(m2$set(m00)$determinant(), det(m00))

  m00 <- t(m0)
  m00[1:3,4] <- 21:23
  expect_equal(m2$set(m0)$transpose()$set_position(21:23)[], m00)

  m00 <- matrix(c(
    1, 0, 0, 128,
    -1,1, 0, 128,
    0,-1, 1, 128,
    0, 0, 4, 1
  ), 4, 4, byrow = TRUE)
  expect_equal(m2$set(m00)$invert()[], solve(m00))
  # microbenchmark::microbenchmark(
  #   m2$set(m00)$invert(),
  #   solve(m00)
  # )
  expect_equal(m2$set(m00)$scale(2,3,4)[], m00 %*% diag(c(2,3,4,1)))

  expect_equal(m2$set(m00)$get_max_scale_on_axis(), sqrt(2))

  m00 <- diag(1, 4)
  m00[1:3,4] <- c(1,2,3)
  expect_equal(m2$make_translation(1,2,3)[], m00)

  theta <- pi / 6
  expect_equal(m2$make_rotation_x(theta)[], matrix(c(
    1, 0, 0, 0,
    0, cos(theta), -sin(theta), 0,
    0, sin(theta), cos(theta), 0,
    0, 0, 0, 1
  ), 4, 4, byrow = TRUE))

  expect_equal(m2$make_rotation_y(theta)[], matrix(c(
    cos(theta), 0, sin(theta), 0,
    0, 1, 0, 0,
    -sin(theta), 0, cos(theta), 0,
    0, 0, 0, 1
  ), 4, 4, byrow = TRUE))

  expect_equal(m2$make_rotation_z(theta)[], matrix(c(
    cos(theta), -sin(theta), 0, 0,
    sin(theta), cos(theta), 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 1
  ), 4, 4, byrow = TRUE))

  ## JS Code:
  # new THREE.Matrix4().makeRotationAxis(new THREE.Vector3(1,2,3), Math.PI / 6).elements.join(",")
  expect_equal(m2$make_rotation_axis(c(1,2,3), theta)[], matrix(c(
    1,1.7679491924311224,-0.598076211353316,0,
    -1.2320508075688772,1.401923788646684,1.3038475772933678,0,
    1.4019237886466838,0.3038475772933678,2.0717967697244903,0,
    0,0,0,1
  ), 4, 4, byrow = FALSE))

  expect_equal(m2$make_scale(1,2,3)[], diag(c(1,2,3,1)))
  expect_equal(m2$make_scale(x)[], diag(c(x[],1)))

  # xy, xz, yx, yz, zx, zy
  expect_equal(m2$make_shear(1,2,3,4,5,6)[], matrix(c(
    1, 3, 5, 0,
    1, 1, 6, 0,
    2, 4, 1, 0,
    0, 0, 0, 1
  ), 4, 4, byrow = TRUE))

  ## JS Code:
  # new THREE.Matrix4().makePerspective(1,2,3,4,5,6).elements.join(",")
  expect_equal(m2$make_perpective(1,2,3,4,5,6)[], matrix(c(
    10,0,0,0,0,-10,0,0,3,-7,-11,-1,0,0,-60,0
  ), 4, 4, byrow = FALSE))

  ## JS Code:
  # new THREE.Matrix4().makeOrthographic(1,2,3,4,5,6).elements.join(",")
  expect_equal(m2$make_orthographic(1,2,3,4,5,6)[], matrix(c(
    2,0,0,0,0,-2,0,0,0,0,-2,0,-3,7,-11,1
  ), 4, 4, byrow = FALSE))

})
